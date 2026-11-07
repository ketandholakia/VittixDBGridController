unit Vittix.DBGrid.Provider.FireDAC;

{$REGION 'Documentation'}
/// <summary>
/// FireDAC implementation of IVittixDataProvider: pushes the grid's filter
/// groups and sort columns into a TFDQuery as parameterized WHERE / ORDER
/// BY clauses wrapped around the application's original SQL:
///
///   SELECT * FROM ( original SQL ) VITTIX_SUB WHERE ... ORDER BY ...
///
/// The original SQL text and the application's own parameter values are
/// remembered and restored by RestoreOriginal (controller teardown,
/// dataset change, mode switch) and after a failed re-query. Queries that
/// cannot be wrapped safely are rejected by CanHandle: commands
/// (EXEC/CALL), UNION queries, and SQL with a top-level ORDER BY or
/// semicolon - the wrapper would change their meaning, so those fall back
/// to client-side filtering instead.
///
/// The current record is preserved across a re-query by locating the
/// previously-current row through its first fields (best effort; the grid
/// goes to the first record when the row is filtered out).
///
/// This unit lives outside the core runtime package so the core does not
/// hard-depend on FireDAC; it is delivered in the
/// VittixDBGridControllerFD package.
/// </summary>
{$ENDREGION}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Stan.Intf,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Filter.Engine,
  Vittix.DBGrid.Provider;

type
  { Forward declaration: the query-lifetime watcher (see TVittixQueryWatch). }
  TVittixFireDACProvider = class;

  /// <summary>Internal TComponent shim so the provider can observe the
  /// lifetime of the application's query. TInterfacedObject cannot receive
  /// FreeNotification directly (that API is TComponent-only); the watcher
  /// is registered on the query and nils the provider's FQuery reference
  /// when the query is destroyed first. The watcher has no owner - it is
  /// created by the provider and freed by its destructor, so it always
  /// outlives the notification registration.</summary>
  TVittixQueryWatch = class(TComponent)
  protected
    FProvider: TVittixFireDACProvider;
  public
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  end;

  /// <summary>IVittixDataProvider for TFDQuery (and simple TFDTable
  /// usage). Construct with the query the grid is bound to; the provider
  /// never takes ownership of the query and observes its destruction
  /// through TVittixQueryWatch, so destroying the query first leaves the
  /// provider inert (no dangling reference) instead of crashing later.</summary>
  TVittixFireDACProvider = class(TInterfacedObject, IVittixDataProvider)
  private
    FQuery: TFDQuery;
    FQueryWatch: TVittixQueryWatch;
    FOriginalSQL: string;
    FOriginalParams: TArray<TVittixSQLParam>;
    FAppParamsSaved: Boolean;
    FLastWhere: string;
    FLastOrderBy: string;
    FLastFallbackReason: string;
    FFilterActive: Boolean;
    FSortActive: Boolean;
    FLastFilterParams: TVittixSQLParams;
    FAggQuery: TFDQuery;
    // Record preservation across a re-query: values of the first fields
    FKeyFields: string;
    FKeyValues: Variant;

    procedure QueryDestroyed;
    function Dialect: TVittixSQLDialect;
    function DialectName: string;
    function CheckWrappable(const ASQL: string; out AReason: string): Boolean;
    function FieldsInfo: TArray<TVittixSQLFieldInfo>;
    procedure SaveAppParams;
    procedure RestoreAppParams(AQuery: TFDQuery);
    function WrappedSQL(const AWhere, AOrderBy: string): string;
    procedure ApplyWrappedSQL(const AWhere, AOrderBy: string;
      const AParams: TVittixSQLParams);
    procedure BindParams(AQuery: TFDQuery; const AParams: TVittixSQLParams);
    procedure ApplySQL(const ANewSQL: string; const AParams: TVittixSQLParams;
      AOwnParams: Boolean);
    procedure SaveCurrentRecordKey;
    procedure RestoreCurrentRecordKey;
    procedure ReopenOriginal;
  public
    constructor Create(AQuery: TFDQuery);
    destructor Destroy; override;

    { IVittixDataProvider }
    function CanHandle(ADataSet: TDataSet): Boolean;
    function ApplyFilter(const AGroups: TVittixFilterConditionGroups;
      const AGlobalSearch: string;
      const AFields: TArray<TVittixSQLFieldInfo>): Boolean;
    function ApplySort(const ASortColumns: TArray<TVittixSortColumn>;
      const AFields: TArray<TVittixSQLFieldInfo>): Boolean;
    procedure ClearFilter;
    procedure ClearSort;
    procedure RestoreOriginal;
    function FetchAggregates(const ARequests: TArray<TVittixAggRequest>;
      out AResults: TArray<TVittixAggResult>): Boolean;
    function GetLastSQLWhere: string;
    function GetLastSQLOrderBy: string;
    function GetLastFallbackReason: string;
    function GetDialectName: string;
  end;

implementation

uses
  System.StrUtils,
  FireDAC.Stan.Param;

{ TVittixQueryWatch }

procedure TVittixQueryWatch.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and Assigned(FProvider) and
     (AComponent = FProvider.FQuery) then
    FProvider.QueryDestroyed;
end;

{ TVittixFireDACProvider }

procedure TVittixFireDACProvider.QueryDestroyed;
begin
  // The application freed its query while this provider was still
  // referenced. Drop every reference into the freed object so neither the
  // interface methods nor this destructor can touch it, and drop the
  // aggregate query too (it borrowed FQuery.Connection).
  FQuery := nil;
  FOriginalSQL := '';
  FAppParamsSaved := False;
  FLastWhere := '';
  FLastOrderBy := '';
  FLastFallbackReason := '';
  FFilterActive := False;
  FSortActive := False;
  FreeAndNil(FAggQuery);
end;

constructor TVittixFireDACProvider.Create(AQuery: TFDQuery);
begin
  inherited Create;
  FQuery := AQuery;
  if Assigned(FQuery) then
  begin
    FQueryWatch := TVittixQueryWatch.Create(nil);
    FQueryWatch.FProvider := Self;
    FQuery.FreeNotification(FQueryWatch);
  end;
end;

destructor TVittixFireDACProvider.Destroy;
begin
  // Stop observing before touching anything else: when the query is gone
  // already the watcher has nilled FQuery and this destructor must not
  // try to restore SQL into a freed object.
  if Assigned(FQueryWatch) then
  begin
    if Assigned(FQuery) then
      FQuery.RemoveFreeNotification(FQueryWatch);
    FQueryWatch.FProvider := nil;
    FreeAndNil(FQueryWatch);
  end;
  // The provider can outlive a controller swap; never leave the app's
  // query carrying the wrapper SQL. During teardown this destructor runs
  // while the grid is being destroyed, so the restore must not re-open
  // the query (DisableControls/Open walk data links that may already be
  // freed) - closing first, detaching from the dying data source and
  // restoring the SQL text is the guarantee that matters here.
  if Assigned(FQuery) and (FOriginalSQL <> '') then
  begin
    // Detach from the data source BEFORE closing: during teardown the
    // grid behind that data source may already be half-dead, and Close
    // would notify it.
    try
      FQuery.DataSource := nil;
    except
    end;
    try
      FQuery.Close;
    except
    end;
    try
      FQuery.SQL.Text := FOriginalSQL;
      RestoreAppParams(FQuery);
    except
    end;
  end;
  FreeAndNil(FAggQuery);
  inherited;
end;

function TVittixFireDACProvider.Dialect: TVittixSQLDialect;
begin
  Result := sdANSI;
  if not Assigned(FQuery) or not Assigned(FQuery.Connection) then
    Exit;
  case FQuery.Connection.RDBMSKind of
    TFDRDBMSKinds.Firebird: Result := sdFirebird;
    TFDRDBMSKinds.SQLite: Result := sdSQLite;
    TFDRDBMSKinds.MySQL: Result := sdMySQL;
    TFDRDBMSKinds.PostgreSQL: Result := sdPostgreSQL;
    TFDRDBMSKinds.MSSQL: Result := sdSQLServer;
  else
    Result := sdANSI;
  end;
end;

function TVittixFireDACProvider.DialectName: string;
begin
  case Dialect of
    sdFirebird: Result := 'Firebird';
    sdSQLite: Result := 'SQLite';
    sdMySQL: Result := 'MySQL';
    sdPostgreSQL: Result := 'PostgreSQL';
    sdSQLServer: Result := 'SQL Server';
  else
    Result := 'ANSI';
  end;
end;

function TVittixFireDACProvider.CheckWrappable(const ASQL: string;
  out AReason: string): Boolean;
var
  UpperSQL: string;
begin
  // Safety over cleverness: any of these shapes would change meaning
  // inside "SELECT * FROM ( ... )", so they fall back to the client.
  UpperSQL := ' ' + UpperCase(Trim(ASQL)) + ' ';
  Result := False;

  if Trim(ASQL) = '' then
  begin
    AReason := 'the query has no SQL text';
    Exit;
  end;

  if UpperSQL.Contains(' UNION ') then
  begin
    AReason := 'the query contains UNION';
    Exit;
  end;

  if UpperSQL.Contains(' ORDER BY ') then
  begin
    // Over-rejective by design: an ORDER BY inside a subquery also trips
    // this, but guessing top-level structure is riskier than declining.
    AReason := 'the query contains ORDER BY';
    Exit;
  end;

  if UpperSQL.Contains(';') then
  begin
    AReason := 'the query contains a semicolon (multi-statement)';
    Exit;
  end;

  if UpperSQL.Contains(' EXEC ') or UpperSQL.Contains(' EXECUTE ') or
     UpperSQL.Contains(' CALL ') or
     UpperSQL.StartsWith(' EXEC ', True) or
     UpperSQL.StartsWith(' EXECUTE ', True) or
     UpperSQL.StartsWith(' CALL ', True) then
  begin
    AReason := 'the query is a stored procedure command';
    Exit;
  end;

  Result := True;
end;

function TVittixFireDACProvider.CanHandle(ADataSet: TDataSet): Boolean;
var
  Reason: string;
begin
  Result := False;
  FLastFallbackReason := '';
  if not Assigned(FQuery) or (ADataSet <> FQuery) then
  begin
    FLastFallbackReason := 'the provider is not bound to this dataset';
    Exit;
  end;
  Result := CheckWrappable(FQuery.SQL.Text, Reason);
  if not Result then
    FLastFallbackReason := Reason;
end;

function TVittixFireDACProvider.FieldsInfo: TArray<TVittixSQLFieldInfo>;
var
  I: Integer;
begin
  SetLength(Result, 0);
  if not Assigned(FQuery) then
    Exit;
  for I := 0 to FQuery.Fields.Count - 1 do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].Name := FQuery.Fields[I].FieldName;
    Result[High(Result)].DataType := FQuery.Fields[I].DataType;
  end;
end;

procedure TVittixFireDACProvider.SaveAppParams;
var
  I: Integer;
begin
  // FireDAC re-derives the Params collection from the SQL text; the
  // application's own parameter values must survive the wrap.
  SetLength(FOriginalParams, 0);
  if not Assigned(FQuery) then
    Exit;
  for I := 0 to FQuery.Params.Count - 1 do
  begin
    SetLength(FOriginalParams, Length(FOriginalParams) + 1);
    FOriginalParams[High(FOriginalParams)].Name := FQuery.Params[I].Name;
    FOriginalParams[High(FOriginalParams)].Value := FQuery.Params[I].Value;
    FOriginalParams[High(FOriginalParams)].DataType := FQuery.Params[I].DataType;
  end;
  FAppParamsSaved := True;
end;

procedure TVittixFireDACProvider.RestoreAppParams(AQuery: TFDQuery);
var
  I, J: Integer;
begin
  // The wrapped SQL keeps the base query's :params by name; re-assign the
  // saved values to the matching (re-parsed) entries.
  for I := 0 to AQuery.Params.Count - 1 do
    for J := 0 to High(FOriginalParams) do
      if SameText(AQuery.Params[I].Name, FOriginalParams[J].Name) then
      begin
        AQuery.Params[I].Value := FOriginalParams[J].Value;
        AQuery.Params[I].DataType := FOriginalParams[J].DataType;
        Break;
      end;
end;

function TVittixFireDACProvider.WrappedSQL(const AWhere,
  AOrderBy: string): string;
begin
  Result := 'SELECT * FROM (' + sLineBreak + FOriginalSQL + sLineBreak +
    ') VITTIX_SUB';
  if AWhere <> '' then
    Result := Result + sLineBreak + 'WHERE ' + AWhere;
  if AOrderBy <> '' then
    Result := Result + sLineBreak + 'ORDER BY ' + AOrderBy;
end;

procedure TVittixFireDACProvider.SaveCurrentRecordKey;
var
  I, MaxFields: Integer;
  Values: TArray<Variant>;
  Names: string;
begin
  // Best-effort record preservation: remember the first few NON-NULL
  // field values of the current row and Locate them after the re-query
  // (a NULL inside a Locate key does not match reliably).
  FKeyFields := '';
  FKeyValues := Null;
  if not Assigned(FQuery) or not FQuery.Active or FQuery.IsEmpty then
    Exit;

  MaxFields := FQuery.Fields.Count;
  if MaxFields > 6 then
    MaxFields := 6;
  SetLength(Values, 0);
  for I := 0 to MaxFields - 1 do
  begin
    if FQuery.Fields[I].IsNull then
      Continue;
    SetLength(Values, Length(Values) + 1);
    Values[High(Values)] := FQuery.Fields[I].Value;
    if Names <> '' then
      Names := Names + ';';
    Names := Names + FQuery.Fields[I].FieldName;
  end;
  if Names = '' then
    Exit;
  FKeyFields := Names;
  FKeyValues := VarArrayOf(Values);
end;

procedure TVittixFireDACProvider.RestoreCurrentRecordKey;
begin
  if (FKeyFields = '') or VarIsNull(FKeyValues) then
    Exit;
  try
    if not FQuery.Locate(FKeyFields, FKeyValues, []) then
      FQuery.First;
  except
    // Locate is a convenience; never break the re-query over it.
    try FQuery.First; except end;
  end;
  FKeyFields := '';
  FKeyValues := Null;
end;

procedure TVittixFireDACProvider.BindParams(AQuery: TFDQuery;
  const AParams: TVittixSQLParams);
var
  I: Integer;
  P: TFDParam;
begin
  // The single binding path for every provider query (main fetch and
  // aggregates alike): re-parsed :Name placeholders get the filter
  // parameter's DataType then Value. Values never enter the SQL text.
  for I := 0 to High(AParams) do
  begin
    P := AQuery.Params.FindParam(AParams[I].Name);
    if P <> nil then
    begin
      P.DataType := AParams[I].DataType;
      P.Value := AParams[I].Value;
    end;
  end;
end;

procedure TVittixFireDACProvider.ApplySQL(const ANewSQL: string;
  const AParams: TVittixSQLParams; AOwnParams: Boolean);
var
  WasOpen: Boolean;
begin
  WasOpen := FQuery.Active;
  FQuery.DisableControls;
  try
    FQuery.Close;
    if not FAppParamsSaved then
      SaveAppParams;
    FQuery.SQL.Text := ANewSQL;
    RestoreAppParams(FQuery);
    if AOwnParams then
      BindParams(FQuery, AParams);
    FQuery.Open;
  finally
    FQuery.EnableControls;
  end;
  if not WasOpen then
    ; // the app had it closed; opening it is the point of the operation
end;

procedure TVittixFireDACProvider.ApplyWrappedSQL(const AWhere,
  AOrderBy: string; const AParams: TVittixSQLParams);
var
  SavedKeyFields: string;
  SavedKeyValues: Variant;
begin
  SaveCurrentRecordKey;
  SavedKeyFields := FKeyFields;
  SavedKeyValues := FKeyValues;
  FKeyFields := ''; // ApplySQL must not capture mid-wrap
  try
    try
      ApplySQL(WrappedSQL(AWhere, AOrderBy), AParams, True);
    except
      on E: Exception do
      begin
        // A failing query (syntax, connection) must leave the grid in its
        // previous state: original SQL back, re-opened, then the error
        // surfaces for OnProviderError handling.
        FLastFallbackReason := E.Message;
        ReopenOriginal;
        raise;
      end;
    end;
  finally
    FKeyFields := SavedKeyFields;
    FKeyValues := SavedKeyValues;
  end;
  RestoreCurrentRecordKey;
end;

procedure TVittixFireDACProvider.ReopenOriginal;
begin
  if not Assigned(FQuery) then
    Exit;
  try
    FQuery.DisableControls;
    try
      FQuery.Close;
      FQuery.SQL.Text := FOriginalSQL;
      RestoreAppParams(FQuery);
      FQuery.Open;
    finally
      FQuery.EnableControls;
    end;
  except
    // Best effort only; the original error matters more.
  end;
end;

procedure TVittixFireDACProvider.RestoreOriginal;
begin
  if not Assigned(FQuery) or (FOriginalSQL = '') then
  begin
    FFilterActive := False;
    FSortActive := False;
    FLastWhere := '';
    FLastOrderBy := '';
    Exit;
  end;
  ReopenOriginal;
  FOriginalSQL := '';
  FAppParamsSaved := False;
  FFilterActive := False;
  FSortActive := False;
  FLastWhere := '';
  FLastOrderBy := '';
end;

function TVittixFireDACProvider.ApplyFilter(
  const AGroups: TVittixFilterConditionGroups; const AGlobalSearch: string;
  const AFields: TArray<TVittixSQLFieldInfo>): Boolean;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  EffectiveWhere: string;
begin
  Result := False;
  FLastFallbackReason := '';
  if not Assigned(FQuery) then
  begin
    FLastFallbackReason := 'no query bound';
    Exit;
  end;

  // The original SQL is snapshotted on the first server operation.
  if FOriginalSQL = '' then
  begin
    if not CheckWrappable(FQuery.SQL.Text, Reason) then
    begin
      FLastFallbackReason := Reason;
      Exit(False);
    end;
    FOriginalSQL := FQuery.SQL.Text;
  end;

  if Length(AGroups) = 0 then
  begin
    // Nothing to filter server-side: drop the WHERE (the sort stays).
    ClearFilter;
    Exit(True);
  end;

  if not TVittixSQLBuilder.BuildWhere(Dialect, AFields, AGroups,
    AGlobalSearch, Where, Params, Reason) then
  begin
    FLastFallbackReason := Reason;
    Exit(False);
  end;

  EffectiveWhere := Where;
  FLastWhere := Where;
  ApplyWrappedSQL(EffectiveWhere, FLastOrderBy, Params);
  FFilterActive := True;
  FLastFilterParams := Params;
  Result := True;
end;

function TVittixFireDACProvider.ApplySort(
  const ASortColumns: TArray<TVittixSortColumn>;
  const AFields: TArray<TVittixSQLFieldInfo>): Boolean;
var
  OrderBy, Reason: string;
begin
  Result := False;
  FLastFallbackReason := '';
  if not Assigned(FQuery) then
  begin
    FLastFallbackReason := 'no query bound';
    Exit;
  end;

  if FOriginalSQL = '' then
  begin
    if not CheckWrappable(FQuery.SQL.Text, Reason) then
    begin
      FLastFallbackReason := Reason;
      Exit(False);
    end;
    FOriginalSQL := FQuery.SQL.Text;
  end;

  if Length(ASortColumns) = 0 then
  begin
    ClearSort;
    Exit(True);
  end;

  if not TVittixSQLBuilder.BuildOrderBy(Dialect, AFields, ASortColumns,
    OrderBy, Reason) then
  begin
    FLastFallbackReason := Reason;
    Exit(False);
  end;

  FLastOrderBy := OrderBy;
  // A WHERE captured earlier stays part of the wrap; re-apply it with the
  // parameter values the filter application last used.
  ApplyWrappedSQL(FLastWhere, OrderBy, FLastFilterParams);
  FSortActive := True;
  Result := True;
end;

procedure TVittixFireDACProvider.ClearFilter;
var
  Where, Reason: string;
begin
  if not Assigned(FQuery) then
    Exit;
  FLastFallbackReason := '';
  if (FOriginalSQL = '') and (FLastWhere = '') then
    Exit;

  // Re-wrap without the WHERE (the ORDER BY survives).
  Where := '';
  FLastWhere := '';
  if FOriginalSQL = '' then
    Exit;
  if FLastOrderBy <> '' then
  begin
    // Re-wrap keeping the sort. The filter parameters no longer apply.
    ApplySQL(WrappedSQL('', FLastOrderBy), nil, False);
  end
  else
  begin
    // No sort either: back to the plain original SQL.
    ReopenOriginal;
    FOriginalSQL := '';
    FAppParamsSaved := False;
  end;
  FFilterActive := False;
  // silence unused hint
  Reason := Where;
  if Reason = '' then ;
end;

procedure TVittixFireDACProvider.ClearSort;
begin
  if not Assigned(FQuery) then
    Exit;
  FLastFallbackReason := '';
  FLastOrderBy := '';
  FSortActive := False;
  if FOriginalSQL = '' then
    Exit;
  if FLastWhere <> '' then
    ApplySQL(WrappedSQL(FLastWhere, ''), nil, False)
  else
  begin
    ReopenOriginal;
    FOriginalSQL := '';
    FAppParamsSaved := False;
  end;
end;

function TVittixFireDACProvider.FetchAggregates(
  const ARequests: TArray<TVittixAggRequest>;
  out AResults: TArray<TVittixAggResult>): Boolean;
var
  I: Integer;
  Selects: TStringList;
  SQL: TStringBuilder;
  FieldName, Col: string;
  RowsField, NullCol: TField;
  Dialect: TVittixSQLDialect;
begin
  Result := False;
  SetLength(AResults, 0);
  if (not Assigned(FQuery)) or (FOriginalSQL = '') then
    Exit;

  Dialect := Self.Dialect;
  Selects := TStringList.Create;
  SQL := TStringBuilder.Create('SELECT COUNT(*) AS AGG_ROWS');
  try
    for I := 0 to High(ARequests) do
    begin
      FieldName := ARequests[I].FieldName;
      Col := TVittixSQLBuilder.QuoteIdentifier(Dialect, FieldName);
      case ARequests[I].AggType of
        vatCount: Selects.Add('0'); // value handled by AGG_ROWS
        vatSum:   Selects.Add(Format('SUM(%s) AS AGG_%d', [Col, I]));
        vatAvg:   Selects.Add(Format('AVG(%s) AS AGG_%d', [Col, I]));
        vatMin:   Selects.Add(Format('MIN(%s) AS AGG_%d', [Col, I]));
        vatMax:   Selects.Add(Format('MAX(%s) AS AGG_%d', [Col, I]));
      else
        Selects.Add(Format('NULL AS AGG_%d', [I]));
      end;
      // NullCount mirrors the client aggregation engine: the number of
      // NULL values of the field inside the group, i.e.
      //   NullCount = COUNT(*) - COUNT(col)
      // COUNT skips NULLs, so this is exact for every aggregate type.
      Selects.Add(Format('COUNT(%s) AS AGG_%d_C', [Col, I]));
    end;

    for I := 0 to Selects.Count - 1 do
      if Selects[I] <> '0' then
        SQL.Append(', ' + Selects[I]);
    SQL.Append(' FROM (' + sLineBreak + FOriginalSQL + sLineBreak +
      ') VITTIX_SUB');
    if FLastWhere <> '' then
      SQL.Append(sLineBreak + 'WHERE ' + FLastWhere);

    if FAggQuery = nil then
      FAggQuery := TFDQuery.Create(nil);
    FAggQuery.Connection := FQuery.Connection;
    FAggQuery.SQL.Text := SQL.ToString;
    // The application's own :params survive the wrap by name ...
    RestoreAppParams(FAggQuery);
    // ... and the filter's :vxN parameters MUST be bound with the exact
    // values the main query carries, or the WHERE compares against NULL
    // and the aggregate reports the wrong group. Same binding path as the
    // main fetch (BindParams), so data types and NULL semantics match.
    BindParams(FAggQuery, FLastFilterParams);
    FAggQuery.Open;

    RowsField := FAggQuery.FindField('AGG_ROWS');
    SetLength(AResults, Length(ARequests));
    for I := 0 to High(ARequests) do
    begin
      AResults[I].FieldName := ARequests[I].FieldName;
      AResults[I].AggType := ARequests[I].AggType;
      if FAggQuery.FindField('AGG_' + IntToStr(I)) <> nil then
        AResults[I].Value := FAggQuery.FieldByName('AGG_' + IntToStr(I)).Value
      else
        AResults[I].Value := Null;
      if RowsField <> nil then
        AResults[I].Count := RowsField.AsInteger;
      NullCol := FAggQuery.FindField('AGG_' + IntToStr(I) + '_C');
      if (RowsField <> nil) and Assigned(NullCol) then
        AResults[I].NullCount := AResults[I].Count - NullCol.AsInteger
      else
        AResults[I].NullCount := 0;
    end;
    Result := True;
  finally
    if FAggQuery <> nil then
    begin
      FAggQuery.Close;
      FAggQuery.SQL.Text := '';
    end;
    SQL.Free;
    Selects.Free;
  end;
end;

function TVittixFireDACProvider.GetLastSQLWhere: string;
begin
  Result := FLastWhere;
end;

function TVittixFireDACProvider.GetLastSQLOrderBy: string;
begin
  Result := FLastOrderBy;
end;

function TVittixFireDACProvider.GetLastFallbackReason: string;
begin
  Result := FLastFallbackReason;
end;

function TVittixFireDACProvider.GetDialectName: string;
begin
  Result := DialectName;
end;

end.
