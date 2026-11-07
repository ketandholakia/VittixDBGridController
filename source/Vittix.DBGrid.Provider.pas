unit Vittix.DBGrid.Provider;

{$REGION 'Documentation'}
/// <summary>
/// Server-side filter/sort provider abstraction (v2.0).
///
/// TVittixSQLBuilder turns the engine's filter groups (single operator
/// table syntax) into a parameterized WHERE fragment and sort columns into
/// an ORDER BY fragment, for a set of SQL dialects. It is pure and
/// dataset-free: the caller resolves the dataset's field list into
/// TVittixSQLFieldInfo entries, and every user value leaves the builder as
/// a typed parameter - never concatenated SQL text.
///
/// IVittixDataProvider is what the controller calls when the grid's
/// FilterMode is vcmServer or vcmAuto. ApplyFilter/ApplySort return False
/// when the request cannot be expressed in SQL, which makes vcmAuto fall
/// back to the client-side engine (and fires OnProviderFallback).
///
/// Client-side filtering remains the default and the correctness baseline;
/// nothing changes for applications that never set FilterMode.
/// </summary>
{$ENDREGION}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Data.DB,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Filter.Engine;

type
  /// <summary>Data routing mode. vcmClient (default) evaluates everything
  /// in the client engine exactly as before; vcmServer pushes filtering
  /// and sorting into the database through the assigned Provider;
  /// vcmAuto uses the server when a provider can handle the dataset and
  /// every active condition is expressible, and the client engine
  /// otherwise. One property governs both filter and sort: a hybrid
  /// (server filter with client sort) would mean two re-queries per
  /// change and two disagreeing row orders for no practical gain.</summary>
  TVittixFilterMode = (vcmClient, vcmServer, vcmAuto);

  /// <summary>SQL dialects the builder knows. The dialect controls
  /// identifier quoting, the ESCAPE literal and explicit NULL placement
  /// in ORDER BY.</summary>
  TVittixSQLDialect = (sdANSI, sdFirebird, sdSQLite, sdMySQL, sdPostgreSQL,
    sdSQLServer);

  /// <summary>One dataset field as seen by the SQL builder. Name is the
  /// dataset's FieldName (which for wrapped queries is the underlying
  /// column name); DataType drives typed parameters and which operators
  /// are expressible.</summary>
  TVittixSQLFieldInfo = record
    Name: string;
    DataType: TFieldType;
  end;

  /// <summary>One sort column for BuildOrderBy.</summary>
  TVittixSortColumn = record
    FieldName: string;
    Descending: Boolean;
  end;

  /// <summary>One aggregate request for the server-side footer
  /// calculation.</summary>
  TVittixAggRequest = record
    FieldName: string;
    AggType: TVittixAggregationType;
  end;

  /// <summary>One aggregate result. Count/NullCount mirror the client
  /// aggregation record semantics (Count counts the rows in the group,
  /// NullCount the NULL values of the field).</summary>
  TVittixAggResult = record
    FieldName: string;
    AggType: TVittixAggregationType;
    Value: Variant;
    Count: Integer;
    NullCount: Integer;
  end;

  /// <summary>A typed SQL parameter. Values never appear in the generated
  /// SQL text - only :Name placeholders do.</summary>
  TVittixSQLParam = record
    Name: string;
    Value: Variant;
    DataType: TFieldType;
  end;
  TVittixSQLParams = TArray<TVittixSQLParam>;

  /// <summary>
  /// Pure, dataset-free SQL generation for the provider. Field names are
  /// validated against the supplied field list (unknown names make the
  /// build fail, never silently pass user text into the SQL), identifiers
  /// are quoted per dialect, and all values travel as typed parameters.
  /// </summary>
  TVittixSQLBuilder = class
  public const
    /// <summary>Upper bound for the columns the global search ORs across;
    /// text columns only, first come first served.</summary>
    GLOBAL_SEARCH_MAX_COLUMNS = 20;
  private
    class function FindField(const AFields: TArray<TVittixSQLFieldInfo>;
      const AName: string; out AIndex: Integer): Boolean; static;
    class function IsTextField(AType: TFieldType): Boolean; static;
    class function IsNumericField(AType: TFieldType): Boolean; static;
    class function IsDateField(AType: TFieldType): Boolean; static;
    class function NextParamName(var ACounter: Integer): string; static;
    class function EscapeLike(const AValue: string): string; static;
    class function EscapeCharLiteral(ADialect: TVittixSQLDialect): string; static;
    class function ParseNeedleDate(AField: TVittixSQLFieldInfo;
      const AText: string; out AValue: TDateTime): Boolean; static;
    class function TryIsoDateTime(const AText: string;
      out AValue: TDateTime): Boolean; static;
    class function ParseNeedleNumber(const AText: string;
      out AValue: Double): Boolean; static;
    class function BuildCondition(ADialect: TVittixSQLDialect;
      const AField: TVittixSQLFieldInfo; const AFilterText: string;
      var ACounter: Integer; out ASQL: string;
      out AParams: TVittixSQLParams; out AReason: string): Boolean; static;
    class function TextOpSQL(ADialect: TVittixSQLDialect;
      const AColumn, AParamName: string; AMode: TVittixFilterMatchMode;
      AEscape: Boolean; ANullPasses: Boolean): string; static;
  public
    /// <summary>Quotes an identifier for the dialect. Names arrive
    /// validated from the dataset's field list; embedded quote characters
    /// are doubled so nothing can break out of the quoting.</summary>
    class function QuoteIdentifier(ADialect: TVittixSQLDialect;
      const AName: string): string; static;

    /// <summary>Builds the WHERE fragment for the filter groups plus the
    /// global search. Returns False with ANotExpressible set when any
    /// condition depends on client display-text semantics (formatted
    /// numbers, display formats of date/number columns) or references an
    /// unknown field - the caller then falls back to the client engine.
    /// AGlobalSearch ORs across the text columns (up to
    /// GLOBAL_SEARCH_MAX_COLUMNS) and replaces the group result, matching
    /// the client semantics.</summary>
    class function BuildWhere(ADialect: TVittixSQLDialect;
      const AFields: TArray<TVittixSQLFieldInfo>;
      const AGroups: TVittixFilterConditionGroups;
      const AGlobalSearch: string;
      out AWhere: string; out AParams: TVittixSQLParams;
      out ANotExpressible: string): Boolean; static;

    /// <summary>Builds the ORDER BY fragment. PostgreSQL and Firebird get
    /// explicit NULLS LAST (ascending) / NULLS FIRST (descending); the
    /// other dialects use their documented default (SQLite, MySQL and SQL
    /// Server order NULLs first ascending; ANSI leaves it
    /// implementation-defined).</summary>
    class function BuildOrderBy(ADialect: TVittixSQLDialect;
      const AFields: TArray<TVittixSQLFieldInfo>;
      const ASortColumns: TArray<TVittixSortColumn>;
      out AOrderBy: string; out ANotExpressible: string): Boolean; static;
  end;

  /// <summary>Implemented by dataset-specific providers (the FireDAC unit
  /// ships one for TFDQuery). The controller calls it when FilterMode is
  /// vcmServer or vcmAuto. Every Apply* returning False means "cannot
  /// express" - the reason is available through GetLastFallbackReason and
  /// the controller falls back to the client engine.</summary>
  IVittixDataProvider = interface
    ['{8E1F2C7A-5B4D-4E63-9A2F-0C7D8E1B2A3F}']
    /// <summary>True when this provider can push filter/sort into
    /// ADataSet (right instance, wrappable SQL shape).</summary>
    function CanHandle(ADataSet: TDataSet): Boolean;

    /// <summary>Applies the filter groups + global search server-side.
    /// False = not expressible (see GetLastFallbackReason).</summary>
    function ApplyFilter(const AGroups: TVittixFilterConditionGroups;
      const AGlobalSearch: string;
      const AFields: TArray<TVittixSQLFieldInfo>): Boolean;

    /// <summary>Applies the sort server-side. False = not expressible.
    /// </summary>
    function ApplySort(const ASortColumns: TArray<TVittixSortColumn>;
      const AFields: TArray<TVittixSQLFieldInfo>): Boolean;

    /// <summary>Removes the server-side WHERE (the sort, if any, stays).
    /// </summary>
    procedure ClearFilter;

    /// <summary>Removes the server-side ORDER BY.</summary>
    procedure ClearSort;

    /// <summary>Puts the dataset's SQL back to exactly what the
    /// application had (called on controller teardown, dataset change and
    /// mode switches).</summary>
    procedure RestoreOriginal;

    /// <summary>Computes footer aggregates with one SELECT against the
    /// same WHERE as the active filter. False when it cannot (then the
    /// controller falls back to the client scan).</summary>
    function FetchAggregates(const ARequests: TArray<TVittixAggRequest>;
      out AResults: TArray<TVittixAggResult>): Boolean;

    function GetLastSQLWhere: string;
    function GetLastSQLOrderBy: string;
    function GetLastFallbackReason: string;
    /// <summary>Dialect name for diagnostics and the footer hint.</summary>
    function GetDialectName: string;
  end;

  /// <summary>Raised by nothing in this unit; kept as the shared base for
  /// provider-related errors so applications can catch generically.</summary>
  EVittixProviderError = class(Exception);

implementation

const
  LIKE_ESCAPE_CHAR = '\';

{ TVittixSQLBuilder }

class function TVittixSQLBuilder.FindField(
  const AFields: TArray<TVittixSQLFieldInfo>; const AName: string;
  out AIndex: Integer): Boolean;
var
  I: Integer;
begin
  // Case-insensitive like TDataSet.FindField
  for I := 0 to High(AFields) do
    if SameText(AFields[I].Name, AName) then
    begin
      AIndex := I;
      Exit(True);
    end;
  AIndex := -1;
  Result := False;
end;

class function TVittixSQLBuilder.IsTextField(AType: TFieldType): Boolean;
begin
  // Memo types are text for LIKE purposes (their AsString is what the
  // client matches against).
  Result := AType in [ftString, ftWideString, ftFixedChar, ftFixedWideChar,
    ftMemo, ftWideMemo, ftFmtMemo, ftGuid];
end;

class function TVittixSQLBuilder.IsNumericField(AType: TFieldType): Boolean;
begin
  Result := AType in [ftSmallint, ftInteger, ftWord, ftLongWord, ftAutoInc,
    ftLargeint, ftShortint, ftByte, ftSingle, ftFloat, ftCurrency, ftBCD,
    ftFMTBcd, ftExtended];
end;

class function TVittixSQLBuilder.IsDateField(AType: TFieldType): Boolean;
begin
  Result := AType in [ftDate, ftTime, ftDateTime, ftTimeStamp];
end;

class function TVittixSQLBuilder.NextParamName(var ACounter: Integer): string;
begin
  // 'vx' prefix keeps clear of application parameter names
  Result := 'vx' + IntToStr(ACounter);
  Inc(ACounter);
end;

class function TVittixSQLBuilder.EscapeLike(const AValue: string): string;
var
  SB: TStringBuilder;
  Ch: Char;
begin
  // %, _ and the escape character itself are escaped so user text can
  // never change the LIKE pattern's shape.
  SB := TStringBuilder.Create(Length(AValue) + 8);
  try
    for Ch in AValue do
    begin
      case Ch of
        '\', '%', '_': SB.Append(LIKE_ESCAPE_CHAR);
      end;
      SB.Append(Ch);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

class function TVittixSQLBuilder.EscapeCharLiteral(
  ADialect: TVittixSQLDialect): string;
begin
  // In a MySQL string literal a backslash escapes itself, so the ESCAPE
  // target must be written as two.
  if ADialect = sdMySQL then
    Result := '''\\'''
  else
    Result := '''\' + '''';
end;

class function TVittixSQLBuilder.ParseNeedleNumber(const AText: string;
  out AValue: Double): Boolean;
begin
  // Locale first, then invariant, mirroring the client-side numeric parse.
  Result := TryStrToFloat(Trim(AText), AValue) or
    TryStrToFloat(Trim(AText), AValue, TFormatSettings.Invariant);
end;

class function TVittixSQLBuilder.TryIsoDateTime(const AText: string;
  out AValue: TDateTime): Boolean;
var
  Parts: TArray<string>;
  TimeParts: TArray<string>;
  Y, Mo, D, H, Mi, S: Integer;
  DatePart, TimePart: TDateTime;
begin
  // Strict ISO: yyyy-mm-dd[ hh:mm[:ss]]. Anything else is declined so
  // locale text keeps falling back to the display-text operators.
  Result := False;
  Parts := AText.Split([' ']);
  if (Length(Parts) < 1) or (Length(Parts) > 2) then
    Exit;
  if Length(Parts[0]) <> 10 then
    Exit;
  DatePart := 0;
  TimePart := 0;
  if (Parts[0][5] <> '-') or (Parts[0][8] <> '-') then
    Exit;
  if (not TryStrToInt(Copy(Parts[0], 1, 4), Y)) or
     (not TryStrToInt(Copy(Parts[0], 6, 2), Mo)) or
     (not TryStrToInt(Copy(Parts[0], 9, 2), D)) then
    Exit;
  if not TryEncodeDate(Y, Mo, D, DatePart) then
    Exit;
  if Length(Parts) = 2 then
  begin
    TimeParts := Parts[1].Split([':']);
    if (Length(TimeParts) < 2) or (Length(TimeParts) > 3) then
      Exit;
    H := 0;
    Mi := 0;
    S := 0;
    if not TryStrToInt(TimeParts[0], H) or
       not TryStrToInt(TimeParts[1], Mi) then
      Exit;
    if Length(TimeParts) = 3 then
      if not TryStrToInt(TimeParts[2], S) then
        Exit;
    if not TryEncodeTime(H, Mi, S, 0, TimePart) then
      Exit;
  end;
  AValue := DatePart + TimePart;
  Result := True;
end;

class function TVittixSQLBuilder.ParseNeedleDate(
  AField: TVittixSQLFieldInfo; const AText: string;
  out AValue: TDateTime): Boolean;
var
  FS: TFormatSettings;
begin
  // Same parse family as the client engine (locale formats), plus an
  // explicit ISO pass ('2026-01-10[ hh:nn:ss]') because neither the
  // locale nor the invariant settings accept dashed ISO everywhere.
  FS := FormatSettings;
  Result := TryStrToDateTime(Trim(AText), AValue, FS) or
    TryStrToDate(Trim(AText), AValue, FS);
  if not Result and (AField.DataType = ftTime) then
    Result := TryStrToTime(Trim(AText), AValue, FS);
  if not Result then
    Result := TryIsoDateTime(Trim(AText), AValue);
end;

class function TVittixSQLBuilder.QuoteIdentifier(
  ADialect: TVittixSQLDialect; const AName: string): string;
begin
  case ADialect of
    sdMySQL:
      Result := '`' + StringReplace(AName, '`', '``', [rfReplaceAll]) + '`';
    sdSQLServer:
      Result := '[' + StringReplace(AName, ']', ']]', [rfReplaceAll]) + ']';
  else
    // ANSI, Firebird, SQLite, PostgreSQL use double quotes; embedded
    // quotes double. The names come from the dataset's own field list, so
    // for Firebird/PostgreSQL they carry exactly the case the server
    // returned - quoting them is safe.
    Result := '"' + StringReplace(AName, '"', '""', [rfReplaceAll]) + '"';
  end;
end;

class function TVittixSQLBuilder.TextOpSQL(ADialect: TVittixSQLDialect;
  const AColumn, AParamName: string; AMode: TVittixFilterMatchMode;
  AEscape: Boolean; ANullPasses: Boolean): string;
var
  Col, Esc: string;
begin
  Col := 'UPPER(' + AColumn + ')';
  Esc := '';
  if AEscape then
    Esc := ' ESCAPE ' + EscapeCharLiteral(ADialect);

  // ANullPasses marks operators where the client matches a NULL field
  // (its display text is ''): SQL NULL comparisons yield NULL, so the
  // NULL case is added explicitly to keep server and client equivalent.
  case AMode of
    vfmContains:        Result := Col + ' LIKE UPPER(:' + AParamName + ')' + Esc;
    vfmStartsWith:      Result := Col + ' LIKE UPPER(:' + AParamName + ')' + Esc;
    vfmEndsWith:        Result := Col + ' LIKE UPPER(:' + AParamName + ')' + Esc;
    vfmEquals:          Result := Col + ' = UPPER(:' + AParamName + ')';
    vfmNotEquals:       Result := Col + ' <> UPPER(:' + AParamName + ')';
    vfmDoesNotContain:  Result := Col + ' NOT LIKE UPPER(:' + AParamName + ')' + Esc;
  else
    Result := '';
  end;

  if (Result <> '') and ANullPasses then
    Result := '(' + AColumn + ' IS NULL OR ' + Result + ')';

end;

class function TVittixSQLBuilder.BuildCondition(ADialect: TVittixSQLDialect;
  const AField: TVittixSQLFieldInfo; const AFilterText: string;
  var ACounter: Integer; out ASQL: string; out AParams: TVittixSQLParams;
  out AReason: string): Boolean;
var
  OperatorIndex: Integer;
  Mode: TVittixFilterMatchMode;
  Needle: string;
  Col: string;
  ParamName: string;
  NumValue, LowValue, HighValue: Double;
  DateValue, DateHigh: TDateTime;
  Parts: TArray<string>;
  TextParam: string;
  BoolValue: Boolean;

  procedure AddParam(const AName: string; const AValue: Variant;
    ADataType: TFieldType);
  begin
    SetLength(AParams, Length(AParams) + 1);
    AParams[High(AParams)].Name := AName;
    AParams[High(AParams)].Value := AValue;
    AParams[High(AParams)].DataType := ADataType;
  end;

begin
  Result := False;
  ASQL := '';
  AReason := '';
  SetLength(AParams, 0);

  Col := QuoteIdentifier(ADialect, AField.Name);

  if VittixFilterTryParseOperatorText(AFilterText, OperatorIndex, Needle) then
    Mode := VittixFilterOperatorDefinition(OperatorIndex).Mode
  else
  begin
    Mode := vfmContains;
    Needle := Trim(AFilterText);
  end;

  // Word operators carry no value
  if Mode in [vfmIsNull, vfmIsNotNull] then
  begin
    if Mode = vfmIsNull then
      ASQL := Col + ' IS NULL'
    else
      ASQL := Col + ' IS NOT NULL';
    Exit(True);
  end;

  if Mode in [vfmIsEmpty, vfmIsNotEmpty] then
  begin
    // Client: IsEmpty = (not null) and (display text = ''). Only text
    // fields have a '' display that maps to '' storage reliably.
    if not IsTextField(AField.DataType) then
    begin
      AReason := 'empty/not-empty on a non-text column depends on the ' +
        'display format';
      Exit(False);
    end;
    if Mode = vfmIsEmpty then
      ASQL := '(' + Col + ' IS NOT NULL AND ' + Col + ' = '''')'
    else
      ASQL := '(' + Col + ' IS NULL OR ' + Col + ' <> '''')';
    Exit(True);
  end;

  // Boolean columns
  if AField.DataType = ftBoolean then
  begin
    if not (Mode in [vfmEquals, vfmNotEquals]) then
    begin
      AReason := 'operator on a boolean column would match display text';
      Exit(False);
    end;
    if SameText(Needle, 'True') then
      BoolValue := True
    else if SameText(Needle, 'False') then
      BoolValue := False
    else
    begin
      // Any other text matches nothing on the client either; express it
      // as an impossible predicate instead of falling back.
      ASQL := '1 = 0';
      Exit(True);
    end;
    ParamName := NextParamName(ACounter);
    AddParam(ParamName, BoolValue, ftBoolean);
    if Mode = vfmEquals then
      ASQL := Col + ' = :' + ParamName
    else
      ASQL := '(' + Col + ' IS NULL OR ' + Col + ' <> :' + ParamName + ')';
    Exit(True);
  end;

  // Ordered / between comparisons
  if Mode in [vfmGreaterThan, vfmGreaterOrEqual, vfmLessThan, vfmLessOrEqual,
    vfmBetween, vfmNotBetween] then
  begin
    // The client compares the DISPLAY TEXT numerically and rejects the
    // record when either side does not parse; on non-numeric columns that
    // display-text arithmetic has no SQL equivalent.
    if not IsNumericField(AField.DataType) then
    begin
      AReason := 'ordered comparison on a non-numeric column matches ' +
        'formatted display text';
      Exit(False);
    end;
    if Mode in [vfmBetween, vfmNotBetween] then
    begin
      Parts := Needle.Split(['|']);
      if (Length(Parts) <> 2) or
         not ParseNeedleNumber(Parts[0], LowValue) or
         not ParseNeedleNumber(Parts[1], HighValue) then
      begin
        AReason := 'between bounds are not numeric';
        Exit(False);
      end;
      ParamName := NextParamName(ACounter);
      AddParam(ParamName, LowValue, ftFloat);
      ASQL := Col + ' >= :' + ParamName;
      ParamName := NextParamName(ACounter);
      AddParam(ParamName, HighValue, ftFloat);
      ASQL := ASQL + ' AND ' + Col + ' <= :' + ParamName;
      if Mode = vfmNotBetween then
        ASQL := 'NOT (' + ASQL + ')';
      Exit(True);
    end;
    if not ParseNeedleNumber(Needle, NumValue) then
    begin
      AReason := 'comparison value is not numeric';
      Exit(False);
    end;
    ParamName := NextParamName(ACounter);
    AddParam(ParamName, NumValue, ftFloat);
    case Mode of
      vfmGreaterThan:    ASQL := Col + ' > :' + ParamName;
      vfmGreaterOrEqual: ASQL := Col + ' >= :' + ParamName;
      vfmLessThan:       ASQL := Col + ' < :' + ParamName;
      vfmLessOrEqual:    ASQL := Col + ' <= :' + ParamName;
    end;
    Exit(True);
  end;

  // Date columns with typed comparisons
  if IsDateField(AField.DataType) and
     (Mode in [vfmEquals, vfmNotEquals]) then
  begin
    if not ParseNeedleDate(AField, Needle, DateValue) then
    begin
      AReason := 'date value does not parse as a date';
      Exit(False);
    end;
    ParamName := NextParamName(ACounter);
    AddParam(ParamName, DateValue, AField.DataType);
    if Mode = vfmEquals then
      ASQL := Col + ' = :' + ParamName
    else
      ASQL := Col + ' <> :' + ParamName;
    Exit(True);
  end;

  // Text semantics for everything else (including text ops on non-text
  // columns, which the client resolves against the display format)
  if not IsTextField(AField.DataType) then
  begin
    AReason := 'operator matches the client display text of a ' +
      'non-text column';
    Exit(False);
  end;

  case Mode of
    vfmEquals:
      begin
        ParamName := NextParamName(ACounter);
        AddParam(ParamName, Needle, ftString);
        ASQL := TextOpSQL(ADialect, Col, ParamName, vfmEquals, False, False);
      end;
    vfmNotEquals:
      begin
        // COALESCE mirrors the client's '' display text for NULLs
        ParamName := NextParamName(ACounter);
        AddParam(ParamName, Needle, ftString);
        ASQL := 'UPPER(COALESCE(' + Col + ', '''')) <> UPPER(:' +
          ParamName + ')';
      end;
    vfmContains, vfmStartsWith, vfmEndsWith, vfmDoesNotContain:
      begin
        ParamName := NextParamName(ACounter);
        case Mode of
          vfmContains:   TextParam := '%' + EscapeLike(Needle) + '%';
          vfmStartsWith: TextParam := EscapeLike(Needle) + '%';
          vfmEndsWith:   TextParam := '%' + EscapeLike(Needle);
        else
          TextParam := '%' + EscapeLike(Needle) + '%';
        end;
        AddParam(ParamName, TextParam, ftString);
        // NULL passes for Contains/StartsWith/EndsWith? Client: display ''
        // does not contain x -> excluded. NULL NOT LIKE -> NULL ->
        // excluded by SQL too. Only DoesNotContain flips (client matches
        // the empty display), so only that one gets the NULL branch.
        ASQL := TextOpSQL(ADialect, Col, ParamName, Mode, True,
          Mode = vfmDoesNotContain);
      end;
  else
    AReason := 'operator has no SQL equivalent';
    Exit(False);
  end;
  Result := True;
end;

class function TVittixSQLBuilder.BuildWhere(ADialect: TVittixSQLDialect;
  const AFields: TArray<TVittixSQLFieldInfo>;
  const AGroups: TVittixFilterConditionGroups;
  const AGlobalSearch: string;
  out AWhere: string; out AParams: TVittixSQLParams;
  out ANotExpressible: string): Boolean;
var
  G, C, I, ParamCounter: Integer;
  CondSQL: string;
  CondParams: TVittixSQLParams;
  GroupSQL: TStringList;
  GroupIndex: Integer;
  FieldIndex: Integer;
  SearchCol: string;
  SearchParam: string;
  SearchParts: TStringList;
  Field: TVittixSQLFieldInfo;
begin
  Result := False;
  AWhere := '';
  SetLength(AParams, 0);
  ANotExpressible := '';
  ParamCounter := 0;

  GroupSQL := TStringList.Create;
  SearchParts := TStringList.Create;
  try
    // Groups: AND within, OR between. Groups with no surviving condition
    // are dropped (mirrors the client compile step).
    for G := 0 to High(AGroups) do
    begin
      GroupIndex := -1;
      for C := 0 to High(AGroups[G]) do
      begin
        if Trim(AGroups[G][C].FilterText) = '' then
          Continue;
        if not FindField(AFields, AGroups[G][C].FieldName, FieldIndex) then
        begin
          ANotExpressible := 'unknown field "' + AGroups[G][C].FieldName + '"';
          Exit(False);
        end;
        Field := AFields[FieldIndex];
        if not BuildCondition(ADialect, Field, AGroups[G][C].FilterText,
          ParamCounter, CondSQL, CondParams, ANotExpressible) then
          Exit(False);
        for I := 0 to High(CondParams) do
        begin
          SetLength(AParams, Length(AParams) + 1);
          AParams[High(AParams)] := CondParams[I];
        end;
        if GroupIndex < 0 then
        begin
          GroupSQL.Add('');
          GroupIndex := GroupSQL.Count - 1;
        end;
        if GroupSQL[GroupIndex] <> '' then
          GroupSQL[GroupIndex] := GroupSQL[GroupIndex] + ' AND ';
        GroupSQL[GroupIndex] := GroupSQL[GroupIndex] + CondSQL;
      end;
    end;

    // Drop empty groups
    I := 0;
    while I < GroupSQL.Count do
      if GroupSQL[I] = '' then
        GroupSQL.Delete(I)
      else
        Inc(I);

    // Global search: OR across the text columns (capped), replacing the
    // group result - the same "replaces" semantics the client path uses.
    if AGlobalSearch <> '' then
    begin
      C := 0;
      for I := 0 to High(AFields) do
      begin
        if C >= GLOBAL_SEARCH_MAX_COLUMNS then
          Break;
        if not IsTextField(AFields[I].DataType) then
          Continue;
        SearchParam := NextParamName(ParamCounter);
        SetLength(AParams, Length(AParams) + 1);
        AParams[High(AParams)].Name := SearchParam;
        AParams[High(AParams)].Value := '%' + EscapeLike(AGlobalSearch) + '%';
        AParams[High(AParams)].DataType := ftString;
        SearchCol := 'UPPER(' + QuoteIdentifier(ADialect, AFields[I].Name) +
          ')';
        SearchParts.Add(SearchCol + ' LIKE UPPER(:' + SearchParam +
          ') ESCAPE ' + EscapeCharLiteral(ADialect));
        Inc(C);
      end;
      if SearchParts.Count > 0 then
      begin
        Result := True;
        AWhere := '(';
        for I := 0 to SearchParts.Count - 1 do
        begin
          if I > 0 then
            AWhere := AWhere + ' OR ';
          AWhere := AWhere + SearchParts[I];
        end;
        AWhere := AWhere + ')';
        Exit;
      end
      else
      begin
        ANotExpressible := 'global search has no text columns to search';
        Exit(False);
      end;
    end;

    if GroupSQL.Count = 0 then
    begin
      // No conditions at all: empty WHERE (caller clears instead)
      Result := True;
      AWhere := '';
      Exit;
    end;

    if GroupSQL.Count = 1 then
      AWhere := GroupSQL[0]
    else
    begin
      AWhere := '';
      for I := 0 to GroupSQL.Count - 1 do
      begin
        if I > 0 then
          AWhere := AWhere + ' OR ';
        AWhere := AWhere + '(' + GroupSQL[I] + ')';
      end;
    end;
    Result := True;
  finally
    SearchParts.Free;
    GroupSQL.Free;
  end;
end;

class function TVittixSQLBuilder.BuildOrderBy(ADialect: TVittixSQLDialect;
  const AFields: TArray<TVittixSQLFieldInfo>;
  const ASortColumns: TArray<TVittixSortColumn>;
  out AOrderBy: string; out ANotExpressible: string): Boolean;
var
  I, FieldIndex: Integer;
  Col: string;
begin
  Result := False;
  AOrderBy := '';
  ANotExpressible := '';
  if Length(ASortColumns) = 0 then
  begin
    Result := True;
    Exit;
  end;

  for I := 0 to High(ASortColumns) do
  begin
    if not FindField(AFields, ASortColumns[I].FieldName, FieldIndex) then
    begin
      ANotExpressible := 'unknown sort field "' +
        ASortColumns[I].FieldName + '"';
      Exit(False);
    end;
    Col := QuoteIdentifier(ADialect, ASortColumns[I].FieldName);
    if I > 0 then
      AOrderBy := AOrderBy + ', ';
    AOrderBy := AOrderBy + Col;
    if ASortColumns[I].Descending then
      AOrderBy := AOrderBy + ' DESC'
    else
      AOrderBy := AOrderBy + ' ASC';
    // Explicit NULL placement where the dialect supports it: ascending
    // sorts put NULLs last, descending ones first (both dialects'
    // defaults differ, so pin them).
    if ADialect in [sdPostgreSQL, sdFirebird] then
    begin
      if ASortColumns[I].Descending then
        AOrderBy := AOrderBy + ' NULLS FIRST'
      else
        AOrderBy := AOrderBy + ' NULLS LAST';
    end;
  end;
  Result := True;
end;

end.
