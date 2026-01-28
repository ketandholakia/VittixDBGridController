// Recommendations:

// Add more comprehensive error handling

// Consider thread safety if applicable

// Add unit tests for edge cases

// Consider adding filter change notifications

// Document the AND/OR logic behavior clearly






unit Vittix.DBGrid.Filter.Engine;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Dialogs,
  Data.DB,
  Vittix.DBGrid.ColumnInfo;

type
  /// <summary>
  /// Internal cache record to map a column info directly to a TField.
  /// Prevents slow FindField lookups during record iteration.
  /// </summary>
  TFilterFieldMap = record
    Info: TVittixDBGridColumnInfo;
    Field: TField;
  end;

  // NEW: Filter validation event
  TFilterValidationEvent = procedure(
    Sender: TObject;
    const FieldName: string;
    const FilterText: string;
    var IsValid: Boolean;
    var ErrorMessage: string
  ) of object;

  /// <summary>
  /// Logic-only dataset filtering engine.
  /// Uses OnFilterRecord and supports:
  /// - Per-column filters (AND)
  /// - Global search (OR)
  /// Also exposes AcceptCurrentRecord for aggregation/footer.
  /// </summary>
  TVittixDBGridFilterEngine = class
  private
    FDataSet: TDataSet;
    FColumns: TVittixDBGridColumns;

    FGlobalSearchText: string;
    FActive: Boolean;

    FOldOnFilterRecord: TFilterRecordEvent;
    FFilterInstalled: Boolean;
    FUpdating: Boolean;

    // Performance optimization: Cache fields instead of looking them up every row
    FFieldCache: TArray<TFilterFieldMap>;
    FOnValidateFilter: TFilterValidationEvent;

    procedure SetActive(const Value: Boolean);
    procedure DoFilterRecord(DataSet: TDataSet; var Accept: Boolean);

    function InternalAcceptRecord(DataSet: TDataSet): Boolean;
    function MatchText(const SearchUpper, ValueUpper: string): Boolean;
    function GetFieldDisplayText(AField: TField): string;

    procedure RebuildFieldCache;
    
    function ValidateFilterText(
      const FieldName: string;
      const FilterText: string
    ): Boolean;

  public
    constructor Create(ADataSet: TDataSet; AColumns: TVittixDBGridColumns);
    destructor Destroy; override;

    procedure ApplyFilter;
    procedure ClearFilter;
    procedure Clear;

    /// <summary>
    /// Returns TRUE if the CURRENT dataset record
    /// passes all active filters.
    /// Used by aggregation engine.
    /// </summary>
    function AcceptCurrentRecord: Boolean;

    property Active: Boolean read FActive write SetActive;
    property GlobalSearchText: string read FGlobalSearchText write FGlobalSearchText;

    property OnValidateFilter: TFilterValidationEvent 
      read FOnValidateFilter write FOnValidateFilter;
  end;

implementation

{ TVittixDBGridFilterEngine }

constructor TVittixDBGridFilterEngine.Create(
  ADataSet: TDataSet;
  AColumns: TVittixDBGridColumns);
begin
  inherited Create;
  FDataSet := ADataSet;
  FColumns := AColumns;
  FActive := False;
  FGlobalSearchText := '';
  FOldOnFilterRecord := nil;
  FFilterInstalled := False;
  FUpdating := False;
end;

destructor TVittixDBGridFilterEngine.Destroy;
begin
  ClearFilter;
  inherited Destroy;
end;

procedure TVittixDBGridFilterEngine.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;
  if FUpdating then Exit;

  FUpdating := True;
  try
    FActive := Value;

    if FActive then
      ApplyFilter
    else
      ClearFilter;
  finally
    FUpdating := False;
  end;
end;

procedure TVittixDBGridFilterEngine.RebuildFieldCache;
var
  I: Integer;
  Field: TField;
begin
  SetLength(FFieldCache, 0);

  if not Assigned(FDataSet) or not Assigned(FColumns) then
    Exit;

  // Pre-fetch TField references.
  // This is done ONCE when filter is applied, not per record.
  for I := 0 to FColumns.Count - 1 do
  begin
    Field := FDataSet.FindField(FColumns[I].FieldName);
    if Assigned(Field) then
    begin
      SetLength(FFieldCache, Length(FFieldCache) + 1);
      FFieldCache[High(FFieldCache)].Info := FColumns[I];
      FFieldCache[High(FFieldCache)].Field := Field;
    end;
  end;
end;

function TVittixDBGridFilterEngine.ValidateFilterText(
  const FieldName: string;
  const FilterText: string): Boolean;
var
  ErrMsg: string;
begin
  Result := True;
  ErrMsg := '';
  
  if Assigned(FOnValidateFilter) then
  begin
    FOnValidateFilter(Self, FieldName, FilterText, Result, ErrMsg);
    
    if not Result then
    begin
      ShowMessage('Invalid filter: ' + ErrMsg);
    end;
  end;
end;

procedure TVittixDBGridFilterEngine.ApplyFilter;
var
  I: Integer;
begin
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;

  // NEW: Validate all filters before applying
  for I := 0 to FColumns.Count - 1 do
  begin
    if FColumns[I].HasFilter then
    begin
      if not ValidateFilterText(
        FColumns[I].FieldName, 
        FColumns[I].FilterText) then
      begin
        Exit; // Don't apply invalid filters
      end;
    end;
  end;

  // 1. Rebuild cache before enabling filter to ensure field pointers are fresh
  RebuildFieldCache;

  // 2. Install the hook if not already done
  if not FFilterInstalled then
  begin
    FOldOnFilterRecord := FDataSet.OnFilterRecord;
    FDataSet.OnFilterRecord := DoFilterRecord;
    FFilterInstalled := True;
  end;

  FDataSet.DisableControls;
  try
    FDataSet.Filtered := True;
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TVittixDBGridFilterEngine.ClearFilter;
begin
  if not Assigned(FDataSet) then Exit;

  FDataSet.DisableControls;
  try
    if FFilterInstalled then
    begin
      FDataSet.Filtered := False;
      
      // Restore the user's original event handler
      FDataSet.OnFilterRecord := FOldOnFilterRecord;
      FOldOnFilterRecord := nil;
      
      FFilterInstalled := False;
    end;
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TVittixDBGridFilterEngine.Clear;
var
  I: Integer;
begin
  FGlobalSearchText := '';
  for I := 0 to FColumns.Count - 1 do
  begin
    FColumns[I].FilterText := '';
    FColumns[I].HasFilter := False;
  end;
  Active := False;
end;

procedure TVittixDBGridFilterEngine.DoFilterRecord(
  DataSet: TDataSet; var Accept: Boolean);
begin
  // 1. Run our internal filter logic first
  Accept := InternalAcceptRecord(DataSet);

  // 2. If our filter passes, AND the user had their own filter event, check that too.
  // This allows your grid filter to coexist with developer code.
  if Accept and Assigned(FOldOnFilterRecord) then
    FOldOnFilterRecord(DataSet, Accept);
end;

function TVittixDBGridFilterEngine.AcceptCurrentRecord: Boolean;
begin
  if not Assigned(FDataSet) or not FActive then
    Exit(True);

  Result := InternalAcceptRecord(FDataSet);
end;

function TVittixDBGridFilterEngine.InternalAcceptRecord(
  DataSet: TDataSet): Boolean;
var
  I: Integer;
  ValueUpper: string;
  SearchUpper: string;
  GlobalMatched: Boolean;
begin
  Result := True;

  if not FActive then Exit;

  // SAFETY: Check if cache is populated
  if Length(FFieldCache) = 0 then Exit;

  // We iterate the CACHE, not the Columns collection.
  // This avoids calling FindField hundreds of times.

  // ------------------------------------------------
  // Per-column filters (AND)
  // ------------------------------------------------
  for I := 0 to High(FFieldCache) do
  begin
    // Check if this cached column has a filter active
    if FFieldCache[I].Info.HasFilter and (FFieldCache[I].Info.FilterText <> '') then
    begin
      SearchUpper := UpperCase(FFieldCache[I].Info.FilterText);
      ValueUpper := UpperCase(GetFieldDisplayText(FFieldCache[I].Field));

      if not MatchText(SearchUpper, ValueUpper) then
        Exit(False); // Failed an AND condition
    end;
  end;

  // ------------------------------------------------
  // Global search (OR)
  // ------------------------------------------------
  if FGlobalSearchText <> '' then
  begin
    SearchUpper := UpperCase(FGlobalSearchText);
    GlobalMatched := False;

    for I := 0 to High(FFieldCache) do
    begin
      ValueUpper := UpperCase(GetFieldDisplayText(FFieldCache[I].Field));
      if MatchText(SearchUpper, ValueUpper) then
      begin
        GlobalMatched := True;
        Break; // Found a match in one column, so the row is valid
      end;
    end;

    Result := GlobalMatched;
  end;
end;

function TVittixDBGridFilterEngine.MatchText(
  const SearchUpper, ValueUpper: string): Boolean;
begin
  Result := (SearchUpper = '') or (Pos(SearchUpper, ValueUpper) > 0);
end;

function TVittixDBGridFilterEngine.GetFieldDisplayText(
  AField: TField): string;
begin
  if not Assigned(AField) then Exit('');

  case AField.DataType of
    ftMemo, ftWideMemo, ftFmtMemo:
      Result := AField.AsString;
  else
    Result := AField.DisplayText;
  end;
end;

end.