unit Vittix.DBGrid.Filter.Engine;

interface

uses
  System.Classes,
  System.SysUtils,
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

  TVittixFilterMatchMode = (
    vfmContains, vfmEquals, vfmStartsWith, vfmEndsWith,
    vfmNotEquals, vfmGreaterThan, vfmGreaterOrEqual, vfmLessThan, vfmLessOrEqual,
    vfmBetween, vfmNotBetween, vfmIsNull, vfmIsNotNull, vfmIsEmpty, vfmIsNotEmpty
  );

  /// <summary>
  /// Single authoritative description of one filter operator: the DSL prefix,
  /// the popup display caption, the match mode it selects, and whether it is
  /// a "word" operator. Word operators (the null/empty family) match only
  /// when they are the ENTIRE filter text and take no value, which keeps
  /// inputs like "nullity" parsing as a plain Contains filter.
  /// The definition order is the filter popup's combo order and MUST stay
  /// stable: the persisted operator memory stores combo indexes.
  /// </summary>
  TVittixFilterOperatorDefinition = record
    Mode: TVittixFilterMatchMode;
    Prefix: string;
    DisplayName: string;
    IsWordOperator: Boolean;
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
    function ParseFilterMode(const FilterText: string; out Mode: TVittixFilterMatchMode;
      out Value: string): Boolean;
    function MatchFilter(const FilterText, ValueText: string; AField: TField): Boolean;
    function GetFieldDisplayText(AField: TField): string;
    function TryParseNumericText(const S: string; out V: Extended): Boolean;
    function TryGetFieldNumeric(AField: TField; out V: Extended): Boolean;

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

    /// <summary>
    /// Drops the cached TField references and (while active on an open
    /// dataset) rebuilds them. Call after dataset close/reopen cycles that do
    /// not go through the controller, which recreates the engines instead.
    /// InternalAcceptRecord also self-checks cache validity per record.
    /// </summary>
    procedure ResetFieldCache;

    property Active: Boolean read FActive write SetActive;
    property GlobalSearchText: string read FGlobalSearchText write FGlobalSearchText;

    property OnValidateFilter: TFilterValidationEvent
      read FOnValidateFilter write FOnValidateFilter;
  end;

/// Number of defined filter operators (equals the popup combo item count).
function VittixFilterOperatorCount: Integer;
/// <summary>Definition by operator index (popup combo order). Out-of-range
/// indexes return the default Contains definition.</summary>
function VittixFilterOperatorDefinition(Index: Integer): TVittixFilterOperatorDefinition;
/// <summary>Operator index for an exact prefix match; 0 (Contains) when the
/// prefix is unknown.</summary>
function VittixFilterOperatorIndexByPrefix(const APrefix: string): Integer;
/// <summary>Splits filter text into operator index + value using the shared
/// operator table. Returns False when the text carries no operator prefix
/// (plain Contains; AValue receives the trimmed text). Word operators consume
/// the whole text (AValue = ''). Longer prefixes win over shorter ones
/// ('&gt;=' before '&gt;', '!..' before '!').</summary>
function VittixFilterTryParseOperatorText(const AText: string;
  out AOperatorIndex: Integer; out AValue: string): Boolean;

implementation

{ =============================================================================
  SHARED FILTER OPERATOR TABLE
  One authoritative definition of the operator prefix DSL. The engine parses
  filter text through it, and the popup builds its combo, generates prefixes
  and restores persisted text through it — the three copies that used to be
  kept in sync by hand are gone.
  ============================================================================= }

var
  // Built once in the initialization section; treated as read-only after.
  GFilterOperators: TArray<TVittixFilterOperatorDefinition>;
  GFilterOperatorParseOrder: TArray<Integer>;

procedure AddFilterOperator(AMode: TVittixFilterMatchMode;
  const APrefix, ADisplayName: string; AWordOperator: Boolean);
begin
  SetLength(GFilterOperators, Length(GFilterOperators) + 1);
  GFilterOperators[High(GFilterOperators)].Mode := AMode;
  GFilterOperators[High(GFilterOperators)].Prefix := APrefix;
  GFilterOperators[High(GFilterOperators)].DisplayName := ADisplayName;
  GFilterOperators[High(GFilterOperators)].IsWordOperator := AWordOperator;
end;

procedure BuildFilterOperators;
begin
  // Order = popup combo order = persisted OperatorIndex values. Do not
  // reorder or insert; only append is safe for forward compatibility.
  AddFilterOperator(vfmContains,       '',      'Contains',         False);
  AddFilterOperator(vfmEquals,         '=',     'Equals',           False);
  AddFilterOperator(vfmStartsWith,     '^',     'Starts With',      False);
  AddFilterOperator(vfmEndsWith,       '$',     'Ends With',        False);
  AddFilterOperator(vfmNotEquals,      '!',     'Does Not Contain', False);
  AddFilterOperator(vfmNotEquals,      '<>',    'Not Equals',       False);
  AddFilterOperator(vfmGreaterThan,    '>',     'Greater Than',     False);
  AddFilterOperator(vfmGreaterOrEqual, '>=',    'Greater or Equal', False);
  AddFilterOperator(vfmLessThan,       '<',     'Less Than',        False);
  AddFilterOperator(vfmLessOrEqual,    '<=',    'Less or Equal',    False);
  AddFilterOperator(vfmBetween,        '..',    'Between',          False);
  AddFilterOperator(vfmNotBetween,     '!..',   'Not Between',      False);
  AddFilterOperator(vfmIsNull,         'null',  'Is Null',          True);
  AddFilterOperator(vfmIsNotNull,      '!null', 'Is Not Null',      True);
  AddFilterOperator(vfmIsEmpty,        'empty', 'Is Empty',         True);
  AddFilterOperator(vfmIsNotEmpty,     '!empty','Is Not Empty',     True);
end;

procedure BuildFilterOperatorParseOrder;
var
  I, J, Key: Integer;
begin
  // Longest prefix first so '>=', '<=', '<>' and '!..' win over the 1-char
  // prefixes they start with. Word operators match the whole text only, so
  // they cannot collide with shorter prefixes ('=null' still parses as
  // Equals with value 'null'); the stable sort keeps their order
  // deterministic anyway.
  SetLength(GFilterOperatorParseOrder, Length(GFilterOperators));
  for I := 0 to High(GFilterOperators) do
    GFilterOperatorParseOrder[I] := I;

  for I := 1 to High(GFilterOperatorParseOrder) do
  begin
    Key := GFilterOperatorParseOrder[I];
    J := I - 1;
    while (J >= 0) and
          (Length(GFilterOperators[GFilterOperatorParseOrder[J]].Prefix) <
           Length(GFilterOperators[Key].Prefix)) do
    begin
      GFilterOperatorParseOrder[J + 1] := GFilterOperatorParseOrder[J];
      Dec(J);
    end;
    GFilterOperatorParseOrder[J + 1] := Key;
  end;
end;

function VittixFilterOperatorCount: Integer;
begin
  Result := Length(GFilterOperators);
end;

function VittixFilterOperatorDefinition(
  Index: Integer): TVittixFilterOperatorDefinition;
begin
  if (Index < 0) or (Index >= Length(GFilterOperators)) then
  begin
    Result.Mode := vfmContains;
    Result.Prefix := '';
    Result.DisplayName := '';
    Result.IsWordOperator := False;
    Exit;
  end;
  Result := GFilterOperators[Index];
end;

function VittixFilterOperatorIndexByPrefix(const APrefix: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(GFilterOperators) do
    if GFilterOperators[I].Prefix = APrefix then
      Exit(I);
  Result := 0;
end;

function VittixFilterTryParseOperatorText(const AText: string;
  out AOperatorIndex: Integer; out AValue: string): Boolean;
var
  Text: string;
  I, DefIndex: Integer;
  Def: TVittixFilterOperatorDefinition;
begin
  Result := False;
  AOperatorIndex := 0;
  Text := Trim(AText);
  AValue := Text;
  if Text = '' then
    Exit(False);

  for I := 0 to High(GFilterOperatorParseOrder) do
  begin
    DefIndex := GFilterOperatorParseOrder[I];
    Def := GFilterOperators[DefIndex];
    if Def.Prefix = '' then
      Continue; // Contains is the fallback, never a prefix match

    if Def.IsWordOperator then
    begin
      // Length-delimited: the WHOLE text must equal the prefix, so inputs
      // like 'nullity' or '!nullable' never parse as null operators.
      if Text = Def.Prefix then
      begin
        Result := True;
        AOperatorIndex := DefIndex;
        AValue := '';
        Exit;
      end;
    end
    else if Copy(Text, 1, Length(Def.Prefix)) = Def.Prefix then
    begin
      Result := True;
      AOperatorIndex := DefIndex;
      AValue := Trim(Copy(Text, Length(Def.Prefix) + 1, MaxInt));
      Exit;
    end;
  end;
end;

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
    if Value then
    begin
      FActive := True;
      try
        ApplyFilter;
      except
        FActive := False;
        ClearFilter;
        raise;
      end;
    end
    else
    begin
      ClearFilter;
      FActive := False;
    end;
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

    // FIX BUG 5: Engines must never call ShowMessage or any VCL UI directly.
    // This violated the separation of concerns the architecture is built on.
    // Raise an exception instead so the calling UI layer can catch and display
    // the error however it chooses (MessageDlg, status bar, inline label, etc.)
    if not Result then
      raise Exception.CreateFmt(
        'Invalid filter for field "%s": %s', [FieldName, ErrMsg]);
  end;
end;

procedure TVittixDBGridFilterEngine.ApplyFilter;
var
  I: Integer;
begin
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;
  if not Assigned(FColumns) then Exit;

  // Validate all filters before applying; raises on invalid input
  for I := 0 to FColumns.Count - 1 do
  begin
    if FColumns[I].HasFilter then
    begin
      try
        ValidateFilterText(FColumns[I].FieldName, FColumns[I].FilterText);
      except
        on E: Exception do
        begin
          // Surface validation error to caller; do not apply broken filter
          raise;
        end;
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
  if Assigned(FColumns) then
    for I := 0 to FColumns.Count - 1 do
    begin
      FColumns[I].FilterText := '';
      FColumns[I].HasFilter := False;
    end;
  Active := False;
end;

procedure TVittixDBGridFilterEngine.ResetFieldCache;
begin
  SetLength(FFieldCache, 0);
  if FActive and Assigned(FDataSet) and FDataSet.Active then
    RebuildFieldCache;
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
  GlobalMatched: Boolean;
begin
  Result := True;

  if not FActive then Exit;

  // SAFETY: Check if cache is populated
  if Length(FFieldCache) = 0 then Exit;

  // STALE-CACHE GUARD: if the dataset was closed/reopened or its fields were
  // recreated since the cache was built, the cached TField pointers dangle.
  // A cached field whose DataSet link no longer matches triggers one rebuild.
  for I := 0 to High(FFieldCache) do
    if (FFieldCache[I].Field = nil) or (FFieldCache[I].Field.DataSet <> DataSet) then
    begin
      RebuildFieldCache;
      if Length(FFieldCache) = 0 then
        Exit(True);
      Break;
    end;

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
      ValueUpper := UpperCase(GetFieldDisplayText(FFieldCache[I].Field));

      if not MatchFilter(FFieldCache[I].Info.FilterText, ValueUpper, FFieldCache[I].Field) then
        Exit(False); // Failed an AND condition
    end;
  end;

  // ------------------------------------------------
  // Global search (OR)
  // ------------------------------------------------
  if FGlobalSearchText <> '' then
  begin
    GlobalMatched := False;

    for I := 0 to High(FFieldCache) do
    begin
      ValueUpper := UpperCase(GetFieldDisplayText(FFieldCache[I].Field));
      if MatchText(UpperCase(FGlobalSearchText), ValueUpper) then
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

function TVittixDBGridFilterEngine.ParseFilterMode(const FilterText: string;
  out Mode: TVittixFilterMatchMode; out Value: string): Boolean;
var
  OperatorIndex: Integer;
begin
  // The shared operator table owns prefix recognition; this method only maps
  // the parsed operator to the engine's match mode. Unrecognized text is a
  // plain Contains filter (Value = trimmed text), so parsing never fails.
  if VittixFilterTryParseOperatorText(FilterText, OperatorIndex, Value) then
    Mode := VittixFilterOperatorDefinition(OperatorIndex).Mode
  else
    Mode := vfmContains;
  Result := True;
end;

function TVittixDBGridFilterEngine.TryParseNumericText(const S: string;
  out V: Extended): Boolean;
var
  Cleaned: string;
  Ch: Char;
begin
  Result := TryStrToFloat(Trim(S), V);
  if Result then Exit;

  // Display text can carry grouping separators, currency symbols and
  // spaces ("$1,234.56"). Strip them so formatted values still compare
  // numerically instead of silently failing the parse (and the record).
  Cleaned := '';
  for Ch in Trim(S) do
    if (Ch <> FormatSettings.ThousandSeparator) and (Ch <> ' ') and (Ch <> #160) and
       (Pos(Ch, FormatSettings.CurrencyString) = 0) then
      Cleaned := Cleaned + Ch;
  Result := (Cleaned <> '') and TryStrToFloat(Cleaned, V);
end;

function TVittixDBGridFilterEngine.TryGetFieldNumeric(AField: TField;
  out V: Extended): Boolean;
begin
  Result := False;
  V := 0;
  if not Assigned(AField) or AField.IsNull then Exit;

  case AField.DataType of
    ftSmallint, ftInteger, ftWord, ftLongWord, ftAutoInc, ftLargeint,
    ftShortint, ftByte, ftSingle, ftFloat, ftCurrency, ftBCD, ftFMTBcd,
    ftExtended:
      begin
        V := AField.AsFloat;
        Result := True;
      end;
  else
    // Textual fields: compare through their (possibly formatted) display text.
    Result := TryParseNumericText(GetFieldDisplayText(AField), V);
  end;
end;

function TVittixDBGridFilterEngine.MatchFilter(
  const FilterText, ValueText: string; AField: TField): Boolean;
var
  Mode: TVittixFilterMatchMode;
  Needle, Hay, LowText, HighText: string;
  FN, VN: Extended;
  StartPos: Integer;
  Parts: TArray<string>;
  HighVal: Extended;
  FieldIsNull: Boolean;
begin
  if Trim(FilterText) = '' then
    Exit(True);

  ParseFilterMode(FilterText, Mode, Needle);
  Hay := Trim(ValueText);

  // NULL and empty are distinct: NULL means no value at all, empty means a
  // blank (but present) value. Only the field itself can tell them apart.
  FieldIsNull := Assigned(AField) and AField.IsNull;

  case Mode of
    vfmContains: Result := Pos(UpperCase(Needle), UpperCase(Hay)) > 0;
    vfmEquals: Result := SameText(Needle, Hay);
    vfmStartsWith: Result := SameText(Copy(Hay, 1, Length(Needle)), Needle);
    vfmEndsWith:
      begin
        StartPos := Length(Hay) - Length(Needle) + 1;
        // A needle longer than the haystack can never be a suffix of it.
        if StartPos < 1 then
          Exit(False);
        Result := SameText(Copy(Hay, StartPos, MaxInt), Needle);
      end;
    vfmNotEquals: Result := Pos(UpperCase(Needle), UpperCase(Hay)) = 0;
    vfmGreaterThan,
    vfmGreaterOrEqual,
    vfmLessThan,
    vfmLessOrEqual,
    vfmBetween,
    vfmNotBetween:
      begin
        if Mode = vfmBetween then
        begin
          if Pos('|', Needle) = 0 then
            Exit(False);

          Parts := Needle.Split(['|']);
          if Length(Parts) <> 2 then
            Exit(False);

          LowText := Trim(Parts[0]);
          HighText := Trim(Parts[1]);
          if TryParseNumericText(LowText, FN) and TryGetFieldNumeric(AField, VN) and
             TryParseNumericText(HighText, HighVal) then
            Result := (VN >= FN) and (VN <= HighVal)
          else
            Result := False;
          Exit;
        end;

        if Mode = vfmNotBetween then
        begin
          if Pos('|', Needle) = 0 then
            Exit(False);

          Parts := Needle.Split(['|']);
          if Length(Parts) <> 2 then
            Exit(False);

          LowText := Trim(Parts[0]);
          HighText := Trim(Parts[1]);
          if TryParseNumericText(LowText, FN) and TryGetFieldNumeric(AField, VN) and
             TryParseNumericText(HighText, HighVal) then
            Result := (VN < FN) or (VN > HighVal)
          else
            Result := False;
          Exit;
        end;

        if TryParseNumericText(Needle, FN) and TryGetFieldNumeric(AField, VN) then
          case Mode of
            vfmGreaterThan: Result := VN > FN;
            vfmGreaterOrEqual: Result := VN >= FN;
            vfmLessThan: Result := VN < FN;
            vfmLessOrEqual: Result := VN <= FN;
          else
            Result := False;
          end
        else
          Result := False;
      end;
    vfmIsNull:
      Result := FieldIsNull;
    vfmIsNotNull:
      Result := not FieldIsNull;
    vfmIsEmpty:
      Result := (not FieldIsNull) and (Hay = '');
    vfmIsNotEmpty:
      Result := FieldIsNull or (Hay <> '');
  else
    Result := False;
  end;
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

initialization
  BuildFilterOperators;
  BuildFilterOperatorParseOrder;

end.
