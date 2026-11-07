unit Vittix.DBGrid.Filter.Engine;

interface

uses
  System.Classes,
  System.SysUtils,
  Data.DB,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Paging.GlobalScroll;

type
  /// <summary>
  /// Internal cache record to map a column info directly to a TField.
  /// Prevents slow FindField lookups during record iteration.
  /// </summary>
  TFilterFieldMap = record
    Info: TVittixDBGridColumnInfo;
    Field: TField;
    FilterTextSnapshot: string;
    NeedleUpper: string;
  end;

  /// <summary>One filter condition in a group: the dataset field name and
  /// the filter text in the existing operator-prefix syntax. Parsing and
  /// matching reuse the single operator table and the per-column match
  /// code — nothing is duplicated.</summary>
  TVittixFilterCondition = record
    FieldName: string;
    FilterText: string;
  end;

  /// <summary>An ordered list of filter groups. Conditions inside one group
  /// are ANDed, groups are ORed. An empty array falls back to the classic
  /// per-column AND behavior.</summary>
  TVittixFilterConditionGroups = TArray<TArray<TVittixFilterCondition>>;

  TVittixFilterMatchMode = (
    vfmContains, vfmEquals, vfmStartsWith, vfmEndsWith,
    vfmNotEquals, vfmGreaterThan, vfmGreaterOrEqual, vfmLessThan, vfmLessOrEqual,
    vfmBetween, vfmNotBetween, vfmIsNull, vfmIsNotNull, vfmIsEmpty, vfmIsNotEmpty,
    vfmDoesNotContain
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

  /// <summary>One precompiled group condition: the operator parse, folded
  /// needle and typed needle values are computed once at ApplyFilter time
  /// so DoFilterRecord never parses per record.</summary>
  TVittixCompiledFilterCondition = record
    Field: TField;
    FieldName: string;
    Mode: TVittixFilterMatchMode;
    Needle: string;
    NeedleUpper: string;
    NumNeedle: Extended;
    HasNumNeedle: Boolean;
    NumLow, NumHigh: Extended;
    HasNumRange: Boolean;
    DateNeedle, DateHigh: TDateTime;
    HasDateNeedle: Boolean;
  end;

  // Filter validation event
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
    FGlobalSearchUpper: string;
    FActive: Boolean;

    FOldOnFilterRecord: TFilterRecordEvent;
    FFilterInstalled: Boolean;
    // The dataset's Filtered value captured when the engine hook was
    // installed, restored on clear so an application's own filter state
    // (Filter string, OnFilterRecord) survives engine teardown.
    FSavedFiltered: Boolean;
    FUpdating: Boolean;

    // Performance optimization: Cache fields instead of looking them up every row
    FFieldCache: TArray<TFilterFieldMap>;
    FOnValidateFilter: TFilterValidationEvent;

    // OR-group filtering: when FFilterGroups is non-empty the classic
    // per-column AND loop is replaced by the compiled group evaluation.
    FFilterGroups: TVittixFilterConditionGroups;
    FCompiledGroups: TArray<TArray<TVittixCompiledFilterCondition>>;

    procedure SetActive(const Value: Boolean);
    procedure SetGlobalSearchText(const Value: string);
    procedure DoFilterRecord(DataSet: TDataSet; var Accept: Boolean);

    function InternalAcceptRecord(DataSet: TDataSet): Boolean;
    function MatchText(const SearchUpper, ValueUpper: string): Boolean;
    function ParseFilterMode(const FilterText: string; out Mode: TVittixFilterMatchMode;
      out Value: string): Boolean;
    function MatchFilter(const FilterText, NeedleUpper, ValueText: string;
      AField: TField): Boolean;
    function MatchCompiled(const ACond: TVittixCompiledFilterCondition;
      AField: TField; const ValueText: string): Boolean;
    procedure RebuildCompiledGroups;
    function TryParseDateText(AField: TField; const Text: string;
      out Value: TDateTime): Boolean;
    function GetFieldDisplayText(AField: TField): string;
    function TryParseNumericText(const S: string; out V: Extended): Boolean;
    function TryGetFieldNumeric(AField: TField; out V: Extended): Boolean;

    procedure RebuildFieldCache;
    procedure UpdateFieldNeedle(var AMap: TFilterFieldMap);

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

    /// <summary>Sets the OR-group filter model (conditions within a group
    /// AND, groups OR). An empty array restores the classic per-column AND
    /// behavior. Applies immediately while the engine is active.</summary>
    procedure SetFilterGroups(const Groups: TVittixFilterConditionGroups);

    /// <summary>True while OR-groups drive the engine (a filter set is
    /// applied).</summary>
    function FilterGroupsActive: Boolean;

    /// <summary>The per-column HasFilter texts as one AND-group (what the
    /// classic client path evaluates). Read-only view for the server
    /// provider routing.</summary>
    function GetColumnConditions: TVittixFilterConditionGroups;

    /// <summary>A copy of the stored OR-groups (a filter set applied via
    /// SetFilterGroups). Empty when the classic column path is active.
    /// </summary>
    function GetStoredGroups: TVittixFilterConditionGroups;

    /// <summary>
    /// Returns TRUE if the CURRENT dataset record
    /// passes all active filters.
    /// Used by aggregation engine.
    /// </summary>
    function AcceptCurrentRecord: Boolean;

    /// <summary>True while this engine's DoFilterRecord is installed as the
    /// dataset's OnFilterRecord hook. When it is (and the dataset is
    /// Filtered), iterating the dataset already yields only accepted rows.
    /// </summary>
    function FilterHookInstalled: Boolean;

    /// <summary>
    /// Drops the cached TField references and (while active on an open
    /// dataset) rebuilds them. Call after dataset close/reopen cycles that do
    /// not go through the controller, which recreates the engines instead.
    /// InternalAcceptRecord also self-checks cache validity per record.
    /// </summary>
    procedure ResetFieldCache;

    property Active: Boolean read FActive write SetActive;
    property GlobalSearchText: string read FGlobalSearchText write SetGlobalSearchText;

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
  AddFilterOperator(vfmDoesNotContain, '!',     'Does Not Contain', False);
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

procedure TVittixDBGridFilterEngine.SetGlobalSearchText(const Value: string);
begin
  FGlobalSearchText := Value;
  FGlobalSearchUpper := UpperCase(Value);
end;

procedure TVittixDBGridFilterEngine.UpdateFieldNeedle(
  var AMap: TFilterFieldMap);
var
  Mode: TVittixFilterMatchMode;
  Needle: string;
begin
  AMap.FilterTextSnapshot := AMap.Info.FilterText;
  ParseFilterMode(AMap.FilterTextSnapshot, Mode, Needle);
  AMap.NeedleUpper := UpperCase(Needle);
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
      UpdateFieldNeedle(FFieldCache[High(FFieldCache)]);
    end;
  end;

  // The compiled OR-groups reference dataset fields too; keep them in sync
  // with every cache rebuild (dataset churn invalidates both).
  if Length(FFilterGroups) > 0 then
    RebuildCompiledGroups;
end;

procedure TVittixDBGridFilterEngine.SetFilterGroups(
  const Groups: TVittixFilterConditionGroups);
begin
  FFilterGroups := Groups;
  SetLength(FCompiledGroups, 0);
  // Re-apply while active so the view updates immediately; otherwise the
  // groups compile on the next ApplyFilter (activation or re-apply).
  if FActive and Assigned(FDataSet) and FDataSet.Active then
    ApplyFilter;
end;

function TVittixDBGridFilterEngine.FilterGroupsActive: Boolean;
begin
  Result := Length(FCompiledGroups) > 0;
end;

function TVittixDBGridFilterEngine.GetColumnConditions: TVittixFilterConditionGroups;
var
  I, Count: Integer;
begin
  SetLength(Result, 1);
  SetLength(Result[0], 0);
  if not Assigned(FColumns) then
    Exit;
  Count := 0;
  for I := 0 to FColumns.Count - 1 do
    if FColumns[I].HasFilter and (Trim(FColumns[I].FilterText) <> '') then
    begin
      SetLength(Result[0], Count + 1);
      Result[0][Count].FieldName := FColumns[I].FieldName;
      Result[0][Count].FilterText := FColumns[I].FilterText;
      Inc(Count);
    end;
end;

function TVittixDBGridFilterEngine.GetStoredGroups: TVittixFilterConditionGroups;
var
  G, C: Integer;
begin
  SetLength(Result, Length(FFilterGroups));
  for G := 0 to High(FFilterGroups) do
  begin
    SetLength(Result[G], Length(FFilterGroups[G]));
    for C := 0 to High(FFilterGroups[G]) do
      Result[G][C] := FFilterGroups[G][C];
  end;
end;

function TVittixDBGridFilterEngine.TryParseDateText(AField: TField;
  const Text: string; out Value: TDateTime): Boolean;
begin
  Result := TryStrToDateTime(Trim(Text), Value, FormatSettings) or
    TryStrToDate(Trim(Text), Value, FormatSettings);
  if not Result and (AField.DataType = ftTime) then
    Result := TryStrToTime(Trim(Text), Value, FormatSettings);
end;

procedure TVittixDBGridFilterEngine.RebuildCompiledGroups;
var
  G, C, OperatorIndex: Integer;
  Cond: TVittixFilterCondition;
  Compiled: TVittixCompiledFilterCondition;
  Field: TField;
  Parts: TArray<string>;

  procedure AddCompiled;
  begin
    SetLength(FCompiledGroups[G], Length(FCompiledGroups[G]) + 1);
    FCompiledGroups[G][High(FCompiledGroups[G])] := Compiled;
  end;

begin
  SetLength(FCompiledGroups, 0);
  if (not Assigned(FDataSet)) or (Length(FFilterGroups) = 0) then
    Exit;

  // Groups are ORed, so an EMPTY group would vacuously match every record
  // and OR the whole filter away. Empty groups are dropped at compile time;
  // when every group ends up empty the array stays empty and the engine
  // falls back to the classic per-column path (no conditions -> pass all).
  SetLength(FCompiledGroups, 0);
  for G := 0 to High(FFilterGroups) do
  begin
    SetLength(FCompiledGroups, Length(FCompiledGroups) + 1);
    SetLength(FCompiledGroups[High(FCompiledGroups)], 0);
    for C := 0 to High(FFilterGroups[G]) do
    begin
      Cond := FFilterGroups[G][C];
      // Empty condition texts are pass-through no-ops in MatchFilter; skip
      // them here so the per-record evaluation keeps the same semantics.
      if Trim(Cond.FilterText) = '' then
        Continue;

      Field := FDataSet.FindField(Cond.FieldName);
      if not Assigned(Field) then
        Continue; // an unknown field can never match

      Compiled := Default(TVittixCompiledFilterCondition);
      Compiled.Field := Field;
      Compiled.FieldName := Cond.FieldName;

      if VittixFilterTryParseOperatorText(Cond.FilterText, OperatorIndex, Compiled.Needle) then
        Compiled.Mode := VittixFilterOperatorDefinition(OperatorIndex).Mode
      else
      begin
        Compiled.Mode := vfmContains;
        Compiled.Needle := Trim(Cond.FilterText);
      end;

      // Keep group and classic-column substring matching on the same
      // case-folded needle representation.
      Compiled.NeedleUpper := UpperCase(Compiled.Needle);
      Compiled.HasNumNeedle := TryParseNumericText(Compiled.Needle, Compiled.NumNeedle);

      Parts := Compiled.Needle.Split(['|']);
      if (Length(Parts) = 2) and
         TryParseNumericText(Trim(Parts[0]), Compiled.NumLow) and
         TryParseNumericText(Trim(Parts[1]), Compiled.NumHigh) then
        Compiled.HasNumRange := True;

      if (Field.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp]) and
         (Compiled.Mode in [vfmEquals, vfmNotEquals, vfmLessThan, vfmGreaterThan,
           vfmLessOrEqual, vfmGreaterOrEqual, vfmBetween, vfmNotBetween]) then
      begin
        if Compiled.Mode in [vfmBetween, vfmNotBetween] then
        begin
          if (Length(Parts) = 2) and
             TryParseDateText(Field, Parts[0], Compiled.DateNeedle) and
             TryParseDateText(Field, Parts[1], Compiled.DateHigh) then
            Compiled.HasDateNeedle := True;
        end
        else if TryParseDateText(Field, Compiled.Needle, Compiled.DateNeedle) then
          Compiled.HasDateNeedle := True;
      end;

      AddCompiled;
    end;

    // Drop the group when none of its conditions compiled to anything.
    if Length(FCompiledGroups[High(FCompiledGroups)]) = 0 then
      SetLength(FCompiledGroups, Length(FCompiledGroups) - 1);
  end;
end;

function TVittixDBGridFilterEngine.MatchCompiled(
  const ACond: TVittixCompiledFilterCondition; AField: TField;
  const ValueText: string): Boolean;
var
  Hay: string;
  VN: Extended;
  StartPos: Integer;
  FieldIsNull: Boolean;
  DateValue: TDateTime;
begin
  Hay := Trim(ValueText);
  FieldIsNull := Assigned(AField) and AField.IsNull;

  // Typed date comparisons with the needle parsed at apply time; an
  // unparseable date text leaves HasDateNeedle False and falls into the
  // display-text operators, exactly like MatchFilter.
  if ACond.HasDateNeedle then
  begin
    // A missing date is not a comparable value, including for inequality.
    if FieldIsNull then
      Exit(False);
    DateValue := AField.AsDateTime;
    case ACond.Mode of
      vfmEquals: Result := DateValue = ACond.DateNeedle;
      vfmNotEquals: Result := DateValue <> ACond.DateNeedle;
      vfmLessThan: Result := DateValue < ACond.DateNeedle;
      vfmGreaterThan: Result := DateValue > ACond.DateNeedle;
      vfmLessOrEqual: Result := DateValue <= ACond.DateNeedle;
      vfmGreaterOrEqual: Result := DateValue >= ACond.DateNeedle;
      vfmBetween: Result := (DateValue >= ACond.DateNeedle) and (DateValue <= ACond.DateHigh);
      vfmNotBetween: Result := (DateValue < ACond.DateNeedle) or (DateValue > ACond.DateHigh);
    else
      Result := False;
    end;
    Exit;
  end;

  case ACond.Mode of
    vfmContains: Result := Pos(ACond.NeedleUpper, Hay) > 0;
    vfmEquals: Result := SameText(ACond.Needle, Hay);
    vfmStartsWith: Result := SameText(Copy(Hay, 1, Length(ACond.Needle)), ACond.Needle);
    vfmEndsWith:
      begin
        StartPos := Length(Hay) - Length(ACond.Needle) + 1;
        // A needle longer than the haystack can never be a suffix of it.
        if StartPos < 1 then
          Exit(False);
        Result := SameText(Copy(Hay, StartPos, MaxInt), ACond.Needle);
      end;
    vfmNotEquals: Result := not SameText(ACond.Needle, Hay);
    vfmDoesNotContain: Result := Pos(ACond.NeedleUpper, Hay) = 0;
    vfmGreaterThan,
    vfmGreaterOrEqual,
    vfmLessThan,
    vfmLessOrEqual:
      begin
        if ACond.HasNumNeedle and TryGetFieldNumeric(AField, VN) then
          case ACond.Mode of
            vfmGreaterThan: Result := VN > ACond.NumNeedle;
            vfmGreaterOrEqual: Result := VN >= ACond.NumNeedle;
            vfmLessThan: Result := VN < ACond.NumNeedle;
            vfmLessOrEqual: Result := VN <= ACond.NumNeedle;
          else
            Result := False;
          end
        else
          Result := False;
      end;
    vfmBetween:
      Result := ACond.HasNumRange and TryGetFieldNumeric(AField, VN) and
        (VN >= ACond.NumLow) and (VN <= ACond.NumHigh);
    vfmNotBetween:
      Result := ACond.HasNumRange and TryGetFieldNumeric(AField, VN) and
        ((VN < ACond.NumLow) or (VN > ACond.NumHigh));
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

    // Engines must never call ShowMessage or any VCL UI directly.
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
  I, G, C: Integer;
  HasConditions: Boolean;
  Paged: IInterface;
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

  // For page-backed datasets (global page scrolling) an apply without any
  // condition must not set Filtered := True: it would hijack the
  // application's Filtered flag for a no-op pass, and virtual mode
  // legitimately treats an active Filtered as "not global" (positions
  // would be meaningless). Verified in the demo self test: dataset churn
  // (close + reopen under a bound grid) ran RefreshFiltering in client
  // mode, which engaged the engine without conditions and silently
  // flipped Filtered on the bound paged TClientDataSet. Datasets without
  // global page scrolling keep the shipped behavior unchanged.
  if Supports(FDataSet, IVittixGlobalPageScroll) then
  begin
    HasConditions := Trim(FGlobalSearchText) <> '';
    if not HasConditions then
      for G := 0 to High(FFilterGroups) do
        for C := 0 to High(FFilterGroups[G]) do
          if Trim(FFilterGroups[G][C].FilterText) <> '' then
            HasConditions := True;
    if not HasConditions then
      for I := 0 to FColumns.Count - 1 do
        if FColumns[I].HasFilter and (Trim(FColumns[I].FilterText) <> '') then
          HasConditions := True;
    if not HasConditions then
      Exit;
  end;

  // Group conditions go through the same validation
  for G := 0 to High(FFilterGroups) do
    for C := 0 to High(FFilterGroups[G]) do
      ValidateFilterText(FFilterGroups[G][C].FieldName, FFilterGroups[G][C].FilterText);

  // 1. Rebuild cache before enabling filter to ensure field pointers are fresh
  RebuildFieldCache;

  // 2. Install the hook if not already done
  if not FFilterInstalled then
  begin
    FOldOnFilterRecord := FDataSet.OnFilterRecord;
    FSavedFiltered := FDataSet.Filtered;
    FDataSet.OnFilterRecord := DoFilterRecord;
    FFilterInstalled := True;
  end;

  FDataSet.DisableControls;
  try
    if FDataSet.Filtered then
    begin
      // Already filtered (the application's own filter): assigning True
      // again is a no-op and would never re-evaluate the records against
      // the new filter. Toggle to force a fresh pass.
      FDataSet.Filtered := False;
      FDataSet.Filtered := True;
    end
    else
      FDataSet.Filtered := True;
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TVittixDBGridFilterEngine.ClearFilter;
var
  CurrentHandler, OurHandler: TFilterRecordEvent;
begin
  if not Assigned(FDataSet) then Exit;

  FDataSet.DisableControls;
  try
    if FFilterInstalled then
    begin
      // Force a re-evaluation pass: assigning the same Filtered value the
      // dataset already has would be a no-op and would leave the engine's
      // filtered view on screen.
      FDataSet.Filtered := False;

      // Restore the user's original event handler...
        CurrentHandler := FDataSet.OnFilterRecord;
        OurHandler := DoFilterRecord;
        // A later application assignment owns the event from that point on.
        if (TMethod(CurrentHandler).Code = TMethod(OurHandler).Code) and
           (TMethod(CurrentHandler).Data = TMethod(OurHandler).Data) then
          FDataSet.OnFilterRecord := FOldOnFilterRecord;
      FOldOnFilterRecord := nil;
      FFilterInstalled := False;

      // ...then restore the Filtered value the dataset had before the
      // engine hook was installed: an application that was already
      // filtering (its own Filter string or OnFilterRecord) keeps
      // filtering after the engine's filter is cleared.
      FDataSet.Filtered := FSavedFiltered;
    end;
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TVittixDBGridFilterEngine.Clear;
var
  I: Integer;
begin
  SetGlobalSearchText('');
  // Clearing all filters also removes any applied filter set
  SetLength(FFilterGroups, 0);
  SetLength(FCompiledGroups, 0);
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

function TVittixDBGridFilterEngine.FilterHookInstalled: Boolean;
begin
  Result := FFilterInstalled;
end;

function TVittixDBGridFilterEngine.InternalAcceptRecord(
  DataSet: TDataSet): Boolean;
var
  I, G, C: Integer;
  ValueUpper: string;
  ValueCache: TArray<string>;
  GlobalMatched: Boolean;
  GroupMatched: Boolean;
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

  // STALE-GROUP GUARD: the compiled group conditions hold TField pointers
  // with the same lifetime as the field cache.
  for G := 0 to High(FCompiledGroups) do
    for C := 0 to High(FCompiledGroups[G]) do
      if (FCompiledGroups[G][C].Field = nil) or
         (FCompiledGroups[G][C].Field.DataSet <> DataSet) then
      begin
        RebuildFieldCache;
        Break;
      end;

  // We iterate the CACHE, not the Columns collection.
  // This avoids calling FindField hundreds of times.

  // ------------------------------------------------
  // OR-group filters (filter sets): conditions AND within a group,
  // groups OR. An empty group is ignored; when every group ends up empty
  // everything passes, like a filter with no active conditions.
  // ------------------------------------------------
  if Length(FCompiledGroups) > 0 then
  begin
    if FGlobalSearchText <> '' then
    begin
      // Same semantics as the legacy path: the global search decides on
      // its own (an OR across all columns that replaces the group result).
      Result := False;
    end
    else
    begin
      // Group evaluation: conditions AND within a group, groups OR. An
      // empty group never reaches here (dropped at compile time).
      for G := 0 to High(FCompiledGroups) do
      begin
        GroupMatched := True;
        for C := 0 to High(FCompiledGroups[G]) do
        begin
          ValueUpper := UpperCase(GetFieldDisplayText(FCompiledGroups[G][C].Field));
          if not MatchCompiled(FCompiledGroups[G][C],
            FCompiledGroups[G][C].Field, ValueUpper) then
          begin
            GroupMatched := False;
            Break; // failed an AND condition inside this group
          end;
        end;
        if GroupMatched then
          Exit(True);
      end;
      Exit(False);
    end;
  end
  else
  begin
    Result := True;

    // ------------------------------------------------
    // Per-column filters (AND)
    // ------------------------------------------------
    for I := 0 to High(FFieldCache) do
    begin
      // Check if this cached column has a filter active
      if FFieldCache[I].Info.HasFilter and (FFieldCache[I].Info.FilterText <> '') then
      begin
        // Direct metadata edits can happen while filtering is active. Fold
        // their new needle once without requiring an explicit ApplyFilter.
        if FFieldCache[I].FilterTextSnapshot <> FFieldCache[I].Info.FilterText then
          UpdateFieldNeedle(FFieldCache[I]);
        ValueUpper := UpperCase(GetFieldDisplayText(FFieldCache[I].Field));
        if FGlobalSearchUpper <> '' then
        begin
          if Length(ValueCache) = 0 then
            SetLength(ValueCache, Length(FFieldCache));
          ValueCache[I] := ValueUpper;
        end;

        if not MatchFilter(FFieldCache[I].Info.FilterText,
          FFieldCache[I].NeedleUpper, ValueUpper, FFieldCache[I].Field) then
          Exit(False); // Failed an AND condition
      end;
    end;

    if FGlobalSearchText = '' then
      Exit(Result);
  end;

  // ------------------------------------------------
  // Global search (OR)
  // ------------------------------------------------
  GlobalMatched := False;
  for I := 0 to High(FFieldCache) do
  begin
    if (Length(ValueCache) > I) and (ValueCache[I] <> '') then
      ValueUpper := ValueCache[I]
    else
      ValueUpper := UpperCase(GetFieldDisplayText(FFieldCache[I].Field));
    if MatchText(FGlobalSearchUpper, ValueUpper) then
    begin
      GlobalMatched := True;
      Break; // Found a match in one column, so the row is valid
    end;
  end;

  Result := GlobalMatched;
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
  const FilterText, NeedleUpper, ValueText: string; AField: TField): Boolean;
var
  Mode: TVittixFilterMatchMode;
  Needle, Hay, LowText, HighText: string;
  FN, VN: Extended;
  StartPos: Integer;
  Parts: TArray<string>;
  HighVal: Extended;
  FieldIsNull: Boolean;
  DateNeedle, DateHigh, DateValue: TDateTime;
  ParsedDate: Boolean;
begin
  if Trim(FilterText) = '' then
    Exit(True);

  ParseFilterMode(FilterText, Mode, Needle);
  Hay := Trim(ValueText);

  // NULL and empty are distinct: NULL means no value at all, empty means a
  // blank (but present) value. Only the field itself can tell them apart.
  FieldIsNull := Assigned(AField) and AField.IsNull;

  if Assigned(AField) and
     (AField.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp]) and
     (Mode in [vfmEquals, vfmNotEquals, vfmLessThan, vfmGreaterThan,
       vfmLessOrEqual, vfmGreaterOrEqual, vfmBetween, vfmNotBetween]) then
  begin
    if Mode in [vfmBetween, vfmNotBetween] then
    begin
      Parts := Needle.Split(['|']);
      ParsedDate := (Length(Parts) = 2) and
        TryParseDateText(AField, Parts[0], DateNeedle) and TryParseDateText(AField, Parts[1], DateHigh);
    end
    else
      ParsedDate := TryParseDateText(AField, Needle, DateNeedle);
    if ParsedDate then
    begin
      // A missing date is not a comparable value, including for inequality.
      if FieldIsNull then Exit(False);
      DateValue := AField.AsDateTime;
      case Mode of
        vfmEquals: Result := DateValue = DateNeedle;
        vfmNotEquals: Result := DateValue <> DateNeedle;
        vfmLessThan: Result := DateValue < DateNeedle;
        vfmGreaterThan: Result := DateValue > DateNeedle;
        vfmLessOrEqual: Result := DateValue <= DateNeedle;
        vfmGreaterOrEqual: Result := DateValue >= DateNeedle;
        vfmBetween: Result := (DateValue >= DateNeedle) and (DateValue <= DateHigh);
        vfmNotBetween: Result := (DateValue < DateNeedle) or (DateValue > DateHigh);
      else
        Result := False;
      end;
      Exit;
    end;
    // Unparseable date text retains the existing display-text operator behavior.
  end;

  case Mode of
    vfmContains: Result := Pos(NeedleUpper, Hay) > 0;
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
    // Not Equals is an exact-value comparison: it matches records whose
    // display text differs from the needle as a whole. Does Not Contain
    // (vfmDoesNotContain, the '!' prefix) is the substring exclusion and
    // keeps the historic Pos-based behaviour of the old vfmNotEquals.
    vfmNotEquals: Result := not SameText(Needle, Hay);
    vfmDoesNotContain: Result := Pos(NeedleUpper, Hay) = 0;
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
