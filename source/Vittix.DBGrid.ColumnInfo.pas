unit Vittix.DBGrid.ColumnInfo;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  System.Math,
  System.JSON,
  Vcl.Graphics;

type
  TVittixCellConditionOperator = (
    vccoEquals,
    vccoNotEquals,
    vccoContains,
    vccoStartsWith,
    vccoEndsWith,
    vccoGreaterThan,
    vccoGreaterOrEqual,
    vccoLessThan,
    vccoLessOrEqual
  );

  TVittixDBGridCellCondition = class(TCollectionItem)
  private
    FFieldName: string;
    FEnabled: Boolean;
    FOperator: TVittixCellConditionOperator;
    FValue: string;
    FBackgroundColor: TColor;
    FFontColor: TColor;
  protected
    function GetDisplayName: string; override;
  published
    property FieldName: string read FFieldName write FFieldName;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property OperatorKind: TVittixCellConditionOperator read FOperator write FOperator default vccoEquals;
    property Value: string read FValue write FValue;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor default clNone;
    property FontColor: TColor read FFontColor write FFontColor default clNone;
  public
    constructor Create(Collection: TCollection); override;
    function Matches(const AValue: string): Boolean;
  end;

  TVittixDBGridCellConditions = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TVittixDBGridCellCondition;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TVittixDBGridCellCondition;
    property Items[Index: Integer]: TVittixDBGridCellCondition read GetItem; default;
  end;

  // ------------------------------------------------------------
  // Sorting
  // ------------------------------------------------------------
  TVittixSortOrder = (vsoNone, vsoAsc, vsoDesc);

  // ------------------------------------------------------------
  // Per-column in-place editor configuration
  // ------------------------------------------------------------
  /// <summary>Custom popup editor opened by F2/double click. veNone keeps
  /// the built-in memo/date behavior (and no editor for other field
  /// types).</summary>
  TVittixEditorKind = (veNone, vePickList, veLookup, veMaskedNumber);

  // ------------------------------------------------------------
  // Footer aggregation types
  // ------------------------------------------------------------
  TVittixAggregationType = (
    vatNone,
    vatCount,
    vatSum,
    vatAvg,
    vatMin,
    vatMax
  );

  // ------------------------------------------------------------
  // Aggregation runtime storage
  // ------------------------------------------------------------
  TVittixAggregation = record
    Count: Integer;
    NullCount: Integer;

    HasInt: Boolean;
    SumInt, MinInt, MaxInt: Int64;

    HasFloat: Boolean;
    SumFloat, MinFloat, MaxFloat: Double;

    HasCurrency: Boolean;
    SumCurrency, MinCurrency, MaxCurrency: Currency;

    // Date/time Min/Max (vatMin/vatMax only)
    HasDate: Boolean;
    MinDate, MaxDate: TDateTime;

    HasString: Boolean;
    MinString, MaxString: string;

    // BCD accumulated in Currency (exact decimal arithmetic) while every
    // value and the running sum fit the Currency range. BCDAsFloat switches
    // the accumulator to the Double path after an overflow; BCDConverted
    // records that the already-accumulated BCD sum was folded in once.
    HasBCD: Boolean;
    SumBCD, MinBCD, MaxBCD: Currency;
    BCDAsFloat: Boolean;
    BCDConverted: Boolean;

    procedure Clear;
    function GetValue(AType: TVittixAggregationType): Variant;
  end;

  // ------------------------------------------------------------
  // Per-column metadata (DESIGN-TIME)
  // ------------------------------------------------------------
  TVittixDBGridColumnInfo = class(TCollectionItem)
  private
    FFieldName: string;
    FSortOrder: TVittixSortOrder;
    FSortIndex: Integer;
    FFilterText: string;
    FHasFilter: Boolean;
    FAggregationType: TVittixAggregationType;
    FFooterText: string;
    FCellConditions: TVittixDBGridCellConditions;
    // Layout schema v2 column settings
    FPinned: Boolean;
    FTitleCaption: string;
    FDisplayFormat: string;
    FExportInclude: Boolean;
    FExportCaption: string;
    // Per-column editor configuration (v2 optional fields)
    FEditorKind: TVittixEditorKind;
    FPickItems: string;
    FHasMin: Boolean;
    FMinValue: Double;
    FHasMax: Boolean;
    FMaxValue: Double;
    FRequired: Boolean;
    FDecimalPlaces: Integer;
    // Runtime-only accumulator state (not streamed)
    FAggregation: TVittixAggregation;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;

    // Standard VCL method to copy settings between objects
    procedure Assign(Source: TPersistent); override;
  published
    property FieldName: string
      read FFieldName write FFieldName;

    // Runtime-only state: not streamed into the DFM (old DFMs that already
    // contain these properties still load them).
    property SortOrder: TVittixSortOrder
      read FSortOrder write FSortOrder stored False default vsoNone;

    property SortIndex: Integer
      read FSortIndex write FSortIndex stored False default -1;

    property FilterText: string
      read FFilterText write FFilterText stored False;

    property HasFilter: Boolean
      read FHasFilter write FHasFilter stored False default False;

    property AggregationType: TVittixAggregationType
      read FAggregationType write FAggregationType default vatNone;

    property FooterText: string
      read FFooterText write FFooterText;
    property CellConditions: TVittixDBGridCellConditions read FCellConditions;

    /// <summary>Column belongs to the frozen (fixed left) region. The grid
    /// derives its FrozenColumns count from the leading run of pinned
    /// visible columns.</summary>
    property Pinned: Boolean
      read FPinned write FPinned default False;

    /// <summary>Custom title caption applied to the grid column by
    /// ApplyLayout. Empty leaves the column's own caption untouched.</summary>
    property TitleCaption: string
      read FTitleCaption write FTitleCaption;

    /// <summary>Custom DisplayFormat applied to the backing field by
    /// ApplyLayout (numeric and datetime fields only). Empty leaves the
    /// field format untouched.</summary>
    property DisplayFormat: string
      read FDisplayFormat write FDisplayFormat;

    /// <summary>When False the column is excluded from every export format
    /// regardless of visibility settings.</summary>
    property ExportInclude: Boolean
      read FExportInclude write FExportInclude default True;

    /// <summary>Custom header caption used by the export engine and the
    /// clipboard copy. Empty falls back to the column title caption.</summary>
    property ExportCaption: string
      read FExportCaption write FExportCaption;

    /// <summary>Custom popup editor opened by F2/double click. Default
    /// veNone keeps the built-in memo/date behavior unchanged.</summary>
    property EditorKind: TVittixEditorKind
      read FEditorKind write FEditorKind default veNone;

    /// <summary>Items for vePickList, one per line. "value=caption" pairs
    /// write the value to integer fields; plain lines write the text to
    /// string fields.</summary>
    property PickItems: string
      read FPickItems write FPickItems;

    /// <summary>Lower bound for veMaskedNumber (and the built-in
    /// validation); only applied when HasMin is True.</summary>
    property HasMin: Boolean
      read FHasMin write FHasMin default False;
    property MinValue: Double
      read FMinValue write FMinValue;

    /// <summary>Upper bound for veMaskedNumber (and the built-in
    /// validation); only applied when HasMax is True.</summary>
    property HasMax: Boolean
      read FHasMax write FHasMax default False;
    property MaxValue: Double
      read FMaxValue write FMaxValue;

    /// <summary>Built-in validation: the editor rejects an empty/null
    /// value while Required is True.</summary>
    property Required: Boolean
      read FRequired write FRequired default False;

    /// <summary>Decimal places enforced by veMaskedNumber; -1 (default)
    /// leaves the typed value unchanged.</summary>
    property DecimalPlaces: Integer
      read FDecimalPlaces write FDecimalPlaces default -1;
  public
    // Runtime aggregation results (direct field access keeps record member
    // calls like Aggregation.Clear mutating the backing field)
    property Aggregation: TVittixAggregation
      read FAggregation write FAggregation;
  end;

  // ------------------------------------------------------------
  // ColumnInfo collection
  // ------------------------------------------------------------
  TVittixDBGridColumns = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TVittixDBGridColumnInfo;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TVittixDBGridColumnInfo;

    function FindByFieldName(const AName: string): TVittixDBGridColumnInfo;

    property Items[Index: Integer]: TVittixDBGridColumnInfo
      read GetItem; default;
  end;

function CellConditionsToJson(Conditions: TVittixDBGridCellConditions): string;
procedure CellConditionsFromJson(Conditions: TVittixDBGridCellConditions; const JsonText: string);

implementation

{ TVittixDBGridCellConditions }

constructor TVittixDBGridCellConditions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TVittixDBGridCellCondition);
end;

function TVittixDBGridCellConditions.Add: TVittixDBGridCellCondition;
begin
  Result := inherited Add as TVittixDBGridCellCondition;
end;

function TVittixDBGridCellConditions.GetItem(
  Index: Integer): TVittixDBGridCellCondition;
begin
  Result := inherited Items[Index] as TVittixDBGridCellCondition;
end;

function TVittixDBGridCellCondition.Matches(const AValue: string): Boolean;
var
  L, R: Extended;
begin
  if not FEnabled then Exit(False);

  case FOperator of
    vccoEquals: Result := SameText(Trim(AValue), Trim(FValue));
    vccoNotEquals: Result := not SameText(Trim(AValue), Trim(FValue));
    vccoContains: Result := ContainsText(AValue, FValue);
    vccoStartsWith: Result := StartsText(FValue, AValue);
    vccoEndsWith: Result := EndsText(FValue, AValue);
    vccoGreaterThan,
    vccoGreaterOrEqual,
    vccoLessThan,
    vccoLessOrEqual:
      begin
        if not TryStrToFloat(Trim(AValue), L) or not TryStrToFloat(Trim(FValue), R) then
          Exit(False);
        case FOperator of
          vccoGreaterThan: Result := L > R;
          vccoGreaterOrEqual: Result := L >= R;
          vccoLessThan: Result := L < R;
          vccoLessOrEqual: Result := L <= R;
        else
          Result := False;
        end;
      end;
  else
    Result := False;
  end;
end;

const
  MAX_CURRENCY = 922337203685477.5807;

{ TVittixAggregation }

procedure TVittixAggregation.Clear;
begin
  Count := 0;
  NullCount := 0;
  HasInt := False;  
  SumInt := 0;  
  MinInt := High(Int64);  
  MaxInt := Low(Int64);
  
  HasFloat := False; 
  SumFloat := 0; 
  MinFloat := MaxDouble; 
  MaxFloat := -MaxDouble;
  
  HasCurrency := False; 
  SumCurrency := 0; 
  MinCurrency := MAX_CURRENCY; 
  MaxCurrency := -MAX_CURRENCY;
  
  HasDate := False;
  MinDate := 0;
  MaxDate := 0;

  HasString := False; 
  MinString := ''; 
  MaxString := '';

  HasBCD := False;
  SumBCD := 0;
  MinBCD := MAX_CURRENCY;
  MaxBCD := -MAX_CURRENCY;
  BCDAsFloat := False;
  BCDConverted := False;
end;

function TVittixAggregation.GetValue(AType: TVittixAggregationType): Variant;
begin
  if Count = 0 then Exit(Null);

  case AType of
    vatCount: Result := Count;
    
    vatSum:
      if HasFloat then Result := SumFloat
      else if HasBCD then Result := SumBCD
      else if HasCurrency then Result := SumCurrency
      else if HasInt then Result := SumInt
      else Result := Null;
      
    vatAvg:
      if HasFloat then Result := SumFloat / Count
      else if HasBCD then Result := SumBCD / Count
      else if HasCurrency then Result := SumCurrency / Count
      else if HasInt then Result := SumInt / Count
      else Result := Null;
      
    vatMin:
      if HasFloat then Result := MinFloat
      else if HasBCD then Result := MinBCD
      else if HasCurrency then Result := MinCurrency
      else if HasInt then Result := MinInt
      else if HasDate then Result := MinDate
      else if HasString then Result := MinString
      else Result := Null;
      
    vatMax:
      if HasFloat then Result := MaxFloat
      else if HasBCD then Result := MaxBCD
      else if HasCurrency then Result := MaxCurrency
      else if HasInt then Result := MaxInt
      else if HasDate then Result := MaxDate
      else if HasString then Result := MaxString
      else Result := Null;
  else
    Result := Null;
  end;
end;

{ TVittixDBGridColumnInfo }

constructor TVittixDBGridColumnInfo.Create(Collection: TCollection);
begin
  inherited;
  FAggregationType := vatNone;
  FFooterText := '';
  FCellConditions := TVittixDBGridCellConditions.Create(Self);
  FSortOrder := vsoNone;
  FSortIndex := -1;
  FPinned := False;
  FTitleCaption := '';
  FDisplayFormat := '';
  FExportInclude := True;
  FExportCaption := '';
  FEditorKind := veNone;
  FPickItems := '';
  FHasMin := False;
  FMinValue := 0;
  FHasMax := False;
  FMaxValue := 0;
  FRequired := False;
  FDecimalPlaces := -1;
  Aggregation.Clear;
end;

procedure TVittixDBGridColumnInfo.Assign(Source: TPersistent);
var
  Src: TVittixDBGridColumnInfo;
begin
  if Source is TVittixDBGridColumnInfo then
  begin
    Src := TVittixDBGridColumnInfo(Source);
    FFieldName := Src.FieldName;
    FSortOrder := Src.SortOrder;
    FSortIndex := Src.SortIndex;
    FFilterText := Src.FilterText;
    FHasFilter := Src.HasFilter;
    FAggregationType := Src.AggregationType;
    FFooterText := Src.FooterText;
    FCellConditions.Assign(Src.CellConditions);
    FPinned := Src.Pinned;
    FTitleCaption := Src.TitleCaption;
    FDisplayFormat := Src.DisplayFormat;
    FExportInclude := Src.ExportInclude;
    FExportCaption := Src.ExportCaption;
    FEditorKind := Src.EditorKind;
    FPickItems := Src.PickItems;
    FHasMin := Src.HasMin;
    FMinValue := Src.MinValue;
    FHasMax := Src.HasMax;
    FMaxValue := Src.MaxValue;
    FRequired := Src.Required;
    FDecimalPlaces := Src.DecimalPlaces;
    // We do NOT copy the runtime Aggregation results, only metadata
  end
  else
    inherited Assign(Source);
end;

constructor TVittixDBGridCellCondition.Create(Collection: TCollection);
begin
  inherited;
  FEnabled := True;
  FOperator := vccoEquals;
  FBackgroundColor := clNone;
  FFontColor := clNone;
end;

function TVittixDBGridCellCondition.GetDisplayName: string;
const
  OperatorNames: array[TVittixCellConditionOperator] of string = (
    'equals', 'does not equal', 'contains', 'starts with', 'ends with',
    'greater than', 'at least', 'less than', 'at most');
var
  FieldLabel: string;
begin
  FieldLabel := FFieldName;
  if FieldLabel = '' then
    FieldLabel := 'Condition ' + IntToStr(Index + 1);

  Result := FieldLabel + ' ' + OperatorNames[FOperator];
  if FValue <> '' then
    Result := Result + ' "' + FValue + '"';
  if not FEnabled then
    Result := Result + ' (disabled)';
end;

function TVittixDBGridColumnInfo.GetDisplayName: string;
begin
  Result := FFieldName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

{ TVittixDBGridColumns }

constructor TVittixDBGridColumns.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TVittixDBGridColumnInfo);
end;

function TVittixDBGridColumns.Add: TVittixDBGridColumnInfo;
begin
  Result := TVittixDBGridColumnInfo(inherited Add);
end;

function TVittixDBGridColumns.GetItem(Index: Integer): TVittixDBGridColumnInfo;
begin
  Result := TVittixDBGridColumnInfo(inherited GetItem(Index));
end;

function TVittixDBGridColumns.FindByFieldName(
  const AName: string): TVittixDBGridColumnInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if SameText(Items[I].FieldName, AName) then
      Exit(Items[I]);
  Result := nil;
end;

function CellConditionsToJson(Conditions: TVittixDBGridCellConditions): string;
var
  Arr: TJSONArray;
  Cond: TVittixDBGridCellCondition;
  Obj: TJSONObject;
  I: Integer;
begin
  Arr := TJSONArray.Create;
  try
    if Assigned(Conditions) then
      for I := 0 to Conditions.Count - 1 do
      begin
        Cond := Conditions[I];
        Obj := TJSONObject.Create;
        Obj.AddPair('fieldName', Cond.FieldName);
        Obj.AddPair('enabled', TJSONBool.Create(Cond.Enabled));
        Obj.AddPair('operatorKind', TJSONNumber.Create(Ord(Cond.OperatorKind)));
        Obj.AddPair('value', Cond.Value);
        Obj.AddPair('backgroundColor', TJSONNumber.Create(Integer(Cond.BackgroundColor)));
        Obj.AddPair('fontColor', TJSONNumber.Create(Integer(Cond.FontColor)));
        Arr.AddElement(Obj);
      end;
    Result := Arr.ToJSON;
  finally
    Arr.Free;
  end;
end;

procedure CellConditionsFromJson(Conditions: TVittixDBGridCellConditions; const JsonText: string);
var
  Arr: TJSONArray;
  I: Integer;
  Obj: TJSONObject;
  Cond: TVittixDBGridCellCondition;
begin
  if not Assigned(Conditions) then Exit;
  Conditions.Clear;
  if JsonText = '' then Exit;
  Arr := TJSONObject.ParseJSONValue(JsonText) as TJSONArray;
  try
    if not Assigned(Arr) then Exit;
    for I := 0 to Arr.Count - 1 do
    begin
      Obj := Arr.Items[I] as TJSONObject;
      Cond := Conditions.Add;
      Cond.FieldName := Obj.GetValue<string>('fieldName', '');
      Cond.Enabled := Obj.GetValue<Boolean>('enabled', True);
      Cond.OperatorKind := TVittixCellConditionOperator(Obj.GetValue<Integer>('operatorKind', 0));
      Cond.Value := Obj.GetValue<string>('value', '');
      Cond.BackgroundColor := TColor(Obj.GetValue<Integer>('backgroundColor', Integer(clNone)));
      Cond.FontColor := TColor(Obj.GetValue<Integer>('fontColor', Integer(clNone)));
    end;
  finally
    Arr.Free;
  end;
end;

end.
