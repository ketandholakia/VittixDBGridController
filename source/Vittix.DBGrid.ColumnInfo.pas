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

    HasString: Boolean;
    MinString, MaxString: string;

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
  protected
    function GetDisplayName: string; override;
  public
    Aggregation: TVittixAggregation;
    constructor Create(Collection: TCollection); override;
    
    // Standard VCL method to copy settings between objects
    procedure Assign(Source: TPersistent); override; 
  published
    property FieldName: string
      read FFieldName write FFieldName;

    property SortOrder: TVittixSortOrder
      read FSortOrder write FSortOrder default vsoNone;

    property SortIndex: Integer
      read FSortIndex write FSortIndex default -1;

    property FilterText: string
      read FFilterText write FFilterText;

    property HasFilter: Boolean
      read FHasFilter write FHasFilter default False;

    property AggregationType: TVittixAggregationType
      read FAggregationType write FAggregationType default vatNone;

    property FooterText: string
      read FFooterText write FFooterText;
    property CellConditions: TVittixDBGridCellConditions read FCellConditions;
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
  
  HasString := False; 
  MinString := ''; 
  MaxString := '';
end;

function TVittixAggregation.GetValue(AType: TVittixAggregationType): Variant;
begin
  if Count = 0 then Exit(Null);

  case AType of
    vatCount: Result := Count;
    
    vatSum:
      if HasFloat then Result := SumFloat
      else if HasCurrency then Result := SumCurrency
      else if HasInt then Result := SumInt
      else Result := Null;
      
    vatAvg:
      if HasFloat then Result := SumFloat / Count
      else if HasCurrency then Result := SumCurrency / Count
      else if HasInt then Result := SumInt / Count
      else Result := Null;
      
    vatMin:
      if HasFloat then Result := MinFloat
      else if HasCurrency then Result := MinCurrency
      else if HasInt then Result := MinInt
      else if HasString then Result := MinString
      else Result := Null;
      
    vatMax:
      if HasFloat then Result := MaxFloat
      else if HasCurrency then Result := MaxCurrency
      else if HasInt then Result := MaxInt
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
