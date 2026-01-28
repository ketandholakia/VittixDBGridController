unit Vittix.DBGrid.ColumnInfo;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.Math;

type
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

implementation

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
    // We do NOT copy the runtime Aggregation results, only metadata
  end
  else
    inherited Assign(Source);
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

end.