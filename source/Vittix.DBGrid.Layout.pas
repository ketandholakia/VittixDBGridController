unit Vittix.DBGrid.Layout;

interface

  uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  Vcl.Graphics,
  Vittix.DBGrid.ColumnInfo;

type
  TVittixDBGridLayoutColumnState = record
    FieldName: string;
    DisplayIndex: Integer;
    Width: Integer;
    Visible: Boolean;
    SortOrder: TVittixSortOrder;
    SortIndex: Integer;
    AggregationType: TVittixAggregationType;
    FooterText: string;
    CellConditionsJson: string;
  end;

  TVittixDBGridLayoutState = class
  private
    FVersion: Integer;
    FFooterVisible: Boolean;
    FAlternatingRowColors: Boolean;
    FAlternateRowColor: TColor;
    FColumns: TList<TVittixDBGridLayoutColumnState>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Version: Integer read FVersion write FVersion;
    property FooterVisible: Boolean read FFooterVisible write FFooterVisible;
    property AlternatingRowColors: Boolean read FAlternatingRowColors write FAlternatingRowColors;
    property AlternateRowColor: TColor read FAlternateRowColor write FAlternateRowColor;
    property Columns: TList<TVittixDBGridLayoutColumnState> read FColumns;
  end;

  IVittixDBGridLayoutStorage = interface
    ['{77F1E9DF-3A3F-4D3E-8C2B-1A7FE40D38C5}']
    procedure SaveToStream(const State: TVittixDBGridLayoutState; Stream: TStream);
    function LoadFromStream(Stream: TStream): TVittixDBGridLayoutState;
  end;

  TVittixDBGridLayoutJsonStorage = class(TInterfacedObject, IVittixDBGridLayoutStorage)
  public
    procedure SaveToStream(const State: TVittixDBGridLayoutState; Stream: TStream);
    function LoadFromStream(Stream: TStream): TVittixDBGridLayoutState;
    class procedure SaveToFile(const State: TVittixDBGridLayoutState; const FileName: string = '');
    class function LoadFromFile(const FileName: string = ''): TVittixDBGridLayoutState;
  end;

function SortOrderToString(Value: TVittixSortOrder): string;
function StringToSortOrder(const Value: string): TVittixSortOrder;
function AggregationTypeToString(Value: TVittixAggregationType): string;
function StringToAggregationType(const Value: string): TVittixAggregationType;

implementation

function SortOrderToString(Value: TVittixSortOrder): string;
begin
  case Value of
    vsoAsc: Result := 'Asc';
    vsoDesc: Result := 'Desc';
  else
    Result := 'None';
  end;
end;

function StringToSortOrder(const Value: string): TVittixSortOrder;
begin
  if SameText(Value, 'Asc') then Exit(vsoAsc);
  if SameText(Value, 'Desc') then Exit(vsoDesc);
  Result := vsoNone;
end;

function AggregationTypeToString(Value: TVittixAggregationType): string;
begin
  case Value of
    vatCount: Result := 'Count';
    vatSum: Result := 'Sum';
    vatAvg: Result := 'Avg';
    vatMin: Result := 'Min';
    vatMax: Result := 'Max';
  else
    Result := 'None';
  end;
end;

function StringToAggregationType(const Value: string): TVittixAggregationType;
begin
  if SameText(Value, 'Count') then Exit(vatCount);
  if SameText(Value, 'Sum') then Exit(vatSum);
  if SameText(Value, 'Avg') then Exit(vatAvg);
  if SameText(Value, 'Min') then Exit(vatMin);
  if SameText(Value, 'Max') then Exit(vatMax);
  Result := vatNone;
end;

constructor TVittixDBGridLayoutState.Create;
begin
  inherited Create;
  FColumns := TList<TVittixDBGridLayoutColumnState>.Create;
  FVersion := 1;
  FFooterVisible := True;
  FAlternatingRowColors := True;
  FAlternateRowColor := $00F7F7F7;
end;

destructor TVittixDBGridLayoutState.Destroy;
begin
  FColumns.Free;
  inherited;
end;

procedure TVittixDBGridLayoutState.Clear;
begin
  FColumns.Clear;
  FVersion := 1;
  FFooterVisible := True;
  FAlternatingRowColors := True;
  FAlternateRowColor := $00F7F7F7;
end;

procedure TVittixDBGridLayoutJsonStorage.SaveToStream(
  const State: TVittixDBGridLayoutState; Stream: TStream);
var
  Root: TJSONObject;
  Columns: TJSONArray;
  ColumnObj: TJSONObject;
  Col: TVittixDBGridLayoutColumnState;
  Bytes: TBytes;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('version', TJSONNumber.Create(State.Version));
    Root.AddPair('footerVisible', TJSONBool.Create(State.FooterVisible));
    Root.AddPair('alternatingRowColors', TJSONBool.Create(State.AlternatingRowColors));
    Root.AddPair('alternateRowColor', TJSONNumber.Create(State.AlternateRowColor));

    Columns := TJSONArray.Create;
    for Col in State.Columns do
    begin
      ColumnObj := TJSONObject.Create;
      ColumnObj.AddPair('fieldName', Col.FieldName);
      ColumnObj.AddPair('displayIndex', TJSONNumber.Create(Col.DisplayIndex));
      ColumnObj.AddPair('width', TJSONNumber.Create(Col.Width));
      ColumnObj.AddPair('visible', TJSONBool.Create(Col.Visible));
      ColumnObj.AddPair('sortOrder', SortOrderToString(Col.SortOrder));
      ColumnObj.AddPair('sortIndex', TJSONNumber.Create(Col.SortIndex));
      ColumnObj.AddPair('aggregationType', AggregationTypeToString(Col.AggregationType));
      ColumnObj.AddPair('footerText', Col.FooterText);
      ColumnObj.AddPair('cellConditionsJson', Col.CellConditionsJson);
      Columns.AddElement(ColumnObj);
    end;

    Root.AddPair('columns', Columns);
    Bytes := TEncoding.UTF8.GetBytes(Root.ToJSON);
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    Root.Free;
  end;
end;

function TVittixDBGridLayoutJsonStorage.LoadFromStream(
  Stream: TStream): TVittixDBGridLayoutState;
var
  Bytes: TBytes;
  JsonText: string;
  Root: TJSONObject;
  Columns: TJSONArray;
  I: Integer;
  ColObj: TJSONObject;
  Col: TVittixDBGridLayoutColumnState;
begin
  Result := TVittixDBGridLayoutState.Create;
  SetLength(Bytes, Stream.Size - Stream.Position);
  if Length(Bytes) > 0 then
    Stream.ReadBuffer(Bytes[0], Length(Bytes));
  JsonText := TEncoding.UTF8.GetString(Bytes);
  Root := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
  try
    if Root = nil then Exit;
    Result.Version := Root.GetValue<Integer>('version', 1);
    Result.FooterVisible := Root.GetValue<Boolean>('footerVisible', True);
    Result.AlternatingRowColors := Root.GetValue<Boolean>('alternatingRowColors', True);
    Result.AlternateRowColor := TColor(Root.GetValue<Integer>('alternateRowColor', $00F7F7F7));

    Columns := Root.GetValue<TJSONArray>('columns');
    if Assigned(Columns) then
      for I := 0 to Columns.Count - 1 do
      begin
        ColObj := Columns.Items[I] as TJSONObject;
        Col.FieldName := ColObj.GetValue<string>('fieldName', '');
        Col.DisplayIndex := ColObj.GetValue<Integer>('displayIndex', -1);
        Col.Width := ColObj.GetValue<Integer>('width', 0);
        Col.Visible := ColObj.GetValue<Boolean>('visible', True);
        Col.SortOrder := StringToSortOrder(ColObj.GetValue<string>('sortOrder', 'None'));
        Col.SortIndex := ColObj.GetValue<Integer>('sortIndex', -1);
        Col.AggregationType := StringToAggregationType(ColObj.GetValue<string>('aggregationType', 'None'));
        Col.FooterText := ColObj.GetValue<string>('footerText', '');
        Col.CellConditionsJson := ColObj.GetValue<string>('cellConditionsJson', '');
        Result.Columns.Add(Col);
      end;
  finally
    Root.Free;
  end;
end;

class procedure TVittixDBGridLayoutJsonStorage.SaveToFile(
  const State: TVittixDBGridLayoutState; const FileName: string);
var
  Stream: TFileStream;
  TargetFile: string;
  Storage: TVittixDBGridLayoutJsonStorage;
begin
  if FileName <> '' then
    TargetFile := FileName
  else
    Exit;

  Storage := TVittixDBGridLayoutJsonStorage.Create;
  try
    Stream := TFileStream.Create(TargetFile, fmCreate);
    try
      Storage.SaveToStream(State, Stream);
    finally
      Stream.Free;
    end;
  finally
    Storage.Free;
  end;
end;

class function TVittixDBGridLayoutJsonStorage.LoadFromFile(
  const FileName: string): TVittixDBGridLayoutState;
var
  Stream: TFileStream;
  SourceFile: string;
  Storage: TVittixDBGridLayoutJsonStorage;
begin
  Result := nil;
  if FileName <> '' then
    SourceFile := FileName
  else
    Exit;

  if not FileExists(SourceFile) then
    Exit;

  Storage := TVittixDBGridLayoutJsonStorage.Create;
  try
    Stream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
    try
      Result := Storage.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    Storage.Free;
  end;
end;

end.
