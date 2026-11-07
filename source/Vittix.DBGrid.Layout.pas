unit Vittix.DBGrid.Layout;

interface

  uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  Vcl.Graphics,
  Vittix.DBGrid.ColumnInfo;

const
  /// <summary>Layout schema version written by this build. v1 files (no
  /// version key, or version 1) load unchanged; unknown fields from future
  /// versions are ignored on load.</summary>
  VITTIX_LAYOUT_SCHEMA_VERSION = 2;

type
  EVittixLayoutError = class(Exception);

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
    // Schema v2
    Pinned: Boolean;
    TitleCaption: string;
    DisplayFormat: string;
    ExportInclude: Boolean;
    ExportCaption: string;
    // Schema v2 optional editor configuration (TVittixEditorKind name)
    EditorKind: string;
    PickItems: string;
    HasMin: Boolean;
    MinValue: Double;
    HasMax: Boolean;
    MaxValue: Double;
    Required: Boolean;
    DecimalPlaces: Integer;
  end;

  TVittixDBGridLayoutState = class
  private
    FVersion: Integer;
    FFooterVisible: Boolean;
    FAlternatingRowColors: Boolean;
    FAlternateRowColor: TColor;
    // v2 optional: name of the applied filter set ('' = none). Applying a
    // layout skips the name silently when the set no longer exists.
    FActiveFilterSetName: string;
    FColumns: TList<TVittixDBGridLayoutColumnState>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Version: Integer read FVersion write FVersion;
    property FooterVisible: Boolean read FFooterVisible write FFooterVisible;
    property AlternatingRowColors: Boolean read FAlternatingRowColors write FAlternatingRowColors;
    property AlternateRowColor: TColor read FAlternateRowColor write FAlternateRowColor;
    property ActiveFilterSetName: string read FActiveFilterSetName write FActiveFilterSetName;
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
    class procedure SaveToFile(const State: TVittixDBGridLayoutState; const FileName: string);
    class function LoadFromFile(const FileName: string): TVittixDBGridLayoutState;
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
  FVersion := VITTIX_LAYOUT_SCHEMA_VERSION;
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
  FVersion := VITTIX_LAYOUT_SCHEMA_VERSION;
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
    // v2 optional field; an empty name means no filter set is applied
    Root.AddPair('activeFilterSet', State.ActiveFilterSetName);

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
      // Schema v2 column settings
      ColumnObj.AddPair('pinned', TJSONBool.Create(Col.Pinned));
      ColumnObj.AddPair('titleCaption', Col.TitleCaption);
      ColumnObj.AddPair('displayFormat', Col.DisplayFormat);
      ColumnObj.AddPair('exportInclude', TJSONBool.Create(Col.ExportInclude));
      ColumnObj.AddPair('exportCaption', Col.ExportCaption);
      // Optional editor configuration
      ColumnObj.AddPair('editorKind', Col.EditorKind);
      ColumnObj.AddPair('pickItems', Col.PickItems);
      ColumnObj.AddPair('hasMin', TJSONBool.Create(Col.HasMin));
      ColumnObj.AddPair('minValue', TJSONNumber.Create(Col.MinValue));
      ColumnObj.AddPair('hasMax', TJSONBool.Create(Col.HasMax));
      ColumnObj.AddPair('maxValue', TJSONNumber.Create(Col.MaxValue));
      ColumnObj.AddPair('required', TJSONBool.Create(Col.Required));
      ColumnObj.AddPair('decimalPlaces', TJSONNumber.Create(Col.DecimalPlaces));
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
  Parsed: TJSONValue;
  Root: TJSONObject;
  Columns: TJSONArray;
  I: Integer;
  ColObj: TJSONObject;
  Col: TVittixDBGridLayoutColumnState;
begin
  if not Assigned(Stream) then
    raise EVittixLayoutError.Create('Layout stream cannot be nil');

  Result := TVittixDBGridLayoutState.Create;
  try
    SetLength(Bytes, Stream.Size - Stream.Position);
    if Length(Bytes) > 0 then
      Stream.ReadBuffer(Bytes[0], Length(Bytes));
    JsonText := TEncoding.UTF8.GetString(Bytes);

    if Trim(JsonText) = '' then
      raise EVittixLayoutError.Create('Layout stream is empty');

    // ParseJSONValue returns nil for invalid JSON; keep the cast explicit so
    // a non-object root (e.g. an array) raises instead of leaking Result.
    Parsed := TJSONObject.ParseJSONValue(JsonText);
    if Parsed = nil then
      raise EVittixLayoutError.Create('Layout stream does not contain valid JSON');
    try
      if not (Parsed is TJSONObject) then
        raise EVittixLayoutError.Create('Layout root element must be a JSON object');
      Root := TJSONObject(Parsed);

      Result.Version := Root.GetValue<Integer>('version', 1);
      Result.FooterVisible := Root.GetValue<Boolean>('footerVisible', True);
      Result.AlternatingRowColors := Root.GetValue<Boolean>('alternatingRowColors', True);
      Result.AlternateRowColor := TColor(Root.GetValue<Integer>('alternateRowColor', $00F7F7F7));
      Result.ActiveFilterSetName := Root.GetValue<string>('activeFilterSet', '');

      Columns := Root.GetValue<TJSONArray>('columns', nil);
      if Assigned(Columns) then
        for I := 0 to Columns.Count - 1 do
        begin
          if not (Columns.Items[I] is TJSONObject) then
            Continue;
          ColObj := TJSONObject(Columns.Items[I]);
          Col.FieldName := ColObj.GetValue<string>('fieldName', '');
          Col.DisplayIndex := ColObj.GetValue<Integer>('displayIndex', -1);
          Col.Width := ColObj.GetValue<Integer>('width', 0);
          Col.Visible := ColObj.GetValue<Boolean>('visible', True);
          Col.SortOrder := StringToSortOrder(ColObj.GetValue<string>('sortOrder', 'None'));
          Col.SortIndex := ColObj.GetValue<Integer>('sortIndex', -1);
          Col.AggregationType := StringToAggregationType(ColObj.GetValue<string>('aggregationType', 'None'));
          Col.FooterText := ColObj.GetValue<string>('footerText', '');
          Col.CellConditionsJson := ColObj.GetValue<string>('cellConditionsJson', '');
          // Schema v2 fields; v1 files (and files from future versions with
          // the fields removed) fall back to the pre-v2 defaults.
          Col.Pinned := ColObj.GetValue<Boolean>('pinned', False);
          Col.TitleCaption := ColObj.GetValue<string>('titleCaption', '');
          Col.DisplayFormat := ColObj.GetValue<string>('displayFormat', '');
          Col.ExportInclude := ColObj.GetValue<Boolean>('exportInclude', True);
          Col.ExportCaption := ColObj.GetValue<string>('exportCaption', '');
          // Optional editor configuration; older files keep the defaults
          // (veNone, no items, no bounds).
          Col.EditorKind := ColObj.GetValue<string>('editorKind', 'veNone');
          // Files written by 1.4.0 may carry an "editMask" key; unknown
          // keys are ignored by the GetValue-based reader.
          Col.PickItems := ColObj.GetValue<string>('pickItems', '');
          Col.HasMin := ColObj.GetValue<Boolean>('hasMin', False);
          Col.MinValue := ColObj.GetValue<Double>('minValue', 0);
          Col.HasMax := ColObj.GetValue<Boolean>('hasMax', False);
          Col.MaxValue := ColObj.GetValue<Double>('maxValue', 0);
          Col.Required := ColObj.GetValue<Boolean>('required', False);
          Col.DecimalPlaces := ColObj.GetValue<Integer>('decimalPlaces', -1);
          Result.Columns.Add(Col);
        end;
    finally
      Parsed.Free;
    end;
  except
    // Never leak the partially-built state when parsing fails
    Result.Free;
    raise;
  end;
end;

class procedure TVittixDBGridLayoutJsonStorage.SaveToFile(
  const State: TVittixDBGridLayoutState; const FileName: string);
var
  Stream: TFileStream;
  TargetFile: string;
  Storage: TVittixDBGridLayoutJsonStorage;
begin
  // A silent no-op on an empty name hides caller bugs; make it explicit.
  if FileName = '' then
    raise EVittixLayoutError.Create('Layout file name cannot be empty');
  TargetFile := FileName;

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
  // A silent nil on an empty name hides caller bugs; make it explicit.
  if FileName = '' then
    raise EVittixLayoutError.Create('Layout file name cannot be empty');
  SourceFile := FileName;

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
