unit Vittix.Tests.LayoutSchema;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  Vcl.DBGrids,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Layout,
  Vittix.DBGrid.Export.Engine,
  Vittix.DBGrid.Controller;

type
  [TestFixture]
  TVittixLayoutSchemaTests = class
  private
    function SaveStateToJson(State: TVittixDBGridLayoutState): string;
    function LoadStateFromJson(const Json: string): TVittixDBGridLayoutState;
  public
    [Test]
    procedure SaveWritesSchemaVersion2;
    [Test]
    procedure V2RoundTripKeepsColumnSettings;
    [Test]
    procedure V1FileLoadsWithDefaults;
    [Test]
    procedure V2MissingOptionalFieldsUseDefaults;
    [Test]
    procedure UnknownFutureFieldsAreIgnored;
    [Test]
    procedure CorruptInputRaisesLayoutError;
    [Test]
    procedure ColumnInfoAssignCopiesV2Settings;
    [Test]
    procedure RuntimeOnlyColumnStateIsNotStored;
    [Test]
    procedure ApplyLayoutPushesTitleCaptionAndDisplayFormat;
    [Test]
    procedure ExportHonorsExportIncludeAndExportCaption;
  end;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  Vittix.Tests.TestData;

{ TVittixLayoutSchemaTests }

function TVittixLayoutSchemaTests.SaveStateToJson(
  State: TVittixDBGridLayoutState): string;
var
  Storage: TVittixDBGridLayoutJsonStorage;
  Stream: TStringStream;
begin
  Storage := TVittixDBGridLayoutJsonStorage.Create;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Storage.SaveToStream(State, Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function TVittixLayoutSchemaTests.LoadStateFromJson(
  const Json: string): TVittixDBGridLayoutState;
var
  Storage: TVittixDBGridLayoutJsonStorage;
  Stream: TStringStream;
begin
  Storage := TVittixDBGridLayoutJsonStorage.Create;
  Stream := TStringStream.Create(Json, TEncoding.UTF8);
  try
    Result := Storage.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TVittixLayoutSchemaTests.SaveWritesSchemaVersion2;
var
  State: TVittixDBGridLayoutState;
  Json: string;
begin
  State := TVittixDBGridLayoutState.Create;
  try
    Json := SaveStateToJson(State);
    Assert.IsTrue(Json.Contains('"version":2'),
      'saving always writes schema v2');
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutSchemaTests.V2RoundTripKeepsColumnSettings;
var
  State, Loaded: TVittixDBGridLayoutState;
  Item: TVittixDBGridLayoutColumnState;
  Json: string;
begin
  State := TVittixDBGridLayoutState.Create;
  try
    Item.FieldName := 'Amount';
    Item.DisplayIndex := 2;
    Item.Width := 120;
    Item.Visible := True;
    Item.SortOrder := vsoNone;
    Item.SortIndex := -1;
    Item.AggregationType := vatNone;
    Item.FooterText := '';
    Item.CellConditionsJson := '';
    Item.Pinned := True;
    Item.TitleCaption := 'Order Total';
    Item.DisplayFormat := '#,##0.00';
    Item.ExportInclude := False;
    Item.ExportCaption := 'Total';
    State.Columns.Add(Item);

    Json := SaveStateToJson(State);
    Loaded := LoadStateFromJson(Json);
    try
      Assert.AreEqual<Integer>(1, Loaded.Columns.Count);
      Assert.IsTrue(Loaded.Columns[0].Pinned, 'pinned round-trips');
      Assert.AreEqual('Order Total', Loaded.Columns[0].TitleCaption);
      Assert.AreEqual('#,##0.00', Loaded.Columns[0].DisplayFormat);
      Assert.IsFalse(Loaded.Columns[0].ExportInclude, 'exportInclude round-trips');
      Assert.AreEqual('Total', Loaded.Columns[0].ExportCaption);
    finally
      Loaded.Free;
    end;
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutSchemaTests.V1FileLoadsWithDefaults;
var
  Loaded: TVittixDBGridLayoutState;
const
  V1_JSON =
    '{"columns":[{"fieldName":"ID","displayIndex":0,"width":50,' +
    '"visible":true,"sortOrder":"None","sortIndex":-1,' +
    '"aggregationType":"None","footerText":"","cellConditionsJson":""}],' +
    '"footerVisible":true,"alternatingRowColors":true,"alternateRowColor":-526345}';
begin
  // A v1 file has no version key at all
  Loaded := LoadStateFromJson(V1_JSON);
  try
    Assert.AreEqual(1, Loaded.Version, 'missing version loads as v1');
    Assert.AreEqual<Integer>(1, Loaded.Columns.Count);
    Assert.IsFalse(Loaded.Columns[0].Pinned, 'v1 loads with 0 frozen columns');
    Assert.AreEqual('', Loaded.Columns[0].TitleCaption);
    Assert.AreEqual('', Loaded.Columns[0].DisplayFormat);
    Assert.IsTrue(Loaded.Columns[0].ExportInclude, 'v1 exports everything');
    Assert.AreEqual('', Loaded.Columns[0].ExportCaption);
  finally
    Loaded.Free;
  end;
end;

procedure TVittixLayoutSchemaTests.V2MissingOptionalFieldsUseDefaults;
var
  Loaded: TVittixDBGridLayoutState;
const
  V2_PARTIAL =
    '{"version":2,"columns":[{"fieldName":"ID","displayIndex":0,' +
    '"width":50,"visible":true}],"footerVisible":true}';
begin
  Loaded := LoadStateFromJson(V2_PARTIAL);
  try
    Assert.AreEqual(2, Loaded.Version);
    Assert.AreEqual<Integer>(1, Loaded.Columns.Count);
    Assert.IsFalse(Loaded.Columns[0].Pinned);
    Assert.IsTrue(Loaded.Columns[0].ExportInclude,
      'missing exportInclude defaults to True');
    Assert.AreEqual('', Loaded.Columns[0].TitleCaption);
    Assert.AreEqual('', Loaded.Columns[0].ExportCaption);
  finally
    Loaded.Free;
  end;
end;

procedure TVittixLayoutSchemaTests.UnknownFutureFieldsAreIgnored;
var
  Loaded: TVittixDBGridLayoutState;
const
  FUTURE_JSON =
    '{"version":99,"columns":[{"fieldName":"ID","displayIndex":0,' +
    '"width":50,"visible":true,"frozenPane":true,"quantumState":"x"}],' +
    '"timeTravelEnabled":true}';
begin
  Loaded := LoadStateFromJson(FUTURE_JSON);
  try
    Assert.AreEqual<Integer>(1, Loaded.Columns.Count,
      'unknown fields are skipped');
    Assert.AreEqual('ID', Loaded.Columns[0].FieldName);
  finally
    Loaded.Free;
  end;
end;

procedure TVittixLayoutSchemaTests.CorruptInputRaisesLayoutError;
begin
  Assert.WillRaise(
    TTestLocalMethod(procedure begin LoadStateFromJson('this is not json'); end),
    EVittixLayoutError);

  Assert.WillRaise(
    TTestLocalMethod(procedure begin LoadStateFromJson('[1,2,3]'); end),
    EVittixLayoutError, 'a non-object root raises');
end;

procedure TVittixLayoutSchemaTests.ColumnInfoAssignCopiesV2Settings;
var
  Source, Target: TVittixDBGridColumnInfo;
begin
  Source := TVittixDBGridColumnInfo.Create(nil);
  Target := TVittixDBGridColumnInfo.Create(nil);
  try
    Source.Pinned := True;
    Source.TitleCaption := 'Custom Title';
    Source.DisplayFormat := '0.000';
    Source.ExportInclude := False;
    Source.ExportCaption := 'Short Name';

    Target.Assign(Source);
    Assert.IsTrue(Target.Pinned);
    Assert.AreEqual('Custom Title', Target.TitleCaption);
    Assert.AreEqual('0.000', Target.DisplayFormat);
    Assert.IsFalse(Target.ExportInclude);
    Assert.AreEqual('Short Name', Target.ExportCaption);
  finally
    Source.Free;
    Target.Free;
  end;
end;

procedure TVittixLayoutSchemaTests.RuntimeOnlyColumnStateIsNotStored;
var
  Prop: PPropInfo;
  Item: TVittixDBGridColumnInfo;
begin
  Item := TVittixDBGridColumnInfo.Create(nil);
  try
    // The DFM must not persist transient runtime state.
    Prop := GetPropInfo(TypeInfo(TVittixDBGridColumnInfo), 'FilterText');
    Assert.IsNotNull(Prop);
    Assert.IsFalse(IsStoredProp(Item, Prop), 'FilterText is not stored');

    Prop := GetPropInfo(TypeInfo(TVittixDBGridColumnInfo), 'HasFilter');
    Assert.IsFalse(IsStoredProp(Item, Prop), 'HasFilter is not stored');

    Prop := GetPropInfo(TypeInfo(TVittixDBGridColumnInfo), 'SortOrder');
    Assert.IsFalse(IsStoredProp(Item, Prop), 'SortOrder is not stored');

    Prop := GetPropInfo(TypeInfo(TVittixDBGridColumnInfo), 'SortIndex');
    Assert.IsFalse(IsStoredProp(Item, Prop), 'SortIndex is not stored');

    // Design-time metadata stays streamed.
    Prop := GetPropInfo(TypeInfo(TVittixDBGridColumnInfo), 'FieldName');
    Assert.IsTrue(IsStoredProp(Item, Prop), 'FieldName is stored');
    Prop := GetPropInfo(TypeInfo(TVittixDBGridColumnInfo), 'DisplayFormat');
    Assert.IsTrue(IsStoredProp(Item, Prop), 'DisplayFormat is stored');
  finally
    Item.Free;
  end;
end;

procedure TVittixLayoutSchemaTests.ApplyLayoutPushesTitleCaptionAndDisplayFormat;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  State: TVittixDBGridLayoutState;
  Item: TVittixDBGridLayoutColumnState;
  I: Integer;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      State := TVittixDBGridLayoutState.Create;
      try
        // Capture the current geometry so the apply only changes the two
        // v2 properties under test
        Grid.Controller.CaptureLayout(State);
        for I := 0 to State.Columns.Count - 1 do
          if SameText(State.Columns[I].FieldName, 'Amount') then
          begin
            Item := State.Columns[I];
            Item.TitleCaption := 'Balance Due';
            Item.DisplayFormat := '#,##0.00';
            State.Columns[I] := Item;
          end;
        Grid.Controller.ApplyLayout(State);
      finally
        State.Free;
      end;

      // Amount is the last column; the title override reached the column
      // and the format reached the TCurrencyField.
      Assert.AreEqual('Balance Due',
        Grid.Controller.FindColumnByFieldName('Amount').Title.Caption,
        'TitleCaption is pushed onto the grid column');
      Assert.AreEqual('#,##0.00',
        TNumericField(DataSet.FieldByName('Amount')).DisplayFormat,
        'DisplayFormat is pushed onto the numeric field');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixLayoutSchemaTests.ExportHonorsExportIncludeAndExportCaption;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Exporter: TVittixDBGridExporter;
  Output: string;
  Info: TVittixDBGridColumnInfo;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Info := Grid.ColumnInfo.FindByFieldName('Notes');
      Assert.IsNotNull(Info);
      Info.ExportInclude := False;
      Info := Grid.ColumnInfo.FindByFieldName('Amount');
      Assert.IsNotNull(Info);
      Info.ExportCaption := 'Order Total';

      Exporter := TVittixDBGridExporter.Create(Grid);
      try
        Output := Exporter.ExportToString(vefCSV);
        Assert.IsFalse(Output.Contains('Notes'), 'ExportInclude=False excluded');
        Assert.IsFalse(Output.Contains('gamma notes'),
          'excluded column carries no data either');
        Assert.IsTrue(Output.Contains('Order Total'),
          'ExportCaption replaces the header caption');
        // An ExportInclude=False column stays excluded even when the
        // visible-only switch is off.
        Exporter.Options.ExportVisibleOnly := False;
        Output := Exporter.ExportToString(vefCSV);
        Assert.IsFalse(Output.Contains('Notes'),
          'ExportInclude wins over ExportVisibleOnly=False');
      finally
        Exporter.Free;
      end;
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

end.
