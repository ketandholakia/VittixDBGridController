unit Vittix.Tests.Layout;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.IniFiles,
  Datasnap.DBClient,
  Vcl.Forms,
  Vcl.DBGrids,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Controller,
  Vittix.DBGrid.ColumnChooser,
  Vittix.DBGrid.Filter.Popup,
  Vittix.DBGrid.FooterPanel,
  Vittix.DBGrid.Layout;

type
  [TestFixture]
  TVittixLayoutTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FController: TVittixDBGridController;
    procedure SetupGrid;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure CaptureLayout_StoresColumnOrder;
    [Test]
    procedure CaptureLayout_StoresWidthAndVisibility;
    [Test]
    procedure ApplyLayout_RestoresColumnOrder;
    [Test]
    procedure ApplyLayout_RestoresWidthAndVisibility;
    [Test]
    procedure ApplyLayout_RestoresFooterCustomization;
    [Test]
    procedure ApplyLayout_RestoresFooterVisible;
    [Test]
    procedure ApplyLayout_IgnoresMissingFields;
    [Test]
    procedure ApplyLayout_IgnoresUnknownSavedFields;
    [Test]
    procedure SaveLoadLayout_IsIdempotent;
    [Test]
    procedure LayoutWorksWithoutActiveDataset;
    [Test]
    procedure FooterCaptionHelperUsesReadableNames;
    [Test]
    procedure ChooserStateRoundTripsThroughIni;
    [Test]
    procedure ChooserStateUsesConfiguredFileName;
    [Test]
    procedure ChooserStateUsesConfiguredRootPath;
    [Test]
    procedure LayoutStorageUsesConfiguredFileName;
    [Test]
    procedure GridSurfaceConfiguresAllPersistencePaths;
    [Test]
    procedure GridPersistenceRootPathFanOutsToHelpers;
    [Test]
    procedure ExplicitPersistenceFilesOverrideRootPath;
    [Test]
    procedure GridPersistenceFilesOverrideRootPathWhenSetAfterRoot;
    [Test]
    procedure GridCanSaveAndLoadLayoutToExplicitFile;
    [Test]
    procedure FooterCanClearAggregationThroughPublicApi;
    [Test]
    procedure ChooserResetRestoresOriginalLayout;
    [Test]
    procedure FooterCanClearAllAggregationsThroughPublicApi;
    [Test]
    procedure ChooserCanAdjustColumnWidthThroughPublicApi;
  end;

implementation

uses
  Vittix.Tests.TestData;

procedure TVittixLayoutTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  SetupGrid;
end;

procedure TVittixLayoutTests.TearDown;
begin
  FController.Free;
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixLayoutTests.SetupGrid;
begin
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  FController := TVittixDBGridController(FGrid.Controller);
end;

procedure TVittixLayoutTests.CaptureLayout_StoresColumnOrder;
var
  State: TVittixDBGridLayoutState;
begin
  FGrid.Columns[2].Index := 0;
  FGrid.Columns[0].Index := 1;
  FGrid.Columns[1].Index := 2;

  State := TVittixDBGridLayoutState.Create;
  try
    FController.CaptureLayout(State);
    Assert.IsTrue(State.Columns.Count > 0);
    Assert.AreEqual('Score', State.Columns[0].FieldName);
    Assert.AreEqual('ID', State.Columns[1].FieldName);
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutTests.CaptureLayout_StoresWidthAndVisibility;
var
  State: TVittixDBGridLayoutState;
begin
  FGrid.Columns[1].Width := 222;
  FGrid.Columns[2].Visible := False;

  State := TVittixDBGridLayoutState.Create;
  try
    FController.CaptureLayout(State);
    Assert.AreEqual(222, State.Columns[1].Width);
    Assert.IsFalse(State.Columns[2].Visible);
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutTests.ApplyLayout_RestoresColumnOrder;
var
  State: TVittixDBGridLayoutState;
begin
  State := TVittixDBGridLayoutState.Create;
  try
    FController.CaptureLayout(State);
    FGrid.Columns[0].Index := 2;
    FGrid.Columns[2].Index := 0;
    FController.ApplyLayout(State);
    Assert.AreEqual(State.Columns[0].FieldName, FGrid.Columns[State.Columns[0].DisplayIndex].FieldName);
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutTests.ApplyLayout_RestoresWidthAndVisibility;
var
  State: TVittixDBGridLayoutState;
begin
  State := TVittixDBGridLayoutState.Create;
  try
    FController.CaptureLayout(State);
    FGrid.Columns[1].Width := 50;
    FGrid.Columns[2].Visible := True;
    FController.ApplyLayout(State);
    Assert.AreEqual(State.Columns[1].Width, FGrid.Columns[1].Width);
    Assert.AreEqual(State.Columns[2].Visible, FGrid.Columns[2].Visible);
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutTests.ApplyLayout_RestoresFooterCustomization;
var
  State: TVittixDBGridLayoutState;
begin
  State := TVittixDBGridLayoutState.Create;
  try
    FController.CaptureLayout(State);

    FController.ShowFooter := False;
    FGrid.ColumnInfoByColumn(FGrid.Columns[1]).AggregationType := vatCount;
    FGrid.ColumnInfoByColumn(FGrid.Columns[2]).AggregationType := vatMax;

    FController.ApplyLayout(State);

    Assert.IsTrue(FController.ShowFooter);
    Assert.AreEqual(vatNone, FGrid.ColumnInfoByColumn(FGrid.Columns[1]).AggregationType);
    Assert.AreEqual(vatNone, FGrid.ColumnInfoByColumn(FGrid.Columns[2]).AggregationType);
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutTests.ApplyLayout_RestoresFooterVisible;
var
  State: TVittixDBGridLayoutState;
begin
  State := TVittixDBGridLayoutState.Create;
  try
    FController.CaptureLayout(State);
    FController.ShowFooter := False;
    FController.ApplyLayout(State);
    Assert.IsTrue(FController.ShowFooter);
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutTests.ApplyLayout_IgnoresMissingFields;
var
  State: TVittixDBGridLayoutState;
  Col: TVittixDBGridLayoutColumnState;
begin
  State := TVittixDBGridLayoutState.Create;
  try
    FController.CaptureLayout(State);
    Col.FieldName := 'MissingField';
    Col.DisplayIndex := 99;
    Col.Width := 123;
    Col.Visible := False;
    Col.SortOrder := vsoAsc;
    Col.SortIndex := 0;
    Col.AggregationType := vatSum;
    State.Columns.Add(Col);
    FController.ApplyLayout(State);
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutTests.ApplyLayout_IgnoresUnknownSavedFields;
var
  State: TVittixDBGridLayoutState;
  Col: TVittixDBGridLayoutColumnState;
begin
  State := TVittixDBGridLayoutState.Create;
  try
    FController.CaptureLayout(State);
    Col.FieldName := 'GhostField';
    Col.DisplayIndex := 7;
    Col.Width := 88;
    Col.Visible := True;
    Col.SortOrder := vsoDesc;
    Col.SortIndex := 0;
    Col.AggregationType := vatMax;
    State.Columns.Add(Col);
    FController.ApplyLayout(State);
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutTests.SaveLoadLayout_IsIdempotent;
var
  State1, State2: TVittixDBGridLayoutState;
  Stream: TMemoryStream;
  Storage: IVittixDBGridLayoutStorage;
  ColState: TVittixDBGridLayoutColumnState;
begin
  State1 := TVittixDBGridLayoutState.Create;
  State2 := nil;
  Stream := TMemoryStream.Create;
  Storage := TVittixDBGridLayoutJsonStorage.Create;
  try
    FController.CaptureLayout(State1);
    State1.FooterVisible := False;
    ColState := State1.Columns[1];
    ColState.AggregationType := vatSum;
    State1.Columns[1] := ColState;
    ColState := State1.Columns[2];
    ColState.AggregationType := vatMax;
    State1.Columns[2] := ColState;
    Storage.SaveToStream(State1, Stream);
    Stream.Position := 0;
    State2 := Storage.LoadFromStream(Stream);
    Assert.AreEqual(State1.Columns.Count, State2.Columns.Count);
    Assert.AreEqual(State1.FooterVisible, State2.FooterVisible);
    Assert.AreEqual(State1.AlternatingRowColors, State2.AlternatingRowColors);
    Assert.AreEqual(State1.AlternateRowColor, State2.AlternateRowColor);
    Assert.AreEqual(State1.Columns[1].AggregationType, State2.Columns[1].AggregationType);
    Assert.AreEqual(State1.Columns[2].AggregationType, State2.Columns[2].AggregationType);
  finally
    State2.Free;
    Stream.Free;
    State1.Free;
  end;
end;

procedure TVittixLayoutTests.LayoutWorksWithoutActiveDataset;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Controller: TVittixDBGridController;
  State: TVittixDBGridLayoutState;
begin
  OwnerForm := TForm.CreateNew(nil);
  State := TVittixDBGridLayoutState.Create;
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.Parent := OwnerForm;
      Controller := TVittixDBGridController(Grid.Controller);
      Controller.CaptureLayout(State);
      Controller.ApplyLayout(State);
      Assert.IsNotNull(Controller);
    finally
      OwnerForm.Free;
    end;
  finally
    State.Free;
  end;
end;

procedure TVittixLayoutTests.FooterCaptionHelperUsesReadableNames;
begin
  Assert.AreEqual('Clear aggregation', TVittixDBGridFooterPanel.AggregationCaption(vatNone));
  Assert.AreEqual('Average', TVittixDBGridFooterPanel.AggregationCaption(vatAvg));
  Assert.AreEqual('Maximum', TVittixDBGridFooterPanel.AggregationCaption(vatMax));
end;

procedure TVittixLayoutTests.ChooserStateRoundTripsThroughIni;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Chooser: TVittixDBGridColumnChooserForm;
  TempFile: string;
  Ini: TIniFile;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridChooser.test.ini');
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.Parent := OwnerForm;
      TVittixDBGridColumnChooserForm.StateFileName := TempFile;
      Chooser := TVittixDBGridColumnChooserForm.CreateChooser(OwnerForm, Grid);
      try
        Chooser.Left := 123;
        Chooser.Top := 234;
        Chooser.Width := 345;
        Chooser.Height := 456;
        Chooser.SearchText := 'Amount';
        Chooser.AllowReorder := False;
        Chooser.SaveDialogState;
        Chooser.Left := 1;
        Chooser.Top := 2;
        Chooser.Width := 3;
        Chooser.Height := 4;
        Chooser.SearchText := '';
        Chooser.AllowReorder := True;
        Chooser.LoadDialogState;
        Assert.AreEqual(123, Chooser.Left);
        Assert.AreEqual(234, Chooser.Top);
        Assert.AreEqual(345, Chooser.Width);
        Assert.AreEqual(456, Chooser.Height);
        Assert.AreEqual('Amount', Chooser.SearchText);
        Assert.IsFalse(Chooser.AllowReorder);
      finally
        Chooser.Free;
      end;

      Ini := TIniFile.Create(TempFile);
      try
        Assert.AreEqual('Amount', Ini.ReadString('Chooser', 'SearchText', ''));
        Assert.IsFalse(Ini.ReadBool('Chooser', 'AllowReorder', True));
      finally
        Ini.Free;
      end;
    finally
      TVittixDBGridColumnChooserForm.StateFileName := '';
      OwnerForm.Free;
    end;
  finally
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixLayoutTests.ChooserStateUsesConfiguredFileName;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Chooser: TVittixDBGridColumnChooserForm;
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridChooser.test.ini');
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.Parent := OwnerForm;
      Grid.ChooserStateFileName := TempFile;
      Chooser := TVittixDBGridColumnChooserForm.CreateChooser(OwnerForm, Grid);
      try
        Chooser.Left := 77;
        Chooser.Top := 88;
        Chooser.SaveDialogState;
      finally
        Chooser.Free;
      end;

      Assert.IsTrue(FileExists(TempFile));
    finally
      OwnerForm.Free;
    end;
  finally
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixLayoutTests.ChooserStateUsesConfiguredRootPath;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Chooser: TVittixDBGridColumnChooserForm;
  RootPath: string;
  PersistedFile: string;
begin
  RootPath := TPath.Combine(TPath.GetTempPath, 'VittixDBGridChooserRoot.test');
  PersistedFile := TPath.Combine(RootPath, 'chooser.ini');
  ForceDirectories(RootPath);
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.Parent := OwnerForm;
      Grid.PersistenceRootPath := RootPath;
      Chooser := TVittixDBGridColumnChooserForm.CreateChooser(OwnerForm, Grid);
      try
        Chooser.Left := 77;
        Chooser.Top := 88;
        Chooser.SaveDialogState;
      finally
        Chooser.Free;
      end;

      Assert.IsTrue(FileExists(PersistedFile));
    finally
      OwnerForm.Free;
    end;
  finally
    if FileExists(PersistedFile) then
      DeleteFile(PersistedFile);
    if TDirectory.Exists(RootPath) then
      TDirectory.Delete(RootPath, True);
  end;
end;

procedure TVittixLayoutTests.LayoutStorageUsesConfiguredFileName;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  TempFile: string;
  State: TVittixDBGridLayoutState;
  Loaded: TVittixDBGridLayoutState;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridLayout.test.json');
  OwnerForm := TForm.CreateNew(nil);
  Grid := TVittixDBGrid.Create(OwnerForm);
  State := TVittixDBGridLayoutState.Create;
  Loaded := nil;
  try
    Grid.LayoutStorageFileName := TempFile;
    TVittixDBGridController(Grid.Controller).CaptureLayout(State);
    TVittixDBGridLayoutJsonStorage.SaveToFile(State);
    Assert.IsTrue(FileExists(TempFile));

    Loaded := TVittixDBGridLayoutJsonStorage.LoadFromFile;
    Assert.IsNotNull(Loaded);
    Assert.AreEqual(State.Columns.Count, Loaded.Columns.Count);
  finally
    Loaded.Free;
    State.Free;
    Grid.Free;
    OwnerForm.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixLayoutTests.GridSurfaceConfiguresAllPersistencePaths;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.LayoutStorageFileName := 'C:\temp\layout.json';
      Grid.ChooserStateFileName := 'C:\temp\chooser.ini';
      Grid.FilterHistoryFileName := 'C:\temp\filter.ini';

      Assert.AreEqual('C:\temp\layout.json', Grid.LayoutStorageFileName);
      Assert.AreEqual('C:\temp\chooser.ini', Grid.ChooserStateFileName);
      Assert.AreEqual('C:\temp\filter.ini', Grid.FilterHistoryFileName);
    finally
      OwnerForm.Free;
    end;
  finally
  end;
end;

procedure TVittixLayoutTests.GridPersistenceRootPathFanOutsToHelpers;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.PersistenceRootPath := 'C:\temp\vittix';

      Assert.AreEqual('C:\temp\vittix', Grid.PersistenceRootPath);
    finally
      OwnerForm.Free;
    end;
  finally
  end;
end;

procedure TVittixLayoutTests.ExplicitPersistenceFilesOverrideRootPath;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.PersistenceRootPath := 'C:\temp\vittix-root';
      Grid.LayoutStorageFileName := 'C:\temp\layout.json';
      Grid.ChooserStateFileName := 'C:\temp\chooser.ini';
      Grid.FilterHistoryFileName := 'C:\temp\filter.ini';

      Assert.AreEqual('C:\temp\layout.json', Grid.LayoutStorageFileName);
      Assert.AreEqual('C:\temp\chooser.ini', Grid.ChooserStateFileName);
      Assert.AreEqual('C:\temp\filter.ini', Grid.FilterHistoryFileName);
    finally
      Grid.Free;
      OwnerForm.Free;
    end;
  finally
  end;
end;

procedure TVittixLayoutTests.GridPersistenceFilesOverrideRootPathWhenSetAfterRoot;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.PersistenceRootPath := 'C:\temp\vittix-root';
      Grid.LayoutStorageFileName := 'C:\temp\layout.json';
      Grid.ChooserStateFileName := 'C:\temp\chooser.ini';
      Grid.FilterHistoryFileName := 'C:\temp\filter.ini';

      Assert.AreEqual('C:\temp\layout.json', Grid.LayoutStorageFileName);
      Assert.AreEqual('C:\temp\chooser.ini', Grid.ChooserStateFileName);
      Assert.AreEqual('C:\temp\filter.ini', Grid.FilterHistoryFileName);
    finally
      OwnerForm.Free;
    end;
  finally
  end;
end;

procedure TVittixLayoutTests.GridCanSaveAndLoadLayoutToExplicitFile;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridLayout.explicit.json');
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.LayoutStorageFileName := TempFile;
      Grid.Columns[0].Width := 180;
      TVittixDBGridController(Grid.Controller).SaveLayoutToFile;

      Grid.Columns[0].Width := 50;
      TVittixDBGridController(Grid.Controller).LoadLayoutFromFile;

      Assert.AreEqual(180, Grid.Columns[0].Width);
      Assert.IsTrue(FileExists(TempFile));
    finally
      Grid.Free;
      OwnerForm.Free;
    end;
  finally
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixLayoutTests.FooterCanClearAggregationThroughPublicApi;
var
  Footer: TVittixDBGridFooterPanel;
  Column: TColumn;
begin
  Footer := TVittixDBGridFooterPanel.Create(FOwnerForm);
  try
    Footer.Attach(FGrid, nil);
    Column := FGrid.Columns[1];
    FGrid.ColumnInfoByColumn(Column).AggregationType := vatSum;

    Footer.ClearAggregationForColumn(Column);

    Assert.AreEqual(vatNone, FGrid.ColumnInfoByColumn(Column).AggregationType);
  finally
    Footer.Free;
  end;
end;

procedure TVittixLayoutTests.ChooserResetRestoresOriginalLayout;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Chooser: TVittixDBGridColumnChooserForm;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.Parent := OwnerForm;
      Chooser := TVittixDBGridColumnChooserForm.CreateChooser(OwnerForm, Grid);
      try
        Grid.Columns[1].Width := 100;
        Grid.Columns[2].Width := 180;
        Grid.Columns[0].Index := 2;
        Grid.Columns[1].Visible := False;
        Chooser.ResetLayout;

        Assert.AreEqual(0, Grid.Columns[0].Index);
        Assert.IsTrue(Grid.Columns[1].Visible);
        Assert.IsTrue(Grid.Columns[2].Visible);
        Assert.AreEqual(100, Grid.Columns[1].Width);
        Assert.AreEqual(180, Grid.Columns[2].Width);
      finally
        Chooser.Free;
      end;
    finally
      Grid.Free;
      OwnerForm.Free;
    end;
  finally
  end;
end;

procedure TVittixLayoutTests.FooterCanClearAllAggregationsThroughPublicApi;
var
  Footer: TVittixDBGridFooterPanel;
begin
  Footer := TVittixDBGridFooterPanel.Create(FOwnerForm);
  try
    Footer.Attach(FGrid, nil);
    FGrid.ColumnInfoByColumn(FGrid.Columns[1]).AggregationType := vatSum;
    FGrid.ColumnInfoByColumn(FGrid.Columns[2]).AggregationType := vatMax;

    Footer.ClearAllAggregations;

    Assert.AreEqual(vatNone, FGrid.ColumnInfoByColumn(FGrid.Columns[1]).AggregationType);
    Assert.AreEqual(vatNone, FGrid.ColumnInfoByColumn(FGrid.Columns[2]).AggregationType);
  finally
    Footer.Free;
  end;
end;

procedure TVittixLayoutTests.ChooserCanAdjustColumnWidthThroughPublicApi;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Chooser: TVittixDBGridColumnChooserForm;
  Column: TColumn;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.Parent := OwnerForm;
      Chooser := TVittixDBGridColumnChooserForm.CreateChooser(OwnerForm, Grid);
      try
        Column := Grid.Columns[1];
        Column.Width := 100;
        Chooser.ResetLayout;
        Chooser.SelectColumnIndex(1);
        Chooser.IncreaseSelectedColumnWidth;
        Assert.IsTrue(Column.Width > 100);
        Chooser.DecreaseSelectedColumnWidth;
        Assert.IsTrue(Column.Width <= 116);
      finally
        Chooser.Free;
      end;
    finally
      Grid.Free;
      OwnerForm.Free;
    end;
  finally
  end;
end;

end.
