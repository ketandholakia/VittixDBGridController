unit Vittix.Tests.Layout;

interface

uses
  System.SysUtils,
  System.Classes,
  Datasnap.DBClient,
  Vcl.Forms,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Controller,
  Vittix.DBGrid.ColumnChooser,
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
begin
  State1 := TVittixDBGridLayoutState.Create;
  State2 := nil;
  Stream := TMemoryStream.Create;
  Storage := TVittixDBGridLayoutJsonStorage.Create;
  try
    FController.CaptureLayout(State1);
    Storage.SaveToStream(State1, Stream);
    Stream.Position := 0;
    State2 := Storage.LoadFromStream(Stream);
    Assert.AreEqual(State1.Columns.Count, State2.Columns.Count);
    Assert.AreEqual(State1.FooterVisible, State2.FooterVisible);
    Assert.AreEqual(State1.AlternatingRowColors, State2.AlternatingRowColors);
    Assert.AreEqual(State1.AlternateRowColor, State2.AlternateRowColor);
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
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Grid := TVittixDBGrid.Create(OwnerForm);
    try
      Grid.Parent := OwnerForm;
      Chooser := TVittixDBGridColumnChooserForm.CreateChooser(OwnerForm, Grid);
      try
        Chooser.Left := 123;
        Chooser.Top := 234;
        Chooser.Width := 345;
        Chooser.Height := 456;
        Chooser.SaveDialogState;
        Chooser.Left := 1;
        Chooser.Top := 2;
        Chooser.Width := 3;
        Chooser.Height := 4;
        Chooser.LoadDialogState;
        Assert.AreEqual(123, Chooser.Left);
        Assert.AreEqual(234, Chooser.Top);
        Assert.AreEqual(345, Chooser.Width);
        Assert.AreEqual(456, Chooser.Height);
      finally
        Chooser.Free;
      end;
    finally
      OwnerForm.Free;
    end;
  finally
  end;
end;

end.
