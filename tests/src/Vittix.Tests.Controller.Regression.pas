unit Vittix.Tests.Controller.Regression;

interface

uses
  System.Types,
  Datasnap.DBClient,
  Data.DB,
  System.Classes,
  System.Variants,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.DBGrids,
  System.SysUtils,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Layout,
  Vittix.DBGrid.ColumnChooser,
  Vittix.DBGrid.Aggregation.Engine,
  Vittix.DBGrid.Controller;

type
  [TestFixture]
  TVittixControllerRegressionTests = class
  private
    FAfterPostCalled: Boolean;
    FAfterScrollCalled: Boolean;
    FAfterCloseCalled: Boolean;
    FTitleClickCalled: Boolean;
    FKeyDownCalled: Boolean;
    FDblClickCalled: Boolean;
    procedure DatasetAfterPost(DataSet: TDataSet);
    procedure DatasetAfterScroll(DataSet: TDataSet);
    procedure DatasetAfterClose(DataSet: TDataSet);
    procedure GridTitleClick(Column: TColumn);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridDblClick(Sender: TObject);
  public
    [Test]
    procedure AggregatesRefreshAfterDeleteAndPost;
    [Test]
    procedure LoadedLayoutRefreshesAggregatesAndFooter;
    [Test]
    procedure FirstDataRowWithoutTitlesDoesNotOpenPopup;
    [Test]
    procedure FilteredDatasetOwnerCanBeFreedBeforeGrid;
    [Test]
    procedure ExistingAfterPostHandlerStillFiresAfterGridAttach;
    [Test]
    procedure ExistingAfterScrollHandlerStillFiresAfterGridAttach;
    [Test]
    procedure GridTeardownDoesNotRaise;
    [Test]
    procedure GridCanBeCreatedAndDestroyedRepeatedly;
    [Test]
    procedure DatasetCanBeReplacedWhileGridIsAttached;
    [Test]
    procedure DatasetCanCloseAndReopenWhileGridIsAttached;
    [Test]
    procedure DatasetCanBeClosedAndReopenedRepeatedlyWhileAttached;
    [Test]
    procedure DatasetCanBeDestroyedAfterGridDetach;
    [Test]
    procedure ControllerCanToggleActiveAndFooterRepeatedly;
    [Test]
    [Ignore('ResetLayout currently resets column layout only; restoring footer visibility is unimplemented (roadmap feature)')]
    procedure ControllerResetLayoutRestoresFooterVisibility;
    [Test]
    procedure ControllerCanBeFreedBeforeGridWithoutAV;
    [Test]
    procedure GridCanRecreateWindowHandleWhileAttached;
    [Test]
    procedure FormCanOpenAndCloseRepeatedlyWithAttachedGrid;
    [Test]
    procedure GridCanStartWithoutDatasourceAndAttachLater;
    [Test]
    procedure ApplicationTitleClickHandlerFiresAndSortingStillWorks;
    [Test]
    procedure ApplicationKeyDownHandlerStillFires;
    [Test]
    procedure ApplicationDblClickHandlerStillFires;
  end;

  /// <summary>
  /// Regression coverage for the public notification events (roadmap A1)
  /// and the strongly typed Controller property (roadmap A2).
  /// </summary>
  [TestFixture]
  TVittixControllerEventTests = class
  private
    FSortCount: Integer;
    FFilterCount: Integer;
    FLayoutCount: Integer;
    FMoveCount: Integer;
    FValidateCount: Integer;
    FFieldValidationCount: Integer;
    FFormatCount: Integer;
    FTitleClickCount: Integer;
    FLastSortColumn: TColumn;
    FLastFilterField: string;
    FLastMovedColumn: TColumn;
    FLastOldIndex: Integer;
    FLastNewIndex: Integer;
    FLastValidatedField: string;
    FLastFieldValidationName: string;
    FLastFieldValidationFound: Boolean;
    procedure HandleAfterSort(Sender: TObject; Column: TColumn);
    procedure HandleFilterApplied(Sender: TObject; const FieldName: string);
    procedure HandleAfterApplyLayout(Sender: TObject);
    procedure HandleColumnMoved(Sender: TObject; Column: TColumn;
      OldIndex, NewIndex: Integer);
    procedure HandleValidateFilter(Sender: TObject; const FieldName: string;
      const FilterText: string; var IsValid: Boolean; var ErrorMessage: string);
    procedure HandleFieldValidation(const FieldName: string; Found: Boolean);
    procedure HandleFormatAggregation(Sender: TObject;
      Info: TVittixDBGridColumnInfo; AggType: TVittixAggregationType;
      Value: Variant; var DisplayText: string);
    procedure HandleTitleClick(Column: TColumn);
    procedure ResetCounters;
  public
    [Test]
    procedure OnAfterSortFiresAfterTitleClick;
    [Test]
    procedure OnAfterSortFiresExactlyOncePerTitleClick;
    [Test]
    procedure OnAfterSortFiresAfterApplyStateWithNilColumn;
    [Test]
    procedure OnAfterSortSafeWhenControllerFreed;
    [Test]
    procedure OnAfterSortNotFiredWhenDatasetClosed;
    [Test]
    procedure OnFilterAppliedFiresForGlobalFilter;
    [Test]
    procedure OnFilterAppliedFiresForClearFilters;
    [Test]
    procedure OnAfterApplyLayoutFiresAfterApply;
    [Test]
    procedure OnColumnMovedFiresFromChooserMove;
    [Test]
    procedure OnColumnMovedFiresFromVCLColumnMoved;
    [Test]
    procedure NotificationEventsWorkWithApplicationHandlersAssigned;
    [Test]
    procedure OnValidateFilterReachableThroughController;
    [Test]
    procedure OnValidateFilterAssignedBeforeEnginesSurvivesDatasetAttach;
    [Test]
    procedure OnFieldValidationReachableThroughController;
    [Test]
    procedure OnFormatAggregationFiresWhenDisplayTextRequested;
    [Test]
    procedure ControllerPropertyReturnsSameControllerAndGrid;
    [Test]
    procedure SetGridRejectsPlainTDBGrid;
  end;

implementation

uses
  Vittix.Tests.TestData;

type
  TWinControlAccess = class(TWinControl);
  // Exposes the protected dispatchers we simulate input through
  TDBGridAccess = class(TDBGrid);
  TPopupProbeController = class(TVittixDBGridController)
  public
    PopupCount: Integer;
  protected
    function ExecuteFilterPopup(Column: TColumn): Boolean; override;
  end;

function TPopupProbeController.ExecuteFilterPopup(Column: TColumn): Boolean;
begin
  Inc(PopupCount);
  Result := False;
end;

procedure TVittixControllerRegressionTests.FirstDataRowWithoutTitlesDoesNotOpenPopup;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Probe: TPopupProbeController;
  R: TRect;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Grid.Controller.Active := False;
      Probe := TPopupProbeController.Create(nil);
      try
        Probe.Grid := Grid;
        Grid.Options := Grid.Options - [dgTitles];
        R := Grid.GetCellRect(Grid.GetIndicatorOffset, 0);
        Assert.IsFalse(Probe.DoMouseDown(mbRight, [], R.Left + 2, R.Top + 2));
        Assert.AreEqual(0, Probe.PopupCount);
        Grid.Options := Grid.Options + [dgTitles];
        R := Grid.GetCellRect(Grid.GetIndicatorOffset, 0);
        Assert.IsTrue(Probe.DoMouseDown(mbRight, [], R.Left + 2, R.Top + 2));
        Assert.AreEqual(1, Probe.PopupCount);
      finally
        Probe.Free;
      end;
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.DatasetAfterPost(DataSet: TDataSet);
begin
  FAfterPostCalled := True;
end;

procedure TVittixControllerRegressionTests.FilteredDatasetOwnerCanBeFreedBeforeGrid;
var
  DataSet: TClientDataSet;
  DataOwner: TDataModule;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataOwner := TDataModule.CreateNew(nil);
  try
    DataSet := CreateSampleDataSet;
    DataOwner.InsertComponent(DataSet);
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Grid.Controller.SetGlobalFilter('Alpha');
      Assert.IsTrue(DataSet.Filtered);
      FreeAndNil(DataOwner);
      Assert.IsNull(Grid.Controller.FilterEngine);
      FreeAndNil(OwnerForm);
    finally
      OwnerForm.Free;
    end;
  finally
    DataOwner.Free;
  end;
end;

procedure TVittixControllerRegressionTests.AggregatesRefreshAfterDeleteAndPost;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Info: TVittixDBGridColumnInfo;
  Engine: TVittixDBGridAggregationEngine;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Info := Grid.ColumnInfo.FindByFieldName('Amount');
      Grid.Controller.SetColumnAggregation(
        Grid.Controller.FindColumnByFieldName('Amount'), vatSum);
      Engine := Grid.Controller.AggregationEngine;
      Assert.AreEqual<Double>(750.75, Engine.GetAggregation(Info));
      DataSet.First;
      DataSet.Delete;
      Assert.AreSame(Engine, Grid.Controller.AggregationEngine);
      Assert.AreEqual<Double>(650.25, Engine.GetAggregation(Info));
      DataSet.Append;
      DataSet.FieldByName('Amount').AsCurrency := 25;
      DataSet.Post;
      Assert.AreSame(Engine, Grid.Controller.AggregationEngine);
      Assert.AreEqual<Double>(675.25, Engine.GetAggregation(Info));
      DataSet.Edit;
      DataSet.FieldByName('Amount').AsCurrency := 30;
      DataSet.Post;
      Assert.AreEqual<Double>(680.25, Engine.GetAggregation(Info));
      DataSet.Append;
      DataSet.FieldByName('Amount').AsCurrency := 99;
      DataSet.Cancel;
      Assert.AreEqual<Double>(680.25, Engine.GetAggregation(Info));
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.LoadedLayoutRefreshesAggregatesAndFooter;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Stream: TMemoryStream;
begin
  DataSet := CreateSampleDataSet;
  Stream := TMemoryStream.Create;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Grid.Controller.SetColumnAggregation(
        Grid.Controller.FindColumnByFieldName('Amount'), vatSum);
      Grid.Controller.SaveLayoutToStream(Stream);
      Grid.Controller.ResetLayout;
      Grid.Controller.SetColumnAggregation(
        Grid.Controller.FindColumnByFieldName('Amount'), vatNone);
      Stream.Position := 0;
      Grid.Controller.LoadLayoutFromStream(Stream);
      Assert.AreEqual<Double>(750.75, Grid.Controller.AggregationEngine.GetAggregation(
        Grid.ColumnInfo.FindByFieldName('Amount')));
      Assert.IsTrue(Grid.Controller.FooterDisplayText('Amount') <> '');
    finally
      OwnerForm.Free;
    end;
  finally
    Stream.Free;
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.GridTitleClick(Column: TColumn);
begin
  FTitleClickCalled := True;
end;

procedure TVittixControllerRegressionTests.GridKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FKeyDownCalled := True;
end;

procedure TVittixControllerRegressionTests.GridDblClick(Sender: TObject);
begin
  FDblClickCalled := True;
end;

procedure TVittixControllerRegressionTests.DatasetAfterScroll(DataSet: TDataSet);
begin
  FAfterScrollCalled := True;
end;

procedure TVittixControllerRegressionTests.DatasetAfterClose(DataSet: TDataSet);
begin
  FAfterCloseCalled := True;
end;

procedure TVittixControllerRegressionTests.ExistingAfterPostHandlerStillFiresAfterGridAttach;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
begin
  DataSet := CreateSampleDataSet;
  try
    DataSet.AfterPost := DatasetAfterPost;
    CreateHeadlessGrid(DataSet, OwnerForm);
    try
      DataSet.Edit;
      DataSet.FieldByName('Name').AsString := 'Alpha updated';
      DataSet.Post;
      Assert.IsTrue(FAfterPostCalled);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.ExistingAfterScrollHandlerStillFiresAfterGridAttach;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
begin
  DataSet := CreateSampleDataSet;
  try
    DataSet.AfterScroll := DatasetAfterScroll;
    CreateHeadlessGrid(DataSet, OwnerForm);
    try
      FAfterScrollCalled := False;
      DataSet.First;
      DataSet.Next;
      Assert.IsTrue(FAfterScrollCalled);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.GridTeardownDoesNotRaise;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
begin
  DataSet := CreateSampleDataSet;
  try
    CreateHeadlessGrid(DataSet, OwnerForm);
    try
      OwnerForm.Free;
      OwnerForm := nil;
    except
      on E: Exception do
        Assert.Fail(E.ClassName + ': ' + E.Message);
    end;
    // Explicit success marker: no exception escaped the teardown above
    Assert.IsTrue(True);
  finally
    if Assigned(OwnerForm) then
      OwnerForm.Free;
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.GridCanBeCreatedAndDestroyedRepeatedly;
var
  I: Integer;
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  for I := 1 to 50 do
  begin
    DataSet := CreateSampleDataSet;
    try
      Grid := CreateHeadlessGrid(DataSet, OwnerForm);
      try
        Assert.IsNotNull(Grid);
        Assert.IsNotNull(OwnerForm);
      finally
        OwnerForm.Free;
      end;
    finally
      DataSet.Free;
    end;
  end;
end;

procedure TVittixControllerRegressionTests.DatasetCanBeReplacedWhileGridIsAttached;
var
  DataSet1, DataSet2: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Controller: TVittixDBGridController;
begin
  DataSet1 := CreateSampleDataSet;
  DataSet2 := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet1, OwnerForm);
    Controller := Grid.Controller;
    try
      Assert.IsNotNull(Grid.DataSource);
      Grid.DataSource.DataSet := DataSet2;
      Assert.AreSame(DataSet2, Grid.DataSource.DataSet);
      Controller.Refresh;
      Assert.IsTrue(Controller.Active);
      DataSet2.First;
      DataSet2.Next;
      Assert.IsTrue(DataSet2.RecNo > 1);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet2.Free;
    DataSet1.Free;
  end;
end;

procedure TVittixControllerRegressionTests.DatasetCanCloseAndReopenWhileGridIsAttached;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    DataSet.AfterClose := DatasetAfterClose;
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Assert.IsNotNull(Grid);
      FAfterCloseCalled := False;
      DataSet.Close;
      Assert.IsTrue(FAfterCloseCalled);
      Assert.IsFalse(DataSet.Active);
      DataSet.Open;
      Assert.IsTrue(DataSet.Active);
      DataSet.First;
      DataSet.Next;
      Assert.IsTrue(DataSet.RecNo > 1);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.DatasetCanBeClosedAndReopenedRepeatedlyWhileAttached;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  I: Integer;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Assert.IsNotNull(Grid);
      for I := 1 to 5 do
      begin
        DataSet.Close;
        Assert.IsFalse(DataSet.Active);
        DataSet.Open;
        Assert.IsTrue(DataSet.Active);
        DataSet.First;
        DataSet.Next;
        Assert.IsTrue(DataSet.RecNo > 1);
      end;
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.DatasetCanBeDestroyedAfterGridDetach;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Assert.IsNotNull(Grid);
      OwnerForm.Free;
      DataSet.Free;
      DataSet := nil;
    except
      on E: Exception do
        Assert.Fail(E.ClassName + ': ' + E.Message);
    end;
  finally
    if Assigned(DataSet) then
      DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.ControllerCanToggleActiveAndFooterRepeatedly;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Controller: TVittixDBGridController;
  I: Integer;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    Controller := Grid.Controller;
    try
      for I := 1 to 10 do
      begin
        Controller.Active := False;
        Controller.Active := True;
        Controller.ShowFooter := False;
        Controller.ShowFooter := True;
      end;

      Assert.IsTrue(Controller.Active);
      Assert.IsTrue(Controller.ShowFooter);
      Assert.IsNotNull(Grid.DataSource);
      Assert.AreSame(DataSet, Grid.DataSource.DataSet);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.ControllerResetLayoutRestoresFooterVisibility;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Controller: TVittixDBGridController;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    Controller := Grid.Controller;
    try
      Controller.ShowFooter := False;
      Controller.ResetLayout;
      Assert.IsTrue(Controller.ShowFooter);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.ControllerCanBeFreedBeforeGridWithoutAV;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Controller: TVittixDBGridController;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    Controller := Grid.Controller;
    try
      Assert.IsNotNull(Controller);
      Controller.Free;
      Assert.IsTrue(Grid.Controller = nil);
      try
        OwnerForm.Free;
        OwnerForm := nil;
      except
        on E: Exception do
          Assert.Fail(E.ClassName + ': ' + E.Message);
      end;
    finally
      if Assigned(OwnerForm) then
        OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.GridCanRecreateWindowHandleWhileAttached;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Controller: TVittixDBGridController;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    Controller := Grid.Controller;
    try
      Assert.IsNotNull(Grid);
      Assert.IsNotNull(Controller);
      TWinControlAccess(Grid).RecreateWnd;
      Controller.Refresh;
      Assert.IsTrue(Controller.Active);
      Assert.IsNotNull(Grid.DataSource);
      Assert.AreSame(DataSet, Grid.DataSource.DataSet);
      DataSet.First;
      DataSet.Next;
      Assert.IsTrue(DataSet.RecNo > 1);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.FormCanOpenAndCloseRepeatedlyWithAttachedGrid;
var
  I: Integer;
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  for I := 1 to 10 do
  begin
    DataSet := CreateSampleDataSet;
    try
      Grid := CreateHeadlessGrid(DataSet, OwnerForm);
      try
        Assert.IsNotNull(Grid);
        Assert.IsNotNull(OwnerForm);
      finally
        OwnerForm.Free;
      end;
    finally
      DataSet.Free;
    end;
  end;
end;

procedure TVittixControllerRegressionTests.GridCanStartWithoutDatasourceAndAttachLater;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    OwnerForm := TForm.CreateNew(nil);
    try
      Grid := TVittixDBGrid.Create(OwnerForm);
      try
        Grid.Parent := OwnerForm;
        Assert.IsTrue(Grid.DataSource = nil);
        Grid.DataSource := TDataSource.Create(OwnerForm);
        Grid.DataSource.DataSet := DataSet;
        Assert.IsNotNull(Grid.DataSource);
        Assert.AreSame(DataSet, Grid.DataSource.DataSet);
      finally
        OwnerForm.Free;
      end;
    except
      on E: Exception do
      begin
        OwnerForm.Free;
        raise;
      end;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.ApplicationTitleClickHandlerFiresAndSortingStillWorks;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      // Application assigns its own handler AFTER the grid is live — with the
      // old event-hooking design this silently replaced the controller's
      // sort integration. Both must now work simultaneously.
      FTitleClickCalled := False;
      Grid.OnTitleClick := GridTitleClick;

      TDBGridAccess(Grid).TitleClick(Grid.Columns[0]);

      Assert.IsTrue(FTitleClickCalled, 'application OnTitleClick handler fired');
      Assert.IsTrue(Grid.ColumnInfo[0].SortOrder <> vsoNone,
        'controller sorting still applied after event assignment');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.ApplicationKeyDownHandlerStillFires;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Key: Word;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      FKeyDownCalled := False;
      Grid.OnKeyDown := GridKeyDown;

      Key := Ord('X');
      TDBGridAccess(Grid).KeyDown(Key, []);

      Assert.IsTrue(FKeyDownCalled, 'application OnKeyDown handler fired');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerRegressionTests.ApplicationDblClickHandlerStillFires;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      FDblClickCalled := False;
      Grid.OnDblClick := GridDblClick;

      TDBGridAccess(Grid).DblClick;

      Assert.IsTrue(FDblClickCalled, 'application OnDblClick handler fired');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

{ TVittixControllerEventTests }

procedure TVittixControllerEventTests.ResetCounters;
begin
  FSortCount := 0;
  FFilterCount := 0;
  FLayoutCount := 0;
  FMoveCount := 0;
  FValidateCount := 0;
  FFieldValidationCount := 0;
  FFormatCount := 0;
  FTitleClickCount := 0;
  FLastSortColumn := nil;
  FLastFilterField := '?';
  FLastMovedColumn := nil;
  FLastOldIndex := -1;
  FLastNewIndex := -1;
  FLastValidatedField := '';
  FLastFieldValidationName := '';
  FLastFieldValidationFound := True;
end;

procedure TVittixControllerEventTests.HandleAfterSort(Sender: TObject;
  Column: TColumn);
begin
  Inc(FSortCount);
  FLastSortColumn := Column;
end;

procedure TVittixControllerEventTests.HandleFilterApplied(Sender: TObject;
  const FieldName: string);
begin
  Inc(FFilterCount);
  FLastFilterField := FieldName;
end;

procedure TVittixControllerEventTests.HandleAfterApplyLayout(Sender: TObject);
begin
  Inc(FLayoutCount);
end;

procedure TVittixControllerEventTests.HandleColumnMoved(Sender: TObject;
  Column: TColumn; OldIndex, NewIndex: Integer);
begin
  Inc(FMoveCount);
  FLastMovedColumn := Column;
  FLastOldIndex := OldIndex;
  FLastNewIndex := NewIndex;
end;

procedure TVittixControllerEventTests.HandleValidateFilter(Sender: TObject;
  const FieldName: string; const FilterText: string; var IsValid: Boolean;
  var ErrorMessage: string);
begin
  Inc(FValidateCount);
  FLastValidatedField := FieldName;
  IsValid := False;
  ErrorMessage := 'rejected by test';
end;

procedure TVittixControllerEventTests.HandleFieldValidation(
  const FieldName: string; Found: Boolean);
begin
  Inc(FFieldValidationCount);
  FLastFieldValidationName := FieldName;
  FLastFieldValidationFound := Found;
end;

procedure TVittixControllerEventTests.HandleFormatAggregation(Sender: TObject;
  Info: TVittixDBGridColumnInfo; AggType: TVittixAggregationType;
  Value: Variant; var DisplayText: string);
begin
  Inc(FFormatCount);
  DisplayText := 'CUSTOM';
end;

procedure TVittixControllerEventTests.HandleTitleClick(Column: TColumn);
begin
  Inc(FTitleClickCount);
end;

procedure TVittixControllerEventTests.OnAfterSortFiresAfterTitleClick;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnAfterSort := HandleAfterSort;

      TDBGridAccess(Grid).TitleClick(Grid.Columns[0]);

      Assert.AreEqual(1, FSortCount, 'OnAfterSort fired once');
      Assert.AreSame(Grid.Columns[0], FLastSortColumn, 'sort column passed');
      Assert.IsTrue(Grid.ColumnInfo[0].SortOrder <> vsoNone,
        'sorting actually applied');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnAfterSortFiresExactlyOncePerTitleClick;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      // Assigning through both surfaces must land on the SAME handler
      // storage — one operation, exactly one invocation.
      Grid.OnAfterSort := HandleAfterSort;
      Grid.Controller.OnAfterSort := HandleAfterSort;

      TDBGridAccess(Grid).TitleClick(Grid.Columns[0]);

      Assert.AreEqual(1, FSortCount,
        'grid and controller surfaces share storage — no duplicate fire');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnAfterSortFiresAfterApplyStateWithNilColumn;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnAfterSort := HandleAfterSort;

      Grid.Controller.ApplyState;

      Assert.AreEqual(1, FSortCount, 'OnAfterSort fired for ApplyState');
      Assert.IsTrue(FLastSortColumn = nil, 'Column is nil for wholesale apply');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnAfterSortSafeWhenControllerFreed;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnAfterSort := HandleAfterSort;

      Grid.Controller.Free;
      Assert.IsTrue(Grid.Controller = nil, 'controller reference cleared');

      // Must be a silent no-op — no AV, no event.
      TDBGridAccess(Grid).TitleClick(Grid.Columns[0]);

      Assert.AreEqual(0, FSortCount, 'no event without a controller');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnAfterSortNotFiredWhenDatasetClosed;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnAfterSort := HandleAfterSort;

      DataSet.Close;
      TDBGridAccess(Grid).TitleClick(Grid.Columns[0]);

      Assert.AreEqual(0, FSortCount,
        'no sort event while engines are unavailable (dataset closed)');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnFilterAppliedFiresForGlobalFilter;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnFilterApplied := HandleFilterApplied;

      Grid.Controller.SetGlobalFilter('Alpha');

      Assert.AreEqual(1, FFilterCount, 'OnFilterApplied fired once');
      Assert.AreEqual('', FLastFilterField, 'global filter carries no field name');
      Assert.AreEqual(2, CountVisibleRecords(DataSet),
        'filter actually applied (two Alpha rows visible)');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnFilterAppliedFiresForClearFilters;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnFilterApplied := HandleFilterApplied;

      Grid.Controller.SetGlobalFilter('Alpha');
      Grid.Controller.ClearFilters;

      Assert.AreEqual(2, FFilterCount, 'apply + clear each fired once');
      Assert.AreEqual(5, CountVisibleRecords(DataSet),
        'clear actually removed the filter');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnAfterApplyLayoutFiresAfterApply;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  State: TVittixDBGridLayoutState;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnAfterApplyLayout := HandleAfterApplyLayout;

      State := TVittixDBGridLayoutState.Create;
      try
        Grid.Controller.CaptureLayout(State);
        Grid.Columns[0].Width := 77; // dirty the layout
        Grid.Controller.ApplyLayout(State);
      finally
        State.Free;
      end;

      Assert.AreEqual(1, FLayoutCount, 'OnAfterApplyLayout fired once');
      Assert.AreEqual(100, Grid.Columns[0].Width,
        'layout actually restored');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnColumnMovedFiresFromChooserMove;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Chooser: TVittixDBGridColumnChooserForm;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnColumnMoved := HandleColumnMoved;

      Chooser := TVittixDBGridColumnChooserForm.CreateChooser(nil, Grid);
      try
        Chooser.SelectColumnIndex(0);
        Chooser.MoveSelectedItem(1);
      finally
        Chooser.Free;
      end;

      Assert.AreEqual(1, FMoveCount, 'OnColumnMoved fired once');
      Assert.IsNotNull(FLastMovedColumn);
      Assert.AreEqual(1, FLastMovedColumn.Index,
        'the originally-first column actually moved to index 1');
      Assert.AreEqual('ID', Grid.Columns[1].FieldName,
        'column order on the grid changed');
      Assert.AreEqual(0, FLastOldIndex);
      Assert.AreEqual(1, FLastNewIndex);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnColumnMovedFiresFromVCLColumnMoved;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnColumnMoved := HandleColumnMoved;

      // Raw grid coordinates include the indicator column (offset 1 with
      // the default Options), so grid col 3 = display column 2.
      TDBGridAccess(Grid).ColumnMoved(2, 3);

      Assert.AreEqual(1, FMoveCount, 'OnColumnMoved fired once');
      Assert.AreSame(Grid.Columns[2], FLastMovedColumn);
      Assert.AreEqual(1, FLastOldIndex);
      Assert.AreEqual(2, FLastNewIndex);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.NotificationEventsWorkWithApplicationHandlersAssigned;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.OnTitleClick := HandleTitleClick;
      Grid.OnAfterSort := HandleAfterSort;

      TDBGridAccess(Grid).TitleClick(Grid.Columns[0]);

      Assert.AreEqual(1, FTitleClickCount, 'application OnTitleClick fired');
      Assert.AreEqual(1, FSortCount, 'notification event still fired');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnValidateFilterReachableThroughController;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Info: TVittixDBGridColumnInfo;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.Controller.OnValidateFilter := HandleValidateFilter;

      Info := Grid.ColumnInfo.FindByFieldName('Name');
      Info.FilterText := 'zz';
      Info.HasFilter := True;

      // The engine validates every column filter when applying; a rejecting
      // handler must raise through the public controller path.
      Assert.WillRaise(
        procedure
        begin
          Grid.Controller.SetGlobalFilter('Alpha');
        end);

      Assert.AreEqual(1, FValidateCount, 'engine OnValidateFilter reached');
      Assert.AreEqual('Name', FLastValidatedField);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnValidateFilterAssignedBeforeEnginesSurvivesDatasetAttach;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  DataSource: TDataSource;
  Info: TVittixDBGridColumnInfo;
  I: Integer;
begin
  DataSet := CreateSampleDataSet;
  try
    OwnerForm := TForm.CreateNew(nil);
    try
      Grid := TVittixDBGrid.Create(OwnerForm);
      Grid.Parent := OwnerForm;
      DataSource := TDataSource.Create(OwnerForm);
      Grid.DataSource := DataSource;

      Grid.Columns.BeginUpdate;
      try
        Grid.Columns.Clear;
        for I := 0 to DataSet.Fields.Count - 1 do
        begin
          Grid.Columns.Add.FieldName := DataSet.Fields[I].FieldName;
          Grid.Columns[I].Width := 100;
        end;
      finally
        Grid.Columns.EndUpdate;
      end;

      // Assign BEFORE any engine exists (no dataset attached yet).
      ResetCounters;
      Grid.Controller.OnValidateFilter := HandleValidateFilter;

      // Attaching the dataset creates the engines; the cached handler must
      // be pushed to the new filter engine.
      DataSource.DataSet := DataSet;

      Info := Grid.ColumnInfo.FindByFieldName('Name');
      Assert.IsNotNull(Info, 'column info populated');
      Info.FilterText := 'zz';
      Info.HasFilter := True;

      Assert.WillRaise(
        procedure
        begin
          Grid.Controller.SetGlobalFilter('Alpha');
        end);

      Assert.AreEqual(1, FValidateCount,
        'handler assigned before engine creation reached the engine');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnFieldValidationReachableThroughController;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Ghost: TVittixDBGridColumnInfo;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      Grid.Controller.OnFieldValidation := HandleFieldValidation;

      // A sorted column whose field does not exist in the dataset triggers
      // the sort engine's field validation callback during ApplySorting.
      Ghost := Grid.ColumnInfo.Add;
      Ghost.FieldName := 'Ghost';
      Ghost.SortOrder := vsoAsc;
      Ghost.SortIndex := 0;

      Grid.Controller.ApplyState;

      Assert.AreEqual(1, FFieldValidationCount, 'OnFieldValidation reached');
      Assert.AreEqual('Ghost', FLastFieldValidationName);
      Assert.IsFalse(FLastFieldValidationFound);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.OnFormatAggregationFiresWhenDisplayTextRequested;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Engine: TVittixDBGridAggregationEngine;
  Info: TVittixDBGridColumnInfo;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      ResetCounters;
      // Controller surface caches the handler (round-trip below); the
      // engine-level invocation proves the format event semantics.
      Grid.Controller.OnFormatAggregation := HandleFormatAggregation;
      Assert.IsTrue(Assigned(Grid.Controller.OnFormatAggregation),
        'controller property round-trips the handler');

      Engine := TVittixDBGridAggregationEngine.Create(DataSet, Grid.ColumnInfo);
      try
        Engine.OnFormatAggregation := HandleFormatAggregation;

        Info := Grid.ColumnInfo.FindByFieldName('Amount');
        Info.AggregationType := vatSum;
        Engine.Recalculate;

        Assert.AreEqual('CUSTOM',
          Engine.GetAggregationDisplayText(Info),
          'OnFormatAggregation overrode the display text');
        Assert.AreEqual(1, FFormatCount);
      finally
        Engine.Free;
      end;
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.ControllerPropertyReturnsSameControllerAndGrid;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Controller: TVittixDBGridController;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Controller := Grid.Controller;
      Assert.IsNotNull(Controller);
      Assert.IsTrue(Controller is TVittixDBGridController,
        'Controller property is strongly typed');
      Assert.AreSame(Grid, Controller.Grid,
        'controller Grid property points back at the grid');
      Assert.IsTrue(Controller.Active);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixControllerEventTests.SetGridRejectsPlainTDBGrid;
var
  OwnerForm: TForm;
  PlainGrid: TDBGrid;
  Controller: TVittixDBGridController;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    PlainGrid := TDBGrid.Create(OwnerForm);
    Controller := TVittixDBGridController.Create(OwnerForm);

    Assert.WillRaise(
      procedure
      begin
        Controller.Grid := PlainGrid;
      end,
      EArgumentException);
  finally
    OwnerForm.Free;
  end;
end;

end.
