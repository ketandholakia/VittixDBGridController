unit Vittix.Tests.Controller.Regression;

interface

uses
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  Vcl.Controls,
  System.SysUtils,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.Controller;

type
  [TestFixture]
  TVittixControllerRegressionTests = class
  private
    FAfterPostCalled: Boolean;
    FAfterScrollCalled: Boolean;
    FAfterCloseCalled: Boolean;
    procedure DatasetAfterPost(DataSet: TDataSet);
    procedure DatasetAfterScroll(DataSet: TDataSet);
    procedure DatasetAfterClose(DataSet: TDataSet);
  public
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
    procedure ControllerResetLayoutRestoresFooterVisibility;
    [Test]
    procedure ControllerCanBeFreedBeforeGridWithoutAV;
    [Test]
    procedure GridCanRecreateWindowHandleWhileAttached;
    [Test]
    procedure FormCanOpenAndCloseRepeatedlyWithAttachedGrid;
    [Test]
    procedure GridCanStartWithoutDatasourceAndAttachLater;
  end;

implementation

uses
  Vittix.Tests.TestData;

type
  TWinControlAccess = class(TWinControl);

procedure TVittixControllerRegressionTests.DatasetAfterPost(DataSet: TDataSet);
begin
  FAfterPostCalled := True;
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
    Controller := TVittixDBGridController(Grid.Controller);
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
    Controller := TVittixDBGridController(Grid.Controller);
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
    Controller := TVittixDBGridController(Grid.Controller);
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
    Controller := TVittixDBGridController(Grid.Controller);
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
    Controller := TVittixDBGridController(Grid.Controller);
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

end.
