unit Vittix.Tests.Controller.Regression;

interface

uses
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  System.SysUtils,
  DUnitX.TestFramework,
  Vittix.DBGrid;

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
    procedure DatasetCanBeDestroyedAfterGridDetach;
  end;

implementation

uses
  Vittix.Tests.TestData;

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
    Assert.WillNotRaise(
      procedure
      begin
        OwnerForm.Free;
      end
    );
  finally
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
begin
  DataSet1 := CreateSampleDataSet;
  DataSet2 := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet1, OwnerForm);
    try
      Assert.IsNotNull(Grid.DataSource);
      Grid.DataSource.DataSet := DataSet2;
      Assert.AreSame(DataSet2, Grid.DataSource.DataSet);
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

end.
