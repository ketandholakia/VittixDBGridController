unit Vittix.Tests.Controller.Regression;

interface

uses
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  DUnitX.TestFramework,
  Vittix.DBGrid;

type
  [TestFixture]
  TVittixControllerRegressionTests = class
  private
    FAfterPostCalled: Boolean;
    FAfterScrollCalled: Boolean;
    procedure DatasetAfterPost(DataSet: TDataSet);
    procedure DatasetAfterScroll(DataSet: TDataSet);
  public
    [Test]
    procedure ExistingAfterPostHandlerStillFiresAfterGridAttach;
    [Test]
    procedure ExistingAfterScrollHandlerStillFiresAfterGridAttach;
    [Test]
    procedure GridTeardownDoesNotRaise;
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

procedure TVittixControllerRegressionTests.ExistingAfterPostHandlerStillFiresAfterGridAttach;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    DataSet.AfterPost := DatasetAfterPost;
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
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
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    DataSet.AfterScroll := DatasetAfterScroll;
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
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
  Grid: TVittixDBGrid;
begin
  DataSet := CreateSampleDataSet;
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
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

end.
