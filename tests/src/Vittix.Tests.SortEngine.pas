unit Vittix.Tests.SortEngine;

interface

uses
  System.Classes,
  Datasnap.DBClient,
  Vcl.Forms,
  Vcl.DBGrids,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Sort.Engine;

type
  [TestFixture]
  TVittixSortEngineTests = class
  private
    FDataSet: TClientDataSet;
    FColumns: TVittixDBGridColumns;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FEngine: TVittixDBGridSortEngine;
    FValidatedField: string;
    FValidationFound: Boolean;
    procedure HandleFieldValidation(const FieldName: string; Found: Boolean);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure ToggleSortTriState;
    [Test]
    procedure SingleColumnSortClearsPreviousColumn;
    [Test]
    procedure MultiColumnSortBuildsExpectedIndexFieldNames;
    [Test]
    procedure UnknownFieldIsSkippedAndReported;
    [Test]
    procedure ClearSortingResetsMetadataAndDatasetIndex;
    [Test]
    procedure SortIndicesAreNormalizedWhenMiddleColumnRemoved;
  end;

implementation

uses
  Vittix.Tests.TestData;

procedure TVittixSortEngineTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FColumns := CreateMatchingColumns(FDataSet);
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  FEngine := TVittixDBGridSortEngine.Create(FDataSet, FColumns);
end;

procedure TVittixSortEngineTests.TearDown;
begin
  FEngine.Free;
  FColumns.Free;
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixSortEngineTests.HandleFieldValidation(const FieldName: string;
  Found: Boolean);
begin
  FValidatedField := FieldName;
  FValidationFound := Found;
end;

procedure TVittixSortEngineTests.ToggleSortTriState;
var
  Column: TColumn;
begin
  Column := FGrid.Columns[1];

  FEngine.ToggleSort(Column, False);
  Assert.AreEqual(vsoAsc, FColumns.FindByFieldName('Name').SortOrder);

  FEngine.ToggleSort(Column, False);
  Assert.AreEqual(vsoDesc, FColumns.FindByFieldName('Name').SortOrder);
  Assert.AreEqual('Name', FDataSet.IndexFields[0].FieldName);

  FEngine.ToggleSort(Column, False);
  Assert.AreEqual(vsoNone, FColumns.FindByFieldName('Name').SortOrder);
end;

procedure TVittixSortEngineTests.SingleColumnSortClearsPreviousColumn;
begin
  FEngine.ToggleSort(FGrid.Columns[1], False);
  FEngine.ToggleSort(FGrid.Columns[2], False);

  Assert.AreEqual(vsoNone, FColumns.FindByFieldName('Name').SortOrder);
  Assert.AreEqual(vsoAsc, FColumns.FindByFieldName('Amount').SortOrder);
  Assert.AreEqual('Amount', FDataSet.IndexFields[0].FieldName);
end;

procedure TVittixSortEngineTests.MultiColumnSortBuildsExpectedIndexFieldNames;
begin
  FEngine.ToggleSort(FGrid.Columns[1], False);
  FEngine.ToggleSort(FGrid.Columns[2], True);
  FEngine.ToggleSort(FGrid.Columns[2], True);

  Assert.AreEqual(0, FColumns.FindByFieldName('Name').SortIndex);
  Assert.AreEqual(1, FColumns.FindByFieldName('Amount').SortIndex);
  FDataSet.First;
  Assert.AreEqual(1, FDataSet.FieldByName('ID').AsInteger);
  FDataSet.Next;
  Assert.AreEqual(4, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixSortEngineTests.UnknownFieldIsSkippedAndReported;
var
  Missing: TVittixDBGridColumnInfo;
begin
  Missing := FColumns.Add;
  Missing.FieldName := 'DoesNotExist';
  Missing.SortOrder := vsoAsc;
  Missing.SortIndex := 0;

  FColumns.FindByFieldName('Name').SortOrder := vsoDesc;
  FColumns.FindByFieldName('Name').SortIndex := 1;

  FEngine.OnFieldValidation := HandleFieldValidation;
  FEngine.ApplySorting;

  Assert.AreEqual('DoesNotExist', FValidatedField);
  Assert.IsFalse(FValidationFound);
  Assert.AreNotEqual('', FDataSet.IndexName);
end;

procedure TVittixSortEngineTests.ClearSortingResetsMetadataAndDatasetIndex;
begin
  FEngine.ToggleSort(FGrid.Columns[1], False);
  FEngine.ToggleSort(FGrid.Columns[2], True);
  FEngine.ClearSorting;

  Assert.AreEqual('', FDataSet.IndexFieldNames);
  Assert.AreEqual('', FDataSet.IndexName);
  Assert.AreEqual(vsoNone, FColumns.FindByFieldName('Name').SortOrder);
  Assert.AreEqual(-1, FColumns.FindByFieldName('Name').SortIndex);
  Assert.AreEqual(vsoNone, FColumns.FindByFieldName('Amount').SortOrder);
  Assert.AreEqual(-1, FColumns.FindByFieldName('Amount').SortIndex);
end;

procedure TVittixSortEngineTests.SortIndicesAreNormalizedWhenMiddleColumnRemoved;
begin
  FEngine.ToggleSort(FGrid.Columns[1], True);
  FEngine.ToggleSort(FGrid.Columns[2], True);
  FEngine.ToggleSort(FGrid.Columns[3], True);

  FEngine.ToggleSort(FGrid.Columns[2], True);
  FEngine.ToggleSort(FGrid.Columns[2], True);

  Assert.AreEqual(vsoNone, FColumns.FindByFieldName('Amount').SortOrder);
  Assert.AreEqual(-1, FColumns.FindByFieldName('Amount').SortIndex);
  Assert.AreEqual(0, FColumns.FindByFieldName('Name').SortIndex);
  Assert.AreEqual(1, FColumns.FindByFieldName('Score').SortIndex);
end;

end.
