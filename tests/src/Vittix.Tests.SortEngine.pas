unit Vittix.Tests.SortEngine;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  Data.DB,
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
    FSortErrorReported: Boolean;
    procedure HandleFieldValidation(const FieldName: string; Found: Boolean);
    procedure HandleSortError(Sender: TObject; Column: TColumn; AError: Exception);
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
    procedure SortSummaryTextReflectsCurrentSortOrder;
    [Test]
    procedure UnknownFieldIsSkippedAndReported;
    [Test]
    procedure ClearSortingResetsMetadataAndDatasetIndex;
    [Test]
    procedure SortIndicesAreNormalizedWhenMiddleColumnRemoved;
    [Test]
    procedure ClearSortingRestoresOriginalDatasetIndexState;
    [Test]
    procedure UnsupportedDatasetRaisesOnApplySorting;
    [Test]
    procedure TitleClickOnUnsupportedDatasetRaisesWithoutHandler;
    [Test]
    procedure TitleClickOnUnsupportedDatasetReportsAndRollsBackWithHandler;
  end;

implementation

uses
  Vittix.Tests.TestData;

type
  /// <summary>A dataset that deliberately does NOT publish IndexFieldNames,
  /// standing in for backends like TADODataSet. Opens as an empty cursor;
  /// the unsupported-sort path never touches records.</summary>
  TDataSetWithoutIndexFields = class(TDataSet)
  private
    FOpened: Boolean;
  protected
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
  end;

function TDataSetWithoutIndexFields.GetRecord(Buffer: TRecordBuffer;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grEOF;
end;

procedure TDataSetWithoutIndexFields.InternalClose;
begin
  FOpened := False;
end;

procedure TDataSetWithoutIndexFields.InternalHandleException;
begin
  // Not reached by the unsupported-sort path
end;

procedure TDataSetWithoutIndexFields.InternalInitFieldDefs;
begin
  // Intentionally fieldless
end;

procedure TDataSetWithoutIndexFields.InternalOpen;
begin
  FOpened := True;
end;

function TDataSetWithoutIndexFields.IsCursorOpen: Boolean;
begin
  Result := FOpened;
end;

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

procedure TVittixSortEngineTests.SortSummaryTextReflectsCurrentSortOrder;
begin
  FEngine.ToggleSort(FGrid.Columns[1], False);
  FEngine.ToggleSort(FGrid.Columns[2], True);
  FEngine.ToggleSort(FGrid.Columns[2], True);

  Assert.AreEqual('Name:A#0,Amount:D#1', FEngine.GetSortSummaryText);
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

procedure TVittixSortEngineTests.HandleSortError(Sender: TObject;
  Column: TColumn; AError: Exception);
begin
  FSortErrorReported := True;
end;

procedure TVittixSortEngineTests.UnsupportedDatasetRaisesOnApplySorting;
var
  BareSet: TDataSetWithoutIndexFields;
  BareColumns: TVittixDBGridColumns;
  BareEngine: TVittixDBGridSortEngine;
  Info: TVittixDBGridColumnInfo;
begin
  BareSet := TDataSetWithoutIndexFields.Create(nil);
  try
    BareSet.Open;
    Assert.IsTrue(BareSet.Active, 'precondition: bare dataset is open');

    BareColumns := TVittixDBGridColumns.Create(nil);
    try
      Info := BareColumns.Add;
      Info.FieldName := 'Any';
      Info.SortOrder := vsoAsc;
      Info.SortIndex := 0;

      BareEngine := TVittixDBGridSortEngine.Create(BareSet, BareColumns);
      try
        Assert.WillRaise(
          procedure
          begin
            BareEngine.ApplySorting;
          end,
          EVittixSortError);
      finally
        BareEngine.Free;
      end;
    finally
      BareColumns.Free;
    end;
  finally
    BareSet.Free;
  end;
end;

procedure TVittixSortEngineTests.TitleClickOnUnsupportedDatasetRaisesWithoutHandler;
var
  BareSet: TDataSetWithoutIndexFields;
  Grid: TVittixDBGrid;
  OwnerForm: TForm;
begin
  BareSet := TDataSetWithoutIndexFields.Create(nil);
  try
    BareSet.Open;
    Grid := CreateHeadlessGrid(BareSet, OwnerForm);
    try
      Grid.Columns.BeginUpdate;
      try
        Grid.Columns.Clear;
        with Grid.Columns.Add do
        begin
          FieldName := 'Any';
          Title.Caption := 'Any';
        end;
      finally
        Grid.Columns.EndUpdate;
      end;

      // No OnSortError handler: the error must stay loud, and the click
      // must not leave a sort arrow behind.
      Assert.WillRaise(
        procedure
        begin
          Grid.Controller.DoTitleClick(Grid.Columns[0]);
        end,
        EVittixSortError);
      Assert.AreEqual(vsoNone, Grid.ColumnInfo[0].SortOrder,
        'failed toggle rolls the column state back');
    finally
      Grid.Free;
      OwnerForm.Free;
    end;
  finally
    BareSet.Free;
  end;
end;

procedure TVittixSortEngineTests.TitleClickOnUnsupportedDatasetReportsAndRollsBackWithHandler;
var
  BareSet: TDataSetWithoutIndexFields;
  Grid: TVittixDBGrid;
  OwnerForm: TForm;
begin
  BareSet := TDataSetWithoutIndexFields.Create(nil);
  try
    BareSet.Open;
    Grid := CreateHeadlessGrid(BareSet, OwnerForm);
    try
      Grid.Columns.BeginUpdate;
      try
        Grid.Columns.Clear;
        with Grid.Columns.Add do
        begin
          FieldName := 'Any';
          Title.Caption := 'Any';
        end;
      finally
        Grid.Columns.EndUpdate;
      end;

      FSortErrorReported := False;
      Grid.Controller.OnSortError := HandleSortError;

      Grid.Controller.DoTitleClick(Grid.Columns[0]);

      Assert.IsTrue(FSortErrorReported, 'OnSortError fired for the failed sort');
      Assert.AreEqual(vsoNone, Grid.ColumnInfo[0].SortOrder,
        'failed toggle rolls the column state back');
    finally
      Grid.Free;
      OwnerForm.Free;
    end;
  finally
    BareSet.Free;
  end;
end;

procedure TVittixSortEngineTests.ClearSortingRestoresOriginalDatasetIndexState;
begin
  FDataSet.IndexFieldNames := 'Name';
  Assert.AreEqual('Name', FDataSet.IndexFieldNames);

  FEngine.ToggleSort(FGrid.Columns[2], False);
  Assert.AreNotEqual('Name', FDataSet.IndexFieldNames);

  FEngine.ClearSorting;

  Assert.AreEqual('Name', FDataSet.IndexFieldNames);
  Assert.AreEqual(vsoNone, FColumns.FindByFieldName('Amount').SortOrder);
  Assert.AreEqual(-1, FColumns.FindByFieldName('Amount').SortIndex);
end;

end.
