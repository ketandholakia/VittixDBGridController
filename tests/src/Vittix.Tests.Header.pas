unit Vittix.Tests.Header;

interface

uses
  System.SysUtils,
  Datasnap.DBClient,
  Vcl.Forms,
  Vcl.DBGrids,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo;

type
  /// <summary>Phase 5: header decoration options and the auto-fit actions.
  /// The actual overlay painting needs a visible window and is verified in
  /// the IDE (see the phase notes).</summary>
  [TestFixture]
  TVittixHeaderTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure HeaderOptionsDefaultTrue;
    [Test]
    procedure AutoFitColumnExpandsNarrowColumn;
    [Test]
    procedure AutoFitAllColumnsResizesVisibleColumns;
    [Test]
    procedure TogglingHeaderOptionsDoesNotRaise;
    [Test]
    procedure ColumnInfoSetterCopiesEntries;
  end;

implementation

uses
  Vittix.Tests.TestData;

procedure TVittixHeaderTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
end;

procedure TVittixHeaderTests.TearDown;
begin
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixHeaderTests.HeaderOptionsDefaultTrue;
begin
  Assert.IsTrue(FGrid.ShowSortArrows);
  Assert.IsTrue(FGrid.ShowFilterIndicators);
  Assert.IsTrue(FGrid.HeaderMenu);
end;

procedure TVittixHeaderTests.AutoFitColumnExpandsNarrowColumn;
var
  Column: TColumn;
begin
  Column := FGrid.Columns[1]; // Name
  Column.Width := 20;
  FGrid.AutoFitColumn(Column);
  Assert.IsTrue(Column.Width > 20,
    'auto-fit grows a column narrower than its content');
  Assert.IsTrue(Column.Width >= 24, 'a minimum width is enforced');
end;

procedure TVittixHeaderTests.AutoFitAllColumnsResizesVisibleColumns;
var
  I, Before, After: Integer;
begin
  Before := 0;
  for I := 0 to FGrid.Columns.Count - 1 do
  begin
    FGrid.Columns[I].Width := 20;
    Inc(Before, FGrid.Columns[I].Width);
  end;

  FGrid.AutoFitAllColumns;

  After := 0;
  for I := 0 to FGrid.Columns.Count - 1 do
    if FGrid.Columns[I].Visible then
      Inc(After, FGrid.Columns[I].Width);
  Assert.IsTrue(After > Before, 'auto-fit all widens the visible columns');
end;

procedure TVittixHeaderTests.TogglingHeaderOptionsDoesNotRaise;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FGrid.ShowSortArrows := False;
      FGrid.ShowFilterIndicators := False;
      FGrid.HeaderMenu := False;
      FGrid.Invalidate;
      FGrid.ShowSortArrows := True;
      FGrid.ShowFilterIndicators := True;
      FGrid.HeaderMenu := True;
      FGrid.Invalidate;
    end);
end;

procedure TVittixHeaderTests.ColumnInfoSetterCopiesEntries;
var
  Source: TVittixDBGridColumns;
  Item: TVittixDBGridColumnInfo;
begin
  Source := TVittixDBGridColumns.Create(nil);
  try
    Item := Source.Add;
    Item.FieldName := 'Name';
    Item.DisplayFormat := '0.0';
    Item.Pinned := True;
    Item := Source.Add;
    Item.FieldName := 'Amount';

    FGrid.ColumnInfo := Source;
    Assert.AreEqual<Integer>(2, FGrid.ColumnInfo.Count,
      'entries were copied into the grid-owned collection');
    Assert.AreEqual('Name', FGrid.ColumnInfo[0].FieldName);
    Assert.AreEqual('0.0', FGrid.ColumnInfo[0].DisplayFormat);
    Assert.IsTrue(FGrid.ColumnInfo[0].Pinned);
  finally
    Source.Free;
  end;

  // The grid still owns a valid collection after the source is freed.
  Assert.AreEqual<Integer>(2, FGrid.ColumnInfo.Count);
end;

end.
