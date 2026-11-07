unit Vittix.Tests.FreezeColumns;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  Vcl.DBGrids,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Layout,
  Vittix.DBGrid.FooterPanel,
  Vittix.DBGrid.ColumnChooser,
  Vittix.DBGrid.Controller;

type
  // Reaches the protected TCustomGrid.FixedCols for assertions
  TGridFixedColsAccess = class(TDBGrid);
  // Reaches the protected ColumnMoved override to simulate a VCL-initiated
  // column drag (direct TColumn.Index assignment does not route through it)
  TVittixGridDragAccess = class(TVittixDBGrid);

  [TestFixture]
  TVittixFreezeColumnsTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FFrozenChangedCount: Integer;
    FColumnMovedCount: Integer;
    procedure HandleFrozenChanged(Sender: TObject);
    procedure HandleColumnMoved(Sender: TObject; Column: TColumn;
      OldIndex, NewIndex: Integer);
    function ColumnOrder: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure FrozenColumnsClampsToVisibleCount;
    [Test]
    procedure FrozenColumnsAppliesFixedColsIncludingIndicator;
    [Test]
    procedure FreezeUpToColumnSetsCountAndPins;
    [Test]
    procedure UnfreezeAllClearsCountAndPins;
    [Test]
    procedure OnFrozenColumnsChangedFiresOncePerChange;
    [Test]
    procedure FrozenColumnCannotMoveIntoScrollingRegion;
    [Test]
    procedure ScrollingColumnCannotMoveIntoFrozenRegion;
    [Test]
    procedure MovesWithinARegionStayAllowed;
    [Test]
    procedure LayoutRoundTripRestoresFrozenColumns;
    [Test]
    procedure V1LayoutLoadsUnfrozen;
    [Test]
    procedure ResetLayoutClearsFreezing;
    [Test]
    procedure FooterRectsStayAlignedWithFrozenColumns;
    [Test]
    procedure FooterHitTestCoversFirstDataColumnWithIndicator;
    [Test]
    procedure FooterHitTestCoversAllColumnsWithoutIndicator;
    [Test]
    procedure ChooserHideShrinksFrozenRegion;
    [Test]
    procedure ChooserMoveAcrossFrozenBoundaryRejected;
  end;

implementation

uses
  Vittix.Tests.TestData;

{ TVittixFreezeColumnsTests }

procedure TVittixFreezeColumnsTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  FFrozenChangedCount := 0;
  FColumnMovedCount := 0;
end;

procedure TVittixFreezeColumnsTests.TearDown;
begin
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixFreezeColumnsTests.HandleFrozenChanged(Sender: TObject);
begin
  Inc(FFrozenChangedCount);
end;

procedure TVittixFreezeColumnsTests.HandleColumnMoved(Sender: TObject;
  Column: TColumn; OldIndex, NewIndex: Integer);
begin
  Inc(FColumnMovedCount);
end;

function TVittixFreezeColumnsTests.ColumnOrder: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FGrid.Columns.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + FGrid.Columns[I].FieldName;
  end;
end;

procedure TVittixFreezeColumnsTests.FrozenColumnsClampsToVisibleCount;
begin
  // 7 visible columns: the task's contract is 0..VisibleColumnCount-1
  FGrid.FrozenColumns := 99;
  Assert.AreEqual(6, FGrid.FrozenColumns, 'clamped to visible count - 1');
  FGrid.FrozenColumns := -3;
  Assert.AreEqual(0, FGrid.FrozenColumns, 'negative clamps to 0');
end;

procedure TVittixFreezeColumnsTests.FrozenColumnsAppliesFixedColsIncludingIndicator;
begin
  Assert.IsTrue(dgIndicator in FGrid.Options, 'fixture has an indicator');
  FGrid.FrozenColumns := 2;
  Assert.AreEqual(3, TGridFixedColsAccess(FGrid).FixedCols,
    'FixedCols = indicator offset + frozen count');
  FGrid.FrozenColumns := 0;
  Assert.AreEqual(1, TGridFixedColsAccess(FGrid).FixedCols,
    'unfrozen grid keeps only the indicator fixed');
end;

procedure TVittixFreezeColumnsTests.FreezeUpToColumnSetsCountAndPins;
var
  Info: TVittixDBGridColumnInfo;
begin
  FGrid.FreezeUpToColumn(FGrid.Controller.FindColumnByFieldName('Amount'));
  Assert.AreEqual(3, FGrid.FrozenColumns,
    'ID, Name and Amount are the first three visible columns');

  Info := FGrid.ColumnInfo.FindByFieldName('ID');
  Assert.IsTrue(Info.Pinned);
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsTrue(Info.Pinned);
  Info := FGrid.ColumnInfo.FindByFieldName('Amount');
  Assert.IsTrue(Info.Pinned);
  Info := FGrid.ColumnInfo.FindByFieldName('Score');
  Assert.IsFalse(Info.Pinned, 'columns after the freeze point stay unpinned');

  // Hidden target is a no-op
  FGrid.Controller.FindColumnByFieldName('Score').Visible := False;
  FGrid.FreezeUpToColumn(FGrid.Controller.FindColumnByFieldName('Score'));
  Assert.AreEqual(3, FGrid.FrozenColumns, 'freezing to a hidden column does nothing');
end;

procedure TVittixFreezeColumnsTests.UnfreezeAllClearsCountAndPins;
var
  Info: TVittixDBGridColumnInfo;
begin
  FGrid.FrozenColumns := 2;
  // A stray pin outside the region must also clear
  FGrid.ColumnInfo.FindByFieldName('Score').Pinned := True;

  FGrid.UnfreezeAll;
  Assert.AreEqual(0, FGrid.FrozenColumns);
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsFalse(Info.Pinned);
  Info := FGrid.ColumnInfo.FindByFieldName('Score');
  Assert.IsFalse(Info.Pinned, 'UnfreezeAll clears every Pinned flag');
end;

procedure TVittixFreezeColumnsTests.OnFrozenColumnsChangedFiresOncePerChange;
begin
  FGrid.OnFrozenColumnsChanged := HandleFrozenChanged;
  FGrid.FrozenColumns := 2;
  Assert.AreEqual(1, FFrozenChangedCount);
  FGrid.FrozenColumns := 2;
  Assert.AreEqual(1, FFrozenChangedCount, 'no event when nothing changed');
  FGrid.FreezeUpToColumn(FGrid.Controller.FindColumnByFieldName('Score'));
  Assert.AreEqual(2, FFrozenChangedCount);
  FGrid.UnfreezeAll;
  Assert.AreEqual(3, FFrozenChangedCount);
end;

procedure TVittixFreezeColumnsTests.FrozenColumnCannotMoveIntoScrollingRegion;
var
  Ind: Integer;
begin
  FGrid.FrozenColumns := 2;
  FGrid.Controller.OnColumnMoved := HandleColumnMoved;
  FColumnMovedCount := 0;
  Ind := FGrid.GetIndicatorOffset;

  // VCL-initiated move path (drag): the ColumnMoved override must roll the
  // move back. Direct TColumn.Index assignment does not route through it
  // (the chooser enforces the same rule on its own path).
  TVittixGridDragAccess(FGrid).ColumnMoved(1 + Ind, 5 + Ind);
  Assert.AreEqual('ID,Name,Amount,Score,Notes,Created,IsActive', ColumnOrder,
    'a frozen column cannot leave the frozen region');
  Assert.AreEqual(0, FColumnMovedCount,
    'a rejected move reports no OnColumnMoved');
end;

procedure TVittixFreezeColumnsTests.ScrollingColumnCannotMoveIntoFrozenRegion;
var
  Ind: Integer;
begin
  FGrid.FrozenColumns := 2;
  FGrid.Controller.OnColumnMoved := HandleColumnMoved;
  FColumnMovedCount := 0;
  Ind := FGrid.GetIndicatorOffset;

  TVittixGridDragAccess(FGrid).ColumnMoved(4 + Ind, 1 + Ind);
  Assert.AreEqual('ID,Name,Amount,Score,Notes,Created,IsActive', ColumnOrder,
    'a scrolling column cannot enter the frozen region');
  Assert.AreEqual(0, FColumnMovedCount);
end;

procedure TVittixFreezeColumnsTests.MovesWithinARegionStayAllowed;
var
  Ind: Integer;
  Order: string;
begin
  FGrid.FrozenColumns := 2;
  FGrid.Controller.OnColumnMoved := HandleColumnMoved;
  FColumnMovedCount := 0;
  Ind := FGrid.GetIndicatorOffset;

  // Reorder inside the frozen region
  TVittixGridDragAccess(FGrid).ColumnMoved(0 + Ind, 1 + Ind);
  Order := ColumnOrder;
  Assert.AreEqual('Name,ID', Copy(Order, 1, 7), 'frozen columns may reorder');
  Assert.AreEqual(1, FColumnMovedCount, 'OnColumnMoved fires exactly once');

  // Reorder inside the scrolling region (IsActive from 6 to 4)
  TVittixGridDragAccess(FGrid).ColumnMoved(6 + Ind, 4 + Ind);
  Assert.AreEqual('Name,ID,Amount,Score,IsActive,Notes,Created', ColumnOrder,
    'scrolling columns may reorder');
  Assert.AreEqual(2, FColumnMovedCount);
end;

procedure TVittixFreezeColumnsTests.LayoutRoundTripRestoresFrozenColumns;
var
  Stream: TMemoryStream;
  Target: TClientDataSet;
  TargetForm: TForm;
  TargetGrid: TVittixDBGrid;
begin
  FGrid.FrozenColumns := 2;

  Stream := TMemoryStream.Create;
  try
    FGrid.Controller.SaveLayoutToStream(Stream);
    Stream.Position := 0;

    Target := CreateSampleDataSet;
    try
      TargetGrid := CreateHeadlessGrid(Target, TargetForm);
      try
        Assert.AreEqual(0, TargetGrid.FrozenColumns);
        Stream.Position := 0;
        TargetGrid.Controller.LoadLayoutFromStream(Stream);
        Assert.AreEqual(2, TargetGrid.FrozenColumns,
          'ApplyLayout restores the frozen region from the Pinned flags');
        Assert.IsTrue(TargetGrid.ColumnInfo.FindByFieldName('Name').Pinned);
      finally
        TargetForm.Free;
      end;
    finally
      Target.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TVittixFreezeColumnsTests.V1LayoutLoadsUnfrozen;
var
  Stream: TMemoryStream;
  Bytes: TBytes;
const
  V1_JSON =
    '{"columns":[{"fieldName":"ID","displayIndex":0,"width":50,' +
    '"visible":true,"sortOrder":"None","sortIndex":-1,' +
    '"aggregationType":"None","footerText":"","cellConditionsJson":""},' +
    '{"fieldName":"Name","displayIndex":1,"width":100,"visible":true,' +
    '"sortOrder":"None","sortIndex":-1,"aggregationType":"None",' +
    '"footerText":"","cellConditionsJson":""}],' +
    '"footerVisible":true,"alternatingRowColors":true,"alternateRowColor":-526345}';
begin
  Stream := TMemoryStream.Create;
  try
    Bytes := TEncoding.UTF8.GetBytes(V1_JSON);
    Stream.WriteBuffer(Bytes[0], Length(Bytes));
    Stream.Position := 0;
    FGrid.Controller.LoadLayoutFromStream(Stream);
    Assert.AreEqual(0, FGrid.FrozenColumns,
      'a v1 layout restores with 0 frozen columns');
  finally
    Stream.Free;
  end;
end;

procedure TVittixFreezeColumnsTests.ResetLayoutClearsFreezing;
begin
  FGrid.FrozenColumns := 3;
  FGrid.Controller.ResetLayout;
  Assert.AreEqual(0, FGrid.FrozenColumns, 'ResetLayout clears freezing');
  Assert.IsFalse(FGrid.ColumnInfo.FindByFieldName('ID').Pinned);
end;

procedure TVittixFreezeColumnsTests.FooterRectsStayAlignedWithFrozenColumns;
var
  Footer: TVittixDBGridFooterPanel;
  FrozenRect, ScrolledRect: TRect;
begin
  FGrid.HandleNeeded;
  // The default form client area fits exactly one 100px scrolling column
  // next to the frozen pair plus the indicator, so LeftCol = 6 after the
  // SelectedIndex jump below.
  FGrid.FrozenColumns := 2;

  Footer := TVittixDBGridFooterPanel.Create(nil);
  try
    Footer.Attach(FGrid, FGrid.Controller.AggregationEngine);

    // Scroll the scrolling region: focus the last column so LeftCol moves
    // past the frozen region.
    FGrid.SelectedIndex := 6;
    Assert.IsTrue(FGrid.GetLeftCol > FGrid.GetIndicatorOffset + 2,
      'precondition: the grid scrolled past the frozen region');

    // A frozen column stays inside the client area...
    FrozenRect := Footer.GetColumnRect(FGrid.Controller.FindColumnByFieldName('ID'));
    Assert.IsTrue(FrozenRect.Left >= 0,
      'frozen column footer rect stays visible while scrolled');
    Assert.IsTrue(FrozenRect.Right > FrozenRect.Left);

    // ...while a scrolled-out column reports a rect left of the client.
    ScrolledRect := Footer.GetColumnRect(FGrid.Columns[2]);
    Assert.IsTrue(ScrolledRect.Right <= 0,
      'scrolled-out columns are outside the visible client area');
  finally
    Footer.Free;
  end;
end;

procedure TVittixFreezeColumnsTests.ChooserHideShrinksFrozenRegion;
var
  Chooser: TVittixDBGridColumnChooserForm;
begin
  FGrid.FrozenColumns := 2;
  Assert.AreEqual(2, FGrid.FrozenColumns);

  // Open the chooser directly (no modal), uncheck the first frozen column
  // and apply - the same ApplySelection path Execute runs on OK.
  Chooser := TVittixDBGridColumnChooserForm.CreateChooser(nil, FGrid);
  try
    // List order mirrors the grid order: ID is the first item.
    Chooser.SetColumnChecked(0, False);
    Chooser.ApplySelection;
  finally
    Chooser.Free;
  end;

  // Recompute from the Pinned flags: ID is hidden, so only Name remains in
  // the leading pinned visible run.
  FGrid.RefreshFrozenColumns;
  Assert.AreEqual(1, FGrid.FrozenColumns,
    'hiding a frozen column shrinks the frozen region');
  Assert.IsFalse(FGrid.Controller.FindColumnByFieldName('ID').Visible);
end;

procedure TVittixFreezeColumnsTests.ChooserMoveAcrossFrozenBoundaryRejected;
var
  Chooser: TVittixDBGridColumnChooserForm;
begin
  FGrid.FrozenColumns := 2;

  Chooser := TVittixDBGridColumnChooserForm.CreateChooser(nil, FGrid);
  try
    // List index 0 (ID) is frozen; moving it down 1 stays inside. Moving
    // it 3 down crosses the boundary and must be refused.
    Chooser.SelectColumnIndex(0);
    Chooser.MoveSelectedItem(1);
    Assert.AreEqual('Name,ID,Amount,Score,Notes,Created,IsActive', ColumnOrder,
      'reorder inside the frozen region works');

    Chooser.SelectColumnIndex(1);
    Chooser.MoveSelectedItem(3);
    Assert.AreEqual('Name,ID,Amount,Score,Notes,Created,IsActive', ColumnOrder,
      'a frozen item cannot move into the scrolling region');

    Chooser.SelectColumnIndex(4);
    Chooser.MoveSelectedItem(-3);
    Assert.AreEqual('Name,ID,Amount,Score,Notes,Created,IsActive', ColumnOrder,
      'a scrolling item cannot move into the frozen region');
  finally
    Chooser.Free;
  end;
end;

procedure TVittixFreezeColumnsTests.FooterHitTestCoversFirstDataColumnWithIndicator;
var
  Footer: TVittixDBGridFooterPanel;
  FirstCol: TColumn;
  Hit: TColumn;
  R: TRect;
begin
  // M6 regression: TDBGrid.Columns is data-only - the indicator is a fixed
  // column OUTSIDE that list. The footer's paint and hit-test walks must
  // start at Columns[0]; starting at the grid's IndicatorOffset (1 with
  // dgIndicator on) skipped the first data column entirely.
  FGrid.HandleNeeded;
  Assert.IsTrue(dgIndicator in FGrid.Options,
    'precondition: the indicator column is enabled');

  FirstCol := FGrid.Columns[0];
  Footer := TVittixDBGridFooterPanel.Create(nil);
  try
    Footer.Attach(FGrid, FGrid.Controller.AggregationEngine);

    // Probe the middle of the first data column's footer cell.
    R := Footer.GetColumnRect(FirstCol);
    Assert.IsTrue(R.Right > R.Left, 'precondition: column has a footer rect');
    Hit := Footer.HitTestColumn((R.Left + R.Right) div 2);
    Assert.AreSame(FirstCol, Hit,
      'the first data column is hit-tested with the indicator enabled');

    // Its neighbour must not answer for the first column's area.
    if FGrid.Columns.Count > 1 then
    begin
      R := Footer.GetColumnRect(FGrid.Columns[1]);
      Hit := Footer.HitTestColumn(R.Left + 1);
      Assert.AreSame(FGrid.Columns[1], Hit,
        'the second column resolves in its own rect');
    end;
  finally
    Footer.Free;
  end;
end;

procedure TVittixFreezeColumnsTests.FooterHitTestCoversAllColumnsWithoutIndicator;
var
  Footer: TVittixDBGridFooterPanel;
  Hit: TColumn;
  R: TRect;
begin
  FGrid.HandleNeeded;
  FGrid.Options := FGrid.Options - [dgIndicator];
  Assert.IsFalse(dgIndicator in FGrid.Options, 'precondition');
  FGrid.FrozenColumns := 1; // frozen region keeps the walk aligned too

  Footer := TVittixDBGridFooterPanel.Create(nil);
  try
    Footer.Attach(FGrid, FGrid.Controller.AggregationEngine);

    R := Footer.GetColumnRect(FGrid.Columns[0]);
    Assert.IsTrue(R.Right > R.Left, 'precondition: rect visible');
    Hit := Footer.HitTestColumn((R.Left + R.Right) div 2);
    Assert.AreSame(FGrid.Columns[0], Hit,
      'first data column hit without the indicator');
  finally
    Footer.Free;
  end;
end;

end.
