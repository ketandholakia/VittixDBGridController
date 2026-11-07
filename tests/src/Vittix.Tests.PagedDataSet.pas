unit Vittix.Tests.PagedDataSet;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Math,
  Data.DB,
  Datasnap.DBClient,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.DBGrids,
  Vcl.DBCtrls,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.Paging.DataSet,
  Vittix.DBGrid.Paging.DataSet.Controls;

type
  // Grants tests access to the protected KeyDown without touching windows.
  TVittixDBGridCracker = class(TVittixDBGrid);
  TVittixGlobalNavigatorCracker = class(TVittixGlobalNavigator)
  public
    function ButtonEnabled(AButton: TNavigateBtn): Boolean;
  end;

  [TestFixture]
  TVittixPagedDataSetTests = class
  private
    FDataSet: TVittixPagedDataSet;
    FFailPage: Integer;
    FLoadCount: Integer;
    FTotalRows: Integer;
    FRowOffset: Integer;
    FProgressCount: Integer;
    FLastProgressCurrent: Integer;
    FLastProgressTotal: Integer;
    procedure LoadPage(Sender: TObject; APageIndex, APageSize: Integer;
      ADataSet: TClientDataSet);
    procedure FilterEvenIds(ADataSet: TDataSet; var Accept: Boolean);
    procedure RecordExportProgress(Sender: TObject; Current, Total: Integer;
      var Cancel: Boolean);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure PagesOnlyRequestedRowsAndRestoresAfterEnd;
    [Test]
    procedure LoaderFailurePreservesPreviousPage;
    [Test]
    procedure InitialLoaderFailureLeavesDatasetClosed;
    [Test]
    procedure EditPreventsPageChange;
    [Test]
    procedure GridRemainsBoundAcrossPageChanges;
    [Test]
    procedure PagerBarNavigatesAndDetaches;
    [Test]
    procedure AutoPageCrossesForwardAndBackwardBoundaries;
    [Test]
    procedure AutoPageStopsAtExactFullLastPage;
    [Test]
    procedure DisabledControlsKeepScansWithinCurrentPage;
    [Test]
    procedure TotalCountKnownDrivesPageCountAndNext;
    [Test]
    procedure ExactMultipleCountNeedsNoEndProbe;
    [Test]
    procedure EmptyTotalCountShowsSinglePage;
    [Test]
    procedure CountChangeAdjustsBoundaries;
    [Test]
    procedure LastPageNavigatesAndGuards;
    [Test]
    procedure ClearTotalCountRestoresProbeNavigation;
    [Test]
    procedure InvalidTotalCountRaises;
    [Test]
    procedure PagerBarShowsPageOfAndLastButton;
    [Test]
    procedure PagerBarShowsSinglePageForEmptySource;
    [Test]
    procedure VirtualScrollExposesGlobalCounts;
    [Test]
    procedure VirtualRecNoSeekAcrossPages;
    [Test]
    procedure VirtualMoveByCrossesAndClamps;
    [Test]
    procedure VirtualFirstLastStayPageLocal;
    [Test]
    procedure VirtualEmptyTotalCount;
    [Test]
    procedure VirtualLoaderFailureRestoresState;
    [Test]
    procedure VirtualGatingOnTotalCount;
    [Test]
    procedure VirtualDisabledControlsScanStaysInPage;
    [Test]
    procedure GlobalFirstLastMethods;
    [Test]
    procedure NavigatorRoutesGlobalEndpointsAndEnablesButtons;
    [Test]
    procedure NavigatorPreservesPageLocalAndOrdinaryDataSets;
    [Test]
    procedure GridThumbEndpointsAndGlobalSeek;
    [Test]
    procedure GridCtrlHomeEndRouteGlobally;
    [Test]
    procedure VirtualSeekDuringEditRaises;
    [Test]
    procedure VisibleGridScrollbarSpansAllRows;
    [Test]
    procedure CrossPageSelectionDropsStaleRows;
    [Test]
    procedure SamePageReloadInvalidatesOldBookmarks;
    [Test]
    procedure ExportUnderVirtualScrollExportsLoadedPage;
    [Test]
    procedure ExportProgressUsesLoadedPageCount;
    [Test]
    procedure ClientFilterSuspendsVirtualScroll;
    [Test]
    procedure CountShrinkBeyondCursorAtGridLevel;
    [Test]
    procedure FilteredToggleInvalidatesPageBookmarks;
    [Test]
    procedure VirtualScrollOffInvalidatesPageBookmarks;
    [Test]
    procedure MultiSelectDeleteRefusesStaleEntries;
    [Test]
    procedure MixedSelectionDeleteIsAllOrNothing;
    [Test]
    procedure SelectionHasStaleEntriesReportsMixedState;
    [Test]
    procedure SelectionDeleteWithoutStaleEntriesWorks;
    [Test]
    procedure SafeDeleteValidPagedSelection;
    [Test]
    procedure SafeDeleteOrdinaryDataSet;
  end;

implementation

uses
  Vittix.Tests.TestData,
  Vittix.DBGrid.Export.Engine;

function TVittixGlobalNavigatorCracker.ButtonEnabled(
  AButton: TNavigateBtn): Boolean;
begin
  Result := Buttons[AButton].Enabled;
end;

procedure TVittixPagedDataSetTests.Setup;
begin
  FDataSet := TVittixPagedDataSet.Create(nil);
  FDataSet.FieldDefs.Add('id', ftInteger);
  FDataSet.FieldDefs.Add('name', ftString, 40);
  FDataSet.PageSize := 10;
  FDataSet.OnLoadPage := LoadPage;
  FFailPage := -1;
  FLoadCount := 0;
  FTotalRows := 23;
  FRowOffset := 0;
  FProgressCount := 0;
  FLastProgressCurrent := 0;
  FLastProgressTotal := 0;
end;

procedure TVittixPagedDataSetTests.RecordExportProgress(Sender: TObject;
  Current, Total: Integer; var Cancel: Boolean);
begin
  Inc(FProgressCount);
  FLastProgressCurrent := Current;
  FLastProgressTotal := Total;
end;

procedure TVittixPagedDataSetTests.TearDown;
begin
  FDataSet.Free;
end;

procedure TVittixPagedDataSetTests.LoadPage(Sender: TObject;
  APageIndex, APageSize: Integer; ADataSet: TClientDataSet);
var
  I: Integer;
begin
  Inc(FLoadCount);
  if APageIndex = FFailPage then
  begin
    ADataSet.AppendRecord([APageIndex * APageSize + 1, 'Partial row']);
    raise Exception.Create('Simulated page-source failure');
  end;
  for I := APageIndex * APageSize + 1 to
    Min((APageIndex + 1) * APageSize, FTotalRows) do
    ADataSet.AppendRecord([I + FRowOffset, 'Row ' + IntToStr(I)]);
end;

procedure TVittixPagedDataSetTests.PagesOnlyRequestedRowsAndRestoresAfterEnd;
begin
  Assert.IsTrue(FDataSet.OpenPage(0));
  Assert.AreEqual(10, FDataSet.RecordCount);
  Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger);
  Assert.IsTrue(FDataSet.NextPage);
  Assert.AreEqual(11, FDataSet.FieldByName('id').AsInteger);
  Assert.IsTrue(FDataSet.NextPage);
  Assert.AreEqual(2, FDataSet.PageIndex);
  Assert.AreEqual(3, FDataSet.RecordCount);
  Assert.AreEqual(21, FDataSet.FieldByName('id').AsInteger);
  Assert.IsFalse(FDataSet.NextPage);
  Assert.AreEqual(2, FDataSet.PageIndex);
  Assert.AreEqual(21, FDataSet.FieldByName('id').AsInteger);
  Assert.IsTrue(FDataSet.PreviousPage);
  Assert.AreEqual(11, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(5, FLoadCount);
end;

procedure TVittixPagedDataSetTests.LoaderFailurePreservesPreviousPage;
var
  Raised: Boolean;
begin
  Assert.IsTrue(FDataSet.OpenPage(0));
  FFailPage := 1;
  Raised := False;
  try
    FDataSet.NextPage;
  except
    on E: Exception do
      Raised := E.Message = 'Simulated page-source failure';
  end;
  Assert.IsTrue(Raised);
  Assert.AreEqual(0, FDataSet.PageIndex);
  Assert.AreEqual(10, FDataSet.RecordCount);
  Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger);
end;

procedure TVittixPagedDataSetTests.InitialLoaderFailureLeavesDatasetClosed;
var
  Raised: Boolean;
begin
  FFailPage := 0;
  Raised := False;
  try
    FDataSet.OpenPage(0);
  except
    on E: Exception do
      Raised := E.Message = 'Simulated page-source failure';
  end;
  Assert.IsTrue(Raised);
  Assert.IsFalse(FDataSet.Active, 'first-load failure restores closed state');
  Assert.IsFalse(FDataSet.PageLoaded);
  Assert.AreEqual(-1, FDataSet.PageIndex);

  FFailPage := -1;
  Assert.IsTrue(FDataSet.OpenPage(0), 'a later load can retry');
  Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger);
end;

procedure TVittixPagedDataSetTests.EditPreventsPageChange;
var
  Raised: Boolean;
begin
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.Edit;
  try
    Raised := False;
    try
      FDataSet.NextPage;
    except
      on E: EInvalidOperation do
        Raised := True;
    end;
    Assert.IsTrue(Raised);
    Assert.AreEqual(dsEdit, FDataSet.State);
    Assert.AreEqual(0, FDataSet.PageIndex);
  finally
    FDataSet.Cancel;
  end;
end;

procedure TVittixPagedDataSetTests.GridRemainsBoundAcrossPageChanges;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  Assert.IsTrue(FDataSet.OpenPage(0));
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    Assert.IsTrue(FDataSet.NextPage);
    Assert.AreSame(FDataSet, Grid.DataSource.DataSet);
    Assert.AreEqual(11, Grid.DataSource.DataSet.FieldByName('id').AsInteger);
    Assert.IsTrue(Grid.ColumnInfo.Count > 0);
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.PagerBarNavigatesAndDetaches;
var
  Bar: TVittixPagedDataSetBar;
  FirstButton, PreviousButton, NextButton: TButton;
  PageLabel: TLabel;
begin
  Bar := TVittixPagedDataSetBar.Create(nil);
  try
    Bar.DataSet := FDataSet;
    FirstButton := TButton(Bar.FindComponent('FirstButton'));
    PreviousButton := TButton(Bar.FindComponent('PreviousButton'));
    NextButton := TButton(Bar.FindComponent('NextButton'));
    PageLabel := TLabel(Bar.FindComponent('PageLabel'));
    Assert.IsFalse(FirstButton.Enabled);
    Assert.IsTrue(NextButton.Enabled);

    NextButton.Click;
    Assert.AreEqual(0, FDataSet.PageIndex);
    NextButton.Click;
    Assert.AreEqual(1, FDataSet.PageIndex);
    Assert.AreEqual('Page 2', PageLabel.Caption);
    Assert.IsTrue(FirstButton.Enabled);
    Assert.IsTrue(PreviousButton.Enabled);
    Assert.AreEqual(11, FDataSet.FieldByName('id').AsInteger);

    NextButton.Click;
    Assert.AreEqual(2, FDataSet.PageIndex);
    Assert.IsFalse(NextButton.Enabled, 'short final page has no next page');

    FDataSet.AutoPageOnScroll := True;
    FDataSet.Prior;
    Assert.AreEqual('Page 2', PageLabel.Caption,
      'scrolling updates the bar without a button click');

    FirstButton.Click;
    Assert.AreEqual(0, FDataSet.PageIndex);
    FreeAndNil(FDataSet);
    Assert.IsFalse(NextButton.Enabled);
  finally
    Bar.Free;
  end;
end;

procedure TVittixPagedDataSetTests.AutoPageCrossesForwardAndBackwardBoundaries;
var
  I: Integer;
begin
  FDataSet.AutoPageOnScroll := True;
  Assert.IsTrue(FDataSet.OpenPage(0));
  for I := 1 to 9 do
    FDataSet.Next;
  Assert.AreEqual(10, FDataSet.FieldByName('id').AsInteger);
  FDataSet.Next;
  Assert.AreEqual(1, FDataSet.PageIndex);
  Assert.AreEqual(11, FDataSet.FieldByName('id').AsInteger);

  FDataSet.Prior;
  Assert.AreEqual(0, FDataSet.PageIndex);
  Assert.AreEqual(10, FDataSet.FieldByName('id').AsInteger);

  Assert.AreEqual(13, FDataSet.MoveBy(13));
  Assert.AreEqual(2, FDataSet.PageIndex);
  Assert.AreEqual(23, FDataSet.FieldByName('id').AsInteger);
  Assert.IsFalse(FDataSet.HasMore);
  Assert.AreEqual(5, FLoadCount);
  FDataSet.Next;
  Assert.AreEqual(23, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(5, FLoadCount, 'short final page needs no empty probe');
end;

procedure TVittixPagedDataSetTests.AutoPageStopsAtExactFullLastPage;
var
  I: Integer;
begin
  FTotalRows := 20;
  FDataSet.AutoPageOnScroll := True;
  Assert.IsTrue(FDataSet.OpenPage(1));
  for I := 1 to 9 do
    FDataSet.Next;
  Assert.AreEqual(20, FDataSet.FieldByName('id').AsInteger);
  FDataSet.Next;
  Assert.AreEqual(1, FDataSet.PageIndex);
  Assert.AreEqual(20, FDataSet.FieldByName('id').AsInteger,
    'empty next page preserves the current record');
  Assert.IsTrue(FDataSet.Eof, 'an EOF loop must terminate');
  Assert.IsFalse(FDataSet.HasMore);
  Assert.AreEqual(2, FLoadCount);
  FDataSet.Next;
  Assert.AreEqual(2, FLoadCount, 'known end does not reload');
end;

procedure TVittixPagedDataSetTests.DisabledControlsKeepScansWithinCurrentPage;
var
  Count: Integer;
begin
  FDataSet.AutoPageOnScroll := True;
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.DisableControls;
  try
    Count := 0;
    FDataSet.First;
    while not FDataSet.Eof do
    begin
      Inc(Count);
      FDataSet.Next;
    end;
    Assert.AreEqual(10, Count);
    Assert.AreEqual(0, FDataSet.PageIndex);
    Assert.AreEqual(1, FLoadCount);
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TVittixPagedDataSetTests.TotalCountKnownDrivesPageCountAndNext;
begin
  Assert.IsFalse(FDataSet.TotalCountKnown);
  Assert.AreEqual(-1, FDataSet.PageCount);
  FDataSet.TotalCount := 23;
  Assert.IsTrue(FDataSet.TotalCountKnown);
  Assert.AreEqual(3, FDataSet.PageCount);
  Assert.IsTrue(FDataSet.OpenPage(0));
  Assert.IsTrue(FDataSet.HasMore);
  Assert.IsTrue(FDataSet.NextPage);
  Assert.IsTrue(FDataSet.NextPage);
  Assert.AreEqual(2, FDataSet.PageIndex);
  Assert.AreEqual(21, FDataSet.FieldByName('id').AsInteger);
  Assert.IsFalse(FDataSet.HasMore);
  Assert.IsFalse(FDataSet.NextPage,
    'a known last page short-circuits without an empty-page probe');
  Assert.AreEqual(3, FLoadCount);
end;

procedure TVittixPagedDataSetTests.ExactMultipleCountNeedsNoEndProbe;
var
  I: Integer;
begin
  FTotalRows := 20;
  FDataSet.TotalCount := 20;
  Assert.AreEqual(2, FDataSet.PageCount);
  FDataSet.AutoPageOnScroll := True;
  Assert.IsTrue(FDataSet.OpenPage(0));
  for I := 1 to 19 do
    FDataSet.Next;
  Assert.AreEqual(1, FDataSet.PageIndex);
  Assert.AreEqual(20, FDataSet.FieldByName('id').AsInteger);
  FDataSet.Next;
  Assert.IsFalse(FDataSet.HasMore,
    'an exact multiple ends at the counted page boundary');
  Assert.AreEqual(2, FLoadCount,
    'the counted boundary replaces the empty-page probe');
end;

procedure TVittixPagedDataSetTests.EmptyTotalCountShowsSinglePage;
begin
  FTotalRows := 0;
  FDataSet.TotalCount := 0;
  Assert.IsTrue(FDataSet.TotalCountKnown, 'zero is a known count');
  Assert.AreEqual(1, FDataSet.PageCount);
  Assert.IsTrue(FDataSet.OpenPage(0), 'page zero may be empty');
  Assert.AreEqual(0, FDataSet.RecordCount);
  Assert.IsFalse(FDataSet.HasMore);
  Assert.IsFalse(FDataSet.NextPage);
  Assert.AreEqual(1, FLoadCount);
end;

procedure TVittixPagedDataSetTests.CountChangeAdjustsBoundaries;
begin
  FTotalRows := 30;
  FDataSet.TotalCount := 30;
  Assert.IsTrue(FDataSet.OpenPage(2));
  Assert.AreEqual(2, FDataSet.PageIndex);
  Assert.IsFalse(FDataSet.HasMore, 'page 3 of 3 is the counted last page');

  FDataSet.TotalCount := 12; // rows were deleted; the last page disappeared
  Assert.AreEqual(2, FDataSet.PageCount);
  Assert.IsFalse(FDataSet.HasMore);
  Assert.IsFalse(FDataSet.NextPage,
    'a shrunken count stops navigation beyond the new last page');

  Assert.IsTrue(FDataSet.PreviousPage);
  Assert.AreEqual(1, FDataSet.PageIndex);

  FDataSet.TotalCount := 45; // rows were added
  Assert.AreEqual(5, FDataSet.PageCount);
  Assert.IsTrue(FDataSet.HasMore, 'a grown count re-enables the next page');
  Assert.IsTrue(FDataSet.NextPage);
  Assert.AreEqual(2, FDataSet.PageIndex);
  Assert.AreEqual(3, FLoadCount);
end;

procedure TVittixPagedDataSetTests.LastPageNavigatesAndGuards;
begin
  Assert.IsFalse(FDataSet.LastPage,
    'an unknown count has no last page to open');
  FDataSet.TotalCount := 23;
  Assert.IsTrue(FDataSet.LastPage);
  Assert.AreEqual(2, FDataSet.PageIndex);
  Assert.AreEqual(21, FDataSet.FieldByName('id').AsInteger);
  Assert.IsFalse(FDataSet.NextPage);
  Assert.AreEqual(1, FLoadCount,
    'the final page opens directly without loading earlier pages');
end;

procedure TVittixPagedDataSetTests.ClearTotalCountRestoresProbeNavigation;
var
  I: Integer;
begin
  FTotalRows := 10;
  FDataSet.TotalCount := 10;
  FDataSet.AutoPageOnScroll := True;
  Assert.IsTrue(FDataSet.OpenPage(0));
  Assert.IsFalse(FDataSet.HasMore, 'the single counted page ends here');
  Assert.AreEqual(1, FLoadCount);

  FDataSet.TotalCount := -1;
  Assert.IsFalse(FDataSet.TotalCountKnown);
  Assert.AreEqual(-1, FDataSet.PageCount);
  Assert.IsTrue(FDataSet.HasMore,
    'clearing the count restores the probe-based has-more state');

  for I := 1 to 9 do
    FDataSet.Next;
  FDataSet.Next; // EOF probes page one, finds it empty, and rolls back
  Assert.AreEqual(2, FLoadCount, 'the probe ran after the count was cleared');
  Assert.IsFalse(FDataSet.HasMore);
  Assert.AreEqual(0, FDataSet.PageIndex);
  Assert.AreEqual(10, FDataSet.FieldByName('id').AsInteger);
end;

procedure TVittixPagedDataSetTests.InvalidTotalCountRaises;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    FDataSet.TotalCount := -2;
  except
    on E: EArgumentOutOfRangeException do
      Raised := True;
  end;
  Assert.IsTrue(Raised);
  Assert.IsFalse(FDataSet.TotalCountKnown);
end;

procedure TVittixPagedDataSetTests.PagerBarShowsPageOfAndLastButton;
var
  Bar: TVittixPagedDataSetBar;
  LastButton: TButton;
  NextButton: TButton;
  PageLabel: TLabel;
begin
  Bar := TVittixPagedDataSetBar.Create(nil);
  try
    Bar.DataSet := FDataSet;
    NextButton := TButton(Bar.FindComponent('NextButton'));
    LastButton := TButton(Bar.FindComponent('LastButton'));
    PageLabel := TLabel(Bar.FindComponent('PageLabel'));
    Assert.IsFalse(LastButton.Enabled,
      'the count is unknown, so Last stays disabled');
    Assert.IsTrue(NextButton.Enabled);

    NextButton.Click;
    Assert.AreEqual('Page 1', PageLabel.Caption);
    NextButton.Click;
    Assert.AreEqual('Page 2', PageLabel.Caption);
    Assert.AreEqual(1, FDataSet.PageIndex);

    FDataSet.TotalCount := 23;
    Assert.AreEqual('Page 2 of 3', PageLabel.Caption,
      'a count change refreshes the caption through the page observer');
    Assert.IsTrue(LastButton.Enabled);

    LastButton.Click;
    Assert.AreEqual(2, FDataSet.PageIndex);
    Assert.AreEqual(21, FDataSet.FieldByName('id').AsInteger);
    Assert.AreEqual('Page 3 of 3', PageLabel.Caption);
    Assert.IsFalse(LastButton.Enabled);
    Assert.IsFalse(NextButton.Enabled);
  finally
    Bar.Free;
  end;
end;

procedure TVittixPagedDataSetTests.PagerBarShowsSinglePageForEmptySource;
var
  Bar: TVittixPagedDataSetBar;
  FirstButton: TButton;
  LastButton: TButton;
  NextButton: TButton;
  PageLabel: TLabel;
begin
  FTotalRows := 0;
  FDataSet.TotalCount := 0;
  Bar := TVittixPagedDataSetBar.Create(nil);
  try
    Bar.DataSet := FDataSet;
    FirstButton := TButton(Bar.FindComponent('FirstButton'));
    NextButton := TButton(Bar.FindComponent('NextButton'));
    LastButton := TButton(Bar.FindComponent('LastButton'));
    PageLabel := TLabel(Bar.FindComponent('PageLabel'));

    NextButton.Click;
    Assert.AreEqual(0, FDataSet.PageIndex);
    Assert.AreEqual('Page 1 of 1', PageLabel.Caption,
      'an empty source still shows its single page');
    Assert.IsFalse(NextButton.Enabled);
    Assert.IsFalse(LastButton.Enabled);
    Assert.IsFalse(FirstButton.Enabled);
  finally
    Bar.Free;
  end;
end;

procedure TVittixPagedDataSetTests.VirtualScrollExposesGlobalCounts;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.AreEqual(230, FDataSet.RecordCount,
    'a known count replaces the loaded-page count');
  Assert.IsTrue(FDataSet.OpenPage(0));
  Assert.AreEqual(230, FDataSet.RecordCount);
  Assert.AreEqual(1, FDataSet.RecNo);
  FDataSet.Next;
  Assert.AreEqual(2, FDataSet.RecNo);
  Assert.IsTrue(FDataSet.NextPage);
  Assert.AreEqual(11, FDataSet.RecNo, 'RecNo is the global row position');

  FDataSet.VirtualScroll := False;
  Assert.AreEqual(10, FDataSet.RecordCount,
    'disabling virtual scroll restores page-local counts');
  Assert.AreEqual(1, FDataSet.RecNo);
end;

procedure TVittixPagedDataSetTests.VirtualRecNoSeekAcrossPages;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  Assert.AreEqual(1, FLoadCount);

  FDataSet.RecNo := 150;
  Assert.AreEqual(14, FDataSet.PageIndex);
  Assert.AreEqual(150, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(150, FDataSet.RecNo);
  Assert.AreEqual(2, FLoadCount);

  FDataSet.RecNo := 145; // inside the loaded page: no reload
  Assert.AreEqual(14, FDataSet.PageIndex);
  Assert.AreEqual(145, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(2, FLoadCount);

  FDataSet.RecNo := 5;
  Assert.AreEqual(0, FDataSet.PageIndex);
  Assert.AreEqual(5, FDataSet.FieldByName('id').AsInteger);

  FDataSet.RecNo := 9999; // clamps to the known total
  Assert.AreEqual(230, FDataSet.RecNo);
  Assert.AreEqual(230, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(4, FLoadCount);
end;

procedure TVittixPagedDataSetTests.VirtualMoveByCrossesAndClamps;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));

  Assert.AreEqual(15, FDataSet.MoveBy(15),
    'virtual scroll crosses pages without AutoPageOnScroll');
  Assert.AreEqual(16, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(16, FDataSet.RecNo);

  Assert.AreEqual(214, FDataSet.MoveBy(10000),
    'a huge distance clamps at the counted end and reports real movement');
  Assert.AreEqual(230, FDataSet.FieldByName('id').AsInteger);

  Assert.AreEqual(0, FDataSet.MoveBy(5), 'the counted end has no next row');

  Assert.AreEqual(-229, FDataSet.MoveBy(-10000));
  Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger);
end;

procedure TVittixPagedDataSetTests.VirtualFirstLastStayPageLocal;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.RecNo := 141;
  Assert.AreEqual(2, FLoadCount);

  FDataSet.First; // non-virtual TDataSet.First: stays on the loaded page
  Assert.AreEqual(141, FDataSet.RecNo,
    'First is page-local even in virtual mode (documented boundary)');
  Assert.AreEqual(141, FDataSet.FieldByName('id').AsInteger);

  FDataSet.Last;
  Assert.AreEqual(150, FDataSet.RecNo);
  Assert.AreEqual(150, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(2, FLoadCount, 'First/Last must not reload pages');
end;

procedure TVittixPagedDataSetTests.VirtualEmptyTotalCount;
begin
  FTotalRows := 0;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 0;
  Assert.AreEqual(0, FDataSet.RecordCount);
  Assert.IsTrue(FDataSet.OpenPage(0), 'page zero may be empty');
  Assert.AreEqual(0, FDataSet.RecordCount);
  FDataSet.RecNo := 5; // no rows to seek to: ignored
  Assert.AreEqual(0, FDataSet.RecNo,
    'an empty page adopted via the atomic swap reports RecNo 0');
  FDataSet.GlobalLast; // safe on an empty source
  Assert.AreEqual(0, FDataSet.PageIndex);
  FDataSet.GlobalFirst;
  Assert.AreEqual(0, FDataSet.PageIndex);
end;

procedure TVittixPagedDataSetTests.VirtualLoaderFailureRestoresState;
var
  Raised: Boolean;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  FFailPage := 14;
  Raised := False;
  try
    FDataSet.RecNo := 150;
  except
    on E: Exception do
      Raised := E.Message = 'Simulated page-source failure';
  end;
  Assert.IsTrue(Raised);
  Assert.AreEqual(0, FDataSet.PageIndex);
  Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(1, FDataSet.RecNo);
  Assert.AreEqual(230, FDataSet.RecordCount,
    'the virtual count survives a failed seek');
end;

procedure TVittixPagedDataSetTests.VirtualGatingOnTotalCount;
begin
  FDataSet.VirtualScroll := True;
  Assert.IsFalse(FDataSet.GlobalScrollActive,
    'virtual scroll needs a known total count');
  Assert.IsTrue(FDataSet.OpenPage(0));
  Assert.AreEqual(10, FDataSet.RecordCount);
  Assert.AreEqual(1, FDataSet.RecNo);
  Assert.AreEqual(9, FDataSet.MoveBy(50),
    'without a total, navigation stays inside the loaded page');
  Assert.AreEqual(10, FDataSet.FieldByName('id').AsInteger);
end;

procedure TVittixPagedDataSetTests.VirtualDisabledControlsScanStaysInPage;
var
  Count: Integer;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.DisableControls;
  try
    Count := 0;
    FDataSet.First;
    while not FDataSet.Eof do
    begin
      Inc(Count);
      FDataSet.Next;
    end;
    Assert.AreEqual(10, Count, 'disabled-control scans stay on the page');
    Assert.AreEqual(0, FDataSet.PageIndex);
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TVittixPagedDataSetTests.GlobalFirstLastMethods;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.AreEqual(230, FDataSet.GlobalRecordCount);
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.RecNo := 100;
  Assert.AreEqual(2, FLoadCount);

  FDataSet.GlobalFirst;
  Assert.AreEqual(0, FDataSet.PageIndex);
  Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(3, FLoadCount);

  FDataSet.GlobalLast;
  Assert.AreEqual(22, FDataSet.PageIndex);
  Assert.AreEqual(230, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(230, FDataSet.RecNo);
  Assert.AreEqual(4, FLoadCount);

  FDataSet.GlobalLast;
  Assert.AreEqual(230, FDataSet.FieldByName('id').AsInteger);
  Assert.AreEqual(4, FLoadCount, 'the last page is already loaded');

  // Without virtual scroll, GlobalLast falls back to the page-local end.
  FDataSet.VirtualScroll := False;
  FDataSet.GlobalLast;
  Assert.AreEqual(230, FDataSet.FieldByName('id').AsInteger,
    'page 22 holds rows 221..230 and Last is its final row');
  Assert.AreEqual(4, FLoadCount);
end;

// Keep the navigator tests alongside the global seek tests: they exercise
// VCL button state as well as the action routed by BtnClick.
procedure TVittixPagedDataSetTests.NavigatorRoutesGlobalEndpointsAndEnablesButtons;
var
  Source: TDataSource;
  Navigator: TVittixGlobalNavigatorCracker;
  OwnerForm: TForm;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  OwnerForm := TForm.CreateNew(nil);
  Source := TDataSource.Create(nil);
  Navigator := TVittixGlobalNavigatorCracker.Create(OwnerForm);
  try
    Navigator.Parent := OwnerForm;
    Navigator.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast];
    Source.DataSet := FDataSet;
    Navigator.DataSource := Source;
    Assert.IsTrue(FDataSet.OpenPage(0));
    Assert.IsFalse(Navigator.ButtonEnabled(nbFirst));
    Assert.IsTrue(Navigator.ButtonEnabled(nbLast));
    FDataSet.RecNo := 100;
    Assert.IsTrue(Navigator.ButtonEnabled(nbFirst),
      'First remains enabled at the start of an interior page');
    Navigator.Enabled := False;
    Assert.IsFalse(Navigator.ButtonEnabled(nbFirst));
    Navigator.Enabled := True;
    Assert.IsTrue(Navigator.ButtonEnabled(nbFirst),
      're-enabling restores the global First button');
    Navigator.BtnClick(nbFirst);
    Assert.AreEqual(1, FDataSet.RecNo);
    Assert.AreEqual(0, FDataSet.PageIndex);
    Navigator.BtnClick(nbLast);
    Assert.AreEqual(230, FDataSet.RecNo);
    Assert.AreEqual(22, FDataSet.PageIndex);
    Assert.IsTrue(Navigator.ButtonEnabled(nbFirst));
    Assert.IsFalse(Navigator.ButtonEnabled(nbLast));
  finally
    Navigator.Free;
    Source.Free;
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.NavigatorPreservesPageLocalAndOrdinaryDataSets;
var
  Source: TDataSource;
  Navigator: TVittixGlobalNavigatorCracker;
  Ordinary: TClientDataSet;
  OwnerForm: TForm;
begin
  FTotalRows := 230;
  FDataSet.TotalCount := 230;
  FDataSet.VirtualScroll := False;
  OwnerForm := TForm.CreateNew(nil);
  Source := TDataSource.Create(nil);
  Navigator := TVittixGlobalNavigatorCracker.Create(OwnerForm);
  Ordinary := TClientDataSet.Create(nil);
  try
    Navigator.Parent := OwnerForm;
    Navigator.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast];
    Source.DataSet := FDataSet;
    Navigator.DataSource := Source;
    Assert.IsTrue(FDataSet.OpenPage(1));
    Navigator.BtnClick(nbLast);
    Assert.AreEqual(20, FDataSet.FieldByName('id').AsInteger);
    Navigator.BtnClick(nbFirst);
    Assert.AreEqual(11, FDataSet.FieldByName('id').AsInteger);
    Assert.IsFalse(Navigator.ButtonEnabled(nbFirst));

    Ordinary.FieldDefs.Add('id', ftInteger);
    Ordinary.CreateDataSet;
    Ordinary.AppendRecord([1]);
    Ordinary.AppendRecord([2]);
    Ordinary.AppendRecord([3]);
    Source.DataSet := Ordinary;
    Navigator.BtnClick(nbFirst);
    Assert.AreEqual(1, Ordinary.FieldByName('id').AsInteger);
    Navigator.BtnClick(nbLast);
    Assert.AreEqual(3, Ordinary.FieldByName('id').AsInteger);
  finally
    Navigator.Free;
    Source.Free;
    Ordinary.Free;
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.GridThumbEndpointsAndGlobalSeek;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.RecNo := 150;
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    SendMessage(Grid.Handle, WM_VSCROLL, MakeWParam(SB_TOP, 0), 0);
    Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger,
      'SB_TOP reaches the first global row');
    Assert.AreEqual(0, FDataSet.PageIndex);

    SendMessage(Grid.Handle, WM_VSCROLL, MakeWParam(SB_BOTTOM, 0), 0);
    Assert.AreEqual(230, FDataSet.FieldByName('id').AsInteger,
      'SB_BOTTOM reaches the last global row');
    Assert.AreEqual(22, FDataSet.PageIndex);

    FDataSet.RecNo := 100; // the same path a mid-range thumb seek takes
    Assert.AreEqual(100, Grid.DataSource.DataSet.FieldByName('id').AsInteger);
    Assert.AreSame(FDataSet, Grid.DataSource.DataSet);
    Assert.IsTrue(Grid.ColumnInfo.Count > 0);
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.GridCtrlHomeEndRouteGlobally;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Key: Word;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.RecNo := 150;
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    Key := VK_HOME;
    TVittixDBGridCracker(Grid).KeyDown(Key, [ssCtrl]);
    Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger,
      'Ctrl+Home reaches the first global row');

    Key := VK_END;
    TVittixDBGridCracker(Grid).KeyDown(Key, [ssCtrl]);
    Assert.AreEqual(230, FDataSet.FieldByName('id').AsInteger,
      'Ctrl+End reaches the last global row');

    // Without virtual mode the inherited page-local behavior is untouched.
    FDataSet.VirtualScroll := False;
    Assert.IsTrue(FDataSet.OpenPage(1));
    Assert.AreEqual(11, FDataSet.FieldByName('id').AsInteger);
    Key := VK_HOME;
    TVittixDBGridCracker(Grid).KeyDown(Key, [ssCtrl]);
    Assert.AreEqual(11, FDataSet.FieldByName('id').AsInteger,
      'without virtual mode Ctrl+Home stays page-local');

    FDataSet.VirtualScroll := True;
    Key := VK_HOME;
    TVittixDBGridCracker(Grid).KeyDown(Key, [ssCtrl]);
    Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger);
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.FilterEvenIds(ADataSet: TDataSet;
  var Accept: Boolean);
begin
  Accept := ADataSet.FieldByName('id').AsInteger mod 2 = 0;
end;

procedure TVittixPagedDataSetTests.VirtualSeekDuringEditRaises;
var
  Raised: Boolean;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.Edit;
  try
    Raised := False;
    try
      FDataSet.RecNo := 150;
    except
      on E: EInvalidOperation do
        Raised := True;
    end;
    Assert.IsTrue(Raised, 'a cross-page seek during an edit must be refused');
    Assert.AreEqual(dsEdit, FDataSet.State);
    Assert.AreEqual(0, FDataSet.PageIndex);
  finally
    FDataSet.Cancel;
  end;
end;

procedure TVittixPagedDataSetTests.VisibleGridScrollbarSpansAllRows;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  SI: TScrollInfo;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    OwnerForm.Show; // a real, visible window for the real scrollbar
    Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger);

    FDataSet.RecNo := 100; // the seek fires the events that update the bar
    SI.cbSize := SizeOf(SI);
    SI.fMask := SIF_ALL;
    Assert.IsTrue(GetScrollInfo(Grid.Handle, SB_VERT, SI));
    Assert.AreEqual(230, SI.nMax - Integer(SI.nPage) + 1,
      'the visible scrollbar range spans every source row');
    Assert.AreEqual(100, SI.nPos,
      'the scrollbar position tracks the global RecNo');

    SendMessage(Grid.Handle, WM_VSCROLL, MakeWParam(SB_TOP, 0), 0);
    Assert.AreEqual(1, FDataSet.RecNo,
      'SB_TOP still routes to the first global row on a visible grid');
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.CrossPageSelectionDropsStaleRows;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  OldBookmark: TBookmark;
  Raised: Boolean;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    Grid.Options := Grid.Options + [dgMultiSelect];
    Grid.SelectedRows.CurrentRowSelected := True; // select global row 1
    OldBookmark := FDataSet.GetBookmark;

    FDataSet.RecNo := 150; // page switch: row 1's buffers are gone
    Assert.IsFalse(FDataSet.BookmarkValid(OldBookmark),
      'a bookmark from a replaced page is invalid');
    Raised := False;
    try
      FDataSet.GotoBookmark(OldBookmark);
    except
      on E: EDatabaseError do
        Raised := True;
    end;
    Assert.IsTrue(Raised,
      'a stale page bookmark raises instead of silently repositioning');

    // A bookmark taken on the current page still round-trips exactly.
    OldBookmark := FDataSet.GetBookmark; // global row 150
    FDataSet.RecNo := 145;
    FDataSet.GotoBookmark(OldBookmark);
    Assert.AreEqual(150, FDataSet.FieldByName('id').AsInteger,
      'a same-page bookmark positions exactly');

    // Grid selection survives page switches, and the grid's prune path
    // (TBookmarkList.Refresh) drops entries from replaced pages.
    Grid.SelectedRows.CurrentRowSelected := True; // select global row 150
    FDataSet.RecNo := 230;
    Assert.IsTrue(Grid.SelectedRows.Count >= 1,
      'selection survives the page switch without exceptions');
    Assert.IsFalse(FDataSet.BookmarkValid(Grid.SelectedRows[0]),
      'selection entries from replaced pages report invalid');
    Grid.SelectedRows.Refresh;
    Assert.AreEqual(0, Grid.SelectedRows.Count,
      'Refresh prunes selections from replaced pages');

    Assert.AreEqual(230, FDataSet.FieldByName('id').AsInteger);
    Assert.AreSame(FDataSet, Grid.DataSource.DataSet);
    Assert.IsTrue(Grid.ColumnInfo.Count > 0);
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.SamePageReloadInvalidatesOldBookmarks;
var
  OldBookmark, NewBookmark: TBookmark;
  Raised: Boolean;
begin
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := FTotalRows;
  Assert.IsTrue(FDataSet.OpenPage(0));
  OldBookmark := FDataSet.GetBookmark;
  Assert.IsTrue(FDataSet.BookmarkValid(OldBookmark));

  FRowOffset := 1000; // same page index now contains different source rows
  Assert.IsTrue(FDataSet.OpenPage(0));
  Assert.AreEqual(1001, FDataSet.FieldByName('id').AsInteger);
  Assert.IsFalse(FDataSet.BookmarkValid(OldBookmark),
    'reloading the same page invalidates its old row positions');
  Raised := False;
  try
    FDataSet.GotoBookmark(OldBookmark);
  except
    on E: EDatabaseError do
      Raised := True;
  end;
  Assert.IsTrue(Raised, 'stale bookmark must not select a different row');

  NewBookmark := FDataSet.GetBookmark;
  Assert.IsTrue(FDataSet.BookmarkValid(NewBookmark));
  Assert.IsTrue(FDataSet.CompareBookmarks(OldBookmark, NewBookmark) <> 0);
end;

procedure TVittixPagedDataSetTests.ExportUnderVirtualScrollExportsLoadedPage;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Exporter: TVittixDBGridExporter;
  ExporterRows: TStringList;
  CSV: string;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(1)); // global rows 11..20
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  ExporterRows := TStringList.Create;
  try
    Exporter := TVittixDBGridExporter.Create(Grid);
    try
      CSV := Exporter.ExportToString(vefCSV);
    finally
      Exporter.Free;
    end;
    ExporterRows.Text := CSV;
    Assert.AreEqual(11, ExporterRows.Count,
      'header plus exactly the ten loaded page rows');
    Assert.IsTrue(ExporterRows.IndexOf('11,Row 11') >= 0, 'page start exported');
    Assert.IsTrue(ExporterRows.IndexOf('20,Row 20') >= 0, 'page end exported');
    Assert.IsTrue(ExporterRows.IndexOf('21,Row 21') < 0, 'next page excluded');
    Assert.IsTrue(ExporterRows.IndexOf('10,Row 10') < 0, 'prior page excluded');
    Assert.AreEqual(11, FDataSet.FieldByName('id').AsInteger,
      'the export restores the pre-export cursor position');
  finally
    ExporterRows.Free;
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.ExportProgressUsesLoadedPageCount;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Exporter: TVittixDBGridExporter;
  Stream: TMemoryStream;
begin
  FTotalRows := 230;
  FDataSet.PageSize := 120;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(1)); // partial final page: rows 121..230
  Assert.AreEqual(110, FDataSet.LoadedRecordCount);
  FDataSet.RecNo := 125;
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    Exporter := TVittixDBGridExporter.Create(Grid);
    try
      Exporter.OnProgress := RecordExportProgress;
      Exporter.ExportToString(vefCSV);
      Assert.AreEqual(1, FProgressCount);
      Assert.AreEqual(100, FLastProgressCurrent);
      Assert.AreEqual(110, FLastProgressTotal,
        'progress total is the loaded page, not the global 230 rows');
      Assert.AreEqual(125, FDataSet.RecNo, 'export preserves the cursor');
      Assert.AreEqual(1, FLoadCount, 'export does not load another page');

      FProgressCount := 0;
      Stream := TMemoryStream.Create;
      try
        Exporter.ExportToStream(Stream, vefExcelXLSX);
      finally
        Stream.Free;
      end;
      Assert.AreEqual(1, FProgressCount);
      Assert.AreEqual(110, FLastProgressTotal,
        'XLSX uses the same page-local total');
      Assert.AreEqual(125, FDataSet.RecNo);
      Assert.AreEqual(1, FLoadCount);

      FProgressCount := 0;
      Exporter.Options.MaxRows := 105;
      Exporter.ExportToString(vefCSV);
      Assert.AreEqual(1, FProgressCount);
      Assert.AreEqual(105, FLastProgressTotal,
        'MaxRows caps the page-local total');
      Assert.AreEqual(125, FDataSet.RecNo);
      Assert.AreEqual(1, FLoadCount);
    finally
      Exporter.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.ClientFilterSuspendsVirtualScroll;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  Assert.IsTrue(FDataSet.GlobalScrollActive);

  FDataSet.OnFilterRecord := FilterEvenIds;
  FDataSet.Filtered := True;
  Assert.IsFalse(FDataSet.GlobalScrollActive,
    'a client filter suspends virtual mode');
  Assert.AreEqual(5, FDataSet.RecordCount,
    'the filtered page shows its own honest scale');
  Assert.IsTrue(FDataSet.Filtered);

  FDataSet.Filtered := False;
  Assert.IsTrue(FDataSet.GlobalScrollActive,
    'clearing the filter resumes virtual mode');
  Assert.AreEqual(230, FDataSet.RecordCount);
end;

procedure TVittixPagedDataSetTests.CountShrinkBeyondCursorAtGridLevel;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    FDataSet.RecNo := 220; // page 21, rows 211..220
    Assert.AreEqual(21, FDataSet.PageIndex);

    FDataSet.TotalCount := 25; // rows deleted upstream: three pages now
    Assert.AreEqual(25, FDataSet.RecordCount);
    Assert.AreEqual(220, FDataSet.RecNo,
      'the cursor stays put until the next navigation (documented)');
    Assert.IsFalse(FDataSet.HasMore);
    Assert.AreSame(FDataSet, Grid.DataSource.DataSet,
      'the grid survives a cursor beyond the counted boundary');

    FDataSet.RecNo := 12;
    Assert.AreEqual(12, FDataSet.FieldByName('id').AsInteger);

    FDataSet.TotalCount := 45; // rows added upstream
    Assert.AreEqual(5, FDataSet.PageCount);
    Assert.IsTrue(FDataSet.HasMore);
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.FilteredToggleInvalidatesPageBookmarks;
var
  Bookmark: TBookmark;
  Raised: Boolean;
begin
  // Reproduction of the wrong-row seam: a page-scoped bookmark taken
  // before a Filtered toggle names a local row whose meaning changes in
  // the filtered view. Using it must read as stale, never reposition.
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.RecNo := 1; // global row 1 = id 1 (odd: filtered out below)
  Bookmark := FDataSet.GetBookmark;
  Assert.IsTrue(FDataSet.BookmarkValid(Bookmark));

  FDataSet.OnFilterRecord := FilterEvenIds;
  FDataSet.Filtered := True; // visible view of the page: ids 2,4,6,8,10
  Assert.IsFalse(FDataSet.GlobalScrollActive);

  Assert.IsFalse(FDataSet.BookmarkValid(Bookmark),
    Format('a page bookmark must not stay valid across a Filtered toggle ' +
      '(it would name filtered-view row 1 = id 2, not the bookmarked id 1; ' +
      'valid=%s)', [BoolToStr(FDataSet.BookmarkValid(Bookmark), True)]));
  Raised := False;
  try
    FDataSet.GotoBookmark(Bookmark);
  except
    on E: EDatabaseError do
      Raised := True;
  end;
  Assert.IsTrue(Raised,
    Format('a stale bookmark must raise instead of repositioning in the ' +
      'filtered view (cursor now on id %d)',
      [FDataSet.FieldByName('id').AsInteger]));

  // The page was never reloaded, so clearing the filter restores both the
  // mode and the bookmark.
  FDataSet.Filtered := False;
  Assert.IsTrue(FDataSet.BookmarkValid(Bookmark),
    'clearing the filter restores the bookmark without a reload');
  FDataSet.GotoBookmark(Bookmark);
  Assert.AreEqual(1, FDataSet.FieldByName('id').AsInteger,
    'the restored bookmark positions on the original row');
end;

procedure TVittixPagedDataSetTests.VirtualScrollOffInvalidatesPageBookmarks;
var
  Bookmark: TBookmark;
  Raised: Boolean;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  FDataSet.RecNo := 3;
  Bookmark := FDataSet.GetBookmark;
  Assert.IsTrue(FDataSet.BookmarkValid(Bookmark));

  FDataSet.VirtualScroll := False;
  Assert.IsFalse(FDataSet.BookmarkValid(Bookmark),
    'page-scoped bookmarks require the virtual mode that minted them');
  Raised := False;
  try
    FDataSet.GotoBookmark(Bookmark);
  except
    on E: EDatabaseError do
      Raised := True;
  end;
  Assert.IsTrue(Raised);
  Assert.AreEqual(3, FDataSet.FieldByName('id').AsInteger,
    'the refused goto leaves the cursor untouched');

  // Native bookmarks keep native semantics while the mode is off.
  FDataSet.RecNo := 5;
  Bookmark := FDataSet.GetBookmark;
  FDataSet.RecNo := 1;
  FDataSet.GotoBookmark(Bookmark);
  Assert.AreEqual(5, FDataSet.FieldByName('id').AsInteger,
    'native bookmarks round-trip when virtual mode is off');
end;

procedure TVittixPagedDataSetTests.MultiSelectDeleteRefusesStaleEntries;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Raised: Boolean;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    Grid.Options := Grid.Options + [dgMultiSelect];
    FDataSet.RecNo := 1;
    Grid.SelectedRows.CurrentRowSelected := True;
    FDataSet.RecNo := 2;
    Grid.SelectedRows.CurrentRowSelected := True;
    FDataSet.RecNo := 3;
    Grid.SelectedRows.CurrentRowSelected := True;
    Assert.AreEqual(3, Grid.SelectedRows.Count);

    FDataSet.RecNo := 150; // page 14 replaces the selection's page
    Raised := False;
    try
      Grid.SelectedRows.Delete;
    except
      on E: EDatabaseError do
        Raised := True;
    end;
    Assert.IsTrue(Raised,
      'deleting a cross-page selection must refuse instead of guessing rows');
    FDataSet.VirtualScroll := False; // page-local scale for the count check
    Assert.AreEqual(10, FDataSet.RecordCount,
      'no row was deleted before the refusal');
    Assert.IsTrue(FDataSet.Locate('id', 141, []), 'page rows intact');
    Assert.IsTrue(FDataSet.Locate('id', 150, []), 'page rows intact');
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.MixedSelectionDeleteIsAllOrNothing;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Raised: Boolean;
  Key: Word;
begin
  // Reproduction: a selection holding both stale and valid page-scoped
  // bookmarks. The VCL TBookmarkList.Delete iterates the sorted list from
  // the end, so the current page's valid entries are deleted first and the
  // first stale entry raises afterwards — a partial deletion. The grid's
  // delete path must refuse before deleting anything.
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    Grid.Options := Grid.Options + [dgMultiSelect] - [dgConfirmDelete];
    FDataSet.RecNo := 1;
    Grid.SelectedRows.CurrentRowSelected := True;
    FDataSet.RecNo := 2;
    Grid.SelectedRows.CurrentRowSelected := True;

    Assert.IsTrue(FDataSet.OpenPage(2)); // page 0 replaced; stale now
    FDataSet.RecNo := 25;
    Grid.SelectedRows.CurrentRowSelected := True;

    Assert.IsTrue(FDataSet.OpenPage(4)); // page 2 replaced too
    FDataSet.RecNo := 45;
    Grid.SelectedRows.CurrentRowSelected := True; // only valid entry
    Assert.AreEqual(4, Grid.SelectedRows.Count);

    Key := VK_DELETE;
    TVittixDBGridCracker(Grid).KeyDown(Key, []);
    Assert.IsTrue(FDataSet.Locate('id', 45, []),
      'plain Delete keeps the VCL behavior and does not delete the selection');

    Raised := False;
    try
      Key := VK_DELETE;
      TVittixDBGridCracker(Grid).KeyDown(Key, [ssCtrl]);
    except
      on E: EDatabaseError do
        Raised := True;
    end;
    Assert.IsTrue(Raised,
      'deleting a mixed selection must be refused');
    Raised := False;
    try
      Grid.DeleteSelectedRowsSafely;
    except
      on E: EDatabaseError do
        Raised := True;
    end;
    Assert.IsTrue(Raised,
      'the public delete method must refuse a mixed selection');
    Assert.AreEqual(4, Grid.SelectedRows.Count,
      'refusal must preserve the selection');
    Assert.IsTrue(FDataSet.Locate('id', 45, []),
      Format('the valid selection row must survive a refused delete (id 45 ' +
        'present=%s)', [BoolToStr(FDataSet.Locate('id', 45, []), True)]));
    Assert.IsTrue(FDataSet.Locate('id', 43, []), 'page rows intact');
    Assert.IsTrue(FDataSet.Locate('id', 46, []), 'page rows intact');
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.SelectionHasStaleEntriesReportsMixedState;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(0));
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    Grid.Options := Grid.Options + [dgMultiSelect];
    FDataSet.RecNo := 1;
    Grid.SelectedRows.CurrentRowSelected := True;
    Assert.IsFalse(Grid.SelectionHasStaleEntries,
      'a fresh single-page selection has no stale entries');

    Assert.IsTrue(FDataSet.OpenPage(2));
    FDataSet.RecNo := 25;
    Grid.SelectedRows.CurrentRowSelected := True;
    Assert.IsTrue(Grid.SelectionHasStaleEntries,
      'a mixed selection reports stale entries');

    Grid.SelectedRows.Refresh;
    Assert.AreEqual(1, Grid.SelectedRows.Count, 'only the valid entry stays');
    Assert.IsFalse(Grid.SelectionHasStaleEntries,
      'a pruned selection has no stale entries');

    // A suspended mode makes every wrapper stale, not just cross-page ones.
    FDataSet.Filtered := True;
    Assert.IsTrue(Grid.SelectionHasStaleEntries,
      'a suspended virtual mode stales the remaining wrapper');
    FDataSet.Filtered := False;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.SelectionDeleteWithoutStaleEntriesWorks;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Key: Word;
begin
  // Control: the pre-check must not interfere with a fully valid selection.
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(4));
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    Grid.Options := Grid.Options + [dgMultiSelect] - [dgConfirmDelete];
    FDataSet.RecNo := 44;
    Grid.SelectedRows.CurrentRowSelected := True;
    FDataSet.RecNo := 47;
    Grid.SelectedRows.CurrentRowSelected := True;
    Assert.IsFalse(Grid.SelectionHasStaleEntries);

    Key := VK_DELETE;
    TVittixDBGridCracker(Grid).KeyDown(Key, [ssCtrl]);

    FDataSet.VirtualScroll := False; // page-local scale for the count check
    Assert.AreEqual(8, FDataSet.RecordCount, 'exactly the two rows deleted');
    Assert.IsFalse(FDataSet.Locate('id', 44, []), 'id 44 deleted');
    Assert.IsFalse(FDataSet.Locate('id', 47, []), 'id 47 deleted');
    Assert.IsTrue(FDataSet.Locate('id', 43, []), 'id 43 intact');
    Assert.IsTrue(FDataSet.Locate('id', 48, []), 'id 48 intact');

  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.SafeDeleteValidPagedSelection;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  View: TClientDataSet;
begin
  FTotalRows := 230;
  FDataSet.VirtualScroll := True;
  FDataSet.TotalCount := 230;
  Assert.IsTrue(FDataSet.OpenPage(4));
  Grid := CreateHeadlessGrid(FDataSet, OwnerForm);
  try
    Grid.Options := Grid.Options + [dgMultiSelect] - [dgConfirmDelete];
    FDataSet.RecNo := 44;
    Grid.SelectedRows.CurrentRowSelected := True;
    FDataSet.RecNo := 47;
    Grid.SelectedRows.CurrentRowSelected := True;
    Grid.DeleteSelectedRowsSafely;
    View := TClientDataSet.Create(nil);
    try
      View.FieldDefs.Assign(FDataSet.FieldDefs);
      View.Data := FDataSet.Data;
      Assert.AreEqual(8, View.RecordCount);
      Assert.IsFalse(View.Locate('id', 44, []));
      Assert.IsFalse(View.Locate('id', 47, []));
      Assert.IsTrue(View.Locate('id', 43, []));
      Assert.IsTrue(View.Locate('id', 48, []));
    finally
      View.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixPagedDataSetTests.SafeDeleteOrdinaryDataSet;
var
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  DataSet: TClientDataSet;
begin
  DataSet := CreateLargeDataSet(5);
  try
    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Grid.Options := Grid.Options + [dgMultiSelect] - [dgConfirmDelete];
      DataSet.RecNo := 2;
      Grid.SelectedRows.CurrentRowSelected := True;
      DataSet.RecNo := 4;
      Grid.SelectedRows.CurrentRowSelected := True;
      Assert.IsFalse(Grid.SelectionHasStaleEntries);
      Grid.DeleteSelectedRowsSafely;
      Assert.AreEqual(3, DataSet.RecordCount);
      Assert.IsFalse(DataSet.Locate('ID', 2, []));
      Assert.IsFalse(DataSet.Locate('ID', 4, []));
      Assert.IsTrue(DataSet.Locate('ID', 1, []));
      Assert.IsTrue(DataSet.Locate('ID', 3, []));
      Assert.IsTrue(DataSet.Locate('ID', 5, []));
      Grid.DeleteSelectedRowsSafely; // empty selection is a no-op
      Assert.AreEqual(3, DataSet.RecordCount);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

end.
