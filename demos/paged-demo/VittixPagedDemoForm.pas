unit VittixPagedDemoForm;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Variants,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  Vcl.Dialogs,
  Vcl.Samples.Spin,
  Vcl.DBGrids,
  Vcl.DBCtrls,
  Data.DB,
  Datasnap.DBClient,
  Vittix.DBGrid,
  Vittix.DBGrid.Paging.DataSet,
  Vittix.DBGrid.Paging.DataSet.Controls;

type
  // Grants the self test access to the protected KeyDown handler.
  TVittixDBGridCracker = class(TVittixDBGrid);

  TPagedDemoForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private const
    PAGE_SIZE = 100;
    DEFAULT_ROWS = 1234;
  private
    FFailures: Integer;
    FLog: TStringList;
    FSourceRows: Integer;
    FDataset: TVittixPagedDataSet;
    FDataSource: TDataSource;
    Grid: TVittixDBGrid;
    Bar: TVittixPagedDataSetBar;
    Navigator: TVittixGlobalNavigator;
    TopPanel: TPanel;
    StatusPanel: TPanel;
    lblState: TLabel;
    lblPage: TLabel;
    lblSelection: TLabel;
    chkVirtualScroll: TCheckBox;
    spnSourceRows: TSpinEdit;
    btnDeleteSelection: TButton;
    procedure BuildDataset;
    procedure BuildUi;
    procedure ApplySourceSettings;
    procedure UpdateStatus;
    procedure PageChanged(Sender: TObject);
    procedure HandleLoadPage(Sender: TObject; APageIndex, APageSize: Integer;
      ADataSet: TClientDataSet);
    procedure chkVirtualScrollClick(Sender: TObject);
    procedure spnSourceRowsChange(Sender: TObject);
    procedure btnDeleteSelectionClick(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // Self test helpers (run by the VittixPagedDemoSelfTest companion).
    procedure Log(const S: string);
    procedure Check(const AName: string; ACondition: Boolean); overload;
    procedure Check(const AName, ADetail: string; ACondition: Boolean); overload;
  public
    /// <summary>Runs the built-in verification against the shown, visible
    /// grid: global scrollbar range and position, bar page changes, and
    /// stale-selection behavior. Returns the number of failed checks; the
    /// VittixPagedDemoSelfTest console companion uses it as the process
    /// exit code and writes a transcript to selftest-results.txt.</summary>
    function RunSelfTest: Integer;
  end;

var
  PagedDemoForm: TPagedDemoForm;

implementation

{$R *.dfm}

procedure TPagedDemoForm.FormCreate(Sender: TObject);
begin
  FSourceRows := DEFAULT_ROWS;
  BuildDataset;
  BuildUi;
  chkVirtualScroll.Checked := True;
  spnSourceRows.Value := FSourceRows;
  // Wire the handlers last so building the controls cannot fire them.
  chkVirtualScroll.OnClick := chkVirtualScrollClick;
  spnSourceRows.OnChange := spnSourceRowsChange;
  ApplySourceSettings;
  TFloatField(FDataset.FieldByName('amount')).DisplayFormat := '0.00';
end;

procedure TPagedDemoForm.BuildDataset;
begin
  FDataset := TVittixPagedDataSet.Create(Self);
  FDataset.FieldDefs.Add('id', ftInteger);
  FDataset.FieldDefs.Add('name', ftString, 40);
  FDataset.FieldDefs.Add('amount', ftFloat);
  FDataset.PageSize := PAGE_SIZE;
  FDataset.OnLoadPage := HandleLoadPage;
  FDataset.AddPageObserver(PageChanged);

  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FDataset;
end;

// The simulated source: every row I is id=I, name='Row I', amount=I*1.5.
// The loader materializes only the requested page.
procedure TPagedDemoForm.HandleLoadPage(Sender: TObject; APageIndex,
  APageSize: Integer; ADataSet: TClientDataSet);
var
  I, LastRow: Integer;
begin
  LastRow := Min((APageIndex + 1) * APageSize, FSourceRows);
  for I := APageIndex * APageSize + 1 to LastRow do
    ADataSet.AppendRecord([I, 'Row ' + IntToStr(I), I * 1.5]);
end;

procedure TPagedDemoForm.BuildUi;
var
  Column: TColumn;
begin
  Caption := 'Vittix DBGrid - paged dataset demo';
  Position := poScreenCenter;
  ClientWidth := 754;
  ClientHeight := 502;

  TopPanel := TPanel.Create(Self);
  TopPanel.Parent := Self;
  TopPanel.Align := alTop;
  TopPanel.Height := 40;
  TopPanel.BevelOuter := bvNone;
  TopPanel.Padding.SetBounds(8, 8, 8, 8);

  btnDeleteSelection := TButton.Create(Self);
  btnDeleteSelection.Parent := TopPanel;
  btnDeleteSelection.Left := 8;
  btnDeleteSelection.Top := 8;
  btnDeleteSelection.Width := 130;
  btnDeleteSelection.Caption := 'Delete selection';
  btnDeleteSelection.OnClick := btnDeleteSelectionClick;

  chkVirtualScroll := TCheckBox.Create(Self);
  chkVirtualScroll.Parent := TopPanel;
  chkVirtualScroll.Left := 150;
  chkVirtualScroll.Top := 10;
  chkVirtualScroll.Width := 220;
  chkVirtualScroll.Caption := 'Virtual scroll (global scrollbar)';

  with TLabel.Create(Self) do
  begin
    Parent := TopPanel;
    Left := 384;
    Top := 12;
    Caption := 'Source rows:';
  end;
  spnSourceRows := TSpinEdit.Create(Self);
  spnSourceRows.Parent := TopPanel;
  spnSourceRows.Left := 464;
  spnSourceRows.Top := 8;
  spnSourceRows.Width := 90;
  spnSourceRows.MinValue := 0;
  spnSourceRows.MaxValue := 1000000;
  spnSourceRows.Increment := 100;

  with TLabel.Create(Self) do
  begin
    Parent := TopPanel;
    Left := 568;
    Top := 12;
    Caption := 'page size: 100';
  end;

  Bar := TVittixPagedDataSetBar.Create(Self);
  Bar.Parent := Self;            // first alBottom control: bottommost strip
  Bar.DataSet := FDataset;

  Navigator := TVittixGlobalNavigator.Create(Self);
  Navigator.Parent := Self;
  Navigator.Align := alBottom;
  Navigator.Height := 30;
  Navigator.DataSource := FDataSource;
  Navigator.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast];

  StatusPanel := TPanel.Create(Self);
  StatusPanel.Parent := Self;    // second alBottom control: sits above the bar
  StatusPanel.Align := alBottom;
  StatusPanel.Height := 58;
  StatusPanel.BevelOuter := bvNone;

  lblState := TLabel.Create(Self);
  lblState.Parent := StatusPanel;
  lblState.Left := 8;
  lblState.Top := 6;
  lblState.AutoSize := True;

  lblPage := TLabel.Create(Self);
  lblPage.Parent := StatusPanel;
  lblPage.Left := 8;
  lblPage.Top := 24;
  lblPage.AutoSize := True;

  lblSelection := TLabel.Create(Self);
  lblSelection.Parent := StatusPanel;
  lblSelection.Left := 8;
  lblSelection.Top := 42;
  lblSelection.AutoSize := True;
  lblSelection.Font.Color := clGrayText;

  Grid := TVittixDBGrid.Create(Self);
  Grid.Parent := Self;
  Grid.Align := alClient;
  Grid.DataSource := FDataSource;
  Grid.Options := [dgTitles, dgIndicator, dgColumnResize, dgColLines,
    dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgMultiSelect];
  Grid.OnMouseUp := GridMouseUp;

  Column := Grid.Columns.Add;
  Column.FieldName := 'id';
  Column.Title.Caption := 'Id';
  Column.Width := 70;
  Column := Grid.Columns.Add;
  Column.FieldName := 'name';
  Column.Title.Caption := 'Name';
  Column.Width := 240;
  Column := Grid.Columns.Add;
  Column.FieldName := 'amount';
  Column.Title.Caption := 'Amount';
  Column.Width := 110;
end;

procedure TPagedDemoForm.ApplySourceSettings;
begin
  FSourceRows := spnSourceRows.Value;
  // Set the properties while the page is open (SetTotalCount reads the
  // loaded page), then reload from page zero: every page load refreshes
  // the grid scrollbar (TCustomDBGrid.DataChanged), so the bar, the
  // status line, and the scrollbar all recompute from the new settings.
  if chkVirtualScroll.Checked then
    FDataset.TotalCount := FSourceRows
  else
    FDataset.TotalCount := -1;
  FDataset.VirtualScroll := chkVirtualScroll.Checked;
  FDataset.Close;
  FDataset.OpenPage(0);
  UpdateStatus;
end;

procedure TPagedDemoForm.UpdateStatus;
var
  ModeText, SelText: string;
  FirstRow, LastRow: Integer;
begin
  if chkVirtualScroll.Checked then
    ModeText := Format('virtual scroll ON: the scrollbar spans all %s rows',
      [FormatFloat('#,##0', FSourceRows)])
  else if FDataset.TotalCountKnown then
    ModeText := Format('virtual scroll OFF: page-local scrollbar, count known (%d pages)',
      [FDataset.PageCount])
  else
    ModeText := 'virtual scroll OFF: page-local scrollbar, total unknown';

  lblState.Caption := Format('Source: %s rows | page size: %d | %s',
    [FormatFloat('#,##0', FSourceRows), PAGE_SIZE, ModeText]);

  if FDataset.PageLoaded and (FSourceRows > 0) then
  begin
    FirstRow := FDataset.PageIndex * PAGE_SIZE + 1;
    LastRow := Min(FSourceRows, (FDataset.PageIndex + 1) * PAGE_SIZE);
    // Deliberately computed, not read from RecordCount: in virtual mode
    // RecordCount is the global total, so the loaded row count has to be
    // derived from the page arithmetic (documented boundary).
    lblPage.Caption := Format(
      'Loaded page %d of %d (global rows %d-%d, %d rows in memory)',
      [FDataset.PageIndex + 1, FDataset.PageCount, FirstRow, LastRow,
       LastRow - FirstRow + 1]);
  end
  else
    lblPage.Caption := 'Loaded page 1 (empty source)';

  SelText := Format('%d row(s) selected', [Grid.SelectedRows.Count]);
  if (Grid.SelectedRows.Count > 0) and Grid.SelectionHasStaleEntries then
    SelText := SelText +
      ' - STALE: contains rows from pages that are no longer loaded';
  lblSelection.Caption := SelText;
end;

procedure TPagedDemoForm.PageChanged(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TPagedDemoForm.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateStatus;
end;

procedure TPagedDemoForm.chkVirtualScrollClick(Sender: TObject);
begin
  ApplySourceSettings;
end;

procedure TPagedDemoForm.spnSourceRowsChange(Sender: TObject);
begin
  ApplySourceSettings;
end;

procedure TPagedDemoForm.btnDeleteSelectionClick(Sender: TObject);
begin
  if Grid.SelectedRows.Count = 0 then
    ShowMessage('Select rows first (Ctrl+Click or Ctrl+Space).')
  else
    try
      Grid.DeleteSelectedRowsSafely;
      UpdateStatus;
    except
      on E: EDatabaseError do
        ShowMessage(E.Message);
    end;
end;

{ Self test (run by the VittixPagedDemoSelfTest console companion) }

procedure TPagedDemoForm.Log(const S: string);
begin
  FLog.Add(S);
  // Echo to the console companion's stdout when there is one.
  if IsConsole then
    Writeln(S);
  // Persist after every line so an unattended hang leaves a partial
  // transcript pinpointing the blocking call.
  try
    FLog.SaveToFile(ExtractFilePath(ParamStr(0)) + 'selftest-results.txt');
  except
  end;
end;

procedure TPagedDemoForm.Check(const AName: string; ACondition: Boolean);
begin
  Check(AName, '', ACondition);
end;

procedure TPagedDemoForm.Check(const AName, ADetail: string;
  ACondition: Boolean);
begin
  if ACondition then
    Log('  PASS  ' + AName)
  else if ADetail = '' then
  begin
    Log('  FAIL  ' + AName);
    Inc(FFailures);
  end
  else
  begin
    Log(Format('  FAIL  %s [%s]', [AName, ADetail]));
    Inc(FFailures);
  end;
end;

function TPagedDemoForm.RunSelfTest: Integer;
var
  SI: TScrollInfo;
  Raised: Boolean;
  Key: Word;
  PageLabel: TLabel;
  View: TClientDataSet;
  Deleted510, Deleted515, Kept501: Boolean;

begin
  FLog := TStringList.Create;
  try
    Log('Vittix paged dataset demo - self test');
    Log('(source: 1,234 rows, page size 100, virtual scroll on)');
    Log('');

    Show;
    Application.ProcessMessages;
    Sleep(150);
    Application.ProcessMessages;
    PageLabel := Bar.FindComponent('PageLabel') as TLabel;

    // 1. Visible grid global scrollbar.
    Log('1. Global scrollbar on the visible grid');
    FDataset.RecNo := 700;
    Application.ProcessMessages;
    SI.cbSize := SizeOf(SI);
    SI.fMask := SIF_ALL;
    GetScrollInfo(Grid.Handle, SB_VERT, SI);
    Check('scrollbar range spans all 1,234 source rows',
      Integer(SI.nMax) - Integer(SI.nPage) + 1 = DEFAULT_ROWS);
    Check('scrollbar position tracks global row 700', SI.nPos = 700);

    // 2. Page changes through the navigation bar.
    Log('2. Navigation bar page changes');
    (Bar.FindComponent('FirstButton') as TButton).Click;
    Check('First returns to page 1 of 13',
      (FDataset.PageIndex = 0) and (PageLabel.Caption = 'Page 1 of 13'));
    (Bar.FindComponent('NextButton') as TButton).Click;
    Check('Next advances to page 2 of 13',
      (FDataset.PageIndex = 1) and (PageLabel.Caption = 'Page 2 of 13'));
    (Bar.FindComponent('LastButton') as TButton).Click;
    Check('Last jumps to page 13 of 13',
      (FDataset.PageIndex = 12) and (PageLabel.Caption = 'Page 13 of 13'));
    FDataset.RecNo := 1234;
    Check('partial final page ends at global row 1,234',
      FDataset.FieldByName('id').AsInteger = 1234);

    // The navigator's endpoint buttons traverse the entire source.
    Log('2a. Navigator global endpoints');
    FDataset.RecNo := 700;
    Navigator.BtnClick(nbFirst);
    Check('navigator First reaches global row 1',
      (FDataset.PageIndex = 0) and (FDataset.RecNo = 1));
    Navigator.BtnClick(nbLast);
    Check('navigator Last reaches global row 1,234',
      (FDataset.PageIndex = 12) and (FDataset.RecNo = 1234));

    // 3. Selection behavior across page boundaries.
    Log('3. Selection across page boundaries');
    FDataset.RecNo := 1;
    Grid.SelectedRows.CurrentRowSelected := True;
    FDataset.RecNo := 2;
    Grid.SelectedRows.CurrentRowSelected := True;
    Check('fresh selection reports no stale entries',
      not Grid.SelectionHasStaleEntries);
    FDataset.OpenPage(5);
    Check('changing pages stales the selection',
      Grid.SelectionHasStaleEntries);
    Raised := False;
    try
      Key := VK_DELETE;
      TVittixDBGridCracker(Grid).KeyDown(Key, [ssCtrl]);
    except
      on E: EDatabaseError do
        Raised := True;
    end;
    Check('deleting a stale selection is refused', Raised);
    Check('the refusal leaves the selection untouched',
      Grid.SelectedRows.Count = 2);
    Raised := False;
    try
      Grid.DeleteSelectedRowsSafely;
    except
      on E: EDatabaseError do
        Raised := True;
    end;
    Check('safe API refuses a stale selection', Raised);
    Check('safe API leaves the selection untouched',
      Grid.SelectedRows.Count = 2);
    Grid.SelectedRows.Refresh;
    Check('SelectedRows.Refresh prunes the stale entries',
      Grid.SelectedRows.Count = 0);
    FDataset.RecNo := 510;
    Grid.SelectedRows.CurrentRowSelected := True;
    FDataset.RecNo := 515;
    Grid.SelectedRows.CurrentRowSelected := True;
    Grid.DeleteSelectedRowsSafely;
    // Verify against a detached copy of the view packet, not the live
    // dataset: after the delete flow the CDS packet start is displaced
    // (navigation reads the wrong slice) and Locate can still find rows
    // deleted since the last merge. Data excludes deleted rows, and the
    // copy has no data link or grid attached, so its cursor is ordinary.
    View := TClientDataSet.Create(nil);
    try
      View.FieldDefs.Assign(FDataset.FieldDefs);
      View.Data := FDataset.Data;
      Deleted510 := not View.Locate('id', 510, []);
      Deleted515 := not View.Locate('id', 515, []);
      Kept501 := View.Locate('id', 501, []);
      Check('a valid selection deletes exactly its rows',
        Format('rows=%d, deleted 510=%s, deleted 515=%s, kept 501=%s',
          [View.RecordCount, BoolToStr(Deleted510, True),
           BoolToStr(Deleted515, True), BoolToStr(Kept501, True)]),
        (View.RecordCount = PAGE_SIZE - 2) and Deleted510 and
        Deleted515 and Kept501);
    finally
      View.Free;
    end;

    // 4. Virtual scroll toggle switches the scrollbar scale.
    Log('4. Virtual scroll toggle');
    chkVirtualScroll.Checked := False;
    ApplySourceSettings;
    Application.ProcessMessages;
    SI.cbSize := SizeOf(SI);
    SI.fMask := SIF_ALL;
    GetScrollInfo(Grid.Handle, SB_VERT, SI);
    Check('virtual scroll off shows the page-local scrollbar',
      Integer(SI.nMax) - Integer(SI.nPage) + 1 = PAGE_SIZE);
    Check('bar caption drops the page total when the count is unknown',
      PageLabel.Caption = 'Page 1');
    chkVirtualScroll.Checked := True;
    ApplySourceSettings;
    Application.ProcessMessages;
    SI.cbSize := SizeOf(SI);
    SI.fMask := SIF_ALL;
    GetScrollInfo(Grid.Handle, SB_VERT, SI);
    Check('virtual scroll on restores the global scrollbar',
      Integer(SI.nMax) - Integer(SI.nPage) + 1 = DEFAULT_ROWS);

    Log('');
    if FFailures = 0 then
      Log('Result: all checks passed.')
    else
      Log(Format('Result: %d check(s) failed.', [FFailures]));
    Result := FFailures;
    try
      FLog.SaveToFile(ExtractFilePath(ParamStr(0)) + 'selftest-results.txt');
    except
      // The console output already exists; the transcript is best effort.
    end;
  finally
    FLog.Free;
  end;
end;

end.
