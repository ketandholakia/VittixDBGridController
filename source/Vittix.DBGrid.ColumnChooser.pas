unit Vittix.DBGrid.ColumnChooser;

{$REGION 'Documentation'}
/// <summary>
/// ENHANCED VERSION - Column Chooser Dialog for TVittixDBGrid
/// 
/// FEATURES IMPLEMENTED:
/// 1. Drag-and-drop column reordering (from TODO list)
/// 2. Select All / Select None shortcuts (Ctrl+A, Ctrl+N)
/// 3. Double-click to toggle visibility
/// 4. Keyboard shortcuts for accessibility
/// 5. Context menu with shortcuts
///
/// FIXES APPLIED:
/// 1. Fixed TPoint.Create compilation error (use Point() function)
/// 2. Added safety checks for column operations
/// 3. Added live reordering during drag-drop
///
/// TODO (Future Enhancements):
/// - Add search/filter capability for many columns
/// - Add form state persistence (size, position)
/// - Add minimum size constraints
/// - Add column width adjustment
/// </summary>
{$ENDREGION}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,        // For TPoint
  System.IOUtils,
  Winapi.Windows,      // For VK_ constants
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.CheckLst,
  Vcl.DBGrids,
  Vcl.Menus,
  Vcl.Dialogs,
  System.IniFiles;

type
  /// <summary>
  /// Enhanced column chooser dialog for TDBGrid with drag-drop reordering
  /// </summary>
  TVittixDBGridColumnChooserForm = class(TForm)
  private
    FGrid: TDBGrid;
    FSearchEdit: TEdit;
    FCheckList: TCheckListBox;
    FButtonPanel: TPanel;
    FSearchSummary: TLabel;
    FBtnOK: TButton;
    FBtnCancel: TButton;
    FPopupMenu: TPopupMenu;
    FMenuItemSelectAll: TMenuItem;
    FMenuItemSelectNone: TMenuItem;
    FMenuItemReset: TMenuItem;
    FMenuItemGrow: TMenuItem;
    FMenuItemShrink: TMenuItem;
    FAllowReorder: Boolean;
    FDraggedIndex: Integer;
    // FIX BUG 9: Snapshot of original column indices taken when dialog opens.
    // On Cancel, we roll back the live reorder that drag-drop applies immediately.
    FOriginalColumnOrder: TArray<Integer>;
    FOriginalColumnWidths: TArray<Integer>;

    procedure BuildColumnList;
    procedure ApplySearchFilter;
    procedure ApplySelection;
    procedure RollbackColumnOrder;
    function GetColumnCaption(AColumn: TColumn): string;
    function GetSearchText: string;
    procedure SetSearchText(const Value: string);
    
    // Event Handlers
    procedure CheckListDblClick(Sender: TObject);
    procedure CheckListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoSelectAll(Sender: TObject);
    procedure DoSelectNone(Sender: TObject);
    procedure DoReset(Sender: TObject);
    procedure DoGrowWidth(Sender: TObject);
    procedure DoShrinkWidth(Sender: TObject);
    procedure RestoreOriginalColumnWidths;
    procedure CheckListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CheckListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SearchEditChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  public
    class var StateFileName: string;
    class var RootPath: string;
    constructor CreateChooser(AOwner: TComponent; AGrid: TDBGrid); reintroduce;
    class function Execute(AGrid: TDBGrid): Boolean;
    procedure ResetLayout;
    procedure RevertTransientChanges;
    procedure SelectColumnIndex(AIndex: Integer);
    function CanReorderColumns: Boolean;
    procedure IncreaseSelectedColumnWidth;
    procedure DecreaseSelectedColumnWidth;
    procedure LoadDialogState;
    procedure SaveDialogState;
    procedure FocusSearchBox;
    function GetSearchSummaryText: string;
    procedure AdjustSelectedColumnWidth(Delta: Integer);
    
    property AllowReorder: Boolean read FAllowReorder write FAllowReorder;
    property SearchText: string read GetSearchText write SetSearchText;
    property SearchSummaryText: string read GetSearchSummaryText;
  end;

implementation

{ TVittixDBGridColumnChooserForm }

constructor TVittixDBGridColumnChooserForm.CreateChooser(
  AOwner: TComponent; AGrid: TDBGrid);
begin
  // Use CreateNew for code-only forms (avoids "Resource not found" errors)
  inherited CreateNew(AOwner);

  if not Assigned(AGrid) then
    raise Exception.Create('Grid parameter cannot be nil');

  FGrid := AGrid;
  FAllowReorder := True;
  FDraggedIndex := -1;

  // FIX BUG 9: Snapshot the current column order so we can roll it back
  // if the user clicks Cancel. Drag-drop applies reordering to the grid live,
  // so without this snapshot, Cancel cannot undo a reorder.
  SetLength(FOriginalColumnOrder, FGrid.Columns.Count);
  SetLength(FOriginalColumnWidths, FGrid.Columns.Count);
  for var K := 0 to FGrid.Columns.Count - 1 do
  begin
    FOriginalColumnOrder[K] := FGrid.Columns[K].Index;
    FOriginalColumnWidths[K] := FGrid.Columns[K].Width;
  end;

  Caption := 'Column Chooser';
  BorderStyle := bsSizeable; // Allow resizing
  Position := poScreenCenter;
  Width := 340;
  Height := 450;
  KeyPreview := True;
  OnKeyDown := FormKeyDown;
  OnResize := FormResize;
  
  // Set minimum size constraints
  Constraints.MinWidth := 250;
  Constraints.MinHeight := 300;
  
  // Create Context Menu
  FPopupMenu := TPopupMenu.Create(Self);
  
  FMenuItemSelectAll := TMenuItem.Create(FPopupMenu);
  FMenuItemSelectAll.Caption := '&Select All';
  FMenuItemSelectAll.ShortCut := TextToShortCut('Ctrl+A');
  FMenuItemSelectAll.OnClick := DoSelectAll;
  FPopupMenu.Items.Add(FMenuItemSelectAll);
  
  FMenuItemSelectNone := TMenuItem.Create(FPopupMenu);
  FMenuItemSelectNone.Caption := 'Select &None';
  FMenuItemSelectNone.ShortCut := TextToShortCut('Ctrl+N');
  FMenuItemSelectNone.OnClick := DoSelectNone;
  FPopupMenu.Items.Add(FMenuItemSelectNone);

  FMenuItemReset := TMenuItem.Create(FPopupMenu);
  FMenuItemReset.Caption := '&Reset Layout';
  FMenuItemReset.ShortCut := TextToShortCut('Ctrl+R');
  FMenuItemReset.OnClick := DoReset;
  FPopupMenu.Items.Add(FMenuItemReset);

  FMenuItemGrow := TMenuItem.Create(FPopupMenu);
  FMenuItemGrow.Caption := 'Increase &Width';
  FMenuItemGrow.ShortCut := TextToShortCut('Ctrl+Plus');
  FMenuItemGrow.OnClick := DoGrowWidth;
  FPopupMenu.Items.Add(FMenuItemGrow);

  FMenuItemShrink := TMenuItem.Create(FPopupMenu);
  FMenuItemShrink.Caption := 'Decrease Width';
  FMenuItemShrink.ShortCut := TextToShortCut('Ctrl+Minus');
  FMenuItemShrink.OnClick := DoShrinkWidth;
  FPopupMenu.Items.Add(FMenuItemShrink);

  // Button Panel
  FSearchEdit := TEdit.Create(Self);
  FSearchEdit.Parent := Self;
  FSearchEdit.Align := alTop;
  FSearchEdit.AlignWithMargins := True;
  FSearchEdit.Margins.SetBounds(8, 8, 8, 0);
  FSearchEdit.TextHint := 'Search columns';
  FSearchEdit.OnChange := SearchEditChange;

  FSearchSummary := TLabel.Create(Self);
  FSearchSummary.Parent := Self;
  FSearchSummary.Align := alTop;
  FSearchSummary.AlignWithMargins := True;
  FSearchSummary.Margins.SetBounds(12, 2, 12, 0);
  FSearchSummary.Caption := '';

  FButtonPanel := TPanel.Create(Self);
  FButtonPanel.Parent := Self;
  FButtonPanel.Align := alBottom;
  FButtonPanel.Height := 48;
  FButtonPanel.BevelOuter := bvNone;

  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent := FButtonPanel;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.Align := alRight;
  FBtnCancel.AlignWithMargins := True;
  FBtnCancel.Margins.SetBounds(4, 8, 8, 8);
  FBtnCancel.Width := 90;
  FBtnCancel.Cancel := True;
  FBtnCancel.TabOrder := 1;

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := FButtonPanel;
  FBtnOK.Caption := 'OK';
  FBtnOK.ModalResult := mrOk;
  FBtnOK.Align := alRight;
  FBtnOK.AlignWithMargins := True;
  FBtnOK.Margins.SetBounds(4, 8, 4, 8);
  FBtnOK.Width := 90;
  FBtnOK.Default := True;
  FBtnOK.TabOrder := 0;

  // CheckList
  FCheckList := TCheckListBox.Create(Self);
  FCheckList.Parent := Self;
  FCheckList.Align := alClient;
  FCheckList.AlignWithMargins := True;
  FCheckList.Margins.SetBounds(8, 8, 8, 0);
  FCheckList.BorderStyle := bsSingle;
  FCheckList.PopupMenu := FPopupMenu;
  FCheckList.OnDblClick := CheckListDblClick;
  FCheckList.OnKeyDown := CheckListKeyDown;
  FCheckList.TabOrder := 0;
  FCheckList.Hint := 'Drag items to reorder columns';
  FCheckList.ShowHint := True;

  // Drag-and-drop support
  FCheckList.DragMode := dmManual;
  FCheckList.OnMouseDown := CheckListMouseDown;
  FCheckList.OnDragOver := CheckListDragOver;
  FCheckList.OnDragDrop := CheckListDragDrop;

  BuildColumnList;
  LoadDialogState;
end;

procedure TVittixDBGridColumnChooserForm.RollbackColumnOrder;
var
  I: Integer;
begin
  // FIX BUG 9: Restore each column to its original index position.
  // We iterate in forward order; the grid engine re-indexes on each assignment.
  if not Assigned(FGrid) then Exit;
  if Length(FOriginalColumnOrder) <> FGrid.Columns.Count then Exit;

  for I := 0 to High(FOriginalColumnOrder) do
  begin
    if (FOriginalColumnOrder[I] < FGrid.Columns.Count) and
       (FGrid.Columns[I].Index <> FOriginalColumnOrder[I]) then
      FGrid.Columns[I].Index := FOriginalColumnOrder[I];
  end;
end;

procedure TVittixDBGridColumnChooserForm.RestoreOriginalColumnWidths;
var
  I: Integer;
begin
  if not Assigned(FGrid) then Exit;
  if Length(FOriginalColumnWidths) <> FGrid.Columns.Count then Exit;

  for I := 0 to FGrid.Columns.Count - 1 do
    FGrid.Columns[I].Width := FOriginalColumnWidths[I];
end;

procedure TVittixDBGridColumnChooserForm.BuildColumnList;
var
  I: Integer;
  Col: TColumn;
begin
  FCheckList.Items.BeginUpdate;
  try
    FCheckList.Clear;

    if not Assigned(FGrid) then Exit;

    for I := 0 to FGrid.Columns.Count - 1 do
    begin
      Col := FGrid.Columns[I];
      // Store TColumn pointer for retrieval, display the caption
      FCheckList.Items.AddObject(GetColumnCaption(Col), Col);
      FCheckList.Checked[I] := Col.Visible;
    end;
  finally
    FCheckList.Items.EndUpdate;
  end;

  ApplySearchFilter;
end;

procedure TVittixDBGridColumnChooserForm.ApplySearchFilter;
var
  I: Integer;
  Query: string;
  Terms: TArray<string>;
  Term: string;
  Col: TColumn;
  CaptionText: string;
  MatchCount: Integer;
  MatchesAllTerms: Boolean;
begin
  Query := Trim(FSearchEdit.Text).ToLower;
  Terms := Query.Split([' '], TStringSplitOptions.ExcludeEmpty);
  MatchCount := 0;
  FCheckList.Items.BeginUpdate;
  try
    for I := 0 to FCheckList.Items.Count - 1 do
    begin
      Col := TColumn(FCheckList.Items.Objects[I]);
      CaptionText := LowerCase(FCheckList.Items[I]);
      MatchesAllTerms := Length(Terms) = 0;
      if Length(Terms) > 0 then
      begin
        MatchesAllTerms := True;
        for Term in Terms do
          if not (CaptionText.Contains(Term) or LowerCase(Col.FieldName).Contains(Term)) then
          begin
            MatchesAllTerms := False;
            Break;
          end;
      end;

      FCheckList.ItemEnabled[I] := MatchesAllTerms;
      if FCheckList.ItemEnabled[I] then
        Inc(MatchCount);
    end;
  finally
    FCheckList.Items.EndUpdate;
  end;

  if Query = '' then
    FSearchSummary.Caption := Format('%d columns', [FCheckList.Items.Count])
  else
    FSearchSummary.Caption := Format('%d matches', [MatchCount]);
end;

function TVittixDBGridColumnChooserForm.GetColumnCaption(
  AColumn: TColumn): string;
begin
  if not Assigned(AColumn) then
    Exit('');

  Result := Trim(AColumn.Title.Caption);
  if Result = '' then
    Result := AColumn.FieldName;
  if Result = '' then
    Result := Format('Column %d', [AColumn.Index]);
end;

function TVittixDBGridColumnChooserForm.GetSearchText: string;
begin
  Result := FSearchEdit.Text;
end;

procedure TVittixDBGridColumnChooserForm.SetSearchText(const Value: string);
begin
  FSearchEdit.Text := Value;
end;

procedure TVittixDBGridColumnChooserForm.CheckListDblClick(Sender: TObject);
var
  Idx: Integer;
begin
  // Toggle checkbox on double click
  Idx := FCheckList.ItemIndex;
  if Idx >= 0 then
    FCheckList.Checked[Idx] := not FCheckList.Checked[Idx];
end;

procedure TVittixDBGridColumnChooserForm.SearchEditChange(Sender: TObject);
begin
  ApplySearchFilter;
end;

procedure TVittixDBGridColumnChooserForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('F')) and (ssCtrl in Shift) then
  begin
    FocusSearchBox;
    Key := 0;
  end;
end;

procedure TVittixDBGridColumnChooserForm.FormResize(Sender: TObject);
begin
  if Width < Constraints.MinWidth then
    Width := Constraints.MinWidth;
  if Height < Constraints.MinHeight then
    Height := Constraints.MinHeight;
end;

procedure TVittixDBGridColumnChooserForm.LoadDialogState;
var
  Ini: TIniFile;
  FileName: string;
begin
  if StateFileName <> '' then
    FileName := StateFileName
  else if RootPath <> '' then
    FileName := TPath.Combine(RootPath, 'chooser.ini')
  else
    FileName := TPath.Combine(ExtractFilePath(ParamStr(0)), 'VittixDBGridChooser.ini');
  Ini := TIniFile.Create(FileName);
  try
    Left := Ini.ReadInteger('Chooser', 'Left', Left);
    Top := Ini.ReadInteger('Chooser', 'Top', Top);
    Width := Ini.ReadInteger('Chooser', 'Width', Width);
    Height := Ini.ReadInteger('Chooser', 'Height', Height);
    FSearchEdit.Text := Ini.ReadString('Chooser', 'SearchText', '');
    FAllowReorder := Ini.ReadBool('Chooser', 'AllowReorder', FAllowReorder);
  finally
    Ini.Free;
  end;
end;

procedure TVittixDBGridColumnChooserForm.SaveDialogState;
var
  Ini: TIniFile;
  FileName: string;
begin
  if StateFileName <> '' then
    FileName := StateFileName
  else if RootPath <> '' then
    FileName := TPath.Combine(RootPath, 'chooser.ini')
  else
    FileName := TPath.Combine(ExtractFilePath(ParamStr(0)), 'VittixDBGridChooser.ini');
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteInteger('Chooser', 'Left', Left);
    Ini.WriteInteger('Chooser', 'Top', Top);
    Ini.WriteInteger('Chooser', 'Width', Width);
    Ini.WriteInteger('Chooser', 'Height', Height);
    Ini.WriteString('Chooser', 'SearchText', FSearchEdit.Text);
    Ini.WriteBool('Chooser', 'AllowReorder', FAllowReorder);
  finally
    Ini.Free;
  end;
end;

procedure TVittixDBGridColumnChooserForm.CheckListKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Keyboard Shortcuts for Select All/None
  if (Key = Ord('A')) and (ssCtrl in Shift) then
  begin
    DoSelectAll(Sender);
    Key := 0;
  end
  else if (Key = Ord('N')) and (ssCtrl in Shift) then
  begin
    DoSelectNone(Sender);
    Key := 0;
  end
  else if (Key = VK_OEM_PLUS) and (ssCtrl in Shift) then
  begin
    DoGrowWidth(Sender);
    Key := 0;
  end
  else if ((Key = VK_OEM_MINUS) or (Key = VK_SUBTRACT)) and (ssCtrl in Shift) then
  begin
    DoShrinkWidth(Sender);
    Key := 0;
  end
  // Space bar to toggle current item
  else if Key = VK_SPACE then
  begin
    if FCheckList.ItemIndex >= 0 then
    begin
      FCheckList.Checked[FCheckList.ItemIndex] := 
        not FCheckList.Checked[FCheckList.ItemIndex];
      Key := 0;
    end;
  end;
end;

procedure TVittixDBGridColumnChooserForm.CheckListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    // FIX: Use Point() function instead of TPoint.Create
    FDraggedIndex := FCheckList.ItemAtPos(Point(X, Y), True);
    if FDraggedIndex >= 0 then
      FCheckList.BeginDrag(False, 5);
  end;
end;

procedure TVittixDBGridColumnChooserForm.CheckListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = FCheckList) and FAllowReorder;
end;

procedure TVittixDBGridColumnChooserForm.CheckListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  DropIndex: Integer;
  DraggedChecked: Boolean;
begin
  if FDraggedIndex < 0 then Exit;

  // FIX: Use Point() function instead of TPoint.Create
  DropIndex := FCheckList.ItemAtPos(Point(X, Y), True);
  
  // If dropped outside items, move to end
  if DropIndex < 0 then
    DropIndex := FCheckList.Items.Count - 1;

  if (DropIndex >= 0) and (DropIndex <> FDraggedIndex) then
  begin
    // Save the checked state before moving
    DraggedChecked := FCheckList.Checked[FDraggedIndex];
    
    // Move the listbox item first (this is visual)
    FCheckList.Items.Move(FDraggedIndex, DropIndex);
    
    // Restore the checked state (Move doesn't preserve it)
    FCheckList.Checked[DropIndex] := DraggedChecked;
    
    // Move the actual grid column to match
    // Safety: Check that column is still valid
    if (FDraggedIndex < FGrid.Columns.Count) and 
       (DropIndex < FGrid.Columns.Count) then
    begin
      FGrid.Columns[FDraggedIndex].Index := DropIndex;
    end;
    
    // Select the moved item
    FCheckList.ItemIndex := DropIndex;
  end;
  
  FDraggedIndex := -1;
end;

procedure TVittixDBGridColumnChooserForm.DoSelectAll(Sender: TObject);
var
  I: Integer;
begin
  FCheckList.Items.BeginUpdate;
  try
    for I := 0 to FCheckList.Count - 1 do
      FCheckList.Checked[I] := True;
  finally
    FCheckList.Items.EndUpdate;
  end;
end;

procedure TVittixDBGridColumnChooserForm.DoSelectNone(Sender: TObject);
var
  I: Integer;
begin
  FCheckList.Items.BeginUpdate;
  try
    for I := 0 to FCheckList.Count - 1 do
      FCheckList.Checked[I] := False;
  finally
    FCheckList.Items.EndUpdate;
  end;
end;

procedure TVittixDBGridColumnChooserForm.DoReset(Sender: TObject);
var
  I: Integer;
begin
  if not Assigned(FGrid) then Exit;

  RollbackColumnOrder;
  RestoreOriginalColumnWidths;

  FCheckList.Items.BeginUpdate;
  try
    for I := 0 to FCheckList.Items.Count - 1 do
      FCheckList.Checked[I] := True;
  finally
    FCheckList.Items.EndUpdate;
  end;

  for I := 0 to FGrid.Columns.Count - 1 do
    FGrid.Columns[I].Visible := True;
end;

procedure TVittixDBGridColumnChooserForm.DoGrowWidth(Sender: TObject);
begin
  IncreaseSelectedColumnWidth;
end;

procedure TVittixDBGridColumnChooserForm.DoShrinkWidth(Sender: TObject);
begin
  DecreaseSelectedColumnWidth;
end;

procedure TVittixDBGridColumnChooserForm.IncreaseSelectedColumnWidth;
begin
  AdjustSelectedColumnWidth(16);
end;

procedure TVittixDBGridColumnChooserForm.DecreaseSelectedColumnWidth;
begin
  AdjustSelectedColumnWidth(-16);
end;

procedure TVittixDBGridColumnChooserForm.FocusSearchBox;
begin
  if Assigned(FSearchEdit) then
  begin
    ActiveControl := FSearchEdit;
    if FSearchEdit.CanFocus then
      FSearchEdit.SetFocus;
    FSearchEdit.SelectAll;
  end;
end;

procedure TVittixDBGridColumnChooserForm.AdjustSelectedColumnWidth(Delta: Integer);
var
  Col: TColumn;
  NewWidth: Integer;
begin
  if FCheckList.ItemIndex < 0 then Exit;
  Col := TColumn(FCheckList.Items.Objects[FCheckList.ItemIndex]);
  if not Assigned(Col) then Exit;

  NewWidth := Col.Width + Delta;
  if NewWidth < 24 then
    NewWidth := 24;
  Col.Width := NewWidth;
end;

function TVittixDBGridColumnChooserForm.GetSearchSummaryText: string;
begin
  if Assigned(FSearchSummary) then
    Result := FSearchSummary.Caption
  else
    Result := '';
end;

procedure TVittixDBGridColumnChooserForm.ResetLayout;
begin
  DoReset(Self);
end;

procedure TVittixDBGridColumnChooserForm.RevertTransientChanges;
begin
  RollbackColumnOrder;
  RestoreOriginalColumnWidths;
end;

function TVittixDBGridColumnChooserForm.CanReorderColumns: Boolean;
begin
  Result := FAllowReorder;
end;

procedure TVittixDBGridColumnChooserForm.SelectColumnIndex(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FCheckList.Items.Count) then
  begin
    FCheckList.ItemIndex := -1;
    Exit;
  end;

  FCheckList.ItemIndex := AIndex;
end;

procedure TVittixDBGridColumnChooserForm.ApplySelection;
var
  I: Integer;
  Col: TColumn;
begin
  if not Assigned(FGrid) then Exit;

  // Note: Reordering was applied live via drag-drop, so we only need to
  // handle visibility here.
  // Apply visibility based on checkbox state
  for I := 0 to FCheckList.Items.Count - 1 do
  begin
    Col := TColumn(FCheckList.Items.Objects[I]);
    // FIX: Enhanced safety check
    if Assigned(Col) and 
       Assigned(Col.Collection) and 
       (Col.Grid = FGrid) and
       (Col.Index < FGrid.Columns.Count) then
    begin
      Col.Visible := FCheckList.Checked[I];
    end;
  end;
end;

class function TVittixDBGridColumnChooserForm.Execute(AGrid: TDBGrid): Boolean;
var
  Frm: TVittixDBGridColumnChooserForm;
begin
  Result := False;
  if not Assigned(AGrid) then
    Exit;

  Frm := TVittixDBGridColumnChooserForm.CreateChooser(nil, AGrid);
  try
    if Frm.ShowModal = mrOk then
    begin
      Frm.ApplySelection;
      Frm.SaveDialogState;
      Result := True;
    end
    else
    begin
      Frm.RevertTransientChanges;
    end;
  finally
    Frm.Free;
  end;
end;

end.
