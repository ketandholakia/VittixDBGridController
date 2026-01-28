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
  Winapi.Windows,      // For VK_ constants
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.CheckLst,
  Vcl.DBGrids,
  Vcl.Menus,
  Vcl.Dialogs;

type
  /// <summary>
  /// Enhanced column chooser dialog for TDBGrid with drag-drop reordering
  /// </summary>
  TVittixDBGridColumnChooserForm = class(TForm)
  private
    FGrid: TDBGrid;
    FCheckList: TCheckListBox;
    FButtonPanel: TPanel;
    FBtnOK: TButton;
    FBtnCancel: TButton;
    FPopupMenu: TPopupMenu;
    FMenuItemSelectAll: TMenuItem;
    FMenuItemSelectNone: TMenuItem;
    FAllowReorder: Boolean;
    FDraggedIndex: Integer;

    procedure BuildColumnList;
    procedure ApplySelection;
    function GetColumnCaption(AColumn: TColumn): string;
    
    // Event Handlers
    procedure CheckListDblClick(Sender: TObject);
    procedure CheckListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoSelectAll(Sender: TObject);
    procedure DoSelectNone(Sender: TObject);
    procedure CheckListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CheckListDragDrop(Sender, Source: TObject; X, Y: Integer);
  public
    constructor CreateChooser(AOwner: TComponent; AGrid: TDBGrid); reintroduce;
    class procedure Execute(AGrid: TDBGrid);
    
    property AllowReorder: Boolean read FAllowReorder write FAllowReorder;
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

  Caption := 'Column Chooser';
  BorderStyle := bsSizeable; // Allow resizing
  Position := poScreenCenter;
  Width := 340;
  Height := 450;
  
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

  // Button Panel
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

procedure TVittixDBGridColumnChooserForm.CheckListDblClick(Sender: TObject);
var
  Idx: Integer;
begin
  // Toggle checkbox on double click
  Idx := FCheckList.ItemIndex;
  if Idx >= 0 then
    FCheckList.Checked[Idx] := not FCheckList.Checked[Idx];
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

class procedure TVittixDBGridColumnChooserForm.Execute(AGrid: TDBGrid);
var
  Frm: TVittixDBGridColumnChooserForm;
begin
  if not Assigned(AGrid) then
    Exit;

  Frm := TVittixDBGridColumnChooserForm.CreateChooser(nil, AGrid);
  try
    if Frm.ShowModal = mrOk then
      Frm.ApplySelection;
  finally
    Frm.Free;
  end;
end;

end.
