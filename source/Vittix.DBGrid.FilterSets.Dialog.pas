unit Vittix.DBGrid.FilterSets.Dialog;

{$REGION 'Documentation'}
/// <summary>
/// Modal manager for the saved filter sets: list, rename, delete, reorder,
/// and an editor for groups and their conditions. The condition editor uses
/// the single operator table from Vittix.DBGrid.Filter.Engine (no
/// duplicated operator logic). "Apply selected" applies the selected set to
/// the grid through the controller. Style-aware through
/// Vittix.DBGrid.Styles.
/// </summary>
{$ENDREGION}

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.DBGrids,
  Vcl.Dialogs,
  Vittix.DBGrid.FilterSets;

type
  TVittixDBGridFilterSetManager = class(TForm)
  private
    FSetList: TVittixFilterSetList;
    FOnChanged: TProc;
    // Called for "Save Current As..."; the controller captures the live
    // per-column filters. Nil hides the button (list-only usage).
    FOnSaveCurrentAs: TProc<string>;
    // Serialized copy of the list as it was when the manager opened.
    // Editor-pane mutations are NOT persisted per keystroke; they stay in
    // memory only, and a Cancel restores this snapshot so no cancelled
    // edit leaks into the caller's shared list or its file.
    FSnapshot: string;
    FSetsList: TListBox;
    FConditionsList: TListBox;
    FPanelLeft: TPanel;
    FPanelRight: TPanel;
    FPanelButtons: TPanel;
    FPanelEditor: TPanel;
    FBtnApply: TButton;
    FBtnClose: TButton;
    FBtnSaveCurrent: TButton;
    FBtnRename: TButton;
    FBtnDelete: TButton;
    FBtnUp: TButton;
    FBtnDown: TButton;
    FBtnAddGroup: TButton;
    FBtnDeleteGroup: TButton;
    FBtnAddCondition: TButton;
    FBtnUpdateCondition: TButton;
    FBtnDeleteCondition: TButton;
    FCmbColumn: TComboBox;
    FCmbOperator: TComboBox;
    FEdtValue: TEdit;
    FColumns: TStrings;

    procedure FillSetsList(ASelected: string);
    procedure FillConditionsList;
    procedure FillColumnCombo;
    procedure FillOperatorCombo;
    function SelectedSet: TVittixFilterSet;
    function SelectedGroupIndex: Integer;
    function SelectedFlatIndex: Integer;
    function BuildConditionText: string;
    procedure SetsClick(Sender: TObject);
    procedure ConditionsClick(Sender: TObject);
  public
    { Editor actions are public so hosts (and tests) can drive the
      manager programmatically; they are the same handlers the buttons
      use. Mutations touch the in-memory list only - Execute persists on
      "Apply & Close" and reverts everything on Cancel. }
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnSaveCurrentClick(Sender: TObject);
    procedure BtnRenameClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
    procedure BtnDownClick(Sender: TObject);
    procedure BtnAddGroupClick(Sender: TObject);
    procedure BtnDeleteGroupClick(Sender: TObject);
    procedure BtnAddConditionClick(Sender: TObject);
    procedure BtnUpdateConditionClick(Sender: TObject);
    procedure BtnDeleteConditionClick(Sender: TObject);

    property SetsListBox: TListBox read FSetsList;
    property ConditionsListBox: TListBox read FConditionsList;
    property ColumnCombo: TComboBox read FCmbColumn;
    property OperatorCombo: TComboBox read FCmbOperator;
    property ValueEdit: TEdit read FEdtValue;
    /// <summary>Creates the manager. ASetList is edited in place;
    /// AColumns supplies the condition editor's column list; OnChanged
    /// fires once on Apply & Close so the caller can save and refresh;
    /// AOnSaveCurrentAs persists only the independently saved set (nil
    /// hides that button).</summary>
    constructor CreateManager(AOwner: TComponent;
      ASetList: TVittixFilterSetList; AColumns: TStrings;
      AOnChanged: TProc; AOnSaveCurrentAs: TProc<string> = nil);

    /// <summary>Shows the manager modally; returns True when the user
    /// closed it with "Apply & Close" and the selected set was applied.</summary>
    function Execute(out AAppliedName: string): Boolean;
  end;

implementation

uses
  System.UITypes,
  Vcl.Themes,
  Vittix.DBGrid.Filter.Engine,
  Vittix.DBGrid.Styles;

procedure CopyNamedFilterSet(const AName: string;
  ASource, ATarget: TVittixFilterSetList);
var
  SourceSet, TargetSet: TVittixFilterSet;
  TargetGroup: TVittixFilterSetGroup;
  I, J: Integer;
  Condition: TVittixFilterCondition;
begin
  SourceSet := ASource.FindByName(AName);
  if not Assigned(SourceSet) then
    Exit;

  TargetSet := ATarget.AddOrUpdate(SourceSet.Name);
  for I := 0 to SourceSet.GroupCount - 1 do
  begin
    TargetGroup := TargetSet.AddGroup;
    for J := 0 to SourceSet[I].ConditionCount - 1 do
    begin
      Condition := SourceSet[I][J];
      TargetGroup.AddCondition(Condition.FieldName, Condition.FilterText);
    end;
  end;
end;

{ TVittixDBGridFilterSetManager }

constructor TVittixDBGridFilterSetManager.CreateManager(AOwner: TComponent;
  ASetList: TVittixFilterSetList; AColumns: TStrings; AOnChanged: TProc;
  AOnSaveCurrentAs: TProc<string>);
var
  StyleActive: Boolean;
begin
  inherited CreateNew(AOwner);

  if not Assigned(ASetList) then
    raise Exception.Create('Filter set list parameter cannot be nil');

  FSetList := ASetList;
  FColumns := AColumns;
  FOnChanged := AOnChanged;
  FOnSaveCurrentAs := AOnSaveCurrentAs;
  // Snapshot BEFORE any dialog interaction: Cancel restores this state,
  // so nothing the user edited in the manager leaks into the shared list.
  FSnapshot := FSetList.SaveToJSON;

  Caption := 'Saved Filters';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  ClientWidth := 640;
  ClientHeight := 420;

  // The group/condition editor needs a grid context to be useful; without
  // columns it still manages sets but cannot add conditions.
  FPanelButtons := TPanel.Create(Self);
  FPanelButtons.Parent := Self;
  FPanelButtons.Align := alBottom;
  FPanelButtons.Height := 44;
  FPanelButtons.BevelOuter := bvNone;
  StyleActive := TVittixStylePalette.IsCustomStyleActive(Self);
  if StyleActive then
    FPanelButtons.ParentBackground := True
  else
  begin
    FPanelButtons.ParentBackground := False;
    FPanelButtons.Color := clBtnFace;
  end;

  FBtnClose := TButton.Create(Self);
  FBtnClose.Parent := FPanelButtons;
  FBtnClose.Caption := 'Close';
  FBtnClose.ModalResult := mrCancel;
  FBtnClose.Cancel := True;
  FBtnClose.Align := alRight;
  FBtnClose.AlignWithMargins := True;
  FBtnClose.Margins.SetBounds(4, 8, 8, 8);
  FBtnClose.Width := 80;

  FBtnApply := TButton.Create(Self);
  FBtnApply.Parent := FPanelButtons;
  FBtnApply.Caption := 'Apply && Close';
  FBtnApply.Default := True;
  FBtnApply.Align := alRight;
  FBtnApply.AlignWithMargins := True;
  FBtnApply.Margins.SetBounds(4, 8, 4, 8);
  FBtnApply.Width := 100;
  FBtnApply.OnClick := BtnApplyClick;

  FBtnSaveCurrent := TButton.Create(Self);
  FBtnSaveCurrent.Parent := FPanelButtons;
  FBtnSaveCurrent.Caption := 'Save Current As...';
  FBtnSaveCurrent.Align := alLeft;
  FBtnSaveCurrent.AlignWithMargins := True;
  FBtnSaveCurrent.Margins.SetBounds(8, 8, 4, 8);
  FBtnSaveCurrent.Width := 120;
  FBtnSaveCurrent.OnClick := BtnSaveCurrentClick;
  FBtnSaveCurrent.Visible := Assigned(FOnSaveCurrentAs);

  FPanelLeft := TPanel.Create(Self);
  FPanelLeft.Parent := Self;
  FPanelLeft.Align := alLeft;
  FPanelLeft.Width := 260;
  FPanelLeft.BevelOuter := bvNone;
  if StyleActive then
    FPanelLeft.ParentBackground := True
  else
  begin
    FPanelLeft.ParentBackground := False;
    FPanelLeft.Color := clBtnFace;
  end;

  FSetsList := TListBox.Create(Self);
  FSetsList.Parent := FPanelLeft;
  FSetsList.Align := alClient;
  FSetsList.AlignWithMargins := True;
  FSetsList.Margins.SetBounds(8, 8, 4, 4);
  FSetsList.OnClick := SetsClick;

  FBtnRename := TButton.Create(Self);
  FBtnRename.Parent := FPanelLeft;
  FBtnRename.Caption := 'Rename...';
  FBtnRename.Align := alBottom;
  FBtnRename.AlignWithMargins := True;
  FBtnRename.Margins.SetBounds(8, 2, 4, 2);
  FBtnRename.Width := 120;
  FBtnRename.OnClick := BtnRenameClick;

  FBtnDelete := TButton.Create(Self);
  FBtnDelete.Parent := FPanelLeft;
  FBtnDelete.Caption := 'Delete';
  FBtnDelete.Align := alBottom;
  FBtnDelete.AlignWithMargins := True;
  FBtnDelete.Margins.SetBounds(8, 2, 4, 2);
  FBtnDelete.Width := 120;
  FBtnDelete.OnClick := BtnDeleteClick;

  FBtnUp := TButton.Create(Self);
  FBtnUp.Parent := FPanelLeft;
  FBtnUp.Caption := 'Move Up';
  FBtnUp.Align := alBottom;
  FBtnUp.AlignWithMargins := True;
  FBtnUp.Margins.SetBounds(8, 2, 4, 2);
  FBtnUp.Width := 120;
  FBtnUp.OnClick := BtnUpClick;

  FBtnDown := TButton.Create(Self);
  FBtnDown.Parent := FPanelLeft;
  FBtnDown.Caption := 'Move Down';
  FBtnDown.Align := alBottom;
  FBtnDown.AlignWithMargins := True;
  FBtnDown.Margins.SetBounds(8, 2, 4, 2);
  FBtnDown.Width := 120;
  FBtnDown.OnClick := BtnDownClick;

  FPanelRight := TPanel.Create(Self);
  FPanelRight.Parent := Self;
  FPanelRight.Align := alClient;
  FPanelRight.BevelOuter := bvNone;

  FConditionsList := TListBox.Create(Self);
  FConditionsList.Parent := FPanelRight;
  FConditionsList.Align := alClient;
  FConditionsList.AlignWithMargins := True;
  FConditionsList.Margins.SetBounds(4, 8, 8, 4);
  FConditionsList.OnClick := ConditionsClick;

  FPanelEditor := TPanel.Create(Self);
  FPanelEditor.Parent := FPanelRight;
  FPanelEditor.Align := alBottom;
  FPanelEditor.Height := 140;
  FPanelEditor.BevelOuter := bvNone;
  if StyleActive then
    FPanelEditor.ParentBackground := True
  else
  begin
    FPanelEditor.ParentBackground := False;
    FPanelEditor.Color := clBtnFace;
  end;

  FBtnAddGroup := TButton.Create(Self);
  FBtnAddGroup.Parent := FPanelEditor;
  FBtnAddGroup.Caption := 'Add Group';
  FBtnAddGroup.SetBounds(8, 8, 100, 25);
  FBtnAddGroup.OnClick := BtnAddGroupClick;

  FBtnDeleteGroup := TButton.Create(Self);
  FBtnDeleteGroup.Parent := FPanelEditor;
  FBtnDeleteGroup.Caption := 'Delete Group';
  FBtnDeleteGroup.SetBounds(114, 8, 100, 25);
  FBtnDeleteGroup.OnClick := BtnDeleteGroupClick;

  FCmbColumn := TComboBox.Create(Self);
  FCmbColumn.Parent := FPanelEditor;
  FCmbColumn.Style := csDropDownList;
  FCmbColumn.SetBounds(8, 40, 180, 21);

  FCmbOperator := TComboBox.Create(Self);
  FCmbOperator.Parent := FPanelEditor;
  FCmbOperator.Style := csDropDownList;
  FCmbOperator.SetBounds(194, 40, 130, 21);
  FillOperatorCombo;
  FCmbOperator.ItemIndex := 0;

  FEdtValue := TEdit.Create(Self);
  FEdtValue.Parent := FPanelEditor;
  FEdtValue.SetBounds(330, 40, 180, 21);

  FBtnAddCondition := TButton.Create(Self);
  FBtnAddCondition.Parent := FPanelEditor;
  FBtnAddCondition.Caption := 'Add Condition';
  FBtnAddCondition.SetBounds(8, 70, 100, 25);
  FBtnAddCondition.OnClick := BtnAddConditionClick;

  FBtnUpdateCondition := TButton.Create(Self);
  FBtnUpdateCondition.Parent := FPanelEditor;
  FBtnUpdateCondition.Caption := 'Update';
  FBtnUpdateCondition.SetBounds(114, 70, 70, 25);
  FBtnUpdateCondition.OnClick := BtnUpdateConditionClick;

  FBtnDeleteCondition := TButton.Create(Self);
  FBtnDeleteCondition.Parent := FPanelEditor;
  FBtnDeleteCondition.Caption := 'Delete';
  FBtnDeleteCondition.SetBounds(190, 70, 70, 25);
  FBtnDeleteCondition.OnClick := BtnDeleteConditionClick;

  FillColumnCombo;
  FillSetsList('');
end;

procedure TVittixDBGridFilterSetManager.FillOperatorCombo;
var
  I: Integer;
begin
  FCmbOperator.Items.BeginUpdate;
  try
    FCmbOperator.Items.Clear;
    for I := 0 to VittixFilterOperatorCount - 1 do
      FCmbOperator.Items.Add(VittixFilterOperatorDefinition(I).DisplayName);
  finally
    FCmbOperator.Items.EndUpdate;
  end;
end;

procedure TVittixDBGridFilterSetManager.FillColumnCombo;
begin
  if Assigned(FColumns) then
    FCmbColumn.Items.Assign(FColumns);
  if FCmbColumn.Items.Count > 0 then
    FCmbColumn.ItemIndex := 0;
end;

procedure TVittixDBGridFilterSetManager.FillSetsList(ASelected: string);
var
  I: Integer;
begin
  FSetsList.Items.BeginUpdate;
  try
    FSetsList.Items.Clear;
    for I := 0 to FSetList.Count - 1 do
      FSetsList.Items.Add(FSetList[I].Name);
    if ASelected <> '' then
      FSetsList.ItemIndex := FSetsList.Items.IndexOf(ASelected)
    else if FSetsList.Items.Count > 0 then
      FSetsList.ItemIndex := 0;
  finally
    FSetsList.Items.EndUpdate;
  end;
  FillConditionsList;
end;

function TVittixDBGridFilterSetManager.SelectedSet: TVittixFilterSet;
begin
  Result := nil;
  if FSetsList.ItemIndex >= 0 then
    Result := FSetList.FindByName(FSetsList.Items[FSetsList.ItemIndex]);
end;

function TVittixDBGridFilterSetManager.SelectedGroupIndex: Integer;
var
  Set_: TVittixFilterSet;
begin
  Result := -1;
  Set_ := SelectedSet;
  if Assigned(Set_) and (Set_.GroupCount > 0) then
    Result := 0; // the editor edits group 1 by default
end;

function TVittixDBGridFilterSetManager.SelectedFlatIndex: Integer;
begin
  Result := FConditionsList.ItemIndex;
end;

procedure TVittixDBGridFilterSetManager.FillConditionsList;
var
  Set_: TVittixFilterSet;
  G, C: Integer;
begin
  FConditionsList.Items.BeginUpdate;
  try
    FConditionsList.Items.Clear;
    Set_ := SelectedSet;
    if not Assigned(Set_) then
      Exit;
    for G := 0 to Set_.GroupCount - 1 do
    begin
      if Set_[G].ConditionCount = 0 then
        FConditionsList.Items.Add(Format('Group %d: (empty - matches all)', [G + 1]))
      else
        for C := 0 to Set_[G].ConditionCount - 1 do
          FConditionsList.Items.Add(Format('Group %d: %s %s',
            [G + 1, Set_[G][C].FieldName, Set_[G][C].FilterText]));
    end;
  finally
    FConditionsList.Items.EndUpdate;
  end;
end;

procedure TVittixDBGridFilterSetManager.SetsClick(Sender: TObject);
begin
  FillConditionsList;
end;

procedure TVittixDBGridFilterSetManager.ConditionsClick(Sender: TObject);
var
  Set_: TVittixFilterSet;
  G, C: Integer;
  CondText: string;
  OperatorIndex: Integer;
begin
  // Load the selected condition into the editors. Entries are laid out as
  // "Group N: Field Text"; the flat index maps back through the set.
  Set_ := SelectedSet;
  if not Assigned(Set_) or (SelectedFlatIndex < 0) then
    Exit;

  // Walk groups again to resolve the flat index (empty groups render one
  // non-selectable informational entry each).
  G := 0;
  C := SelectedFlatIndex;
  while (G < Set_.GroupCount) and (C >= 0) do
  begin
    if Set_[G].ConditionCount = 0 then
    begin
      if C = 0 then
        Exit; // the "(empty)" entry has nothing to load
      Dec(C);
      Inc(G);
      Continue;
    end;
    if C < Set_[G].ConditionCount then
      Break;
    Dec(C, Set_[G].ConditionCount);
    Inc(G);
  end;
  if (G >= Set_.GroupCount) or (C < 0) or (C >= Set_[G].ConditionCount) then
    Exit;

  FCmbColumn.ItemIndex := FCmbColumn.Items.IndexOf(Set_[G][C].FieldName);
  CondText := Set_[G][C].FilterText;
  if VittixFilterTryParseOperatorText(CondText, OperatorIndex, CondText) then
    FCmbOperator.ItemIndex := OperatorIndex
  else
    FCmbOperator.ItemIndex := 0;
  FEdtValue.Text := CondText;
end;

function TVittixDBGridFilterSetManager.BuildConditionText: string;
var
  Def: TVittixFilterOperatorDefinition;
begin
  // Conditions reuse the shared operator table: word operators carry no
  // value, everything else is prefix + typed text (same as the popup).
  Def := VittixFilterOperatorDefinition(FCmbOperator.ItemIndex);
  if Def.IsWordOperator then
    Result := Def.Prefix
  else
    Result := Def.Prefix + Trim(FEdtValue.Text);
end;

procedure TVittixDBGridFilterSetManager.BtnApplyClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TVittixDBGridFilterSetManager.BtnCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TVittixDBGridFilterSetManager.BtnSaveCurrentClick(Sender: TObject);
var
  Name: string;
  WorkingState: string;
  SavedState: string;
  SavedSets: TVittixFilterSetList;
begin
  Name := '';
  if not InputQuery('Save Current Filters', 'Filter set name:', Name) then
    Exit;
  if Trim(Name) = '' then
    Exit;
  if not Assigned(FOnSaveCurrentAs) then
    Exit;

  // Save Current As is an independent commit: serialize the current edit
  // session, temporarily restore the last committed baseline, then let the
  // callback add and persist only the explicitly named current filter set.
  // This prevents unrelated, unsaved manager edits from reaching disk.
  WorkingState := FSetList.SaveToJSON;
  SavedSets := TVittixFilterSetList.Create;
  try
    FSetList.LoadFromJSON(FSnapshot);
    try
      FOnSaveCurrentAs(Trim(Name));
      SavedState := FSetList.SaveToJSON;
      SavedSets.LoadFromJSON(SavedState);
    except
      FSetList.LoadFromJSON(WorkingState);
      raise;
    end;

    // Keep editing the user's working copy, but advance the Cancel baseline
    // and replace just the saved set in that copy. Earlier edits to other
    // sets remain pending and can still be discarded by Cancel.
    FSetList.LoadFromJSON(WorkingState);
    CopyNamedFilterSet(Name, SavedSets, FSetList);
    FSnapshot := SavedState;
  finally
    SavedSets.Free;
  end;

  FillSetsList(Trim(Name));
end;

procedure TVittixDBGridFilterSetManager.BtnRenameClick(Sender: TObject);
var
  OldName, NewName: string;
begin
  if FSetsList.ItemIndex < 0 then Exit;
  OldName := FSetsList.Items[FSetsList.ItemIndex];
  NewName := OldName;
  if not InputQuery('Rename Filter Set', 'New name:', NewName) then
    Exit;
  if (Trim(NewName) = '') or SameText(OldName, Trim(NewName)) then
    Exit;
  if FSetList.Rename(OldName, Trim(NewName)) then
    FillSetsList(Trim(NewName));
  // Persistence is centralized: Execute fires OnChanged once on
  // "Apply & Close"; Cancel reverts the rename via the snapshot.
end;

procedure TVittixDBGridFilterSetManager.BtnDeleteClick(Sender: TObject);
var
  Name: string;
begin
  if FSetsList.ItemIndex < 0 then Exit;
  Name := FSetsList.Items[FSetsList.ItemIndex];
  if MessageDlg(Format('Delete filter set "%s"?', [Name]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  if FSetList.Delete(Name) then
    FillSetsList('');
end;

procedure TVittixDBGridFilterSetManager.BtnUpClick(Sender: TObject);
begin
  if FSetsList.ItemIndex < 0 then Exit;
  if FSetList.MoveUp(FSetsList.Items[FSetsList.ItemIndex]) then
    FillSetsList(FSetsList.Items[FSetsList.ItemIndex]);
end;

procedure TVittixDBGridFilterSetManager.BtnDownClick(Sender: TObject);
begin
  if FSetsList.ItemIndex < 0 then Exit;
  if FSetList.MoveDown(FSetsList.Items[FSetsList.ItemIndex]) then
    FillSetsList(FSetsList.Items[FSetsList.ItemIndex]);
end;

procedure TVittixDBGridFilterSetManager.BtnAddGroupClick(Sender: TObject);
var
  Set_: TVittixFilterSet;
begin
  Set_ := SelectedSet;
  if not Assigned(Set_) then Exit;
  Set_.AddGroup;
  FillConditionsList;
end;

procedure TVittixDBGridFilterSetManager.BtnDeleteGroupClick(Sender: TObject);
var
  Set_: TVittixFilterSet;
begin
  Set_ := SelectedSet;
  if not Assigned(Set_) or (Set_.GroupCount = 0) then Exit;
  // The editor edits the last group; delete that one.
  Set_.DeleteGroup(Set_.GroupCount - 1);
  FillConditionsList;
end;

procedure TVittixDBGridFilterSetManager.BtnAddConditionClick(Sender: TObject);
var
  Set_: TVittixFilterSet;
  Group: TVittixFilterSetGroup;
  FieldName: string;
begin
  Set_ := SelectedSet;
  if not Assigned(Set_) then Exit;
  if FCmbColumn.ItemIndex < 0 then
    Exit; // no columns available (no grid context)
  FieldName := FCmbColumn.Items[FCmbColumn.ItemIndex];

  // Conditions append to the last group; a set without groups gets one.
  if Set_.GroupCount = 0 then
    Set_.AddGroup;
  Group := Set_[Set_.GroupCount - 1];
  Group.AddCondition(FieldName, BuildConditionText);
  FillConditionsList;
end;

procedure TVittixDBGridFilterSetManager.BtnUpdateConditionClick(Sender: TObject);
var
  Set_: TVittixFilterSet;
  G, C: Integer;
  Cond: TVittixFilterCondition;
begin
  Set_ := SelectedSet;
  if not Assigned(Set_) or (SelectedFlatIndex < 0) then Exit;
  if FCmbColumn.ItemIndex < 0 then Exit;

  // Resolve the flat index exactly like ConditionsClick
  G := 0;
  C := SelectedFlatIndex;
  while (G < Set_.GroupCount) and (C >= 0) do
  begin
    if Set_[G].ConditionCount = 0 then
    begin
      if C = 0 then
        Exit;
      Dec(C);
      Inc(G);
      Continue;
    end;
    if C < Set_[G].ConditionCount then
      Break;
    Dec(C, Set_[G].ConditionCount);
    Inc(G);
  end;
  if (G >= Set_.GroupCount) or (C < 0) or (C >= Set_[G].ConditionCount) then
    Exit;

  Cond.FieldName := FCmbColumn.Items[FCmbColumn.ItemIndex];
  Cond.FilterText := BuildConditionText;
  Set_[G].Conditions[C] := Cond;
  FillConditionsList;
end;

procedure TVittixDBGridFilterSetManager.BtnDeleteConditionClick(Sender: TObject);
var
  Set_: TVittixFilterSet;
  G, C: Integer;
begin
  Set_ := SelectedSet;
  if not Assigned(Set_) or (SelectedFlatIndex < 0) then Exit;

  G := 0;
  C := SelectedFlatIndex;
  while (G < Set_.GroupCount) and (C >= 0) do
  begin
    if Set_[G].ConditionCount = 0 then
    begin
      if C = 0 then
        Exit;
      Dec(C);
      Inc(G);
      Continue;
    end;
    if C < Set_[G].ConditionCount then
      Break;
    Dec(C, Set_[G].ConditionCount);
    Inc(G);
  end;
  if (G >= Set_.GroupCount) or (C < 0) or (C >= Set_[G].ConditionCount) then
    Exit;

  Set_[G].DeleteCondition(C);
  FillConditionsList;
end;

function TVittixDBGridFilterSetManager.Execute(out AAppliedName: string): Boolean;
begin
  AAppliedName := '';
  Result := ShowModal = mrOk;
  if Result then
  begin
    // Single persistence notification for the whole editing session: the
    // caller saves the list once, in exactly the state the user applied.
    if Assigned(FOnChanged) then
      FOnChanged();
    if FSetsList.ItemIndex >= 0 then
      AAppliedName := FSetsList.Items[FSetsList.ItemIndex];
    if AAppliedName = '' then
      Result := False;
  end
  else
    // Cancel (Close button, ESC): revert every in-memory edit this dialog
    // made. The snapshot also covers renames, deletes and reorders; only
    // "Save Current As" moved the snapshot forward (that set is kept).
    FSetList.LoadFromJSON(FSnapshot);
end;

end.
