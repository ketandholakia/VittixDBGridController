unit Vittix.DBGrid.Filter.Popup;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.DBGrids,
  Vcl.Menus,
  Winapi.Windows,
  Vcl.Graphics,
  Winapi.Messages,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Filter.Engine;

type
  /// <summary>
  /// Popup dialog for editing a column filter
  /// </summary>
  TVittixDBGridFilterPopup = class(TForm)
  private
    FRecentCombo: TComboBox;
    FOperatorCombo: TComboBox;
    FButtonPanel: TPanel;
    FBtnOK: TButton;
    FBtnClear: TButton;
    FBtnClearHistory: TButton;
    FBtnSaved: TButton;
    FSavedMenu: TPopupMenu;
    FBtnCancel: TButton;
    FLabelTitle: TLabel;
    FValidationLabel: TLabel;
    FUseDistinctValuesOnly: Boolean;

    FColumnInfo: TVittixDBGridColumnInfo;
    FOriginalText: string;
    // Per-instance persistence settings resolved from the owning grid.
    // Class vars below remain only as process-wide fallback defaults so that
    // popups created without a TVittixDBGrid owner keep working.
    FRootPath: string;
    FHistoryFileName: string;
    // Scoped history key prevents two grids sharing history for
    // same-named fields. Key is "OwnerClassName.FieldName".
    FHistoryKey: string;
    FOperatorHistoryKey: string;

    procedure BtnClearClick(Sender: TObject);
    procedure BtnClearHistoryClick(Sender: TObject);
    procedure BtnSavedClick(Sender: TObject);
    procedure BuildSavedMenu;
    procedure ApplySavedSet(Sender: TObject);
    procedure SaveCurrentAsClick(Sender: TObject);
    procedure ManageClick(Sender: TObject);
    procedure ApplyChanges;
    procedure ApplySavedTextToControls(const AText: string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboChange(Sender: TObject);
    procedure LoadDistinctValues;
    function ValidateInput: Boolean;
    function GetOperatorPrefix: string;
    function OperatorIndexFromPrefix(const Prefix: string): Integer;
    function GetOperatorIndex: Integer;
    function GetFilterText: string;
    procedure SetFilterText(const Value: string);
    function NormalizeDisplayFilterText(const Value: string): string;
    procedure LoadPersistedHistory;
    function GetHistoryPath: string;
  public
    // Process-wide fallbacks used only when the popup owner is not a
    // TVittixDBGrid with its own persistence settings.
    class var HistoryFileName: string;
    class var RootPath: string;
    OnValidateFilterInput: TFilterValidationEvent;

    constructor CreatePopup(
      AOwner: TComponent;
      AColumnInfo: TVittixDBGridColumnInfo
    ); reintroduce;

    /// <summary>Opens a filter editor for AColumnInfo. Applies the filter
    /// on OK and returns True; returns False on cancel or nil metadata.
    /// AOnValidate can reject the entered filter.</summary>
    class function Execute(
      AOwner: TComponent;
      AColumnInfo: TVittixDBGridColumnInfo;
      AOnValidate: TFilterValidationEvent = nil
    ): Boolean;
    procedure PersistHistory;
    procedure ClearHistory;
    function ValidateCurrentInput: Boolean;
    procedure CommitCurrentValue;
    procedure ExecuteClearHistoryShortcut;
    function GetButtonShortcutSummaryText: string;

    property OperatorIndex: Integer read GetOperatorIndex;
    property FilterText: string read GetFilterText write SetFilterText;
    property UseDistinctValuesOnly: Boolean read FUseDistinctValuesOnly write FUseDistinctValuesOnly;
  end;

implementation

uses
  Data.DB,
  System.IOUtils,
  System.IniFiles,
  System.Math,
  System.Generics.Collections,
  Vcl.Dialogs,
  Vittix.DBGrid,
  Vittix.DBGrid.Controller,
  Vittix.DBGrid.FilterSets.Dialog,
  Vittix.DBGrid.Styles;

var
  GFilterHistory: TObjectDictionary<string, TStringList>;
const
  BlankValueCaption = '(Blank)';

{ TVittixDBGridFilterPopup }

// Forward declaration: CreatePopup enables/disables the Saved button before
// the helper's body appears further down.
function PopupOwnerController(
  APopup: TVittixDBGridFilterPopup): TVittixDBGridController; forward;

// Splits a stored filter text into operator selection + value text, applying
// both to the combo controls. Used for the column's active filter and for
// restoring persisted history so both paths stay consistent. The shared
// operator table performs the split exactly the way the engine parses it.
procedure TVittixDBGridFilterPopup.ApplySavedTextToControls(const AText: string);
var
  OperatorIndex: Integer;
  Value: string;
begin
  if AText = '' then
  begin
    FRecentCombo.Text := '';
    Exit;
  end;

  if VittixFilterTryParseOperatorText(AText, OperatorIndex, Value) then
  begin
    FOperatorCombo.ItemIndex := OperatorIndex;
    FRecentCombo.Text := Value;
  end
  else
    // Plain contains value; the combo never shows a raw operator prefix.
    FRecentCombo.Text := AText;
end;

constructor TVittixDBGridFilterPopup.CreatePopup(
  AOwner: TComponent;
  AColumnInfo: TVittixDBGridColumnInfo);
var
  LHistory: TStringList;
  I: Integer;
begin
  inherited CreateNew(AOwner);

  if not Assigned(AColumnInfo) then
    raise Exception.Create('ColumnInfo parameter cannot be nil');

  FColumnInfo := AColumnInfo;
  FOriginalText := '';
  FUseDistinctValuesOnly := False;

  // Per-grid persistence settings: the controller passes the grid as owner,
  // so resolve root path / history file from the grid itself instead of the
  // process-wide class vars (two grids must not overwrite each other).
  if AOwner is TVittixDBGrid then
  begin
    FRootPath := TVittixDBGrid(AOwner).PersistenceRootPath;
    FHistoryFileName := TVittixDBGrid(AOwner).FilterHistoryFileName;
  end;

  // Build a scoped history key using the owner's class name so that
  // two grids on the same form don't share filter history for the same field.
  if Assigned(AOwner) then
    FHistoryKey := AOwner.ClassName + '.' + AColumnInfo.FieldName
  else
    FHistoryKey := AColumnInfo.FieldName;
  FOperatorHistoryKey := FHistoryKey + '.operator';

  // Dialog Setup
  Caption := 'Filter Column';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  // Widened for the Saved filters button; the button panel packs its
  // left- and right-aligned buttons from both ends.
  ClientWidth := 460;
  ClientHeight := 140;
  KeyPreview := True; // Enable ESC/ENTER handling at form level
  OnKeyDown := FormKeyDown;

  // Title Label
  FLabelTitle := TLabel.Create(Self);
  FLabelTitle.Parent := Self;
  FLabelTitle.Align := alTop;
  FLabelTitle.AlignWithMargins := True;
  FLabelTitle.Margins.SetBounds(12, 12, 12, 0);
  FLabelTitle.Caption := Format('Filter for "%s":', [AColumnInfo.FieldName]);
  FLabelTitle.Font.Style := [fsBold];

  // Button Panel (Bottom)
  FButtonPanel := TPanel.Create(Self);
  FButtonPanel.Parent := Self;
  FButtonPanel.Align := alBottom;
  FButtonPanel.Height := 48;
  FButtonPanel.BevelOuter := bvNone;
  if TVittixStylePalette.IsCustomStyleActive(Self) then
    // A custom style paints the panel surface itself; the hard-coded face
    // color would punch a classic-gray hole into the styled dialog.
    FButtonPanel.ParentBackground := True
  else
  begin
    FButtonPanel.ParentBackground := False;
    FButtonPanel.Color := clBtnFace;
  end;

  // Buttons
  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent := FButtonPanel;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.Align := alRight;
  FBtnCancel.AlignWithMargins := True;
  FBtnCancel.Margins.SetBounds(4, 8, 8, 8);
  FBtnCancel.Width := 80;

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := FButtonPanel;
  FBtnOK.Caption := 'OK';
  FBtnOK.ModalResult := mrOk;
  FBtnOK.Align := alRight;
  FBtnOK.AlignWithMargins := True;
  FBtnOK.Margins.SetBounds(4, 8, 4, 8);
  FBtnOK.Width := 80;
  FBtnOK.Default := True;

  FBtnClear := TButton.Create(Self);
  FBtnClear.Parent := FButtonPanel;
  FBtnClear.Caption := 'Clear Filter';
  FBtnClear.Align := alLeft;
  FBtnClear.AlignWithMargins := True;
  FBtnClear.Margins.SetBounds(8, 8, 4, 8);
  FBtnClear.Width := 90;
  FBtnClear.OnClick := BtnClearClick;

  FBtnClearHistory := TButton.Create(Self);
  FBtnClearHistory.Parent := FButtonPanel;
  FBtnClearHistory.Caption := 'Clear History';
  FBtnClearHistory.Align := alLeft;
  FBtnClearHistory.AlignWithMargins := True;
  FBtnClearHistory.Margins.SetBounds(4, 8, 4, 8);
  FBtnClearHistory.Width := 100;
  FBtnClearHistory.OnClick := BtnClearHistoryClick;

  // Saved filters button: opens the set menu (apply / save as / manage).
  // Disabled when the owner is not a Vittix grid with a controller.
  FBtnSaved := TButton.Create(Self);
  FBtnSaved.Parent := FButtonPanel;
  FBtnSaved.Caption := 'Saved...';
  FBtnSaved.Align := alLeft;
  FBtnSaved.AlignWithMargins := True;
  FBtnSaved.Margins.SetBounds(4, 8, 4, 8);
  FBtnSaved.Width := 84;
  FBtnSaved.OnClick := BtnSavedClick;
  // Enabled only when the owner is a Vittix grid with a controller.
  FBtnSaved.Enabled := Assigned(PopupOwnerController(Self));

  FSavedMenu := TPopupMenu.Create(Self);

  // Validation Label
  FValidationLabel := TLabel.Create(Self);
  FValidationLabel.Parent := Self;
  FValidationLabel.Align := alBottom;
  FValidationLabel.AlignWithMargins := True;
  FValidationLabel.Margins.SetBounds(12, 2, 12, 2);
  // Resolved through the style service so styled apps get a readable red
  // variant; without a custom style this is plain clRed as before.
  FValidationLabel.Font.Color := TVittixStylePalette.ErrorColor(Self);
  FValidationLabel.Font.Style := [fsBold];
  FValidationLabel.Height := 20;
  FValidationLabel.Visible := False;

  // Recent Combo Box
  FRecentCombo := TComboBox.Create(Self);
  FRecentCombo.Parent := Self;
  FRecentCombo.Align := alTop;
  FRecentCombo.AlignWithMargins := True;
  FRecentCombo.Margins.SetBounds(12, 6, 12, 0);
  FRecentCombo.Style := csDropDown;

  FOperatorCombo := TComboBox.Create(Self);
  FOperatorCombo.Parent := Self;
  FOperatorCombo.Align := alTop;
  FOperatorCombo.AlignWithMargins := True;
  FOperatorCombo.Margins.SetBounds(12, 6, 12, 0);
  FOperatorCombo.Style := csDropDownList;
  // Combo items come from the shared operator table; its index order is the
  // persistence contract (stored OperatorIndex values).
  for I := 0 to VittixFilterOperatorCount - 1 do
    FOperatorCombo.Items.Add(VittixFilterOperatorDefinition(I).DisplayName);
  FOperatorCombo.ItemIndex := 0;
  
  // Load existing filter
  FOriginalText := Trim(FColumnInfo.FilterText);
  ApplySavedTextToControls(FOriginalText);

  // Feature: an "empty" filter reloads as the distinct-list display label
  // so the combo shows what the user originally picked.
  if (FOperatorCombo.ItemIndex = OperatorIndexFromPrefix('empty')) and
     (Trim(FRecentCombo.Text) = '') then
    FRecentCombo.Text := BlankValueCaption;

  if GFilterHistory.TryGetValue(FHistoryKey, LHistory) then
    FRecentCombo.Items.Assign(LHistory);

  // Persisted-history suggestions (INI, per-grid file) may only pre-fill the
  // dialog when the column has no active filter. In-memory operator memory
  // is deliberately NOT consulted: its key is owner-class based, so it leaks
  // between grids and sessions with the same field name.
  if FOriginalText = '' then
  begin
    LoadDistinctValues;
    LoadPersistedHistory;
  end
  else
    LoadDistinctValues;
  
  // Select all text so user can type to replace immediately
  FRecentCombo.SelectAll;

  ActiveControl := FRecentCombo;
  FRecentCombo.OnChange := ComboChange;
end;

function TVittixDBGridFilterPopup.NormalizeDisplayFilterText(
  const Value: string): string;
begin
  if SameText(Trim(Value), 'empty') then
    Exit(BlankValueCaption);
  Result := Value;
end;

procedure TVittixDBGridFilterPopup.PersistHistory;
begin
  ApplyChanges;
end;

function TVittixDBGridFilterPopup.ValidateCurrentInput: Boolean;
begin
  Result := ValidateInput;
end;

procedure TVittixDBGridFilterPopup.ClearHistory;
var
  LHistory: TStringList;
begin
  if GFilterHistory.TryGetValue(FHistoryKey, LHistory) then
    LHistory.Clear;
  if GFilterHistory.TryGetValue(FOperatorHistoryKey, LHistory) then
    LHistory.Clear;

  if FileExists(GetHistoryPath) then
    TFile.Delete(GetHistoryPath);

  FRecentCombo.Text := '';
  FOperatorCombo.ItemIndex := 0;
  ValidateInput;
end;

procedure TVittixDBGridFilterPopup.LoadPersistedHistory;
var
  Ini: TIniFile;
  FileName: string;
  SavedText: string;
  SavedOperator: Integer;
begin
  FileName := GetHistoryPath;
  if (FileName = '') or not FileExists(FileName) then
    Exit;

  Ini := TIniFile.Create(FileName);
  try
    SavedText := Ini.ReadString(FHistoryKey, 'LastFilter', '');

    if SavedText <> '' then
      // Restore through the same operator/value split used for active
      // filters so the combo never shows a raw operator prefix.
      ApplySavedTextToControls(NormalizeDisplayFilterText(SavedText))
    else
    begin
      // Operator-only memory (cleared text, remembered mode)
      SavedOperator := Ini.ReadInteger(FHistoryKey, 'OperatorIndex', FOperatorCombo.ItemIndex);
      if SavedOperator < 0 then
        SavedOperator := 0;
      if SavedOperator > FOperatorCombo.Items.Count - 1 then
        SavedOperator := FOperatorCombo.Items.Count - 1;
      FOperatorCombo.ItemIndex := SavedOperator;
    end;
  finally
    Ini.Free;
  end;
end;

function TVittixDBGridFilterPopup.GetHistoryPath: string;
begin
  // Resolution order: per-instance settings (from the owning grid) override
  // the process-wide class-var fallbacks.
  if FHistoryFileName <> '' then
    Exit(FHistoryFileName);
  if FRootPath <> '' then
    Exit(TPath.Combine(FRootPath, 'filter.ini'));
  if HistoryFileName <> '' then
    Exit(HistoryFileName);
  if RootPath <> '' then
    Exit(TPath.Combine(RootPath, 'filter.ini'));
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'VittixDBGridFilterHistory.ini';
end;

procedure TVittixDBGridFilterPopup.FormKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Key := 0;
  end;
  if Key = VK_RETURN then
  begin
    ApplyChanges;
    ModalResult := mrOk;
    Key := 0;
  end;
  if (Key = Ord('H')) and (ssCtrl in Shift) and (ssShift in Shift) then
  begin
    ExecuteClearHistoryShortcut;
    Key := 0;
  end;
end;

procedure TVittixDBGridFilterPopup.BtnClearClick(Sender: TObject);
begin
  FRecentCombo.Text := '';
  ApplyChanges;
  ModalResult := mrOk;
end;

procedure TVittixDBGridFilterPopup.CommitCurrentValue;
begin
  ApplyChanges;
end;

procedure TVittixDBGridFilterPopup.ExecuteClearHistoryShortcut;
begin
  ClearHistory;
end;

function TVittixDBGridFilterPopup.GetButtonShortcutSummaryText: string;
begin
  Result := 'Clear Filter=none;Clear History=Ctrl+Shift+H';
end;

procedure TVittixDBGridFilterPopup.BtnClearHistoryClick(Sender: TObject);
begin
  ClearHistory;
end;

procedure TVittixDBGridFilterPopup.ComboChange(Sender: TObject);
begin
  ValidateInput;
end;

function TVittixDBGridFilterPopup.GetOperatorPrefix: string;
begin
  if (FOperatorCombo.ItemIndex >= 0) and
     (FOperatorCombo.ItemIndex < VittixFilterOperatorCount) then
    Result := VittixFilterOperatorDefinition(FOperatorCombo.ItemIndex).Prefix
  else
    Result := '';
end;

function TVittixDBGridFilterPopup.OperatorIndexFromPrefix(
  const Prefix: string): Integer;
begin
  Result := VittixFilterOperatorIndexByPrefix(Prefix);
end;

function TVittixDBGridFilterPopup.GetOperatorIndex: Integer;
begin
  Result := FOperatorCombo.ItemIndex;
end;

function TVittixDBGridFilterPopup.GetFilterText: string;
begin
  Result := FRecentCombo.Text;
end;

procedure TVittixDBGridFilterPopup.SetFilterText(const Value: string);
begin
  FRecentCombo.Text := Value;
end;

procedure TVittixDBGridFilterPopup.LoadDistinctValues;
var
  Grid: TDBGrid;
  DataSet: TDataSet;
  Field: TField;
  Values: TStringList;
  Seen: TDictionary<string, Byte>;
  Bookmark: TBookmark;
  Value: string;
  ValueKey: string;
  HasBlank: Boolean;
begin
  if not (Owner is TDBGrid) then
    Exit;

  Grid := TDBGrid(Owner);
  if not Assigned(Grid.DataSource) then
    Exit;

  DataSet := Grid.DataSource.DataSet;
  if not Assigned(DataSet) or not DataSet.Active or
     (DataSet.State in dsEditModes) then
    Exit;

  Field := DataSet.FindField(FColumnInfo.FieldName);
  if not Assigned(Field) then
    Exit;

  Values := TStringList.Create;
  Seen := nil;
  try
    Seen := TDictionary<string, Byte>.Create;
    HasBlank := False;

    Bookmark := nil;
    if not DataSet.IsEmpty then
      Bookmark := DataSet.GetBookmark;
    DataSet.DisableControls;
    try
      DataSet.First;
      while not DataSet.Eof do
      begin
        if Field.IsNull then
          HasBlank := True
        else
        begin
          Value := Trim(Field.AsString);
          if Value = '' then
            HasBlank := True
          else
          begin
            ValueKey := AnsiUpperCase(Value);
            if not Seen.ContainsKey(ValueKey) then
            begin
              Seen.Add(ValueKey, 0);
              Values.Add(Value);
            end;
          end;
        end;
        DataSet.Next;
      end;
    finally
      try
        if Bookmark <> nil then
        begin
          try
            if DataSet.BookmarkValid(Bookmark) then
              DataSet.GotoBookmark(Bookmark);
          finally
            DataSet.FreeBookmark(Bookmark);
          end;
        end;
      finally
        DataSet.EnableControls;
      end;
    end;

    Values.Sort;
    if HasBlank then
      FRecentCombo.Items.Add(BlankValueCaption);
    if Values.Count > 0 then
      FRecentCombo.Items.AddStrings(Values);
  finally
    Seen.Free;
    Values.Free;
  end;
end;

function TVittixDBGridFilterPopup.ValidateInput: Boolean;
var
  IsValid: Boolean;
  ErrMsg: string;
  I: Integer;
  Found: Boolean;
begin
  IsValid := True;
  ErrMsg := '';

  if Assigned(OnValidateFilterInput) then
  begin
    OnValidateFilterInput(
      Self,
      FColumnInfo.FieldName,
      FRecentCombo.Text,
      IsValid,
      ErrMsg
    );
  end;

  if IsValid and FUseDistinctValuesOnly and (Trim(FRecentCombo.Text) <> '') and
    not VittixFilterOperatorDefinition(FOperatorCombo.ItemIndex).IsWordOperator then
  begin
    if SameText(Trim(FRecentCombo.Text), BlankValueCaption) then
    begin
      FOperatorCombo.ItemIndex := OperatorIndexFromPrefix('empty');
      Exit(True);
    end;
    Found := False;
    for I := 0 to FRecentCombo.Items.Count - 1 do
      if SameText(FRecentCombo.Items[I], FRecentCombo.Text) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
    begin
      IsValid := False;
      ErrMsg := 'Value must match one of the available items.';
    end;
  end;

  if IsValid then
  begin
    FValidationLabel.Visible := False;
    FBtnOK.Enabled := True;
  end
  else
  begin
    FValidationLabel.Caption := ErrMsg;
    FValidationLabel.Visible := True;
    FBtnOK.Enabled := False;
  end;

  Result := IsValid;
end;

procedure TVittixDBGridFilterPopup.ApplyChanges;
var
  NewText: string;
  LHistory: TStringList;
  Idx: Integer;
begin
  if not Assigned(FColumnInfo) then Exit;

  // Only apply if valid
  if not ValidateInput then Exit;

  // Word operators (null/empty family) carry no value; everything else is
  // prefix + typed text.
  if VittixFilterOperatorDefinition(FOperatorCombo.ItemIndex).IsWordOperator then
    NewText := GetOperatorPrefix
  else
    NewText := GetOperatorPrefix + Trim(FRecentCombo.Text);

  // Update history
  if NewText <> '' then
  begin
    if not GFilterHistory.TryGetValue(FHistoryKey, LHistory) then
    begin
      LHistory := TStringList.Create;
      GFilterHistory.Add(FHistoryKey, LHistory);
    end;

    Idx := LHistory.IndexOf(NewText);
    if Idx >= 0 then
      LHistory.Delete(Idx);
    LHistory.Insert(0, NewText);

    while LHistory.Count > 5 do
      LHistory.Delete(LHistory.Count - 1);
  end;

  if not GFilterHistory.TryGetValue(FOperatorHistoryKey, LHistory) then
  begin
    LHistory := TStringList.Create;
    GFilterHistory.Add(FOperatorHistoryKey, LHistory);
  end;
  LHistory.Clear;
  LHistory.Add(IntToStr(FOperatorCombo.ItemIndex));

  // Persist the current state on every commit — even when the filter text is
  // unchanged — so "last used filter/operator" survives restarts and empty
  // filters still create/refresh the history file.
  try
    with TIniFile.Create(GetHistoryPath) do
    try
      WriteString(FHistoryKey, 'LastFilter', NewText);
      WriteInteger(FHistoryKey, 'OperatorIndex', FOperatorCombo.ItemIndex);
    finally
      Free;
    end;
  except
    // Non-fatal; in-memory history still works. Surface the cause when
    // debugging (locked file, read-only media, ...) instead of hiding it.
    on E: Exception do
      {$IFDEF DEBUG}
      OutputDebugString(PChar('[Vittix] Filter history INI write failed: ' +
        E.ClassName + ' - ' + E.Message));
      {$ENDIF}
  end;

  // Optimistic update: Only change if different
  if NewText = FOriginalText then Exit;

  FColumnInfo.FilterText := NewText;
  FColumnInfo.HasFilter := NewText <> '';

  FOriginalText := NewText;
end;

class function TVittixDBGridFilterPopup.Execute(
  AOwner: TComponent;
  AColumnInfo: TVittixDBGridColumnInfo;
  AOnValidate: TFilterValidationEvent): Boolean;
var
  Frm: TVittixDBGridFilterPopup;
begin
  Result := False;

  if not Assigned(AColumnInfo) then Exit;

  Frm := TVittixDBGridFilterPopup.CreatePopup(AOwner, AColumnInfo);
  try
    Frm.OnValidateFilterInput := AOnValidate;
    Frm.ValidateInput;

    if Frm.ShowModal = mrOk then
    begin
      Frm.ApplyChanges;
      Result := True;
    end;
  finally
    Frm.Free;
  end;
end;

{ --- Saved filter sets menu --- }

// Execute passes the grid as owner; nil for non-Vittix usage. A unit-level
// helper because the controller type cannot appear in this class's
// interface (the controller's interface uses this unit).
function PopupOwnerController(
  APopup: TVittixDBGridFilterPopup): TVittixDBGridController;
begin
  Result := nil;
  if (APopup.Owner is TVittixDBGrid) and
     Assigned(TVittixDBGrid(APopup.Owner).Controller) then
    Result := TVittixDBGrid(APopup.Owner).Controller;
end;

procedure TVittixDBGridFilterPopup.BtnSavedClick(Sender: TObject);
var
  P: TPoint;
begin
  BuildSavedMenu;
  P := FBtnSaved.ClientToScreen(Point(0, FBtnSaved.Height));
  FSavedMenu.Popup(P.X, P.Y);
end;

procedure TVittixDBGridFilterPopup.ApplySavedSet(Sender: TObject);
var
  Controller: TVittixDBGridController;
begin
  Controller := PopupOwnerController(Self);
  if not Assigned(Controller) then Exit;
  if Controller.ApplyFilterSet(TMenuItem(Sender).Caption) then
  begin
    // Reflect the applied first-group conditions in this popup's editors.
    FOriginalText := FColumnInfo.FilterText;
    ApplySavedTextToControls(FOriginalText);
  end;
end;

procedure TVittixDBGridFilterPopup.SaveCurrentAsClick(Sender: TObject);
var
  Controller: TVittixDBGridController;
  Name: string;
begin
  Controller := PopupOwnerController(Self);
  if not Assigned(Controller) then Exit;

  Name := '';
  if not InputQuery('Save Current Filters', 'Filter set name:', Name) then
    Exit;
  if Trim(Name) = '' then
    Exit;
  Controller.SaveCurrentFilterAs(Trim(Name));
end;

procedure TVittixDBGridFilterPopup.ManageClick(Sender: TObject);
var
  Controller: TVittixDBGridController;
  Dialog: TVittixDBGridFilterSetManager;
  Columns: TStringList;
  I: Integer;
  AppliedName: string;
begin
  Controller := PopupOwnerController(Self);
  if not Assigned(Controller) then Exit;

  Columns := TStringList.Create;
  try
    if Assigned(Owner) and (Owner is TDBGrid) then
      for I := 0 to TDBGrid(Owner).Columns.Count - 1 do
        if TDBGrid(Owner).Columns[I].FieldName <> '' then
          Columns.Add(TDBGrid(Owner).Columns[I].FieldName);

    Dialog := TVittixDBGridFilterSetManager.CreateManager(Self,
      Controller.FilterSetList, Columns,
      procedure
      begin
        // The manager edits the list in place; persist after every change.
        Controller.PersistFilterSets;
      end,
      procedure(AName: string)
      begin
        Controller.SaveCurrentFilterAs(AName);
      end);
    try
      if Dialog.Execute(AppliedName) and (AppliedName <> '') then
        Controller.ApplyFilterSet(AppliedName);
    finally
      Dialog.Free;
    end;
  finally
    Columns.Free;
  end;
end;

procedure TVittixDBGridFilterPopup.BuildSavedMenu;
var
  Controller: TVittixDBGridController;
  Item: TMenuItem;
  Names: TArray<string>;
  I: Integer;
begin
  FSavedMenu.Items.Clear;
  Controller := PopupOwnerController(Self);
  if not Assigned(Controller) then
    Exit;

  Names := Controller.FilterSetNames;
  for I := 0 to High(Names) do
  begin
    Item := TMenuItem.Create(FSavedMenu);
    Item.Caption := Names[I];
    Item.OnClick := ApplySavedSet;
    FSavedMenu.Items.Add(Item);
  end;

  Item := TMenuItem.Create(FSavedMenu);
  Item.Caption := '-';
  FSavedMenu.Items.Add(Item);

  Item := TMenuItem.Create(FSavedMenu);
  Item.Caption := 'Save current as...';
  Item.OnClick := SaveCurrentAsClick;
  FSavedMenu.Items.Add(Item);

  Item := TMenuItem.Create(FSavedMenu);
  Item.Caption := 'Manage...';
  Item.OnClick := ManageClick;
  FSavedMenu.Items.Add(Item);
end;

initialization
  GFilterHistory := TObjectDictionary<string, TStringList>.Create([doOwnsValues]);
  TVittixDBGridFilterPopup.HistoryFileName := '';
  TVittixDBGridFilterPopup.RootPath := '';

finalization
  GFilterHistory.Free;

end.
