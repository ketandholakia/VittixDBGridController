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
  Vittix.DBGrid;

var
  GFilterHistory: TObjectDictionary<string, TStringList>;
const
  BlankValueCaption = '(Blank)';

{ TVittixDBGridFilterPopup }

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
  ClientWidth := 340;
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
  FButtonPanel.ParentBackground := False;
  FButtonPanel.Color := clBtnFace;

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

  // Validation Label
  FValidationLabel := TLabel.Create(Self);
  FValidationLabel.Parent := Self;
  FValidationLabel.Align := alBottom;
  FValidationLabel.AlignWithMargins := True;
  FValidationLabel.Margins.SetBounds(12, 2, 12, 2);
  FValidationLabel.Font.Color := clRed;
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
  HasBlank: Boolean;
begin
  if not (Owner is TDBGrid) then
    Exit;

  Grid := TDBGrid(Owner);
  if not Assigned(Grid.DataSource) then
    Exit;

  DataSet := Grid.DataSource.DataSet;
  if not Assigned(DataSet) or not DataSet.Active then
    Exit;

  Field := DataSet.FindField(FColumnInfo.FieldName);
  if not Assigned(Field) then
    Exit;

  Values := TStringList.Create;
  try
    Values.Sorted := True;
    Values.Duplicates := dupIgnore;
    HasBlank := False;

    DataSet.DisableControls;
    try
      DataSet.First;
      while not DataSet.Eof do
      begin
        if Field.IsNull or (Trim(Field.AsString) = '') then
          HasBlank := True
        else
          Values.Add(Trim(Field.AsString));
        DataSet.Next;
      end;
    finally
      DataSet.EnableControls;
    end;

    if HasBlank then
      FRecentCombo.Items.Add(BlankValueCaption);
    if Values.Count > 0 then
      FRecentCombo.Items.AddStrings(Values);
  finally
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

initialization
  GFilterHistory := TObjectDictionary<string, TStringList>.Create([doOwnsValues]);
  TVittixDBGridFilterPopup.HistoryFileName := '';
  TVittixDBGridFilterPopup.RootPath := '';

finalization
  GFilterHistory.Free;

end.
