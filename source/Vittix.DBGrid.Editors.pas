unit Vittix.DBGrid.Editors;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.UITypes,
  Vittix.DBGrid.ColumnInfo,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.DBGrids,
  Vcl.Dialogs,
  Winapi.Windows, // Added for VK_ codes
  Winapi.Messages,
  Data.DB;

type
  /// <summary>
  /// Memo editor dialog
  /// </summary>
  TVittixDBGridMemoEditor = class(TForm)
  private
    FMemo: TMemo;
    FButtonPanel: TPanel;
    FBtnOK: TButton;
    FBtnCancel: TButton;
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor CreateEditor(
      AOwner: TComponent;
      const AText: string
    ); reintroduce;

    function Execute(var AText: string): Boolean;
  end;

  /// <summary>
  /// Date / Time editor dialog
  /// </summary>
  TVittixDBGridDateTimeEditor = class(TForm)
  private
    FDatePicker: TDateTimePicker;
    FTimePicker: TDateTimePicker;
    FTimePanel: TPanel;
    FButtonPanel: TPanel;
    FBtnOK: TButton;
    FBtnCancel: TButton;
    FBtnClear: TButton; // New: Allow setting to NULL
    FFieldType: TFieldType;
    FCanBeNull: Boolean;
    FIsNull: Boolean;
    
    procedure BtnClearClick(Sender: TObject);
  public
    constructor CreateEditor(
      AOwner: TComponent;
      const AValue: TDateTime;
      AFieldType: TFieldType;
      ACanBeNull: Boolean;
      AIsNull: Boolean
    ); reintroduce;

    function Execute(var AValue: TDateTime; var SetNull: Boolean): Boolean;
  end;

  // Validation event types
  TFieldValidationEvent = procedure(
    Sender: TObject;
    Field: TField;
    const NewValue: Variant;
    var IsValid: Boolean;
    var ErrorMessage: string
  ) of object;

  /// <summary>Raised by every Vittix popup editor before a value is
  /// committed: built-in Required/Min/Max rules run first, then this event.
  /// Accept = False keeps the editor open and shows Message.</summary>
  TVittixValidateCellEvent = procedure(
    Sender: TObject;
    Column: TColumn;
    Field: TField;
    var NewValue: Variant;
    var Accept: Boolean;
    var Message: string
  ) of object;

  /// <summary>One parsed pick-list line. "value=caption" pairs carry
  /// HasValue = True; plain lines write their text directly.</summary>
  TVittixPickItem = record
    Value: string;
    Caption: string;
    HasValue: Boolean;
  end;

  /// <summary>
  /// Pick-list popup editor (vePickList): a list of PickItems; Enter or a
  /// double click accepts the selection, Esc cancels.
  /// </summary>
  TVittixDBGridPickListEditor = class(TForm)
  private
    FList: TListBox;
    FButtonPanel: TPanel;
    FBtnOK: TButton;
    FBtnCancel: TButton;
    FItems: TArray<TVittixPickItem>;
    FSelectedValue: Variant;
    procedure ListDblClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor CreateEditor(AOwner: TComponent;
      const APickItems: string); reintroduce;
    function ShowAndPick(out AValue: Variant): Boolean;
  end;

  /// <summary>
  /// Lookup popup editor (veLookup): incremental search box plus a list
  /// from the field's LookupDataSet; accepting writes the key field value.
  /// </summary>
  TVittixDBGridLookupEditor = class(TForm)
  private
    FSearch: TEdit;
    FList: TListBox;
    FButtonPanel: TPanel;
    FBtnOK: TButton;
    FBtnCancel: TButton;
    FKeys: TArray<Variant>;
    FDisplayTexts: TStringList;
    FSelectedKey: Variant;
    procedure SearchChange(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FillList(const AFilter: string);
  public
    destructor Destroy; override;
    constructor CreateEditor(AOwner: TComponent;
      ALookupDataSet: TDataSet; const AKeyField, ADisplayField: string); reintroduce;
    function ShowAndPick(out AKey: Variant): Boolean;
  end;

  /// <summary>
  /// Masked numeric popup editor (veMaskedNumber): filters keystrokes to
  /// digits, one decimal separator and a leading minus (when Min &lt; 0);
  /// parsing and bounds run on OK through the shared validation.
  /// </summary>
  TVittixDBGridNumericEditor = class(TForm)
  private
    FEdit: TEdit;
    FButtonPanel: TPanel;
    FBtnOK: TButton;
    FBtnCancel: TButton;
    FAllowNegative: Boolean;
    function GetEditorText: string;
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor CreateEditor(AOwner: TComponent; const AText: string;
      AAllowNegative: Boolean); reintroduce;
    property EditorText: string read GetEditorText;
    function ShowAndEdit: Boolean;
  end;

  /// <summary>
  /// Editor dispatcher with validation support
  /// </summary>
  TVittixDBGridEditors = class
  private
    class var FOnValidateField: TFieldValidationEvent;

    class function EditPickList(Grid: TDBGrid; Column: TColumn;
      AInfo: TVittixDBGridColumnInfo; const APickItems: string;
      AOnValidateCell: TVittixValidateCellEvent): Boolean;
    class function EditLookup(Grid: TDBGrid; Column: TColumn;
      AInfo: TVittixDBGridColumnInfo;
      AOnValidateCell: TVittixValidateCellEvent): Boolean;
    class function EditMaskedNumber(Grid: TDBGrid; Column: TColumn;
      AInfo: TVittixDBGridColumnInfo;
      AOnValidateCell: TVittixValidateCellEvent): Boolean;
  public
    /// <summary>The shared commit gate: built-in rules (Required,
    /// Min/Max) first, then OnValidateCell, then the legacy global hook.
    /// AMessage carries the rejection reason. Public for tests and for
    /// applications hosting their own editors.</summary>
    class function RunValidation(Grid: TDBGrid; Column: TColumn;
      AInfo: TVittixDBGridColumnInfo; AField: TField;
      var ANewValue: Variant; AOnValidateCell: TVittixValidateCellEvent;
      out AMessage: string): Boolean;

    class function EditField(
      Grid: TDBGrid;
      Column: TColumn;
      AOnValidateCell: TVittixValidateCellEvent = nil
    ): Boolean;

    /// <summary>Splits PickItems text into items; "value=caption" pairs
    /// carry HasValue. Non-visual helper (unit tested).</summary>
    class function ParsePickItems(
      const APickItems: string): TArray<TVittixPickItem>;

    /// <summary>Runs the built-in rules (Required, Min/Max) for a new
    /// editor value. Non-visual helper (unit tested); the OnValidateCell
    /// event and the legacy hook run AFTER this in EditField.</summary>
    class function ValidateEditorValue(AInfo: TVittixDBGridColumnInfo;
      AField: TField; const ANewValue: Variant;
      out AMessage: string): Boolean;

    /// <summary>Parses the masked numeric entry: the locale decimal
    /// separator and '.' are both accepted, thousands separators are not.
    /// Non-visual helper (unit tested).</summary>
    class function TryParseNumericInput(const AText: string;
      out AValue: Double): Boolean;

    /// <summary>Rounds to APlaces decimals; APlaces &lt; 0 returns the
    /// value unchanged.</summary>
    class function ApplyDecimalPlaces(AValue: Double;
      APlaces: Integer): Double;

    // Global validation hook (legacy; runs after the per-column rules)
    class property OnValidateField: TFieldValidationEvent
      read FOnValidateField write FOnValidateField;
  end;

implementation

uses
  System.Math,
  System.StrUtils,
  Vcl.Graphics,
  Vcl.Themes,
  Vittix.DBGrid,
  Vittix.DBGrid.Styles;

{ ===== Pick List Editor ===== }

constructor TVittixDBGridPickListEditor.CreateEditor(AOwner: TComponent;
  const APickItems: string);
var
  I: Integer;
  StyleActive: Boolean;
begin
  inherited CreateNew(AOwner);

  Caption := 'Select Value';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Width := 300;
  Height := 320;

  FButtonPanel := TPanel.Create(Self);
  FButtonPanel.Parent := Self;
  FButtonPanel.Align := alBottom;
  FButtonPanel.Height := 48;
  FButtonPanel.BevelOuter := bvNone;
  StyleActive := TVittixStylePalette.IsCustomStyleActive(Self);
  if StyleActive then
    FButtonPanel.ParentBackground := True
  else
  begin
    FButtonPanel.ParentBackground := False;
    FButtonPanel.Color := clBtnFace;
  end;

  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent := FButtonPanel;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.Cancel := True;
  FBtnCancel.Align := alRight;
  FBtnCancel.AlignWithMargins := True;
  FBtnCancel.Margins.SetBounds(4, 8, 8, 8);
  FBtnCancel.Width := 80;

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := FButtonPanel;
  FBtnOK.Caption := 'OK';
  FBtnOK.Default := True;
  FBtnOK.Align := alRight;
  FBtnOK.AlignWithMargins := True;
  FBtnOK.Margins.SetBounds(4, 8, 4, 8);
  FBtnOK.Width := 80;

  FList := TListBox.Create(Self);
  FList.Parent := Self;
  FList.Align := alClient;
  FList.AlignWithMargins := True;
  FList.Margins.SetBounds(8, 8, 8, 4);
  FList.OnDblClick := ListDblClick;
  FList.OnKeyDown := ListKeyDown;

  // Items render as captions; a paired value (value=caption) is what an
  // integer field receives on commit.
  FItems := TVittixDBGridEditors.ParsePickItems(APickItems);
  for I := 0 to High(FItems) do
    FList.Items.Add(FItems[I].Caption);
  if FList.Items.Count > 0 then
    FList.ItemIndex := 0;
  ActiveControl := FList;
end;

procedure TVittixDBGridPickListEditor.ListDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TVittixDBGridPickListEditor.ListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ModalResult := mrOk;
    Key := 0;
  end;
end;

function TVittixDBGridPickListEditor.ShowAndPick(out AValue: Variant): Boolean;
var
  Index: Integer;
  Item: TVittixPickItem;
begin
  Result := ShowModal = mrOk;
  if not Result then
    Exit;

  Index := FList.ItemIndex;
  if (Index < 0) or (Index > High(FItems)) then
    Exit(False);

  // A paired value is reported as the value text (the dispatcher maps it
  // to the field); a plain caption writes itself.
  Item := FItems[Index];
  if Item.HasValue then
    AValue := Item.Value
  else
    AValue := Item.Caption;
end;

{ ===== Lookup Editor ===== }

constructor TVittixDBGridLookupEditor.CreateEditor(AOwner: TComponent;
  ALookupDataSet: TDataSet; const AKeyField, ADisplayField: string);
var
  StyleActive: Boolean;
begin
  inherited CreateNew(AOwner);

  Caption := 'Select Item';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Width := 360;
  Height := 420;

  FButtonPanel := TPanel.Create(Self);
  FButtonPanel.Parent := Self;
  FButtonPanel.Align := alBottom;
  FButtonPanel.Height := 48;
  FButtonPanel.BevelOuter := bvNone;
  StyleActive := TVittixStylePalette.IsCustomStyleActive(Self);
  if StyleActive then
    FButtonPanel.ParentBackground := True
  else
  begin
    FButtonPanel.ParentBackground := False;
    FButtonPanel.Color := clBtnFace;
  end;

  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent := FButtonPanel;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.Cancel := True;
  FBtnCancel.Align := alRight;
  FBtnCancel.AlignWithMargins := True;
  FBtnCancel.Margins.SetBounds(4, 8, 8, 8);
  FBtnCancel.Width := 80;

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := FButtonPanel;
  FBtnOK.Caption := 'OK';
  FBtnOK.Default := True;
  FBtnOK.Align := alRight;
  FBtnOK.AlignWithMargins := True;
  FBtnOK.Margins.SetBounds(4, 8, 4, 8);
  FBtnOK.Width := 80;

  FSearch := TEdit.Create(Self);
  FSearch.Parent := Self;
  FSearch.Align := alTop;
  FSearch.AlignWithMargins := True;
  FSearch.Margins.SetBounds(8, 8, 8, 0);
  FSearch.TextHint := 'Type to filter...';
  FSearch.OnChange := SearchChange;

  FList := TListBox.Create(Self);
  FList.Parent := Self;
  FList.Align := alClient;
  FList.AlignWithMargins := True;
  FList.Margins.SetBounds(8, 4, 8, 4);
  FList.OnDblClick := ListDblClick;
  FList.OnKeyDown := ListKeyDown;

  // The list snapshots the lookup dataset once; the search box filters the
  // snapshot incrementally (no repeated dataset scans while typing).
  FKeys := nil;
  FDisplayTexts := TStringList.Create;
  if Assigned(ALookupDataSet) and ALookupDataSet.Active then
  begin
    ALookupDataSet.DisableControls;
    try
      ALookupDataSet.First;
      while not ALookupDataSet.Eof do
      begin
        SetLength(FKeys, Length(FKeys) + 1);
        FKeys[High(FKeys)] := ALookupDataSet.FieldByName(AKeyField).Value;
        FDisplayTexts.Add(
          ALookupDataSet.FieldByName(ADisplayField).DisplayText);
        ALookupDataSet.Next;
      end;
    finally
      ALookupDataSet.EnableControls;
    end;
  end;
  FillList('');
  ActiveControl := FSearch;
end;

destructor TVittixDBGridLookupEditor.Destroy;
begin
  FreeAndNil(FDisplayTexts);
  inherited;
end;

procedure TVittixDBGridLookupEditor.FillList(const AFilter: string);
var
  I: Integer;
  UpperFilter: string;
begin
  FList.Items.BeginUpdate;
  try
    FList.Items.Clear;
    UpperFilter := UpperCase(AFilter);
    for I := 0 to FDisplayTexts.Count - 1 do
      if (AFilter = '') or
         (Pos(UpperFilter, UpperCase(FDisplayTexts[I])) > 0) then
        FList.Items.AddObject(FDisplayTexts[I], TObject(I));
    if FList.Items.Count > 0 then
      FList.ItemIndex := 0;
  finally
    FList.Items.EndUpdate;
  end;
end;

procedure TVittixDBGridLookupEditor.SearchChange(Sender: TObject);
begin
  FillList(FSearch.Text);
end;

procedure TVittixDBGridLookupEditor.ListDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TVittixDBGridLookupEditor.ListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ModalResult := mrOk;
    Key := 0;
  end;
end;

function TVittixDBGridLookupEditor.ShowAndPick(out AKey: Variant): Boolean;
var
  SnapshotIndex: Integer;
begin
  Result := ShowModal = mrOk;
  if not Result then
    Exit;

  if (FList.ItemIndex < 0) or (FList.ItemIndex >= FList.Items.Count) then
    Exit(False);

  SnapshotIndex := NativeInt(FList.Items.Objects[FList.ItemIndex]);
  if (SnapshotIndex < 0) or (SnapshotIndex > High(FKeys)) then
    Exit(False);
  AKey := FKeys[SnapshotIndex];
end;

{ ===== Masked Numeric Editor ===== }

constructor TVittixDBGridNumericEditor.CreateEditor(AOwner: TComponent;
  const AText: string; AAllowNegative: Boolean);
var
  StyleActive: Boolean;
begin
  inherited CreateNew(AOwner);

  Caption := 'Edit Number';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Width := 280;
  Height := 150;

  FAllowNegative := AAllowNegative;

  FButtonPanel := TPanel.Create(Self);
  FButtonPanel.Parent := Self;
  FButtonPanel.Align := alBottom;
  FButtonPanel.Height := 48;
  FButtonPanel.BevelOuter := bvNone;
  StyleActive := TVittixStylePalette.IsCustomStyleActive(Self);
  if StyleActive then
    FButtonPanel.ParentBackground := True
  else
  begin
    FButtonPanel.ParentBackground := False;
    FButtonPanel.Color := clBtnFace;
  end;

  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent := FButtonPanel;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.Cancel := True;
  FBtnCancel.Align := alRight;
  FBtnCancel.AlignWithMargins := True;
  FBtnCancel.Margins.SetBounds(4, 8, 8, 8);
  FBtnCancel.Width := 80;

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := FButtonPanel;
  FBtnOK.Caption := 'OK';
  FBtnOK.Default := True;
  FBtnOK.Align := alRight;
  FBtnOK.AlignWithMargins := True;
  FBtnOK.Margins.SetBounds(4, 8, 4, 8);
  FBtnOK.Width := 80;

  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Align := alTop;
  FEdit.AlignWithMargins := True;
  FEdit.Margins.SetBounds(16, 24, 16, 0);
  FEdit.Text := AText;
  FEdit.OnKeyPress := EditKeyPress;
  FEdit.OnKeyDown := EditKeyDown;
  ActiveControl := FEdit;
end;

function TVittixDBGridNumericEditor.GetEditorText: string;
begin
  Result := FEdit.Text;
end;

procedure TVittixDBGridNumericEditor.EditKeyPress(Sender: TObject;
  var Key: Char);
var
  Sep: Char;
begin
  // Digits, backspace and at most one decimal separator (the locale one
  // and '.' are both accepted for entry). A minus is allowed only as the
  // first character and only when the column's Min allows negatives.
  Sep := FormatSettings.DecimalSeparator;
  case Key of
    '0'..'9', #8:
      Exit;
  else
    if (Key = Sep) or (Key = '.') then
    begin
      // One separator only
      if (Pos(Sep, FEdit.Text) > 0) or (Pos('.', FEdit.Text) > 0) then
        Key := #0;
      Exit;
    end;
    if (Key = '-') and FAllowNegative and (FEdit.SelStart = 0) and
       (Pos('-', FEdit.Text) = 0) then
      Exit;
  end;
  Key := #0;
end;

procedure TVittixDBGridNumericEditor.EditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ModalResult := mrOk;
    Key := 0;
  end;
end;

function TVittixDBGridNumericEditor.ShowAndEdit: Boolean;
begin
  Result := ShowModal = mrOk;
end;

{ ===== Non-visual helpers ===== }

constructor TVittixDBGridMemoEditor.CreateEditor(
  AOwner: TComponent;
  const AText: string);
begin
  inherited CreateNew(AOwner);

  Caption := 'Edit Text';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Width := 520;
  Height := 360;

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

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := FButtonPanel;
  FBtnOK.Caption := 'OK';
  FBtnOK.ModalResult := mrOk;
  FBtnOK.Align := alRight;
  FBtnOK.AlignWithMargins := True;
  FBtnOK.Margins.SetBounds(4, 8, 4, 8);
  FBtnOK.Width := 90;
  FBtnOK.Default := True;

  FMemo := TMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Align := alClient;
  FMemo.ScrollBars := ssVertical;
  FMemo.WordWrap := True;
  FMemo.Lines.Text := AText;
  FMemo.OnKeyDown := MemoKeyDown; // Hook for Ctrl+A

  ActiveControl := FMemo;
end;

procedure TVittixDBGridMemoEditor.MemoKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Support Ctrl+A to select all text
  if (Key = Ord('A')) and (ssCtrl in Shift) then
  begin
    FMemo.SelectAll;
    Key := 0;
  end;
end;

function TVittixDBGridMemoEditor.Execute(var AText: string): Boolean;
begin
  Result := ShowModal = mrOk;
  if Result then
    AText := FMemo.Lines.Text;
end;

{ ===== Date / Time Editor ===== }

constructor TVittixDBGridDateTimeEditor.CreateEditor(
  AOwner: TComponent;
  const AValue: TDateTime;
  AFieldType: TFieldType;
  ACanBeNull: Boolean;
  AIsNull: Boolean);
begin
  inherited CreateNew(AOwner);

  FFieldType := AFieldType;
  FCanBeNull := ACanBeNull;
  FIsNull := AIsNull;

  Caption := 'Select Date / Time';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Width := 340; // Widened slightly for Clear button

  if AFieldType = ftDateTime then
    Height := 200
  else
    Height := 160;

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
  FBtnCancel.Width := 80;
  FBtnCancel.Cancel := True;

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := FButtonPanel;
  FBtnOK.Caption := 'OK';
  FBtnOK.ModalResult := mrOk;
  FBtnOK.Align := alRight;
  FBtnOK.AlignWithMargins := True;
  FBtnOK.Margins.SetBounds(4, 8, 4, 8);
  FBtnOK.Width := 80;
  FBtnOK.Default := True;

  // New: Clear Button
  if FCanBeNull then
  begin
    FBtnClear := TButton.Create(Self);
    FBtnClear.Parent := FButtonPanel;
    FBtnClear.Caption := 'Clear';
    FBtnClear.Align := alLeft;
    FBtnClear.AlignWithMargins := True;
    FBtnClear.Margins.SetBounds(8, 8, 4, 8);
    FBtnClear.Width := 80;
    FBtnClear.OnClick := BtnClearClick;
  end;

  // Date picker setup
  FDatePicker := TDateTimePicker.Create(Self);
  FDatePicker.Parent := Self;
  FDatePicker.Align := alTop;
  FDatePicker.AlignWithMargins := True;
  FDatePicker.Margins.SetBounds(16, 20, 16, 0);
  
  if not AIsNull then
    FDatePicker.DateTime := AValue
  else
    FDatePicker.DateTime := Now; // Default to now if null, but don't set flag yet
    
  FDatePicker.Kind := dtkDate;

  case AFieldType of
    ftTime:
      begin
        FDatePicker.Visible := False;
        FTimePicker := TDateTimePicker.Create(Self);
        FTimePicker.Parent := Self;
        FTimePicker.Align := alTop;
        FTimePicker.AlignWithMargins := True;
        FTimePicker.Margins.SetBounds(16, 20, 16, 0);
        if not AIsNull then
          FTimePicker.DateTime := AValue
        else
          FTimePicker.DateTime := Now;
        FTimePicker.Kind := dtkTime;
        ActiveControl := FTimePicker;
      end;
    ftDateTime:
      begin
        FTimePanel := TPanel.Create(Self);
        FTimePanel.Parent := Self;
        FTimePanel.Align := alTop;
        FTimePanel.Height := 40;
        FTimePanel.BevelOuter := bvNone;
        FTimePanel.AlignWithMargins := True;
        FTimePanel.Margins.SetBounds(0, 8, 0, 0);

        FTimePicker := TDateTimePicker.Create(Self);
        FTimePicker.Parent := FTimePanel;
        FTimePicker.Align := alTop;
        FTimePicker.AlignWithMargins := True;
        FTimePicker.Margins.SetBounds(16, 0, 16, 0);
        if not AIsNull then
          FTimePicker.DateTime := AValue
        else
          FTimePicker.DateTime := Now;
        FTimePicker.Kind := dtkTime;

        ActiveControl := FDatePicker;
      end;
  else
    FTimePicker := nil;
    ActiveControl := FDatePicker;
  end;
end;

procedure TVittixDBGridDateTimeEditor.BtnClearClick(Sender: TObject);
begin
  FIsNull := True;
  ModalResult := mrOk;
end;

function TVittixDBGridDateTimeEditor.Execute(
  var AValue: TDateTime; var SetNull: Boolean): Boolean;
var
  DatePart, TimePart: TDateTime;
begin
  SetNull := False;
  Result := ShowModal = mrOk;
  
  if Result then
  begin
    if FIsNull then
    begin
      SetNull := True;
      Exit;
    end;

    case FFieldType of
      ftTime:
        AValue := FTimePicker.DateTime;
      ftDateTime:
        begin
          DatePart := Trunc(FDatePicker.DateTime);
          TimePart := Frac(FTimePicker.DateTime);
          AValue := DatePart + TimePart;
        end;
    else
      AValue := FDatePicker.DateTime;
    end;
  end;
end;

{ ===== Non-visual helpers ===== }

// Ordinal and float variants only: strings are not coerced here so the
// Min/Max bounds never fire on text fields.
function TrySimpleVariantToFloat(const AValue: Variant;
  out ANumeric: Extended): Boolean;
begin
  Result := VarIsOrdinal(AValue) or VarIsFloat(AValue);
  if Result then
    ANumeric := AValue;
end;

class function TVittixDBGridEditors.ParsePickItems(
  const APickItems: string): TArray<TVittixPickItem>;
var
  Lines: TStringList;
  I, SepPos: Integer;
  Item: TVittixPickItem;
  Line: string;
begin
  Result := nil;
  Lines := TStringList.Create;
  try
    Lines.Text := APickItems;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if Line = '' then
        Continue;

      Item := Default(TVittixPickItem);
      SepPos := Pos('=', Line);
      if SepPos > 0 then
      begin
        // "value=caption": an integer field receives the value, the list
        // shows the caption.
        Item.Value := Trim(Copy(Line, 1, SepPos - 1));
        Item.Caption := Trim(Copy(Line, SepPos + 1, MaxInt));
        Item.HasValue := Item.Value <> '';
        if Item.Caption = '' then
          Item.Caption := Item.Value;
      end
      else
        Item.Caption := Line;
      if Item.Caption <> '' then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Item;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

class function TVittixDBGridEditors.ValidateEditorValue(
  AInfo: TVittixDBGridColumnInfo; AField: TField;
  const ANewValue: Variant; out AMessage: string): Boolean;
var
  NumValue: Extended;
  FieldName: string;
begin
  Result := True;
  AMessage := '';
  FieldName := '';
  if Assigned(AField) then
    FieldName := AField.FieldName;

  // Required: nothing at all, or a blank string, is rejected.
  if Assigned(AInfo) and AInfo.Required then
  begin
    if VarIsNull(ANewValue) or VarIsEmpty(ANewValue) or
       (VarIsStr(ANewValue) and (Trim(ANewValue) = '')) then
    begin
      AMessage := Format('A value is required for "%s".', [FieldName]);
      Exit(False);
    end;
  end;

  // Min/Max apply to numeric values only (the masked numeric editor's
  // domain); text values skip the bounds.
  if Assigned(AInfo) and (AInfo.HasMin or AInfo.HasMax) and
     TrySimpleVariantToFloat(ANewValue, NumValue) then
  begin
    if AInfo.HasMin and (NumValue < AInfo.MinValue) then
    begin
      AMessage := Format('The value for "%s" must not be below %g.',
        [FieldName, AInfo.MinValue]);
      Exit(False);
    end;
    if AInfo.HasMax and (NumValue > AInfo.MaxValue) then
    begin
      AMessage := Format('The value for "%s" must not exceed %g.',
        [FieldName, AInfo.MaxValue]);
      Exit(False);
    end;
  end;
end;

class function TVittixDBGridEditors.TryParseNumericInput(
  const AText: string; out AValue: Double): Boolean;
var
  Cleaned: string;
  Sep: Char;
  DotCount, I: Integer;
begin
  Result := False;
  Cleaned := Trim(AText);
  if Cleaned = '' then
    Exit;

  // Both the locale separator and '.' are accepted for entry (a user with
  // a comma locale can still type a dot); at most ONE separator total.
  Sep := FormatSettings.DecimalSeparator;
  DotCount := 0;
  for I := 1 to Length(Cleaned) do
    if (Cleaned[I] = Sep) or (Cleaned[I] = '.') then
      Inc(DotCount);
  if DotCount > 1 then
    Exit;

  Cleaned := StringReplace(Cleaned, '.', Sep, [rfReplaceAll]);
  if TryStrToFloat(Cleaned, AValue, FormatSettings) then
    Exit(True);

  // A ',' entry on a dot-locale machine: fall back to treating ',' as the
  // separator with an invariant parse.
  if Sep <> ',' then
  begin
    Cleaned := StringReplace(Trim(AText), ',', '.', [rfReplaceAll]);
    Result := TryStrToFloat(Cleaned, AValue, TFormatSettings.Invariant);
  end;
end;

class function TVittixDBGridEditors.ApplyDecimalPlaces(
  AValue: Double; APlaces: Integer): Double;
begin
  if APlaces < 0 then
    Result := AValue
  else
    Result := SimpleRoundTo(AValue, -APlaces);
end;

{ ===== Dispatcher ===== }

class function TVittixDBGridEditors.RunValidation(Grid: TDBGrid;
  Column: TColumn; AInfo: TVittixDBGridColumnInfo; AField: TField;
  var ANewValue: Variant; AOnValidateCell: TVittixValidateCellEvent;
  out AMessage: string): Boolean;
var
  Accept: Boolean;
begin
  // 1. Built-in rules (Required, Min/Max) run first.
  Result := ValidateEditorValue(AInfo, AField, ANewValue, AMessage);
  if not Result then
    Exit;

  // 2. The per-column validation event.
  Accept := True;
  if Assigned(AOnValidateCell) then
  begin
    AOnValidateCell(Grid, Column, AField, ANewValue, Accept, AMessage);
    if not Accept then
      Exit(False);
  end;

  // 3. The legacy global hook keeps its final-gate position.
  if Assigned(FOnValidateField) then
  begin
    Accept := True;
    FOnValidateField(nil, AField, ANewValue, Accept, AMessage);
    Result := Accept;
  end;
end;

class function TVittixDBGridEditors.EditPickList(Grid: TDBGrid;
  Column: TColumn; AInfo: TVittixDBGridColumnInfo; const APickItems: string;
  AOnValidateCell: TVittixValidateCellEvent): Boolean;
var
  Editor: TVittixDBGridPickListEditor;
  NewValue: Variant;
  ErrorMsg: string;
  Committed: Boolean;
begin
  Result := False;
  Editor := TVittixDBGridPickListEditor.CreateEditor(Grid, APickItems);
  try
    // A rejected value re-opens the dialog so the user can correct or
    // cancel; Esc (mrCancel) aborts the whole edit.
    repeat
      Committed := False;
      if not Editor.ShowAndPick(NewValue) then
        Exit(False);

      if RunValidation(Grid, Column, AInfo, Column.Field, NewValue,
        AOnValidateCell, ErrorMsg) then
      begin
        Column.Field.Value := NewValue;
        Committed := True;
      end
      else
        MessageDlg('Validation Error' + sLineBreak + ErrorMsg,
          mtError, [mbOK], 0);
    until Committed;
    Result := True;
  finally
    Editor.Free;
  end;
end;

class function TVittixDBGridEditors.EditLookup(Grid: TDBGrid;
  Column: TColumn; AInfo: TVittixDBGridColumnInfo;
  AOnValidateCell: TVittixValidateCellEvent): Boolean;
var
  Editor: TVittixDBGridLookupEditor;
  Field: TField;
  NewValue: Variant;
  ErrorMsg: string;
  Committed: Boolean;
  KeyField, DisplayField: string;
begin
  Result := False;
  Field := Column.Field;

  // Lookup source: the field's own lookup configuration. A multi-field key
  // is not supported by the popup list (single KeyFields only).
  if not Assigned(Field.LookupDataSet) or not Field.LookupDataSet.Active then
    Exit(False);
  KeyField := Field.LookupKeyFields;
  if Pos(';', KeyField) > 0 then
    Exit(False);
  DisplayField := Field.LookupResultField;
  if DisplayField = '' then
    Exit(False);

  Editor := TVittixDBGridLookupEditor.CreateEditor(Grid,
    Field.LookupDataSet, KeyField, DisplayField);
  try
    repeat
      Committed := False;
      if not Editor.ShowAndPick(NewValue) then
        Exit(False);

      if RunValidation(Grid, Column, AInfo, Field, NewValue,
        AOnValidateCell, ErrorMsg) then
      begin
        Field.Value := NewValue;
        Committed := True;
      end
      else
        MessageDlg('Validation Error' + sLineBreak + ErrorMsg,
          mtError, [mbOK], 0);
    until Committed;
    Result := True;
  finally
    Editor.Free;
  end;
end;

class function TVittixDBGridEditors.EditMaskedNumber(Grid: TDBGrid;
  Column: TColumn; AInfo: TVittixDBGridColumnInfo;
  AOnValidateCell: TVittixValidateCellEvent): Boolean;
var
  Editor: TVittixDBGridNumericEditor;
  Field: TField;
  ErrorMsg: string;
  Committed: Boolean;
  NewValue: Variant;
  NumValue: Double;
  CurrentText: string;
begin
  Result := False;
  Field := Column.Field;

  if Field.IsNull then
    CurrentText := ''
  else
    CurrentText := Field.AsString;

  Editor := TVittixDBGridNumericEditor.CreateEditor(Grid, CurrentText,
    not AInfo.HasMin or (AInfo.MinValue < 0));
  try
    repeat
      Committed := False;
      if not Editor.ShowAndEdit then
        Exit(False);

      if not TryParseNumericInput(Editor.EditorText, NumValue) then
      begin
        MessageDlg('Validation Error' + sLineBreak +
          'Enter a number (digits and one decimal separator).',
          mtError, [mbOK], 0);
        Continue;
      end;

      NumValue := ApplyDecimalPlaces(NumValue, AInfo.DecimalPlaces);

      // Integer fields take whole numbers only; a fractional entry is
      // rejected instead of silently truncated.
      if Field.DataType in [ftSmallint, ftInteger, ftWord, ftLargeint,
        ftShortint, ftByte, ftLongWord, ftAutoInc] then
      begin
        if Frac(NumValue) <> 0 then
        begin
          MessageDlg('Validation Error' + sLineBreak +
            'The field accepts whole numbers only.', mtError, [mbOK], 0);
          Continue;
        end;
        NewValue := Round(NumValue);
      end
      else
        NewValue := NumValue;

      if RunValidation(Grid, Column, AInfo, Field, NewValue,
        AOnValidateCell, ErrorMsg) then
      begin
        Field.Value := NewValue;
        Committed := True;
      end
      else
        MessageDlg('Validation Error' + sLineBreak + ErrorMsg,
          mtError, [mbOK], 0);
    until Committed;
    Result := True;
  finally
    Editor.Free;
  end;
end;

class function TVittixDBGridEditors.EditField(Grid: TDBGrid;
  Column: TColumn; AOnValidateCell: TVittixValidateCellEvent): Boolean;
var
  Field: TField;
  Text: string;
  DT: TDateTime;
  MemoEditor: TVittixDBGridMemoEditor;
  DateEditor: TVittixDBGridDateTimeEditor;
  WasEditing: Boolean;
  SetNull: Boolean;
  NewValue: Variant;
  ErrorMsg: string;
  Committed: Boolean;
  Info: TVittixDBGridColumnInfo;
  EditorKind: TVittixEditorKind;
begin
  Result := False;

  if (Grid = nil) or (Column = nil) then Exit;

  // Read-only columns never open an editor, even when the field itself
  // would accept writes.
  if Column.ReadOnly then Exit;

  Field := Column.Field;
  // Basic checks
  if not Assigned(Field) or not Field.CanModify then Exit;
  if not Assigned(Field.DataSet) then Exit;

  // Per-column editor configuration wins over the field-type defaults.
  EditorKind := veNone;
  if Grid is TVittixDBGrid then
  begin
    Info := TVittixDBGrid(Grid).ColumnInfoByColumn(Column);
    if Assigned(Info) then
      EditorKind := Info.EditorKind;
  end
  else
    Info := nil;

  // Track if we were already editing to handle Cancel properly
  WasEditing := Field.DataSet.State in dsEditModes;

  // If the dataset cannot edit (e.g. ReadOnly), let the exception raise
  // so the user knows why the dialog didn't open.
  if not WasEditing then
    Field.DataSet.Edit;

  try
    case EditorKind of
      vePickList:
      begin
        Result := EditPickList(Grid, Column, Info, Info.PickItems,
          AOnValidateCell);
        Exit;
      end;
      veLookup:
      begin
        Result := EditLookup(Grid, Column, Info, AOnValidateCell);
        Exit;
      end;
      veMaskedNumber:
      begin
        Result := EditMaskedNumber(Grid, Column, Info, AOnValidateCell);
        Exit;
      end;
    end;

    // --- Memo Handling ---
    if Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo] then
    begin
      MemoEditor := nil;
      try
        Text := Field.AsString;
        MemoEditor := TVittixDBGridMemoEditor.CreateEditor(Grid, Text);
        // A rejected value re-opens the dialog so the user can correct or
        // cancel; Esc (mrCancel) aborts the whole edit.
        repeat
          Committed := False;
          if not MemoEditor.Execute(Text) then
            Exit;

          NewValue := Text;
          if RunValidation(Grid, Column, Info, Field, NewValue,
            AOnValidateCell, ErrorMsg) then
          begin
            Field.AsString := Text;
            Committed := True;
          end
          else
            MessageDlg('Validation Error' + sLineBreak + ErrorMsg,
              mtError, [mbOK], 0);
        until Committed;
        Result := True;
      finally
        MemoEditor.Free;
      end;
    end

    // --- Date/Time Handling ---
    else if Field.DataType in [ftDate, ftTime, ftDateTime] then
    begin
      DateEditor := nil;
      try
        DT := 0;
        if not Field.IsNull then
          DT := Field.AsDateTime;

        DateEditor := TVittixDBGridDateTimeEditor.CreateEditor(
          Grid,
          DT,
          Field.DataType,
          not Field.Required, // Pass True if field can be NULL
          Field.IsNull
        );

        repeat
          Committed := False;
          if not DateEditor.Execute(DT, SetNull) then
            Exit;

          if SetNull then
            NewValue := Null
          else
            NewValue := DT;

          if RunValidation(Grid, Column, Info, Field, NewValue,
            AOnValidateCell, ErrorMsg) then
          begin
            if SetNull then
              Field.Clear
            else
              Field.AsDateTime := DT;
            Committed := True;
          end
          else
            MessageDlg('Validation Error' + sLineBreak + ErrorMsg,
              mtError, [mbOK], 0);
        until Committed;
        Result := True;
      finally
        DateEditor.Free;
      end;
    end;

  finally
    // If we initiated the edit state but the user Cancelled the dialog,
    // we should cancel the dataset state to revert to Browse mode.
    if not WasEditing and not Result and
       (Field.DataSet.State in dsEditModes) then
    begin
      try
        Field.DataSet.Cancel;
      except
        on E: Exception do
          // Log the error instead of silently ignoring
          {$IFDEF DEBUG}
          OutputDebugString(PChar('Cancel failed: ' + E.Message));
          {$ENDIF}
      end;
    end;
  end;
end;

end.