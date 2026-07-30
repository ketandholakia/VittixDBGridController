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
    FBtnCancel: TButton;
    FLabelTitle: TLabel;
    FValidationLabel: TLabel;

    FColumnInfo: TVittixDBGridColumnInfo;
    FOriginalText: string;
    // FIX BUG 12: Scoped history key prevents two grids sharing history for
    // same-named fields. Key is "OwnerClassName.FieldName".
    FHistoryKey: string;
    FOperatorHistoryKey: string;

    procedure BtnClearClick(Sender: TObject);
    procedure ApplyChanges;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboChange(Sender: TObject);
    procedure LoadDistinctValues;
    function ValidateInput: Boolean;
    function GetOperatorPrefix: string;
    function OperatorIndexFromPrefix(const Prefix: string): Integer;
    function GetOperatorIndex: Integer;
    function GetFilterText: string;
  public
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

    property OperatorIndex: Integer read GetOperatorIndex;
    property FilterText: string read GetFilterText;
  end;

implementation

uses
  Data.DB,
  System.Math,
  System.Generics.Collections;

var
  GFilterHistory: TObjectDictionary<string, TStringList>;

{ TVittixDBGridFilterPopup }

constructor TVittixDBGridFilterPopup.CreatePopup(
  AOwner: TComponent;
  AColumnInfo: TVittixDBGridColumnInfo);
var
  LHistory: TStringList;
begin
  inherited CreateNew(AOwner);

  if not Assigned(AColumnInfo) then
    raise Exception.Create('ColumnInfo parameter cannot be nil');

  FColumnInfo := AColumnInfo;
  FOriginalText := '';

  // FIX BUG 12: Build a scoped history key using owner's class name so that
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
  FOperatorCombo.Items.Add('Contains');
  FOperatorCombo.Items.Add('Equals');
  FOperatorCombo.Items.Add('Starts With');
  FOperatorCombo.Items.Add('Ends With');
  FOperatorCombo.Items.Add('Not Equals');
  FOperatorCombo.Items.Add('Greater Than');
  FOperatorCombo.Items.Add('Greater or Equal');
  FOperatorCombo.Items.Add('Less Than');
  FOperatorCombo.Items.Add('Less or Equal');
  FOperatorCombo.ItemIndex := 0;
  
  // Load existing filter
  FOriginalText := Trim(FColumnInfo.FilterText);
  if FOriginalText <> '' then
  begin
    if Copy(FOriginalText, 1, 2) = '>=' then
    begin
      FOperatorCombo.ItemIndex := OperatorIndexFromPrefix('>=');
      FRecentCombo.Text := Trim(Copy(FOriginalText, 3, MaxInt));
    end
    else if Copy(FOriginalText, 1, 2) = '<=' then
    begin
      FOperatorCombo.ItemIndex := OperatorIndexFromPrefix('<=');
      FRecentCombo.Text := Trim(Copy(FOriginalText, 3, MaxInt));
    end
    else if Copy(FOriginalText, 1, 2) = '<>' then
    begin
      FOperatorCombo.ItemIndex := OperatorIndexFromPrefix('<>');
      FRecentCombo.Text := Trim(Copy(FOriginalText, 3, MaxInt));
    end
    else if (FOriginalText[1] = '=') or (FOriginalText[1] = '^') or
      (FOriginalText[1] = '$') or (FOriginalText[1] = '>') or
      (FOriginalText[1] = '<') then
    begin
      FOperatorCombo.ItemIndex := OperatorIndexFromPrefix(FOriginalText[1]);
      FRecentCombo.Text := Trim(Copy(FOriginalText, 2, MaxInt));
    end
    else
      FRecentCombo.Text := FOriginalText;
  end
  else
    FRecentCombo.Text := '';

  if GFilterHistory.TryGetValue(FHistoryKey, LHistory) then
    FRecentCombo.Items.Assign(LHistory);

  if GFilterHistory.TryGetValue(FOperatorHistoryKey, LHistory) and (LHistory.Count > 0) then
  begin
    FOperatorCombo.ItemIndex := StrToIntDef(LHistory[0], FOperatorCombo.ItemIndex);
    if FOperatorCombo.ItemIndex < 0 then
      FOperatorCombo.ItemIndex := 0;
    if FOperatorCombo.ItemIndex > FOperatorCombo.Items.Count - 1 then
      FOperatorCombo.ItemIndex := FOperatorCombo.Items.Count - 1;
  end;

  LoadDistinctValues;
  
  // Select all text so user can type to replace immediately
  FRecentCombo.SelectAll;

  ActiveControl := FRecentCombo;
  FRecentCombo.OnChange := ComboChange;
end;

procedure TVittixDBGridFilterPopup.FormKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Key := 0;
  end;
end;

procedure TVittixDBGridFilterPopup.BtnClearClick(Sender: TObject);
begin
  FRecentCombo.Text := '';
  ApplyChanges;
  ModalResult := mrOk;
end;

procedure TVittixDBGridFilterPopup.ComboChange(Sender: TObject);
begin
  ValidateInput;
end;

function TVittixDBGridFilterPopup.GetOperatorPrefix: string;
begin
  case FOperatorCombo.ItemIndex of
    1: Result := '=';
    2: Result := '^';
    3: Result := '$';
    4: Result := '<>';
    5: Result := '>';
    6: Result := '>=';
    7: Result := '<';
    8: Result := '<=';
  else
    Result := '';
  end;
end;

function TVittixDBGridFilterPopup.OperatorIndexFromPrefix(
  const Prefix: string): Integer;
begin
  if Prefix = '=' then Exit(1);
  if Prefix = '^' then Exit(2);
  if Prefix = '$' then Exit(3);
  if Prefix = '<>' then Exit(4);
  if Prefix = '>' then Exit(5);
  if Prefix = '>=' then Exit(6);
  if Prefix = '<' then Exit(7);
  if Prefix = '<=' then Exit(8);
  Result := 0;
end;

function TVittixDBGridFilterPopup.GetOperatorIndex: Integer;
begin
  Result := FOperatorCombo.ItemIndex;
end;

function TVittixDBGridFilterPopup.GetFilterText: string;
begin
  Result := FRecentCombo.Text;
end;

procedure TVittixDBGridFilterPopup.LoadDistinctValues;
var
  Grid: TDBGrid;
  DataSet: TDataSet;
  Field: TField;
  Values: TStringList;
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

    DataSet.DisableControls;
    try
      DataSet.First;
      while not DataSet.Eof do
      begin
        if not Field.IsNull then
          Values.Add(Trim(Field.AsString));
        DataSet.Next;
      end;
    finally
      DataSet.EnableControls;
    end;

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

finalization
  GFilterHistory.Free;

end.
