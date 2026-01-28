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
    FButtonPanel: TPanel;
    FBtnOK: TButton;
    FBtnClear: TButton;
    FBtnCancel: TButton;
    FLabelTitle: TLabel;
    FValidationLabel: TLabel;

    FColumnInfo: TVittixDBGridColumnInfo;
    FOriginalText: string;

    procedure BtnClearClick(Sender: TObject);
    procedure ApplyChanges;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboChange(Sender: TObject);
    function ValidateInput: Boolean;
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
  end;

implementation

uses
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
  
  // Load existing filter
  FOriginalText := Trim(FColumnInfo.FilterText);
  FRecentCombo.Text := FOriginalText;

  if GFilterHistory.TryGetValue(FColumnInfo.FieldName, LHistory) then
    FRecentCombo.Items.Assign(LHistory);
  
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

  NewText := Trim(FRecentCombo.Text);

  // Update history
  if NewText <> '' then
  begin
    if not GFilterHistory.TryGetValue(FColumnInfo.FieldName, LHistory) then
    begin
      LHistory := TStringList.Create;
      GFilterHistory.Add(FColumnInfo.FieldName, LHistory);
    end;

    Idx := LHistory.IndexOf(NewText);
    if Idx >= 0 then
      LHistory.Delete(Idx);
    LHistory.Insert(0, NewText);

    while LHistory.Count > 5 do
      LHistory.Delete(LHistory.Count - 1);
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

finalization
  GFilterHistory.Free;

end.