unit Vittix.DBGrid.Editors;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.UITypes,
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

  // NEW: Validation event types
  TFieldValidationEvent = procedure(
    Sender: TObject;
    Field: TField;
    const NewValue: Variant;
    var IsValid: Boolean;
    var ErrorMessage: string
  ) of object;

  /// <summary>
  /// Editor dispatcher with validation support
  /// </summary>
  TVittixDBGridEditors = class
  private
    class var FOnValidateField: TFieldValidationEvent;
  public
    class function EditField(
      Grid: TDBGrid;
      Column: TColumn
    ): Boolean;
    
    // NEW: Global validation hook
    class property OnValidateField: TFieldValidationEvent 
      read FOnValidateField write FOnValidateField;
  end;

implementation

{ ===== Memo Editor ===== }

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

{ ===== Dispatcher ===== }

class function TVittixDBGridEditors.EditField(
  Grid: TDBGrid;
  Column: TColumn): Boolean;
var
  Field: TField;
  Text: string;
  DT: TDateTime;
  MemoEditor: TVittixDBGridMemoEditor;
  DateEditor: TVittixDBGridDateTimeEditor;
  WasEditing: Boolean;
  SetNull: Boolean;
  NewValue: Variant;
  IsValid: Boolean;
  ErrorMsg: string;
begin
  Result := False;

  if (Grid = nil) or (Column = nil) then Exit;

  Field := Column.Field;
  // Basic checks
  if not Assigned(Field) or not Field.CanModify then Exit;
  if not Assigned(Field.DataSet) then Exit;

  // Track if we were already editing to handle Cancel properly
  WasEditing := Field.DataSet.State in dsEditModes;

  // FIX: Silent failure removed. 
  // If the dataset cannot edit (e.g. ReadOnly), let the exception raise
  // so the user knows why the dialog didn't open.
  if not WasEditing then
    Field.DataSet.Edit;

  try
    // --- Memo Handling ---
    if Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo] then
    begin
      MemoEditor := nil;
      try
        Text := Field.AsString;
        MemoEditor := TVittixDBGridMemoEditor.CreateEditor(Grid, Text);
        if MemoEditor.Execute(Text) then
        begin
          // NEW: Validate before assigning
          if Assigned(FOnValidateField) then
          begin
            NewValue := Text;
            IsValid := True;
            ErrorMsg := '';
            FOnValidateField(nil, Field, NewValue, IsValid, ErrorMsg);
            
            if not IsValid then
            begin
              MessageDlg(
                'Validation Error' + sLineBreak + ErrorMsg,
                mtError,
                [mbOK],
                0
              );
              Exit; // Don't save invalid data
            end;
          end;
          
          Field.AsString := Text;
          Result := True;
        end;
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

        if DateEditor.Execute(DT, SetNull) then
        begin
          // NEW: Validate before assigning
          if Assigned(FOnValidateField) then
          begin
            if SetNull then
              NewValue := Null
            else
              NewValue := DT;
              
            IsValid := True;
            ErrorMsg := '';
            FOnValidateField(nil, Field, NewValue, IsValid, ErrorMsg);
            
            if not IsValid then
            begin
              MessageDlg(
                'Validation Error' + sLineBreak + ErrorMsg,
                mtError,
                [mbOK],
                0
              );
              Exit;
            end;
          end;
          
          if SetNull then
            Field.Clear
          else
            Field.AsDateTime := DT;
          Result := True;
        end;
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