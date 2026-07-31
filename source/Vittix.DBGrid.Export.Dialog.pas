unit Vittix.DBGrid.Export.Dialog;

{$REGION 'Documentation'}
/// <summary>
/// Export Dialog for Vittix.DBGrid Component Suite
/// 
/// Provides a user-friendly interface for configuring and executing exports.
/// 
/// USAGE:
///   if TVittixExportDialog.Execute(VittixGrid) then
///     ShowMessage('Export completed successfully');
/// </summary>
{$ENDREGION}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vittix.DBGrid,
  Vittix.DBGrid.Export.Engine;

type
  TfrmExportDialog = class(TForm)
    pnlTop: TPanel;
    lblTitle: TLabel;
    pnlButtons: TPanel;
    btnExport: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    tabFormat: TTabSheet;
    tabOptions: TTabSheet;
    tabPreview: TTabSheet;
    grpFormat: TGroupBox;
    rbCSV: TRadioButton;
    rbTSV: TRadioButton;
    rbExcel: TRadioButton;
    rbHTML: TRadioButton;
    rbXML: TRadioButton;
    rbJSON: TRadioButton;
    rbText: TRadioButton;
    grpDestination: TGroupBox;
    rbFile: TRadioButton;
    rbClipboard: TRadioButton;
    edtFileName: TEdit;
    btnBrowse: TButton;
    grpScope: TGroupBox;
    chkVisibleOnly: TCheckBox;
    chkFilteredOnly: TCheckBox;
    chkIncludeHeaders: TCheckBox;
    // chkIncludeFooter is NOT in the DFM - created dynamically in FormCreate
    grpFormatting: TGroupBox;
    lblDateFormat: TLabel;
    edtDateFormat: TEdit;
    lblTimeFormat: TLabel;
    edtTimeFormat: TEdit;
    lblCurrencyFormat: TLabel;
    edtCurrencyFormat: TEdit;
    memoPreview: TMemo;
    ProgressBar1: TProgressBar;
    lblProgress: TLabel;
    SaveDialog1: TSaveDialog;
    
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure rbFormatClick(Sender: TObject);
    procedure rbFileClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    
  private
    FGrid: TVittixDBGrid;
    FExporter: TVittixDBGridExporter;
    chkIncludeFooter: TCheckBox; // Created dynamically in FormCreate (not in DFM)
    
    procedure LoadDefaults;
    procedure UpdateFileName;
    procedure GeneratePreview;
    procedure ExportProgress(Sender: TObject; Current, Total: Integer; var Cancel: Boolean);
    function GetSelectedFormat: TVittixExportFormat;
    function GetFileExtension: string;
    function GetStatePath: string;
    function GetIncludeFooterChecked: Boolean;
    procedure SetIncludeFooterChecked(const Value: Boolean);
    function GetTextFormatChecked: Boolean;
    procedure SetTextFormatChecked(const Value: Boolean);
    
  public
    class var StateFileName: string;
    class var RootPath: string;
    class function Execute(AGrid: TVittixDBGrid): Boolean;
    procedure LoadDialogState;
    procedure SaveDialogState;
    property IncludeFooterChecked: Boolean read GetIncludeFooterChecked write SetIncludeFooterChecked;
    property TextFormatChecked: Boolean read GetTextFormatChecked write SetTextFormatChecked;
    function GetPreviewText: string;
    function GetActivePageIndex: Integer;
    procedure SetActivePageIndex(Value: Integer);
  end;

  TVittixExportDialog = TfrmExportDialog;

var
  frmExportDialog: TfrmExportDialog;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  System.IniFiles;

{ TfrmExportDialog }

class function TfrmExportDialog.Execute(AGrid: TVittixDBGrid): Boolean;
var
  Dlg: TfrmExportDialog;
begin
  if not Assigned(AGrid) then
    raise Exception.Create('Grid is not assigned');
    
  if not Assigned(AGrid.DataSource) or not Assigned(AGrid.DataSource.DataSet) then
    raise Exception.Create('Grid does not have a valid DataSource');
    
  if not AGrid.DataSource.DataSet.Active then
    raise Exception.Create('Dataset is not active');
    
  Dlg := TfrmExportDialog.Create(nil);
  try
    Dlg.FGrid := AGrid;
    // LoadDefaults is called from FormCreate, not here.
    // Calling it before ShowModal means the form controls don't exist yet.
    Result := Dlg.ShowModal = mrOk;
  finally
    Dlg.Free;
  end;
end;

procedure TfrmExportDialog.FormCreate(Sender: TObject);
begin
  // chkIncludeFooter is not in the DFM — create it in code.
  // This must happen in FormCreate (not Execute) so controls exist before use.
  chkIncludeFooter := TCheckBox.Create(Self);
  chkIncludeFooter.Parent := grpScope;
  chkIncludeFooter.Name := 'chkIncludeFooter';
  chkIncludeFooter.Caption := 'Include Footer';
  chkIncludeFooter.Left := chkIncludeHeaders.Left;
  chkIncludeFooter.Top := chkIncludeHeaders.Top + chkIncludeHeaders.Height + 6;
  chkIncludeFooter.Width := chkIncludeHeaders.Width;

  rbText := TRadioButton.Create(Self);
  rbText.Parent := grpFormat;
  rbText.Name := 'rbText';
  rbText.Caption := 'Text';
  rbText.Left := rbJSON.Left;
  rbText.Top := rbJSON.Top + rbJSON.Height + 6;
  rbText.Width := rbJSON.Width;

  PageControl1.ActivePageIndex := 0;
  ProgressBar1.Visible := False;
  lblProgress.Visible := False;

  // Load defaults now that all controls exist
  LoadDefaults;
  LoadDialogState;
end;

procedure TfrmExportDialog.LoadDefaults;
begin
  // Format defaults
  rbCSV.Checked := True;
  rbFile.Checked := True;
  
  // Options defaults
  chkVisibleOnly.Checked := True;
  chkFilteredOnly.Checked := True;
  chkIncludeHeaders.Checked := True;
  chkIncludeFooter.Checked := False;
  
  // Formatting defaults
  edtDateFormat.Text := 'yyyy-mm-dd';
  edtTimeFormat.Text := 'hh:nn:ss';
  edtCurrencyFormat.Text := '#,##0.00';
  
  UpdateFileName;
end;

function TfrmExportDialog.GetStatePath: string;
begin
  if StateFileName <> '' then
    Exit(StateFileName);
  if RootPath <> '' then
    Exit(TPath.Combine(RootPath, 'export.ini'));
  Result := TPath.Combine(TPath.GetDocumentsPath, 'VittixDBGridExport.ini');
end;

function TfrmExportDialog.GetIncludeFooterChecked: Boolean;
begin
  Result := Assigned(chkIncludeFooter) and chkIncludeFooter.Checked;
end;

procedure TfrmExportDialog.SetIncludeFooterChecked(const Value: Boolean);
begin
  if Assigned(chkIncludeFooter) then
    chkIncludeFooter.Checked := Value;
end;

function TfrmExportDialog.GetTextFormatChecked: Boolean;
begin
  Result := Assigned(rbText) and rbText.Checked;
end;

procedure TfrmExportDialog.SetTextFormatChecked(const Value: Boolean);
begin
  if Assigned(rbText) then
    rbText.Checked := Value;
end;

procedure TfrmExportDialog.LoadDialogState;
var
  Ini: TIniFile;
  FileName: string;
  FormatIndex: Integer;
begin
  FileName := GetStatePath;
  if (FileName = '') or not FileExists(FileName) then
    Exit;

  Ini := TIniFile.Create(FileName);
  try
    FormatIndex := Ini.ReadInteger('Export', 'Format', 0);
    Left := Ini.ReadInteger('Export', 'Left', Left);
    Top := Ini.ReadInteger('Export', 'Top', Top);
    Width := Ini.ReadInteger('Export', 'Width', Width);
    Height := Ini.ReadInteger('Export', 'Height', Height);
    SetActivePageIndex(Ini.ReadInteger('Export', 'ActivePage', PageControl1.ActivePageIndex));
    case FormatIndex of
      0: rbCSV.Checked := True;
      1: rbTSV.Checked := True;
      2: rbExcel.Checked := True;
      3: rbHTML.Checked := True;
      4: rbXML.Checked := True;
      5: rbJSON.Checked := True;
      6: rbText.Checked := True;
    end;

    rbFile.Checked := Ini.ReadBool('Export', 'DestinationFile', True);
    rbClipboard.Checked := not rbFile.Checked;
    edtFileName.Text := Ini.ReadString('Export', 'FileName', edtFileName.Text);
    chkVisibleOnly.Checked := Ini.ReadBool('Export', 'VisibleOnly', chkVisibleOnly.Checked);
    chkFilteredOnly.Checked := Ini.ReadBool('Export', 'FilteredOnly', chkFilteredOnly.Checked);
    chkIncludeHeaders.Checked := Ini.ReadBool('Export', 'IncludeHeaders', chkIncludeHeaders.Checked);
    chkIncludeFooter.Checked := Ini.ReadBool('Export', 'IncludeFooter', chkIncludeFooter.Checked);
    edtDateFormat.Text := Ini.ReadString('Export', 'DateFormat', edtDateFormat.Text);
    edtTimeFormat.Text := Ini.ReadString('Export', 'TimeFormat', edtTimeFormat.Text);
    edtCurrencyFormat.Text := Ini.ReadString('Export', 'CurrencyFormat', edtCurrencyFormat.Text);
    rbFormatClick(Self);
    rbFileClick(Self);
  finally
    Ini.Free;
  end;
end;

procedure TfrmExportDialog.SaveDialogState;
var
  Ini: TIniFile;
  FileName: string;
begin
  FileName := GetStatePath;
  if FileName = '' then
    Exit;

  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteInteger('Export', 'Format', Ord(GetSelectedFormat));
    Ini.WriteInteger('Export', 'Left', Left);
    Ini.WriteInteger('Export', 'Top', Top);
    Ini.WriteInteger('Export', 'Width', Width);
    Ini.WriteInteger('Export', 'Height', Height);
    Ini.WriteInteger('Export', 'ActivePage', GetActivePageIndex);
    Ini.WriteBool('Export', 'DestinationFile', rbFile.Checked);
    Ini.WriteString('Export', 'FileName', edtFileName.Text);
    Ini.WriteBool('Export', 'VisibleOnly', chkVisibleOnly.Checked);
    Ini.WriteBool('Export', 'FilteredOnly', chkFilteredOnly.Checked);
    Ini.WriteBool('Export', 'IncludeHeaders', chkIncludeHeaders.Checked);
    Ini.WriteBool('Export', 'IncludeFooter', chkIncludeFooter.Checked);
    Ini.WriteString('Export', 'DateFormat', edtDateFormat.Text);
    Ini.WriteString('Export', 'TimeFormat', edtTimeFormat.Text);
    Ini.WriteString('Export', 'CurrencyFormat', edtCurrencyFormat.Text);
  finally
    Ini.Free;
  end;
end;

procedure TfrmExportDialog.UpdateFileName;
var
  BaseName: string;
begin
  if Assigned(FGrid) and Assigned(FGrid.Owner) and (FGrid.Owner is TForm) then
    BaseName := TForm(FGrid.Owner).Caption
  else
    BaseName := 'Export';
    
  BaseName := StringReplace(BaseName, ' ', '_', [rfReplaceAll]);
  BaseName := StringReplace(BaseName, '-', '_', [rfReplaceAll]);
  
  edtFileName.Text := TPath.Combine(
    TPath.GetDocumentsPath,
    Format('%s_%s%s', [
      BaseName,
      FormatDateTime('yyyymmdd_hhnnss', Now),
      GetFileExtension
    ])
  );
end;

function TfrmExportDialog.GetFileExtension: string;
begin
  case GetSelectedFormat of
    vefCSV:       Result := '.csv';
    vefTSV:       Result := '.tsv';
    vefExcelXLSX: Result := '.xlsx';
    vefHTML:      Result := '.html';
    vefXML:       Result := '.xml';
    vefJSON:      Result := '.json';
    vefText:      Result := '.txt';
  else
    Result := '.txt';
  end;
end;

function TfrmExportDialog.GetSelectedFormat: TVittixExportFormat;
begin
  if rbCSV.Checked then
    Result := vefCSV
  else if rbTSV.Checked then
    Result := vefTSV
  else if rbExcel.Checked then
    Result := vefExcelXLSX
  else if rbHTML.Checked then
    Result := vefHTML
  else if rbXML.Checked then
    Result := vefXML
  else if rbJSON.Checked then
    Result := vefJSON
  else if rbText.Checked then
    Result := vefText
  else
    Result := vefCSV;
end;

procedure TfrmExportDialog.rbFormatClick(Sender: TObject);
begin
  UpdateFileName;
  
  // Update SaveDialog filter
  case GetSelectedFormat of
    vefCSV:
      SaveDialog1.Filter := 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*';
    vefTSV:
      SaveDialog1.Filter := 'TSV Files (*.tsv)|*.tsv|All Files (*.*)|*.*';
    vefExcelXLSX:
      SaveDialog1.Filter := 'Excel Files (*.xlsx)|*.xlsx|All Files (*.*)|*.*';
    vefHTML:
      SaveDialog1.Filter := 'HTML Files (*.html)|*.html|All Files (*.*)|*.*';
    vefXML:
      SaveDialog1.Filter := 'XML Files (*.xml)|*.xml|All Files (*.*)|*.*';
    vefJSON:
      SaveDialog1.Filter := 'JSON Files (*.json)|*.json|All Files (*.*)|*.*';
    vefText:
      SaveDialog1.Filter := 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
  end;
end;

procedure TfrmExportDialog.rbFileClick(Sender: TObject);
begin
  edtFileName.Enabled := rbFile.Checked;
  btnBrowse.Enabled := rbFile.Checked;
end;

procedure TfrmExportDialog.btnBrowseClick(Sender: TObject);
begin
  SaveDialog1.FileName := edtFileName.Text;
  
  if SaveDialog1.Execute then
    edtFileName.Text := SaveDialog1.FileName;
end;

procedure TfrmExportDialog.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = tabPreview then
    GeneratePreview;
end;

procedure TfrmExportDialog.GeneratePreview;
var
  PreviewData: string;
  Exporter: TVittixDBGridExporter;
begin
  memoPreview.Lines.Clear;
  memoPreview.Lines.Add(GetPreviewText);
  
  Exporter := TVittixDBGridExporter.Create(FGrid);
  try
    // Configure options
    Exporter.Options.ExportVisibleOnly := chkVisibleOnly.Checked;
    Exporter.Options.ExportFilteredOnly := chkFilteredOnly.Checked;
    Exporter.Options.IncludeHeaders := chkIncludeHeaders.Checked;
    Exporter.Options.DateFormat := edtDateFormat.Text;
    Exporter.Options.TimeFormat := edtTimeFormat.Text;
    Exporter.Options.CurrencyFormat := edtCurrencyFormat.Text;

    if Assigned(FGrid.DataSource.DataSet) then
    begin
      FGrid.DataSource.DataSet.DisableControls;
      try
        FGrid.DataSource.DataSet.First;
        
        // Generate preview based on format
        case GetSelectedFormat of
          vefCSV, vefTSV:
            PreviewData := Exporter.ExportToString(GetSelectedFormat);
          vefHTML, vefXML, vefJSON:
            PreviewData := Exporter.ExportToString(GetSelectedFormat);
        else
          PreviewData := 'Preview not available for this format';
        end;
        
      finally
        FGrid.DataSource.DataSet.EnableControls;
      end;
    end;
    
    memoPreview.Lines.Text := PreviewData;
    
    // Truncate if too long
    if memoPreview.Lines.Count > 50 then
    begin
      while memoPreview.Lines.Count > 50 do
        memoPreview.Lines.Delete(memoPreview.Lines.Count - 1);
      memoPreview.Lines.Add('...');
      memoPreview.Lines.Add('(Preview truncated - showing first 50 lines)');
    end;
    
  finally
    Exporter.Free;
  end;
end;

function TfrmExportDialog.GetPreviewText: string;
begin
  Result := 'Generating preview...';
end;

function TfrmExportDialog.GetActivePageIndex: Integer;
begin
  Result := PageControl1.ActivePageIndex;
end;

procedure TfrmExportDialog.SetActivePageIndex(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value > PageControl1.PageCount - 1 then
    Value := PageControl1.PageCount - 1;
  PageControl1.ActivePageIndex := Value;
end;

procedure TfrmExportDialog.btnExportClick(Sender: TObject);
var
  ExportFormat: TVittixExportFormat;
begin
  // Validate
  if rbFile.Checked and (Trim(edtFileName.Text) = '') then
  begin
    MessageDlg('Please specify a file name', mtError, [mbOK], 0);
    edtFileName.SetFocus;
    Exit;
  end;
  
  ExportFormat := GetSelectedFormat;
  
  // Create exporter
  FExporter := TVittixDBGridExporter.Create(FGrid);
  try
    // Configure options
    FExporter.Options.ExportVisibleOnly := chkVisibleOnly.Checked;
    FExporter.Options.ExportFilteredOnly := chkFilteredOnly.Checked;
    FExporter.Options.IncludeHeaders := chkIncludeHeaders.Checked;
    FExporter.Options.IncludeFooter := chkIncludeFooter.Checked;
    FExporter.Options.DateFormat := edtDateFormat.Text;
    FExporter.Options.TimeFormat := edtTimeFormat.Text;
    FExporter.Options.CurrencyFormat := edtCurrencyFormat.Text;
    FExporter.OnProgress := ExportProgress;
    
    // Show progress
    ProgressBar1.Visible := True;
    lblProgress.Visible := True;
    btnExport.Enabled := False;
    btnCancel.Caption := 'Cancel';
    
    try
      // Export
      if rbFile.Checked then
      begin
        FExporter.ExportToFile(edtFileName.Text, ExportFormat);
        MessageDlg(
          Format('Export completed successfully!%s%sFile: %s', [
            sLineBreak, sLineBreak, edtFileName.Text
          ]),
          mtInformation,
          [mbOK],
          0
        );
      end
      else // Clipboard
      begin
        FExporter.ExportToClipboard(ExportFormat);
        MessageDlg(
          'Data exported to clipboard successfully!',
          mtInformation,
          [mbOK],
          0
        );
      end;
      
      SaveDialogState;
      ModalResult := mrOk;
      
    except
      on E: Exception do
      begin
        MessageDlg(
          'Export failed: ' + E.Message,
          mtError,
          [mbOK],
          0
        );
      end;
    end;
    
  finally
    ProgressBar1.Visible := False;
    lblProgress.Visible := False;
    btnExport.Enabled := True;
    btnCancel.Caption := 'Close';
    FExporter.Free;
    FExporter := nil;
  end;
end;

procedure TfrmExportDialog.btnCancelClick(Sender: TObject);
begin
  if Assigned(FExporter) then
  begin
    if MessageDlg('Cancel export?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FExporter.Cancel;
      ModalResult := mrCancel;
    end;
  end
  else
    ModalResult := mrCancel;
end;

procedure TfrmExportDialog.ExportProgress(Sender: TObject; Current, Total: Integer;
  var Cancel: Boolean);
begin
  if Total > 0 then
  begin
    ProgressBar1.Max := Total;
    ProgressBar1.Position := Current;
    lblProgress.Caption := Format('Exporting... %d of %d records', [Current, Total]);
  end;
  
  Application.ProcessMessages;
end;

initialization
  TfrmExportDialog.StateFileName := '';
  TfrmExportDialog.RootPath := '';

end.
