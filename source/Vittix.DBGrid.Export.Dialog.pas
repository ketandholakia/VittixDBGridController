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
    grpDestination: TGroupBox;
    rbFile: TRadioButton;
    rbClipboard: TRadioButton;
    edtFileName: TEdit;
    btnBrowse: TButton;
    grpScope: TGroupBox;
    chkVisibleOnly: TCheckBox;
    chkFilteredOnly: TCheckBox;
    chkIncludeHeaders: TCheckBox;
    chkIncludeFooter: TCheckBox;
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
    
    procedure LoadDefaults;
    procedure UpdateFileName;
    procedure GeneratePreview;
    procedure ExportProgress(Sender: TObject; Current, Total: Integer; var Cancel: Boolean);
    function GetSelectedFormat: TVittixExportFormat;
    function GetFileExtension: string;
    
  public
    class function Execute(AGrid: TVittixDBGrid): Boolean;
  end;

  TVittixExportDialog = TfrmExportDialog;

var
  frmExportDialog: TfrmExportDialog;

implementation

{$R *.dfm}

uses
  System.IOUtils;

{ TfrmExportDialog }

class function TfrmExportDialog.Execute(AGrid: TVittixDBGrid): Boolean;
var
  Dlg: TfrmExportDialog;
begin
  Result := False;
  
  if not Assigned(AGrid) then
    raise Exception.Create('Grid is not assigned');
    
  if not Assigned(AGrid.DataSource) or not Assigned(AGrid.DataSource.DataSet) then
    raise Exception.Create('Grid does not have a valid DataSource');
    
  if not AGrid.DataSource.DataSet.Active then
    raise Exception.Create('Dataset is not active');
    
  Dlg := TfrmExportDialog.Create(nil);
  try
    Dlg.FGrid := AGrid;
    Dlg.LoadDefaults;
    Result := Dlg.ShowModal = mrOk;
  finally
    Dlg.Free;
  end;
end;

procedure TfrmExportDialog.FormCreate(Sender: TObject);
begin
  // Ensure chkIncludeFooter exists (handle potential DFM mismatch)
  if (chkIncludeFooter = nil) and (chkIncludeHeaders <> nil) and (grpScope <> nil) then
  begin
    chkIncludeFooter := TCheckBox.Create(Self);
    chkIncludeFooter.Parent := grpScope;
    chkIncludeFooter.Name := 'chkIncludeFooter';
    chkIncludeFooter.Caption := 'Include Footer';
    chkIncludeFooter.Left := chkIncludeHeaders.Left;
    chkIncludeFooter.Top := chkIncludeHeaders.Top + chkIncludeHeaders.Height + 6;
  end;

  PageControl1.ActivePageIndex := 0;
  ProgressBar1.Visible := False;
  lblProgress.Visible := False;
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
  SavedRecordCount: Integer;
begin
  memoPreview.Lines.Clear;
  memoPreview.Lines.Add('Generating preview...');
  
  Exporter := TVittixDBGridExporter.Create(FGrid);
  try
    // Configure options
    Exporter.Options.ExportVisibleOnly := chkVisibleOnly.Checked;
    Exporter.Options.ExportFilteredOnly := chkFilteredOnly.Checked;
    Exporter.Options.IncludeHeaders := chkIncludeHeaders.Checked;
    Exporter.Options.DateFormat := edtDateFormat.Text;
    Exporter.Options.TimeFormat := edtTimeFormat.Text;
    Exporter.Options.CurrencyFormat := edtCurrencyFormat.Text;
    
    // Limit preview to first 10 rows
    SavedRecordCount := 0;
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

end.
