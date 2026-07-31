unit VittixDBGridForm;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Math,
  System.IOUtils,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.DBGrids,
  Vcl.Menus,
  Vcl.ComCtrls,
  Data.DB,
  Datasnap.DBClient,

  // Vittix DBGrid
  Vittix.DBGrid,
  Vittix.DBGrid.Controller,
  Vittix.DBGrid.ColumnInfo, // This unit is already included by Vittix.DBGrid.Controller, but explicit is fine.
  Vittix.DBGrid.Export.Dialog, // Add this unit
  Vittix.DBGrid.Editors;

type
  TfrmVittixDemo = class(TForm)
    // Main Grid
    VittixGrid: TVittixDBGrid;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;

    // Panels
    pnlTop: TPanel;
    pnlToolbar: TPanel;
    pnlStatus: TPanel;

    // Top Panel
    lblTitle: TLabel;
    lblSubtitle: TLabel;

    // Toolbar Controls
    grpDataOperations: TGroupBox;
    btnAddRecord: TButton;
    btnEditRecord: TButton;
    btnDeleteRecord: TButton;
    btnRefreshData: TButton;

    grpFiltering: TGroupBox;
    lblGlobalSearch: TLabel;
    edtGlobalSearch: TEdit;
    btnClearFilters: TButton;
    chkShowFiltered: TCheckBox;

    grpDisplay: TGroupBox;
    chkAlternateRows: TCheckBox;
    chkShowFooter: TCheckBox;
    btnColumnChooser: TButton;

    grpExport: TGroupBox;
    btnExportDialog: TButton; // Renamed from btnExportCSV
    btnSaveConfig: TButton;
    btnLoadConfig: TButton;

    // Status Bar
    lblRecordCount: TLabel;
    lblFilterStatus: TLabel;
    lblSortStatus: TLabel;

    // Dataset Fields
    ClientDataSet1ID: TIntegerField;
    ClientDataSet1CompanyName: TStringField;
    ClientDataSet1ContactName: TStringField;
    ClientDataSet1Email: TStringField;
    ClientDataSet1Phone: TStringField;
    ClientDataSet1Country: TStringField;
    ClientDataSet1City: TStringField;
    ClientDataSet1OrderDate: TDateTimeField;
    ClientDataSet1TotalAmount: TCurrencyField;
    ClientDataSet1Quantity: TIntegerField;
    ClientDataSet1Status: TStringField;
    ClientDataSet1Notes: TMemoField;
    ClientDataSet1Discount: TFloatField;
    ClientDataSet1ShippingDate: TDateTimeField;
    ClientDataSet1PaymentMethod: TStringField;

    // Popup Menus
    PopupMenu1: TPopupMenu;
    mnuEditRecord: TMenuItem;
    mnuDeleteRecord: TMenuItem;
    N1: TMenuItem;
    mnuCopyCell: TMenuItem;
    mnuCopyRow: TMenuItem;

    // Save/Load Dialogs
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;

    // Timer for search delay
    tmrSearchDelay: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    // Data Operations
    procedure btnAddRecordClick(Sender: TObject);
    procedure btnEditRecordClick(Sender: TObject);
    procedure btnDeleteRecordClick(Sender: TObject);
    procedure btnRefreshDataClick(Sender: TObject);

    // Filtering
    procedure edtGlobalSearchChange(Sender: TObject);
    procedure tmrSearchDelayTimer(Sender: TObject);
    procedure btnClearFiltersClick(Sender: TObject);
    procedure chkShowFilteredClick(Sender: TObject);

    // Display
    procedure chkAlternateRowsClick(Sender: TObject);
    procedure chkShowFooterClick(Sender: TObject);
    procedure btnColumnChooserClick(Sender: TObject);

    // Export & Config
    procedure btnExportDialogClick(Sender: TObject); // Renamed from btnExportCSVClick
    procedure btnSaveConfigClick(Sender: TObject);
    procedure btnLoadConfigClick(Sender: TObject);

    // Grid Events
    procedure VittixGridTitleClick(Column: TColumn);
    procedure VittixGridDblClick(Sender: TObject);

    // Popup Menu
    procedure mnuEditRecordClick(Sender: TObject);
    procedure mnuDeleteRecordClick(Sender: TObject);
    procedure mnuCopyCellClick(Sender: TObject);
    procedure mnuCopyRowClick(Sender: TObject);

    // Dataset Events
    procedure ClientDataSet1AfterPost(DataSet: TDataSet);
    procedure ClientDataSet1AfterDelete(DataSet: TDataSet);
    procedure ClientDataSet1AfterScroll(DataSet: TDataSet);

  private
    function GetDemoStatePath: string;
    function GetGridController: TVittixDBGridController;
    procedure InitializeDataset;
    procedure LoadSampleData;
    procedure UpdateStatusBar;
    procedure ConfigurePersistence;
    procedure SyncDisplayOptions;
    procedure LoadSavedLayout;
    procedure SaveColumnConfiguration(const FileName: string);
    procedure LoadColumnConfiguration(const FileName: string);
    procedure SetupAggregations;
  public
    { Public declarations }
  end;

var
  frmVittixDemo: TfrmVittixDemo;

implementation

{$R *.dfm}

uses
  Clipbrd;

{ TfrmVittixDemo }

procedure TfrmVittixDemo.FormCreate(Sender: TObject);
begin
  Caption := 'Vittix DBGrid - Complete Feature Demo';

  ConfigurePersistence;

  // Initialize dataset
  InitializeDataset;
  LoadSampleData;

  // Setup aggregations
  SetupAggregations;

  // Update status
  UpdateStatusBar;

  // Configure search delay
  tmrSearchDelay.Interval := 500; // 500ms delay
  tmrSearchDelay.Enabled := False;

  ShowMessage(
    'Welcome to Vittix DBGrid Feature Demo!' + sLineBreak + sLineBreak +
    'Features to try:' + sLineBreak +
    '- Click column headers to sort, or Ctrl+Click for multi-column sorting' + sLineBreak +
    '- Right-click a column title to open the filter popup; Enter applies and Esc cancels' + sLineBreak +
    '- Filter popup supports Not Between and history clearing with Ctrl+Shift+H' + sLineBreak +
    '- Use Global Search to search across all columns' + sLineBreak +
    '- Column Chooser supports Ctrl+F search, Ctrl+A/N selection, Ctrl+Up/Down reorder, Ctrl+R reset, and Esc clear' + sLineBreak +
    '- Footer shortcuts support Del, Ctrl+Del, Ctrl+C, Ctrl+Shift+C, and Ctrl+Shift+F' + sLineBreak +
    '- Export Data supports CSV, TSV, Excel, HTML, XML, JSON, clipboard, and text' + sLineBreak +
    '- Demo state is stored in features-demo-state next to the executable' + sLineBreak +
    '- Use Save Layout and Load Layout to persist or restore the current grid state'
  );
end;

procedure TfrmVittixDemo.FormDestroy(Sender: TObject);
begin
  // Cleanup handled automatically
end;

function TfrmVittixDemo.GetDemoStatePath: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'features-demo-state');
  try
    ForceDirectories(Result);
  except
    Result := TPath.Combine(TPath.GetTempPath, 'VittixDBGridDemo');
    ForceDirectories(Result);
  end;
end;

function TfrmVittixDemo.GetGridController: TVittixDBGridController;
begin
  Result := nil;
  if Assigned(VittixGrid.Controller) and (VittixGrid.Controller is TVittixDBGridController) then
    Result := TVittixDBGridController(VittixGrid.Controller);
end;

procedure TfrmVittixDemo.ConfigurePersistence;
var
  StatePath: string;
begin
  StatePath := GetDemoStatePath;

  VittixGrid.PersistenceRootPath := StatePath;
  VittixGrid.LayoutStorageFileName := TPath.Combine(StatePath, 'layout.json');
  VittixGrid.ChooserStateFileName := TPath.Combine(StatePath, 'chooser.ini');
  VittixGrid.FilterHistoryFileName := TPath.Combine(StatePath, 'filter.ini');

  TfrmExportDialog.RootPath := StatePath;
  TfrmExportDialog.StateFileName := TPath.Combine(StatePath, 'export.ini');
end;

procedure TfrmVittixDemo.SyncDisplayOptions;
begin
  chkAlternateRows.Checked := VittixGrid.AlternatingRowColors;
  chkShowFooter.Checked := VittixGrid.FooterVisible;
  chkShowFiltered.Checked := ClientDataSet1.Filtered;
end;

procedure TfrmVittixDemo.LoadSavedLayout;
var
  Controller: TVittixDBGridController;
begin
  Controller := GetGridController;
  if Assigned(Controller) and FileExists(VittixGrid.LayoutStorageFileName) then
  begin
    try
      Controller.LoadLayoutFromFile;
    except
      // Ignore corrupted layout files in the demo.
    end;
  end;

  SyncDisplayOptions;
end;

procedure TfrmVittixDemo.InitializeDataset;
begin
  ClientDataSet1.Close;

  if ClientDataSet1.FieldCount = 0 then
  begin
    ClientDataSet1.FieldDefs.Clear;
    ClientDataSet1.FieldDefs.Add('ID', ftInteger);
    ClientDataSet1.FieldDefs.Add('CompanyName', ftString, 100);
    ClientDataSet1.FieldDefs.Add('ContactName', ftString, 100);
    ClientDataSet1.FieldDefs.Add('Email', ftString, 100);
    ClientDataSet1.FieldDefs.Add('Phone', ftString, 20);
    ClientDataSet1.FieldDefs.Add('Country', ftString, 50);
    ClientDataSet1.FieldDefs.Add('City', ftString, 50);
    ClientDataSet1.FieldDefs.Add('OrderDate', ftDateTime);
    ClientDataSet1.FieldDefs.Add('TotalAmount', ftCurrency);
    ClientDataSet1.FieldDefs.Add('Quantity', ftInteger);
    ClientDataSet1.FieldDefs.Add('Status', ftString, 20);
    ClientDataSet1.FieldDefs.Add('Notes', ftMemo);
    ClientDataSet1.FieldDefs.Add('Discount', ftFloat);
    ClientDataSet1.FieldDefs.Add('ShippingDate', ftDateTime);
    ClientDataSet1.FieldDefs.Add('PaymentMethod', ftString, 20);
  end;

  ClientDataSet1.CreateDataSet;
  ClientDataSet1.LogChanges := False;
end;

procedure TfrmVittixDemo.LoadSampleData;
const
  Companies: array[0..9] of string = (
    'Acme Corp', 'TechnoSoft', 'Global Industries', 'Innovative Solutions',
    'Premier Systems', 'Digital Dynamics', 'Enterprise Group', 'Summit Technologies',
    'Apex Corporation', 'Quantum Systems'
  );

  Contacts: array[0..9] of string = (
    'John Smith', 'Jane Doe', 'Robert Johnson', 'Mary Williams',
    'Michael Brown', 'Sarah Davis', 'David Wilson', 'Emily Taylor',
    'James Anderson', 'Lisa Martinez'
  );

  Countries: array[0..4] of string = (
    'USA', 'UK', 'Germany', 'France', 'Japan'
  );

  Cities: array[0..4] of string = (
    'New York', 'London', 'Berlin', 'Paris', 'Tokyo'
  );

  Statuses: array[0..3] of string = (
    'Active', 'Pending', 'Completed', 'Cancelled'
  );

  PaymentMethods: array[0..3] of string = (
    'Credit Card', 'Bank Transfer', 'PayPal', 'Cash'
  );

var
  I: Integer;
begin
  ClientDataSet1.DisableControls;
  try
    for I := 1 to 50 do
    begin
      ClientDataSet1.Append;

      ClientDataSet1.FieldByName('ID').AsInteger := I;
      ClientDataSet1.FieldByName('CompanyName').AsString :=
        Companies[Random(Length(Companies))];
      ClientDataSet1.FieldByName('ContactName').AsString :=
        Contacts[Random(Length(Contacts))];
      ClientDataSet1.FieldByName('Email').AsString :=
        Format('contact%d@example.com', [I]);
      ClientDataSet1.FieldByName('Phone').AsString :=
        Format('+1-555-%04d', [Random(10000)]);
      ClientDataSet1.FieldByName('Country').AsString :=
        Countries[Random(Length(Countries))];
      ClientDataSet1.FieldByName('City').AsString :=
        Cities[Random(Length(Cities))];
      ClientDataSet1.FieldByName('OrderDate').AsDateTime :=
        Now - Random(365);
      ClientDataSet1.FieldByName('TotalAmount').AsCurrency :=
        100 + Random(10000) + (Random(100) / 100);
      ClientDataSet1.FieldByName('Quantity').AsInteger :=
        1 + Random(100);
      ClientDataSet1.FieldByName('Status').AsString :=
        Statuses[Random(Length(Statuses))];
      ClientDataSet1.FieldByName('Notes').AsString :=
        Format('Order notes for record %d. This is sample data for demonstration.', [I]);
      ClientDataSet1.FieldByName('Discount').AsFloat :=
        Random(30) / 100; // 0-30% discount
      ClientDataSet1.FieldByName('ShippingDate').AsDateTime :=
        Now + Random(30);
      ClientDataSet1.FieldByName('PaymentMethod').AsString :=
        PaymentMethods[Random(Length(PaymentMethods))];

      ClientDataSet1.Post;
    end;
  finally
    ClientDataSet1.EnableControls;
  end;

  ClientDataSet1.First;
end;

procedure TfrmVittixDemo.SetupAggregations;
begin
  if Assigned(VittixGrid.Controller) and (VittixGrid.Controller is TVittixDBGridController) then
  begin
    TVittixDBGridController(VittixGrid.Controller).SetColumnAggregation(
      VittixGrid.Columns.Items[8], // TotalAmount
      vatSum
    );

    TVittixDBGridController(VittixGrid.Controller).SetColumnAggregation(
      VittixGrid.Columns.Items[9], // Quantity
      vatSum
    );

    TVittixDBGridController(VittixGrid.Controller).SetColumnAggregation(
      VittixGrid.Columns.Items[0], // ID
      vatCount
    );
  end;
end;

procedure TfrmVittixDemo.UpdateStatusBar;
var
  SortText: string;
  I: Integer;
begin
  lblRecordCount.Caption := Format('Records: %d', [ClientDataSet1.RecordCount]);

  if ClientDataSet1.Filtered then
    lblFilterStatus.Caption := 'Filter: Active'
  else
    lblFilterStatus.Caption := 'Filter: None';

  SortText := '';
  for I := 0 to VittixGrid.ColumnInfo.Count - 1 do
  begin
    if VittixGrid.ColumnInfo[I].SortOrder <> vsoNone then
    begin
      if SortText <> '' then
        SortText := SortText + ', ';
      SortText := SortText + VittixGrid.ColumnInfo[I].FieldName;
      if VittixGrid.ColumnInfo[I].SortOrder = vsoDesc then
        SortText := SortText + ' ↓'
      else
        SortText := SortText + ' ↑';
    end;
  end;

  if SortText <> '' then
    lblSortStatus.Caption := 'Sort: ' + SortText
  else
    lblSortStatus.Caption := 'Sort: None';
end;

{ Data Operations }

procedure TfrmVittixDemo.btnAddRecordClick(Sender: TObject);
var
  NewID: Integer;
begin
  NewID := 1;
  ClientDataSet1.First;
  while not ClientDataSet1.Eof do
  begin
    if ClientDataSet1.FieldByName('ID').AsInteger >= NewID then
      NewID := ClientDataSet1.FieldByName('ID').AsInteger + 1;
    ClientDataSet1.Next;
  end;

  ClientDataSet1.Append;
  ClientDataSet1.FieldByName('ID').AsInteger := NewID;
  ClientDataSet1.FieldByName('CompanyName').AsString := 'New Company';
  ClientDataSet1.FieldByName('ContactName').AsString := 'New Contact';
  ClientDataSet1.FieldByName('Email').AsString := 'new@example.com';
  ClientDataSet1.FieldByName('Phone').AsString := '+1-555-0000';
  ClientDataSet1.FieldByName('Country').AsString := 'USA';
  ClientDataSet1.FieldByName('City').AsString := 'New York';
  ClientDataSet1.FieldByName('OrderDate').AsDateTime := Now;
  ClientDataSet1.FieldByName('TotalAmount').AsCurrency := 0;
  ClientDataSet1.FieldByName('Quantity').AsInteger := 0;
  ClientDataSet1.FieldByName('Status').AsString := 'Pending';
  ClientDataSet1.FieldByName('Discount').AsFloat := 0;
  ClientDataSet1.FieldByName('ShippingDate').AsDateTime := Now + 7;
  ClientDataSet1.FieldByName('PaymentMethod').AsString := 'Credit Card';
  ClientDataSet1.Post;

  ShowMessage('New record added. You can now edit it in the grid.');
end;

procedure TfrmVittixDemo.btnEditRecordClick(Sender: TObject);
begin
  if ClientDataSet1.IsEmpty then
  begin
    ShowMessage('No record to edit');
    Exit;
  end;

  if MessageDlg('Edit Notes field?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    TVittixDBGridEditors.EditField(VittixGrid, VittixGrid.Columns.Items[11]);
  end;
end;

procedure TfrmVittixDemo.btnDeleteRecordClick(Sender: TObject);
begin
  if ClientDataSet1.IsEmpty then
  begin
    ShowMessage('No record to delete');
    Exit;
  end;

  if MessageDlg(
    Format('Delete record ID %d?', [ClientDataSet1.FieldByName('ID').AsInteger]),
    mtConfirmation,
    [mbYes, mbNo],
    0
  ) = mrYes then
  begin
    ClientDataSet1.Delete;
  end;
end;

procedure TfrmVittixDemo.btnRefreshDataClick(Sender: TObject);
var
  Controller: TVittixDBGridController;
begin
  Controller := GetGridController;
  if Assigned(Controller) then
    Controller.Refresh;

  UpdateStatusBar;
  ShowMessage('Grid refreshed');
end;

{ Filtering }

procedure TfrmVittixDemo.edtGlobalSearchChange(Sender: TObject);
begin
  tmrSearchDelay.Enabled := False;
  tmrSearchDelay.Enabled := True;
end;

procedure TfrmVittixDemo.tmrSearchDelayTimer(Sender: TObject);
begin
  tmrSearchDelay.Enabled := False;

  if Assigned(VittixGrid.Controller) and (VittixGrid.Controller is TVittixDBGridController) then
  begin
    TVittixDBGridController(VittixGrid.Controller).SetGlobalFilter(edtGlobalSearch.Text);
    UpdateStatusBar;
  end;
end;

procedure TfrmVittixDemo.btnClearFiltersClick(Sender: TObject);
var
  Controller: TVittixDBGridController;
begin
  edtGlobalSearch.Clear;
  Controller := GetGridController;
  if Assigned(Controller) then
    Controller.ClearFilters;
  chkShowFiltered.Checked := False;
  UpdateStatusBar;
  ShowMessage('All filters cleared');
end;

procedure TfrmVittixDemo.chkShowFilteredClick(Sender: TObject);
begin
  ClientDataSet1.Filtered := chkShowFiltered.Checked;
  UpdateStatusBar;
end;

{ Display }

procedure TfrmVittixDemo.chkAlternateRowsClick(Sender: TObject);
begin
  VittixGrid.AlternatingRowColors := chkAlternateRows.Checked;
  VittixGrid.Invalidate;
end;

procedure TfrmVittixDemo.chkShowFooterClick(Sender: TObject);
begin
  VittixGrid.FooterVisible := chkShowFooter.Checked;
end;

procedure TfrmVittixDemo.btnColumnChooserClick(Sender: TObject);
var
  Controller: TVittixDBGridController;
begin
  Controller := GetGridController;
  if Assigned(Controller) then
    Controller.ShowColumnChooser;
end;

{ Export & Config }

procedure TfrmVittixDemo.btnExportDialogClick(Sender: TObject);
begin
  // Show the professional export dialog
  if TVittixExportDialog.Execute(VittixGrid) then
  begin
    // The dialog handles messages and shows its own success/failure messages
    // You might add a custom message here if the dialog doesn't provide one
    // ShowMessage('Export operation completed via dialog!');
  end;
end;

procedure TfrmVittixDemo.btnSaveConfigClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt := 'json';
  SaveDialog1.FileName := 'grid_layout.json';
  SaveDialog1.InitialDir := GetDemoStatePath;
  SaveDialog1.Filter := 'Layout Files (*.json)|*.json|All Files (*.*)|*.*';

  if SaveDialog1.Execute then
  begin
    SaveColumnConfiguration(SaveDialog1.FileName);
    ShowMessage('Layout saved to: ' + SaveDialog1.FileName);
  end;
end;

procedure TfrmVittixDemo.SaveColumnConfiguration(const FileName: string);
var
  Controller: TVittixDBGridController;
begin
  Controller := GetGridController;
  if Assigned(Controller) then
  begin
    if FileName <> '' then
      Controller.SaveLayoutToFile(FileName)
    else
      Controller.SaveLayoutToFile;
  end;
end;

procedure TfrmVittixDemo.btnLoadConfigClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Layout Files (*.json)|*.json|All Files (*.*)|*.*';
  OpenDialog1.DefaultExt := 'json';
  OpenDialog1.FileName := 'grid_layout.json';
  OpenDialog1.InitialDir := GetDemoStatePath;

  if OpenDialog1.Execute then
  begin
    LoadColumnConfiguration(OpenDialog1.FileName);
    ShowMessage('Layout loaded from: ' + OpenDialog1.FileName);
  end;
end;

procedure TfrmVittixDemo.LoadColumnConfiguration(const FileName: string);
var
  Controller: TVittixDBGridController;
begin
  Controller := GetGridController;
  if Assigned(Controller) then
  begin
    if FileName <> '' then
      Controller.LoadLayoutFromFile(FileName)
    else
      Controller.LoadLayoutFromFile;
  end;

  SyncDisplayOptions;
  UpdateStatusBar;
end;

{ Grid Events }

procedure TfrmVittixDemo.VittixGridTitleClick(Column: TColumn);
begin
  UpdateStatusBar;
end;

procedure TfrmVittixDemo.VittixGridDblClick(Sender: TObject);
begin
  btnEditRecordClick(Sender);
end;

{ Popup Menu }

procedure TfrmVittixDemo.mnuEditRecordClick(Sender: TObject);
begin
  btnEditRecordClick(Sender);
end;

procedure TfrmVittixDemo.mnuDeleteRecordClick(Sender: TObject);
begin
  btnDeleteRecordClick(Sender);
end;

procedure TfrmVittixDemo.mnuCopyCellClick(Sender: TObject);
begin
  if Assigned(VittixGrid.SelectedField) then
  begin
    Clipboard.AsText := VittixGrid.SelectedField.DisplayText;
    ShowMessage('Cell value copied to clipboard');
  end;
end;

procedure TfrmVittixDemo.mnuCopyRowClick(Sender: TObject);
var
  I: Integer;
  RowText: string;
begin
  RowText := '';
  for I := 0 to VittixGrid.Columns.Count - 1 do
  begin
    if VittixGrid.Columns[I].Visible then
    begin
      if RowText <> '' then
        RowText := RowText + #9;
      RowText := RowText + VittixGrid.Columns[I].Field.DisplayText;
    end;
  end;

  Clipboard.AsText := RowText;
  ShowMessage('Row data copied to clipboard');
end;

{ Dataset Events }

procedure TfrmVittixDemo.ClientDataSet1AfterPost(DataSet: TDataSet);
begin
  UpdateStatusBar;
end;

procedure TfrmVittixDemo.ClientDataSet1AfterDelete(DataSet: TDataSet);
begin
  UpdateStatusBar;
end;

procedure TfrmVittixDemo.ClientDataSet1AfterScroll(DataSet: TDataSet);
begin
  UpdateStatusBar;
end;

end.
