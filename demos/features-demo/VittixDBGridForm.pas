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
  Vcl.Themes,
  Data.DB,
  Datasnap.DBClient,

  // Vittix DBGrid
  Vittix.DBGrid,
  Vittix.DBGrid.Controller,
  Vittix.DBGrid.ColumnInfo, // This unit is already included by Vittix.DBGrid.Controller, but explicit is fine.
  Vittix.DBGrid.Export.Dialog, // Add this unit
  Vittix.DBGrid.Export.Engine,
  Vittix.DBGrid.Editors,
  Vittix.DBGrid.FilterSets.Dialog,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.Phys.SQLite,
  FireDAC.VCLUI.Wait,
  Vittix.DBGrid.Provider,
  Vittix.DBGrid.Provider.FireDAC;

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
    btnSaveFilterSet: TButton;
    btnManageFilterSets: TButton;

    grpDisplay: TGroupBox;
    chkAlternateRows: TCheckBox;
    chkShowFooter: TCheckBox;
    btnColumnChooser: TButton;

    grpExport: TGroupBox;
    btnExportDialog: TButton; // Renamed from btnExportCSV
    btnExportXlsx: TButton;
    btnPrintPreview: TButton;
    btnSaveConfig: TButton;
    btnLoadConfig: TButton;

    // VCL Style switcher
    grpStyles: TGroupBox;
    lblStyle: TLabel;
    cmbStyle: TComboBox;

    // Server-side filter/sort demo
    grpServer: TGroupBox;
    lblFilterMode: TLabel;
    cmbFilterMode: TComboBox;
    lblServerInfo: TLabel;

    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;

    // Status Bar
    lblRecordCount: TLabel;
    lblFilterStatus: TLabel;
    lblSortStatus: TLabel;
    lblSearchStatus: TLabel;

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
    procedure btnSaveFilterSetClick(Sender: TObject);
    procedure btnManageFilterSetsClick(Sender: TObject);
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
    procedure btnExportXlsxClick(Sender: TObject);
    procedure btnPrintPreviewClick(Sender: TObject);
    procedure btnSaveConfigClick(Sender: TObject);
    procedure btnLoadConfigClick(Sender: TObject);

    // VCL Style switcher
    procedure cmbStyleChange(Sender: TObject);

    // Server-side filter/sort
    procedure cmbFilterModeChange(Sender: TObject);
    procedure CreateServerTable(ARowCount: Integer);
    procedure UpdateServerInfo(const AExtra: string);

    // Grid Events
    procedure VittixGridTitleClick(Column: TColumn);
    procedure VittixGridDblClick(Sender: TObject);
    procedure GridIncrementalSearch(Sender: TObject; const SearchText: string;
      Found: Boolean);

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
    procedure InitStyleCombo;
    procedure SelectActiveStyle;
    procedure LoadStyleFilesFromFolder;
    procedure CreateStatusLookup;
    procedure ConfigureColumnEditors;
    procedure SetupServerDemo;
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

  // Create the StatusId/StatusName lookup fields while the dataset is still
  // closed; adding fields to an open dataset raises "Cannot perform this
  // operation on an open dataset".
  CreateStatusLookup;

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

  // Incremental (type-ahead) search status: type into the grid and the
  // buffer shows here and in the footer hint.
  VittixGrid.OnIncrementalSearch := GridIncrementalSearch;

  // Style switcher: styles embedded via the project resources (or falling
  // back to a styles folder next to the exe) show up in the combo.
  InitStyleCombo;

  // Per-column editors and saved filter sets
  ConfigureColumnEditors;
  VittixGrid.Controller.FilterSetsFileName :=
    TPath.Combine(GetDemoStatePath, 'filtersets.json');

  // Server-side provider demo: a separate SQLite grid panel is skipped;
  // instead the main grid can be switched to a 100k-row SQLite table.
  SetupServerDemo;

  ShowMessage(
    'Welcome to Vittix DBGrid Feature Demo!' + sLineBreak + sLineBreak +
    'Features to try:' + sLineBreak +
    '- Click column headers to sort, or Ctrl+Click for multi-column sorting' + sLineBreak +
    '- Type inside the grid for incremental search (Esc clears, Backspace undoes); the buffer shows in the status bar and footer' + sLineBreak +
    '- Ctrl+C copies the current cell; select rows (Ctrl+Click) and Ctrl+C copies them as TSV, Ctrl+Shift+C with headers - the footer shows selection aggregates' + sLineBreak +
    '- Right-click a column title to open the filter popup; Enter applies and Esc cancels' + sLineBreak +
    '- Filter popup supports Not Between and history clearing with Ctrl+Shift+H' + sLineBreak +
    '- Use Global Search to search across all columns' + sLineBreak +
    '- Column Chooser supports Ctrl+F search, Ctrl+A/N selection, Ctrl+Up/Down reorder, Ctrl+R reset, and Esc clear' + sLineBreak +
    '- Footer shortcuts support Del, Ctrl+Del, Ctrl+C, Ctrl+Shift+C, and Ctrl+Shift+F' + sLineBreak +
    '- Export Data supports CSV, TSV, Excel, HTML, XML, JSON, clipboard, and text; XLSX File writes typed numeric/date cells directly' + sLineBreak +
    '- Switch the VCL style in the combo to see the footer, filter popup and export dialog follow the theme' + sLineBreak +
    '- F2 / double click on Status opens a pick list, Total Amount a masked numeric, Status Name a lookup' + sLineBreak +
    '- Save the active filters with Save Filter Set, re-apply or manage them from the filter popup''s Saved menu' + sLineBreak +
    '- Switch the data mode combo to Server/Auto to push filters and sorting into an in-memory SQLite table (100k rows)' + sLineBreak +
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
  // TVittixDBGrid.Controller is strongly typed; nil only during teardown.
  Result := VittixGrid.Controller;
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
      // Every 5th record leaves Notes and ShippingDate NULL so the XLSX
      // export demonstrates empty cells next to typed numbers and dates.
      if (I mod 5) <> 0 then
      begin
        ClientDataSet1.FieldByName('Notes').AsString :=
          Format('Order notes for record %d. This is sample data for demonstration.', [I]);
        ClientDataSet1.FieldByName('ShippingDate').AsDateTime :=
          Now + Random(30);
      end;
      ClientDataSet1.FieldByName('Discount').AsFloat :=
        Random(30) / 100; // 0-30% discount
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
  if Assigned(VittixGrid.Controller) then
  begin
    VittixGrid.Controller.SetColumnAggregation(
      VittixGrid.Columns.Items[8], // TotalAmount
      vatSum
    );

    VittixGrid.Controller.SetColumnAggregation(
      VittixGrid.Columns.Items[9], // Quantity
      vatSum
    );

    VittixGrid.Controller.SetColumnAggregation(
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

  if Assigned(VittixGrid.Controller) then
  begin
    VittixGrid.Controller.SetGlobalFilter(edtGlobalSearch.Text);
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

procedure TfrmVittixDemo.btnExportXlsxClick(Sender: TObject);
var
  Exporter: TVittixDBGridExporter;
  FileName: string;
begin
  // One-click XLSX: typed numeric/boolean cells, Excel serial dates with
  // date styles, and NULL cells (every 5th row's Notes/Shipping Date).
  FileName := TPath.Combine(GetDemoStatePath,
    Format('demo_export_%s.xlsx', [FormatDateTime('yyyymmdd_hhnnss', Now)]));

  Exporter := TVittixDBGridExporter.Create(VittixGrid);
  try
    Exporter.ExportToExcel(FileName);
  finally
    Exporter.Free;
  end;

  ShowMessage('XLSX written to:' + sLineBreak + FileName + sLineBreak + sLineBreak +
    'Numbers, dates and booleans are typed cells; every 5th row carries NULL cells.');
end;

procedure TfrmVittixDemo.btnPrintPreviewClick(Sender: TObject);
var
  Exporter: TVittixDBGridExporter;
begin
  SaveDialog1.DefaultExt := 'html';
  SaveDialog1.FileName := 'grid_print_preview.html';
  SaveDialog1.InitialDir := GetDemoStatePath;
  SaveDialog1.Filter := 'HTML Files (*.html)|*.html';
  if not SaveDialog1.Execute then
    Exit;

  Exporter := TVittixDBGridExporter.Create(VittixGrid);
  grpExport.Enabled := False;
  try
    Exporter.Options.IncludeFooter := VittixGrid.FooterVisible;
    Exporter.OpenHTMLPrintPreview(SaveDialog1.FileName);
  finally
    grpExport.Enabled := True;
    Exporter.Free;
  end;
end;

{ Server-side filter/sort demo }

procedure TfrmVittixDemo.CreateServerTable(ARowCount: Integer);
var
  StartTime: TDateTime;
begin
  StartTime := Now;
  FDConnection1.ExecSQL(
    'CREATE TABLE IF NOT EXISTS orders (id INTEGER PRIMARY KEY, ' +
    'name TEXT, amount REAL, created TIMESTAMP, active INTEGER)');
  FDConnection1.ExecSQL('DELETE FROM orders');
  // One recursive-CTE INSERT: fast even for 100k rows, with NULL names
  // and amounts, unicode text and spread dates mixed in.
  FDConnection1.ExecSQL(
    'INSERT INTO orders (id, name, amount, created, active) ' +
    'WITH RECURSIVE seq(i) AS (' +
    '  SELECT 1 UNION ALL SELECT i + 1 FROM seq WHERE i < ' +
    IntToStr(ARowCount) + ') ' +
    'SELECT i, ' +
    '  CASE WHEN i % 7 = 0 THEN NULL ' +
    '       WHEN i % 3 = 0 THEN ''Émile '' || i ' +
    '       ELSE ''Customer '' || i END, ' +
    '  CASE WHEN i % 5 = 0 THEN NULL ELSE i * 1.25 END, ' +
    '  DATE(''2020-01-01'', ''+'' || (i % 2000) || '' days''), ' +
    '  CASE WHEN i % 2 = 0 THEN 1 ELSE 0 END ' +
    'FROM seq');
  UpdateServerInfo(Format('fixture: %d rows in %.1fs',
    [ARowCount, (Now - StartTime) * SecsPerDay]));
end;

procedure TfrmVittixDemo.SetupServerDemo;
begin
  cmbFilterMode.Items.BeginUpdate;
  try
    cmbFilterMode.Items.Clear;
    cmbFilterMode.Items.Add('Client');
    cmbFilterMode.Items.Add('Server');
    cmbFilterMode.Items.Add('Auto');
    cmbFilterMode.ItemIndex := 0;
  finally
    cmbFilterMode.Items.EndUpdate;
  end;
  cmbFilterMode.OnChange := nil; // no re-entrance while building

  FDConnection1 := TFDConnection.Create(Self);
  FDConnection1.DriverName := 'SQLite';
  FDConnection1.Params.Values['Database'] := ':memory:';

  FDQuery1 := TFDQuery.Create(Self);
  FDQuery1.Connection := FDConnection1;

  CreateServerTable(100000);
  FDQuery1.SQL.Text := 'SELECT id, name, amount, created, active FROM orders';
  FDQuery1.Open;

  // A provider bound to the SQLite query enables the server modes.
  VittixGrid.Controller.Provider := TVittixFireDACProvider.Create(FDQuery1);

  // Move the grid onto the big table (the ClientDataSet demo data stays
  // reachable through the Data Operations group only in Client mode).
  cmbFilterMode.OnChange := cmbFilterModeChange;
  lblServerInfo.Caption := 'data: SQLite :memory:, 100000 rows';
end;

procedure TfrmVittixDemo.cmbFilterModeChange(Sender: TObject);
var
  StartTime: TDateTime;
  ElapsedMs: Double;
begin
  StartTime := Now;
  case cmbFilterMode.ItemIndex of
    1: VittixGrid.FilterMode := vcmServer;
    2: VittixGrid.FilterMode := vcmAuto;
  else
    VittixGrid.FilterMode := vcmClient;
  end;
  // Re-route the current filter/sort state through the chosen mode.
  VittixGrid.Controller.RefreshFiltering;
  VittixGrid.Controller.RefreshSorting;
  ElapsedMs := (Now - StartTime) * MSecsPerDay;
  UpdateServerInfo(Format('switched to %s in %.0f ms',
    [cmbFilterMode.Text, ElapsedMs]));
end;

procedure TfrmVittixDemo.UpdateServerInfo(const AExtra: string);
begin
  lblServerInfo.Caption := 'data: SQLite :memory:, 100000 rows, ' + AExtra;
end;

{ Per-column editors and saved filter sets }

procedure TfrmVittixDemo.CreateStatusLookup;
var
  Lookup: TClientDataSet;
  LookupField: TStringField;
  KeyField: TIntegerField;
begin
  // A small code dataset feeds the lookup editor demo: StatusId (int) on
  // the main dataset resolves to a status name through the lookup field.
  Lookup := TClientDataSet.Create(Self);
  Lookup.FieldDefs.Add('Code', ftInteger);
  Lookup.FieldDefs.Add('Name', ftString, 30);
  Lookup.CreateDataSet;
  Lookup.LogChanges := False;
  Lookup.AppendRecord([1, 'Active']);
  Lookup.AppendRecord([2, 'Pending']);
  Lookup.AppendRecord([3, 'Completed']);
  Lookup.AppendRecord([4, 'Cancelled']);

  KeyField := TIntegerField.Create(Self);
  KeyField.FieldName := 'StatusId';
  KeyField.DataSet := ClientDataSet1;

  LookupField := TStringField.Create(Self);
  LookupField.FieldName := 'StatusName';
  LookupField.LookupDataSet := Lookup;
  LookupField.LookupKeyFields := 'Code';
  LookupField.LookupResultField := 'Name';
  LookupField.KeyFields := 'StatusId';
  LookupField.Lookup := True;
  LookupField.Size := 30;
  LookupField.DataSet := ClientDataSet1;
end;

procedure TfrmVittixDemo.ConfigureColumnEditors;
var
  Info: TVittixDBGridColumnInfo;
  Column: TColumn;
begin
  // Pick list on Status: plain captions write to the string field.
  Info := VittixGrid.ColumnInfo.FindByFieldName('Status');
  if Assigned(Info) then
  begin
    Info.EditorKind := vePickList;
    Info.PickItems := 'Active' + sLineBreak + 'Pending' + sLineBreak +
      'Completed' + sLineBreak + 'Cancelled';
    Info.Required := True;
  end;

  // Masked numeric on TotalAmount: non-negative, two decimals.
  Info := VittixGrid.ColumnInfo.FindByFieldName('TotalAmount');
  if Assigned(Info) then
  begin
    Info.EditorKind := veMaskedNumber;
    Info.HasMin := True;
    Info.MinValue := 0;
    Info.DecimalPlaces := 2;
  end;

  // A grid column for the lookup field; the lookup editor opens from it.
  Column := VittixGrid.Controller.FindColumnByFieldName('StatusName');
  if Column = nil then
  begin
    Column := VittixGrid.Columns.Add;
    Column.FieldName := 'StatusName';
    Column.Title.Caption := 'Status Name';
    Column.Width := 100;
  end;
  Info := VittixGrid.ColumnInfo.FindByFieldName('StatusName');
  if Assigned(Info) then
    Info.EditorKind := veLookup;
end;

procedure TfrmVittixDemo.btnSaveFilterSetClick(Sender: TObject);
var
  Name: string;
begin
  Name := '';
  if not InputQuery('Save Current Filters', 'Filter set name:', Name) then
    Exit;
  if Trim(Name) = '' then
    Exit;
  VittixGrid.Controller.SaveCurrentFilterAs(Trim(Name));
  ShowMessage('Filter set "' + Trim(Name) + '" saved.');
end;

procedure TfrmVittixDemo.btnManageFilterSetsClick(Sender: TObject);
var
  Columns: TStringList;
  I: Integer;
  AppliedName: string;
begin
  Columns := TStringList.Create;
  try
    for I := 0 to VittixGrid.Columns.Count - 1 do
      if VittixGrid.Columns[I].FieldName <> '' then
        Columns.Add(VittixGrid.Columns[I].FieldName);

    with TVittixDBGridFilterSetManager.CreateManager(Self,
      VittixGrid.Controller.FilterSetList, Columns,
      procedure
      begin
        VittixGrid.Controller.PersistFilterSets;
      end,
      procedure(AName: string)
      begin
        VittixGrid.Controller.SaveCurrentFilterAs(AName);
      end) do
    try
      if Execute(AppliedName) and (AppliedName <> '') then
        VittixGrid.Controller.ApplyFilterSet(AppliedName);
    finally
      Free;
    end;
  finally
    Columns.Free;
  end;
end;

{ VCL Style switcher }

procedure TfrmVittixDemo.InitStyleCombo;
var
  StyleName: string;
  Names: TStringList;
begin
  cmbStyle.Items.BeginUpdate;
  try
    cmbStyle.Items.Clear;
    cmbStyle.Items.Add('System default');

    // Styles embedded through the project resources are already registered
    // here. When the executable was built without them, fall back to .vsf
    // files in a styles folder next to it; a build with neither still works
    // and simply offers the system default.
    if Length(TStyleManager.StyleNames) <= 1 then
      LoadStyleFilesFromFolder;

    Names := TStringList.Create;
    try
      for StyleName in TStyleManager.StyleNames do
        if not SameText(StyleName, TStyleManager.SystemStyle.Name) then
          Names.Add(StyleName);
      Names.Sort;
      cmbStyle.Items.AddStrings(Names);
    finally
      Names.Free;
    end;
  finally
    cmbStyle.Items.EndUpdate;
  end;

  SelectActiveStyle;
end;

procedure TfrmVittixDemo.LoadStyleFilesFromFolder;
var
  StyleDir: string;
  FileName: string;
begin
  StyleDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'styles');
  if not TDirectory.Exists(StyleDir) then
    Exit;

  for FileName in TDirectory.GetFiles(StyleDir, '*.vsf') do
  begin
    try
      TStyleManager.LoadFromFile(FileName);
    except
      // An unreadable style file must not break demo startup.
    end;
  end;
end;

procedure TfrmVittixDemo.SelectActiveStyle;
var
  I: Integer;
  Active: string;
begin
  Active := TStyleManager.ActiveStyle.Name;
  if SameText(Active, TStyleManager.SystemStyle.Name) then
    Active := 'System default';
  I := cmbStyle.Items.IndexOf(Active);
  if I >= 0 then
    cmbStyle.ItemIndex := I;
end;

procedure TfrmVittixDemo.cmbStyleChange(Sender: TObject);
var
  StyleName: string;
begin
  StyleName := cmbStyle.Text;
  if StyleName = 'System default' then
    StyleName := TStyleManager.SystemStyle.Name;

  // TrySetStyle returns False (instead of raising) when the style is not
  // registered, e.g. a build without the style resources.
  if not TStyleManager.TrySetStyle(StyleName, False) then
  begin
    ShowMessage(Format('The style "%s" is not available in this build.',
      [cmbStyle.Text]));
    SelectActiveStyle;
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

procedure TfrmVittixDemo.GridIncrementalSearch(Sender: TObject;
  const SearchText: string; Found: Boolean);
begin
  if SearchText = '' then
    lblSearchStatus.Caption := 'Search: -'
  else if Found then
    lblSearchStatus.Caption := 'Search: ' + SearchText
  else
    lblSearchStatus.Caption := 'Search: ' + SearchText + ' (no match)';
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
