unit VittixDBGridForm;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Math,
  System.IniFiles,
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
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.ColumnChooser,
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
    btnExportCSV: TButton;
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
    mnuExportSelection: TMenuItem;
    N2: TMenuItem;
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
    procedure btnExportCSVClick(Sender: TObject);
    procedure btnSaveConfigClick(Sender: TObject);
    procedure btnLoadConfigClick(Sender: TObject);

    // Grid Events
    procedure VittixGridTitleClick(Column: TColumn);
    procedure VittixGridDblClick(Sender: TObject);

    // Popup Menu
    procedure mnuEditRecordClick(Sender: TObject);
    procedure mnuDeleteRecordClick(Sender: TObject);
    procedure mnuExportSelectionClick(Sender: TObject);
    procedure mnuCopyCellClick(Sender: TObject);
    procedure mnuCopyRowClick(Sender: TObject);

    // Dataset Events
    procedure ClientDataSet1AfterPost(DataSet: TDataSet);
    procedure ClientDataSet1AfterDelete(DataSet: TDataSet);
    procedure ClientDataSet1AfterScroll(DataSet: TDataSet);

  private
    procedure InitializeDataset;
    procedure LoadSampleData;
    procedure UpdateStatusBar;
    procedure ExportToCSV(const FileName: string);
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

  // Initialize dataset
  InitializeDataset;
  LoadSampleData;

  // Setup aggregations
  SetupAggregations;

  // Initial UI state
  chkAlternateRows.Checked := VittixGrid.AlternatingRowColors;
  chkShowFooter.Checked := VittixGrid.FooterVisible;

  // Update status
  UpdateStatusBar;

  // Configure search delay
  tmrSearchDelay.Interval := 500; // 500ms delay
  tmrSearchDelay.Enabled := False;

  ShowMessage(
    'Welcome to Vittix DBGrid Feature Demo!' + sLineBreak + sLineBreak +
    'Features to try:' + sLineBreak +
    '• Click column headers to sort (Ctrl+Click for multi-column)' + sLineBreak +
    '• Right-click column headers to filter' + sLineBreak +
    '• Use Global Search to search all columns' + sLineBreak +
    '• Right-click footer to change aggregations' + sLineBreak +
    '• Double-click row to edit' + sLineBreak +
    '• Right-click grid for context menu' + sLineBreak +
    '• Use Column Chooser to show/hide/reorder columns'
  );
end;

procedure TfrmVittixDemo.FormDestroy(Sender: TObject);
begin
  // Cleanup handled automatically
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
  lblRecordCount.Caption := Format('Records: %d of %d', [
    ClientDataSet1.RecordCount,
    ClientDataSet1.RecordCount
  ]);

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
begin
  if Assigned(VittixGrid.Controller) and (VittixGrid.Controller is TVittixDBGridController) then
    TVittixDBGridController(VittixGrid.Controller).Refresh;

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
begin
  edtGlobalSearch.Clear;
  if Assigned(VittixGrid.Controller) and (VittixGrid.Controller is TVittixDBGridController) then
  begin
    TVittixDBGridController(VittixGrid.Controller).ClearFilters;
    UpdateStatusBar;
  end;
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
begin
  TVittixDBGridColumnChooserForm.Execute(VittixGrid);
end;

{ Export & Config }

procedure TfrmVittixDemo.btnExportCSVClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*';
  SaveDialog1.DefaultExt := 'csv';
  SaveDialog1.FileName := 'export_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.csv';

  if SaveDialog1.Execute then
  begin
    ExportToCSV(SaveDialog1.FileName);
    ShowMessage('Data exported to: ' + SaveDialog1.FileName);
  end;
end;

procedure TfrmVittixDemo.ExportToCSV(const FileName: string);
var
  CSV: TStringList;
  Line: string;
  I: Integer;
begin
  CSV := TStringList.Create;
  try
    // Header
    Line := '';
    for I := 0 to VittixGrid.Columns.Count - 1 do
    begin
      if VittixGrid.Columns[I].Visible then
      begin
        if Line <> '' then Line := Line + ',';
        Line := Line + '"' + VittixGrid.Columns[I].Title.Caption + '"';
      end;
    end;
    CSV.Add(Line);

    // Data
    ClientDataSet1.DisableControls;
    try
      ClientDataSet1.First;
      while not ClientDataSet1.Eof do
      begin
        Line := '';
        for I := 0 to VittixGrid.Columns.Count - 1 do
        begin
          if VittixGrid.Columns[I].Visible then
          begin
            if Line <> '' then Line := Line + ',';
            Line := Line + '"' +
              StringReplace(
                VittixGrid.Columns[I].Field.DisplayText,
                '"',
                '""',
                [rfReplaceAll]
              ) + '"';
          end;
        end;
        CSV.Add(Line);
        ClientDataSet1.Next;
      end;
    finally
      ClientDataSet1.EnableControls;
    end;

    CSV.SaveToFile(FileName);
  finally
    CSV.Free;
  end;
end;

procedure TfrmVittixDemo.btnSaveConfigClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Config Files (*.ini)|*.ini|All Files (*.*)|*.*';
  SaveDialog1.DefaultExt := 'ini';
  SaveDialog1.FileName := 'grid_config.ini';

  if SaveDialog1.Execute then
  begin
    SaveColumnConfiguration(SaveDialog1.FileName);
    ShowMessage('Configuration saved to: ' + SaveDialog1.FileName);
  end;
end;

procedure TfrmVittixDemo.SaveColumnConfiguration(const FileName: string);
var
  Ini: TIniFile;
  I: Integer;
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
begin
  Ini := TIniFile.Create(FileName);
  try
    for I := 0 to VittixGrid.Columns.Count - 1 do
    begin
      Col := VittixGrid.Columns[I];
      Ini.WriteBool('Columns', Col.FieldName + '_Visible', Col.Visible);
      Ini.WriteInteger('Columns', Col.FieldName + '_Width', Col.Width);
      Ini.WriteInteger('Columns', Col.FieldName + '_Index', Col.Index);
    end;

    for I := 0 to VittixGrid.ColumnInfo.Count - 1 do
    begin
      Info := VittixGrid.ColumnInfo[I];
      Ini.WriteInteger('ColumnInfo', Info.FieldName + '_SortOrder', Ord(Info.SortOrder));
      Ini.WriteInteger('ColumnInfo', Info.FieldName + '_SortIndex', Info.SortIndex);
      Ini.WriteString('ColumnInfo', Info.FieldName + '_FilterText', Info.FilterText);
      Ini.WriteBool('ColumnInfo', Info.FieldName + '_HasFilter', Info.HasFilter);
      Ini.WriteInteger('ColumnInfo', Info.FieldName + '_AggregationType', Ord(Info.AggregationType));
    end;

    Ini.WriteBool('Display', 'AlternatingRows', VittixGrid.AlternatingRowColors);
    Ini.WriteBool('Display', 'ShowFooter', VittixGrid.FooterVisible);
  finally
    Ini.Free;
  end;
end;

procedure TfrmVittixDemo.btnLoadConfigClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Config Files (*.ini)|*.ini|All Files (*.*)|*.*';
  OpenDialog1.DefaultExt := 'ini';
  OpenDialog1.FileName := 'grid_config.ini';

  if OpenDialog1.Execute then
  begin
    LoadColumnConfiguration(OpenDialog1.FileName);
    ShowMessage('Configuration loaded from: ' + OpenDialog1.FileName);
  end;
end;

procedure TfrmVittixDemo.LoadColumnConfiguration(const FileName: string);
var
  Ini: TIniFile;
  I: Integer;
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
begin
  Ini := TIniFile.Create(FileName);
  try
    for I := 0 to VittixGrid.Columns.Count - 1 do
    begin
      Col := VittixGrid.Columns[I];
      Col.Visible := Ini.ReadBool('Columns', Col.FieldName + '_Visible', Col.Visible);
      Col.Width := Ini.ReadInteger('Columns', Col.FieldName + '_Width', Col.Width);
    end;

    for I := 0 to VittixGrid.ColumnInfo.Count - 1 do
    begin
      Info := VittixGrid.ColumnInfo[I];
      Info.SortOrder := TVittixSortOrder(
        Ini.ReadInteger('ColumnInfo', Info.FieldName + '_SortOrder', Ord(Info.SortOrder))
      );
      Info.SortIndex := Ini.ReadInteger('ColumnInfo', Info.FieldName + '_SortIndex', Info.SortIndex);
      Info.FilterText := Ini.ReadString('ColumnInfo', Info.FieldName + '_FilterText', Info.FilterText);
      Info.HasFilter := Ini.ReadBool('ColumnInfo', Info.FieldName + '_HasFilter', Info.HasFilter);
      Info.AggregationType := TVittixAggregationType(
        Ini.ReadInteger('ColumnInfo', Info.FieldName + '_AggregationType', Ord(Info.AggregationType))
      );
    end;

    VittixGrid.AlternatingRowColors :=
      Ini.ReadBool('Display', 'AlternatingRows', True);
    VittixGrid.FooterVisible :=
      Ini.ReadBool('Display', 'ShowFooter', True);

    chkAlternateRows.Checked := VittixGrid.AlternatingRowColors;
    chkShowFooter.Checked := VittixGrid.FooterVisible;

    if Assigned(VittixGrid.Controller) and (VittixGrid.Controller is TVittixDBGridController) then
      TVittixDBGridController(VittixGrid.Controller).ApplyState;

    VittixGrid.Invalidate;
    UpdateStatusBar;
  finally
    Ini.Free;
  end;
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

procedure TfrmVittixDemo.mnuExportSelectionClick(Sender: TObject);
begin
  ShowMessage('Export selection feature - would export selected rows only');
end;

// FIX: Removed intermediate TColumn logic that caused E2003
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
