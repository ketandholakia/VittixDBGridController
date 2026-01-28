object frmVittixDemo: TfrmVittixDemo
  Left = 0
  Top = 0
  Caption = 'Vittix DBGrid - Complete Feature Demonstration'
  ClientHeight = 700
  ClientWidth = 1200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1200
    Height = 70
    Align = alTop
    BevelOuter = bvNone
    Color = 2763306
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 16
      Top = 12
      Width = 373
      Height = 30
      Caption = 'Vittix DBGrid Complete Feature Demo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSubtitle: TLabel
      Left = 16
      Top = 43
      Width = 504
      Height = 15
      Caption = 
        'Demonstrating all features: Sorting, Filtering, Aggregations, Ex' +
        'port, Column Chooser, and more!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
  end
  object pnlToolbar: TPanel
    Left = 0
    Top = 70
    Width = 1200
    Height = 120
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object grpDataOperations: TGroupBox
      AlignWithMargins = True
      Left = 8
      Top = 3
      Width = 185
      Height = 114
      Margins.Left = 8
      Align = alLeft
      Caption = ' Data Operations '
      TabOrder = 0
      object btnAddRecord: TButton
        Left = 8
        Top = 20
        Width = 169
        Height = 25
        Caption = 'Add New Record'
        TabOrder = 0
        OnClick = btnAddRecordClick
      end
      object btnEditRecord: TButton
        Left = 8
        Top = 47
        Width = 169
        Height = 25
        Caption = 'Edit Notes (Memo Editor)'
        TabOrder = 1
        OnClick = btnEditRecordClick
      end
      object btnDeleteRecord: TButton
        Left = 8
        Top = 74
        Width = 169
        Height = 25
        Caption = 'Delete Selected Record'
        TabOrder = 2
        OnClick = btnDeleteRecordClick
      end
    end
    object grpFiltering: TGroupBox
      AlignWithMargins = True
      Left = 199
      Top = 3
      Width = 265
      Height = 114
      Align = alLeft
      Caption = ' Filtering & Search '
      TabOrder = 1
      object lblGlobalSearch: TLabel
        Left = 8
        Top = 20
        Width = 69
        Height = 13
        Caption = 'Global Search:'
      end
      object edtGlobalSearch: TEdit
        Left = 8
        Top = 39
        Width = 249
        Height = 21
        TabOrder = 0
        TextHint = 'Search across all columns...'
        OnChange = edtGlobalSearchChange
      end
      object btnClearFilters: TButton
        Left = 8
        Top = 66
        Width = 120
        Height = 25
        Caption = 'Clear All Filters'
        TabOrder = 1
        OnClick = btnClearFiltersClick
      end
      object chkShowFiltered: TCheckBox
        Left = 134
        Top = 70
        Width = 123
        Height = 17
        Caption = 'Apply Dataset Filter'
        TabOrder = 2
        OnClick = chkShowFilteredClick
      end
    end
    object grpDisplay: TGroupBox
      AlignWithMargins = True
      Left = 470
      Top = 3
      Width = 225
      Height = 114
      Align = alLeft
      Caption = ' Display Options '
      TabOrder = 2
      object chkAlternateRows: TCheckBox
        Left = 8
        Top = 24
        Width = 161
        Height = 17
        Caption = 'Alternating Row Colors'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkAlternateRowsClick
      end
      object chkShowFooter: TCheckBox
        Left = 8
        Top = 47
        Width = 161
        Height = 17
        Caption = 'Show Footer Panel'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = chkShowFooterClick
      end
      object btnColumnChooser: TButton
        Left = 8
        Top = 70
        Width = 209
        Height = 25
        Caption = 'Column Chooser...'
        TabOrder = 2
        OnClick = btnColumnChooserClick
      end
    end
    object grpExport: TGroupBox
      AlignWithMargins = True
      Left = 701
      Top = 3
      Width = 185
      Height = 114
      Align = alLeft
      Caption = ' Export && Configuration '
      TabOrder = 3
      object btnExportCSV: TButton
        Left = 8
        Top = 20
        Width = 169
        Height = 25
        Caption = 'Export to CSV...'
        TabOrder = 0
        OnClick = btnExportCSVClick
      end
      object btnSaveConfig: TButton
        Left = 8
        Top = 51
        Width = 169
        Height = 25
        Caption = 'Save Configuration...'
        TabOrder = 1
        OnClick = btnSaveConfigClick
      end
      object btnLoadConfig: TButton
        Left = 8
        Top = 82
        Width = 169
        Height = 25
        Caption = 'Load Configuration...'
        TabOrder = 2
        OnClick = btnLoadConfigClick
      end
    end
    object btnRefreshData: TButton
      Left = 892
      Top = 23
      Width = 120
      Height = 25
      Caption = 'Refresh Grid'
      TabOrder = 4
      OnClick = btnRefreshDataClick
    end
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 679
    Width = 1200
    Height = 21
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblRecordCount: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 3
      Width = 83
      Height = 15
      Margins.Left = 8
      Align = alLeft
      Caption = 'Records: 0 of 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblFilterStatus: TLabel
      AlignWithMargins = True
      Left = 97
      Top = 3
      Width = 61
      Height = 15
      Align = alLeft
      Caption = 'Filter: None'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 135
    end
    object lblSortStatus: TLabel
      AlignWithMargins = True
      Left = 164
      Top = 3
      Width = 56
      Height = 15
      Align = alLeft
      Caption = 'Sort: None'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 219
    end
  end
  object VittixGrid: TVittixDBGrid
    Left = 0
    Top = 190
    Width = 1200
    Height = 489
    Align = alClient
    DataSource = DataSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    OnDblClick = VittixGridDblClick
    OnTitleClick = VittixGridTitleClick
    FooterVisible = True
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Width = 50
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CompanyName'
        Title.Caption = 'Company Name'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ContactName'
        Title.Caption = 'Contact Name'
        Width = 120
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Email'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Phone'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Country'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'City'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OrderDate'
        Title.Caption = 'Order Date'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TotalAmount'
        Title.Caption = 'Total Amount'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Quantity'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Status'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Notes'
        Width = 200
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Discount'
        Title.Caption = 'Discount %'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShippingDate'
        Title.Caption = 'Shipping Date'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PaymentMethod'
        Title.Caption = 'Payment Method'
        Width = 120
        Visible = True
      end>
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 600
    Top = 400
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 520
    Top = 400
    object ClientDataSet1ID: TIntegerField
      FieldName = 'ID'
    end
    object ClientDataSet1CompanyName: TStringField
      FieldName = 'CompanyName'
      Size = 100
    end
    object ClientDataSet1ContactName: TStringField
      FieldName = 'ContactName'
      Size = 100
    end
    object ClientDataSet1Email: TStringField
      FieldName = 'Email'
      Size = 100
    end
    object ClientDataSet1Phone: TStringField
      FieldName = 'Phone'
    end
    object ClientDataSet1Country: TStringField
      FieldName = 'Country'
      Size = 50
    end
    object ClientDataSet1City: TStringField
      FieldName = 'City'
      Size = 50
    end
    object ClientDataSet1OrderDate: TDateTimeField
      FieldName = 'OrderDate'
    end
    object ClientDataSet1TotalAmount: TCurrencyField
      FieldName = 'TotalAmount'
    end
    object ClientDataSet1Quantity: TIntegerField
      FieldName = 'Quantity'
    end
    object ClientDataSet1Status: TStringField
      FieldName = 'Status'
    end
    object ClientDataSet1Notes: TMemoField
      FieldName = 'Notes'
      BlobType = ftMemo
    end
    object ClientDataSet1Discount: TFloatField
      FieldName = 'Discount'
      DisplayFormat = '0.00'
    end
    object ClientDataSet1ShippingDate: TDateTimeField
      FieldName = 'ShippingDate'
    end
    object ClientDataSet1PaymentMethod: TStringField
      FieldName = 'PaymentMethod'
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 680
    Top = 400
    object mnuEditRecord: TMenuItem
      Caption = 'Edit Record'
      OnClick = mnuEditRecordClick
    end
    object mnuDeleteRecord: TMenuItem
      Caption = 'Delete Record'
      OnClick = mnuDeleteRecordClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuExportSelection: TMenuItem
      Caption = 'Export Selection'
      OnClick = mnuExportSelectionClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mnuCopyCell: TMenuItem
      Caption = 'Copy Cell'
      OnClick = mnuCopyCellClick
    end
    object mnuCopyRow: TMenuItem
      Caption = 'Copy Row'
      OnClick = mnuCopyRowClick
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 760
    Top = 400
  end
  object OpenDialog1: TOpenDialog
    Left = 840
    Top = 400
  end
  object tmrSearchDelay: TTimer
    Enabled = False
    OnTimer = tmrSearchDelayTimer
    Left = 920
    Top = 400
  end
end
