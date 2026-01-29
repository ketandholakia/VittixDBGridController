object frmExportDialog: TfrmExportDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Export Data'
  ClientHeight = 520
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    Color = 2763306
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      AlignWithMargins = True
      Left = 16
      Top = 8
      Width = 568
      Height = 34
      Margins.Left = 16
      Margins.Top = 8
      Margins.Right = 16
      Margins.Bottom = 8
      Align = alClient
      Caption = 
        'Export Grid Data'#13#10'Choose format, configure options, and export ' +
        'your data'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 479
    Width = 600
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnExport: TButton
      Left = 424
      Top = 8
      Width = 80
      Height = 25
      Caption = 'Export'
      Default = True
      TabOrder = 0
      OnClick = btnExportClick
    end
    object btnCancel: TButton
      Left = 510
      Top = 8
      Width = 80
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 50
    Width = 600
    Height = 407
    ActivePage = tabFormat
    Align = alClient
    TabOrder = 2
    OnChange = PageControl1Change
    object tabFormat: TTabSheet
      Caption = 'Format && Destination'
      object grpFormat: TGroupBox
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 576
        Height = 153
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alTop
        Caption = ' Export Format '
        TabOrder = 0
        object rbCSV: TRadioButton
          Left = 16
          Top = 24
          Width = 200
          Height = 17
          Caption = 'CSV (Comma-Separated Values)'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbFormatClick
        end
        object rbTSV: TRadioButton
          Left = 16
          Top = 47
          Width = 200
          Height = 17
          Caption = 'TSV (Tab-Separated Values)'
          TabOrder = 1
          OnClick = rbFormatClick
        end
        object rbExcel: TRadioButton
          Left = 16
          Top = 70
          Width = 200
          Height = 17
          Caption = 'Excel (XLSX)'
          TabOrder = 2
          OnClick = rbFormatClick
        end
        object rbHTML: TRadioButton
          Left = 16
          Top = 93
          Width = 200
          Height = 17
          Caption = 'HTML Table'
          TabOrder = 3
          OnClick = rbFormatClick
        end
        object rbXML: TRadioButton
          Left = 16
          Top = 116
          Width = 200
          Height = 17
          Caption = 'XML Document'
          TabOrder = 4
          OnClick = rbFormatClick
        end
        object rbJSON: TRadioButton
          Left = 296
          Top = 24
          Width = 200
          Height = 17
          Caption = 'JSON Array'
          TabOrder = 5
          OnClick = rbFormatClick
        end
      end
      object grpDestination: TGroupBox
        AlignWithMargins = True
        Left = 8
        Top = 177
        Width = 576
        Height = 121
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alTop
        Caption = ' Destination '
        TabOrder = 1
        object rbFile: TRadioButton
          Left = 16
          Top = 24
          Width = 113
          Height = 17
          Caption = 'Save to File'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbFileClick
        end
        object rbClipboard: TRadioButton
          Left = 16
          Top = 85
          Width = 113
          Height = 17
          Caption = 'Copy to Clipboard'
          TabOrder = 1
          OnClick = rbFileClick
        end
        object edtFileName: TEdit
          Left = 32
          Top = 47
          Width = 457
          Height = 21
          TabOrder = 2
          TextHint = 'Enter file path...'
        end
        object btnBrowse: TButton
          Left = 495
          Top = 45
          Width = 65
          Height = 25
          Caption = 'Browse...'
          TabOrder = 3
          OnClick = btnBrowseClick
        end
      end
    end
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      object grpScope: TGroupBox
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 576
        Height = 105
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alTop
        Caption = ' Export Scope '
        TabOrder = 0
        object chkVisibleOnly: TCheckBox
          Left = 16
          Top = 24
          Width = 250
          Height = 17
          Caption = 'Export visible columns only'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object chkFilteredOnly: TCheckBox
          Left = 16
          Top = 47
          Width = 250
          Height = 17
          Caption = 'Export filtered rows only'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object chkIncludeHeaders: TCheckBox
          Left = 16
          Top = 70
          Width = 250
          Height = 17
          Caption = 'Include column headers'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
      end
      object grpFormatting: TGroupBox
        AlignWithMargins = True
        Left = 8
        Top = 129
        Width = 576
        Height = 161
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alTop
        Caption = ' Data Formatting '
        TabOrder = 1
        object lblDateFormat: TLabel
          Left = 16
          Top = 27
          Width = 64
          Height = 13
          Caption = 'Date Format:'
        end
        object lblTimeFormat: TLabel
          Left = 16
          Top = 67
          Width = 64
          Height = 13
          Caption = 'Time Format:'
        end
        object lblCurrencyFormat: TLabel
          Left = 16
          Top = 107
          Width = 85
          Height = 13
          Caption = 'Currency Format:'
        end
        object edtDateFormat: TEdit
          Left = 16
          Top = 43
          Width = 200
          Height = 21
          TabOrder = 0
          Text = 'yyyy-mm-dd'
        end
        object edtTimeFormat: TEdit
          Left = 16
          Top = 83
          Width = 200
          Height = 21
          TabOrder = 1
          Text = 'hh:nn:ss'
        end
        object edtCurrencyFormat: TEdit
          Left = 16
          Top = 123
          Width = 200
          Height = 21
          TabOrder = 2
          Text = '#,##0.00'
        end
      end
    end
    object tabPreview: TTabSheet
      Caption = 'Preview'
      ImageIndex = 2
      object memoPreview: TMemo
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 576
        Height = 355
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 457
    Width = 600
    Height = 22
    Align = alBottom
    TabOrder = 3
    Visible = False
  end
  object lblProgress: TLabel
    Left = 8
    Top = 463
    Width = 71
    Height = 13
    Caption = 'Exporting...'
    Visible = False
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'csv'
    Filter = 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 520
    Top = 80
  end
end
