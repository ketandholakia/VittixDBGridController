unit Vittix.DBGrid.Export.Engine;

{$REGION 'Documentation'}
/// <summary>
/// Export Engine for Vittix.DBGrid Component Suite
///
/// SUPPORTED FORMATS:
/// 1. Excel (XLSX) - Built-in SpreadsheetML writer (no external dependencies)
/// 2. CSV - Comma-Separated Values
/// 3. TSV - Tab-Separated Values
/// 4. HTML - Formatted HTML table
/// 5. XML - Structured XML document
/// 6. JSON - JSON array format
/// 7. Clipboard - Copy to Windows clipboard
/// 8. Text - Fixed-width text format
/// (PDF and legacy XLS are not implemented; PDF requires a reporting engine.)
///
/// FEATURES:
/// - Export visible columns only or all columns
/// - Export filtered data or all data
/// - Optional header and footer (aggregation) rows
/// - Custom formatting per column
/// - Progress callback with cancellation
/// - Unicode support
/// - Configurable delimiters and encoding
///
/// Machine-readable formats (CSV, TSV, XML, JSON, XLSX) write numbers with
/// full precision and an invariant decimal separator; HTML and Text keep the
/// configurable display formatting (FloatFormat, CurrencyFormat).
///
/// USAGE:
///   var Exporter: TVittixDBGridExporter;
///   Exporter := TVittixDBGridExporter.Create(VittixGrid);
///   try
///     Exporter.ExportToExcel('output.xlsx');
///   finally
///     Exporter.Free;
///   end;
/// </summary>
{$ENDREGION}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.Generics.Collections,
  System.Zip,
  Vcl.DBGrids,
  Vcl.Clipbrd,
  Data.DB,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo;

type
  // Raised for unsupported/unimplemented export formats and engine misuse
  EVittixExportError = class(Exception);

  // Export format enumeration
  TVittixExportFormat = (
    vefCSV,           // Comma-Separated Values
    vefTSV,           // Tab-Separated Values
    vefExcelXLSX,     // Excel 2007+ (XML-based)
    vefHTML,          // HTML table
    vefXML,           // XML document
    vefJSON,          // JSON array
    vefPDF,           // PDF document (requires reporting component)
    vefClipboard,     // Copy to clipboard
    vefText           // Fixed-width text
  );

  // Export options
  TVittixExportOptions = class(TPersistent)
  private
    FExportVisibleOnly: Boolean;
    FExportFilteredOnly: Boolean;
    FIncludeHeaders: Boolean;
    FIncludeFooter: Boolean;
    FDateFormat: string;
    FTimeFormat: string;
    FDateTimeFormat: string;
    FCurrencyFormat: string;
    FFloatFormat: string;
    FBooleanAsText: Boolean;
    FTrueText: string;
    FFalseText: string;
    FNullText: string;
    FDelimiter: Char;
    FQuoteChar: Char;
    // FEncoding is private as it's set to UTF8 by default and not exposed for simplicity.
    FEncoding: TEncoding;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure SetDefaults;
  published
    property ExportVisibleOnly: Boolean read FExportVisibleOnly write FExportVisibleOnly default True;
    property ExportFilteredOnly: Boolean read FExportFilteredOnly write FExportFilteredOnly default True;
    property IncludeHeaders: Boolean read FIncludeHeaders write FIncludeHeaders default True;
    property IncludeFooter: Boolean read FIncludeFooter write FIncludeFooter default False;
    property DateFormat: string read FDateFormat write FDateFormat;
    property TimeFormat: string read FTimeFormat write FTimeFormat;
    property DateTimeFormat: string read FDateTimeFormat write FDateTimeFormat;
    property CurrencyFormat: string read FCurrencyFormat write FCurrencyFormat;
    property FloatFormat: string read FFloatFormat write FFloatFormat;
    property BooleanAsText: Boolean read FBooleanAsText write FBooleanAsText default True;
    property TrueText: string read FTrueText write FTrueText;
    property FalseText: string read FFalseText write FFalseText;
    property NullText: string read FNullText write FNullText;
    property Delimiter: Char read FDelimiter write FDelimiter default ',';
    property QuoteChar: Char read FQuoteChar write FQuoteChar default '"';
    property Encoding: TEncoding read FEncoding write FEncoding; // Added public property for encoding
  end;

  // Progress callback
  TVittixExportProgressEvent = procedure(Sender: TObject; Current, Total: Integer; 
    var Cancel: Boolean) of object;

  // Main exporter class
  TVittixDBGridExporter = class(TComponent)
  private
    FGrid: TVittixDBGrid;
    FDataset: TDataSet;
    FOptions: TVittixExportOptions;
    FOnProgress: TVittixExportProgressEvent;
    FCancelled: Boolean;

    // function GetVisibleColumns: TList<TColumn>; // Removed, logic integrated into GetExportColumns
    function GetExportColumns: TList<TColumn>;
    function FormatFieldValue(Field: TField): string; overload;
    function FormatFieldValue(Field: TField;
      AMachineReadable: Boolean): string; overload;
    function FormatNumberInvariant(Value: Double): string;
    function IsNumericValueText(const Value: string): Boolean;
    function IsNumericFieldType(AField: TField): Boolean;
    function JSONValueForField(AField: TField): string;
    function EscapeCSV(const Value: string): string;
    function NeutralizeFormulaInjection(const Value: string): string;
    function EscapeHTML(const Value: string): string;
    function EscapeXML(const Value: string): string;
    function EscapeJSON(const Value: string): string;
    procedure CheckProgress(Current, Total: Integer);
    function SanitizeXMLTagName(const TagName: string): string;
    procedure ExportToTextStream(Stream: TStream);
    procedure ExportToFileAtomic(const FileName: string; const ExportProc: TProc<TStream>);

    function BeginExportIteration(out AFilteredToggledOff: Boolean): TBookmark;
    procedure EndExportIteration(ABookmark: TBookmark; const AFilteredToggledOff: Boolean);
    function BuildFooterRow(const AColumns: TList<TColumn>): TArray<string>;

  public
    constructor Create(AGrid: TVittixDBGrid); reintroduce;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    // Main export methods
    procedure ExportToFile(const FileName: string; Format: TVittixExportFormat);
    procedure ExportToStream(Stream: TStream; Format: TVittixExportFormat);
    function ExportToString(Format: TVittixExportFormat): string;
    
    // Format-specific exports
    procedure ExportToCSV(const FileName: string);
    procedure ExportToTSV(const FileName: string);
    procedure ExportToExcel(const FileName: string);
    procedure ExportToHTML(const FileName: string);
    procedure ExportToXML(const FileName: string);
    procedure ExportToJSON(const FileName: string);
    procedure ExportToClipboard(Format: TVittixExportFormat = vefTSV);
    
    // Stream versions
    procedure ExportToCSVStream(Stream: TStream);
    procedure ExportToTSVStream(Stream: TStream);
    procedure ExportToHTMLStream(Stream: TStream);
    procedure ExportToXMLStream(Stream: TStream);
    procedure ExportToJSONStream(Stream: TStream);
    
    procedure Cancel;
    
    property Grid: TVittixDBGrid read FGrid;
    property Dataset: TDataSet read FDataset;
    property Options: TVittixExportOptions read FOptions write FOptions;
    property OnProgress: TVittixExportProgressEvent read FOnProgress write FOnProgress;
  end;

  // Helper class for Excel export
  TVittixExcelExporter = class
  private
    FExporter: TVittixDBGridExporter;
    function BuildSheetXML: string;
    function ColumnLetter(Index: Integer): string;
  public
    constructor Create(AExporter: TVittixDBGridExporter);
    procedure ExportToXLSX(Stream: TStream);
  end;

implementation

uses
  System.Math,
  System.IOUtils,
  System.DateUtils,
  System.StrUtils,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Dialogs,
  Vittix.DBGrid.Controller;

{ TVittixExportOptions }

constructor TVittixExportOptions.Create;
begin
  inherited;
  SetDefaults;
end;

procedure TVittixExportOptions.SetDefaults;
begin
  FExportVisibleOnly := True;
  FExportFilteredOnly := True;
  FIncludeHeaders := True;
  FIncludeFooter := False;
  FDateFormat := 'yyyy-mm-dd';
  FTimeFormat := 'hh:nn:ss';
  FDateTimeFormat := 'yyyy-mm-dd hh:nn:ss';
  FCurrencyFormat := '#,##0.00';
  FFloatFormat := '0.00';
  FBooleanAsText := True;
  FTrueText := 'Yes';
  FFalseText := 'No';
  FNullText := '';
  FDelimiter := ',';
  FQuoteChar := '"';
  FEncoding := TEncoding.UTF8;
end;

procedure TVittixExportOptions.Assign(Source: TPersistent);
var
  Src: TVittixExportOptions;
begin
  if Source is TVittixExportOptions then
  begin
    Src := TVittixExportOptions(Source);
    FExportVisibleOnly := Src.ExportVisibleOnly;
    FExportFilteredOnly := Src.ExportFilteredOnly;
    FIncludeHeaders := Src.IncludeHeaders;
    FIncludeFooter := Src.IncludeFooter;
    FDateFormat := Src.DateFormat;
    FTimeFormat := Src.TimeFormat;
    FDateTimeFormat := Src.DateTimeFormat;
    FCurrencyFormat := Src.CurrencyFormat;
    FFloatFormat := Src.FloatFormat;
    FBooleanAsText := Src.BooleanAsText;
    FTrueText := Src.TrueText;
    FFalseText := Src.FalseText;
    FNullText := Src.NullText;
    FDelimiter := Src.Delimiter;
    FEncoding := Src.FEncoding;   // Added FEncoding
    FQuoteChar := Src.QuoteChar;
  end
  else
    inherited;
end;

{ TVittixDBGridExporter }

constructor TVittixDBGridExporter.Create(AGrid: TVittixDBGrid);
begin
  inherited Create(nil);
  FGrid := AGrid;
  FDataset := nil;
  
  if Assigned(FGrid) and Assigned(FGrid.DataSource) then
    FDataset := FGrid.DataSource.DataSet;
    
  FOptions := TVittixExportOptions.Create;
  FCancelled := False;

  // The exporter only holds raw references; ask both components to signal
  // their destruction so the pointers are cleared instead of dangling
  // (a dataset on a data module outlives no one's assumptions).
  if Assigned(FGrid) then
    FGrid.FreeNotification(Self);
  if Assigned(FDataset) then
    FDataset.FreeNotification(Self);
end;

destructor TVittixDBGridExporter.Destroy;
begin
  FOptions.Free;
  inherited;
end;

procedure TVittixDBGridExporter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FDataset then
      FDataset := nil
    else if AComponent = FGrid then
    begin
      FGrid := nil;
      FDataset := nil;
    end;
  end;
end;

function TVittixDBGridExporter.GetExportColumns: TList<TColumn>;
var
  I: Integer;
begin
  Result := TList<TColumn>.Create;
  
  if not Assigned(FGrid) then
    Exit;
    
  if FOptions.ExportVisibleOnly then
  begin
    for I := 0 to FGrid.Columns.Count - 1 do
    begin
      if FGrid.Columns[I].Visible then
        Result.Add(FGrid.Columns[I]);
    end;
  end
  else
  begin
    for I := 0 to FGrid.Columns.Count - 1 do
      Result.Add(FGrid.Columns[I]);
  end;
end;

function TVittixDBGridExporter.FormatFieldValue(Field: TField): string;
begin
  Result := FormatFieldValue(Field, False);
end;

function TVittixDBGridExporter.FormatFieldValue(Field: TField;
  AMachineReadable: Boolean): string;
begin
  if Field.IsNull then
  begin
    Result := FOptions.NullText;
    Exit;
  end;

  case Field.DataType of
    ftString, ftWideString, ftMemo, ftWideMemo, ftFmtMemo:
      Result := Field.AsString;

    ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
      Result := Field.AsString;

    ftBoolean:
      if FOptions.BooleanAsText then
        Result := IfThen(Field.AsBoolean, FOptions.TrueText, FOptions.FalseText)
      else
        Result := Field.AsString;

    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      if AMachineReadable then
        Result := FormatNumberInvariant(Field.AsFloat)
      else if Field.DataType = ftCurrency then
        Result := FormatFloat(FOptions.CurrencyFormat, Field.AsFloat)
      else
        Result := FormatFloat(FOptions.FloatFormat, Field.AsFloat);

    ftDate:
      Result := FormatDateTime(FOptions.DateFormat, Field.AsDateTime);

    ftTime:
      Result := FormatDateTime(FOptions.TimeFormat, Field.AsDateTime);

    ftDateTime, ftTimeStamp:
      Result := FormatDateTime(FOptions.DateTimeFormat, Field.AsDateTime);

  else
    Result := Field.AsString;
  end;
end;

function TVittixDBGridExporter.FormatNumberInvariant(Value: Double): string;
begin
  // Full precision (the default '0.00' display format silently rounds
  // 1234.5678 down to 1234.57) with '.' as decimal separator regardless of
  // the machine locale, so machine-readable output stays parseable.
  Result := FloatToStrF(Value, ffGeneral, 15, 0, TFormatSettings.Invariant);
end;

function TVittixDBGridExporter.IsNumericValueText(const Value: string): Boolean;
var
  V: Extended;
begin
  // Locale parse first, then invariant, so "1234.5" counts as numeric even
  // on machines whose decimal separator is ','.
  Result := TryStrToFloat(Trim(Value), V);
  if not Result then
    Result := TryStrToFloat(Trim(Value), V, TFormatSettings.Invariant);
end;

function TVittixDBGridExporter.IsNumericFieldType(AField: TField): Boolean;
begin
  Result := AField.DataType in [ftSmallint, ftInteger, ftWord, ftLongWord,
    ftAutoInc, ftLargeint, ftShortint, ftByte, ftSingle, ftFloat,
    ftCurrency, ftBCD, ftFMTBcd, ftExtended];
end;

function TVittixDBGridExporter.EscapeCSV(const Value: string): string;
var
  NeedsQuotes: Boolean;
begin
  // Formula-injection neutralization is only for text Excel could evaluate.
  // Values that parse as numbers (-5, -12.50, +441234567890) are genuine
  // numeric data and must survive export unchanged so they paste back into
  // Excel as numbers instead of text.
  if IsNumericValueText(Value) then
    Result := Value
  else
    Result := NeutralizeFormulaInjection(Value);

  NeedsQuotes := (Pos(FOptions.Delimiter, Value) > 0) or
                 (Pos(FOptions.QuoteChar, Value) > 0) or
                 (Pos(#13, Result) > 0) or
                 (Pos(#10, Result) > 0);

  if NeedsQuotes then
  begin
    Result := StringReplace(Result, FOptions.QuoteChar,
      FOptions.QuoteChar + FOptions.QuoteChar, [rfReplaceAll]);
    Result := FOptions.QuoteChar + Result + FOptions.QuoteChar;
  end
end;

function TVittixDBGridExporter.NeutralizeFormulaInjection(const Value: string): string;
begin
  Result := Value;
  if Result = '' then
    Exit;

  case Result[1] of
    '=', '+', '-', '@':
      Result := '''' + Result;
  end;
end;

function TVittixDBGridExporter.EscapeHTML(const Value: string): string;
begin
  Result := StringReplace(Value, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
end;

function TVittixDBGridExporter.EscapeXML(const Value: string): string;
var
  SB: TStringBuilder;
  Ch: Char;
begin
  SB := TStringBuilder.Create(Length(Value) + 8);
  try
    for Ch in Value do
    begin
      // Characters illegal in XML 1.0 (0x00-0x08, 0x0B, 0x0C, 0x0E-0x1F)
      // make Excel reject the file; drop them. Tab, CR and LF are legal.
      if (Ord(Ch) < $20) and not (Ord(Ch) in [9, 10, 13]) then
        Continue;

      case Ch of
        '&': SB.Append('&amp;');
        '<': SB.Append('&lt;');
        '>': SB.Append('&gt;');
        '"': SB.Append('&quot;');
        '''': SB.Append('&apos;');
      else
        SB.Append(Ch);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TVittixDBGridExporter.EscapeJSON(const Value: string): string;
var
  SB: TStringBuilder;
  Ch: Char;
begin
  SB := TStringBuilder.Create(Length(Value) + 8);
  try
    for Ch in Value do
    begin
      case Ch of
        '\': SB.Append('\\');
        '"': SB.Append('\"');
        #8:  SB.Append('\b');
        #9:  SB.Append('\t');
        #10: SB.Append('\n');
        #12: SB.Append('\f');
        #13: SB.Append('\r');
      else
        // Escape the remaining control characters (0x00-0x1F): raw control
        // bytes produce invalid JSON that most parsers reject.
        if Ord(Ch) < $20 then
        begin
          SB.Append('\u');
          SB.Append(IntToHex(Ord(Ch), 4));
        end
        else
          SB.Append(Ch);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TVittixDBGridExporter.SanitizeXMLTagName(const TagName: string): string;
var
  SB: TStringBuilder;
  Ch: Char;
begin
  SB := TStringBuilder.Create(Length(TagName) + 1);
  try
    for Ch in TagName do
    begin
      if CharInSet(Ch, ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.']) then
        SB.Append(Ch)
      else
        SB.Append('_');
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;

  // XML names must not start with a digit, '-' or '.'.
  if (Length(Result) > 0) and CharInSet(Result[1], ['0'..'9', '-', '.']) then
    Result := '_' + Result;
  if Result = '' then
    Result := '_field';
end;

procedure TVittixDBGridExporter.CheckProgress(Current, Total: Integer);
var
  Cancel: Boolean;
begin
  if Assigned(FOnProgress) then
  begin
    Cancel := False;
    FOnProgress(Self, Current, Total, Cancel);
    if Cancel then
      FCancelled := True;
  end;
  
  Application.ProcessMessages;
end;

procedure TVittixDBGridExporter.Cancel;
begin
  FCancelled := True;
end;

{ Export to File }

procedure TVittixDBGridExporter.ExportToFile(const FileName: string; 
  Format: TVittixExportFormat);
begin
  ExportToFileAtomic(
    FileName,
    procedure(Stream: TStream)
    begin
      ExportToStream(Stream, Format);
    end);
end;

procedure TVittixDBGridExporter.ExportToFileAtomic(const FileName: string;
  const ExportProc: TProc<TStream>);
var
  TempFileName: string;
  BackupFileName: string;
  TempStream: TFileStream;
begin
  // Every entry point must reset the cancel flag here: the file-format
  // methods (ExportToCSV, ExportToHTML, ...) come straight through this
  // helper, so a cancelled export must not poison the next one.
  FCancelled := False;

  // Stage the temp file next to the target so the final Move stays on one
  // volume (an atomic rename rather than a slow cross-volume copy).
  TempFileName := TPath.Combine(TPath.GetDirectoryName(FileName),
    '~vittix-' + TPath.GetGUIDFileName(False) + '.tmp');
  BackupFileName := TempFileName + '.bak';
  try
    TempStream := TFileStream.Create(TempFileName, fmCreate);
    try
      ExportProc(TempStream);
      if FCancelled then
        raise EAbort.Create('Export cancelled');
    finally
      TempStream.Free;
    end;

    // TFile.Replace swaps the staged file over the existing target in one
    // operation: if the target is locked the original file survives, which
    // a delete-then-move sequence cannot guarantee. A backup name must be
    // supplied (an empty one raises before the swap), and is removed again
    // once the swap succeeded.
    if TFile.Exists(FileName) then
    begin
      TFile.Replace(TempFileName, FileName, BackupFileName);
      if TFile.Exists(BackupFileName) then
        try TFile.Delete(BackupFileName) except end;
    end
    else
      TFile.Move(TempFileName, FileName);
  except
    // Never leave the staged temp file behind, cancellation included.
    on E: Exception do
    begin
      if TFile.Exists(TempFileName) then
        try TFile.Delete(TempFileName) except end;
      raise;
    end;
  end;
end;

procedure TVittixDBGridExporter.ExportToStream(Stream: TStream; 
  Format: TVittixExportFormat);
var
  ExcelExporter: TVittixExcelExporter;
begin
  FCancelled := False;
  
  case Format of
    vefCSV:        ExportToCSVStream(Stream);
    vefTSV:        ExportToTSVStream(Stream);
    vefText:       ExportToTextStream(Stream);
    vefHTML:       ExportToHTMLStream(Stream);
    vefXML:        ExportToXMLStream(Stream);
    vefJSON:       ExportToJSONStream(Stream);

    vefExcelXLSX:
    begin
      ExcelExporter := TVittixExcelExporter.Create(Self);
      try
        ExcelExporter.ExportToXLSX(Stream);
      finally ExcelExporter.Free; end;
    end;
    vefPDF:        raise EVittixExportError.Create('PDF export requires a reporting component and is not implemented in the core engine.');
  else
    raise EVittixExportError.Create('Unsupported export format');
  end;
end;

function TVittixDBGridExporter.ExportToString(Format: TVittixExportFormat): string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    ExportToStream(Stream, Format);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

{ Export iteration and footer helpers }

function TVittixDBGridExporter.BeginExportIteration(
  out AFilteredToggledOff: Boolean): TBookmark;
begin
  Result := nil;
  AFilteredToggledOff := False;
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;
  // Always preserve the current dataset position across the export.
  Result := FDataSet.GetBookMark;
  // When ExportFilteredOnly is False but the dataset is currently filtered
  // (controller filter active), export every record by temporarily disabling
  // filtering. The fixed DataLinkDataSetChanged no longer tears down engines
  // for same-dataset Filtered changes, so this is safe.
  AFilteredToggledOff := (not FOptions.ExportFilteredOnly) and FDataSet.Filtered;
  if AFilteredToggledOff then
    FDataSet.Filtered := False;
end;

procedure TVittixDBGridExporter.EndExportIteration(
  ABookmark: TBookmark; const AFilteredToggledOff: Boolean);
begin
  if not Assigned(FDataSet) or not Assigned(ABookmark) then Exit;
  // Re-enable filtering first: the bookmark was captured in the filtered
  // view and re-filtering resets the cursor, so the position must be
  // restored only after the dataset is back in its original state.
  if AFilteredToggledOff then
    FDataSet.Filtered := True;
  try
    FDataSet.GotoBookMark(ABookmark);
  except
    try
      FDataSet.First;
    except
    end;
  end;
  FDataSet.FreeBookmark(ABookmark);
end;

function TVittixDBGridExporter.BuildFooterRow(
  const AColumns: TList<TColumn>): TArray<string>;
var
  I: Integer;
  Col: TColumn;
  Ctrl: TVittixDBGridController;
begin
  SetLength(Result, AColumns.Count);
  for I := 0 to AColumns.Count - 1 do
  begin
    Col := AColumns[I];
    if not Assigned(Col) or (Col.FieldName = '') then
      Continue;
    Result[I] := '';
    if not Assigned(FGrid) then Continue;
    Ctrl := FGrid.Controller;
    if Assigned(Ctrl) then
      Result[I] := Ctrl.FooterDisplayText(Col.FieldName);
  end;
end;

{ CSV Export }

procedure TVittixDBGridExporter.ExportToCSV(const FileName: string);
begin
  ExportToFileAtomic(
    FileName,
    procedure(Stream: TStream)
    begin
      ExportToCSVStream(Stream);
    end);
end;

{
  TVittixDBGridExporter.ExportToCSVStream
}
procedure TVittixDBGridExporter.ExportToCSVStream(Stream: TStream);
var
  Writer: TStreamWriter;
  Columns: TList<TColumn>;
  Line: string;
  I, RowCount: Integer;
  Col: TColumn;
  FooterRow: TArray<string>;
  Bookmark: TBookmark;
  FilteredToggledOff: Boolean;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise EVittixExportError.Create('Dataset is not active');

  // Direct stream calls bypass ExportToFileAtomic, so reset the flag here
  // as well: one cancelled export must not poison the next.
  FCancelled := False;

  Writer := TStreamWriter.Create(Stream, FOptions.Encoding);
  try
    Columns := GetExportColumns;
    try
      // Write header
      if FOptions.IncludeHeaders then
      begin
        Line := '';
        for I := 0 to Columns.Count - 1 do
        begin
          if I > 0 then
            Line := Line + FOptions.Delimiter;
          Line := Line + EscapeCSV(Columns[I].Title.Caption);
        end;
        Writer.WriteLine(Line);
      end;

      // Prepare iteration: preserve position; when ExportFilteredOnly is False
      // and the dataset is filtered, export all records.
      Bookmark := BeginExportIteration(FilteredToggledOff);

      // Write data
      FDataset.DisableControls;
      try
        FDataset.First;
        RowCount := 0;

        while not FDataset.Eof do
        begin
          if FCancelled then
            Break;

          Line := '';
          for I := 0 to Columns.Count - 1 do
          begin
            if I > 0 then
              Line := Line + FOptions.Delimiter;

            Col := Columns[I];
            if Assigned(Col.Field) then
              Line := Line + EscapeCSV(FormatFieldValue(Col.Field, True));
          end;

          Writer.WriteLine(Line);

          Inc(RowCount);
          if RowCount mod 100 = 0 then
            CheckProgress(RowCount, FDataset.RecordCount);

          FDataset.Next;
        end;
      finally
        EndExportIteration(Bookmark, FilteredToggledOff);
        FDataset.EnableControls;
      end;

      // Footer row
      if FOptions.IncludeFooter then
      begin
        FooterRow := BuildFooterRow(Columns);
        Line := '';
        for I := 0 to Columns.Count - 1 do
        begin
          if I > 0 then
            Line := Line + FOptions.Delimiter;
          Line := Line + EscapeCSV(FooterRow[I]);
        end;
        Writer.WriteLine(Line);
      end;

    finally
      Columns.Free;
    end;
  finally
    Writer.Free;
  end;
end;

{ TSV Export }

procedure TVittixDBGridExporter.ExportToTSV(const FileName: string);
var
  OldDelimiter: Char;
begin
  OldDelimiter := FOptions.Delimiter;
  try
    FOptions.Delimiter := #9; // Tab
    ExportToCSV(FileName); // This will call ExportToCSVStream internally
  finally
    FOptions.Delimiter := OldDelimiter;
  end;
end;

procedure TVittixDBGridExporter.ExportToTextStream(Stream: TStream);
var
  Writer: TStreamWriter;
  Columns: TList<TColumn>;
  Line: string;
  I, J, RowCount: Integer;
  Col: TColumn;
  MaxLengths: TArray<Integer>;
  // Single traversal: rows are formatted in the first pass so no second
  // dataset scan is needed to compute column widths (server-side datasets
  // make a second First/Next sweep expensive).
  RowCache: TArray<TArray<string>>;
  RowData: TArray<string>;
  FooterRow: TArray<string>;
  Bookmark: TBookmark;
  FilteredToggledOff: Boolean;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise EVittixExportError.Create('Dataset is not active');

  FCancelled := False;

  Writer := TStreamWriter.Create(Stream, FOptions.Encoding);
  try
    Columns := GetExportColumns;
    try
      SetLength(MaxLengths, Columns.Count);
      for I := 0 to Columns.Count - 1 do
        MaxLengths[I] := Length(Columns[I].Title.Caption);

      SetLength(RowCache, 0);

      // Prepare iteration: preserve position; when ExportFilteredOnly is False
      // and the dataset is filtered, export all records.
      Bookmark := BeginExportIteration(FilteredToggledOff);

      // Single pass: collect all data AND compute max widths simultaneously
      FDataset.DisableControls;
      try
        FDataset.First;
        RowCount := 0;

        while not FDataset.Eof do
        begin
          if FCancelled then Break;

          SetLength(RowData, Columns.Count);
          for I := 0 to Columns.Count - 1 do
          begin
            Col := Columns[I];
            if Assigned(Col.Field) then
              // A fixed-width row cannot carry line breaks: flatten any
              // embedded CR/LF so one record stays one output line.
              RowData[I] := StringReplace(
                StringReplace(FormatFieldValue(Col.Field), sLineBreak, ' ', [rfReplaceAll]),
                #10, ' ', [rfReplaceAll])
            else
              RowData[I] := '';

            if Length(RowData[I]) > MaxLengths[I] then
              MaxLengths[I] := Length(RowData[I]);
          end;

          SetLength(RowCache, Length(RowCache) + 1);
          RowCache[High(RowCache)] := RowData;

          Inc(RowCount);
          if RowCount mod 100 = 0 then
            CheckProgress(RowCount, FDataset.RecordCount);

          FDataset.Next;
        end;
      finally
        EndExportIteration(Bookmark, FilteredToggledOff);
        FDataset.EnableControls;
      end;

      // Write header from in-memory widths (no dataset access needed)
      if FOptions.IncludeHeaders then
      begin
        Line := '';
        for I := 0 to Columns.Count - 1 do
          Line := Line + Format('%-*s', [MaxLengths[I] + 1, Columns[I].Title.Caption]);
        Writer.WriteLine(Line);
        // Separator line
        Line := '';
        for I := 0 to Columns.Count - 1 do
          Line := Line + StringOfChar('-', MaxLengths[I]) + ' ';
        Writer.WriteLine(Line);
      end;

      // Write data from cache — no second dataset traversal
      for J := 0 to High(RowCache) do
      begin
        if FCancelled then Break;
        Line := '';
        for I := 0 to Columns.Count - 1 do
          Line := Line + Format('%-*s', [MaxLengths[I] + 1, RowCache[J][I]]);
        Writer.WriteLine(Line);
      end;

      // Footer row (aggregation / footer text), aligned like the data rows
      if FOptions.IncludeFooter then
      begin
        FooterRow := BuildFooterRow(Columns);
        Line := '';
        for I := 0 to Columns.Count - 1 do
          Line := Line + Format('%-*s', [MaxLengths[I] + 1, FooterRow[I]]);
        Writer.WriteLine(Line);
      end;

    finally
      Columns.Free;
    end;
  finally
    Writer.Free;
  end;
end;

procedure TVittixDBGridExporter.ExportToTSVStream(Stream: TStream);
var
  OldDelimiter: Char;
begin
  OldDelimiter := FOptions.Delimiter;
  try
    FOptions.Delimiter := #9; // Tab
    ExportToCSVStream(Stream);
  finally
    FOptions.Delimiter := OldDelimiter;
  end;
end;

{ HTML Export }

procedure TVittixDBGridExporter.ExportToHTML(const FileName: string);
begin
  ExportToFileAtomic(
    FileName,
    procedure(Stream: TStream)
    begin
      ExportToHTMLStream(Stream);
    end);
end;

procedure TVittixDBGridExporter.ExportToHTMLStream(Stream: TStream);
var
  Writer: TStreamWriter;
  Columns: TList<TColumn>;
  I, RowCount: Integer;
  Col: TColumn;
  FooterRow: TArray<string>;
  Bookmark: TBookmark;
  FilteredToggledOff: Boolean;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise EVittixExportError.Create('Dataset is not active');

  FCancelled := False;

  Writer := TStreamWriter.Create(Stream, TEncoding.UTF8);
  try
    Columns := GetExportColumns;
    try
      // HTML header
      Writer.WriteLine('<!DOCTYPE html>');
      Writer.WriteLine('<html>');
      Writer.WriteLine('<head>');
      Writer.WriteLine('<meta charset="UTF-8">');
      Writer.WriteLine('<title>Exported Data</title>');
      Writer.WriteLine('<style>');
      Writer.WriteLine('table { border-collapse: collapse; width: 100%; font-family: Arial, sans-serif; }');
      Writer.WriteLine('th { background-color: #4CAF50; color: white; padding: 8px; text-align: left; border: 1px solid #ddd; }');
      Writer.WriteLine('td { padding: 8px; border: 1px solid #ddd; }');
      Writer.WriteLine('tr:nth-child(even) { background-color: #f2f2f2; }');
      Writer.WriteLine('tr:hover { background-color: #ddd; }');
      Writer.WriteLine('tr.footer-row td { font-weight: bold; background-color: #d9e1f2; }');
      Writer.WriteLine('</style>');
      Writer.WriteLine('</head>');
      Writer.WriteLine('<body>');
      Writer.WriteLine('<table>');

      // Table header
      if FOptions.IncludeHeaders then
      begin
        Writer.Write('<thead><tr>');
        for I := 0 to Columns.Count - 1 do
        begin
          Writer.Write('<th>');
          Writer.Write(EscapeHTML(Columns[I].Title.Caption));
          Writer.Write('</th>');
        end;
        Writer.WriteLine('</tr></thead>');
      end;

      // Table body
      Writer.WriteLine('<tbody>');

      // Prepare iteration: preserve position; when ExportFilteredOnly is False
      // and the dataset is filtered, export all records.
      Bookmark := BeginExportIteration(FilteredToggledOff);

      FDataset.DisableControls;
      try
        FDataset.First;
        RowCount := 0;

        while not FDataset.Eof do
        begin
          if FCancelled then
            Break;

          Writer.Write('<tr>');

          for I := 0 to Columns.Count - 1 do
          begin
            Writer.Write('<td>');
            Col := Columns[I];
            if Assigned(Col.Field) then
              Writer.Write(EscapeHTML(FormatFieldValue(Col.Field)));
            Writer.Write('</td>');
          end;

          Writer.WriteLine('</tr>');

          Inc(RowCount);
          if RowCount mod 100 = 0 then
            CheckProgress(RowCount, FDataset.RecordCount);

          FDataset.Next;
        end;
      finally
        EndExportIteration(Bookmark, FilteredToggledOff);
        FDataset.EnableControls;
      end;

      // Footer row (aggregation / footer text)
      if FOptions.IncludeFooter then
      begin
        FooterRow := BuildFooterRow(Columns);
        Writer.Write('<tr class="footer-row">');
        for I := 0 to Columns.Count - 1 do
        begin
          Writer.Write('<td>');
          Writer.Write(EscapeHTML(FooterRow[I]));
          Writer.Write('</td>');
        end;
        Writer.WriteLine('</tr>');
      end;

      Writer.WriteLine('</tbody>');
      Writer.WriteLine('</table>');
      Writer.WriteLine('</body>');
      Writer.WriteLine('</html>');

    finally
      Columns.Free;
    end;
  finally
    Writer.Free;
  end;
end;

{ XML Export }

procedure TVittixDBGridExporter.ExportToXML(const FileName: string);
begin
  ExportToFileAtomic(
    FileName,
    procedure(Stream: TStream)
    begin
      ExportToXMLStream(Stream);
    end);
end;

procedure TVittixDBGridExporter.ExportToXMLStream(Stream: TStream);
var
  Writer: TStreamWriter;
  Columns: TList<TColumn>;
  I, RowCount: Integer;
  Col: TColumn;
  FieldName: string;
  FooterRow: TArray<string>;
  Bookmark: TBookmark;
  FilteredToggledOff: Boolean;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise EVittixExportError.Create('Dataset is not active');

  FCancelled := False;

  Writer := TStreamWriter.Create(Stream, TEncoding.UTF8);
  try
    Columns := GetExportColumns;
    try
      Writer.WriteLine('<?xml version="1.0" encoding="UTF-8"?>');
      Writer.WriteLine('<data>');

      // Prepare iteration: preserve position; when ExportFilteredOnly is False
      // and the dataset is filtered, export all records.
      Bookmark := BeginExportIteration(FilteredToggledOff);

      FDataset.DisableControls;
      try
        FDataset.First;
        RowCount := 0;

        while not FDataset.Eof do
        begin
          if FCancelled then
            Break;

          Writer.WriteLine('  <row>');

          for I := 0 to Columns.Count - 1 do
          begin
            Col := Columns[I];
            if Assigned(Col.Field) then
            begin
              FieldName := SanitizeXMLTagName(Col.Field.FieldName);
              Writer.Write('    <'); // Indent for readability
              Writer.Write(FieldName);
              Writer.Write('>');
              Writer.Write(EscapeXML(FormatFieldValue(Col.Field, True)));
              Writer.Write('</');
              Writer.Write(FieldName);
              Writer.WriteLine('>');
            end;
          end;

          Writer.WriteLine('  </row>');

          Inc(RowCount);
          if RowCount mod 100 = 0 then
            CheckProgress(RowCount, FDataset.RecordCount);

          FDataset.Next;
        end;
      finally
        EndExportIteration(Bookmark, FilteredToggledOff);
        FDataset.EnableControls;
      end;

      // Footer row (aggregation / footer text)
      if FOptions.IncludeFooter then
      begin
        FooterRow := BuildFooterRow(Columns);
        Writer.WriteLine('  <footer>');
        for I := 0 to Columns.Count - 1 do
        begin
          Col := Columns[I];
          if not Assigned(Col) or (Col.FieldName = '') then
            Continue;
          if Assigned(Col.Field) then
            FieldName := SanitizeXMLTagName(Col.Field.FieldName)
          else
            FieldName := SanitizeXMLTagName(Col.FieldName);
          Writer.Write('    <');
          Writer.Write(FieldName);
          Writer.Write('>');
          Writer.Write(EscapeXML(FooterRow[I]));
          Writer.Write('</');
          Writer.Write(FieldName);
          Writer.WriteLine('>');
        end;
        Writer.WriteLine('  </footer>');
      end;

      Writer.WriteLine('</data>');

    finally
      Columns.Free;
    end;
  finally
    Writer.Free;
  end;
end;

{ JSON Export }

procedure TVittixDBGridExporter.ExportToJSON(const FileName: string);
begin
  ExportToFileAtomic(
    FileName,
    procedure(Stream: TStream)
    begin
      ExportToJSONStream(Stream);
    end);
end;

function TVittixDBGridExporter.JSONValueForField(AField: TField): string;
begin
  if AField.IsNull then
    Exit('null');

  case AField.DataType of
    ftBoolean:
      if AField.AsBoolean then
        Result := 'true'
      else
        Result := 'false';

    ftSmallint, ftInteger, ftWord, ftLongWord, ftAutoInc, ftLargeint,
    ftShortint, ftByte:
      Result := AField.AsString;

    ftSingle, ftFloat, ftCurrency, ftBCD, ftFMTBcd, ftExtended:
      Result := FormatNumberInvariant(AField.AsFloat);
  else
    Result := '"' + EscapeJSON(FormatFieldValue(AField, True)) + '"';
  end;
end;

procedure TVittixDBGridExporter.ExportToJSONStream(Stream: TStream);
var
  Writer: TStreamWriter;
  Columns: TList<TColumn>;
  I, RowCount: Integer;
  Col: TColumn;
  FirstRow, FirstCol: Boolean;
  FooterRow: TArray<string>;
  Bookmark: TBookmark;
  FilteredToggledOff: Boolean;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise EVittixExportError.Create('Dataset is not active');

  FCancelled := False;

  Writer := TStreamWriter.Create(Stream, TEncoding.UTF8);
  try
    Columns := GetExportColumns;
    try
      Writer.WriteLine('[');

      // Prepare iteration: preserve position; when ExportFilteredOnly is False
      // and the dataset is filtered, export all records.
      Bookmark := BeginExportIteration(FilteredToggledOff);

      FDataset.DisableControls;
      try
        FDataset.First;
        RowCount := 0;
        FirstRow := True;

        while not FDataset.Eof do
        begin
          if FCancelled then
            Break;

          if not FirstRow then
            Writer.WriteLine(',');
          FirstRow := False;

          Writer.Write('  {');

          FirstCol := True;
          for I := 0 to Columns.Count - 1 do
          begin
            Col := Columns[I];
            if Assigned(Col.Field) then
            begin
              if not FirstCol then
                Writer.Write(', ');
              FirstCol := False;

              Writer.Write('"');
              Writer.Write(EscapeJSON(Col.Field.FieldName));
              Writer.Write('": ');
              Writer.Write(JSONValueForField(Col.Field));
            end;
          end;

          Writer.Write('}');

          Inc(RowCount);
          if RowCount mod 100 = 0 then
            CheckProgress(RowCount, FDataset.RecordCount);

          FDataset.Next;
        end;

        // Footer values are display text, so they stay strings. The
        // "__footer__" key marks the trailing object so consumers iterating
        // data rows never mistake it for a record.
        if FOptions.IncludeFooter then
        begin
          FooterRow := BuildFooterRow(Columns);
          if not FirstRow then
            Writer.WriteLine(',');
          Writer.Write('  {"__footer__": {');

          FirstCol := True;
          for I := 0 to Columns.Count - 1 do
          begin
            Col := Columns[I];
            if not Assigned(Col) or (Col.FieldName = '') then
              Continue;
            if not FirstCol then
              Writer.Write(', ');
            FirstCol := False;

            Writer.Write('"');
            if Assigned(Col.Field) then
              Writer.Write(EscapeJSON(Col.Field.FieldName))
            else
              Writer.Write(EscapeJSON(Col.FieldName));
            Writer.Write('": "');
            Writer.Write(EscapeJSON(FooterRow[I]));
            Writer.Write('"');
          end;

          Writer.Write('}}');
        end;

        Writer.WriteLine;
      finally
        EndExportIteration(Bookmark, FilteredToggledOff);
        FDataset.EnableControls;
      end;

      Writer.WriteLine(']');

    finally
      Columns.Free;
    end;
  finally
    Writer.Free;
  end;
end;

{ Excel Export }

procedure TVittixDBGridExporter.ExportToExcel(const FileName: string);
begin
  if not SameText(TPath.GetExtension(FileName), '.xlsx') then
    raise EVittixExportError.Create('Only .xlsx export is supported');

  ExportToFileAtomic(
    FileName,
    procedure(Stream: TStream)
    var
      ExcelExporter: TVittixExcelExporter;
    begin
      ExcelExporter := TVittixExcelExporter.Create(Self);
      try
        ExcelExporter.ExportToXLSX(Stream);
      finally
        ExcelExporter.Free;
      end;
    end);
end;

{ Clipboard Export }

procedure TVittixDBGridExporter.ExportToClipboard(Format: TVittixExportFormat);
var
  Data: string;
begin
  Data := ExportToString(Format);
  Clipboard.AsText := Data;
end;

{ TVittixExcelExporter }

constructor TVittixExcelExporter.Create(AExporter: TVittixDBGridExporter);
begin
  inherited Create;
  FExporter := AExporter;
end;

function TVittixExcelExporter.ColumnLetter(Index: Integer): string;
begin
  Result := '';
  Inc(Index);

  while Index > 0 do
  begin
    Result := Chr(Ord('A') + ((Index - 1) mod 26)) + Result;
    Index := (Index - 1) div 26;
  end;
end;

function TVittixExcelExporter.BuildSheetXML: string;
  var
    XML: TStringList;
    Columns: TList<TColumn>;
    I, Row, RowCount: Integer;
    Col: TColumn;
    Value: string;
    FooterRow: TArray<string>;
    Bookmark: TBookmark;
    FilteredToggledOff: Boolean;
  begin
    if not Assigned(FExporter.Dataset) or not FExporter.Dataset.Active then
      raise EVittixExportError.Create('Dataset is not active');

    XML := TStringList.Create;
    try
      XML.Add('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
      XML.Add('<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">');
      XML.Add('<sheetData>');

      Columns := FExporter.GetExportColumns;
      try
        Row := 1;

        if FExporter.Options.IncludeHeaders then
        begin
          XML.Add(Format('<row r="%d">', [Row]));
          for I := 0 to Columns.Count - 1 do
          begin
            XML.Add(Format('<c r="%s%d" t="inlineStr">', [
              ColumnLetter(I), Row
            ]));
            XML.Add('<is><t xml:space="preserve">' +
              FExporter.EscapeXML(Columns[I].Title.Caption) + '</t></is>');
            XML.Add('</c>');
          end;
          XML.Add('</row>');
          Inc(Row);
        end;

        // Data rows
        // Prepare iteration: preserve position; when ExportFilteredOnly is False
        // and the dataset is filtered, export all records.
        Bookmark := FExporter.BeginExportIteration(FilteredToggledOff);

        FExporter.Dataset.DisableControls;
        try
          FExporter.Dataset.First;
          RowCount := 0;

          while not FExporter.Dataset.Eof do
          begin
            if FExporter.FCancelled then
              Break;

            XML.Add(Format('<row r="%d">', [Row]));

            for I := 0 to Columns.Count - 1 do
            begin
              Col := Columns[I];
              if Assigned(Col.Field) then
              begin
                if FExporter.IsNumericFieldType(Col.Field) and not Col.Field.IsNull then
                begin
                  // Numbers as real numeric cells so Excel computes on them
                  // instead of treating every value as text.
                  XML.Add(Format('<c r="%s%d"><v>%s</v></c>', [
                    ColumnLetter(I), Row,
                    FExporter.FormatNumberInvariant(Col.Field.AsFloat)
                  ]));
                end
                else if (Col.Field.DataType = ftBoolean) and not Col.Field.IsNull then
                begin
                  XML.Add(Format('<c r="%s%d" t="b"><v>%d</v></c>', [
                    ColumnLetter(I), Row, Ord(Col.Field.AsBoolean)
                  ]));
                end
                else
                begin
                  Value := FExporter.FormatFieldValue(Col.Field, True);
                  XML.Add(Format('<c r="%s%d" t="inlineStr">', [
                    ColumnLetter(I), Row
                  ]));
                  XML.Add('<is><t xml:space="preserve">' +
                    FExporter.EscapeXML(Value) + '</t></is>');
                  XML.Add('</c>');
                end;
              end;
            end;

            XML.Add('</row>');

            Inc(Row);
            Inc(RowCount);
            if RowCount mod 100 = 0 then
              FExporter.CheckProgress(RowCount, FExporter.Dataset.RecordCount);

            FExporter.Dataset.Next;
          end;
        finally
          FExporter.EndExportIteration(Bookmark, FilteredToggledOff);
          FExporter.Dataset.EnableControls;
        end;

        // Footer row
        if FExporter.Options.IncludeFooter then
        begin
          FooterRow := FExporter.BuildFooterRow(Columns);
          XML.Add(Format('<row r="%d">', [Row]));
          for I := 0 to Columns.Count - 1 do
          begin
            XML.Add(Format('<c r="%s%d" t="inlineStr">', [
              ColumnLetter(I), Row
            ]));
            XML.Add('<is><t xml:space="preserve">' +
              FExporter.EscapeXML(FooterRow[I]) + '</t></is>');
            XML.Add('</c>');
          end;
          XML.Add('</row>');
        end;

      finally
        Columns.Free;
      end;

      XML.Add('</sheetData>');
      XML.Add('</worksheet>');

      Result := XML.Text;
    finally
      XML.Free;
    end;
  end;

procedure TVittixExcelExporter.ExportToXLSX(Stream: TStream);
var
  Zip: TZipFile;
  TempStream: TMemoryStream;
  SheetXML: string;

  procedure AddEntry(const ZipPath, Content: string);
  var
    EntryStream: TMemoryStream;
    Bytes: TBytes;
  begin
    Bytes := TEncoding.UTF8.GetBytes(Content);
    EntryStream := TMemoryStream.Create;
    try
      if Length(Bytes) > 0 then
        EntryStream.WriteBuffer(Bytes[0], Length(Bytes));
      EntryStream.Position := 0;
      Zip.Add(EntryStream, ZipPath);
    finally
      EntryStream.Free;
    end;
  end;
begin
  if not Assigned(FExporter.Dataset) or not FExporter.Dataset.Active then
    raise EVittixExportError.Create('Dataset is not active');

  SheetXML := BuildSheetXML;
  TempStream := TMemoryStream.Create;
  Zip := TZipFile.Create;
  try
    Zip.Open(TempStream, zmWrite);
    AddEntry('[Content_Types].xml',
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">' +
      '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>' +
      '<Default Extension="xml" ContentType="application/xml"/>' +
      '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>' +
      '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>' +
      '</Types>');
    AddEntry('_rels/.rels',
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>' +
      '</Relationships>');
    AddEntry('xl/workbook.xml',
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ' +
      'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">' +
      '<sheets><sheet name="Export" sheetId="1" r:id="rId1"/></sheets>' +
      '</workbook>');
    AddEntry('xl/_rels/workbook.xml.rels',
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>' +
      '</Relationships>');
    AddEntry('xl/worksheets/sheet1.xml', SheetXML);
    Zip.Close;

    TempStream.Position := 0;
    Stream.CopyFrom(TempStream, TempStream.Size);
  finally
    Zip.Free;
    TempStream.Free;
  end;
end;

end.
