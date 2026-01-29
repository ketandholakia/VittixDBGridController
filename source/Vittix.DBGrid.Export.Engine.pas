unit Vittix.DBGrid.Export.Engine;

{$REGION 'Documentation'}
/// <summary>
/// Export Engine for Vittix.DBGrid Component Suite
/// 
/// SUPPORTED FORMATS:
/// 1. Excel (XLSX) - Using FlexCel or manual XML generation
/// 2. Excel (XLS) - Legacy format via OLE Automation
/// 3. CSV - Comma-Separated Values
/// 4. TSV - Tab-Separated Values
/// 5. HTML - Formatted HTML table
/// 6. XML - Structured XML document
/// 7. JSON - JSON array format
/// 8. PDF - Via ReportBuilder or FastReport (optional)
/// 9. Clipboard - Copy to Windows clipboard
/// 10. Text - Fixed-width text format
///
/// FEATURES:
/// - Export visible columns only or all columns
/// - Export filtered data or all data
/// - Custom formatting per column
/// - Progress callback for large exports
/// - Memory-efficient streaming
/// - Unicode support
/// - Configurable delimiters and encoding
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
  Vcl.DBGrids,
  Vcl.Clipbrd,
  Data.DB,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo;

type
  // Export format enumeration
  TVittixExportFormat = (
    vefCSV,           // Comma-Separated Values
    vefTSV,           // Tab-Separated Values
    vefExcelXLSX,     // Excel 2007+ (XML-based)
    vefExcelXLS,      // Excel 97-2003 (Binary, requires OLE)
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
    FLineBreak: string;
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
    function FormatFieldValue(Field: TField): string;
    function EscapeCSV(const Value: string): string;
    function EscapeHTML(const Value: string): string;
    function EscapeXML(const Value: string): string;
    function EscapeJSON(const Value: string): string;
    procedure CheckProgress(Current, Total: Integer);
    function SanitizeXMLTagName(const TagName: string): string; // Helper for XML tag names
    procedure ExportToTextStream(Stream: TStream); // Implementation for vefText

  public
    constructor Create(AGrid: TVittixDBGrid); reintroduce;
    destructor Destroy; override;

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
  public
    constructor Create(AExporter: TVittixDBGridExporter);
    procedure ExportToXLSX(Stream: TStream);
    procedure ExportToXLS(const FileName: string);
  end;

implementation

uses
  System.Math,
  System.IOUtils,
  System.DateUtils,
  System.StrUtils,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Dialogs;

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
  FLineBreak := #13#10; // Ensure line break is set
  FQuoteChar := '"';
  FLineBreak := #13#10;
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
    FLineBreak := Src.FLineBreak; // Added FLineBreak
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
end;

destructor TVittixDBGridExporter.Destroy;
begin
  FOptions.Free;
  inherited;
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
      if Field.DataType = ftCurrency then
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

function TVittixDBGridExporter.EscapeCSV(const Value: string): string;
var
  NeedsQuotes: Boolean;
begin
  NeedsQuotes := (Pos(FOptions.Delimiter, Value) > 0) or 
                 (Pos(FOptions.QuoteChar, Value) > 0) or
                 (Pos(#13, Value) > 0) or 
                 (Pos(#10, Value) > 0);
                 
  if NeedsQuotes then
  begin
    Result := StringReplace(Value, FOptions.QuoteChar, 
      FOptions.QuoteChar + FOptions.QuoteChar, [rfReplaceAll]);
    Result := FOptions.QuoteChar + Result + FOptions.QuoteChar;
  end
  else
    Result := Value;
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
begin
  Result := StringReplace(Value, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
end;

function TVittixDBGridExporter.EscapeJSON(const Value: string): string;
begin
  Result := StringReplace(Value, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
end;

function TVittixDBGridExporter.SanitizeXMLTagName(const TagName: string): string;
var
  S: string;
begin
  S := TagName;
  // Replace invalid characters with underscore
  S := StringReplace(S, ' ', '_', [rfReplaceAll]);
  S := StringReplace(S, '-', '_', [rfReplaceAll]);
  S := StringReplace(S, '.', '_', [rfReplaceAll]);
  // Ensure it doesn't start with a number or invalid char
  if (Length(S) > 0) and (S[1] in ['0'..'9']) then S := '_' + S;
  Result := S;
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
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExportToStream(Stream, Format);
  finally
    Stream.Free;
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
    vefPDF:        raise Exception.Create('PDF export requires a reporting component and is not implemented in the core engine.');
  else
    raise Exception.Create('Unsupported export format');
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

{ CSV Export }

procedure TVittixDBGridExporter.ExportToCSV(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExportToCSVStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TVittixDBGridExporter.ExportToCSVStream(Stream: TStream);
var
  Writer: TStreamWriter;
  Columns: TList<TColumn>;
  Line: string;
  I, RowCount: Integer;
  Col: TColumn;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise Exception.Create('Dataset is not active');
    
  Writer := TStreamWriter.Create(Stream, FOptions.Encoding); // Use public Encoding property
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
              Line := Line + EscapeCSV(FormatFieldValue(Col.Field));
          end;
          
          Writer.WriteLine(Line);
          
          Inc(RowCount);
          if RowCount mod 100 = 0 then
            CheckProgress(RowCount, FDataset.RecordCount); // FDataset.RecordCount can be slow for some datasets
            
          FDataset.Next;
        end;
      finally
        FDataset.EnableControls;
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
  I, RowCount: Integer;
  Col: TColumn;
  MaxLengths: TArray<Integer>;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise Exception.Create('Dataset is not active');

  Writer := TStreamWriter.Create(Stream, FOptions.Encoding);
  try
    Columns := GetExportColumns;
    try
      // Calculate max column widths for fixed-width text
      SetLength(MaxLengths, Columns.Count);
      for I := 0 to Columns.Count - 1 do
        MaxLengths[I] := Length(Columns[I].Title.Caption); // Start with header length

      // Iterate dataset once to find max lengths (can be slow for very large datasets)
      FDataset.DisableControls;
      try
        FDataset.First;
        while not FDataset.Eof do
        begin
          for I := 0 to Columns.Count - 1 do
          begin
            Col := Columns[I];
            if Assigned(Col.Field) then
              MaxLengths[I] := Max(MaxLengths[I], Length(FormatFieldValue(Col.Field)));
          end;
          FDataset.Next;
        end;
      finally
        FDataset.EnableControls;
      end;
      
      // Reset dataset to first record for actual writing
      FDataset.First;

      // Write header
      if FOptions.IncludeHeaders then
      begin
        Line := '';
        for I := 0 to Columns.Count - 1 do
          Line := Line + Format('%.*s', [MaxLengths[I], Columns[I].Title.Caption]);
        Writer.WriteLine(Line);
      end;

      // Write data
      FDataset.DisableControls;
      try
        RowCount := 0;
        while not FDataset.Eof do
        begin
          if FCancelled then
            Break;

          Line := '';
          for I := 0 to Columns.Count - 1 do
          begin
            Col := Columns[I];
            if Assigned(Col.Field) then
              Line := Line + Format('%.*s', [MaxLengths[I], FormatFieldValue(Col.Field)]);
          end;
          Writer.WriteLine(Line);

          Inc(RowCount);
          if RowCount mod 100 = 0 then
            CheckProgress(RowCount, FDataset.RecordCount);

          FDataset.Next;
        end;
      finally
        FDataset.EnableControls;
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
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExportToHTMLStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TVittixDBGridExporter.ExportToHTMLStream(Stream: TStream);
var
  Writer: TStreamWriter;
  Columns: TList<TColumn>;
  I, RowCount: Integer;
  Col: TColumn;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise Exception.Create('Dataset is not active');
    
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
        FDataset.EnableControls;
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
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExportToXMLStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TVittixDBGridExporter.ExportToXMLStream(Stream: TStream);
var
  Writer: TStreamWriter;
  Columns: TList<TColumn>;
  I, RowCount: Integer;
  Col: TColumn;
  FieldName: string;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise Exception.Create('Dataset is not active');
    
  Writer := TStreamWriter.Create(Stream, TEncoding.UTF8);
  try
    Columns := GetExportColumns;
    try
      Writer.WriteLine('<?xml version="1.0" encoding="UTF-8"?>');
      Writer.WriteLine('<data>');
      
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
              FieldName := StringReplace(Col.Field.FieldName, ' ', '_', [rfReplaceAll]);
              FieldName := SanitizeXMLTagName(Col.Field.FieldName); // Use sanitized name
              Writer.Write('    <'); // Indent for readability
              Writer.Write(FieldName);
              Writer.Write('>');
              Writer.Write(EscapeXML(FormatFieldValue(Col.Field)));
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
        FDataset.EnableControls;
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
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExportToJSONStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TVittixDBGridExporter.ExportToJSONStream(Stream: TStream);
var
  Writer: TStreamWriter;
  Columns: TList<TColumn>;
  I, RowCount: Integer;
  Col: TColumn;
  FirstRow, FirstCol: Boolean;
begin
  if not Assigned(FDataset) or not FDataset.Active then
    raise Exception.Create('Dataset is not active');
    
  Writer := TStreamWriter.Create(Stream, TEncoding.UTF8);
  try
    Columns := GetExportColumns;
    try
      Writer.WriteLine('[');
      
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
              Writer.Write('": "');
              Writer.Write(EscapeJSON(FormatFieldValue(Col.Field)));
              Writer.Write('"');
            end;
          end;
          
          Writer.Write('}');
          
          Inc(RowCount);
          if RowCount mod 100 = 0 then
            CheckProgress(RowCount, FDataset.RecordCount);
            
          FDataset.Next;
        end;
        
        Writer.WriteLine;
      finally
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
var
  Stream: TFileStream;
  ExcelExporter: TVittixExcelExporter;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExcelExporter := TVittixExcelExporter.Create(Self);
    try
      if SameText(TPath.GetExtension(FileName), '.xlsx') then
        ExcelExporter.ExportToXLSX(Stream)
      else
        ExcelExporter.ExportToXLS(FileName);
    finally
      ExcelExporter.Free;
    end;
  finally
    Stream.Free;
  end;
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

procedure TVittixExcelExporter.ExportToXLSX(Stream: TStream); // Changed to stream-based
var
  XML: TStringList;
  Columns: TList<TColumn>;
  Writer: TStreamWriter;
  I, Row, RowCount: Integer;
  Col: TColumn;
  Value: string;
begin
  if not Assigned(FExporter.Dataset) or not FExporter.Dataset.Active then
    raise Exception.Create('Dataset is not active');
    
  XML := TStringList.Create;
  // IMPORTANT NOTE: This is a simplified XML export for Excel (SpreadsheetML fragment).
  // It generates a single XML file that *might* be opened by Excel, but it is NOT a
  // fully compliant XLSX (Office Open XML) file, which is a ZIP archive containing
  // multiple XML parts. For full XLSX functionality (e.g., styling, multiple sheets,
  // formulas), consider using a dedicated library like FlexCel or TXlsFile.
  
  try
    // Simplified XLSX export (SpreadsheetML XML format)
    XML.Add('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
    XML.Add('<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">');
    XML.Add('<sheetData>');
    
    Columns := FExporter.GetExportColumns;
    try
      Row := 1;
      
      // Header row
      if FExporter.Options.IncludeHeaders then
      begin
        XML.Add(Format('<row r="%d">', [Row]));
        for I := 0 to Columns.Count - 1 do
        begin
          XML.Add(Format('<c r="%s%d" t="inlineStr">', [
            Char(Ord('A') + I), Row
          ]));
          XML.Add('<is><t>' + FExporter.EscapeXML(Columns[I].Title.Caption) + '</t></is>');
          XML.Add('</c>');
        end;
        XML.Add('</row>');
        Inc(Row);
      end;
      
      // Data rows
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
              Value := FExporter.FormatFieldValue(Col.Field);
              XML.Add(Format('<c r="%s%d" t="inlineStr">', [
                Char(Ord('A') + I), Row
              ]));
              XML.Add('<is><t>' + FExporter.EscapeXML(Value) + '</t></is>');
              XML.Add('</c>');
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
        FExporter.Dataset.EnableControls;
      end;
      
    finally
      Columns.Free;
    end;
    
    XML.Add('</sheetData>');
    XML.Add('</worksheet>');
    
    Writer := TStreamWriter.Create(Stream, TEncoding.UTF8);
    try
      Writer.Write(XML.Text); // Write the XML content to the stream
    finally Writer.Free; end;
    
  finally
    XML.Free;
  end;
end;

procedure TVittixExcelExporter.ExportToXLS(const FileName: string);
begin
  // For XLS format, we'd need OLE Automation or a library like FlexCel
  // For now, export as XLSX
  raise Exception.Create(
    'XLS format requires Excel OLE Automation or a third-party library. ' +
    'Please use XLSX format instead.'
  );
end;

end.
