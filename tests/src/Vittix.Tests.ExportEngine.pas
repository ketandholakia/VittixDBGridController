unit Vittix.Tests.ExportEngine;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.Zip,
  Datasnap.DBClient,
  Vcl.Forms,
  Vcl.Clipbrd,
  Vcl.DBGrids,
  Vcl.Grids,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.Export.Dialog,
  Vittix.DBGrid.Export.Engine,
  Vittix.DBGrid.Filter.Engine,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Aggregation.Engine,
  Vittix.DBGrid.Controller;

type
  [TestFixture]
  TVittixExportEngineTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FExporter: TVittixDBGridExporter;
    FProgressCount: Integer;
    FLastProgressCurrent: Integer;
    FLastProgressTotal: Integer;
    FNestedRejected: Boolean;
    procedure AttemptNestedExport(Sender: TObject; Current, Total: Integer;
      var Cancel: Boolean);
    procedure FailDuringExport(Sender: TObject; Current, Total: Integer;
      var Cancel: Boolean);
    procedure CancelAtFirstProgress(Sender: TObject; Current, Total: Integer;
      var Cancel: Boolean);
    procedure RecordProgress(Sender: TObject; Current, Total: Integer;
      var Cancel: Boolean);
    function ExtractSheetXml(Stream: TMemoryStream): string;
  public
    [Test]
    procedure ExportDisablesGridAndRejectsReentry;
    [Test]
    procedure ExportRestoresGridAfterProgressException;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure CsvEscapesDelimiterQuotesAndLineBreaks;
    [Test]
    procedure HtmlAndXmlEscapeSpecialCharactersWithoutLosingUnicode;
    [Test]
    procedure JsonEscapesQuotesWithoutLosingUnicode;
    [Test]
    procedure XlsxProducesValidZipPackage;
    [Test]
    procedure XlsxUsesColumnLettersPastZ;
    [Test]
    procedure CancelStopsExportEarly;
    [Test]
    procedure CancelledFileExportDoesNotOverwriteExistingFile;
    [Test]
    procedure CsvNeutralizesFormulaLeadingValues;
    [Test]
    procedure TsvNeutralizesFormulaLeadingValues;
    [Test]
    procedure CsvNeutralizesFormulaLikeText;
    [Test]
    procedure CsvLocaleFormatOptionKeepsDisplayFormatting;
    [Test]
    procedure EmptyDatasetExportsInEveryFormat;
    [Test]
    procedure CsvKeepsNumericValuesUnneutralized;
    [Test]
    procedure CancelledExportDoesNotPoisonNextExport;
    [Test]
    procedure JsonExportsTypedValues;
    [Test]
    procedure JsonEscapesControlCharacters;
    [Test]
    procedure MachineFormatsKeepFullFloatPrecision;
    [Test]
    procedure XmlDropsIllegalControlCharacters;
    [Test]
    procedure XlsxWritesNumbersAsNumericCells;
    [Test]
    procedure XlsxReportsProgressDuringExport;
    [Test]
    procedure ClipboardExportWritesExpectedText;
    [Test]
    procedure ExportDialogStateRoundTripsThroughIni;
    [Test]
    procedure ExportDialogGeometryAndPageRoundTrip;
    [Test]
    procedure ExportDialogSupportsTextFormat;
    [Test]
    procedure ExportDialogPreviewSupportsTextFormat;
    [Test]
    procedure ExportDialogRemembersDestinationPerFormat;
  end;

  [TestFixture]
  TVittixExportFilteredOnlyTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FExporter: TVittixDBGridExporter;
    FFilterEngine: TVittixDBGridFilterEngine;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure NoFilter_ExportFilteredOnlyFalse_ExportAll;
    [Test]
    procedure NoFilter_ExportFilteredOnlyTrue_ExportAll;
    [Test]
    procedure ActiveFilter_ExportFilteredOnlyTrue_OnlyFiltered;
    [Test]
    procedure ActiveFilter_ExportFilteredOnlyFalse_AllRecords;
    [Test]
    procedure ActiveFilter_ExportFilteredOnlyTrue_EmptyFilteredResult;
    [Test]
    procedure PositionPreserved_AfterExport;
    [Test]
    procedure FilterStateUnchanged_AfterExport;
    [Test]
    procedure Csv_ExportFilteredOnlyTrue_WithActiveFilter;
    [Test]
    procedure Html_ExportFilteredOnlyTrue_WithActiveFilter;
    [Test]
    procedure Xlsx_ExportFilteredOnlyTrue_WithActiveFilter;
    [Test]
    procedure Xml_ExportFilteredOnlyTrue_WithActiveFilter;
    [Test]
    procedure Json_ExportFilteredOnlyTrue_WithActiveFilter;
    [Test]
    procedure Text_ExportFilteredOnlyTrue_WithActiveFilter;
    [Test]
    procedure Xml_ExportFilteredOnlyFalse_AllRecords;
    [Test]
    procedure Json_ExportFilteredOnlyFalse_AllRecords;
    [Test]
    procedure Text_ExportFilteredOnlyFalse_AllRecords;
    [Test]
    procedure PositionPreserved_AfterXmlExport;
    [Test]
    procedure PositionPreserved_AfterJsonExport;
    [Test]
    procedure PositionPreserved_AfterTextExport;
    function ExtractSheetXml(Stream: TMemoryStream): string;
  end;

  [TestFixture]
  TVittixExportIncludeFooterTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FExporter: TVittixDBGridExporter;
    procedure SetAggregation(AFieldName: string; AAgg: TVittixAggregationType);
    procedure SetFooterText(AFieldName: string; AText: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure IncludeFooterFalse_NoFooterRow_Csv;
    [Test]
    procedure IncludeFooterTrue_NoAggregation_EmptyFooterRow_Csv;
    [Test]
    procedure IncludeFooterTrue_WithAggregation_FooterRow_Csv;
    [Test]
    procedure IncludeFooterTrue_MultipleAggregatedColumns_FooterRow_Csv;
    [Test]
    procedure IncludeFooterTrue_FooterTextOverridesAggregation_Csv;
    [Test]
    procedure IncludeFooterFalse_NoFooterRow_Html;
    [Test]
    procedure IncludeFooterTrue_WithAggregation_FooterRow_Html;
    [Test]
    procedure IncludeFooterTrue_WithAggregation_FooterRow_Xlsx;
    [Test]
    procedure IncludeFooterTrue_WithAggregation_FooterRow_Xml;
    [Test]
    procedure IncludeFooterTrue_WithAggregation_FooterRow_Json;
    [Test]
    procedure IncludeFooterTrue_WithAggregation_FooterRow_Text;
    [Test]
    procedure IncludeFooterTrue_EmptyDataset_EmptyFooterRow_Csv;
    [Test]
    procedure IncludeFooterRespectsExportVisibleOnly_Csv;
    function ExtractSheetXml(Stream: TMemoryStream): string;
  end;

implementation

uses
  System.IOUtils,
  System.JSON,
  Vittix.Tests.TestData,
  Vittix.DBGrid.Clipboard;

// Splits CSV text into logical records. Commas, quotes and line breaks
// inside quoted fields (RFC 4180 style) must not start a new record.
function SplitCsvRecords(const Text: string): TStringList;
var
  I: Integer;
  InQuotes: Boolean;
  Current: string;
begin
  Result := TStringList.Create;
  InQuotes := False;
  Current := '';
  I := 1;
  while I <= Length(Text) do
  begin
    case Text[I] of
      #10:
        begin
          if InQuotes then
            Current := Current + Text[I]
          else
          begin
            Result.Add(Current);
            Current := '';
          end;
        end;
      #13:
        begin
          if InQuotes then
            Current := Current + Text[I];
        end;
      '"':
        begin
          Current := Current + '"';
          if (I < Length(Text)) and (Text[I + 1] = '"') then
          begin
            Current := Current + '"';
            Inc(I);
          end
          else
            InQuotes := not InQuotes;
        end;
    else
      Current := Current + Text[I];
    end;
    Inc(I);
  end;
  if Current <> '' then
    Result.Add(Current);
end;

procedure TVittixExportEngineTests.AttemptNestedExport(Sender: TObject;
  Current, Total: Integer; var Cancel: Boolean);
var
  Stream: TMemoryStream;
begin
  Assert.IsFalse(FGrid.Enabled);
  Stream := TMemoryStream.Create;
  try
    try
      FExporter.ExportToTSVStream(Stream);
      Assert.Fail('Nested export was accepted');
    except
      on E: EVittixExportError do
      begin
        Assert.IsTrue(Pos('already', E.Message) > 0);
        FNestedRejected := True;
      end;
    end;
  finally
    Stream.Free;
  end;
  Cancel := True;
end;

procedure TVittixExportEngineTests.FailDuringExport(Sender: TObject;
  Current, Total: Integer; var Cancel: Boolean);
begin
  Assert.IsFalse(FGrid.Enabled);
  raise EAbort.Create('progress failed');
end;

procedure TVittixExportEngineTests.ExportDisablesGridAndRejectsReentry;
var
  I: Integer;
begin
  for I := 6 to 110 do FDataSet.AppendRecord([I]);
  FNestedRejected := False;
  FExporter.OnProgress := AttemptNestedExport;
  FExporter.ExportToString(vefCSV);
  Assert.IsTrue(FNestedRejected);
  Assert.IsTrue(FGrid.Enabled);
  FGrid.Enabled := False;
  FExporter.OnProgress := nil;
  FExporter.ExportToString(vefJSON);
  Assert.IsFalse(FGrid.Enabled);
end;

procedure TVittixExportEngineTests.ExportRestoresGridAfterProgressException;
var
  I: Integer;
begin
  for I := 6 to 110 do FDataSet.AppendRecord([I]);
  FExporter.OnProgress := FailDuringExport;
  Assert.WillRaise(procedure begin FExporter.ExportToString(vefCSV); end, EAbort);
  Assert.IsTrue(FGrid.Enabled);
  FExporter.OnProgress := nil;
  Assert.IsTrue(FExporter.ExportToString(vefCSV) <> '');
end;

procedure TVittixExportEngineTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  FExporter := TVittixDBGridExporter.Create(FGrid);
end;

procedure TVittixExportEngineTests.TearDown;
begin
  FExporter.Free;
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixExportEngineTests.CancelAtFirstProgress(Sender: TObject;
  Current, Total: Integer; var Cancel: Boolean);
begin
  Cancel := True;
end;

procedure TVittixExportEngineTests.RecordProgress(Sender: TObject;
  Current, Total: Integer; var Cancel: Boolean);
begin
  Inc(FProgressCount);
  FLastProgressCurrent := Current;
  FLastProgressTotal := Total;
end;

function TVittixExportEngineTests.ExtractSheetXml(Stream: TMemoryStream): string;
var
  Zip: TZipFile;
  TempDir: string;
begin
  Result := '';
  TempDir := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString);
  ForceDirectories(TempDir);
  Zip := TZipFile.Create;
  try
    Stream.Position := 0;
    Zip.Open(Stream, zmRead);
    Zip.ExtractAll(TempDir);
    Result := TFile.ReadAllText(
      TPath.Combine(TempDir, 'xl\worksheets\sheet1.xml'),
      TEncoding.UTF8
    );
  finally
    Zip.Free;
    TDirectory.Delete(TempDir, True);
  end;
end;

procedure TVittixExportEngineTests.CsvEscapesDelimiterQuotesAndLineBreaks;
var
  Output: string;
begin
  Output := FExporter.ExportToString(vefCSV);

  Assert.IsTrue(Output.Contains('"first, ""quoted""'));
  Assert.IsTrue(Output.Contains('second line"'));
end;

procedure TVittixExportEngineTests.HtmlAndXmlEscapeSpecialCharactersWithoutLosingUnicode;
var
  Html: string;
  Xml: string;
begin
  Html := FExporter.ExportToString(vefHTML);
  Xml := FExporter.ExportToString(vefXML);

  Assert.IsTrue(Html.Contains('&lt;tag&gt; &amp; &quot;quote&quot;'));
  Assert.IsTrue(Html.Contains('અમદાવાદ'));
  Assert.IsTrue(Xml.Contains('&lt;tag&gt; &amp; &quot;quote&quot;'));
  Assert.IsTrue(Xml.Contains('અમદાવાદ'));
end;

procedure TVittixExportEngineTests.JsonEscapesQuotesWithoutLosingUnicode;
var
  Json: string;
begin
  Json := FExporter.ExportToString(vefJSON);

  Assert.IsTrue(Json.Contains('\"quote\"'));
  Assert.IsTrue(Json.Contains('અમદાવાદ'));
end;

procedure TVittixExportEngineTests.XlsxProducesValidZipPackage;
var
  Stream: TMemoryStream;
  Zip: TZipFile;
begin
  Stream := TMemoryStream.Create;
  try
    FExporter.ExportToStream(Stream, vefExcelXLSX);
    Stream.Position := 0;

    Zip := TZipFile.Create;
    try
      Zip.Open(Stream, zmRead);
      Assert.IsTrue(Zip.IndexOf('[Content_Types].xml') >= 0);
      Assert.IsTrue(Zip.IndexOf('_rels/.rels') >= 0);
      Assert.IsTrue(Zip.IndexOf('xl/workbook.xml') >= 0);
      Assert.IsTrue(Zip.IndexOf('xl/_rels/workbook.xml.rels') >= 0);
      Assert.IsTrue(Zip.IndexOf('xl/worksheets/sheet1.xml') >= 0);
    finally
      Zip.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TVittixExportEngineTests.XlsxUsesColumnLettersPastZ;
var
  WideDataSet: TClientDataSet;
  WideForm: TForm;
  WideGrid: TVittixDBGrid;
  WideExporter: TVittixDBGridExporter;
  Stream: TMemoryStream;
  SheetXml: string;
begin
  WideDataSet := CreateWideDataSet(28);
  try
    WideGrid := CreateHeadlessGrid(WideDataSet, WideForm);
    try
      WideExporter := TVittixDBGridExporter.Create(WideGrid);
      try
        Stream := TMemoryStream.Create;
        try
          WideExporter.ExportToStream(Stream, vefExcelXLSX);
          SheetXml := ExtractSheetXml(Stream);
        finally
          Stream.Free;
        end;
      finally
        WideExporter.Free;
      end;
    finally
      WideForm.Free;
    end;
  finally
    WideDataSet.Free;
  end;

  Assert.IsTrue(SheetXml.Contains('r="AA1"'));
  Assert.IsTrue(SheetXml.Contains('r="AB1"'));
end;

procedure TVittixExportEngineTests.CancelStopsExportEarly;
var
  LargeDataSet: TClientDataSet;
  LargeForm: TForm;
  LargeGrid: TVittixDBGrid;
  LargeExporter: TVittixDBGridExporter;
  Output: string;
  LineCount: Integer;
  Lines: TStringList;
begin
  LargeDataSet := CreateLargeDataSet(250);
  try
    LargeGrid := CreateHeadlessGrid(LargeDataSet, LargeForm);
    try
      LargeExporter := TVittixDBGridExporter.Create(LargeGrid);
      try
        LargeExporter.OnProgress := CancelAtFirstProgress;
        Output := LargeExporter.ExportToString(vefCSV);
        Lines := TStringList.Create;
        try
          Lines.Text := Output;
          LineCount := Lines.Count;
        finally
          Lines.Free;
        end;
      finally
        LargeExporter.Free;
      end;
    finally
      LargeForm.Free;
    end;
  finally
    LargeDataSet.Free;
  end;

  Assert.IsTrue(LineCount < 251);
end;

procedure TVittixExportEngineTests.CancelledFileExportDoesNotOverwriteExistingFile;
var
  TempFileName: string;
  OriginalText: string;
  LargeDataSet: TClientDataSet;
  LargeForm: TForm;
  LargeGrid: TVittixDBGrid;
  LargeExporter: TVittixDBGridExporter;
begin
  TempFileName := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.csv');
  OriginalText := 'keep me';
  TFile.WriteAllText(TempFileName, OriginalText, TEncoding.UTF8);

  LargeDataSet := CreateLargeDataSet(250);
  try
    LargeGrid := CreateHeadlessGrid(LargeDataSet, LargeForm);
    try
      LargeExporter := TVittixDBGridExporter.Create(LargeGrid);
      try
        LargeExporter.OnProgress := CancelAtFirstProgress;
        Assert.WillRaise(
          procedure
          begin
            LargeExporter.ExportToCSV(TempFileName);
          end,
          EAbort
        );
      finally
        LargeExporter.Free;
      end;
    finally
      LargeForm.Free;
    end;
  finally
    LargeDataSet.Free;
  end;

  Assert.AreEqual(OriginalText, TFile.ReadAllText(TempFileName, TEncoding.UTF8));
  TFile.Delete(TempFileName);
end;

procedure TVittixExportEngineTests.CsvNeutralizesFormulaLeadingValues;
var
  Output: string;
begin
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := '=SUM(1,2)';
  FDataSet.Post;

  Output := FExporter.ExportToString(vefCSV);

  Assert.IsTrue(Output.Contains('''=SUM(1,2)'));
  Assert.IsFalse(Output.Contains(#10'=SUM(1,2)'));
end;

procedure TVittixExportEngineTests.TsvNeutralizesFormulaLeadingValues;
var
  Output: string;
begin
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := '+SUM(1,2)';
  FDataSet.Post;

  Output := FExporter.ExportToString(vefTSV);

  Assert.IsTrue(Output.Contains('''+SUM(1,2)'));
end;

procedure TVittixExportEngineTests.CsvKeepsNumericValuesUnneutralized;
var
  Output: string;
begin
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := '+441234567890';
  FDataSet.FieldByName('Amount').AsCurrency := -12.50;
  FDataSet.Post;

  Output := FExporter.ExportToString(vefCSV);

  // Numbers that parse as numbers (-12.50, +441234567890) are genuine
  // numeric data: they must survive as numbers so Excel does not turn
  // them into text cells with a leading apostrophe.
  Assert.IsTrue(Output.Contains('-12.5'), 'negative amount exported as a number');
  Assert.IsFalse(Output.Contains('''-12.5'), 'negative amount not neutralized');
  Assert.IsTrue(Output.Contains('+441234567890'), 'plus-prefixed number exported as-is');
  Assert.IsFalse(Output.Contains('''+441234567890'), 'plus-prefixed number not neutralized');
end;

procedure TVittixExportEngineTests.CancelledExportDoesNotPoisonNextExport;
var
  TempFile: string;
  LargeDataSet: TClientDataSet;
  LargeForm: TForm;
  LargeGrid: TVittixDBGrid;
  LargeExporter: TVittixDBGridExporter;
  Lines: TStringList;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.csv');
  LargeDataSet := CreateLargeDataSet(250);
  try
    LargeGrid := CreateHeadlessGrid(LargeDataSet, LargeForm);
    try
      LargeExporter := TVittixDBGridExporter.Create(LargeGrid);
      try
        // The first export is cancelled and aborts...
        LargeExporter.OnProgress := CancelAtFirstProgress;
        Assert.WillRaise(
          procedure
          begin
            LargeExporter.ExportToCSV(TempFile);
          end,
          EAbort
        );

        // ...and the second export on the SAME exporter must run to
        // completion: the cancel flag used to stay set for the file paths.
        LargeExporter.OnProgress := nil;
        LargeExporter.ExportToCSV(TempFile);

        Lines := TStringList.Create;
        try
          Lines.Text := TFile.ReadAllText(TempFile, TEncoding.UTF8);
          Assert.AreEqual(251, Lines.Count, 'header + 250 rows after re-export');
        finally
          Lines.Free;
        end;
      finally
        LargeExporter.Free;
      end;
    finally
      LargeForm.Free;
    end;
  finally
    LargeDataSet.Free;
    if FileExists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixExportEngineTests.JsonExportsTypedValues;
var
  Json: string;
  Parsed: TJSONValue;
begin
  Json := FExporter.ExportToString(vefJSON);

  // Numbers as numbers, booleans as true/false, nulls as null
  Assert.IsTrue(Json.Contains('"ID": 1'), 'ID exported as a JSON number');
  Assert.IsTrue(Json.Contains('"Score": 3.5'), 'Score exported as a JSON number');
  Assert.IsTrue(Json.Contains('"IsActive": true'), 'boolean true');
  Assert.IsTrue(Json.Contains('"IsActive": false'), 'boolean false');
  Assert.IsTrue(Json.Contains('"Notes": null'), 'null exported as null');

  // The whole export must parse as real JSON
  Parsed := TJSONObject.ParseJSONValue(Json);
  try
    Assert.IsNotNull(Parsed, 'export produces valid JSON');
    Assert.IsTrue(Parsed is TJSONArray, 'top level is a JSON array');
  finally
    Parsed.Free;
  end;
end;

procedure TVittixExportEngineTests.JsonEscapesControlCharacters;
var
  Json: string;
  Parsed: TJSONValue;
begin
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Notes').AsString := 'bell'#7'and'#27'esc';
  FDataSet.Post;

  Json := FExporter.ExportToString(vefJSON);

  // Control characters below 0x20 must be \u-escaped, not emitted raw
  Assert.IsTrue(Json.Contains('\u0007'), '0x07 escaped as \u0007');
  Assert.IsTrue(Json.Contains('\u001B'), '0x1B escaped as \u001B');

  Parsed := TJSONObject.ParseJSONValue(Json);
  try
    Assert.IsNotNull(Parsed, 'control characters do not break the JSON');
  finally
    Parsed.Free;
  end;
end;

procedure TVittixExportEngineTests.MachineFormatsKeepFullFloatPrecision;
var
  Csv: string;
  Json: string;
  Html: string;
begin
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Score').AsFloat := 1234.5678;
  FDataSet.Post;

  Csv := FExporter.ExportToString(vefCSV);
  Json := FExporter.ExportToString(vefJSON);
  Html := FExporter.ExportToString(vefHTML);

  // Machine formats keep the stored precision with an invariant separator
  Assert.IsTrue(Csv.Contains('1234.5678'), 'CSV keeps full precision');
  Assert.IsTrue(Json.Contains('"Score": 1234.5678'), 'JSON keeps full precision');
  // HTML keeps the configured display formatting (default 0.00)
  Assert.IsTrue(Html.Contains('1234.57') or Html.Contains('1234,57'),
    'HTML keeps display formatting');
end;

procedure TVittixExportEngineTests.XmlDropsIllegalControlCharacters;
var
  Xml: string;
begin
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Notes').AsString := 'bad'#1'char'#2'here';
  FDataSet.Post;

  Xml := FExporter.ExportToString(vefXML);

  // 0x01/0x02 are illegal in XML 1.0 and make Excel reject the file
  Assert.IsFalse(Xml.Contains(#1), '0x01 dropped');
  Assert.IsFalse(Xml.Contains(#2), '0x02 dropped');
  Assert.IsTrue(Xml.Contains('badcharhere'), 'surrounding text preserved');
end;

procedure TVittixExportEngineTests.CsvNeutralizesFormulaLikeText;
var
  Output: string;
begin
  // A string cell that only LOOKS like it could compute: it is not a valid
  // number, so the injection guard must still fire.
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := '-1+1';
  FDataSet.Post;

  Output := FExporter.ExportToString(vefCSV);

  Assert.IsTrue(Output.Contains('''-1+1'),
    'formula-like text still neutralized');
end;

procedure TVittixExportEngineTests.CsvLocaleFormatOptionKeepsDisplayFormatting;
var
  Output: string;
begin
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Score').AsFloat := 1234.5678;
  FDataSet.Post;

  // Opt-out for consumers who open the CSV in locale-sensitive tools
  FExporter.Options.ExportLocaleFormat := True;
  Output := FExporter.ExportToString(vefCSV);

  Assert.IsTrue(Output.Contains('1234.57') or Output.Contains('1234,57'),
    'ExportLocaleFormat keeps the configured display formatting');
  Assert.IsFalse(Output.Contains('1234.5678'),
    'full precision must not be used when the locale option is on');
end;

procedure TVittixExportEngineTests.EmptyDatasetExportsInEveryFormat;
var
  EmptySet: TClientDataSet;
  EmptyForm: TForm;
  EmptyGrid: TVittixDBGrid;
  EmptyExporter: TVittixDBGridExporter;
  Output: string;
  Lines: TStringList;
  Stream: TMemoryStream;
  Parsed: TJSONValue;
begin
  EmptySet := CreateSampleDataSet;
  try
    EmptySet.EmptyDataSet;
    EmptyGrid := CreateHeadlessGrid(EmptySet, EmptyForm);
    try
      EmptyExporter := TVittixDBGridExporter.Create(EmptyGrid);
      try
        // Row-oriented formats: header only, never an exception
        Output := EmptyExporter.ExportToString(vefCSV);
        Lines := TStringList.Create;
        try
          Lines.Text := Output;
          Assert.AreEqual(1, Lines.Count, 'CSV: header only');
        finally
          Lines.Free;
        end;

        Output := EmptyExporter.ExportToString(vefTSV);
        Assert.IsTrue(Output.Contains('ID'), 'TSV header present');
        Output := EmptyExporter.ExportToString(vefText);
        Assert.IsTrue(Output.Contains('ID'), 'Text header present');

        Output := EmptyExporter.ExportToString(vefHTML);
        Assert.IsTrue(Output.Contains('<table>'), 'HTML table emitted');

        Output := EmptyExporter.ExportToString(vefXML);
        Assert.IsTrue(Output.Contains('<data>'), 'XML root emitted');
        Assert.IsTrue(Output.Contains('</data>'), 'XML root closed');

        Output := EmptyExporter.ExportToString(vefJSON);
        Parsed := TJSONObject.ParseJSONValue(Output);
        try
          Assert.IsNotNull(Parsed, 'empty JSON parses');
          Assert.IsTrue(Parsed is TJSONArray, 'empty JSON is an array');
        finally
          Parsed.Free;
        end;

        // XLSX must stay a valid zip package
        Stream := TMemoryStream.Create;
        try
          EmptyExporter.ExportToStream(Stream, vefExcelXLSX);
          Stream.Position := 0;
          Assert.IsTrue(ExtractSheetXml(Stream).Contains('<sheetData>'),
            'empty XLSX sheet valid');
        finally
          Stream.Free;
        end;

        // Footer options must not break the empty-dataset shapes either
        EmptyExporter.Options.IncludeFooter := True;
        Output := EmptyExporter.ExportToString(vefJSON);
        Parsed := TJSONObject.ParseJSONValue(Output);
        try
          Assert.IsNotNull(Parsed, 'empty JSON with footer parses');
        finally
          Parsed.Free;
        end;
        Output := EmptyExporter.ExportToString(vefXML);
        Assert.IsTrue(Output.Contains('<footer>'), 'empty XML footer emitted');
      finally
        EmptyExporter.Free;
      end;
    finally
      EmptyForm.Free;
    end;
  finally
    EmptySet.Free;
  end;
end;

procedure TVittixExportEngineTests.XlsxWritesNumbersAsNumericCells;
var
  Stream: TMemoryStream;
  SheetXml: string;
begin
  Stream := TMemoryStream.Create;
  try
    FExporter.ExportToStream(Stream, vefExcelXLSX);
    Stream.Position := 0;
    SheetXml := ExtractSheetXml(Stream);

    // Amount (column C, row 2 = 100.50) as a numeric cell, not inlineStr
    Assert.IsTrue(SheetXml.Contains('<c r="C2"><v>100.5</v></c>'),
      'numbers are stored as numeric cells');
    Assert.IsTrue(SheetXml.Contains('xml:space="preserve"'),
      'string cells preserve whitespace');
  finally
    Stream.Free;
  end;
end;

procedure TVittixExportEngineTests.XlsxReportsProgressDuringExport;
var
  BigSet: TClientDataSet;
  BigOwner: TForm;
  BigGrid: TVittixDBGrid;
  BigExporter: TVittixDBGridExporter;
  Stream: TMemoryStream;
begin
  // Progress fires every 100 rows; the 5-row fixture dataset can never
  // trigger it, so bind a dedicated larger dataset.
  BigSet := CreateLargeDataSet(250);
  try
    BigGrid := CreateHeadlessGrid(BigSet, BigOwner);
    try
      BigExporter := TVittixDBGridExporter.Create(BigGrid);
      try
        FProgressCount := 0;
        FLastProgressCurrent := 0;
        FLastProgressTotal := 0;
        BigExporter.OnProgress := RecordProgress;
        Stream := TMemoryStream.Create;
        try
          BigExporter.ExportToStream(Stream, vefExcelXLSX);
        finally
          Stream.Free;
        end;

        Assert.IsTrue(FProgressCount > 0);
        Assert.IsTrue(FLastProgressCurrent > 0);
        Assert.IsTrue(FLastProgressTotal > 0);
      finally
        BigExporter.Free;
      end;
    finally
      BigOwner.Free;
    end;
  finally
    BigSet.Free;
  end;
end;

procedure TVittixExportEngineTests.ClipboardExportWritesExpectedText;
begin
  VittixSetClipboardText('');
  FExporter.ExportToClipboard(vefTSV);

  Assert.IsTrue(VittixGetClipboardText.Contains('ID'#9'Name'#9'Amount'));
  Assert.IsTrue(VittixGetClipboardText.Contains('1'#9'Alpha'));
end;

procedure TVittixExportEngineTests.ExportDialogStateRoundTripsThroughIni;
var
  TempFile: string;
  Dlg: TfrmExportDialog;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridExportDialog.test.ini');
  TfrmExportDialog.StateFileName := TempFile;
  try
    Dlg := TfrmExportDialog.Create(nil);
    try
      Dlg.rbTSV.Checked := True;
      Dlg.rbFile.Checked := False;
      Dlg.rbClipboard.Checked := True;
      Dlg.chkVisibleOnly.Checked := False;
      Dlg.chkFilteredOnly.Checked := False;
      Dlg.chkIncludeHeaders.Checked := False;
      Dlg.IncludeFooterChecked := True;
      Dlg.edtDateFormat.Text := 'dd/mm/yyyy';
      Dlg.edtTimeFormat.Text := 'hh:nn';
      Dlg.edtCurrencyFormat.Text := '0.000';
      Dlg.edtFileName.Text := 'C:\temp\export.tsv';
      Dlg.SaveDialogState;
    finally
      Dlg.Free;
    end;

    Dlg := TfrmExportDialog.Create(nil);
    try
      Assert.IsTrue(Dlg.rbTSV.Checked);
      Assert.IsTrue(Dlg.rbClipboard.Checked);
      Assert.IsFalse(Dlg.chkVisibleOnly.Checked);
      Assert.IsFalse(Dlg.chkFilteredOnly.Checked);
      Assert.IsFalse(Dlg.chkIncludeHeaders.Checked);
      Assert.IsTrue(Dlg.IncludeFooterChecked);
      Assert.AreEqual('dd/mm/yyyy', Dlg.edtDateFormat.Text);
      Assert.AreEqual('hh:nn', Dlg.edtTimeFormat.Text);
      Assert.AreEqual('0.000', Dlg.edtCurrencyFormat.Text);
      Assert.AreEqual('C:\temp\export.tsv', Dlg.edtFileName.Text);
    finally
      Dlg.Free;
    end;
  finally
    TfrmExportDialog.StateFileName := '';
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixExportEngineTests.ExportDialogGeometryAndPageRoundTrip;
var
  TempFile: string;
  Dlg: TfrmExportDialog;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridExportDialog.geometry.test.ini');
  TfrmExportDialog.StateFileName := TempFile;
  try
    Dlg := TfrmExportDialog.Create(nil);
    try
      Dlg.Left := 123;
      Dlg.Top := 234;
      Dlg.Width := 456;
      Dlg.Height := 567;
      Dlg.SetActivePageIndex(2);
      Dlg.SaveDialogState;
    finally
      Dlg.Free;
    end;

    Dlg := TfrmExportDialog.Create(nil);
    try
      Assert.AreEqual(123, Dlg.Left);
      Assert.AreEqual(234, Dlg.Top);
      Assert.AreEqual(456, Dlg.Width);
      Assert.AreEqual(567, Dlg.Height);
      Assert.AreEqual(2, Dlg.GetActivePageIndex);
    finally
      Dlg.Free;
    end;
  finally
    TfrmExportDialog.StateFileName := '';
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixExportEngineTests.ExportDialogSupportsTextFormat;
var
  TempFile: string;
  Dlg: TfrmExportDialog;
  Ini: TIniFile;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridExportDialog.text.test.ini');
  TfrmExportDialog.StateFileName := TempFile;
  try
    Dlg := TfrmExportDialog.Create(nil);
    try
      Dlg.TextFormatChecked := True;
      Dlg.SaveDialogState;
    finally
      Dlg.Free;
    end;

    Ini := TIniFile.Create(TempFile);
    try
      Assert.AreEqual(8, Ini.ReadInteger('Export', 'Format', -1));
    finally
      Ini.Free;
    end;

    Dlg := TfrmExportDialog.Create(nil);
    try
      Assert.IsTrue(Dlg.TextFormatChecked);
    finally
      Dlg.Free;
    end;
  finally
    TfrmExportDialog.StateFileName := '';
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixExportEngineTests.ExportDialogRemembersDestinationPerFormat;
var
  TempFile: string;
  Dlg: TfrmExportDialog;
  Ini: TIniFile;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridExportDialog.performat.test.ini');
  TfrmExportDialog.StateFileName := TempFile;
  try
    Dlg := TfrmExportDialog.Create(nil);
    try
      Dlg.rbCSV.Checked := True;
      Dlg.rbFile.Checked := True;
      Dlg.edtFileName.Text := 'C:\temp\export.csv';
      Dlg.SaveDialogState;

      Dlg.rbTSV.Checked := True;
      Dlg.rbFile.Checked := False;
      Dlg.rbClipboard.Checked := True;
      Dlg.edtFileName.Text := 'C:\temp\export.tsv';
      Dlg.SaveDialogState;
    finally
      Dlg.Free;
    end;

    Ini := TIniFile.Create(TempFile);
    try
      Assert.IsTrue(Ini.ReadBool('Export', 'DestinationFile_CSV', False));
      Assert.AreEqual('C:\temp\export.csv', Ini.ReadString('Export', 'FileName_CSV', ''));
      Assert.IsFalse(Ini.ReadBool('Export', 'DestinationFile_TSV', True));
      Assert.AreEqual('C:\temp\export.tsv', Ini.ReadString('Export', 'FileName_TSV', ''));
    finally
      Ini.Free;
    end;

    Ini := TIniFile.Create(TempFile);
    try
      Ini.WriteInteger('Export', 'Format', 0);
    finally
      Ini.Free;
    end;

    Dlg := TfrmExportDialog.Create(nil);
    try
      Dlg.LoadDialogState;
      Assert.IsTrue(Dlg.rbFile.Checked);
      Assert.AreEqual('C:\temp\export.csv', Dlg.edtFileName.Text);
    finally
      Dlg.Free;
    end;

    Ini := TIniFile.Create(TempFile);
    try
      Ini.WriteInteger('Export', 'Format', 1);
    finally
      Ini.Free;
    end;

    Dlg := TfrmExportDialog.Create(nil);
    try
      Dlg.LoadDialogState;
      Assert.IsTrue(Dlg.rbClipboard.Checked);
      Assert.AreEqual('C:\temp\export.tsv', Dlg.edtFileName.Text);
    finally
      Dlg.Free;
    end;
  finally
    TfrmExportDialog.StateFileName := '';
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixExportEngineTests.ExportDialogPreviewSupportsTextFormat;
var
  Dlg: TfrmExportDialog;
begin
  Dlg := TfrmExportDialog.Create(nil);
  try
    Dlg.TextFormatChecked := True;
    Assert.IsTrue(Dlg.TextFormatChecked);
  finally
    Dlg.Free;
  end;
end;

{ =============================================================================
  B1: ExportFilteredOnly regression tests
  ============================================================================= }

{ TVittixExportFilteredOnlyTests }

function TVittixExportFilteredOnlyTests.ExtractSheetXml(Stream: TMemoryStream): string;
var
  Zip: TZipFile;
  TempDir: string;
begin
  Result := '';
  TempDir := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString);
  ForceDirectories(TempDir);
  Zip := TZipFile.Create;
  try
    Stream.Position := 0;
    Zip.Open(Stream, zmRead);
    Zip.ExtractAll(TempDir);
    Result := TFile.ReadAllText(
      TPath.Combine(TempDir, 'xl\worksheets\sheet1.xml'),
      TEncoding.UTF8
    );
  finally
    Zip.Free;
    TDirectory.Delete(TempDir, True);
  end;
end;

procedure TVittixExportFilteredOnlyTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  FExporter := TVittixDBGridExporter.Create(FGrid);
  // Access the filter engine through the controller for applying filters
  FFilterEngine := FGrid.Controller.FilterEngine;
end;

procedure TVittixExportFilteredOnlyTests.TearDown;
begin
  FExporter.Free;
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixExportFilteredOnlyTests.NoFilter_ExportFilteredOnlyFalse_ExportAll;
var
  Output: string;
  Lines: TStringList;
begin
  FExporter.Options.ExportFilteredOnly := False;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    // Header + 5 data rows
    Assert.AreEqual(6, Lines.Count);
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportFilteredOnlyTests.NoFilter_ExportFilteredOnlyTrue_ExportAll;
var
  Output: string;
  Lines: TStringList;
begin
  FExporter.Options.ExportFilteredOnly := True;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    // No filter active => all records exported
    Assert.AreEqual(6, Lines.Count);
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportFilteredOnlyTests.ActiveFilter_ExportFilteredOnlyTrue_OnlyFiltered;
var
  Output: string;
  Lines: TStringList;
  Info: TVittixDBGridColumnInfo;
begin
  // Apply a filter: Name = 'Alpha' (2 matching rows)
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := True;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    // Header + 2 filtered rows
    Assert.AreEqual(3, Lines.Count);
    Assert.IsTrue(Lines[1].Contains('Alpha'));
    Assert.IsTrue(Lines[2].Contains('Alpha'));
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportFilteredOnlyTests.ActiveFilter_ExportFilteredOnlyFalse_AllRecords;
var
  Output: string;
  Lines: TStringList;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := False;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    // ExportFilteredOnly=False exports ALL 5 data rows despite active filter
    Assert.AreEqual(6, Lines.Count);
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportFilteredOnlyTests.ActiveFilter_ExportFilteredOnlyTrue_EmptyFilteredResult;
var
  Output: string;
  Lines: TStringList;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'NonExistentValueXYZ';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := True;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    // Only header row when filtered result is empty
    Assert.AreEqual(1, Lines.Count);
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportFilteredOnlyTests.PositionPreserved_AfterExport;
var
  Info: TVittixDBGridColumnInfo;
begin
  // Apply a filter first: Name = 'Alpha' (2 visible records: ID 1 and 4)
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  // Move to ID 4 (second visible record)
  Assert.IsTrue(FDataSet.Locate('ID', 4, []));

  // ExportFilteredOnly=False temporarily unfilters the dataset for the
  // export; the bookmark restore must bring the same record back.
  FExporter.Options.ExportFilteredOnly := False;
  FExporter.ExportToString(vefCSV);

  Assert.AreEqual(4, FDataSet.FieldByName('ID').AsInteger,
    'Dataset position should be preserved after export');
  Assert.IsTrue(FDataSet.Filtered,
    'Dataset filtering should be re-enabled after export');
end;

procedure TVittixExportFilteredOnlyTests.FilterStateUnchanged_AfterExport;
var
  Output: string;
  Info: TVittixDBGridColumnInfo;
  FilterWasActive: Boolean;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;
  FilterWasActive := FGrid.Controller.FilterEngine.Active;
  Assert.IsTrue(FilterWasActive,
    'Precondition: filter should be active before export');

  FExporter.Options.ExportFilteredOnly := False;
  Output := FExporter.ExportToString(vefCSV);

  // Filter engine should still be active with the same filter
  Assert.IsTrue(FGrid.Controller.FilterEngine.Active,
    'Filter engine should remain active after export');
  Assert.AreEqual('Alpha', Info.FilterText,
    'Filter text should be unchanged');
  Assert.IsTrue(Info.HasFilter,
    'HasFilter flag should be unchanged');
end;

procedure TVittixExportFilteredOnlyTests.Csv_ExportFilteredOnlyTrue_WithActiveFilter;
var
  Output: string;
  Lines: TStringList;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := True;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    Assert.AreEqual(3, Lines.Count); // header + 2 rows
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportFilteredOnlyTests.Html_ExportFilteredOnlyTrue_WithActiveFilter;
var
  Output: string;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := True;
  Output := FExporter.ExportToString(vefHTML);
  // Header row + 2 filtered data rows = 3 <tr> total
  Assert.AreEqual(3, (Length(Output) - Length(StringReplace(Output, '<tr', '', [rfReplaceAll]))) div 3);
end;

procedure TVittixExportFilteredOnlyTests.Xlsx_ExportFilteredOnlyTrue_WithActiveFilter;
var
  Stream: TMemoryStream;
  SheetXml: string;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := True;
  Stream := TMemoryStream.Create;
  try
    FExporter.ExportToStream(Stream, vefExcelXLSX);
    Stream.Position := 0;
    SheetXml := ExtractSheetXml(Stream);
    // Header row + 2 data rows = 3 rows in sheetData
    Assert.IsTrue(SheetXml.Contains('r="A1"'));
    Assert.IsTrue(SheetXml.Contains('r="A3"'));
    Assert.IsFalse(SheetXml.Contains('r="A4"'));
  finally
    Stream.Free;
  end;
end;

{ B3: XML/JSON/Text parity — ExportFilteredOnly, cursor restore }

function CountXmlRows(const Xml: string): Integer;
var
  SearchFrom: Integer;
begin
  Result := 0;
  SearchFrom := 1;
  while Pos('<row>', Xml, SearchFrom) > 0 do
  begin
    Inc(Result);
    SearchFrom := Pos('<row>', Xml, SearchFrom) + 1;
  end;
end;

function CountJsonRows(const Json: string): Integer;
var
  P: Integer;
  KeyLen: Integer;
begin
  // Data rows carry {"ID": <number>; the footer object (when present) holds
  // "ID": "<text>" nested inside {"__footer__": ...} and must not be
  // counted, so keys followed by a quoted value are skipped.
  Result := 0;
  KeyLen := Length('{"ID": ');
  P := Pos('{"ID": ', Json);
  while P > 0 do
  begin
    if (P + KeyLen <= Length(Json)) and (Json[P + KeyLen] <> '"') then
      Inc(Result);
    P := Pos('{"ID": ', Json, P + 1);
  end;
end;

procedure TVittixExportFilteredOnlyTests.Xml_ExportFilteredOnlyTrue_WithActiveFilter;
var
  Output: string;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := True;
  Output := FExporter.ExportToString(vefXML);
  Assert.AreEqual(2, CountXmlRows(Output), 'only the 2 filtered rows');
end;

procedure TVittixExportFilteredOnlyTests.Json_ExportFilteredOnlyTrue_WithActiveFilter;
var
  Output: string;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := True;
  Output := FExporter.ExportToString(vefJSON);
  Assert.AreEqual(2, CountJsonRows(Output), 'only the 2 filtered rows');
end;

procedure TVittixExportFilteredOnlyTests.Text_ExportFilteredOnlyTrue_WithActiveFilter;
var
  Output: string;
  Lines: TStringList;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := True;
  Output := FExporter.ExportToString(vefText);
  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    // header + separator + 2 filtered rows
    Assert.AreEqual(4, Lines.Count);
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportFilteredOnlyTests.Xml_ExportFilteredOnlyFalse_AllRecords;
var
  Output: string;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := False;
  Output := FExporter.ExportToString(vefXML);
  Assert.AreEqual(5, CountXmlRows(Output), 'all records despite the filter');
end;

procedure TVittixExportFilteredOnlyTests.Json_ExportFilteredOnlyFalse_AllRecords;
var
  Output: string;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := False;
  Output := FExporter.ExportToString(vefJSON);
  Assert.AreEqual(5, CountJsonRows(Output), 'all records despite the filter');
end;

procedure TVittixExportFilteredOnlyTests.Text_ExportFilteredOnlyFalse_AllRecords;
var
  Output: string;
  Lines: TStringList;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  FExporter.Options.ExportFilteredOnly := False;
  Output := FExporter.ExportToString(vefText);
  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    // header + separator + all 5 rows
    Assert.AreEqual(7, Lines.Count);
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportFilteredOnlyTests.PositionPreserved_AfterXmlExport;
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  Assert.IsTrue(FDataSet.Locate('ID', 4, []));
  FExporter.ExportToString(vefXML);

  // The export used to leave the cursor at EOF, jumping the grid to the
  // last record.
  Assert.AreEqual(4, FDataSet.FieldByName('ID').AsInteger,
    'XML export preserves the dataset position');
end;

procedure TVittixExportFilteredOnlyTests.PositionPreserved_AfterJsonExport;
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  Assert.IsTrue(FDataSet.Locate('ID', 4, []));
  FExporter.ExportToString(vefJSON);

  Assert.AreEqual(4, FDataSet.FieldByName('ID').AsInteger,
    'JSON export preserves the dataset position');
end;

procedure TVittixExportFilteredOnlyTests.PositionPreserved_AfterTextExport;
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FFilterEngine.Active := True;

  Assert.IsTrue(FDataSet.Locate('ID', 4, []));
  FExporter.ExportToString(vefText);

  Assert.AreEqual(4, FDataSet.FieldByName('ID').AsInteger,
    'Text export preserves the dataset position');
end;

{ =============================================================================
  B1: IncludeFooter regression tests
  ============================================================================= }

{ TVittixExportIncludeFooterTests }

procedure TVittixExportIncludeFooterTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  FExporter := TVittixDBGridExporter.Create(FGrid);
end;

procedure TVittixExportIncludeFooterTests.TearDown;
begin
  FExporter.Free;
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixExportIncludeFooterTests.SetAggregation(AFieldName: string; AAgg: TVittixAggregationType);
var
  Col: TColumn;
begin
  Col := FGrid.Controller.FindColumnByFieldName(AFieldName);
  Assert.IsNotNull(Col);
  // Go through the controller API: it marks the aggregation dirty and
  // recalculates. Writing Info.AggregationType directly leaves the runtime
  // aggregation state stale, so footer text would depend on unrelated
  // recalculation side effects.
  FGrid.Controller.SetColumnAggregation(Col, AAgg);
end;

procedure TVittixExportIncludeFooterTests.SetFooterText(AFieldName: string; AText: string);
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName(AFieldName);
  Assert.IsNotNull(Info);
  Info.FooterText := AText;
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterFalse_NoFooterRow_Csv;
var
  Output: string;
  Lines: TStringList;
begin
  SetAggregation('Amount', vatSum);
  FExporter.Options.IncludeFooter := False;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    // 5 data rows + header = 6 lines, NO footer
    Assert.AreEqual(6, Lines.Count);
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_NoAggregation_EmptyFooterRow_Csv;
var
  Output: string;
  Lines: TStringList;
  DelimCount: Integer;
begin
  // No aggregation configured anywhere
  FExporter.Options.IncludeFooter := True;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    // 5 data rows + header + footer = 7 lines
    Assert.AreEqual(7, Lines.Count);
    // Footer line should have 6 empty fields (ID,Name,Amount,Score,Notes,Created,IsActive = 7 columns)
    // Actually CountVisibleColumns depends on visible columns - the test grid has all columns visible
    DelimCount := 0;
    for var Ch in Lines[6] do
      if Ch = ',' then Inc(DelimCount);
    Assert.AreEqual(6, DelimCount, 'Footer row should have 7 empty cells (6 delimiters)');
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_WithAggregation_FooterRow_Csv;
var
  Output: string;
  Lines: TStringList;
  FooterLine: string;
begin
  SetAggregation('Amount', vatSum);
  FExporter.Options.IncludeFooter := True;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    Assert.AreEqual(7, Lines.Count); // header + 5 data + footer
    FooterLine := Lines[6];
    // Amount column (index 2) should have the sum
    Assert.IsTrue(FooterLine.Contains('750.75') or FooterLine.Contains('750,75'),
      'Footer should contain Sum of Amount (750.75)');
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_MultipleAggregatedColumns_FooterRow_Csv;
var
  Output: string;
  Lines: TStringList;
  FooterLine: string;
begin
  SetAggregation('Amount', vatSum);
  SetAggregation('Score', vatAvg); // Score column for avg
  SetAggregation('ID', vatCount);
  FExporter.Options.IncludeFooter := True;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    Assert.AreEqual(7, Lines.Count);
    FooterLine := Lines[6];
    Assert.IsTrue(FooterLine.Contains('750.75') or FooterLine.Contains('750,75'), 'Amount sum');
    Assert.IsTrue(FooterLine.Contains('5.21') or FooterLine.Contains('5,21') or FooterLine.Contains('5.2') or FooterLine.Contains('5,2'), 'Score avg');
    Assert.IsTrue(FooterLine.Contains('5'), 'ID count');
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_FooterTextOverridesAggregation_Csv;
var
  Output: string;
  Lines: TStringList;
  FooterLine: string;
begin
  SetAggregation('Amount', vatSum);
  SetFooterText('Amount', 'CUSTOM TOTAL');
  FExporter.Options.IncludeFooter := True;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    FooterLine := Lines[6];
    Assert.IsTrue(FooterLine.Contains('CUSTOM TOTAL'),
      'FooterText should override aggregation display');
    Assert.IsFalse(FooterLine.Contains('750.75'),
      'Aggregation value should not appear when FooterText is set');
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterFalse_NoFooterRow_Html;
var
  Output: string;
  TrCount: Integer;
begin
  SetAggregation('Amount', vatSum);
  FExporter.Options.IncludeFooter := False;
  Output := FExporter.ExportToString(vefHTML);
  TrCount := (Length(Output) - Length(StringReplace(Output, '<tr', '', [rfReplaceAll]))) div 3;
  // Header row + 5 data rows = 6 <tr> (no footer)
  Assert.AreEqual(6, TrCount);
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_WithAggregation_FooterRow_Html;
var
  Output: string;
  TrCount: Integer;
begin
  SetAggregation('Amount', vatSum);
  FExporter.Options.IncludeFooter := True;
  Output := FExporter.ExportToString(vefHTML);
  TrCount := (Length(Output) - Length(StringReplace(Output, '<tr', '', [rfReplaceAll]))) div 3;
  // Header row + 5 data rows + footer row = 7 <tr>
  Assert.AreEqual(7, TrCount);
  Assert.IsTrue(Output.Contains('footer-row'),
    'Footer row should have footer-row class');
  Assert.IsTrue(Output.Contains('750.75') or Output.Contains('750,75'),
    'Footer should contain aggregation value');
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_WithAggregation_FooterRow_Xlsx;
var
  Stream: TMemoryStream;
  SheetXml: string;
begin
  SetAggregation('Amount', vatSum);
  FExporter.Options.IncludeFooter := True;
  Stream := TMemoryStream.Create;
  try
    FExporter.ExportToStream(Stream, vefExcelXLSX);
    Stream.Position := 0;
    SheetXml := ExtractSheetXml(Stream);
    // Header + 5 data + footer = 7 rows
    Assert.IsTrue(SheetXml.Contains('r="A1"'));
    Assert.IsTrue(SheetXml.Contains('r="A7"'));
    Assert.IsFalse(SheetXml.Contains('r="A8"'));
    Assert.IsTrue(SheetXml.Contains('750.75') or SheetXml.Contains('750,75'),
      'Footer row should contain aggregation value');
  finally
    Stream.Free;
  end;
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_WithAggregation_FooterRow_Xml;
var
  Output: string;
begin
  SetAggregation('Amount', vatSum);
  FExporter.Options.IncludeFooter := True;
  Output := FExporter.ExportToString(vefXML);

  Assert.AreEqual(5, CountXmlRows(Output), '5 data rows');
  Assert.IsTrue(Output.Contains('<footer>'), 'footer element present');
  Assert.IsTrue(Output.Contains('750.75') or Output.Contains('750,75'),
    'footer should contain the Amount sum');
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_WithAggregation_FooterRow_Json;
var
  Output: string;
begin
  SetAggregation('Amount', vatSum);
  FExporter.Options.IncludeFooter := True;
  Output := FExporter.ExportToString(vefJSON);

  Assert.AreEqual(5, CountJsonRows(Output), '5 data rows');
  Assert.IsTrue(Output.Contains('"__footer__"'), 'marked footer object present');
  Assert.IsTrue(Output.Contains('750.75') or Output.Contains('750,75'),
    'footer should contain the Amount sum');
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_WithAggregation_FooterRow_Text;
var
  Output: string;
  Lines: TStringList;
begin
  SetAggregation('Amount', vatSum);
  FExporter.Options.IncludeFooter := True;
  Output := FExporter.ExportToString(vefText);

  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    // header + separator + 5 data rows + footer
    Assert.AreEqual(8, Lines.Count);
    Assert.IsTrue(Lines[7].Contains('750.75') or Lines[7].Contains('750,75'),
      'footer line should contain the Amount sum');
  finally
    Lines.Free;
  end;
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterTrue_EmptyDataset_EmptyFooterRow_Csv;
var
  EmptyDataSet: TClientDataSet;
  EmptyForm: TForm;
  EmptyGrid: TVittixDBGrid;
  EmptyExporter: TVittixDBGridExporter;
  Output: string;
  Lines: TStringList;
begin
  EmptyDataSet := CreateSampleDataSet;
  try
    EmptyDataSet.EmptyDataSet; // no records
    EmptyGrid := CreateHeadlessGrid(EmptyDataSet, EmptyForm);
    try
      EmptyExporter := TVittixDBGridExporter.Create(EmptyGrid);
      try
        SetAggregation('Amount', vatSum);
        EmptyExporter.Options.IncludeFooter := True;
        Output := EmptyExporter.ExportToString(vefCSV);
        Lines := TStringList.Create;
        try
          Lines.Text := Output;
          // Header + footer = 2 lines (no data rows); 7 columns -> 6 commas
          Assert.AreEqual(2, Lines.Count);
          Assert.AreEqual(',,,,,,', Lines[1], 'Footer row should be empty cells');
        finally
          Lines.Free;
        end;
      finally
        EmptyExporter.Free;
      end;
    finally
      EmptyForm.Free;
    end;
  finally
    EmptyDataSet.Free;
  end;
end;

procedure TVittixExportIncludeFooterTests.IncludeFooterRespectsExportVisibleOnly_Csv;
var
  Output: string;
  Lines: TStringList;
  FooterLine: string;
  DelimCount: Integer;
begin
  // Hide the Amount column
  FGrid.Controller.FindColumnByFieldName('Amount').Visible := False;
  SetAggregation('Amount', vatSum);
  FExporter.Options.IncludeFooter := True;
  FExporter.Options.ExportVisibleOnly := True;
  Output := FExporter.ExportToString(vefCSV);
  Lines := SplitCsvRecords(Output);
  try
    // Footer should only have cells for visible columns (6 columns, no Amount)
    Assert.AreEqual(7, Lines.Count); // header + 5 data + footer
    FooterLine := Lines[6];
    DelimCount := 0;
    for var Ch in FooterLine do
      if Ch = ',' then Inc(DelimCount);
    Assert.AreEqual(5, DelimCount, 'Footer should have 6 cells (visible columns only)');
  finally
    Lines.Free;
  end;
end;

{ TVittixExportIncludeFooterTests }

function TVittixExportIncludeFooterTests.ExtractSheetXml(Stream: TMemoryStream): string;
var
  Zip: TZipFile;
  TempDir: string;
begin
  Result := '';
  TempDir := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString);
  ForceDirectories(TempDir);
  Zip := TZipFile.Create;
  try
    Stream.Position := 0;
    Zip.Open(Stream, zmRead);
    Zip.ExtractAll(TempDir);
    Result := TFile.ReadAllText(
      TPath.Combine(TempDir, 'xl\worksheets\sheet1.xml'),
      TEncoding.UTF8
    );
  finally
    Zip.Free;
    TDirectory.Delete(TempDir, True);
  end;
end;

end.
