unit Vittix.Tests.ExportEngine;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Zip,
  Datasnap.DBClient,
  Vcl.Forms,
  Vcl.Clipbrd,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.Export.Dialog,
  Vittix.DBGrid.Export.Engine;

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
    procedure CancelAtFirstProgress(Sender: TObject; Current, Total: Integer;
      var Cancel: Boolean);
    procedure RecordProgress(Sender: TObject; Current, Total: Integer;
      var Cancel: Boolean);
    function ExtractSheetXml(Stream: TMemoryStream): string;
  public
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
    procedure XlsxReportsProgressDuringExport;
    [Test]
    procedure ClipboardExportWritesExpectedText;
    [Test]
    procedure ExportDialogStateRoundTripsThroughIni;
  end;

implementation

uses
  System.IOUtils,
  Vittix.Tests.TestData;

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

procedure TVittixExportEngineTests.XlsxReportsProgressDuringExport;
var
  Stream: TMemoryStream;
begin
  FProgressCount := 0;
  FLastProgressCurrent := 0;
  FLastProgressTotal := 0;
  FExporter.OnProgress := RecordProgress;
  Stream := TMemoryStream.Create;
  try
    FExporter.ExportToStream(Stream, vefExcelXLSX);
  finally
    Stream.Free;
  end;

  Assert.IsTrue(FProgressCount > 0);
  Assert.IsTrue(FLastProgressCurrent > 0);
  Assert.IsTrue(FLastProgressTotal > 0);
end;

procedure TVittixExportEngineTests.ClipboardExportWritesExpectedText;
begin
  Clipboard.AsText := '';
  FExporter.ExportToClipboard(vefTSV);

  Assert.IsTrue(Clipboard.AsText.Contains('ID'#9'Name'#9'Amount'));
  Assert.IsTrue(Clipboard.AsText.Contains('1'#9'Alpha'));
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

end.
