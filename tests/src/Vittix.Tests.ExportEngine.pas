unit Vittix.Tests.ExportEngine;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Zip,
  Datasnap.DBClient,
  Vcl.Forms,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.Export.Engine;

type
  [TestFixture]
  TVittixExportEngineTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FExporter: TVittixDBGridExporter;
    procedure CancelAtFirstProgress(Sender: TObject; Current, Total: Integer;
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

end.
