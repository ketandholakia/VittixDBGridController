unit Vittix.Tests.ClipboardCopy;

interface

uses
  System.SysUtils,
  System.Classes,
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  Vcl.DBGrids,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Export.Engine,
  Vittix.DBGrid.FooterPanel,
  Vittix.DBGrid.Controller;

type
  [TestFixture]
  TVittixClipboardCopyTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    procedure SelectRow(ID: Integer);
    function CopyText(AIncludeHeaders: Boolean): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure SingleCellCopiesCurrentValue;
    [Test]
    procedure MultiSelectCopiesSelectedRowsTabDelimited;
    [Test]
    procedure HeadersOnlyWhenRequested;
    [Test]
    procedure CtrlShiftCForcesHeadersThroughController;
    [Test]
    procedure HiddenColumnsExcluded;
    [Test]
    procedure NullValuesExportAsEmptyFields;
    [Test]
    procedure TabsQuotesAndNewlinesAreQuoted;
    [Test]
    procedure NumericValuesKeepInvariantPrecision;
    [Test]
    procedure LocaleOptionSwitchesToDisplayText;
    [Test]
    procedure ClipboardRoundTrip;
    [Test]
    procedure CursorPreservedAfterCopy;
    [Test]
    procedure ShouldBypassFunctionTruthTable;
    [Test]
    procedure ClipboardBypassDuringDatasetEdit;
    [Test]
    procedure ClipboardBypassWithInplaceEditor;
    [Test]
    procedure FooterSelectionAggregateSumAndCount;
    [Test]
    procedure FooterSelectionAggregateLeavesDatasetAggregationUntouched;
    [Test]
    procedure FooterNoSelectionAggregateWithoutMultiSelect;
  end;

implementation

uses
  Vittix.Tests.TestData,
  Vittix.DBGrid.Clipboard;

{ TVittixClipboardCopyTests }

procedure TVittixClipboardCopyTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
end;

procedure TVittixClipboardCopyTests.TearDown;
begin
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixClipboardCopyTests.SelectRow(ID: Integer);
begin
  FDataSet.Locate('ID', ID, []);
  FGrid.SelectedRows.CurrentRowSelected := True;
end;

function TVittixClipboardCopyTests.CopyText(AIncludeHeaders: Boolean): string;
var
  Exporter: TVittixDBGridExporter;
begin
  // The controller's Ctrl+C path runs through the same public method on a
  // transient exporter; driving it directly keeps the assertions free of
  // clipboard contention.
  Exporter := TVittixDBGridExporter.Create(FGrid);
  try
    Result := Exporter.BuildCopyText(AIncludeHeaders);
  finally
    Exporter.Free;
  end;
end;

procedure TVittixClipboardCopyTests.SingleCellCopiesCurrentValue;
begin
  FDataSet.Locate('ID', 5, []);
  Assert.AreEqual('5', CopyText(False),
    'without a multi-row selection Ctrl+C copies the current cell only');
end;

procedure TVittixClipboardCopyTests.MultiSelectCopiesSelectedRowsTabDelimited;
var
  Lines: TStringList;
begin
  // Rows 2 and 4 carry no embedded line breaks (row 1 would split the raw
  // text; that case is covered by TabsQuotesAndNewlinesAreQuoted).
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(2);
  SelectRow(4);

  Lines := TStringList.Create;
  try
    Lines.Text := CopyText(False);
    Assert.AreEqual(2, Lines.Count, 'exactly the two selected rows');
    Assert.IsTrue(Lines[0].StartsWith('2'#9'beta'), 'row ID 2 first');
    Assert.IsTrue(Lines[1].StartsWith('4'#9'Alpha'), 'row ID 4 second');
    // Row 4 Amount = 50.25 -> invariant cell in the third column
    Assert.IsTrue(Lines[1].Contains(#9'50.25'#9), 'invariant currency value');
  finally
    Lines.Free;
  end;
end;

procedure TVittixClipboardCopyTests.HeadersOnlyWhenRequested;
var
  Text: string;
  Lines: TStringList;
begin
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(1);
  SelectRow(2);

  Lines := TStringList.Create;
  try
    Text := CopyText(False);
    // Row 1 carries a quoted embedded line break, so a raw line split would
    // cut it; assert on the prefix and on the second row's values instead.
    Assert.IsTrue(Text.StartsWith('1'#9'Alpha'),
      'no header row by default: output starts with the first selected row');
    Assert.IsTrue(Text.Contains(#9'200'#9),
      'the second selected row is present');

    Text := CopyText(True);
    Lines.Text := Text;
    Assert.AreEqual('ID'#9'Name'#9'Amount'#9'Score'#9'Notes'#9'Created'#9'IsActive',
      Lines[0], 'header row of the visible columns');
  finally
    Lines.Free;
  end;
end;

procedure TVittixClipboardCopyTests.CtrlShiftCForcesHeadersThroughController;
var
  Key: Word;
  Text: string;
begin
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(2);
  SelectRow(3);

  Key := Ord('C');
  Assert.IsTrue(FGrid.Controller.DoKeyDown(Key, [ssCtrl, ssShift]),
    'Ctrl+Shift+C is consumed by the controller');
  Text := VittixGetClipboardText;
  Assert.IsTrue(Text.StartsWith('ID'#9'Name'),
    'Ctrl+Shift+C always includes the header row');
end;

procedure TVittixClipboardCopyTests.HiddenColumnsExcluded;
var
  Lines: TStringList;
  Tabs, I: Integer;
begin
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  FGrid.Controller.FindColumnByFieldName('Notes').Visible := False;
  FGrid.Controller.FindColumnByFieldName('Created').Visible := False;
  SelectRow(1);
  SelectRow(2);

  Lines := TStringList.Create;
  try
    Lines.Text := CopyText(True);
    Assert.AreEqual('ID'#9'Name'#9'Amount'#9'Score'#9'IsActive', Lines[0],
      'only the visible columns, in grid order');
    Tabs := 0;
    for I := 1 to Length(Lines[0]) do
      if Lines[0][I] = #9 then
        Inc(Tabs);
    Assert.AreEqual(4, Tabs, 'five visible fields -> four delimiters');
    Assert.IsFalse(Lines[1].Contains('first'), 'hidden Notes content excluded');
  finally
    Lines.Free;
  end;
end;

procedure TVittixClipboardCopyTests.NullValuesExportAsEmptyFields;
var
  Lines: TStringList;
begin
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(2);
  SelectRow(3); // Amount is NULL on row 3

  Lines := TStringList.Create;
  try
    Lines.Text := CopyText(False);
    // 3 <tab> Gamma <tab> <empty Amount> <tab> 1 ... (row 3 may sort after
    // row 2 in the selection walk, so search the whole text)
    Assert.IsTrue(Lines.Text.Contains('3'#9'Gamma'#9#9'1'),
      'a null value yields an empty field, not the null text marker');
  finally
    Lines.Free;
  end;
end;

procedure TVittixClipboardCopyTests.TabsQuotesAndNewlinesAreQuoted;
var
  Text: string;
begin
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := 'a'#9'b';
  FDataSet.FieldByName('Notes').AsString := 'say "hi"' + sLineBreak + 'bye';
  FDataSet.Post;

  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(1);
  SelectRow(2);

  Text := CopyText(True);
  // Values containing a delimiter/quote/line break are quoted, inner
  // quotes doubled (Excel TSV convention).
  Assert.IsTrue(Text.Contains('"a'#9'b"'), 'a tab inside a value is quoted');
  Assert.IsTrue(Text.Contains('"say ""hi""' + sLineBreak + 'bye"'),
    'quotes and a line break inside one value are quoted and doubled');
end;

procedure TVittixClipboardCopyTests.NumericValuesKeepInvariantPrecision;
var
  Copy: string;
begin
  FDataSet.Edit;
  FDataSet.FieldByName('Score').AsFloat := 1234.5678;
  FDataSet.Post;

  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(1);
  SelectRow(4);

  Copy := CopyText(False);
  Assert.IsTrue(Copy.Contains(#9'1234.5678'#9),
    'full precision invariant formatting');
  Assert.IsFalse(Copy.Contains('1234.57'),
    'display rounding must not leak into the copy');
end;

procedure TVittixClipboardCopyTests.LocaleOptionSwitchesToDisplayText;
var
  Exporter: TVittixDBGridExporter;
begin
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(1);
  SelectRow(2);

  // The locale opt-out lives on the exporter options; the same public copy
  // builder must then emit the display text instead of the invariant value.
  Exporter := TVittixDBGridExporter.Create(FGrid);
  try
    Exporter.Options.ExportLocaleFormat := True;
    Assert.IsTrue(Exporter.BuildCopyText(False).Contains(#9'3.50'#9),
      'Score 3.5 with FloatFormat 0.00 becomes the display text');
  finally
    Exporter.Free;
  end;
end;

procedure TVittixClipboardCopyTests.ClipboardRoundTrip;
var
  Key: Word;
begin
  Key := Ord('C');
  Assert.IsTrue(FGrid.Controller.DoKeyDown(Key, [ssCtrl]),
    'plain Ctrl+C is consumed by the controller');
  // No multi-row selection here: the copy is the single current cell value.
  Assert.AreEqual('1', VittixGetClipboardText,
    'the clipboard holds the current cell copy');
end;

procedure TVittixClipboardCopyTests.CursorPreservedAfterCopy;
var
  Key: Word;
begin
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(2);
  SelectRow(5);

  Key := Ord('C');
  FGrid.Controller.DoKeyDown(Key, [ssCtrl]);
  Assert.AreEqual(5, FDataSet.FieldByName('ID').AsInteger,
    'the copy restores the cursor to the record it started on');
end;

procedure TVittixClipboardCopyTests.ShouldBypassFunctionTruthTable;
begin
  // The decision is isolated so it can be verified without an editor UI
  Assert.IsTrue(TVittixDBGridController.ShouldBypassClipboardShortcut(True, dsBrowse),
    'editor up: the editor owns the clipboard keys');
  Assert.IsTrue(TVittixDBGridController.ShouldBypassClipboardShortcut(True, dsInsert),
    'editor up during insert');
  Assert.IsTrue(TVittixDBGridController.ShouldBypassClipboardShortcut(False, dsEdit),
    'pending edit: the grid copy must not walk bookmarks');
  Assert.IsTrue(TVittixDBGridController.ShouldBypassClipboardShortcut(False, dsInsert),
    'pending insert');
  Assert.IsFalse(TVittixDBGridController.ShouldBypassClipboardShortcut(False, dsBrowse),
    'browsing: the grid copy runs');
  Assert.IsFalse(TVittixDBGridController.ShouldBypassClipboardShortcut(False, dsInactive),
    'closed dataset: no bypass needed (DoKeyPress handles safety)');
end;

procedure TVittixClipboardCopyTests.ClipboardBypassDuringDatasetEdit;
var
  Key: Word;
begin
  FDataSet.Edit;
  try
    Key := Ord('C');
    Assert.IsFalse(FGrid.Controller.DoKeyDown(Key, [ssCtrl]),
      'Ctrl+C during an edit is not consumed by the grid copy');
    Assert.AreEqual(Ord('C'), Key,
      'the key is left untouched for the editor/normal processing');

    Key := Ord('A');
    Assert.IsFalse(FGrid.Controller.DoKeyDown(Key, [ssCtrl]),
      'Ctrl+A during an edit is not consumed either');
  finally
    FDataSet.Cancel;
  end;
end;

procedure TVittixClipboardCopyTests.ClipboardBypassWithInplaceEditor;
var
  Key: Word;
begin
  FGrid.HandleNeeded;
  FDataSet.Edit;
  try
    FGrid.EditorMode := True;
    try
      Key := Ord('C');
      Assert.IsFalse(FGrid.Controller.DoKeyDown(Key, [ssCtrl]),
        'with the in-place editor up, Ctrl+C belongs to the editor');
      Assert.AreEqual(Ord('C'), Key);
    finally
      FGrid.EditorMode := False;
    end;
  finally
    FDataSet.Cancel;
  end;
end;

procedure TVittixClipboardCopyTests.FooterSelectionAggregateSumAndCount;
var
  Footer: TVittixDBGridFooterPanel;
  AmountCol: TColumn;
  NameCol: TColumn;
begin
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(1); // Amount 100.50
  SelectRow(2); // Amount 200.00

  AmountCol := FGrid.Controller.FindColumnByFieldName('Amount');
  NameCol := FGrid.Controller.FindColumnByFieldName('Name');

  // The controller owns its own footer panel; tests attach a detached one
  // over the same grid/engine, like the regression suite does.
  Footer := TVittixDBGridFooterPanel.Create(nil);
  try
    Footer.Attach(FGrid, FGrid.Controller.AggregationEngine);
    Assert.IsTrue(Footer.SelectionAggregateActive);
    Assert.AreEqual('Sum: 300.50', Footer.GetSelectionAggregateText(AmountCol));
    Assert.AreEqual('Count: 2', Footer.GetSelectionAggregateText(NameCol));
  finally
    Footer.Free;
  end;
end;

procedure TVittixClipboardCopyTests.FooterSelectionAggregateLeavesDatasetAggregationUntouched;
var
  Footer: TVittixDBGridFooterPanel;
  AmountCol: TColumn;
begin
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  FGrid.Controller.SetColumnAggregation(
    FGrid.Controller.FindColumnByFieldName('Amount'), vatSum);
  SelectRow(1);
  SelectRow(2);

  AmountCol := FGrid.Controller.FindColumnByFieldName('Amount');
  Footer := TVittixDBGridFooterPanel.Create(nil);
  try
    Footer.Attach(FGrid, FGrid.Controller.AggregationEngine);
    Assert.AreEqual('Sum: 300.50', Footer.GetSelectionAggregateText(AmountCol),
      'the selection aggregate covers only the selected rows');
    Assert.AreEqual('750.75', FGrid.Controller.FooterDisplayText('Amount'),
      'the dataset-wide aggregation state is untouched');
  finally
    Footer.Free;
  end;
end;

procedure TVittixClipboardCopyTests.FooterNoSelectionAggregateWithoutMultiSelect;
var
  Footer: TVittixDBGridFooterPanel;
  AmountCol: TColumn;
begin
  // One row selected is not a multi-row selection
  FGrid.Options := FGrid.Options + [dgMultiSelect];
  SelectRow(1);

  AmountCol := FGrid.Controller.FindColumnByFieldName('Amount');
  Footer := TVittixDBGridFooterPanel.Create(nil);
  try
    Footer.Attach(FGrid, FGrid.Controller.AggregationEngine);
    Assert.IsFalse(Footer.SelectionAggregateActive);
    Assert.AreEqual('', Footer.GetSelectionAggregateText(AmountCol));
  finally
    Footer.Free;
  end;
end;

end.
