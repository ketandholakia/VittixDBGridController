unit Vittix.Tests.IncrementalSearch;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Diagnostics,
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  Vcl.DBGrids,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Controller;

type
  // Exposes the protected KeyPress override so tests can simulate typing
  // without a real keyboard focus.
  TVittixGridKeyAccess = class(TVittixDBGrid)
  public
    procedure PressKey(var Key: Char);
    /// <summary>Routes the key through the controller exactly like KeyPress
    /// does and reports whether it was consumed (True) or left for the grid
    /// and its in-place editor (False).</summary>
    function TryPressKey(var Key: Char): Boolean;
  end;

  // Simulates a dataset whose Locate is unsupported (raises), forcing the
  // controller onto its cursor-restoring fallback scan.
  TNoLocateDataSet = class(TClientDataSet)
  public
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
  end;

  // Counts Locate calls so tests can prove which search path ran.
  TCountingLocateDataSet = class(TClientDataSet)
  public
    LocateCount: Integer;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
  end;

  /// <summary>
  /// Search path selection (wide-string Locate fast path vs fallback scan)
  /// and scan performance on a large dataset.
  /// </summary>
  [TestFixture]
  TVittixSearchPathTests = class
  private
    FOwnerForm: TForm;
    procedure SendKey(AGrid: TVittixDBGrid; Ch: Char);
  public
    [Test]
    procedure WideFieldUsesLocateFastPath;
    [Test]
    procedure NarrowFieldWithUnicodeBufferSkipsLocate;
    [Test]
    procedure AsciiBufferUsesLocateOnNarrowField;
    [Test]
    procedure LargeDatasetScanStaysFast;
  end;

  [TestFixture]
  TVittixIncrementalSearchTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FEventText: string;
    FEventFound: Boolean;
    FEventCount: Integer;
    procedure HandleSearch(Sender: TObject; const SearchText: string;
      Found: Boolean);
    procedure SendKey(Ch: Char);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure MatchMovesToFirstPrefixRecord;
    [Test]
    procedure NoMatchKeepsPositionAndBuffer;
    [Test]
    procedure BackspaceShortensBufferAndRelocates;
    [Test]
    procedure EscResetsBuffer;
    [Test]
    procedure TimeoutExpiryStartsFreshSearch;
    [Test]
    procedure UnsortedGridSearchesCurrentColumn;
    [Test]
    procedure SortedColumnSearchesSortColumn;
    [Test]
    procedure EditModeBypassesSearch;
    [Test]
    procedure FilteredDatasetSearchesVisibleRecordsOnly;
    [Test]
    procedure ClosedDatasetIsSafeAndResetsBuffer;
    [Test]
    procedure DisabledSearchPassesKeyThrough;
    [Test]
    procedure EventFiresWithFoundState;
    [Test]
    procedure FallbackScanLocatesWithoutLocateSupport;
    [Test]
    procedure FallbackScanRestoresPositionOnNoMatch;
  end;

  /// <summary>
  /// Unicode correctness of the type-ahead search: accented Latin,
  /// Cyrillic, Greek and the caseless Indic scripts must match
  /// case-insensitively without corrupting the text; surrogate pairs must
  /// enter and leave the buffer as one unit.
  /// </summary>
  [TestFixture]
  TVittixUnicodeSearchTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FEventCount: Integer;
    procedure HandleSearch(Sender: TObject; const SearchText: string;
      Found: Boolean);
    procedure SendKey(Ch: Char);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure AccentedLatinMatchesCaseInsensitively;
    [Test]
    procedure AccentedLatinNoMatchKeepsPosition;
    [Test]
    procedure CyrillicMatchesCaseInsensitively;
    [Test]
    procedure GreekMatchesCaseInsensitively;
    [Test]
    procedure IndicScriptsMatchWithoutCaseCorruption;
    [Test]
    procedure IndicCharSequenceTriggersSearchPerChar;
    [Test]
    procedure SurrogatePairIsDeferredThenSearchedAsOneUnit;
    [Test]
    procedure BackspaceRemovesCompleteSurrogatePair;
    [Test]
    procedure BackspaceRemovesSingleCombiningMark;
  end;

  /// <summary>Phase 2: IncrementalSearchMode decides whether a printable
  /// key is consumed for type-ahead or left to the in-place editor.</summary>
  [TestFixture]
  TVittixIncrementalSearchModeTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    function TypeKey(Ch: Char): Boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure EditableGridDoesNotConsumeKeyInAuto;
    [Test]
    procedure ReadOnlyGridSearchesInAuto;
    [Test]
    procedure NoEditingOptionSearchesInAuto;
    [Test]
    procedure ReadOnlyColumnSearchesInAuto;
    [Test]
    procedure AlwaysModeConsumesOnEditableGrid;
    [Test]
    procedure OffModePassesKeyThrough;
    [Test]
    procedure IncrementalSearchFalseStillDisables;
  end;

implementation

uses
  Vittix.Tests.TestData;

{ helpers }

procedure TVittixGridKeyAccess.PressKey(var Key: Char);
begin
  KeyPress(Key);
end;

function TVittixGridKeyAccess.TryPressKey(var Key: Char): Boolean;
begin
  Result := Assigned(Controller) and Controller.DoKeyPress(Key);
end;

function TNoLocateDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  // Raise exactly like a dataset that cannot locate
  raise EDatabaseError.CreateFmt(
    '%s does not support Locate', [ClassName]);
end;

function TCountingLocateDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  Inc(LocateCount);
  Result := inherited Locate(KeyFields, KeyValues, Options);
end;

{ TVittixSearchPathTests }

procedure TVittixSearchPathTests.SendKey(AGrid: TVittixDBGrid; Ch: Char);
begin
  TVittixGridKeyAccess(AGrid).PressKey(Ch);
end;

procedure TVittixSearchPathTests.WideFieldUsesLocateFastPath;
var
  DataSet: TCountingLocateDataSet;
  Grid: TVittixDBGrid;
begin
  // Midas keeps strings wide internally, so ftWideString Locate must match
  // Unicode case-insensitively without the codepage collapse ftString has.
  DataSet := TCountingLocateDataSet.Create(nil);
  try
    DataSet.FieldDefs.Add('ID', ftInteger);
    DataSet.FieldDefs.Add('Name', ftWideString, 60);
    DataSet.CreateDataSet;
    DataSet.AppendRecord([1, 'Émile']);
    DataSet.AppendRecord([2, 'élan']);
    DataSet.AppendRecord([3, 'Привет']);
    DataSet.AppendRecord([4, 'नमस्ते']);
    DataSet.AppendRecord([5, 'નમસ્તે']);

    Grid := CreateHeadlessGrid(DataSet, FOwnerForm);
    Grid.IncrementalSearchMode := ismAlways;
    try
      Grid.Controller.DoTitleClick(Grid.Columns[1]);

      SendKey(Grid, Chr($043F)); // lower-case Cyrillic pe
      Assert.AreEqual(1, DataSet.LocateCount,
        'wide field + non-ASCII buffer takes the Locate fast path');
      Assert.AreEqual('Привет', DataSet.FieldByName('Name').AsString,
        'the fast path folds Cyrillic case correctly');

      SendKey(Grid, #27);
      SendKey(Grid, Chr($00E9)); // e-acute
      Assert.AreEqual(2, DataSet.LocateCount);
      Assert.IsTrue((DataSet.FieldByName('Name').AsString = 'Émile') or
        (DataSet.FieldByName('Name').AsString = 'élan'),
        'the fast path folds accented Latin correctly');
    finally
      FOwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixSearchPathTests.NarrowFieldWithUnicodeBufferSkipsLocate;
var
  DataSet: TCountingLocateDataSet;
  Grid: TVittixDBGrid;
begin
  DataSet := TCountingLocateDataSet.Create(nil);
  try
    DataSet.FieldDefs.Add('ID', ftInteger);
    DataSet.FieldDefs.Add('Name', ftString, 60);
    DataSet.CreateDataSet;
    DataSet.AppendRecord([1, 'Alpha']);
    DataSet.AppendRecord([2, 'beta']);

    Grid := CreateHeadlessGrid(DataSet, FOwnerForm);
    Grid.IncrementalSearchMode := ismAlways;
    try
      Grid.Controller.DoTitleClick(Grid.Columns[1]);

      SendKey(Grid, Chr($00E9)); // e-acute: no codepage-safe Locate on ftString
      Assert.AreEqual(0, DataSet.LocateCount,
        'non-ASCII buffer on an ANSI field must bypass Locate');
      Assert.AreEqual('beta', DataSet.FieldByName('Name').AsString,
        'the scan runs instead and finds nothing, keeping the position');
      Assert.AreEqual<NativeInt>(1, Length(Grid.Controller.IncrementalSearchBuffer),
        'the buffer survives the failed search');
    finally
      FOwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixSearchPathTests.AsciiBufferUsesLocateOnNarrowField;
var
  DataSet: TCountingLocateDataSet;
  Grid: TVittixDBGrid;
begin
  DataSet := TCountingLocateDataSet.Create(nil);
  try
    DataSet.FieldDefs.Add('ID', ftInteger);
    DataSet.FieldDefs.Add('Name', ftString, 60);
    DataSet.CreateDataSet;
    DataSet.AppendRecord([1, 'Alpha']);
    DataSet.AppendRecord([2, 'beta']);

    Grid := CreateHeadlessGrid(DataSet, FOwnerForm);
    Grid.IncrementalSearchMode := ismAlways;
    try
      Grid.Controller.DoTitleClick(Grid.Columns[1]);

      SendKey(Grid, 'B');
      Assert.AreEqual(1, DataSet.LocateCount,
        'ASCII buffer keeps the Locate fast path on any field');
      Assert.AreEqual('beta', DataSet.FieldByName('Name').AsString);
    finally
      FOwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixSearchPathTests.LargeDatasetScanStaysFast;
var
  DataSet: TNoLocateDataSet;
  Grid: TVittixDBGrid;
  I: Integer;
  Watch: TStopwatch;
  Key: Char;
  ElapsedMs: Int64;
begin
  // 50k rows, UNSORTED, with Name as the first column (so it is the search
  // column without a sort) and the target as the LAST record: the scan is a
  // genuine full sweep. Sorting would park the accented target near the
  // index start and make the measurement meaningless.
  DataSet := TNoLocateDataSet.Create(nil);
  try
    DataSet.FieldDefs.Add('Name', ftWideString, 60);
    DataSet.FieldDefs.Add('ID', ftInteger);
    DataSet.CreateDataSet;
    DataSet.DisableControls;
    try
      for I := 1 to 50000 do
        DataSet.AppendRecord([Format('Row %d plain', [I]), I]);
      DataSet.AppendRecord(['Émile dernière', 50001]);
    finally
      DataSet.EnableControls;
    end;

    Grid := CreateHeadlessGrid(DataSet, FOwnerForm);
    Grid.IncrementalSearchMode := ismAlways;
    try
      Watch := TStopwatch.StartNew;
      Key := Chr($00C9); // E-acute
      TVittixGridKeyAccess(Grid).PressKey(Key);
      ElapsedMs := Watch.ElapsedMilliseconds;

      Assert.AreEqual('Émile dernière', DataSet.FieldByName('Name').AsString,
        'the full-dataset scan found the last record');
      // Generous bound so slow CI machines stay green.
      Assert.IsTrue(ElapsedMs < 10000,
        Format('scan took %d ms for 50k rows', [ElapsedMs]));
      Writeln(Format('50k-row Unicode prefix scan: %d ms', [ElapsedMs]));
    finally
      FOwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

{ TVittixIncrementalSearchTests }

procedure TVittixIncrementalSearchTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  // These tests exercise the search itself; ismAuto (the new default) would
  // hand printable keys to the writable grid's in-place editor.
  FGrid.IncrementalSearchMode := ismAlways;
  FEventCount := 0;
  FEventText := '';
  FEventFound := False;
end;

procedure TVittixIncrementalSearchTests.TearDown;
begin
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixIncrementalSearchTests.HandleSearch(Sender: TObject;
  const SearchText: string; Found: Boolean);
begin
  Inc(FEventCount);
  FEventText := SearchText;
  FEventFound := Found;
end;

procedure TVittixIncrementalSearchTests.SendKey(Ch: Char);
begin
  TVittixGridKeyAccess(FGrid).PressKey(Ch);
end;

procedure TVittixIncrementalSearchTests.MatchMovesToFirstPrefixRecord;
begin
  // Unsorted fixture: the searched column is the ID column ('1'..'5').
  SendKey('3');
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger,
    'the first record whose current column starts with the buffer');
  Assert.AreEqual('3', FGrid.Controller.IncrementalSearchBuffer);
end;

procedure TVittixIncrementalSearchTests.NoMatchKeepsPositionAndBuffer;
begin
  SendKey('Z');
  Assert.AreEqual(1, FDataSet.FieldByName('ID').AsInteger,
    'a failed search keeps the cursor on the current record');
  Assert.AreEqual('Z', FGrid.Controller.IncrementalSearchBuffer,
    'the buffer survives a failed search so Backspace can recover');
end;

procedure TVittixIncrementalSearchTests.BackspaceShortensBufferAndRelocates;
begin
  SendKey('3');
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger);
  SendKey('5');
  Assert.AreEqual('35', FGrid.Controller.IncrementalSearchBuffer);
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger,
    'no record starts with 35; cursor stays');
  SendKey(#8);
  Assert.AreEqual('3', FGrid.Controller.IncrementalSearchBuffer,
    'backspace removes the last character');
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger,
    'relocating the shortened buffer keeps the match');
  SendKey(#8);
  Assert.AreEqual('', FGrid.Controller.IncrementalSearchBuffer,
    'empty buffer after removing the last character');
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger,
    'an empty buffer does not move the cursor');
end;

procedure TVittixIncrementalSearchTests.EscResetsBuffer;
begin
  SendKey('3');
  Assert.AreEqual('3', FGrid.Controller.IncrementalSearchBuffer);
  SendKey(#27);
  Assert.AreEqual('', FGrid.Controller.IncrementalSearchBuffer,
    'Esc clears the buffer');
end;

procedure TVittixIncrementalSearchTests.TimeoutExpiryStartsFreshSearch;
begin
  FGrid.IncrementalSearchTimeout := 0;
  SendKey('3');
  Assert.AreEqual('3', FGrid.Controller.IncrementalSearchBuffer);
  SendKey('5');
  // Timeout 0 expires the '3' immediately: the '5' must start a fresh
  // search (finding ID 5), not accumulate into a doomed '35'.
  Assert.AreEqual('5', FGrid.Controller.IncrementalSearchBuffer,
    'an expired buffer restarts fresh');
  Assert.AreEqual(5, FDataSet.FieldByName('ID').AsInteger,
    'the fresh buffer locates its own record');
end;

procedure TVittixIncrementalSearchTests.UnsortedGridSearchesCurrentColumn;
begin
  // Headless grid: no current column selected, so the search falls back to
  // the first column (ID). SendKey('4') must locate ID 4.
  SendKey('4');
  Assert.AreEqual(4, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixIncrementalSearchTests.SortedColumnSearchesSortColumn;
begin
  // Sort by Name ascending through the same interactive path a header click
  // uses: Alpha, Alpha, beta, Delta, Gamma.
  FGrid.Controller.DoTitleClick(FGrid.Columns[1]);
  Assert.AreEqual('Alpha', FDataSet.FieldByName('Name').AsString,
    'precondition: dataset sorted by Name');

  SendKey('D');
  Assert.AreEqual('Delta', FDataSet.FieldByName('Name').AsString,
    'the search follows the sort column, not the current column');
  Assert.AreEqual('D', FGrid.Controller.IncrementalSearchBuffer);

  SendKey('E');
  Assert.AreEqual('Delta', FDataSet.FieldByName('Name').AsString,
    'DE has no match; cursor stays on Delta');
end;

procedure TVittixIncrementalSearchTests.EditModeBypassesSearch;
var
  Key: Char;
begin
  FDataSet.Edit;
  try
    Key := 'A';
    TVittixGridKeyAccess(FGrid).PressKey(Key);
    Assert.AreEqual('', FGrid.Controller.IncrementalSearchBuffer,
      'an active edit must not accumulate a search buffer');
    Assert.AreNotEqual(#0, Key, 'the keystroke is not consumed');
  finally
    FDataSet.Cancel;
  end;
end;

procedure TVittixIncrementalSearchTests.FilteredDatasetSearchesVisibleRecordsOnly;
var
  Info: TVittixDBGridColumnInfo;
begin
  // Name = 'Alpha': visible records are ID 1 and ID 4.
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FGrid.Controller.FilterEngine.Active := True;

  // ID 3 is filtered out and must not be reachable by the search.
  SendKey('3');
  Assert.AreEqual(1, FDataSet.FieldByName('ID').AsInteger,
    'records hidden by the filter are not located');

  SendKey(#27);
  SendKey('4');
  Assert.AreEqual(4, FDataSet.FieldByName('ID').AsInteger,
    'a visible record is located');
end;

procedure TVittixIncrementalSearchTests.ClosedDatasetIsSafeAndResetsBuffer;
var
  Key: Char;
begin
  SendKey('4');
  Assert.AreEqual('4', FGrid.Controller.IncrementalSearchBuffer);

  FDataSet.Close;
  Key := 'A';
  TVittixGridKeyAccess(FGrid).PressKey(Key);
  Assert.AreEqual('', FGrid.Controller.IncrementalSearchBuffer,
    'a closed dataset resets the pending buffer');
  Assert.AreNotEqual(#0, Key, 'the keystroke passes through untouched');
  Assert.WillNotRaise(
    procedure
    var
      K: Char;
    begin
      K := 'B';
      TVittixGridKeyAccess(FGrid).PressKey(K);
    end);

  FDataSet.Open;
  Assert.AreEqual('', FGrid.Controller.IncrementalSearchBuffer,
    'reopening starts with an empty buffer');
end;

procedure TVittixIncrementalSearchTests.DisabledSearchPassesKeyThrough;
var
  Key: Char;
begin
  FGrid.IncrementalSearch := False;
  Key := 'A';
  TVittixGridKeyAccess(FGrid).PressKey(Key);
  Assert.AreEqual('', FGrid.Controller.IncrementalSearchBuffer);
  Assert.AreNotEqual(#0, Key, 'the key reaches normal grid processing');
end;

procedure TVittixIncrementalSearchTests.EventFiresWithFoundState;
begin
  FGrid.OnIncrementalSearch := HandleSearch;

  SendKey('3');
  Assert.AreEqual(1, FEventCount);
  Assert.AreEqual('3', FEventText);
  Assert.IsTrue(FEventFound);

  SendKey('9');
  Assert.AreEqual(2, FEventCount);
  Assert.AreEqual('39', FEventText);
  Assert.IsFalse(FEventFound);

  SendKey(#27);
  Assert.AreEqual(3, FEventCount);
  Assert.AreEqual('', FEventText, 'a reset fires with an empty buffer');
  Assert.IsFalse(FEventFound);
end;

procedure TVittixIncrementalSearchTests.FallbackScanLocatesWithoutLocateSupport;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Key: Char;
begin
  DataSet := TNoLocateDataSet.Create(nil);
  try
    // Rebuild the fixture rows on the no-Locate double
    DataSet.FieldDefs.Add('ID', ftInteger);
    DataSet.FieldDefs.Add('Name', ftString, 50);
    DataSet.CreateDataSet;
    DataSet.AppendRecord([1, 'Alpha']);
    DataSet.AppendRecord([2, 'beta']);
    DataSet.AppendRecord([3, 'Gamma']);

    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      Key := 'G';
      TVittixGridKeyAccess(Grid).PressKey(Key);
      Assert.AreEqual('Gamma', DataSet.FieldByName('Name').AsString,
        'the fallback scan finds the first prefix match');

      Key := #27;
      TVittixGridKeyAccess(Grid).PressKey(Key);
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TVittixIncrementalSearchTests.FallbackScanRestoresPositionOnNoMatch;
var
  DataSet: TClientDataSet;
  OwnerForm: TForm;
  Grid: TVittixDBGrid;
  Key: Char;
begin
  DataSet := TNoLocateDataSet.Create(nil);
  try
    DataSet.FieldDefs.Add('ID', ftInteger);
    DataSet.FieldDefs.Add('Name', ftString, 50);
    DataSet.CreateDataSet;
    DataSet.AppendRecord([1, 'Alpha']);
    DataSet.AppendRecord([2, 'beta']);

    Grid := CreateHeadlessGrid(DataSet, OwnerForm);
    try
      DataSet.First;
      DataSet.Next; // position on ID 2 without Locate (the double raises)
      Key := 'Z';
      TVittixGridKeyAccess(Grid).PressKey(Key);
      Assert.AreEqual(2, DataSet.FieldByName('ID').AsInteger,
        'a failed fallback scan restores the starting position');
    finally
      OwnerForm.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

{ =============================================================================
  Unicode correctness (accented Latin, Cyrillic, Greek, Indic) and
  surrogate-pair / combining-mark buffer handling.
  ============================================================================= }

{ TVittixUnicodeSearchTests }

procedure TVittixUnicodeSearchTests.Setup;
begin
  FDataSet := TClientDataSet.Create(nil);
  FDataSet.FieldDefs.Add('ID', ftInteger);
  // ftWideString: an ftString field converts through the system codepage,
  // which collapses every non-ANSI character to '?' before the search ever
  // runs.
  FDataSet.FieldDefs.Add('Name', ftWideString, 60);
  FDataSet.CreateDataSet;
  // Accented Latin in both casings, Cyrillic, Greek, Devanagari, Gujarati.
  FDataSet.AppendRecord([1, 'Émile']);
  FDataSet.AppendRecord([2, 'élan']);
  FDataSet.AppendRecord([3, 'Привет']);
  FDataSet.AppendRecord([4, 'Γειά σου']);
  FDataSet.AppendRecord([5, 'नमस्ते']);
  FDataSet.AppendRecord([6, 'નમસ્તે']);

  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  FGrid.IncrementalSearchMode := ismAlways;
  // Sort by Name so the type-ahead search runs against the text column
  // (an unsorted headless grid falls back to the numeric ID column).
  FGrid.Controller.DoTitleClick(FGrid.Columns[1]);
  FEventCount := 0;
end;

procedure TVittixUnicodeSearchTests.TearDown;
begin
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixUnicodeSearchTests.HandleSearch(Sender: TObject;
  const SearchText: string; Found: Boolean);
begin
  Inc(FEventCount);
end;

procedure TVittixUnicodeSearchTests.SendKey(Ch: Char);
begin
  TVittixGridKeyAccess(FGrid).PressKey(Ch);
end;

procedure TVittixUnicodeSearchTests.AccentedLatinMatchesCaseInsensitively;
begin
  // Lower-case buffer against a capital-É record and vice versa
  SendKey(Chr($00E9)); // e-acute
  Assert.IsTrue((FDataSet.FieldByName('Name').AsString = 'Émile') or
    (FDataSet.FieldByName('Name').AsString = 'élan'),
    'lower-case é must match both casings, got ' +
    FDataSet.FieldByName('Name').AsString);

  SendKey(#27);
  SendKey(Chr($00C9)); // E-acute
  Assert.IsTrue((FDataSet.FieldByName('Name').AsString = 'Émile') or
    (FDataSet.FieldByName('Name').AsString = 'élan'),
    'capital É must match the lower-case records');
end;

procedure TVittixUnicodeSearchTests.AccentedLatinNoMatchKeepsPosition;
var
  MatchedName: string;
begin
  SendKey(Chr($00E9)); // e-acute // lands on one of the accented records
  MatchedName := FDataSet.FieldByName('Name').AsString;

  SendKey('Z'); // extends to 'éZ': no record matches, cursor must stay
  Assert.AreEqual(MatchedName, FDataSet.FieldByName('Name').AsString,
    'a failed search keeps the cursor on the matched record');
  Assert.AreEqual('éZ', FGrid.Controller.IncrementalSearchBuffer,
    'the buffer survives a failed search');
end;

procedure TVittixUnicodeSearchTests.CyrillicMatchesCaseInsensitively;
begin
  SendKey(Chr($041F)); // capital Pe // capital
  Assert.AreEqual('Привет', FDataSet.FieldByName('Name').AsString,
    'capital П locates the record');

  SendKey(#27);
  SendKey(Chr($043F)); // lower-case pe // lower-case
  Assert.AreEqual('Привет', FDataSet.FieldByName('Name').AsString,
    'lower-case п must fold onto П');
end;

procedure TVittixUnicodeSearchTests.GreekMatchesCaseInsensitively;
begin
  SendKey(Chr($03B3)); // lower-case gamma // lower-case gamma against a capital-Γ record
  Assert.AreEqual('Γειά σου', FDataSet.FieldByName('Name').AsString,
    'lower-case γ must fold onto Γ');

  SendKey(#27);
  SendKey(Chr($0393)); // capital gamma
  Assert.AreEqual('Γειά σου', FDataSet.FieldByName('Name').AsString,
    'capital Γ locates the record directly');
end;

procedure TVittixUnicodeSearchTests.IndicScriptsMatchWithoutCaseCorruption;
begin
  // Devanagari and Gujarati are caseless: matching must be an exact prefix
  // test with no case folding mangling the code points.
  SendKey(Chr($0928)); // न
  Assert.AreEqual('नमस्ते', FDataSet.FieldByName('Name').AsString,
    'Devanagari prefix locates the record');

  SendKey(#27);
  SendKey(Chr($0AA8)); // ન
  Assert.AreEqual('નમસ્તે', FDataSet.FieldByName('Name').AsString,
    'Gujarati prefix locates the record');

  // The SECOND character of the Devanagari word is म: as a prefix it must
  // not match anything (proves the match is positional, not contains).
  SendKey(#27);
  SendKey(Chr($092E)); // म
  Assert.AreEqual('નમસ્તે', FDataSet.FieldByName('Name').AsString,
    'a mid-word character is not a prefix match');
end;

procedure TVittixUnicodeSearchTests.IndicCharSequenceTriggersSearchPerChar;
begin
  FGrid.OnIncrementalSearch := HandleSearch;

  // A keyboard hook injecting WM_CHAR messages delivers one Char at a
  // time; each BMP character must immediately trigger a search.
  SendKey(Chr($0928)); // न
  Assert.AreEqual(1, FEventCount, 'first char searched');
  Assert.AreEqual('नमस्ते', FDataSet.FieldByName('Name').AsString);

  SendKey(Chr($092E)); // म
  Assert.AreEqual(2, FEventCount, 'second char searched');
  Assert.AreEqual('नमस्ते', FDataSet.FieldByName('Name').AsString,
    'नम is still a prefix of नमस्ते');

  SendKey(Chr($0938)); // स
  Assert.AreEqual(3, FEventCount, 'third char searched');
  Assert.AreEqual('नमस्ते', FDataSet.FieldByName('Name').AsString);
  Assert.AreEqual('नमस', FGrid.Controller.IncrementalSearchBuffer);
end;

procedure TVittixUnicodeSearchTests.SurrogatePairIsDeferredThenSearchedAsOneUnit;
var
  High, Low: Char;
begin
  FGrid.OnIncrementalSearch := HandleSearch;

  High := Char($D83D); // high half of U+1F600
  Low := Char($DE00);

  SendKey(High);
  Assert.AreEqual<NativeInt>(1, Length(FGrid.Controller.IncrementalSearchBuffer),
    'the high surrogate enters the buffer');
  Assert.AreEqual(0, FEventCount,
    'a lone high surrogate must not trigger a (guaranteed-miss) search');

  SendKey(Low);
  Assert.AreEqual<NativeInt>(2, Length(FGrid.Controller.IncrementalSearchBuffer),
    'the low surrogate completes the pair in the buffer');
  Assert.AreEqual(1, FEventCount,
    'the completed pair triggers exactly one search');
end;

procedure TVittixUnicodeSearchTests.BackspaceRemovesCompleteSurrogatePair;
var
  High, Low: Char;
begin
  High := Char($D83D);
  Low := Char($DE00);

  SendKey(High);
  SendKey(Low);
  SendKey(#8);
  Assert.AreEqual<NativeInt>(0, Length(FGrid.Controller.IncrementalSearchBuffer),
    'backspace removes both halves of the pair, never a lone surrogate');

  SendKey('a');
  SendKey(High);
  SendKey(Low);
  SendKey(#8);
  Assert.AreEqual('a', FGrid.Controller.IncrementalSearchBuffer,
    'the pair before backspace is removed as one unit');

  // A lone high surrogate (no low half arrived) is removed as one Char.
  SendKey(High);
  SendKey(#8);
  Assert.AreEqual('a', FGrid.Controller.IncrementalSearchBuffer,
    'a lone high surrogate is removed by a single backspace');
end;

procedure TVittixUnicodeSearchTests.BackspaceRemovesSingleCombiningMark;
begin
  // न + matra ा (U+093E) forms ना. Combining marks are one Char per
  // backspace, matching the Windows EDIT control's deletion granularity.
  SendKey(Chr($0928));
  SendKey(Chr($093E));
  Assert.AreEqual<NativeInt>(2, Length(FGrid.Controller.IncrementalSearchBuffer));

  SendKey(#8);
  Assert.AreEqual<NativeInt>(1, Length(FGrid.Controller.IncrementalSearchBuffer),
    'backspace removes the combining mark only');
  Assert.AreEqual(Chr($0928), FGrid.Controller.IncrementalSearchBuffer,
    'the base character remains');
end;

{ TVittixIncrementalSearchModeTests }

procedure TVittixIncrementalSearchModeTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  // Default ismAuto plus the writable client dataset makes the grid editable.
end;

procedure TVittixIncrementalSearchModeTests.TearDown;
begin
  FOwnerForm.Free;
  FDataSet.Free;
end;

function TVittixIncrementalSearchModeTests.TypeKey(Ch: Char): Boolean;
var
  Key: Char;
begin
  Key := Ch;
  Result := TVittixGridKeyAccess(FGrid).TryPressKey(Key);
end;

procedure TVittixIncrementalSearchModeTests.EditableGridDoesNotConsumeKeyInAuto;
begin
  Assert.AreEqual(ismAuto, FGrid.IncrementalSearchMode, 'default mode');
  Assert.IsFalse(TypeKey('3'),
    'a writable grid must keep the printable key for in-place editing');
  Assert.AreEqual('', FGrid.Controller.IncrementalSearchBuffer,
    'no search buffer was started');
  Assert.AreEqual(1, FDataSet.FieldByName('ID').AsInteger,
    'the cursor did not move');
end;

procedure TVittixIncrementalSearchModeTests.ReadOnlyGridSearchesInAuto;
begin
  FGrid.ReadOnly := True;
  Assert.IsTrue(TypeKey('3'), 'a read-only grid consumes the key for search');
  Assert.AreEqual('3', FGrid.Controller.IncrementalSearchBuffer);
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger,
    'the read-only grid located the record');
end;

procedure TVittixIncrementalSearchModeTests.NoEditingOptionSearchesInAuto;
begin
  FGrid.Options := FGrid.Options - [dgEditing];
  Assert.IsTrue(TypeKey('3'), 'without dgEditing the grid cannot edit');
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixIncrementalSearchModeTests.ReadOnlyColumnSearchesInAuto;
begin
  FGrid.Columns[0].ReadOnly := True;
  Assert.IsTrue(TypeKey('3'), 'a read-only current column cannot edit');
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixIncrementalSearchModeTests.AlwaysModeConsumesOnEditableGrid;
begin
  FGrid.IncrementalSearchMode := ismAlways;
  Assert.IsTrue(TypeKey('3'),
    'ismAlways keeps the pre-2.1 consume-on-editable behavior');
  Assert.AreEqual('3', FGrid.Controller.IncrementalSearchBuffer);
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixIncrementalSearchModeTests.OffModePassesKeyThrough;
begin
  FGrid.IncrementalSearchMode := ismOff;
  Assert.IsFalse(TypeKey('3'), 'ismOff never consumes printable keys');
  Assert.AreEqual('', FGrid.Controller.IncrementalSearchBuffer);
end;

procedure TVittixIncrementalSearchModeTests.IncrementalSearchFalseStillDisables;
begin
  FGrid.IncrementalSearch := False;
  FGrid.IncrementalSearchMode := ismAlways;
  Assert.IsFalse(TypeKey('3'),
    'the legacy Boolean still turns search off regardless of the mode');
end;

end.
