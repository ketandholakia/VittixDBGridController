unit Vittix.Tests.FilterEngine;

interface

uses
  System.SysUtils,
  System.Variants,
  Datasnap.DBClient,
  Data.DB,
  System.IOUtils,
  Vcl.Forms,
  Vcl.DBGrids,
  DUnitX.TestFramework,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Filter.Popup,
  Vittix.DBGrid.Filter.Engine;

type
  [TestFixture]
  TVittixFilterEngineTests = class
  private
    FDataSet: TClientDataSet;
    FColumns: TVittixDBGridColumns;
    FEngine: TVittixDBGridFilterEngine;
    FOuterHandlerCalled: Boolean;
    procedure RejectAllButRowFour(DataSet: TDataSet; var Accept: Boolean);
    procedure AcceptLowIds(DataSet: TDataSet; var Accept: Boolean);
    procedure RejectAllFilters(Sender: TObject; const FieldName, FilterText: string;
      var IsValid: Boolean; var ErrorMessage: string);
  public
    [Test]
    procedure ClearFilterPreservesReplacementHandler;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure ColumnFilterIsCaseInsensitive;
    [Test]
    procedure MultipleColumnFiltersCombineWithAnd;
    [Test]
    procedure GlobalSearchMatchesAcrossColumns;
    [Test]
    procedure MemoFieldFilterUsesAsString;
    [Test]
    procedure InvalidFilterRaisesAndDoesNotEnableFiltering;
    [Test]
    procedure FilterChainsToExistingHandler;
    [Test]
    procedure ClearResetsState;
    [Test]
    procedure InvalidFilterRollsBackActiveState;
    [Test]
    procedure ClearAllowsFilterToBeReapplied;
    [Test]
    procedure FilterOperatorsSupportEqualsAndComparisonModes;
    [Test]
    procedure FilterOperatorsSupportBetweenRanges;
    [Test]
    procedure FilterOperatorsSupportNotBetweenRanges;
    [Test]
    procedure FilterOperatorsSupportNullChecks;
    [Test]
    procedure FilterOperatorsSupportEmptyChecks;
    [Test]
    procedure NotEqualsOperatorMatchesExactValue;
    [Test]
    procedure DoesNotContainOperatorExcludesSubstring;
    [Test]
    procedure ClearFilterRestoresOriginalFilteredState;
    [Test]
    procedure ClearFilterRestoresOriginalOnFilterRecord;
    [Test]
    procedure FilterPopupRestoresOperatorFromSavedText;
    [Test]
    procedure FilterPopupRestoresBetweenOperatorFromSavedText;
    [Test]
    procedure FilterPopupRestoresNotBetweenOperatorFromSavedText;
    [Test]
    procedure FilterPopupRestoresNullOperatorFromSavedText;
    [Test]
    procedure FilterPopupRestoresEmptyOperatorFromSavedText;
    [Test]
    procedure FilterPopupLoadsPersistedHistory;
    [Test]
    procedure FilterPopupCanClearPersistedHistory;
    [Test]
    procedure FilterPopupEnterCommitsCurrentValue;
    [Test]
    procedure FilterPopupReportsButtonShortcuts;
    [Test]
    procedure FilterPopupClearHistoryResetsCurrentState;
    [Test]
    procedure FilterPopupUsesConfiguredRootPath;
    [Test]
    procedure FilterPopupClearHistoryClearsInMemoryState;
    [Test]
    procedure FilterPopupUsesConfiguredFileName;
    [Test]
    procedure FilterPopupFileNameOverridesRootPath;
    [Test]
    procedure FilterPopupLoadsExplicitFileBeforeRootPath;
    [Test]
    procedure FilterPopupClearHistoryDeletesRootPathFile;
    [Test]
    procedure FilterPopupCanRestrictValuesToDistinctList;
    [Test]
    procedure FilterPopupDistinctValuesIncludeBlankEntry;
    [Test]
    procedure FilterPopupBlankDistinctSelectionPersistsCleanToken;
    [Test]
    procedure FilterPopupBlankDistinctSelectionReloadsAsDisplayLabel;
  end;

implementation

uses
  Vittix.Tests.TestData;

procedure TVittixFilterEngineTests.ClearFilterPreservesReplacementHandler;
var
  Expected, Actual: TFilterRecordEvent;
begin
  FEngine.Active := True;
  FDataSet.OnFilterRecord := AcceptLowIds;
  Expected := AcceptLowIds;
  FEngine.ClearFilter;
  Actual := FDataSet.OnFilterRecord;
  Assert.IsTrue((TMethod(Expected).Code = TMethod(Actual).Code) and
    (TMethod(Expected).Data = TMethod(Actual).Data));
end;

procedure TVittixFilterEngineTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FColumns := CreateMatchingColumns(FDataSet);
  FEngine := TVittixDBGridFilterEngine.Create(FDataSet, FColumns);
end;

procedure TVittixFilterEngineTests.TearDown;
begin
  FEngine.Free;
  FColumns.Free;
  FDataSet.Free;
end;

procedure TVittixFilterEngineTests.RejectAllButRowFour(DataSet: TDataSet;
  var Accept: Boolean);
begin
  FOuterHandlerCalled := True;
  Accept := Accept and (DataSet.FieldByName('ID').AsInteger = 4);
end;

procedure TVittixFilterEngineTests.RejectAllFilters(Sender: TObject;
  const FieldName, FilterText: string; var IsValid: Boolean;
  var ErrorMessage: string);
begin
  IsValid := False;
  ErrorMessage := 'blocked in test';
end;

procedure TVittixFilterEngineTests.ColumnFilterIsCaseInsensitive;
var
  VisibleIds: string;
begin
  FColumns.FindByFieldName('Name').FilterText := 'ALPHA';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FEngine.Active := True;

  Assert.AreEqual(2, CountVisibleRecords(FDataSet));
  VisibleIds := '';
  FDataSet.First;
  while not FDataSet.Eof do
  begin
    VisibleIds := VisibleIds + IntToStr(FDataSet.FieldByName('ID').AsInteger) + ';';
    FDataSet.Next;
  end;
  Assert.IsTrue((VisibleIds = '1;4;') or (VisibleIds = '4;1;'));
end;

procedure TVittixFilterEngineTests.MultipleColumnFiltersCombineWithAnd;
begin
  FColumns.FindByFieldName('Name').FilterText := 'ALPHA';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FColumns.FindByFieldName('Notes').FilterText := 'dup';
  FColumns.FindByFieldName('Notes').HasFilter := True;
  FEngine.Active := True;

  Assert.AreEqual(1, CountVisibleRecords(FDataSet));
  Assert.AreEqual(4, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixFilterEngineTests.GlobalSearchMatchesAcrossColumns;
begin
  FEngine.GlobalSearchText := 'GAMMA NOTES';
  FEngine.Active := True;

  Assert.AreEqual(1, CountVisibleRecords(FDataSet));
  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixFilterEngineTests.MemoFieldFilterUsesAsString;
begin
  FColumns.FindByFieldName('Notes').FilterText := 'અમદાવાદ';
  FColumns.FindByFieldName('Notes').HasFilter := True;
  FEngine.Active := True;

  FDataSet.Locate('ID', 5, []);
  Assert.IsTrue(FEngine.AcceptCurrentRecord);
end;

procedure TVittixFilterEngineTests.InvalidFilterRaisesAndDoesNotEnableFiltering;
begin
  FColumns.FindByFieldName('Name').FilterText := 'bad';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FEngine.OnValidateFilter := RejectAllFilters;

  Assert.WillRaise(
    procedure
    begin
      FEngine.Active := True;
    end,
    Exception
  );

  Assert.IsFalse(FDataSet.Filtered);
end;

procedure TVittixFilterEngineTests.FilterChainsToExistingHandler;
begin
  FDataSet.OnFilterRecord := RejectAllButRowFour;
  FColumns.FindByFieldName('Name').FilterText := 'Alpha';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FEngine.Active := True;

  Assert.IsTrue(FOuterHandlerCalled);
  Assert.AreEqual(1, CountVisibleRecords(FDataSet));
  Assert.AreEqual(4, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixFilterEngineTests.ClearResetsState;
begin
  FColumns.FindByFieldName('Name').FilterText := 'Alpha';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FEngine.GlobalSearchText := 'gamma';
  FEngine.Active := True;

  FEngine.Clear;

  Assert.IsFalse(FEngine.Active);
  Assert.AreEqual('', FEngine.GlobalSearchText);
  Assert.AreEqual('', FColumns.FindByFieldName('Name').FilterText);
  Assert.IsFalse(FColumns.FindByFieldName('Name').HasFilter);
end;

procedure TVittixFilterEngineTests.InvalidFilterRollsBackActiveState;
begin
  FColumns.FindByFieldName('Name').FilterText := 'bad';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FEngine.OnValidateFilter := RejectAllFilters;

  Assert.WillRaise(
    procedure
    begin
      FEngine.Active := True;
    end,
    Exception
  );

  Assert.IsFalse(FEngine.Active);
  Assert.IsFalse(FDataSet.Filtered);
end;

procedure TVittixFilterEngineTests.ClearAllowsFilterToBeReapplied;
begin
  FColumns.FindByFieldName('Name').FilterText := 'Alpha';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FEngine.Active := True;
  Assert.AreEqual(2, CountVisibleRecords(FDataSet));

  FEngine.Clear;
  Assert.IsFalse(FEngine.Active);
  Assert.IsFalse(FDataSet.Filtered);

  FColumns.FindByFieldName('Name').FilterText := 'beta';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FEngine.Active := True;

  Assert.IsTrue(FEngine.Active);
  Assert.AreEqual(1, CountVisibleRecords(FDataSet));
  Assert.AreEqual(2, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixFilterEngineTests.FilterOperatorsSupportEqualsAndComparisonModes;
begin
  FColumns.FindByFieldName('Name').FilterText := '=Beta';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FEngine.Active := True;
  Assert.AreEqual(1, CountVisibleRecords(FDataSet));
  Assert.AreEqual(2, FDataSet.FieldByName('ID').AsInteger);

  FEngine.Clear;
  FColumns.FindByFieldName('Name').FilterText := '!Alpha';
  FColumns.FindByFieldName('Name').HasFilter := True;
  FEngine.Active := True;
  // Does-not-contain: beta, Gamma, Delta remain visible
  Assert.AreEqual(3, CountVisibleRecords(FDataSet));

  FEngine.Clear;
  FColumns.FindByFieldName('Amount').FilterText := '>250';
  FColumns.FindByFieldName('Amount').HasFilter := True;
  FEngine.Active := True;
  // Amount values: 100.50, 200.00, NULL, 50.25, 400.00 — only 400 is > 250
  Assert.AreEqual(1, CountVisibleRecords(FDataSet));
  Assert.IsTrue(FDataSet.Locate('ID', 5, []));
end;

procedure TVittixFilterEngineTests.FilterOperatorsSupportBetweenRanges;
begin
  FColumns.FindByFieldName('Amount').FilterText := '..150|300';
  FColumns.FindByFieldName('Amount').HasFilter := True;
  FEngine.Active := True;

  // Only 200.00 lies between 150 and 300; the value-based comparison must
  // see through the currency display text.
  Assert.AreEqual(1, CountVisibleRecords(FDataSet));
  Assert.IsTrue(FDataSet.Locate('ID', 2, []));
end;

procedure TVittixFilterEngineTests.FilterOperatorsSupportNotBetweenRanges;
begin
  FColumns.FindByFieldName('Amount').FilterText := '!..150|300';
  FColumns.FindByFieldName('Amount').HasFilter := True;
  FEngine.Active := True;

  // 100.50, 50.25 and 400.00 are outside; the NULL amount is excluded
  // (SQL semantics: NULL not between yields NULL).
  Assert.AreEqual(3, CountVisibleRecords(FDataSet));
  FDataSet.First;
  Assert.AreEqual(1, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixFilterEngineTests.FilterOperatorsSupportNullChecks;
begin
  // Amount is NULL on record 3
  FColumns.FindByFieldName('Amount').FilterText := 'null';
  FColumns.FindByFieldName('Amount').HasFilter := True;
  FEngine.Active := True;
  Assert.AreEqual(1, CountVisibleRecords(FDataSet));
  FDataSet.First;
  Assert.IsTrue(FDataSet.FieldByName('Amount').IsNull);

  FEngine.Clear;
  FColumns.FindByFieldName('Amount').FilterText := '!null';
  FColumns.FindByFieldName('Amount').HasFilter := True;
  FEngine.Active := True;
  Assert.AreEqual(4, CountVisibleRecords(FDataSet));
end;

procedure TVittixFilterEngineTests.FilterOperatorsSupportEmptyChecks;
var
  LocalSet: TClientDataSet;
  LocalColumns: TVittixDBGridColumns;
  LocalEngine: TVittixDBGridFilterEngine;
begin
  // NULL and empty are distinct: build a dataset that actually contains both.
  LocalSet := TClientDataSet.Create(nil);
  try
    LocalSet.FieldDefs.Add('Name', ftString, 50);
    LocalSet.CreateDataSet;
    LocalSet.Open;
    LocalSet.AppendRecord(['Alpha']);
    LocalSet.AppendRecord(['']);      // empty string, not null
    LocalSet.AppendRecord([Null]);    // real null

    LocalColumns := CreateMatchingColumns(LocalSet);
    try
      LocalEngine := TVittixDBGridFilterEngine.Create(LocalSet, LocalColumns);
      try
        LocalColumns.FindByFieldName('Name').FilterText := 'empty';
        LocalColumns.FindByFieldName('Name').HasFilter := True;
        LocalEngine.Active := True;
        // Only the empty-string record; NULL must not count as empty
        Assert.AreEqual(1, CountVisibleRecords(LocalSet));
        LocalSet.First;
        Assert.IsFalse(LocalSet.FieldByName('Name').IsNull);
        Assert.AreEqual('', LocalSet.FieldByName('Name').AsString);

        LocalEngine.Clear;
        LocalColumns.FindByFieldName('Name').FilterText := '!empty';
        LocalColumns.FindByFieldName('Name').HasFilter := True;
        LocalEngine.Active := True;
        // Alpha and the NULL record are not empty
        Assert.AreEqual(2, CountVisibleRecords(LocalSet));
      finally
        LocalEngine.Free;
      end;
    finally
      LocalColumns.Free;
    end;
  finally
    LocalSet.Free;
  end;
end;

procedure TVittixFilterEngineTests.AcceptLowIds(DataSet: TDataSet;
  var Accept: Boolean);
begin
  Accept := DataSet.FieldByName('ID').AsInteger <= 2;
end;

procedure TVittixFilterEngineTests.ClearFilterRestoresOriginalOnFilterRecord;
var
  Info: TVittixDBGridColumnInfo;
  SavedHandler: TFilterRecordEvent;
begin
  // The application filters on its own before the engine hooks in
  SavedHandler := AcceptLowIds;
  FDataSet.OnFilterRecord := SavedHandler;
  FDataSet.Filtered := True;
  Assert.AreEqual(2, CountVisibleRecords(FDataSet), 'app filter active');

  Info := FColumns.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FEngine.Active := True;
  // Engine chain: Alpha rows are ID 1 and 4; the app handler drops ID 4
  Assert.AreEqual(1, CountVisibleRecords(FDataSet), 'engine chains the app filter');

  FEngine.Active := False;

  // Handler reference and Filtered=True must both be restored
  Assert.IsTrue(
    (TMethod(FDataSet.OnFilterRecord).Code = TMethod(SavedHandler).Code) and
    (TMethod(FDataSet.OnFilterRecord).Data = TMethod(SavedHandler).Data),
    'original OnFilterRecord restored');
  Assert.IsTrue(FDataSet.Filtered, 'original Filtered=True restored');
  Assert.AreEqual(2, CountVisibleRecords(FDataSet), 'app filter still applies');
end;

procedure TVittixFilterEngineTests.NotEqualsOperatorMatchesExactValue;
var
  Info: TVittixDBGridColumnInfo;
begin
  // A row whose value merely CONTAINS the needle must survive <>: the
  // operator used to behave as does-not-contain and wrongly exclude it.
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := 'AlphaX';
  FDataSet.Post;

  Info := FColumns.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := '<>Alpha';
  Info.HasFilter := True;
  FEngine.Active := True;

  // Row 1 was edited to AlphaX: only the exact 'Alpha' (ID 4) drops out,
  // AlphaX survives. The old does-not-contain behaviour excluded it too.
  Assert.AreEqual(4, CountVisibleRecords(FDataSet),
    '<> keeps rows that contain but do not equal the needle');

  // Exact inequality still excludes the equal value
  Info.FilterText := '<>beta';
  FEngine.ApplyFilter;
  Assert.AreEqual(4, CountVisibleRecords(FDataSet),
    '<> excludes exactly the equal value');
end;

procedure TVittixFilterEngineTests.DoesNotContainOperatorExcludesSubstring;
var
  Info: TVittixDBGridColumnInfo;
begin
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := 'AlphaX';
  FDataSet.Post;

  Info := FColumns.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := '!Alpha';
  Info.HasFilter := True;
  FEngine.Active := True;

  // '!' keeps its historic substring exclusion: AlphaX and Alpha both drop
  Assert.AreEqual(3, CountVisibleRecords(FDataSet),
    '! excludes every value containing the needle');
end;

procedure TVittixFilterEngineTests.ClearFilterRestoresOriginalFilteredState;
var
  Info: TVittixDBGridColumnInfo;
begin
  // The application was already filtering before the engine installed its
  // hook; clearing the engine's filter must not switch that off.
  FDataSet.Filtered := True;

  Info := FColumns.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.FilterText := 'Alpha';
  Info.HasFilter := True;
  FEngine.Active := True;
  Assert.IsTrue(FDataSet.Filtered);
  Assert.AreEqual(2, CountVisibleRecords(FDataSet));

  FEngine.Active := False;

  Assert.IsTrue(FDataSet.Filtered,
    'original Filtered=True must survive engine teardown');
end;

procedure TVittixFilterEngineTests.FilterPopupRestoresOperatorFromSavedText;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Amount');
    Info.FilterText := '>=250';
    Info.HasFilter := True;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual(7, Popup.OperatorIndex);
      Assert.AreEqual('250', Popup.FilterText);
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupRestoresBetweenOperatorFromSavedText;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Amount');
    Info.FilterText := '..150|300';
    Info.HasFilter := True;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual(10, Popup.OperatorIndex);
      Assert.AreEqual('150|300', Popup.FilterText);
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupRestoresNotBetweenOperatorFromSavedText;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Amount');
    Info.FilterText := '!..150|300';
    Info.HasFilter := True;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual(11, Popup.OperatorIndex);
      Assert.AreEqual('150|300', Popup.FilterText);
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupRestoresNullOperatorFromSavedText;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Info.FilterText := 'null';
    Info.HasFilter := True;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual(12, Popup.OperatorIndex);
      Assert.AreEqual('', Popup.FilterText);
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupRestoresEmptyOperatorFromSavedText;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Info.FilterText := 'empty';
    Info.HasFilter := True;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual(14, Popup.OperatorIndex);
      // "empty" filters reload as the distinct-list display label
      Assert.AreEqual('(Blank)', Popup.FilterText);
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupLoadsPersistedHistory;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterHistory.test.ini');
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    TVittixDBGridFilterPopup.HistoryFileName := TempFile;

    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.PersistHistory;
    finally
      Popup.Free;
    end;

    Info.FilterText := '!Alpha';
    Info.HasFilter := True;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.PersistHistory;
    finally
      Popup.Free;
    end;

    Info.FilterText := '';
    Info.HasFilter := False;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      // Persisted history restores the last used operator AND value text
      // (operator prefix split out, not shown raw)
      Assert.AreEqual(4, Popup.OperatorIndex);
      Assert.AreEqual('Alpha', Popup.FilterText);
      Popup.PersistHistory;
    finally
      Popup.Free;
    end;

    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual(4, Popup.OperatorIndex);
      Assert.AreEqual('Alpha', Popup.FilterText);
      Popup.PersistHistory;
    finally
      Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.HistoryFileName := '';
    OwnerForm.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupCanClearPersistedHistory;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterHistory.clear.ini');
  TVittixDBGridFilterPopup.HistoryFileName := TempFile;
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.PersistHistory;
      Assert.IsTrue(FileExists(TempFile));
      Popup.ClearHistory;
      Assert.IsFalse(FileExists(TempFile));
    finally
      Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.HistoryFileName := '';
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupEnterCommitsCurrentValue;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilter.enter.test.ini');
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Info.FilterText := '';
    Info.HasFilter := False;
    // Isolated history file: the commit must not inherit an operator from
    // any earlier test's persisted state.
    TVittixDBGridFilterPopup.HistoryFileName := TempFile;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.FilterText := 'Alpha';
      Popup.CommitCurrentValue;
      Assert.AreEqual('Alpha', Info.FilterText);
      Assert.IsTrue(Info.HasFilter);
    finally
      Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.HistoryFileName := '';
    OwnerForm.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupReportsButtonShortcuts;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual('Clear Filter=none;Clear History=Ctrl+Shift+H', Popup.GetButtonShortcutSummaryText);
      Popup.ExecuteClearHistoryShortcut;
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupClearHistoryResetsCurrentState;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.FilterText := 'Alpha';
      Popup.ClearHistory;
      Assert.AreEqual('', Popup.FilterText);
      Assert.AreEqual(0, Popup.OperatorIndex);
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupUsesConfiguredRootPath;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  RootPath: string;
  PersistedFile: string;
begin
  RootPath := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterRoot.test');
  PersistedFile := TPath.Combine(RootPath, 'filter.ini');
  ForceDirectories(RootPath);
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    TVittixDBGridFilterPopup.RootPath := RootPath;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.PersistHistory;
      Assert.IsTrue(FileExists(PersistedFile));
    finally
      Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.RootPath := '';
    OwnerForm.Free;
    if FileExists(PersistedFile) then
      DeleteFile(PersistedFile);
    if TDirectory.Exists(RootPath) then
      TDirectory.Delete(RootPath, True);
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupClearHistoryClearsInMemoryState;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterHistory.memory.ini');
  TVittixDBGridFilterPopup.HistoryFileName := TempFile;
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.PersistHistory;
      Popup.ClearHistory;
      Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
      try
        Assert.AreEqual('', Popup.FilterText);
        Assert.AreEqual(0, Popup.OperatorIndex);
      finally
        Popup.Free;
        Popup := nil;
      end;
    finally
      if Assigned(Popup) then
        Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.HistoryFileName := '';
    OwnerForm.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupUsesConfiguredFileName;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterHistory.explicit.ini');
  TVittixDBGridFilterPopup.HistoryFileName := TempFile;
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.PersistHistory;
      Assert.IsTrue(FileExists(TempFile));
    finally
      Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.HistoryFileName := '';
    OwnerForm.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupFileNameOverridesRootPath;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  RootPath: string;
  ExplicitFile: string;
  RootFile: string;
begin
  RootPath := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterRoot.override.test');
  ExplicitFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterExplicit.override.ini');
  RootFile := TPath.Combine(RootPath, 'filter.ini');
  ForceDirectories(RootPath);
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    TVittixDBGridFilterPopup.RootPath := RootPath;
    TVittixDBGridFilterPopup.HistoryFileName := ExplicitFile;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.PersistHistory;
      Assert.IsTrue(FileExists(ExplicitFile));
      Assert.IsFalse(FileExists(RootFile));
    finally
      Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.RootPath := '';
    TVittixDBGridFilterPopup.HistoryFileName := '';
    OwnerForm.Free;
    if FileExists(ExplicitFile) then
      DeleteFile(ExplicitFile);
    if FileExists(RootFile) then
      DeleteFile(RootFile);
    if TDirectory.Exists(RootPath) then
      TDirectory.Delete(RootPath, True);
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupLoadsExplicitFileBeforeRootPath;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  RootPath: string;
  ExplicitFile: string;
begin
  RootPath := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterRoot.load.test');
  ExplicitFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterExplicit.load.ini');
  ForceDirectories(RootPath);
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    TVittixDBGridFilterPopup.RootPath := RootPath;
    TVittixDBGridFilterPopup.HistoryFileName := ExplicitFile;

    Info.FilterText := '=Alpha';
    Info.HasFilter := True;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.PersistHistory;
    finally
      Popup.Free;
    end;

    Info.FilterText := '';
    Info.HasFilter := False;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual('Alpha', Popup.FilterText);
      Assert.AreEqual(1, Popup.OperatorIndex);
    finally
      Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.RootPath := '';
    TVittixDBGridFilterPopup.HistoryFileName := '';
    OwnerForm.Free;
    if FileExists(ExplicitFile) then
      DeleteFile(ExplicitFile);
    if TDirectory.Exists(RootPath) then
      TDirectory.Delete(RootPath, True);
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupClearHistoryDeletesRootPathFile;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  RootPath: string;
  PersistedFile: string;
begin
  RootPath := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilterRoot.clear.test');
  PersistedFile := TPath.Combine(RootPath, 'filter.ini');
  ForceDirectories(RootPath);
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    TVittixDBGridFilterPopup.RootPath := RootPath;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.PersistHistory;
      Assert.IsTrue(FileExists(PersistedFile));
      Popup.ClearHistory;
      Assert.IsFalse(FileExists(PersistedFile));
    finally
      Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.RootPath := '';
    OwnerForm.Free;
    if FileExists(PersistedFile) then
      DeleteFile(PersistedFile);
    if TDirectory.Exists(RootPath) then
      TDirectory.Delete(RootPath, True);
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupCanRestrictValuesToDistinctList;
var
  OwnerForm: TForm;
  Grid: TDBGrid;
  Source: TDataSource;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    // Distinct values only load when the popup owner is a grid bound to the
    // dataset; a plain form owner must not pass by accident via leftover
    // in-memory history from earlier tests.
    Grid := TDBGrid.Create(OwnerForm);
    Grid.Parent := OwnerForm;
    Source := TDataSource.Create(OwnerForm);
    Source.DataSet := FDataSet;
    Grid.DataSource := Source;

    Info := FColumns.FindByFieldName('Name');
    Popup := TVittixDBGridFilterPopup.CreatePopup(Grid, Info);
    try
      Popup.UseDistinctValuesOnly := True;
      Popup.FilterText := 'Alpha';
      Assert.IsTrue(Popup.ValidateCurrentInput);

      Popup.FilterText := 'NotInList';
      Assert.IsFalse(Popup.ValidateCurrentInput);
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupDistinctValuesIncludeBlankEntry;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.UseDistinctValuesOnly := True;
      Assert.IsTrue(Popup.ValidateCurrentInput);
      Popup.FilterText := '(Blank)';
      Assert.IsTrue(Popup.ValidateCurrentInput);
      Assert.AreEqual(14, Popup.OperatorIndex);
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupBlankDistinctSelectionPersistsCleanToken;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.UseDistinctValuesOnly := True;
      Popup.FilterText := '(Blank)';
      Popup.CommitCurrentValue;
      Assert.AreEqual('empty', Info.FilterText);
      Assert.IsTrue(Info.HasFilter);
    finally
      Popup.Free;
    end;
  finally
    OwnerForm.Free;
  end;
end;

procedure TVittixFilterEngineTests.FilterPopupBlankDistinctSelectionReloadsAsDisplayLabel;
var
  OwnerForm: TForm;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, 'VittixDBGridFilter.blank.test.ini');
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    TVittixDBGridFilterPopup.HistoryFileName := TempFile;
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Popup.UseDistinctValuesOnly := True;
      Popup.FilterText := '(Blank)';
      Popup.CommitCurrentValue;
    finally
      Popup.Free;
    end;

    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual('(Blank)', Popup.FilterText);
      Assert.AreEqual(14, Popup.OperatorIndex);
    finally
      Popup.Free;
    end;
  finally
    TVittixDBGridFilterPopup.HistoryFileName := '';
    OwnerForm.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

end.
