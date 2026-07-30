unit Vittix.Tests.FilterEngine;

interface

uses
  System.SysUtils,
  Datasnap.DBClient,
  Data.DB,
  System.IOUtils,
  Vcl.Forms,
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
    procedure RejectAllFilters(Sender: TObject; const FieldName, FilterText: string;
      var IsValid: Boolean; var ErrorMessage: string);
  public
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
    procedure FilterPopupRestoresOperatorFromSavedText;
    [Test]
    procedure FilterPopupRestoresBetweenOperatorFromSavedText;
    [Test]
    procedure FilterPopupRestoresNotBetweenOperatorFromSavedText;
    [Test]
    procedure FilterPopupLoadsPersistedHistory;
    [Test]
    procedure FilterPopupCanClearPersistedHistory;
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
  end;

implementation

uses
  Vittix.Tests.TestData;

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
  Assert.AreEqual(1, CountVisibleRecords(FDataSet));
  Assert.AreEqual(2, FDataSet.FieldByName('ID').AsInteger);

  FEngine.Clear;
  FColumns.FindByFieldName('Amount').FilterText := '>250';
  FColumns.FindByFieldName('Amount').HasFilter := True;
  FEngine.Active := True;
  Assert.AreEqual(3, CountVisibleRecords(FDataSet));
end;

procedure TVittixFilterEngineTests.FilterOperatorsSupportBetweenRanges;
begin
  FColumns.FindByFieldName('Amount').FilterText := '..150|300';
  FColumns.FindByFieldName('Amount').HasFilter := True;
  FEngine.Active := True;

  Assert.AreEqual(2, CountVisibleRecords(FDataSet));
  Assert.AreEqual(150, FDataSet.FieldByName('Amount').AsInteger);
end;

procedure TVittixFilterEngineTests.FilterOperatorsSupportNotBetweenRanges;
begin
  FColumns.FindByFieldName('Amount').FilterText := '!..150|300';
  FColumns.FindByFieldName('Amount').HasFilter := True;
  FEngine.Active := True;

  Assert.AreEqual(3, CountVisibleRecords(FDataSet));
  Assert.AreEqual(50, FDataSet.FieldByName('Amount').AsInteger);
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
      Assert.AreEqual(4, Popup.OperatorIndex);
      Assert.AreEqual('', Popup.FilterText);
      Popup.PersistHistory;
    finally
      Popup.Free;
    end;

    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
    try
      Assert.AreEqual(4, Popup.OperatorIndex);
      Assert.AreEqual('', Popup.FilterText);
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
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
begin
  OwnerForm := TForm.CreateNew(nil);
  try
    Info := FColumns.FindByFieldName('Name');
    Popup := TVittixDBGridFilterPopup.CreatePopup(OwnerForm, Info);
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

end.
