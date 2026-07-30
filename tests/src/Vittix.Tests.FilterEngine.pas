unit Vittix.Tests.FilterEngine;

interface

uses
  System.SysUtils,
  Datasnap.DBClient,
  Data.DB,
  DUnitX.TestFramework,
  Vittix.DBGrid.ColumnInfo,
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
  FColumns.FindByFieldName('Amount').FilterText := '>250';
  FColumns.FindByFieldName('Amount').HasFilter := True;
  FEngine.Active := True;
  Assert.AreEqual(3, CountVisibleRecords(FDataSet));
end;

end.
