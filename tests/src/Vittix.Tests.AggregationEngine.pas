unit Vittix.Tests.AggregationEngine;

interface

uses
  System.SysUtils,
  Datasnap.DBClient,
  DUnitX.TestFramework,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Aggregation.Engine;

type
  [TestFixture]
  TVittixAggregationEngineTests = class
  private
    FDataSet: TClientDataSet;
    FColumns: TVittixDBGridColumns;
    FEngine: TVittixDBGridAggregationEngine;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure NullValuesAreExcludedFromSum;
    [Test]
    procedure RecalculateRestoresBookmarkPosition;
    [Test]
    procedure StringMinAndMaxUseExpectedOrdering;
    [Test]
    procedure OnAcceptRecordRestrictsAggregation;
    [Test]
    procedure EmptyDatasetDoesNotRaise;
  end;

implementation

uses
  System.Variants,
  Vittix.Tests.TestData;

procedure TVittixAggregationEngineTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FColumns := CreateMatchingColumns(FDataSet);
  FEngine := TVittixDBGridAggregationEngine.Create(FDataSet, FColumns);
end;

procedure TVittixAggregationEngineTests.TearDown;
begin
  FEngine.Free;
  FColumns.Free;
  FDataSet.Free;
end;

procedure TVittixAggregationEngineTests.NullValuesAreExcludedFromSum;
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FColumns.FindByFieldName('Amount');
  Info.AggregationType := vatSum;

  FEngine.Recalculate;

  Assert.AreEqual(1, Info.Aggregation.NullCount);
  Assert.AreEqual(4, Info.Aggregation.Count);
  Assert.AreEqual(750.75, Double(FEngine.GetAggregation(Info)), 0.0001);
end;

procedure TVittixAggregationEngineTests.RecalculateRestoresBookmarkPosition;
begin
  FColumns.FindByFieldName('Amount').AggregationType := vatSum;

  FDataSet.Locate('ID', 3, []);
  FEngine.Recalculate;

  Assert.AreEqual(3, FDataSet.FieldByName('ID').AsInteger);
end;

procedure TVittixAggregationEngineTests.StringMinAndMaxUseExpectedOrdering;
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FColumns.FindByFieldName('Name');
  Info.AggregationType := vatMin;
  FEngine.Recalculate;
  Assert.AreEqual('Alpha', VarToStr(FEngine.GetAggregation(Info)));

  Info.AggregationType := vatMax;
  FEngine.Recalculate;
  Assert.AreEqual('beta', VarToStr(FEngine.GetAggregation(Info)));
end;

procedure TVittixAggregationEngineTests.OnAcceptRecordRestrictsAggregation;
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FColumns.FindByFieldName('Amount');
  Info.AggregationType := vatSum;
  FEngine.OnAcceptRecord :=
    function: Boolean
    begin
      Result := (FDataSet.FieldByName('ID').AsInteger mod 2) = 0;
    end;

  FEngine.Recalculate;

  Assert.AreEqual(250.25, Double(FEngine.GetAggregation(Info)), 0.0001);
end;

procedure TVittixAggregationEngineTests.EmptyDatasetDoesNotRaise;
var
  Info: TVittixDBGridColumnInfo;
begin
  FDataSet.EmptyDataSet;
  Info := FColumns.FindByFieldName('Amount');
  Info.AggregationType := vatSum;

  Assert.WillNotRaise(
    procedure
    begin
      FEngine.Recalculate;
    end
  );

  Assert.IsTrue(VarIsNull(FEngine.GetAggregation(Info)));
end;

end.
