unit Vittix.Tests.AggregationEngine;

interface

uses
  System.SysUtils,
  Data.DB,
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
    [Test]
    procedure ActiveAggregationSummaryReportsFieldModes;
    [Test]
    procedure FullScanCountTracksOnlyDatasetScans;

    // Phase 3: numeric coverage, BCD precision, date Min/Max, accumulator
    // write-back and the filtered-dataset fast path.
    [Test]
    procedure ExtraNumericTypesUseExpectedAccumulator;
    [Test]
    procedure BCDSumIsExactForRepeatedTenths;
    [Test]
    procedure DateMinMaxReturnDateTimeAndHonorDisplayFormat;
    [Test]
    procedure LocalAccumulatorsMatchPerRowResults;
    [Test]
    procedure FilterHookActiveSkipsAcceptCallback;
    [Test]
    procedure WithoutFilterHookTheAcceptCallbackStillRuns;
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

procedure TVittixAggregationEngineTests.FullScanCountTracksOnlyDatasetScans;
begin
  Assert.AreEqual<Int64>(0, FEngine.FullScanCount);
  FEngine.Recalculate; // No active aggregation: no record traversal.
  Assert.AreEqual<Int64>(0, FEngine.FullScanCount);

  FColumns.FindByFieldName('Amount').AggregationType := vatSum;
  FDataSet.Edit;
  FEngine.Recalculate; // Navigation during an edit must remain deferred.
  Assert.AreEqual<Int64>(0, FEngine.FullScanCount);
  FDataSet.Cancel;

  FEngine.Recalculate;
  Assert.AreEqual<Int64>(1, FEngine.FullScanCount);
  FEngine.Recalculate;
  Assert.AreEqual<Int64>(2, FEngine.FullScanCount);

  FDataSet.Close;
  FEngine.Recalculate;
  Assert.AreEqual<Int64>(2, FEngine.FullScanCount);
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

procedure TVittixAggregationEngineTests.ActiveAggregationSummaryReportsFieldModes;
begin
  FColumns.FindByFieldName('Amount').AggregationType := vatSum;
  FColumns.FindByFieldName('Name').AggregationType := vatMax;

  // The summary follows column order (ID, Name, Amount, ...) — Name first
  Assert.AreEqual(
    'Name:Max,Amount:Sum',
    FEngine.GetActiveAggregationSummaryText
  );
end;

{ Phase 3 }

procedure TVittixAggregationEngineTests.ExtraNumericTypesUseExpectedAccumulator;
var
  DataSet: TClientDataSet;
  Cols: TVittixDBGridColumns;
  Engine: TVittixDBGridAggregationEngine;
begin
  DataSet := TClientDataSet.Create(nil);
  Cols := nil;
  Engine := nil;
  try
    with DataSet.FieldDefs do
    begin
      Add('S', ftShortint);
      Add('B', ftByte);
      Add('L', ftLongWord);
      Add('F', ftSingle);
      Add('E', ftExtended);
    end;
    DataSet.CreateDataSet;
    DataSet.AppendRecord([-2, 200, Int64(4000000000), 1.5, 2.5]);
    DataSet.AppendRecord([3, 55, Int64(1000000000), 2.5, 3.5]);

    Cols := CreateMatchingColumns(DataSet);
    Engine := TVittixDBGridAggregationEngine.Create(DataSet, Cols);
    Cols.FindByFieldName('S').AggregationType := vatSum;
    Cols.FindByFieldName('B').AggregationType := vatSum;
    Cols.FindByFieldName('L').AggregationType := vatSum;
    Cols.FindByFieldName('F').AggregationType := vatSum;
    Cols.FindByFieldName('E').AggregationType := vatSum;
    Engine.Recalculate;

    Assert.AreEqual<Int64>(1,
      Engine.GetAggregation(Cols.FindByFieldName('S')),
      'ftShortint takes the Int64 path');
    Assert.AreEqual<Int64>(255,
      Engine.GetAggregation(Cols.FindByFieldName('B')),
      'ftByte takes the Int64 path');
    Assert.AreEqual(Int64(5000000000),
      Int64(Engine.GetAggregation(Cols.FindByFieldName('L'))),
      'ftLongWord sums beyond 32 bits');
    Assert.AreEqual(4.0, Double(Engine.GetAggregation(Cols.FindByFieldName('F'))),
      0.0001, 'ftSingle takes the float path');
    Assert.AreEqual(6.0, Double(Engine.GetAggregation(Cols.FindByFieldName('E'))),
      0.0001, 'ftExtended takes the float path');
  finally
    Engine.Free;
    Cols.Free;
    DataSet.Free;
  end;
end;

procedure TVittixAggregationEngineTests.BCDSumIsExactForRepeatedTenths;
const
  RowCount = 1000000;
var
  DataSet: TClientDataSet;
  Cols: TVittixDBGridColumns;
  Engine: TVittixDBGridAggregationEngine;
  Info: TVittixDBGridColumnInfo;
  I: Integer;
begin
  DataSet := TClientDataSet.Create(nil);
  Cols := nil;
  Engine := nil;
  try
    // TClientDataSet's BCD storage accepts a 4/4 field for fractional
    // values; the aggregate accumulator itself is Currency-backed.
    DataSet.FieldDefs.Add('Val', ftBCD);
    DataSet.FieldDefs[0].Size := 4;
    DataSet.FieldDefs[0].Precision := 4;
    DataSet.CreateDataSet;
    DataSet.DisableControls;
    try
      for I := 1 to RowCount do
        DataSet.AppendRecord([0.1]);
    finally
      DataSet.EnableControls;
    end;

    Cols := CreateMatchingColumns(DataSet);
    Engine := TVittixDBGridAggregationEngine.Create(DataSet, Cols);
    Info := Cols.FindByFieldName('Val');
    Info.AggregationType := vatSum;
    Engine.Recalculate;

    // Double accumulation of 0.1 drifts; the Currency/BCD path is exact.
    Assert.AreEqual(100000.0, Double(Engine.GetAggregation(Info)), 0.0);
  finally
    Engine.Free;
    Cols.Free;
    DataSet.Free;
  end;
end;

procedure TVittixAggregationEngineTests.DateMinMaxReturnDateTimeAndHonorDisplayFormat;
var
  Info: TVittixDBGridColumnInfo;
  Field: TDateTimeField;
begin
  Info := FColumns.FindByFieldName('Created');
  Field := FDataSet.FieldByName('Created') as TDateTimeField;
  Field.DisplayFormat := 'yyyy-mm-dd';

  Info.AggregationType := vatMin;
  FEngine.Recalculate;
  Assert.AreEqual(EncodeDate(2025, 12, 25),
    Double(FEngine.GetAggregation(Info)), 0.0001, 'earliest Created');
  Assert.AreEqual('2025-12-25', FEngine.GetAggregationDisplayText(Info),
    'Min uses the field DisplayFormat');

  Info.AggregationType := vatMax;
  FEngine.Recalculate;
  Assert.AreEqual(EncodeDate(2026, 4, 20),
    Double(FEngine.GetAggregation(Info)), 0.0001, 'latest Created');
  Assert.AreEqual('2026-04-20', FEngine.GetAggregationDisplayText(Info),
    'Max uses the field DisplayFormat');
end;

procedure TVittixAggregationEngineTests.LocalAccumulatorsMatchPerRowResults;
var
  Amount, Score, Name, IdInfo: TVittixDBGridColumnInfo;
begin
  Amount := FColumns.FindByFieldName('Amount');
  Score := FColumns.FindByFieldName('Score');
  Name := FColumns.FindByFieldName('Name');
  IdInfo := FColumns.FindByFieldName('ID');
  Amount.AggregationType := vatSum;
  Score.AggregationType := vatAvg;
  Name.AggregationType := vatMax;
  IdInfo.AggregationType := vatCount;

  FEngine.Recalculate;
  // Known fixture totals; one scan must fill every active column.
  Assert.AreEqual(750.75, Double(FEngine.GetAggregation(Amount)), 0.0001,
    'Amount SUM');
  Assert.AreEqual(1, Amount.Aggregation.NullCount,
    'Amount NULL count preserved');
  Assert.AreEqual(4, Amount.Aggregation.Count, 'Amount non-null count');
  Assert.AreEqual(5.21, Double(FEngine.GetAggregation(Score)), 0.0001,
    'Score AVG');
  Assert.AreEqual('beta', VarToStr(FEngine.GetAggregation(Name)), 'Name MAX');
  Assert.AreEqual(5, Integer(FEngine.GetAggregation(IdInfo)), 'ID COUNT');

  // A second scan must reproduce the exact same values (the local
  // accumulators reset rather than double-accumulate).
  FEngine.Recalculate;
  Assert.AreEqual(750.75, Double(FEngine.GetAggregation(Amount)), 0.0001);
  Assert.AreEqual(5.21, Double(FEngine.GetAggregation(Score)), 0.0001);
  Assert.AreEqual('beta', VarToStr(FEngine.GetAggregation(Name)));
  Assert.AreEqual(5, Integer(FEngine.GetAggregation(IdInfo)));
end;

procedure TVittixAggregationEngineTests.FilterHookActiveSkipsAcceptCallback;
var
  DataSet: TClientDataSet;
  Cols: TVittixDBGridColumns;
  Engine: TVittixDBGridAggregationEngine;
  Info: TVittixDBGridColumnInfo;
  CallCount: Integer;
begin
  DataSet := TClientDataSet.Create(nil);
  Cols := nil;
  Engine := nil;
  try
    DataSet.FieldDefs.Add('ID', ftInteger);
    DataSet.CreateDataSet;
    DataSet.AppendRecord([1]);
    DataSet.AppendRecord([2]);
    DataSet.AppendRecord([3]);
    DataSet.AppendRecord([4]);
    DataSet.AppendRecord([5]);
    DataSet.Filter := 'ID >= 3';
    DataSet.Filtered := True;

    Cols := CreateMatchingColumns(DataSet);
    Engine := TVittixDBGridAggregationEngine.Create(DataSet, Cols);
    Info := Cols.FindByFieldName('ID');
    Info.AggregationType := vatCount;

    CallCount := 0;
    Engine.OnAcceptRecord :=
      function: Boolean
      begin
        Inc(CallCount);
        Result := False; // would exclude everything if it ran
      end;
    Engine.FilterHookActive :=
      function: Boolean
      begin
        Result := True;
      end;

    Engine.Recalculate;
    Assert.AreEqual(0, CallCount,
      'the redundant per-record callback is skipped');
    Assert.AreEqual(3, Integer(Engine.GetAggregation(Info)),
      'the dataset Filter already restricts the scan');
  finally
    Engine.Free;
    Cols.Free;
    DataSet.Free;
  end;
end;

procedure TVittixAggregationEngineTests.WithoutFilterHookTheAcceptCallbackStillRuns;
var
  DataSet: TClientDataSet;
  Cols: TVittixDBGridColumns;
  Engine: TVittixDBGridAggregationEngine;
  Info: TVittixDBGridColumnInfo;
  CallCount: Integer;
begin
  DataSet := TClientDataSet.Create(nil);
  Cols := nil;
  Engine := nil;
  try
    DataSet.FieldDefs.Add('ID', ftInteger);
    DataSet.CreateDataSet;
    DataSet.AppendRecord([1]);
    DataSet.AppendRecord([2]);
    DataSet.AppendRecord([3]);
    DataSet.Filter := 'ID >= 2';
    DataSet.Filtered := True;

    Cols := CreateMatchingColumns(DataSet);
    Engine := TVittixDBGridAggregationEngine.Create(DataSet, Cols);
    Info := Cols.FindByFieldName('ID');
    Info.AggregationType := vatCount;

    CallCount := 0;
    Engine.OnAcceptRecord :=
      function: Boolean
      begin
        Inc(CallCount);
        Result := True;
      end;
    // No FilterHookActive: the callback must still run per record.

    Engine.Recalculate;
    Assert.AreEqual(2, CallCount,
      'without the engine hook the callback still decides each record');
    Assert.AreEqual(2, Integer(Engine.GetAggregation(Info)));
  finally
    Engine.Free;
    Cols.Free;
    DataSet.Free;
  end;
end;

end.
