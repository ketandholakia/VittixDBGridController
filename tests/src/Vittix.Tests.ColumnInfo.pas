unit Vittix.Tests.ColumnInfo;

interface

uses
  System.SysUtils,
  System.Variants,
  DUnitX.TestFramework,
  Vittix.DBGrid.ColumnInfo;

type
  [TestFixture]
  TVittixColumnInfoTests = class
  public
    [Test]
    procedure SumReturnsNullWhenCountIsZero;
    [Test]
    procedure AvgDividesByCount;
    [Test]
    procedure SumUsesFloatPrecedenceOverInt;
    [Test]
    procedure StringMinMaxUsesAssignedValues;
    [Test]
    procedure ClearResetsNumericSentinels;
  end;

implementation

procedure TVittixColumnInfoTests.SumReturnsNullWhenCountIsZero;
var
  Agg: TVittixAggregation;
begin
  Agg.Clear;
  Assert.IsTrue(VarIsNull(Agg.GetValue(vatSum)));
end;

procedure TVittixColumnInfoTests.AvgDividesByCount;
var
  Agg: TVittixAggregation;
begin
  Agg.Clear;
  Agg.HasInt := True;
  Agg.SumInt := 9;
  Agg.Count := 3;

  Assert.AreEqual(3.0, Double(Agg.GetValue(vatAvg)), 0.0001);
end;

procedure TVittixColumnInfoTests.SumUsesFloatPrecedenceOverInt;
var
  Agg: TVittixAggregation;
begin
  Agg.Clear;
  Agg.Count := 1;
  Agg.HasInt := True;
  Agg.SumInt := 10;
  Agg.HasFloat := True;
  Agg.SumFloat := 12.5;

  Assert.AreEqual(12.5, Double(Agg.GetValue(vatSum)), 0.0001);
end;

procedure TVittixColumnInfoTests.StringMinMaxUsesAssignedValues;
var
  Agg: TVittixAggregation;
begin
  Agg.Clear;
  Agg.Count := 2;
  Agg.HasString := True;
  Agg.MinString := 'Alpha';
  Agg.MaxString := 'beta';

  Assert.AreEqual('Alpha', string(Agg.GetValue(vatMin)));
  Assert.AreEqual('beta', string(Agg.GetValue(vatMax)));
end;

procedure TVittixColumnInfoTests.ClearResetsNumericSentinels;
var
  Agg: TVittixAggregation;
begin
  Agg.Clear;

  Assert.AreEqual(High(Int64), Agg.MinInt);
  Assert.AreEqual(Low(Int64), Agg.MaxInt);
  Assert.IsTrue(Agg.MinFloat > 1.0E300);
  Assert.IsTrue(Agg.MaxFloat < -1.0E300);
end;

end.
