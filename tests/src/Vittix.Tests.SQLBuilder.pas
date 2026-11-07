unit Vittix.Tests.SQLBuilder;

interface

uses
  System.SysUtils,
  System.Variants,
  Data.DB,
  DUnitX.TestFramework,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Filter.Engine,
  Vittix.DBGrid.Provider;

type
  [TestFixture]
  TVittixSQLBuilderTests = class
  private
    FFields: TArray<TVittixSQLFieldInfo>;
    function Build(const ADialect: TVittixSQLDialect;
      const AGroups: TVittixFilterConditionGroups;
      out AWhere: string; out AParams: TVittixSQLParams;
      out AReason: string): Boolean;
    function BuildSort(const ADialect: TVittixSQLDialect;
      const AColumns: TArray<TVittixSortColumn>;
      out AOrderBy: string; out AReason: string): Boolean;
    function Cond(const AField, AText: string): TVittixFilterCondition;
    function ParamValue(const AParams: TVittixSQLParams;
      const AName: string): Variant;
  public
    [Setup]
    procedure Setup;

    // operators
    [Test]
    procedure ContainsUsesLikeWithEscape;
    [Test]
    procedure StartsWithEndsWithPatterns;
    [Test]
    procedure EqualsTextIsCaseFolded;
    [Test]
    procedure NotEqualsCoalescesNull;
    [Test]
    procedure DoesNotContainPassesNull;
    [Test]
    procedure NullOperatorsAreTyped;
    [Test]
    procedure EmptyOperatorTextOnly;
    [Test]
    procedure OrderedComparisonsAreParameterized;
    [Test]
    procedure BetweenProducesTwoParams;
    [Test]
    procedure DateEqualityIsTypedParameter;
    [Test]
    procedure BooleanEqualsTrueFalse;
    [Test]
    procedure UnknownFieldIsRejected;

    // not-expressible reasons
    [Test]
    procedure DisplayTextOperatorsAreNotExpressible;
    [Test]
    procedure UnparseableDateIsNotExpressible;
    [Test]
    procedure NonNumericOrderedComparisonIsNotExpressible;

    // groups
    [Test]
    procedure OneGroupProducesSingleAnd;
    [Test]
    procedure TwoGroupsProduceOrWithParens;
    [Test]
    procedure EmptyGroupsDropped;

    // global search
    [Test]
    procedure GlobalSearchOrsAcrossTextColumns;
    [Test]
    procedure GlobalSearchReplacesGroups;
    [Test]
    procedure GlobalSearchCappedAt20Columns;

    // ORDER BY
    [Test]
    procedure OrderByQuotingAndDirections;
    [Test]
    procedure OrderByNullPlacementForSupportedDialects;
    [Test]
    procedure OrderByUnknownFieldRejected;

    // injection
    [Test]
    procedure InjectionPayloadsStayInParameters;
    [Test]
    procedure LikeWildcardsInValuesAreEscaped;

    // identifier quoting per dialect
    [Test]
    procedure QuotingPerDialect;
  end;

implementation

function TVittixSQLBuilderTests.Cond(const AField,
  AText: string): TVittixFilterCondition;
begin
  Result.FieldName := AField;
  Result.FilterText := AText;
end;

procedure TVittixSQLBuilderTests.Setup;
begin
  SetLength(FFields, 5);
  FFields[0].Name := 'ID';
  FFields[0].DataType := ftInteger;
  FFields[1].Name := 'Name';
  FFields[1].DataType := ftWideString;
  FFields[2].Name := 'Amount';
  FFields[2].DataType := ftCurrency;
  FFields[3].Name := 'Created';
  FFields[3].DataType := ftDateTime;
  FFields[4].Name := 'IsActive';
  FFields[4].DataType := ftBoolean;
end;

function TVittixSQLBuilderTests.Build(const ADialect: TVittixSQLDialect;
  const AGroups: TVittixFilterConditionGroups;
  out AWhere: string; out AParams: TVittixSQLParams;
  out AReason: string): Boolean;
begin
  Result := TVittixSQLBuilder.BuildWhere(ADialect, FFields, AGroups, '',
    AWhere, AParams, AReason);
end;

function TVittixSQLBuilderTests.BuildSort(const ADialect: TVittixSQLDialect;
  const AColumns: TArray<TVittixSortColumn>;
  out AOrderBy: string; out AReason: string): Boolean;
begin
  Result := TVittixSQLBuilder.BuildOrderBy(ADialect, FFields, AColumns,
    AOrderBy, AReason);
end;

function TVittixSQLBuilderTests.ParamValue(const AParams: TVittixSQLParams;
  const AName: string): Variant;
var
  I: Integer;
begin
  Result := Null;
  for I := 0 to High(AParams) do
    if SameText(AParams[I].Name, AName) then
      Exit(AParams[I].Value);
end;

procedure TVittixSQLBuilderTests.ContainsUsesLikeWithEscape;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
  I: Integer;
  Found: Boolean;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Name', 'alpha');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));

  // One LIKE with UPPER on both sides and an ESCAPE clause
  Assert.IsTrue(Where.Contains('UPPER("Name") LIKE UPPER(:vx'), Where);
  Assert.AreEqual(1, Length(Params), 'one needle parameter');
  Assert.AreEqual(ftString, Params[0].DataType);
  Assert.AreEqual('%alpha%', string(Params[0].Value));
  Assert.IsTrue(Where.Contains('ESCAPE'), Where);
  Found := Pos(':vx0', Where) > 0;
end;

procedure TVittixSQLBuilderTests.StartsWithEndsWithPatterns;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 2);
  Groups[0][0] := Cond('Name', '^Al');
  Groups[0][1] := Cond('Name', '$pha');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  // StartsWith anchors to the front, EndsWith to the back ($ prefix)
  Assert.AreEqual('Al%', string(Params[0].Value));
  Assert.AreEqual('%pha', string(Params[1].Value));
  Assert.AreEqual(2, Length(Params));
end;

procedure TVittixSQLBuilderTests.EqualsTextIsCaseFolded;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Name', '=Alpha');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('UPPER("Name") = UPPER(:vx0)', Where, Where);
  Assert.AreEqual('Alpha', string(ParamValue(Params, 'vx0')));
end;

procedure TVittixSQLBuilderTests.NotEqualsCoalescesNull;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Name', '<>Alpha');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('UPPER(COALESCE("Name", '''')) <> UPPER(:vx0)', Where, Where);
end;

procedure TVittixSQLBuilderTests.DoesNotContainPassesNull;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Name', '!alpha');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  // The client matches a NULL field for Does Not Contain (its display
  // text is empty); the SQL carries the NULL branch explicitly.
  Assert.IsTrue(Where.StartsWith('("Name" IS NULL OR '), Where);
  Assert.IsTrue(Where.Contains('NOT LIKE'), Where);
end;

procedure TVittixSQLBuilderTests.NullOperatorsAreTyped;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 2);
  Groups[0][0] := Cond('Name', 'null');
  Groups[0][1] := Cond('Amount', '!null');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('"Name" IS NULL AND "Amount" IS NOT NULL', Where, Where);
  Assert.AreEqual(0, Length(Params), 'word operators carry no parameters');
end;

procedure TVittixSQLBuilderTests.EmptyOperatorTextOnly;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 2);
  Groups[0][0] := Cond('Name', 'empty');
  Groups[0][1] := Cond('Name', '!empty');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual(
    '("Name" IS NOT NULL AND "Name" = '''') AND ("Name" IS NULL OR "Name" <> '''')',
    Where, Where);
end;

procedure TVittixSQLBuilderTests.OrderedComparisonsAreParameterized;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Amount', '>250');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('"Amount" > :vx0', Where, Where);
  Assert.AreEqual(varDouble, VarType(ParamValue(Params, 'vx0')),
    'the bound is a numeric variant');
  Assert.AreEqual<Double>(250.0, Double(ParamValue(Params, 'vx0')));
end;

procedure TVittixSQLBuilderTests.BetweenProducesTwoParams;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Amount', '..50|300');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('"Amount" >= :vx0 AND "Amount" <= :vx1', Where, Where);
  Assert.AreEqual<Double>(50.0, Double(ParamValue(Params, 'vx0')));
  Assert.AreEqual<Double>(300.0, Double(ParamValue(Params, 'vx1')));

  // Not Between negates the whole range
  Groups[0][0] := Cond('Amount', '!..50|300');
  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('NOT ("Amount" >= :vx0 AND "Amount" <= :vx1)', Where, Where);
end;

procedure TVittixSQLBuilderTests.DateEqualityIsTypedParameter;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Created', '=2026-01-10');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('"Created" = :vx0', Where, Where);
  Assert.AreEqual(ftDateTime, Params[0].DataType,
    'the date travels as a typed parameter');
  Assert.AreEqual(varDate, VarType(ParamValue(Params, 'vx0')));
end;

procedure TVittixSQLBuilderTests.BooleanEqualsTrueFalse;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('IsActive', '=True');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('"IsActive" = :vx0', Where, Where);
  Assert.AreEqual(ftBoolean, Params[0].DataType);
  Assert.IsTrue(Boolean(ParamValue(Params, 'vx0')));

  Groups[0][0] := Cond('IsActive', '=False');
  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.IsFalse(Boolean(ParamValue(Params, 'vx0')));

  // Anything else matches nothing on the client either: impossible probe.
  Groups[0][0] := Cond('IsActive', '=banana');
  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('1 = 0', Where, Where);
end;

procedure TVittixSQLBuilderTests.UnknownFieldIsRejected;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Hacker; DROP TABLE x', '=1');

  Assert.IsFalse(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.IsTrue(Reason.Contains('unknown field'), Reason);
end;

procedure TVittixSQLBuilderTests.DisplayTextOperatorsAreNotExpressible;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  // Contains on a numeric column matches the display text client-side;
  // that has no SQL equivalent.
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Amount', '5');
  Assert.IsFalse(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.IsTrue(Reason.Contains('display text'), Reason);

  // Same for Is Empty on a numeric column (display formats).
  Groups[0][0] := Cond('Amount', 'empty');
  Assert.IsFalse(Build(sdANSI, Groups, Where, Params, Reason));
end;

procedure TVittixSQLBuilderTests.UnparseableDateIsNotExpressible;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Created', '=not a date');

  Assert.IsFalse(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.IsTrue(Reason.Contains('date'), Reason);
end;

procedure TVittixSQLBuilderTests.NonNumericOrderedComparisonIsNotExpressible;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Name', '>250');

  Assert.IsFalse(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.IsTrue(Reason.Contains('non-numeric'), Reason);
end;

procedure TVittixSQLBuilderTests.OneGroupProducesSingleAnd;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 2);
  Groups[0][0] := Cond('Name', '^A');
  Groups[0][1] := Cond('ID', '>2');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  // One group: a flat AND (equivalent to the client single-group result)
  Assert.AreEqual('UPPER("Name") LIKE UPPER(:vx0) ESCAPE ''\'' AND "ID" > :vx1',
    Where, Where);
end;

procedure TVittixSQLBuilderTests.TwoGroupsProduceOrWithParens;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 2);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Name', '^A');
  SetLength(Groups[1], 1);
  Groups[1][0] := Cond('ID', '>2');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  // Multi-group: every group parenthesized, ORed together
  Assert.AreEqual(
    '(UPPER("Name") LIKE UPPER(:vx0) ESCAPE ''\'') OR ("ID" > :vx1)',
    Where, Where);
end;

procedure TVittixSQLBuilderTests.EmptyGroupsDropped;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  // One real group plus two empty ones: the empties vanish (mirrors the
  // client compile step).
  SetLength(Groups, 3);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('ID', '>2');
  SetLength(Groups[1], 0);
  SetLength(Groups[2], 0);

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('"ID" > :vx0', Where, Where);

  // Only empty groups: no WHERE at all
  SetLength(Groups, 2);
  SetLength(Groups[0], 0);
  SetLength(Groups[1], 0);
  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('', Where, Where);
end;

procedure TVittixSQLBuilderTests.GlobalSearchOrsAcrossTextColumns;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 0);
  Assert.IsTrue(TVittixSQLBuilder.BuildWhere(sdANSI, FFields, Groups,
    'alpha', Where, Params, Reason));

  // ID/Amount/Created are not text: the search covers Name only here.
  Assert.AreEqual('(UPPER("Name") LIKE UPPER(:vx0) ESCAPE ''\'')', Where, Where);
  Assert.AreEqual('%alpha%', string(ParamValue(Params, 'vx0')));
end;

procedure TVittixSQLBuilderTests.GlobalSearchReplacesGroups;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  // The client global search REPLACES the group result; the SQL mirrors
  // that: only the search OR appears.
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('ID', '>2');

  Assert.IsTrue(TVittixSQLBuilder.BuildWhere(sdANSI, FFields, Groups,
    'alpha', Where, Params, Reason));
  Assert.IsFalse(Where.Contains('"ID"'), Where);
  Assert.IsTrue(Where.Contains('UPPER("Name")'), Where);
end;

procedure TVittixSQLBuilderTests.GlobalSearchCappedAt20Columns;
var
  Wide: TArray<TVittixSQLFieldInfo>;
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
  I, Count: Integer;
begin
  SetLength(Wide, 30);
  for I := 0 to 29 do
  begin
    Wide[I].Name := 'T' + IntToStr(I);
    Wide[I].DataType := ftWideString;
  end;
  SetLength(Groups, 0);

  Assert.IsTrue(TVittixSQLBuilder.BuildWhere(sdANSI, Wide, Groups,
    'x', Where, Params, Reason));
  Count := 0;
  for I := 1 to Length(Where) - 2 do
    if Copy(Where, I, 5) = ' LIKE' then
      Inc(Count);
  Assert.AreEqual(TVittixSQLBuilder.GLOBAL_SEARCH_MAX_COLUMNS, Count,
    'the global search caps the column list');
end;

procedure TVittixSQLBuilderTests.OrderByQuotingAndDirections;
var
  OrderBy, Reason: string;
  Cols: TArray<TVittixSortColumn>;
begin
  SetLength(Cols, 2);
  Cols[0].FieldName := 'Name';
  Cols[0].Descending := False;
  Cols[1].FieldName := 'ID';
  Cols[1].Descending := True;

  Assert.IsTrue(BuildSort(sdANSI, Cols, OrderBy, Reason));
  Assert.AreEqual('"Name" ASC, "ID" DESC', OrderBy, OrderBy);
end;

procedure TVittixSQLBuilderTests.OrderByNullPlacementForSupportedDialects;
var
  OrderBy, Reason: string;
  Cols: TArray<TVittixSortColumn>;
begin
  SetLength(Cols, 1);
  Cols[0].FieldName := 'Name';
  Cols[0].Descending := False;

  Assert.IsTrue(BuildSort(sdPostgreSQL, Cols, OrderBy, Reason));
  Assert.AreEqual('"Name" ASC NULLS LAST', OrderBy, OrderBy);

  Cols[0].Descending := True;
  Assert.IsTrue(BuildSort(sdPostgreSQL, Cols, OrderBy, Reason));
  Assert.AreEqual('"Name" DESC NULLS FIRST', OrderBy, OrderBy);

  Assert.IsTrue(BuildSort(sdFirebird, Cols, OrderBy, Reason));
  Assert.AreEqual('"Name" DESC NULLS FIRST', OrderBy, OrderBy);

  // Documented defaults: SQLite/MySQL/SQL Server keep their native order.
  Cols[0].Descending := False;
  Assert.IsTrue(BuildSort(sdSQLite, Cols, OrderBy, Reason));
  Assert.AreEqual('"Name" ASC', OrderBy, OrderBy);
  Assert.IsTrue(BuildSort(sdMySQL, Cols, OrderBy, Reason));
  Assert.AreEqual('`Name` ASC', OrderBy, OrderBy);
end;

procedure TVittixSQLBuilderTests.OrderByUnknownFieldRejected;
var
  OrderBy, Reason: string;
  Cols: TArray<TVittixSortColumn>;
begin
  SetLength(Cols, 1);
  Cols[0].FieldName := 'Nope';
  Cols[0].Descending := False;
  Assert.IsFalse(BuildSort(sdANSI, Cols, OrderBy, Reason));
end;

procedure TVittixSQLBuilderTests.InjectionPayloadsStayInParameters;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
  Payloads: TArray<string>;
  I: Integer;
  AllText: string;
begin
  Payloads := TArray<string>.Create(
    'x''; DROP TABLE users; --',
    'x'' OR ''1''=''1',
    'a''; DELETE FROM t; --',
    'x'' /* comment */',
    '%_%%_',
    '\''; --',
    #13#10 + 'GO');
  // The identifier slot must also reject anything unknown; values can
  // never leak into the SQL text.
  for I := 0 to High(Payloads) do
  begin
    SetLength(Groups, 1);
    SetLength(Groups[0], 2);
    Groups[0][0] := Cond('Name', Payloads[I]);
    Groups[0][1] := Cond('Name', '=' + Payloads[I]);

    Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason),
      'payload ' + IntToStr(I) + ' should be expressible');
    AllText := Where;
    Assert.IsFalse(AllText.Contains('''; DROP'), 'payload 0 text');
    Assert.IsFalse(AllText.Contains(''' OR '''), 'payload 1 text');
    Assert.IsFalse(AllText.Contains('DELETE FROM'), 'payload 2 text');
    Assert.IsFalse(AllText.Contains('/*'), 'payload 3 text');
    // The only quotes in the text are the structural ones (UPPER("col"))
    // and the ESCAPE literal; every payload byte sits in a parameter.
  end;
end;

procedure TVittixSQLBuilderTests.LikeWildcardsInValuesAreEscaped;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Name', '50%_off');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  // % and _ are escaped in the PARAMETER, not the SQL
  Assert.AreEqual('%50\%\_off%', string(ParamValue(Params, 'vx0')));
end;

procedure TVittixSQLBuilderTests.QuotingPerDialect;
var
  Where, Reason: string;
  Params: TVittixSQLParams;
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Cond('Name', 'null');

  Assert.IsTrue(Build(sdANSI, Groups, Where, Params, Reason));
  Assert.AreEqual('"Name" IS NULL', Where, Where);

  Assert.IsTrue(Build(sdMySQL, Groups, Where, Params, Reason));
  Assert.AreEqual('`Name` IS NULL', Where, Where);

  Assert.IsTrue(Build(sdSQLServer, Groups, Where, Params, Reason));
  Assert.AreEqual('[Name] IS NULL', Where, Where);

  Assert.IsTrue(Build(sdSQLite, Groups, Where, Params, Reason));
  Assert.AreEqual('"Name" IS NULL', Where, Where);
end;

end.
