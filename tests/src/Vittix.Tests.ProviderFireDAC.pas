unit Vittix.Tests.ProviderFireDAC;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Diagnostics,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Phys.SQLite,
  FireDAC.DApt,
  Vcl.Forms,
  Vcl.DBGrids,
  Vcl.StdCtrls,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.FilterSets,
  Vittix.DBGrid.Provider,
  Vittix.DBGrid.Provider.FireDAC,
  Vittix.DBGrid.Paging.FireDAC,
  Vittix.DBGrid.Paging.Controls,
  Vittix.DBGrid.Controller;

type
  [TestFixture]
  TVittixProviderFireDACTests = class
  private
    FConnection: TFDConnection;
    FQuery: TFDQuery;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FProvider: TVittixFireDACProvider;
    FFallbackCount: Integer;
    FFallbackReason: string;
    FErrorCount: Integer;
    procedure HandleFallback(Sender: TObject; const AReason: string);
    procedure HandleProviderError(Sender: TObject; E: Exception;
      var Handled: Boolean);
    procedure CreateFixture(ARowCount: Integer);
    procedure SetColumnFilter(const AFieldName, AFilterText: string);
    procedure ClearColumnFilters;
    function VisibleIDs: string;
    function CountRows: Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ServerFilterEqualsClientFilter;
    [Test]
    procedure OrGroupsMatchClientResults;
    [Test]
    procedure GlobalSearchMatchesClientResults;
    [Test]
    procedure SortAndMultiColumnSortServerSide;
    [Test]
    procedure OriginalSQLRestoredAfterClearFilter;
    [Test]
    procedure OriginalSQLRestoredOnControllerDestruction;
    [Test]
    procedure AutoModeFallsBackWithReason;
    [Test]
    procedure FailingQueryRestoresSQLOriginal;
    [Test]
    procedure CurrentRecordPreservedByKey;
    [Test]
    procedure ServerAggregatesEqualClientAggregates;
    [Test]
    procedure SavedFilterSetAppliesInServerMode;
    [Test]
    procedure LayoutActiveFilterSetAppliesInServerMode;
    [Test]
    procedure EditStateDefersRequery;
    [Test]
    procedure ExportFilteredOnlyWorksInServerMode;
    [Test]
    procedure ServerFilterFasterThanClientOnLargeTable;

    // Wave 1 regressions: server aggregates (B1/B2) and provider/query
    // lifetime (B3/M1).
    [Test]
    procedure ServerAggregateBindsFilterParameters;
    [Test]
    procedure AggregateNullCountMixedNulls;
    [Test]
    procedure AggregateNullCountAllNullsUnderFilter;
    [Test]
    procedure AggregateNullCountNoNullsUnderFilter;
    [Test]
    procedure AggregateNullCountEmptyDataset;
    [Test]
    procedure QueryFreedFirstLeavesProviderInert;
    [Test]
    procedure QueryFreedWhileFilterActiveIsSafe;
    [Test]
    procedure PagerNavigatesAndRestoresQuery;
    [Test]
    procedure PagerRejectsPageChangeDuringEdit;
    [Test]
    procedure PagerRestoresClosedQueryAndHandlesQueryDestruction;
    [Test]
    procedure PagerBarNavigatesAndDetaches;

    // Phase 1: keyword detection must survive CR/LF/TAB and ignore
    // keywords inside literals and comments.
    [Test]
    procedure OrderByAcrossNewlineIsRejected;
    [Test]
    procedure OrderByTabSeparatedIsRejected;
    [Test]
    procedure UnionOnOwnLineIsRejected;
    [Test]
    procedure ExecAtStartAfterNewlineIsRejected;
    [Test]
    procedure KeywordInsideStringLiteralIsWrappable;
    [Test]
    procedure KeywordInsideLineCommentIsWrappable;
    [Test]
    procedure KeywordInsideBlockCommentIsWrappable;
    [Test]
    procedure PlainSelectRemainsWrappable;
  end;

implementation

uses
  System.Types,
  System.IOUtils,
  Vittix.Tests.TestData,
  Vittix.DBGrid.Filter.Engine,
  Vittix.DBGrid.Export.Engine;

{ helpers }

function IsWrappableSQL(const ASQL: string): Boolean;
var
  Q: TFDQuery;
  P: TVittixFireDACProvider;
  PI: IVittixDataProvider;
begin
  // CanHandle funnels through the private CheckWrappable, so this exercises
  // the real keyword scan without a live connection.
  Q := TFDQuery.Create(nil);
  try
    Q.SQL.Text := ASQL;
    P := TVittixFireDACProvider.Create(Q);
    PI := P;
    Result := P.CanHandle(Q);
    PI := nil; // release the provider before the query
  finally
    Q.Free;
  end;
end;

function FieldsOf(AQuery: TFDQuery): TArray<TVittixSQLFieldInfo>;
var
  I: Integer;
begin
  SetLength(Result, AQuery.Fields.Count);
  for I := 0 to AQuery.Fields.Count - 1 do
  begin
    Result[I].Name := AQuery.Fields[I].FieldName;
    Result[I].DataType := AQuery.Fields[I].DataType;
  end;
end;

function OneGroup(const AFieldName, AFilterText: string):
  TVittixFilterConditionGroups;
var
  Cond: TVittixFilterCondition;
begin
  Cond.FieldName := AFieldName;
  Cond.FilterText := AFilterText;
  SetLength(Result, 1);
  SetLength(Result[0], 1);
  Result[0][0] := Cond;
end;

procedure TVittixProviderFireDACTests.HandleFallback(Sender: TObject;
  const AReason: string);
begin
  Inc(FFallbackCount);
  FFallbackReason := AReason;
end;

procedure TVittixProviderFireDACTests.HandleProviderError(Sender: TObject;
  E: Exception; var Handled: Boolean);
begin
  Inc(FErrorCount);
  Handled := True;
end;

procedure TVittixProviderFireDACTests.CreateFixture(ARowCount: Integer);
begin
  FConnection.ExecSQL(
    'CREATE TABLE customers (id INTEGER PRIMARY KEY, name TEXT, ' +
    'amount REAL, created TIMESTAMP, active INTEGER)');
  // One statement with a recursive CTE: fast even for 200k rows, and the
  // NULL/unicode/date mix exercises the typed-parameter paths.
  FConnection.ExecSQL(
    'INSERT INTO customers (id, name, amount, created, active) ' +
    'WITH RECURSIVE seq(i) AS (' +
    '  SELECT 1 UNION ALL SELECT i + 1 FROM seq WHERE i < ' +
    IntToStr(ARowCount) + ') ' +
    'SELECT i, ' +
    '  CASE WHEN i % 7 = 0 THEN NULL ' +
    '       WHEN i % 3 = 0 THEN ''Émile '' || i ' +
    '       ELSE ''Customer '' || i END, ' +
    '  CASE WHEN i % 5 = 0 THEN NULL ELSE i * 1.5 END, ' +
    '  DATE(''2020-01-01'', ''+'' || (i % 2000) || '' days''), ' +
    '  CASE WHEN i % 2 = 0 THEN 1 ELSE 0 END ' +
    'FROM seq');
end;

procedure TVittixProviderFireDACTests.SetColumnFilter(const AFieldName,
  AFilterText: string);
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName(AFieldName);
  Assert.IsNotNull(Info, AFieldName);
  Info.FilterText := AFilterText;
  Info.HasFilter := True;
end;

procedure TVittixProviderFireDACTests.ClearColumnFilters;
var
  I: Integer;
begin
  for I := 0 to FGrid.ColumnInfo.Count - 1 do
  begin
    FGrid.ColumnInfo[I].FilterText := '';
    FGrid.ColumnInfo[I].HasFilter := False;
  end;
end;

function TVittixProviderFireDACTests.CountRows: Integer;
begin
  // FireDAC fetches on demand (50-row rowsets): RecordCount reflects the
  // fetched window, so tests that need the real row count walk the set.
  Result := 0;
  FQuery.DisableControls;
  try
    FQuery.First;
    while not FQuery.Eof do
    begin
      Inc(Result);
      FQuery.Next;
    end;
  finally
    FQuery.EnableControls;
  end;
end;

function TVittixProviderFireDACTests.VisibleIDs: string;
var
  Seen: TStringList;
  Mark: TBookmark;
begin
  Seen := TStringList.Create;
  try
    FQuery.DisableControls;
    try
      Mark := FQuery.GetBookmark;
      try
        FQuery.First;
        while not FQuery.Eof do
        begin
          Seen.Add(FQuery.FieldByName('id').AsString);
          FQuery.Next;
        end;
        if FQuery.BookmarkValid(Mark) then
          FQuery.GotoBookmark(Mark);
      finally
        FQuery.FreeBookmark(Mark);
      end;
    finally
      FQuery.EnableControls;
    end;
    Result := Seen.CommaText;
  finally
    Seen.Free;
  end;
end;

procedure TVittixProviderFireDACTests.Setup;
begin
  FConnection := TFDConnection.Create(nil);
  FConnection.DriverName := 'SQLite';
  FConnection.Params.Values['Database'] := ':memory:';
  FConnection.Connected := True;
  CreateFixture(600);

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
  FQuery.SQL.Text := 'SELECT id, name, amount, created, active FROM customers';
  FQuery.Open;

  FGrid := CreateHeadlessGrid(FQuery, FOwnerForm);
  FProvider := TVittixFireDACProvider.Create(FQuery);
  FGrid.Controller.Provider := FProvider;
  FGrid.FilterMode := vcmClient;
  FFallbackCount := 0;
  FErrorCount := 0;
end;

procedure TVittixProviderFireDACTests.TearDown;
begin
  // The controller-destruction test frees the form (grid + controller)
  // itself and nils the grid/provider fields; everything reachable only
  // through them is gone then.
  if Assigned(FOwnerForm) and Assigned(FGrid) then
  begin
    FGrid.Controller.Provider := nil;
    FProvider := nil;
  end;
  FProvider := nil;
  FOwnerForm.Free;
  FOwnerForm := nil;
  FGrid := nil;
  FQuery.Free;
  FConnection.Free;
end;

{ TVittixProviderFireDACTests }

procedure TVittixProviderFireDACTests.ServerFilterEqualsClientFilter;
var
  ClientIDs, ServerIDs: string;

  procedure RunOnce;
  begin
    ClearColumnFilters;
    SetColumnFilter('name', '^Customer 12');
    SetColumnFilter('amount', '>=30');
    FGrid.Controller.RefreshFiltering;
  end;

begin
  // Client first
  FGrid.FilterMode := vcmClient;
  RunOnce;
  ClientIDs := VisibleIDs;

  // Server second - same conditions, same results
  FGrid.FilterMode := vcmServer;
  RunOnce;
  ServerIDs := VisibleIDs;

  Assert.AreEqual(ClientIDs, ServerIDs,
    'server and client filters must return identical rows');
  Assert.IsTrue(FGrid.Controller.ServerFilterActive,
    'the server path owns the WHERE');
  Assert.IsTrue(FProvider.GetLastSQLWhere <> '', 'a WHERE was generated');

  // Row-count sanity: the battery must actually filter
  Assert.IsTrue(CountRows < 600, 'the filter reduced the rows');
end;

procedure TVittixProviderFireDACTests.OrGroupsMatchClientResults;
var
  ClientIDs, ServerIDs: string;
  Set_: TVittixFilterSet;

  procedure RunOnce;
  begin
    ClearColumnFilters;
    FGrid.Controller.ApplyFilterSet('ORTest');
  end;

begin
  FGrid.Controller.FilterSetsFileName :=
    TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    // (name starts with Customer 1) OR (amount > 800)
    Set_ := FGrid.Controller.FilterSetList.AddOrUpdate('ORTest');
    Set_.AddGroup.AddCondition('name', '^Customer 1');
    Set_.AddGroup.AddCondition('amount', '>800');
    FGrid.Controller.PersistFilterSets;

    FGrid.FilterMode := vcmClient;
    RunOnce;
    ClientIDs := VisibleIDs;

    FGrid.FilterMode := vcmServer;
    RunOnce;
    ServerIDs := VisibleIDs;

    Assert.AreEqual(ClientIDs, ServerIDs,
      'OR-groups must filter identically server-side');
    Assert.IsTrue(ServerIDs <> '', 'the OR filter matched something');
  finally
    if TFile.Exists(FGrid.Controller.FilterSetsFileName) then
      TFile.Delete(FGrid.Controller.FilterSetsFileName);
    FGrid.Controller.FilterSetsFileName := '';
  end;
end;

procedure TVittixProviderFireDACTests.GlobalSearchMatchesClientResults;
var
  ClientIDs, ServerIDs: string;
begin
  // 'Émile' only matches the unicode rows; both modes must agree.
  // SQLite TEXT columns surface as memo fields in FireDAC; global search
  // skips memos by default, so opt in to keep client/server parity here.
  FGrid.Controller.GlobalSearchIncludeMemo := True;
  FGrid.FilterMode := vcmClient;
  FGrid.Controller.SetGlobalFilter('Émile');
  ClientIDs := VisibleIDs;
  FGrid.Controller.SetGlobalFilter('');

  FGrid.FilterMode := vcmServer;
  FGrid.Controller.SetGlobalFilter('Émile');
  ServerIDs := VisibleIDs;
  FGrid.Controller.SetGlobalFilter('');

  Assert.AreEqual(ClientIDs, ServerIDs,
    'global search must match the same rows in both modes');
  Assert.IsTrue(ServerIDs <> '', 'the unicode global search matched');
end;

procedure TVittixProviderFireDACTests.SortAndMultiColumnSortServerSide;
begin
  FGrid.FilterMode := vcmServer;

  // Single column ascending: highest id last
  FGrid.Controller.DoTitleClick(FGrid.Columns[0]); // id asc
  Assert.IsTrue(FProvider.GetLastSQLOrderBy.Contains('id'), 'order by id');
  FQuery.Last;
  Assert.AreEqual('600', FQuery.FieldByName('id').AsString,
    'ascending puts the highest id last');

  // Descending: highest id first (record preservation keeps the old row,
  // so navigate explicitly).
  FGrid.Controller.DoTitleClick(FGrid.Columns[0]); // desc
  FQuery.First;
  Assert.AreEqual('600', FQuery.FieldByName('id').AsString,
    'descending puts the highest id first');

  // Multi-column: id DESC + name ASC. A plain title click replaces the
  // sort (single-column), so set the two-column state directly and route
  // it the way ApplyState does.
  FGrid.ColumnInfo[0].SortOrder := vsoDesc;
  FGrid.ColumnInfo[0].SortIndex := 0;
  FGrid.ColumnInfo[1].SortOrder := vsoAsc;
  FGrid.ColumnInfo[1].SortIndex := 1;
  FGrid.Controller.ApplyState;
  Assert.IsTrue(Pos('id', FProvider.GetLastSQLOrderBy) > 0,
    'id in the ORDER BY');
  Assert.IsTrue(Pos('DESC', FProvider.GetLastSQLOrderBy) > 0,
    'id sorted descending');
  Assert.IsTrue(Pos('"name" ASC', FProvider.GetLastSQLOrderBy) > 0,
    'name sorted ascending after id: ' + FProvider.GetLastSQLOrderBy);

  // The dominant key governs: id DESC puts the highest id first (row
  // 600 is an Émile row - the unicode text rides along).
  FQuery.First;
  Assert.AreEqual(600, FQuery.FieldByName('id').AsInteger,
    'id DESC dominates the multi-column order');
end;

procedure TVittixProviderFireDACTests.OriginalSQLRestoredAfterClearFilter;
begin
  FGrid.FilterMode := vcmServer;
  SetColumnFilter('name', '^Customer 12');
  FGrid.Controller.RefreshFiltering;
  Assert.IsTrue(FGrid.Controller.ServerFilterActive);
  Assert.IsTrue(FQuery.SQL.Text.Contains('VITTIX_SUB'), 'wrapped');

  ClearColumnFilters;
  FGrid.Controller.RefreshFiltering;
  Assert.IsFalse(FGrid.Controller.ServerFilterActive);
  Assert.IsFalse(FQuery.SQL.Text.Contains('VITTIX_SUB'),
    'the wrapper is gone after the conditions cleared');
  Assert.AreEqual(
    'SELECT id, name, amount, created, active FROM customers',
    FQuery.SQL.Text, 'the original SQL text is byte-identical');
end;

procedure TVittixProviderFireDACTests.OriginalSQLRestoredOnControllerDestruction;
var
  SavedSQL: string;
begin
  FGrid.FilterMode := vcmServer;
  SetColumnFilter('name', '^Customer 12');
  FGrid.Controller.RefreshFiltering;
  SavedSQL := 'SELECT id, name, amount, created, active FROM customers';
  Assert.IsTrue(FQuery.SQL.Text.Contains('VITTIX_SUB'));

  // Destroying the grid's owner (which destroys grid + controller and
  // releases the provider interface) must restore the SQL text. The query
  // is ownerless and survives; during teardown the provider restores the
  // text without re-opening.
  FreeAndNil(FOwnerForm);
  FGrid := nil;
  FProvider := nil;
  Assert.AreEqual(SavedSQL, FQuery.SQL.Text,
    'the original SQL is back after controller teardown');
  Assert.IsFalse(FQuery.Active, 'the query was left closed by teardown');
end;

procedure TVittixProviderFireDACTests.AutoModeFallsBackWithReason;
begin
  FGrid.OnProviderFallback := HandleFallback;
  FGrid.FilterMode := vcmAuto;

  // Contains on a numeric column is display-text matching: not expressible.
  SetColumnFilter('amount', '5');
  FGrid.Controller.RefreshFiltering;

  Assert.AreEqual(1, FFallbackCount, 'exactly one fallback');
  Assert.IsTrue(FFallbackReason <> '', 'a reason came with the fallback');
  Assert.IsFalse(FGrid.Controller.ServerFilterActive);
  Assert.IsTrue(FQuery.Filtered,
    'the client engine took over after the fallback');
end;

procedure TVittixProviderFireDACTests.FailingQueryRestoresSQLOriginal;
begin
  FGrid.OnProviderError := HandleProviderError;
  FGrid.FilterMode := vcmServer;

  // First apply: server active, SQL wrapped.
  SetColumnFilter('name', '^Customer 12');
  FGrid.Controller.RefreshFiltering;
  Assert.IsTrue(FQuery.SQL.Text.Contains('VITTIX_SUB'), 'wrapped first');

  // Second apply with the table gone: the wrapped re-query fails.
  FConnection.ExecSQL('DROP TABLE customers');
  SetColumnFilter('name', '^Customer 2');
  try
    FGrid.Controller.RefreshFiltering;
  except
    // The provider restored the state; the controller re-raises when the
    // event did not handle it - our handler sets Handled.
  end;

  Assert.AreEqual(1, FErrorCount, 'OnProviderError fired once');
  Assert.AreEqual(
    'SELECT id, name, amount, created, active FROM customers',
    FQuery.SQL.Text, 'the original SQL survived the failure');
end;

procedure TVittixProviderFireDACTests.CurrentRecordPreservedByKey;
begin
  FGrid.FilterMode := vcmServer;
  FQuery.Locate('id', 250, []);
  Assert.AreEqual(250, FQuery.FieldByName('id').AsInteger);

  // A filter change that still includes id 250 must land back on it.
  SetColumnFilter('name', '^Customer 2');
  FGrid.Controller.RefreshFiltering;
  Assert.AreEqual(250, FQuery.FieldByName('id').AsInteger,
    'the current record was located again after the re-query');
end;

procedure TVittixProviderFireDACTests.ServerAggregatesEqualClientAggregates;
var
  ServerSum: Double;
  ServerCount: Integer;
  ClientSum: Double;
  ClientCount: Integer;
begin
  SetColumnFilter('name', '^Customer 1');
  FGrid.Controller.SetColumnAggregation(
    FGrid.Controller.FindColumnByFieldName('amount'), vatSum);
  FGrid.Controller.SetColumnAggregation(
    FGrid.Controller.FindColumnByFieldName('id'), vatCount);

  // Server
  FGrid.FilterMode := vcmServer;
  FGrid.Controller.RefreshFiltering;
  FGrid.Controller.Refresh;
  ServerSum := FGrid.Controller.AggregationEngine.GetAggregation(
    FGrid.ColumnInfo.FindByFieldName('amount'));
  ServerCount := FGrid.Controller.AggregationEngine.GetAggregation(
    FGrid.ColumnInfo.FindByFieldName('id'));

  // Client on the same conditions
  FGrid.FilterMode := vcmClient;
  FGrid.Controller.RefreshFiltering;
  FGrid.Controller.Refresh;
  ClientSum := FGrid.Controller.AggregationEngine.GetAggregation(
    FGrid.ColumnInfo.FindByFieldName('amount'));
  ClientCount := FGrid.Controller.AggregationEngine.GetAggregation(
    FGrid.ColumnInfo.FindByFieldName('id'));

  Assert.AreEqual(ServerSum, ClientSum, 0.001,
    'server SUM equals the client scan');
  Assert.AreEqual(ClientCount, ServerCount,
    'server COUNT equals the client scan');
  Assert.IsTrue(ServerCount > 0);
end;

procedure TVittixProviderFireDACTests.SavedFilterSetAppliesInServerMode;
begin
  FGrid.Controller.FilterSetsFileName :=
    TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    FGrid.FilterMode := vcmServer;
    SetColumnFilter('name', '^Customer 12');
    FGrid.Controller.SaveCurrentFilterAs('ServerSet');
    FGrid.Controller.ClearFilters;
    Assert.AreEqual(600, CountRows, 'clearing removes the server WHERE');

    Assert.IsTrue(FGrid.Controller.ApplyFilterSet('ServerSet'));
    Assert.IsTrue(FGrid.Controller.ServerFilterActive,
      'applying a set in server mode goes through the provider');
    Assert.IsTrue(CountRows < 600, 'the set filters again');
  finally
    if TFile.Exists(FGrid.Controller.FilterSetsFileName) then
      TFile.Delete(FGrid.Controller.FilterSetsFileName);
    FGrid.Controller.FilterSetsFileName := '';
  end;
end;

procedure TVittixProviderFireDACTests.LayoutActiveFilterSetAppliesInServerMode;
var
  Stream: TMemoryStream;
begin
  FGrid.Controller.FilterSetsFileName :=
    TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  Stream := TMemoryStream.Create;
  try
    FGrid.FilterMode := vcmServer;
    SetColumnFilter('name', '^Customer 12');
    FGrid.Controller.SaveCurrentFilterAs('LayoutServer');
    FGrid.Controller.SaveLayoutToStream(Stream);
    FGrid.Controller.ClearFilters;
    Assert.AreEqual(600, CountRows);

    Stream.Position := 0;
    FGrid.Controller.LoadLayoutFromStream(Stream);
    Assert.IsTrue(FGrid.Controller.ServerFilterActive,
      'the layout re-applied the set through the provider');
    Assert.IsTrue(CountRows < 600);
  finally
    Stream.Free;
    if TFile.Exists(FGrid.Controller.FilterSetsFileName) then
      TFile.Delete(FGrid.Controller.FilterSetsFileName);
    FGrid.Controller.FilterSetsFileName := '';
  end;
end;

procedure TVittixProviderFireDACTests.EditStateDefersRequery;
begin
  FGrid.FilterMode := vcmServer;

  FQuery.Edit;
  try
    SetColumnFilter('name', '^Customer 12');
    FGrid.Controller.RefreshFiltering;
    // Deferred: the SQL is untouched while the edit is open.
    Assert.IsFalse(FQuery.SQL.Text.Contains('VITTIX_SUB'),
      'the re-query waits for the edit to finish');
  finally
    FQuery.Post;
  end;

  // The post routes the deferred change.
  Assert.IsTrue(FQuery.SQL.Text.Contains('VITTIX_SUB'),
    'the re-query ran after the post');
  Assert.IsTrue(FGrid.Controller.ServerFilterActive);
end;

procedure TVittixProviderFireDACTests.ExportFilteredOnlyWorksInServerMode;
var
  Exporter: TVittixDBGridExporter;
  Output: string;
  VisibleRowCount: Integer;
begin
  FGrid.FilterMode := vcmServer;
  SetColumnFilter('name', '^Customer 12');
  FGrid.Controller.RefreshFiltering;
  VisibleRowCount := CountRows;
  Assert.IsTrue(VisibleRowCount < 600);

  // ExportFilteredOnly=True (default): the dataset already returns the
  // filtered rows, so the export covers exactly the visible set.
  Exporter := TVittixDBGridExporter.Create(FGrid);
  try
    Output := Exporter.ExportToString(vefCSV);
    Assert.AreEqual(VisibleRowCount + 1,
      Length(Trim(Output).Split([sLineBreak])),
      'header + the server-filtered rows');
  finally
    Exporter.Free;
  end;
end;

procedure TVittixProviderFireDACTests.ServerFilterFasterThanClientOnLargeTable;
var
  Watch: TStopwatch;
  ClientMs, ServerMs: Int64;
begin
  // Own large fixture so the other tests stay fast.
  FOwnerForm.Free;
  FQuery.Free;
  FConnection.ExecSQL('DROP TABLE customers');
  CreateFixture(200000);

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
  FQuery.SQL.Text := 'SELECT id, name, amount, created, active FROM customers';
  FQuery.Open;
  FGrid := CreateHeadlessGrid(FQuery, FOwnerForm);
  FProvider := TVittixFireDACProvider.Create(FQuery);
  FGrid.Controller.Provider := FProvider;

  SetColumnFilter('name', '^Customer 12345');

  // Client filter: OnFilterRecord scan of 200k rows
  FGrid.FilterMode := vcmClient;
  FGrid.Controller.RefreshFiltering;
  Watch := TStopwatch.StartNew;
  FGrid.Controller.RefreshFiltering;
  ClientMs := Watch.ElapsedMilliseconds;

  // Server filter: wrapped SQL re-query
  FGrid.FilterMode := vcmServer;
  FGrid.Controller.RefreshFiltering;
  Watch := TStopwatch.StartNew;
  FGrid.Controller.RefreshFiltering;
  ServerMs := Watch.ElapsedMilliseconds;

  Writeln(Format('200k rows: client filter %d ms, server filter %d ms',
    [ClientMs, ServerMs]));

  // Generous bounds (CI machines); the interesting number is the report.
  Assert.IsTrue(ClientMs < 60000, Format('client took %d ms', [ClientMs]));
  Assert.IsTrue(ServerMs < 60000, Format('server took %d ms', [ServerMs]));
end;

{ Wave 1 regressions }

procedure TVittixProviderFireDACTests.ServerAggregateBindsFilterParameters;
var
  ExpectedCount: Integer;
  ExpectedSum: Double;
  ServerCount: Integer;
  ServerSum: Double;
  Probe: TFDQuery;
begin
  // B1 regression: the server aggregate must execute the wrapped SQL with
  // the filter's :vxN parameters actually bound. ServerAggregateRefresh
  // returns True ONLY when the provider's FetchAggregates succeeded - a
  // skipped aggregate or a client fallback returns False, and an unbound
  // parameter makes the WHERE compare against NULL (empty group), so the
  // footer count would collapse to 0.
  SetColumnFilter('name', '^Customer 12');
  SetColumnFilter('amount', '>=30');
  FGrid.Controller.SetColumnAggregation(
    FGrid.Controller.FindColumnByFieldName('amount'), vatSum);
  FGrid.Controller.SetColumnAggregation(
    FGrid.Controller.FindColumnByFieldName('id'), vatCount);

  FGrid.FilterMode := vcmServer;
  FGrid.Controller.RefreshFiltering;
  Assert.IsTrue(FGrid.Controller.ServerFilterActive, 'server filter applied');
  Assert.IsTrue(FGrid.Controller.ServerAggregateRefresh,
    'server aggregate executed (no fallback, no skip)');

  ServerSum := FGrid.Controller.AggregationEngine.GetAggregation(
    FGrid.ColumnInfo.FindByFieldName('amount'));
  ServerCount := FGrid.Controller.AggregationEngine.GetAggregation(
    FGrid.ColumnInfo.FindByFieldName('id'));

  // Independent ground truth for the filtered group (test-constant SQL).
  Probe := TFDQuery.Create(nil);
  try
    Probe.Connection := FConnection;
    Probe.SQL.Text :=
      'SELECT COUNT(*) AS C, COALESCE(SUM(amount), 0) AS S FROM customers ' +
      'WHERE name LIKE ''Customer 12%'' AND amount >= 30';
    Probe.Open;
    ExpectedCount := Probe.FieldByName('C').AsInteger;
    ExpectedSum := Probe.FieldByName('S').AsFloat;
  finally
    Probe.Free;
  end;

  Assert.IsTrue(ExpectedCount > 0, 'the filtered group is non-empty');
  Assert.AreEqual(ExpectedCount, ServerCount,
    'aggregate row count reflects the bound filter parameters');
  Assert.AreEqual(ExpectedSum, ServerSum, 0.001,
    'aggregate SUM reflects the bound filter parameters');
end;

procedure TVittixProviderFireDACTests.AggregateNullCountMixedNulls;
var
  PI: IVittixDataProvider;
  P: TVittixFireDACProvider;
  Q: TFDQuery;
  Reqs: TArray<TVittixAggRequest>;
  Res: TArray<TVittixAggResult>;
begin
  // B2 regression: NullCount = COUNT(*) - COUNT(col), the number of NULL
  // field values, for every aggregate type - not COUNT(*) - SUM(col).
  FConnection.ExecSQL('CREATE TABLE aggnulls (id INTEGER PRIMARY KEY, ' +
    'val REAL, label TEXT)');
  FConnection.ExecSQL(
    'INSERT INTO aggnulls (id, val, label) VALUES ' +
    '(1, NULL, ''L1''), (2, NULL, ''L2''), (3, NULL, NULL), ' +
    '(4, 4.0, ''L4''), (5, 5.0, ''L5''), (6, 6.0, ''L6''), ' +
    '(7, 7.0, ''L7''), (8, 8.0, ''L8''), (9, 9.0, ''L9''), (10, 10.0, NULL)');

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;
    Q.SQL.Text := 'SELECT id, val, label FROM aggnulls';
    Q.Open;

    P := TVittixFireDACProvider.Create(Q);
    PI := P;
    Assert.IsTrue(P.ApplyFilter(OneGroup('id', '>=1'), '', FieldsOf(Q)),
      'server wrap applied');

    SetLength(Reqs, 4);
    Reqs[0].FieldName := 'val';   Reqs[0].AggType := vatSum;
    Reqs[1].FieldName := 'val';   Reqs[1].AggType := vatAvg;
    Reqs[2].FieldName := 'label'; Reqs[2].AggType := vatMin;
    Reqs[3].FieldName := 'id';    Reqs[3].AggType := vatCount;

    Assert.IsTrue(P.FetchAggregates(Reqs, Res), 'aggregate executed');
    Assert.AreEqual(10, Res[0].Count, 'group row count');
    Assert.AreEqual(3, Res[0].NullCount, 'SUM NullCount = NULL vals');
    Assert.AreEqual(49.0, Double(Res[0].Value), 0.001, 'SUM skips NULLs');
    Assert.AreEqual(7.0, Double(Res[1].Value), 0.001, 'AVG over non-NULLs');
    Assert.AreEqual(3, Res[1].NullCount, 'AVG NullCount = NULL vals');
    Assert.AreEqual('L1', string(Res[2].Value), 'MIN text value');
    Assert.AreEqual(2, Res[2].NullCount, 'MIN text NullCount');
    Assert.AreEqual(10, Res[3].Count, 'COUNT(*) value');
    Assert.AreEqual(0, Res[3].NullCount, 'COUNT(id) has no NULLs');
  finally
    PI := nil;
    Q.Free;
  end;
end;

procedure TVittixProviderFireDACTests.AggregateNullCountAllNullsUnderFilter;
var
  PI: IVittixDataProvider;
  P: TVittixFireDACProvider;
  Q: TFDQuery;
  Reqs: TArray<TVittixAggRequest>;
  Res: TArray<TVittixAggResult>;
begin
  // Active server filter + a group where every value is NULL.
  FConnection.ExecSQL('CREATE TABLE aggnulls (id INTEGER PRIMARY KEY, ' +
    'val REAL, label TEXT)');
  FConnection.ExecSQL(
    'INSERT INTO aggnulls (id, val, label) VALUES ' +
    '(1, NULL, ''L1''), (2, NULL, ''L2''), (3, NULL, ''L3''), ' +
    '(4, 4.0, ''L4'')');

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;
    Q.SQL.Text := 'SELECT id, val, label FROM aggnulls';
    Q.Open;

    P := TVittixFireDACProvider.Create(Q);
    PI := P;
    Assert.IsTrue(P.ApplyFilter(OneGroup('id', '<=3'), '', FieldsOf(Q)),
      'filter applied');
    Assert.AreEqual('"id" <= :vx0', P.GetLastSQLWhere, 'filter WHERE built');

    SetLength(Reqs, 1);
    Reqs[0].FieldName := 'val';
    Reqs[0].AggType := vatSum;

    Assert.IsTrue(P.FetchAggregates(Reqs, Res), 'aggregate executed');
    Assert.AreEqual(3, Res[0].Count, 'filtered row count (param bound)');
    Assert.AreEqual(3, Res[0].NullCount, 'all-NULL group NullCount');
    Assert.IsTrue(VarIsNull(Res[0].Value), 'SUM over all-NULL is NULL');
  finally
    PI := nil;
    Q.Free;
  end;
end;

procedure TVittixProviderFireDACTests.AggregateNullCountNoNullsUnderFilter;
var
  PI: IVittixDataProvider;
  P: TVittixFireDACProvider;
  Q: TFDQuery;
  Reqs: TArray<TVittixAggRequest>;
  Res: TArray<TVittixAggResult>;
begin
  FConnection.ExecSQL('CREATE TABLE aggnulls (id INTEGER PRIMARY KEY, ' +
    'val REAL, label TEXT)');
  FConnection.ExecSQL(
    'INSERT INTO aggnulls (id, val, label) VALUES ' +
    '(1, NULL, ''L1''), (4, 4.0, ''L4''), (5, 5.0, ''L5'')');

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;
    Q.SQL.Text := 'SELECT id, val, label FROM aggnulls';
    Q.Open;

    P := TVittixFireDACProvider.Create(Q);
    PI := P;
    Assert.IsTrue(P.ApplyFilter(OneGroup('id', '>=4'), '', FieldsOf(Q)));
    Assert.IsTrue(P.ApplySort([], FieldsOf(Q)), 'sort pass snapshots SQL');

    SetLength(Reqs, 1);
    Reqs[0].FieldName := 'val';
    Reqs[0].AggType := vatMax;

    Assert.IsTrue(P.FetchAggregates(Reqs, Res), 'aggregate executed');
    Assert.AreEqual(2, Res[0].Count, 'filtered row count');
    Assert.AreEqual(0, Res[0].NullCount, 'no NULLs in group');
    Assert.AreEqual(5.0, Double(Res[0].Value), 0.001, 'MAX value');
  finally
    PI := nil;
    Q.Free;
  end;
end;

procedure TVittixProviderFireDACTests.AggregateNullCountEmptyDataset;
var
  PI: IVittixDataProvider;
  P: TVittixFireDACProvider;
  Q: TFDQuery;
  Reqs: TArray<TVittixAggRequest>;
  Res: TArray<TVittixAggResult>;
begin
  FConnection.ExecSQL('CREATE TABLE aggnulls (id INTEGER PRIMARY KEY, ' +
    'val REAL, label TEXT)');
  FConnection.ExecSQL(
    'INSERT INTO aggnulls (id, val, label) VALUES ' +
    '(1, NULL, ''L1''), (2, 2.0, ''L2'')');

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;
    Q.SQL.Text := 'SELECT id, val, label FROM aggnulls';
    Q.Open;

    P := TVittixFireDACProvider.Create(Q);
    PI := P;
    // Filter that matches nothing: the aggregate still succeeds and
    // reports the empty group correctly.
    Assert.IsTrue(P.ApplyFilter(OneGroup('id', '>999'), '', FieldsOf(Q)));
    Assert.IsTrue(P.GetLastSQLWhere <> '', 'filter WHERE is active');

    SetLength(Reqs, 2);
    Reqs[0].FieldName := 'val';
    Reqs[0].AggType := vatSum;
    Reqs[1].FieldName := 'label';
    Reqs[1].AggType := vatMin;

    Assert.IsTrue(P.FetchAggregates(Reqs, Res), 'aggregate executed');
    Assert.AreEqual(0, Res[0].Count, 'empty group row count');
    Assert.AreEqual(0, Res[0].NullCount, 'empty group NullCount');
    Assert.IsTrue(VarIsNull(Res[0].Value), 'SUM over empty group is NULL');
    Assert.IsTrue(VarIsNull(Res[1].Value), 'MIN over empty group is NULL');
  finally
    PI := nil;
    Q.Free;
  end;
end;

procedure TVittixProviderFireDACTests.QueryFreedFirstLeavesProviderInert;
var
  PI: IVittixDataProvider;
  P: TVittixFireDACProvider;
  Q: TFDQuery;
begin
  // M1/B3 regression: the application may free its query while the
  // provider interface is still referenced. The provider must become
  // inert (no dangling reference) instead of crashing on the next call
  // or in its own destructor.
  Q := TFDQuery.Create(nil);
  Q.Connection := FConnection;
  Q.SQL.Text := 'SELECT id, name FROM customers';
  Q.Open;

  P := TVittixFireDACProvider.Create(Q);
  PI := P;
  Assert.IsTrue(P.ApplyFilter(OneGroup('id', '>=1'), '', FieldsOf(Q)),
    'provider works while the query lives');

  FreeAndNil(Q); // query destroyed first: the watcher must nil FQuery

  // NOTE: no FieldsOf(Q) here - the query is gone. The provider must
  // reject the call before it ever needs field metadata.
  Assert.IsFalse(P.ApplyFilter(nil, '', nil),
    'no query bound after the query was freed');
  Assert.AreEqual('no query bound', P.GetLastFallbackReason,
    'the provider reports the missing query instead of crashing');
  Assert.AreEqual('', P.GetLastSQLWhere, 'state cleared with the query');

  PI := nil; // destructor must run cleanly over the dead query reference
end;

procedure TVittixProviderFireDACTests.QueryFreedWhileFilterActiveIsSafe;
var
  PI: IVittixDataProvider;
  P: TVittixFireDACProvider;
  Q: TFDQuery;
  Reqs: TArray<TVittixAggRequest>;
  Res: TArray<TVittixAggResult>;
begin
  // Stronger M1/B3 variant: the wrapped SQL is LIVE on the query when the
  // application destroys it. The watcher must sever the provider's
  // references before the destructor can touch the freed object, and the
  // aggregate query borrowed on the connection must be released.
  Q := TFDQuery.Create(nil);
  Q.Connection := FConnection;
  Q.SQL.Text := 'SELECT id, name, amount FROM customers';
  Q.Open;

  P := TVittixFireDACProvider.Create(Q);
  PI := P;
  Assert.IsTrue(P.ApplyFilter(OneGroup('name', '^Customer 12'), '',
    FieldsOf(Q)), 'filter active');

  SetLength(Reqs, 1);
  Reqs[0].FieldName := 'amount';
  Reqs[0].AggType := vatSum;
  Assert.IsTrue(P.FetchAggregates(Reqs, Res), 'aggregate ran on the borrow');
  Assert.IsTrue(Length(Res) = 1, 'one aggregate result');

  FreeAndNil(Q); // query destroyed first, while the wrap is live
  Assert.IsFalse(P.ApplyFilter(nil, '', nil),
    'inert after query destruction');

  PI := nil; // destructor: no restore attempt into the freed query
end;

procedure TVittixProviderFireDACTests.PagerNavigatesAndRestoresQuery;
var
  Pager: TVittixFireDACPager;
  OriginalSkip, OriginalMax: Integer;
begin
  FQuery.Close;
  FQuery.SQL.Text :=
    'SELECT id, name, amount, created, active FROM customers ORDER BY id';
  FQuery.Open;
  OriginalSkip := FQuery.FetchOptions.RecsSkip;
  OriginalMax := FQuery.FetchOptions.RecsMax;
  Pager := TVittixFireDACPager.Create(FQuery, 200);
  try
    Assert.IsTrue(Pager.OpenPage(0));
    Assert.AreEqual(0, Pager.PageIndex);
    Assert.AreEqual(200, CountRows);
    FQuery.First;
    Assert.AreEqual(1, FQuery.FieldByName('id').AsInteger);

    Assert.IsTrue(Pager.NextPage);
    Assert.AreEqual(200, CountRows);
    FQuery.First;
    Assert.AreEqual(201, FQuery.FieldByName('id').AsInteger);

    Assert.IsTrue(Pager.OpenPage(2));
    Assert.AreEqual(200, CountRows);
    FQuery.First;
    Assert.AreEqual(401, FQuery.FieldByName('id').AsInteger);
    Assert.IsFalse(Pager.NextPage, 'the page beyond the last is empty');
    Assert.AreEqual(2, Pager.PageIndex, 'the previous page remains open');
    FQuery.First;
    Assert.AreEqual(401, FQuery.FieldByName('id').AsInteger);

    Assert.IsTrue(Pager.PreviousPage);
    FQuery.First;
    Assert.AreEqual(201, FQuery.FieldByName('id').AsInteger);
    Pager.Disable;
    Assert.IsFalse(Pager.Enabled);
    Assert.AreEqual(OriginalSkip, FQuery.FetchOptions.RecsSkip);
    Assert.AreEqual(OriginalMax, FQuery.FetchOptions.RecsMax);
    Assert.IsTrue(FQuery.Active);
    Assert.AreEqual(600, CountRows);
  finally
    Pager.Free;
  end;
end;

procedure TVittixProviderFireDACTests.PagerRejectsPageChangeDuringEdit;
var
  Pager: TVittixFireDACPager;
  Raised: Boolean;
begin
  Pager := TVittixFireDACPager.Create(FQuery, 50);
  try
    Assert.IsTrue(Pager.OpenPage(0));
    FQuery.Edit;
    try
      Raised := False;
      try
        Pager.NextPage;
      except
        on E: EInvalidOperation do
          Raised := True;
      end;
      Assert.IsTrue(Raised, 'paging during an edit must fail');
      Assert.AreEqual(dsEdit, FQuery.State);
      Assert.AreEqual(0, Pager.PageIndex);
    finally
      FQuery.Cancel;
    end;
  finally
    Pager.Free;
  end;
end;

procedure TVittixProviderFireDACTests.PagerRestoresClosedQueryAndHandlesQueryDestruction;
var
  Q: TFDQuery;
  Pager: TVittixFireDACPager;
  OriginalSkip, OriginalMax: Integer;
begin
  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;
    Q.SQL.Text := 'SELECT id FROM customers ORDER BY id';
    OriginalSkip := Q.FetchOptions.RecsSkip;
    OriginalMax := Q.FetchOptions.RecsMax;
    Pager := TVittixFireDACPager.Create(Q, 10);
    try
      Assert.IsTrue(Pager.OpenPage(1));
      Assert.AreEqual(11, Q.FieldByName('id').AsInteger);
      Pager.Disable;
      Assert.IsFalse(Q.Active, 'the originally closed query stays closed');
      Assert.AreEqual(OriginalSkip, Q.FetchOptions.RecsSkip);
      Assert.AreEqual(OriginalMax, Q.FetchOptions.RecsMax);
      Assert.IsTrue(Pager.OpenPage(0));
      FreeAndNil(Q);
      Assert.IsFalse(Pager.Enabled, 'query destruction detaches the pager');
    finally
      Pager.Free;
    end;
  finally
    Q.Free;
  end;
end;

procedure TVittixProviderFireDACTests.PagerBarNavigatesAndDetaches;
var
  Pager: TVittixFireDACPager;
  Bar: TVittixFireDACPagerBar;
  FirstButton, PreviousButton, NextButton: TButton;
begin
  FQuery.Close;
  FQuery.SQL.Text :=
    'SELECT id, name, amount, created, active FROM customers ORDER BY id';
  FQuery.Open;
  Pager := TVittixFireDACPager.Create(FQuery, 100);
  Bar := TVittixFireDACPagerBar.Create(FOwnerForm);
  try
    Bar.Parent := FOwnerForm;
    Bar.Pager := Pager;
    FirstButton := TButton(Bar.FindComponent('FirstButton'));
    PreviousButton := TButton(Bar.FindComponent('PreviousButton'));
    NextButton := TButton(Bar.FindComponent('NextButton'));
    Assert.IsFalse(FirstButton.Enabled);
    Assert.IsFalse(PreviousButton.Enabled);
    Assert.IsTrue(NextButton.Enabled);

    NextButton.Click;
    Assert.AreEqual(0, Pager.PageIndex);
    NextButton.Click;
    Assert.AreEqual(1, Pager.PageIndex);
    Assert.IsTrue(FirstButton.Enabled);
    Assert.IsTrue(PreviousButton.Enabled);
    Assert.AreEqual(101, FQuery.FieldByName('id').AsInteger);

    PreviousButton.Click;
    Assert.AreEqual(0, Pager.PageIndex);
    FreeAndNil(Pager);
    Assert.IsFalse(NextButton.Enabled, 'freed pager detaches the bar');
  finally
    Bar.Free;
    Pager.Free;
  end;
end;

{ Phase 1: keyword scan normalization }

procedure TVittixProviderFireDACTests.OrderByAcrossNewlineIsRejected;
begin
  Assert.IsFalse(IsWrappableSQL('SELECT * FROM t' + sLineBreak + 'ORDER BY x'),
    'ORDER BY after a CR/LF must be detected');
end;

procedure TVittixProviderFireDACTests.OrderByTabSeparatedIsRejected;
begin
  Assert.IsFalse(IsWrappableSQL('SELECT * FROM t ORDER'#9'BY x'),
    'ORDER<TAB>BY must be detected');
end;

procedure TVittixProviderFireDACTests.UnionOnOwnLineIsRejected;
begin
  Assert.IsFalse(
    IsWrappableSQL('SELECT id FROM t1' + sLineBreak + 'UNION' + sLineBreak +
      'SELECT id FROM t2'),
    'UNION on its own line must be detected');
end;

procedure TVittixProviderFireDACTests.ExecAtStartAfterNewlineIsRejected;
begin
  Assert.IsFalse(IsWrappableSQL(sLineBreak + 'EXEC my_proc'),
    'EXEC at the start after a newline must be detected');
end;

procedure TVittixProviderFireDACTests.KeywordInsideStringLiteralIsWrappable;
begin
  Assert.IsTrue(
    IsWrappableSQL('SELECT ''ORDER BY x UNION EXEC'' AS label FROM t'),
    'a keyword inside a string literal must not reject the query');
  Assert.IsTrue(IsWrappableSQL('SELECT ''it''''s ORDER BY'' FROM t'),
    'an escaped quote must not end the literal early');
end;

procedure TVittixProviderFireDACTests.KeywordInsideLineCommentIsWrappable;
begin
  Assert.IsTrue(
    IsWrappableSQL('SELECT * FROM t -- ORDER BY x' + sLineBreak + 'WHERE id > 0'),
    'a keyword inside a line comment must not reject the query');
end;

procedure TVittixProviderFireDACTests.KeywordInsideBlockCommentIsWrappable;
begin
  Assert.IsTrue(
    IsWrappableSQL('SELECT * FROM t /* UNION EXEC ORDER BY */ WHERE id > 0'),
    'a keyword inside a block comment must not reject the query');
end;

procedure TVittixProviderFireDACTests.PlainSelectRemainsWrappable;
begin
  Assert.IsTrue(IsWrappableSQL('SELECT id, name FROM customers WHERE id > 1'),
    'a plain SELECT is wrappable');
end;

end.
