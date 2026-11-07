unit Vittix.Tests.FilterSets;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.IOUtils,
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.DBGrids,
  Vcl.ExtCtrls,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Layout,
  Vittix.DBGrid.FilterSets,
  Vittix.DBGrid.Controller;

type
  [TestFixture]
  TVittixFilterSetTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FAppliedCount: Integer;
    FAppliedName: string;
    FChangedCount: Integer;
    procedure HandleFilterSetApplied(Sender: TObject; const AName: string);
    procedure HandleFilterSetsChanged(Sender: TObject);
    procedure ApplyLegacyFilter(const AFieldName, AFilterText: string);
    function VisibleIDs: string;
    function CommaNames(List: TVittixFilterSetList): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // B1: group semantics on the engine
    [Test]
    procedure GroupAndWithinOrBetween;
    [Test]
    procedure EmptyGroupIgnored;
    [Test]
    procedure LegacySingleGroupEquivalence;
    [Test]
    procedure GlobalSearchStillOrsOnTopOfGroups;

    // B2: controller API + persistence
    [Test]
    procedure SaveCurrentFilterAsThenApplyRestoresRecords;
    [Test]
    procedure ListPersistenceRoundTrip;
    [Test]
    procedure CorruptFileRaisesOnFirstUse;
    [Test]
    procedure TwoGridsUseSeparateFiles;
    [Test]
    procedure RenameDeleteAndReorder;
    [Test]
    procedure EventsFireExactlyOnce;
    [Test]
    procedure ClearFiltersClearsActiveSetName;
    [Test]
    procedure ClosedDatasetApplySurvivesReopen;

    // B2: layout integration
    [Test]
    procedure LayoutRestoresActiveFilterSet;
    [Test]
    procedure LayoutSkipsUnknownFilterSet;

    // Wave 2 regressions: M4 replacement, M5 duplicate handling,
    // M3 dialog persistence/cancel
    [Test]
    procedure SaveCurrentFilterAsReplacesExistingGroups;
    [Test]
    procedure DuplicateNamesFailLoadExactly;
    [Test]
    procedure DuplicateNamesFailLoadCaseInsensitively;
    [Test]
    procedure EmptyNameFailsLoad;
    [Test]
    procedure WrongTypedFilterSetsKeyRaisesFilterSetError;
    [Test]
    procedure FailedLoadPreservesInMemoryState;
    [Test]
    procedure JSONRoundTripMatchesFileRoundTrip;
    [Test]
    procedure DialogEditsDoNotNotifyUntilClose;
    [Test]
    procedure DialogCancelRevertsEdits;
    [Test]
    procedure DialogSaveCurrentAsCancelKeepsOnlyTheSavedSet;
    [Test]
    procedure DialogApplyPersistsOnceAndRoundTrips;
  end;

implementation

uses
  Vittix.Tests.TestData,
  Vittix.DBGrid.FilterSets.Dialog,
  Vittix.DBGrid.Filter.Engine;

{ helpers }

type
  { Drives the modal filter-set manager from the message loop: the timer
    tick runs the per-test edit sequence, then sets the close result. }
  TFilterSetDialogDriver = class
  public
    Done: Boolean;
    Dlg: TVittixDBGridFilterSetManager;
    EditProc: TProc;
    FinishResult: Integer;
    SaveCurrentAsName: string;
    SaveCurrentAsPromptHandled: Boolean;
    ErrorText: string;
    procedure OnTimerTick(Sender: TObject);
  end;

procedure TFilterSetDialogDriver.OnTimerTick(Sender: TObject);
var
  I, J: Integer;
  Prompt: TForm;
begin
  if Done then
  begin
    // InputQuery opens its own modal form while the manager's timer callback
    // is active. Handle that nested prompt on the next timer message.
    if (SaveCurrentAsName <> '') and not SaveCurrentAsPromptHandled then
    begin
      for I := 0 to Screen.FormCount - 1 do
      begin
        Prompt := Screen.Forms[I];
        if Prompt.Caption <> 'Save Current Filters' then
          Continue;
        for J := 0 to Prompt.ComponentCount - 1 do
          if Prompt.Components[J] is TEdit then
            TEdit(Prompt.Components[J]).Text := SaveCurrentAsName;
        SaveCurrentAsPromptHandled := True;
        Prompt.ModalResult := mrOk;
        Break;
      end;
    end;
    Exit;
  end;
  Done := True;
  try
    if not Assigned(Dlg) then
      ErrorText := 'driver has no dialog reference';
    try
      if Assigned(EditProc) then
        EditProc();
    except
      // Surface failures as test assertions instead of an Application
      // error box that blocks a headless run.
      on E: Exception do
        ErrorText := E.ClassName + ': ' + E.Message;
    end;
    if Assigned(Dlg) then
      Dlg.ModalResult := FinishResult;
  except
    on E: Exception do
      ErrorText := E.ClassName + ': ' + E.Message;
  end;
end;



function Condition(const AFieldName, AFilterText: string): TVittixFilterCondition;
begin
  Result.FieldName := AFieldName;
  Result.FilterText := AFilterText;
end;

{ TVittixFilterSetTests }

procedure TVittixFilterSetTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  FAppliedCount := 0;
  FChangedCount := 0;
end;

procedure TVittixFilterSetTests.TearDown;
begin
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixFilterSetTests.HandleFilterSetApplied(Sender: TObject;
  const AName: string);
begin
  Inc(FAppliedCount);
  FAppliedName := AName;
end;

procedure TVittixFilterSetTests.HandleFilterSetsChanged(Sender: TObject);
begin
  Inc(FChangedCount);
end;

procedure TVittixFilterSetTests.ApplyLegacyFilter(const AFieldName,
  AFilterText: string);
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName(AFieldName);
  Assert.IsNotNull(Info, AFieldName);
  Info.FilterText := AFilterText;
  Info.HasFilter := True;
end;

function TVittixFilterSetTests.VisibleIDs: string;
var
  Seen: TStringList;
begin
  Seen := TStringList.Create;
  try
    FDataSet.DisableControls;
    try
      FDataSet.First;
      while not FDataSet.Eof do
      begin
        Seen.Add(FDataSet.FieldByName('ID').AsString);
        FDataSet.Next;
      end;
    finally
      FDataSet.EnableControls;
    end;
    Result := Seen.CommaText;
  finally
    Seen.Free;
  end;
end;

procedure TVittixFilterSetTests.GroupAndWithinOrBetween;
var
  Groups: TVittixFilterConditionGroups;
begin
  // (Name starts with A) OR (Amount > 300 AND IsActive True)
  SetLength(Groups, 2);
  SetLength(Groups[0], 1);
  Groups[0][0] := Condition('Name', '^A');
  SetLength(Groups[1], 2);
  Groups[1][0] := Condition('Amount', '>300');
  Groups[1][1] := Condition('IsActive', '=True');

  FGrid.Controller.FilterEngine.SetFilterGroups(Groups);
  FGrid.Controller.FilterEngine.Active := True;

  // Fixture: 1 Alpha(100.50,T) 2 beta(200,F) 3 Gamma(1,T) 4 Alpha(50.25,T)
  //          5 Delta(400,F). Group 1 -> IDs 1,4; group 2 -> ID 5 (Amount
  //          matches but IsActive is False, so no). Amount > 300 alone
  //          would be ID 5; the AND keeps it out.
  Assert.AreEqual('1,4', VisibleIDs, 'AND within a group, OR between groups');
end;

procedure TVittixFilterSetTests.EmptyGroupIgnored;
var
  Groups: TVittixFilterConditionGroups;
begin
  // One real group plus one empty group: the empty group is ignored, so
  // the result is exactly the real group's.
  SetLength(Groups, 2);
  SetLength(Groups[0], 1);
  Groups[0][0] := Condition('Name', '^A');
  SetLength(Groups[1], 0);

  FGrid.Controller.FilterEngine.SetFilterGroups(Groups);
  FGrid.Controller.FilterEngine.Active := True;
  Assert.AreEqual('1,4', VisibleIDs, 'an empty group is ignored');

  // Only empty groups: everything passes.
  SetLength(Groups, 2);
  SetLength(Groups[0], 0);
  SetLength(Groups[1], 0);
  FGrid.Controller.FilterEngine.SetFilterGroups(Groups);
  Assert.AreEqual('1,2,3,4,5', VisibleIDs,
    'all-empty groups behave like no filter');
end;

procedure TVittixFilterSetTests.LegacySingleGroupEquivalence;
var
  Groups: TVittixFilterConditionGroups;
  LegacyIDs, GroupIDs: string;
begin
  // A battery of operators through BOTH paths must produce the same rows.
  ApplyLegacyFilter('Name', '^A');
  ApplyLegacyFilter('Amount', '..50|250');
  ApplyLegacyFilter('Score', '>=3.5');
  FGrid.Controller.FilterEngine.Active := True;
  LegacyIDs := VisibleIDs;

  // Reset the legacy texts and express the same conditions as ONE group.
  FGrid.Controller.ClearFilters;
  SetLength(Groups, 1);
  SetLength(Groups[0], 3);
  Groups[0][0] := Condition('Name', '^A');
  Groups[0][1] := Condition('Amount', '..50|250');
  Groups[0][2] := Condition('Score', '>=3.5');
  FGrid.Controller.FilterEngine.SetFilterGroups(Groups);
  FGrid.Controller.FilterEngine.Active := True;
  GroupIDs := VisibleIDs;

  Assert.AreEqual(LegacyIDs, GroupIDs,
    'one AND-group must equal the classic per-column AND');
  Assert.AreEqual('1,4', GroupIDs, 'precondition: the battery filters to 1,4');
end;

procedure TVittixFilterSetTests.GlobalSearchStillOrsOnTopOfGroups;
var
  Groups: TVittixFilterConditionGroups;
begin
  SetLength(Groups, 1);
  SetLength(Groups[0], 1);
  Groups[0][0] := Condition('Name', '^A');
  FGrid.Controller.FilterEngine.SetFilterGroups(Groups);
  FGrid.Controller.FilterEngine.Active := True;
  Assert.AreEqual('1,4', VisibleIDs);

  // The global search ORs across all columns on top of the group result.
  FGrid.Controller.SetGlobalFilter('Gamma');
  Assert.AreEqual('3', VisibleIDs,
    'group AND global search: only Gamma matches the global text');
  FGrid.Controller.SetGlobalFilter('');
end;

procedure TVittixFilterSetTests.SaveCurrentFilterAsThenApplyRestoresRecords;
var
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    FGrid.Controller.FilterSetsFileName := TempFile;
    ApplyLegacyFilter('Name', '^A');
    FGrid.Controller.FilterEngine.Active := True;

    FGrid.Controller.SaveCurrentFilterAs('Alpha rows');
    Assert.AreEqual('Alpha rows', FGrid.Controller.ActiveFilterSetName);
    Assert.IsTrue(TFile.Exists(TempFile), 'the set persisted immediately');

    FGrid.Controller.ClearFilters;
    Assert.AreEqual('1,2,3,4,5', VisibleIDs);
    Assert.AreEqual('', FGrid.Controller.ActiveFilterSetName);

    Assert.IsTrue(FGrid.Controller.ApplyFilterSet('ALPHA ROWS'),
      'lookup is case-insensitive');
    Assert.AreEqual('1,4', VisibleIDs, 'applying restores the filter');
    Assert.AreEqual('Alpha rows', FGrid.Controller.ActiveFilterSetName);
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.ListPersistenceRoundTrip;
var
  TempFile: string;
  List: TVittixFilterSetList;
  Set_: TVittixFilterSet;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    List := TVittixFilterSetList.Create;
    try
      Set_ := List.AddOrUpdate('Big');
      Set_.AddGroup.AddCondition('Amount', '>300');
      Set_.AddGroup.AddCondition('Name', '^A');
      List.SaveToFile(TempFile);

      List.Free;
      List := TVittixFilterSetList.Create;
      List.LoadFromFile(TempFile);

      Assert.AreEqual(1, List.Count);
      Assert.AreEqual('Big', List[0].Name);
      Assert.AreEqual(2, List[0].GroupCount);
      Assert.AreEqual('Amount', List[0][0][0].FieldName);
      Assert.AreEqual('>300', List[0][0][0].FilterText);
      Assert.AreEqual('^A', List[0][1][0].FilterText);
      Assert.IsTrue(TFile.ReadAllText(TempFile).Contains('"schemaVersion":1'),
        'the file carries the schema version');
    finally
      List.Free;
    end;
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.CorruptFileRaisesOnFirstUse;
var
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    TFile.WriteAllText(TempFile, 'not json at all {{{', TEncoding.UTF8);
    FGrid.Controller.FilterSetsFileName := TempFile;

    // First list access loads the file and must surface the corruption
    // instead of silently dropping the saved sets.
    Assert.WillRaise(
      procedure
      begin
        FGrid.Controller.ApplyFilterSet('Anything');
      end,
      EVittixFilterSetError);

    Assert.WillRaise(
      procedure
      begin
        FGrid.Controller.FilterSetNames;
      end,
      EVittixFilterSetError);
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.TwoGridsUseSeparateFiles;
var
  FileA, FileB: string;
  DataSetB: TClientDataSet;
  OwnerB: TForm;
  GridB: TVittixDBGrid;
begin
  FileA := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  FileB := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  DataSetB := CreateSampleDataSet;
  try
    FGrid.Controller.FilterSetsFileName := FileA;

    GridB := CreateHeadlessGrid(DataSetB, OwnerB);
    try
      GridB.Controller.FilterSetsFileName := FileB;

      FGrid.Controller.SaveCurrentFilterAs('OnlyOnA');
      Assert.AreEqual<NativeInt>(1, Length(FGrid.Controller.FilterSetNames));
      Assert.AreEqual<NativeInt>(0,
        Length(GridB.Controller.FilterSetNames),
        'the second grid must not see the first grid''s sets');

      // And per-instance settings survive: saving on B does not touch A.
      GridB.Controller.SaveCurrentFilterAs('OnlyOnB');
      Assert.AreEqual<NativeInt>(1, Length(FGrid.Controller.FilterSetNames));
      Assert.AreEqual<NativeInt>(1,
        Length(GridB.Controller.FilterSetNames));
    finally
      OwnerB.Free;
    end;
  finally
    DataSetB.Free;
    if TFile.Exists(FileA) then TFile.Delete(FileA);
    if TFile.Exists(FileB) then TFile.Delete(FileB);
  end;
end;

function TVittixFilterSetTests.CommaNames(List: TVittixFilterSetList): string;
var
  Names: TStringList;
  I: Integer;
begin
  Names := TStringList.Create;
  try
    for I := 0 to List.Count - 1 do
      Names.Add(List[I].Name);
    Result := Names.CommaText;
  finally
    Names.Free;
  end;
end;

procedure TVittixFilterSetTests.RenameDeleteAndReorder;
var
  List: TVittixFilterSetList;
begin
  List := TVittixFilterSetList.Create;
  try
    List.AddOrUpdate('A').AddGroup.AddCondition('ID', '>0');
    List.AddOrUpdate('B').AddGroup.AddCondition('ID', '>1');
    List.AddOrUpdate('C').AddGroup.AddCondition('ID', '>2');

    Assert.IsTrue(List.MoveUp('B'));
    Assert.AreEqual('B,A,C', CommaNames(List));
    Assert.IsFalse(List.MoveUp('B'), 'already at the top');
    Assert.IsTrue(List.MoveDown('B'));
    Assert.AreEqual('A,B,C', CommaNames(List));

    Assert.IsTrue(List.Rename('B', 'Beta'));
    Assert.IsNotNull(List.FindByName('Beta'));
    Assert.IsFalse(List.Rename('Beta', 'A'), 'duplicate names rejected');

    Assert.IsTrue(List.Delete('Beta'));
    Assert.AreEqual(2, List.Count);
    Assert.IsFalse(List.Delete('Beta'), 'unknown name returns False');
  finally
    List.Free;
  end;
end;

procedure TVittixFilterSetTests.EventsFireExactlyOnce;
var
  HandlerA, HandlerB: TVittixFilterSetAppliedEvent;
begin
  FGrid.OnFilterSetApplied := HandleFilterSetApplied;
  FGrid.OnFilterSetsChanged := HandleFilterSetsChanged;

  ApplyLegacyFilter('Name', '^A');
  FGrid.Controller.FilterEngine.Active := True;

  FGrid.Controller.SaveCurrentFilterAs('EVT');
  Assert.AreEqual(1, FChangedCount, 'OnFilterSetsChanged fires once per save');
  Assert.AreEqual(0, FAppliedCount, 'saving does not count as applying');

  FGrid.Controller.ClearFilters;
  FGrid.Controller.ApplyFilterSet('EVT');
  Assert.AreEqual(1, FAppliedCount, 'OnFilterSetApplied fires exactly once');
  Assert.AreEqual('EVT', FAppliedName);

  // Controller-side storage is shared with the grid: both surfaces expose
  // the same handler pointer.
  HandlerA := FGrid.Controller.OnFilterSetApplied;
  HandlerB := FGrid.OnFilterSetApplied;
  Assert.IsTrue((TMethod(HandlerA).Code = TMethod(HandlerB).Code) and
    (TMethod(HandlerA).Data = TMethod(HandlerB).Data),
    'both surfaces share one handler');

  FGrid.Controller.DeleteFilterSet('EVT');
  Assert.AreEqual(2, FChangedCount);
  Assert.AreEqual('', FGrid.Controller.ActiveFilterSetName,
    'deleting the active set clears its name');
end;

procedure TVittixFilterSetTests.ClearFiltersClearsActiveSetName;
begin
  ApplyLegacyFilter('Name', '^A');
  FGrid.Controller.FilterEngine.Active := True;
  FGrid.Controller.SaveCurrentFilterAs('Temp');
  Assert.AreNotEqual('', FGrid.Controller.ActiveFilterSetName);

  FGrid.Controller.ClearFilters;
  Assert.AreEqual('', FGrid.Controller.ActiveFilterSetName,
    'ClearFilters clears the active set name');
  Assert.IsFalse(FGrid.Controller.FilterEngine.FilterGroupsActive,
    'the engine groups are removed too');
end;

procedure TVittixFilterSetTests.ClosedDatasetApplySurvivesReopen;
var
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    FGrid.Controller.FilterSetsFileName := TempFile;
    ApplyLegacyFilter('Name', '^A');
    FGrid.Controller.FilterEngine.Active := True;
    FGrid.Controller.SaveCurrentFilterAs('Closed');
    FGrid.Controller.ClearFilters;

    FDataSet.Close;
    Assert.IsTrue(FGrid.Controller.ApplyFilterSet('Closed'),
      'applying with a closed dataset stores the name without raising');

    FDataSet.Open;
    // The engines are recreated on reopen; the active set re-binds.
    Assert.AreEqual('1,4', VisibleIDs,
      'the set filters again after the dataset reopens');
    Assert.AreEqual('Closed', FGrid.Controller.ActiveFilterSetName);
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.LayoutRestoresActiveFilterSet;
var
  Stream: TMemoryStream;
begin
  FGrid.Controller.FilterSetsFileName :=
    TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    ApplyLegacyFilter('Name', '^A');
    FGrid.Controller.FilterEngine.Active := True;
    FGrid.Controller.SaveCurrentFilterAs('LayoutSet');

    Stream := TMemoryStream.Create;
    try
      FGrid.Controller.SaveLayoutToStream(Stream);
      FGrid.Controller.ClearFilters;
      Assert.AreEqual('1,2,3,4,5', VisibleIDs);

      Stream.Position := 0;
      FGrid.Controller.LoadLayoutFromStream(Stream);
      Assert.AreEqual('1,4', VisibleIDs,
        'the layout re-applies the active filter set');
      Assert.AreEqual('LayoutSet', FGrid.Controller.ActiveFilterSetName);
    finally
      Stream.Free;
    end;
  finally
    if TFile.Exists(FGrid.Controller.FilterSetsFileName) then
      TFile.Delete(FGrid.Controller.FilterSetsFileName);
  end;
end;

procedure TVittixFilterSetTests.LayoutSkipsUnknownFilterSet;
var
  State: TVittixDBGridLayoutState;
begin
  // A layout saved elsewhere references a set this grid does not have.
  State := TVittixDBGridLayoutState.Create;
  try
    FGrid.Controller.CaptureLayout(State);
    State.ActiveFilterSetName := 'NotSavedHere';
    FGrid.Controller.ApplyLayout(State);
    Assert.AreEqual('', FGrid.Controller.ActiveFilterSetName,
      'an unknown set is skipped silently');
    Assert.AreEqual('1,2,3,4,5', VisibleIDs);
  finally
    State.Free;
  end;
end;

{ Wave 2 regressions }

procedure TVittixFilterSetTests.SaveCurrentFilterAsReplacesExistingGroups;
var
  TempFile: string;
  List: TVittixFilterSetList;
begin
  // M4 regression: re-saving over an existing name must REPLACE the set's
  // groups, not stack a second AND/OR group on top of the old one (which
  // would OR the old filters back in on apply).
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    FGrid.Controller.FilterSetsFileName := TempFile;

    ApplyLegacyFilter('Name', '^A');
    FGrid.Controller.FilterEngine.Active := True;
    FGrid.Controller.SaveCurrentFilterAs('Dup');

    FGrid.Controller.ClearFilters;
    ApplyLegacyFilter('ID', '<=3');
    FGrid.Controller.FilterEngine.Active := True;
    FGrid.Controller.SaveCurrentFilterAs('Dup');

    FGrid.Controller.ClearFilters;
    Assert.AreEqual('1,2,3,4,5', VisibleIDs, 'filters cleared');

    List := TVittixFilterSetList.Create;
    try
      List.LoadFromFile(TempFile);
      Assert.AreEqual(1, List.Count, 'one set with that name');
      Assert.AreEqual(1, List[0].GroupCount,
        'the replaced set has exactly one group - old groups are gone');
      Assert.AreEqual(1, List[0][0].ConditionCount);
      Assert.AreEqual('<=3', List[0][0][0].FilterText);
    finally
      List.Free;
    end;

    // Applying must reproduce ONLY the new filter (an old stacked group
    // would OR the Name-filtered rows back in).
    Assert.IsTrue(FGrid.Controller.ApplyFilterSet('Dup'));
    Assert.AreEqual('1,2,3', VisibleIDs,
      'the applied set reflects the replacement, not a stack');
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.DuplicateNamesFailLoadExactly;
var
  TempFile: string;
  List: TVittixFilterSetList;
begin
  // M5 regression: two sets with the same name must fail the load
  // EXPLICITLY - AddOrUpdate would silently clear the first entry and the
  // next autosave would persist that loss.
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    TFile.WriteAllText(TempFile,
      '{"schemaVersion":1,"filterSets":[' +
      '{"name":"Sales","groups":[{"conditions":[{"fieldName":"Amount","filterText":">300"}]}]},' +
      '{"name":"Sales","groups":[{"conditions":[{"fieldName":"Name","filterText":"^A"}]}]}]}',
      TEncoding.UTF8);

    List := TVittixFilterSetList.Create;
    try
      Assert.WillRaise(
        TTestLocalMethod(procedure begin List.LoadFromFile(TempFile); end),
        EVittixFilterSetError);
      Assert.AreEqual(0, List.Count, 'nothing was swapped in on failure');
    finally
      List.Free;
    end;
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.DuplicateNamesFailLoadCaseInsensitively;
var
  TempFile: string;
  List: TVittixFilterSetList;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    TFile.WriteAllText(TempFile,
      '{"schemaVersion":1,"filterSets":[' +
      '{"name":"Sales","groups":[]},' +
      '{"name":"SALES","groups":[]}]}',
      TEncoding.UTF8);

    List := TVittixFilterSetList.Create;
    try
      Assert.WillRaise(
        TTestLocalMethod(procedure begin List.LoadFromFile(TempFile); end),
        EVittixFilterSetError,
        'lookup is case-insensitive, so are duplicates');
    finally
      List.Free;
    end;
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.EmptyNameFailsLoad;
var
  TempFile: string;
  List: TVittixFilterSetList;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    TFile.WriteAllText(TempFile,
      '{"schemaVersion":1,"filterSets":[' +
      '{"name":"","groups":[]}]}',
      TEncoding.UTF8);

    List := TVittixFilterSetList.Create;
    try
      Assert.WillRaise(
        TTestLocalMethod(procedure begin List.LoadFromFile(TempFile); end),
        EVittixFilterSetError);
      Assert.AreEqual(0, List.Count);
    finally
      List.Free;
    end;
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.WrongTypedFilterSetsKeyRaisesFilterSetError;
var
  TempFile: string;
  List: TVittixFilterSetList;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    TFile.WriteAllText(TempFile,
      '{"schemaVersion":1,"filterSets":5}', TEncoding.UTF8);

    List := TVittixFilterSetList.Create;
    try
      Assert.WillRaise(
        TTestLocalMethod(procedure begin List.LoadFromFile(TempFile); end),
        EVittixFilterSetError,
        'a wrong-typed key raises the documented exception type');
    finally
      List.Free;
    end;
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.FailedLoadPreservesInMemoryState;
var
  TempFile: string;
  List: TVittixFilterSetList;
  Set_: TVittixFilterSet;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  try
    List := TVittixFilterSetList.Create;
    try
      Set_ := List.AddOrUpdate('Keeper');
      Set_.AddGroup.AddCondition('Amount', '>100');

      TFile.WriteAllText(TempFile,
        '{"schemaVersion":1,"filterSets":[' +
        '{"name":"Keeper","groups":[]},' +
        '{"name":"Broken' + '}},', TEncoding.UTF8); // truncated JSON

      Assert.WillRaise(
        TTestLocalMethod(procedure begin List.LoadFromFile(TempFile); end),
        EVittixFilterSetError);

      Assert.AreEqual(1, List.Count, 'in-memory state survives the failure');
      Assert.AreEqual('>100', List[0][0][0].FilterText);
    finally
      List.Free;
    end;
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.JSONRoundTripMatchesFileRoundTrip;
var
  List, Loaded: TVittixFilterSetList;
  Set_: TVittixFilterSet;
  JSONText: string;
begin
  List := TVittixFilterSetList.Create;
  try
    Set_ := List.AddOrUpdate('Big');
    Set_.AddGroup.AddCondition('Amount', '>300');
    Set_.AddGroup.AddCondition('Name', '^A');
    List.AddOrUpdate('Empty');

    JSONText := List.SaveToJSON;

    Loaded := TVittixFilterSetList.Create;
    try
      Loaded.LoadFromJSON(JSONText);
      Assert.AreEqual(CommaNames(Loaded), CommaNames(List),
        'same sets in the same order');
      Assert.AreEqual(2, Loaded[0].GroupCount);
      Assert.AreEqual('>300', Loaded[0][0][0].FilterText);
      Assert.AreEqual('^A', Loaded[0][1][0].FilterText);
      Assert.AreEqual(0, Loaded[1].GroupCount, 'empty sets round-trip');
    finally
      Loaded.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TVittixFilterSetTests.DialogEditsDoNotNotifyUntilClose;
var
  Cols: TStringList;
  List: TVittixFilterSetList;
  Set_: TVittixFilterSet;
  Dlg: TVittixDBGridFilterSetManager;
  ChangedCount: Integer;

  procedure OnChanged;
  begin
    Inc(ChangedCount);
  end;
begin
  List := TVittixFilterSetList.Create;
  Cols := TStringList.Create;
  try
    Set_ := List.AddOrUpdate('Work');
    Set_.AddGroup.AddCondition('Amount', '>300');

    Cols.Add('Name');
    Cols.Add('Amount');

    ChangedCount := 0;
    Dlg := TVittixDBGridFilterSetManager.CreateManager(nil, List, Cols,
      procedure begin Inc(ChangedCount); end, nil);
    try
      Dlg.SetsListBox.ItemIndex := 0;
      Dlg.BtnAddGroupClick(nil);
      Dlg.ValueEdit.Text := '99';
      Dlg.BtnAddConditionClick(nil);

      Assert.AreEqual(0, ChangedCount,
        'editor-pane edits do not fire persistence notifications');
      Assert.AreEqual(2, List[0].GroupCount, 'in-memory edit is visible');
      Assert.AreEqual(1, List[0][1].ConditionCount, 'condition added');

      // Select the added condition (flat index 0 = group 1's condition)
      // and edit it: still no persistence notification.
      Dlg.ConditionsListBox.ItemIndex := 0;
      Dlg.ValueEdit.Text := '555';
      Dlg.BtnUpdateConditionClick(nil);
      Assert.AreEqual(0, ChangedCount, 'update also defers persistence');
      Assert.IsTrue(List[0][0][0].FilterText.EndsWith('555'),
        'the updated condition text landed in the set');

      Dlg.ConditionsListBox.ItemIndex := 0;
      Dlg.BtnDeleteConditionClick(nil);
      Assert.AreEqual(0, ChangedCount, 'delete also defers persistence');
      Assert.AreEqual(0, List[0][0].ConditionCount, 'condition removed');
    finally
      Dlg.Free;
    end;
  finally
    Cols.Free;
    List.Free;
  end;
end;

procedure TVittixFilterSetTests.DialogCancelRevertsEdits;
var
  Cols: TStringList;
  List: TVittixFilterSetList;
  Set_: TVittixFilterSet;
  Dlg: TVittixDBGridFilterSetManager;
  Driver: TFilterSetDialogDriver;
  Timer: TTimer;
  AppliedName: string;
  ChangedCount: Integer;
  Applied: Boolean;
begin
  List := TVittixFilterSetList.Create;
  Cols := TStringList.Create;
  try
    Set_ := List.AddOrUpdate('Work');
    Set_.AddGroup.AddCondition('Amount', '>300');

    Cols.Add('Name');
    Cols.Add('Amount');
    ChangedCount := 0;

    Driver := TFilterSetDialogDriver.Create;
    try
      Driver.FinishResult := mrCancel;
      Driver.EditProc := procedure
      begin
        Dlg.SetsListBox.ItemIndex := 0;
        Dlg.BtnAddGroupClick(nil);
        Dlg.ValueEdit.Text := '99';
        Dlg.BtnAddConditionClick(nil);
      end;

      Dlg := TVittixDBGridFilterSetManager.CreateManager(nil, List, Cols,
        procedure begin Inc(ChangedCount); end, nil);
      Driver.Dlg := Dlg;
      try
        Timer := TTimer.Create(nil);
        try
          Timer.Interval := 1;
          Timer.OnTimer := Driver.OnTimerTick;
          Timer.Enabled := True;
          Applied := Dlg.Execute(AppliedName);
        finally
          Timer.Enabled := False;
          Timer.Free;
        end;

        Assert.IsFalse(Applied, 'cancel does not apply');
        Assert.AreEqual('', Driver.ErrorText, 'no exception inside the dialog session');
        Assert.AreEqual('', AppliedName);
        Assert.AreEqual(0, ChangedCount,
          'cancel fires no persistence notification');
        Assert.AreEqual(1, List[0].GroupCount,
          'the snapshot restored the pre-dialog state');
        Assert.AreEqual(1, List[0][0].ConditionCount);
        Assert.AreEqual('>300', List[0][0][0].FilterText);
      finally
        Dlg.Free;
      end;
    finally
      Driver.Free;
    end;
  finally
    Cols.Free;
    List.Free;
  end;
end;

procedure TVittixFilterSetTests.DialogSaveCurrentAsCancelKeepsOnlyTheSavedSet;
var
  List, Reloaded: TVittixFilterSetList;
  Dlg: TVittixDBGridFilterSetManager;
  Driver: TFilterSetDialogDriver;
  Timer: TTimer;
  TempFile, AppliedName: string;
  Applied: Boolean;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  FGrid.Controller.FilterSetsFileName := TempFile;
  List := FGrid.Controller.FilterSetList;
  List.AddOrUpdate('Work').AddGroup.AddCondition('Amount', '>300');
  FGrid.Controller.PersistFilterSets;

  Driver := TFilterSetDialogDriver.Create;
  try
    Driver.FinishResult := mrCancel;
    Driver.SaveCurrentAsName := 'Keep';
    Driver.EditProc := procedure
    begin
      Dlg.SetsListBox.ItemIndex := 0;
      Dlg.BtnAddGroupClick(nil);
      Dlg.BtnSaveCurrentClick(nil);
    end;

    Dlg := TVittixDBGridFilterSetManager.CreateManager(nil, List, nil,
      procedure begin List.SaveToFile(TempFile); end,
      procedure(AName: string) begin FGrid.Controller.SaveCurrentFilterAs(AName); end);
    Driver.Dlg := Dlg;
    try
      Timer := TTimer.Create(nil);
      try
        Timer.Interval := 1;
        Timer.OnTimer := Driver.OnTimerTick;
        Timer.Enabled := True;
        Applied := Dlg.Execute(AppliedName);
      finally
        Timer.Enabled := False;
        Timer.Free;
      end;

      Assert.IsFalse(Applied, 'Cancel does not apply the edited set');
      Assert.AreEqual('', AppliedName);
      Assert.AreEqual('', Driver.ErrorText, 'dialog driver completed without an exception');
      Assert.IsTrue(Driver.SaveCurrentAsPromptHandled,
        'the nested Save Current As prompt was accepted');
      Assert.AreEqual(1, List.FindByName('Work').GroupCount,
        'the earlier edit to Work is reverted in memory');
      Assert.AreEqual(1, List.FindByName('Work')[0].ConditionCount);
      Assert.AreEqual('>300', List.FindByName('Work')[0][0].FilterText);
      Assert.IsNotNull(List.FindByName('Keep'),
        'the explicitly saved current filter set survives Cancel');

      Reloaded := TVittixFilterSetList.Create;
      try
        Reloaded.LoadFromFile(TempFile);
        Assert.AreEqual(1, Reloaded.FindByName('Work').GroupCount,
          'unrelated manager edits were never persisted');
        Assert.IsNotNull(Reloaded.FindByName('Keep'),
          'the saved set is durable after Cancel');
      finally
        Reloaded.Free;
      end;
    finally
      Dlg.Free;
    end;
  finally
    Driver.Free;
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

procedure TVittixFilterSetTests.DialogApplyPersistsOnceAndRoundTrips;
var
  Cols: TStringList;
  List, Reloaded: TVittixFilterSetList;
  Set_: TVittixFilterSet;
  Dlg: TVittixDBGridFilterSetManager;
  Driver: TFilterSetDialogDriver;
  Timer: TTimer;
  TempFile: string;
  AppliedName: string;
  ChangedCount: Integer;
  Applied: Boolean;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TGuid.NewGuid.ToString + '.json');
  List := TVittixFilterSetList.Create;
  Cols := TStringList.Create;
  try
    Set_ := List.AddOrUpdate('Work');
    Set_.AddGroup.AddCondition('Amount', '>300');
    List.FileName := TempFile;

    Cols.Add('Name');
    Cols.Add('Amount');
    ChangedCount := 0;

    Driver := TFilterSetDialogDriver.Create;
    try
      Driver.FinishResult := mrOk;
      Driver.EditProc := procedure
      begin
        Dlg.SetsListBox.ItemIndex := 0;
        Dlg.BtnAddGroupClick(nil);
        Dlg.ValueEdit.Text := '99';
        Dlg.BtnAddConditionClick(nil);
      end;

      Dlg := TVittixDBGridFilterSetManager.CreateManager(nil, List, Cols,
        procedure
        begin
          Inc(ChangedCount);
          List.SaveToFile(TempFile); // same wiring as the demo
        end, nil);
      Driver.Dlg := Dlg;
      try
        Timer := TTimer.Create(nil);
        try
          Timer.Interval := 1;
          Timer.OnTimer := Driver.OnTimerTick;
          Timer.Enabled := True;
          Applied := Dlg.Execute(AppliedName);
        finally
          Timer.Enabled := False;
          Timer.Free;
        end;

        Assert.AreEqual('', Driver.ErrorText, 'no exception inside the dialog session');
        Assert.IsTrue(Applied, 'apply closes successfully');
        Assert.AreEqual('Work', AppliedName);
        Assert.AreEqual(1, ChangedCount,
          'exactly ONE persistence notification per session');
        Assert.AreEqual(2, List[0].GroupCount, 'edits kept after apply');

        Reloaded := TVittixFilterSetList.Create;
        try
          Reloaded.LoadFromFile(TempFile);
          Assert.AreEqual(2, Reloaded[0].GroupCount,
            'the persisted file carries the applied state');
          Assert.AreEqual('99', Reloaded[0][1][0].FilterText);
        finally
          Reloaded.Free;
        end;
      finally
        Dlg.Free;
      end;
    finally
      Driver.Free;
    end;
  finally
    Cols.Free;
    List.Free;
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
  end;
end;

end.
