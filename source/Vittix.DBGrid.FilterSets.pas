unit Vittix.DBGrid.FilterSets;

{$REGION 'Documentation'}
/// <summary>
/// Saved filter sets: named, persisted groups of column filters. A set is
/// an ordered list of groups; conditions inside a group are ANDed, groups
/// are ORed. Conditions reuse the engine's operator-prefix syntax and the
/// single operator table (see Vittix.DBGrid.Filter.Engine).
///
/// The list persists as JSON with a schemaVersion field, written atomically
/// (staged temp file next to the target, then replaced). A corrupt file
/// raises EVittixFilterSetError instead of being silently dropped.
/// </summary>
{$ENDREGION}

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  Vittix.DBGrid.Filter.Engine;

const
  /// <summary>Layout/JSON schema version written by this unit. Files
  /// without the field are treated as version 1 (identical shape).</summary>
  VITTIX_FILTERSET_SCHEMA_VERSION = 1;

type
  /// <summary>Raised when a filter-set file cannot be parsed or a requested
  /// set does not exist. Never silently drops data.</summary>
  EVittixFilterSetError = class(Exception);

  TVittixFilterSet = class;

  /// <summary>One AND-group inside a set: an ordered list of conditions
  /// (FieldName + operator-prefixed FilterText).</summary>
  TVittixFilterSetGroup = class
  private
    FConditions: TList<TVittixFilterCondition>;
    function GetCondition(Index: Integer): TVittixFilterCondition;
    procedure SetCondition(Index: Integer; const Value: TVittixFilterCondition);
    function GetConditionCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function AddCondition(const AFieldName, AFilterText: string): Integer;
    procedure DeleteCondition(Index: Integer);
    procedure Clear;

    property Conditions[Index: Integer]: TVittixFilterCondition
      read GetCondition write SetCondition; default;
    property ConditionCount: Integer read GetConditionCount;
  end;

  /// <summary>A named filter set: an ordered list of AND-groups that the
  /// engine ORs together.</summary>
  TVittixFilterSet = class
  private
    FName: string;
    FGroups: TObjectList<TVittixFilterSetGroup>;
    function GetGroup(Index: Integer): TVittixFilterSetGroup;
    function GetGroupCount: Integer;
  public
    constructor Create(const AName: string = '');
    destructor Destroy; override;

    function AddGroup: TVittixFilterSetGroup;
    procedure DeleteGroup(Index: Integer);
    procedure Clear;

    property Name: string read FName write FName;
    property Groups[Index: Integer]: TVittixFilterSetGroup read GetGroup; default;
    property GroupCount: Integer read GetGroupCount;
  end;

  /// <summary>An ordered collection of named filter sets with JSON file
  /// persistence (atomic replace, missing directories created).</summary>
  TVittixFilterSetList = class
  private
    FSets: TObjectList<TVittixFilterSet>;
    FFileName: string;
    function GetSet(Index: Integer): TVittixFilterSet;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Adds (or replaces, same name case-insensitive) a set.
    /// Returns the stored instance.</summary>
    function AddOrUpdate(const AName: string): TVittixFilterSet;
    /// <summary>Deletes the named set; False when no set carries it.</summary>
    function Delete(const AName: string): Boolean;
    /// <summary>Renames AName to ANewName; False when AName is unknown or
    /// ANewName already exists.</summary>
    function Rename(const AName, ANewName: string): Boolean;
    /// <summary>Finds a set by (case-insensitive) name; nil when missing.</summary>
    function FindByName(const AName: string): TVittixFilterSet;
    /// <summary>Swaps a set with its predecessor/successor; False at the
    /// boundary.</summary>
    function MoveUp(const AName: string): Boolean;
    function MoveDown(const AName: string): Boolean;
    /// <summary>Case-insensitive sorted name list (for UI and lookups).</summary>
    procedure GetNames(ANames: TStrings);

    /// <summary>Loads from JSON; raises EVittixFilterSetError on malformed
    /// input. Replaces the in-memory list only on success.</summary>
    procedure LoadFromFile(const AFileName: string);
    /// <summary>Writes the list atomically (staged temp file, then
    /// replace), creating missing directories first.</summary>
    procedure SaveToFile(const AFileName: string);

    /// <summary>The canonical serializer text (same JSON SaveToFile
    /// writes). Raises EVittixFilterSetError on malformed input; the
    /// in-memory list is only replaced after the whole text parsed
    /// cleanly. Used for snapshots (dialog cancel/restore) and tests.</summary>
    function SaveToJSON: string;
    procedure LoadFromJSON(const AJSON: string);

    /// <summary>File name the list was last loaded from / saved to; used
    /// by the controller as the default persistence target.</summary>
    property FileName: string read FFileName write FFileName;
    property Sets[Index: Integer]: TVittixFilterSet read GetSet; default;
    property Count: Integer read GetCount;
  private
    /// <summary>Parses AJSONText and swaps the parsed sets into this list
    /// (all-or-nothing). ASourceName appears in error messages. Shared by
    /// LoadFromFile and LoadFromJSON.</summary>
    procedure ReadFromJSON(const AJSONText, ASourceName: string);
  end;

implementation

{ TVittixFilterSetGroup }

constructor TVittixFilterSetGroup.Create;
begin
  inherited Create;
  FConditions := TList<TVittixFilterCondition>.Create;
end;

destructor TVittixFilterSetGroup.Destroy;
begin
  FConditions.Free;
  inherited;
end;

function TVittixFilterSetGroup.GetCondition(
  Index: Integer): TVittixFilterCondition;
begin
  Result := FConditions.Items[Index];
end;

procedure TVittixFilterSetGroup.SetCondition(Index: Integer;
  const Value: TVittixFilterCondition);
begin
  FConditions.Items[Index] := Value;
end;

function TVittixFilterSetGroup.GetConditionCount: Integer;
begin
  Result := FConditions.Count;
end;

function TVittixFilterSetGroup.AddCondition(
  const AFieldName, AFilterText: string): Integer;
var
  Cond: TVittixFilterCondition;
begin
  Cond.FieldName := AFieldName;
  Cond.FilterText := AFilterText;
  FConditions.Add(Cond);
  Result := FConditions.Count - 1;
end;

procedure TVittixFilterSetGroup.DeleteCondition(Index: Integer);
begin
  FConditions.Delete(Index);
end;

procedure TVittixFilterSetGroup.Clear;
begin
  FConditions.Clear;
end;

{ TVittixFilterSet }

constructor TVittixFilterSet.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FGroups := TObjectList<TVittixFilterSetGroup>.Create(True);
end;

destructor TVittixFilterSet.Destroy;
begin
  FGroups.Free;
  inherited;
end;

function TVittixFilterSet.GetGroup(Index: Integer): TVittixFilterSetGroup;
begin
  Result := FGroups.Items[Index];
end;

function TVittixFilterSet.GetGroupCount: Integer;
begin
  Result := FGroups.Count;
end;

function TVittixFilterSet.AddGroup: TVittixFilterSetGroup;
begin
  Result := TVittixFilterSetGroup.Create;
  FGroups.Add(Result);
end;

procedure TVittixFilterSet.DeleteGroup(Index: Integer);
begin
  FGroups.Delete(Index);
end;

procedure TVittixFilterSet.Clear;
begin
  FGroups.Clear;
end;

{ TVittixFilterSetList }

constructor TVittixFilterSetList.Create;
begin
  inherited Create;
  FSets := TObjectList<TVittixFilterSet>.Create(True);
end;

destructor TVittixFilterSetList.Destroy;
begin
  FSets.Free;
  inherited;
end;

function TVittixFilterSetList.GetSet(Index: Integer): TVittixFilterSet;
begin
  Result := FSets.Items[Index];
end;

function TVittixFilterSetList.GetCount: Integer;
begin
  Result := FSets.Count;
end;

function TVittixFilterSetList.AddOrUpdate(const AName: string): TVittixFilterSet;
var
  Existing: TVittixFilterSet;
begin
  if AName = '' then
    raise EVittixFilterSetError.Create('A filter set needs a name');

  Existing := FindByName(AName);
  if Assigned(Existing) then
  begin
    Existing.Clear; // reuse the instance so held references stay valid
    Exit(Existing);
  end;

  Result := TVittixFilterSet.Create(AName);
  FSets.Add(Result);
end;

function TVittixFilterSetList.Delete(const AName: string): Boolean;
var
  Existing: TVittixFilterSet;
begin
  Existing := FindByName(AName);
  Result := Assigned(Existing);
  if Result then
    FSets.Remove(Existing);
end;

function TVittixFilterSetList.Rename(const AName, ANewName: string): Boolean;
var
  Existing: TVittixFilterSet;
begin
  if (ANewName = '') or (SameText(AName, ANewName) = False) and
     (FindByName(ANewName) <> nil) then
    Exit(False);

  Existing := FindByName(AName);
  Result := Assigned(Existing);
  if Result then
    Existing.Name := ANewName;
end;

function TVittixFilterSetList.FindByName(const AName: string): TVittixFilterSet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FSets.Count - 1 do
    if SameText(FSets.Items[I].Name, AName) then
      Exit(FSets.Items[I]);
end;

function TVittixFilterSetList.MoveUp(const AName: string): Boolean;
var
  Existing: TVittixFilterSet;
  Index: Integer;
begin
  Existing := FindByName(AName);
  Result := Assigned(Existing);
  if not Result then
    Exit;
  Index := FSets.IndexOf(Existing);
  if Index <= 0 then
    Exit(False);
  FSets.Exchange(Index, Index - 1);
end;

function TVittixFilterSetList.MoveDown(const AName: string): Boolean;
var
  Existing: TVittixFilterSet;
  Index: Integer;
begin
  Existing := FindByName(AName);
  Result := Assigned(Existing);
  if not Result then
    Exit;
  Index := FSets.IndexOf(Existing);
  if Index >= FSets.Count - 1 then
    Exit(False);
  FSets.Exchange(Index, Index + 1);
end;

procedure TVittixFilterSetList.GetNames(ANames: TStrings);
var
  I: Integer;
begin
  ANames.BeginUpdate;
  try
    ANames.Clear;
    for I := 0 to FSets.Count - 1 do
      ANames.Add(FSets.Items[I].Name);
  finally
    ANames.EndUpdate;
  end;
end;

procedure TVittixFilterSetList.LoadFromFile(const AFileName: string);
var
  JsonText: string;
begin
  if AFileName = '' then
    raise EVittixFilterSetError.Create('Filter set file name cannot be empty');
  if not TFile.Exists(AFileName) then
    Exit; // no file yet: an empty list is the correct initial state

  JsonText := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  if Trim(JsonText) = '' then
    raise EVittixFilterSetError.CreateFmt(
      'Filter set file "%s" is empty', [AFileName]);
  ReadFromJSON(JsonText, AFileName);
  // The successful load makes this the persistence target.
  FFileName := AFileName;
end;

procedure TVittixFilterSetList.ReadFromJSON(
  const AJSONText, ASourceName: string);
var
  Parsed: TJSONValue;
  ParsedFilterSets: TJSONValue;
  Root: TJSONObject;
  SetsArray, GroupsArray, ConditionsArray: TJSONArray;
  SetObj, GroupObj, CondObj: TJSONObject;
  SetName: string;
  I, J, K: Integer;
  NewList: TVittixFilterSetList;
  NewSet: TVittixFilterSet;
  NewGroup: TVittixFilterSetGroup;

  function ReadString(Obj: TJSONObject; const Key, Default: string): string;
  var
    Value: TJSONValue;
  begin
    Value := Obj.FindValue(Key);
    if Value is TJSONString then
      Result := Value.Value
    else
      Result := Default;
  end;

begin
  NewList := TVittixFilterSetList.Create;
  try
    Parsed := TJSONObject.ParseJSONValue(AJSONText);
    if Parsed = nil then
      raise EVittixFilterSetError.CreateFmt(
        'Filter set file "%s" does not contain valid JSON', [ASourceName]);
    try
      if not (Parsed is TJSONObject) then
        raise EVittixFilterSetError.CreateFmt(
          'Filter set file "%s" must contain a JSON object', [ASourceName]);
      Root := TJSONObject(Parsed);

      // Future schema versions: unknown fields are ignored on load.
      // A WRONG-TYPED filterSets key must raise the documented exception
      // (GetValue's default only applies to a missing key; a present
      // non-array value would otherwise escape as a raw RTL JSON error).
      ParsedFilterSets := Root.FindValue('filterSets');
      if Assigned(ParsedFilterSets) and not (ParsedFilterSets is TJSONArray) then
        raise EVittixFilterSetError.CreateFmt(
          'Filter set file "%s": "filterSets" must be an array',
          [ASourceName]);
      SetsArray := TJSONArray(ParsedFilterSets);
      if Assigned(SetsArray) then
        for I := 0 to SetsArray.Count - 1 do
        begin
          if not (SetsArray.Items[I] is TJSONObject) then
            raise EVittixFilterSetError.CreateFmt(
              'Filter set file "%s": filterSets[%d] is not an object',
              [ASourceName, I]);
          SetObj := TJSONObject(SetsArray.Items[I]);
          SetName := Trim(ReadString(SetObj, 'name', ''));
          // Duplicate names (case-insensitive, matching FindByName) must
          // fail the load EXPLICITLY: AddOrUpdate would silently clear the
          // first entry, and the next autosave would persist that loss.
          if NewList.FindByName(SetName) <> nil then
            raise EVittixFilterSetError.CreateFmt(
              'Filter set file "%s": duplicate filter set name "%s"',
              [ASourceName, SetName]);
          NewSet := NewList.AddOrUpdate(SetName);
          GroupsArray := SetObj.GetValue<TJSONArray>('groups', nil);
          if Assigned(GroupsArray) then
            for J := 0 to GroupsArray.Count - 1 do
            begin
              if not (GroupsArray.Items[J] is TJSONObject) then
                raise EVittixFilterSetError.CreateFmt(
                  'Filter set file "%s": groups[%d] is not an object',
                  [ASourceName, J]);
              GroupObj := TJSONObject(GroupsArray.Items[J]);
              NewGroup := NewSet.AddGroup;
              ConditionsArray := GroupObj.GetValue<TJSONArray>('conditions', nil);
              if Assigned(ConditionsArray) then
                for K := 0 to ConditionsArray.Count - 1 do
                begin
                  if not (ConditionsArray.Items[K] is TJSONObject) then
                    raise EVittixFilterSetError.CreateFmt(
                      'Filter set file "%s": conditions[%d] is not an object',
                      [ASourceName, K]);
                  CondObj := TJSONObject(ConditionsArray.Items[K]);
                  NewGroup.AddCondition(
                    ReadString(CondObj, 'fieldName', ''),
                    ReadString(CondObj, 'filterText', ''));
                end;
            end;
        end;
    finally
      Parsed.Free;
    end;

    // Only swap the in-memory state after the whole text parsed cleanly,
    // so malformed input never truncates loaded data. Extract moves each
    // set without freeing it (both lists own their objects).
    FSets.Clear;
    while NewList.FSets.Count > 0 do
      FSets.Add(NewList.FSets.Extract(NewList.FSets.Items[0]));
  finally
    NewList.Free;
  end;
end;

procedure TVittixFilterSetList.LoadFromJSON(const AJSON: string);
begin
  ReadFromJSON(AJSON, '<filter sets JSON>');
end;

function TVittixFilterSetList.SaveToJSON: string;
var
  Root: TJSONObject;
  SetsArray, GroupsArray, ConditionsArray: TJSONArray;
  SetObj, GroupObj, CondObj: TJSONObject;
  I, J, K: Integer;

  procedure WriteString(Obj: TJSONObject; const Key, Value: string);
  begin
    Obj.AddPair(Key, Value);
  end;

begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('schemaVersion',
      TJSONNumber.Create(VITTIX_FILTERSET_SCHEMA_VERSION));

    SetsArray := TJSONArray.Create;
    for I := 0 to FSets.Count - 1 do
    begin
      SetObj := TJSONObject.Create;
      WriteString(SetObj, 'name', FSets.Items[I].Name);
      GroupsArray := TJSONArray.Create;
      for J := 0 to FSets.Items[I].GroupCount - 1 do
      begin
        GroupObj := TJSONObject.Create;
        ConditionsArray := TJSONArray.Create;
        for K := 0 to FSets.Items[I][J].ConditionCount - 1 do
        begin
          CondObj := TJSONObject.Create;
          WriteString(CondObj, 'fieldName', FSets.Items[I][J][K].FieldName);
          WriteString(CondObj, 'filterText', FSets.Items[I][J][K].FilterText);
          ConditionsArray.AddElement(CondObj);
        end;
        GroupObj.AddPair('conditions', ConditionsArray);
        GroupsArray.AddElement(GroupObj);
      end;
      SetObj.AddPair('groups', GroupsArray);
      SetsArray.AddElement(SetObj);
    end;
    Root.AddPair('filterSets', SetsArray);

    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

procedure TVittixFilterSetList.SaveToFile(const AFileName: string);
var
  Bytes: TBytes;
  TargetFile, TempFile, BackupFile: string;
  Stream: TFileStream;
begin
  if AFileName = '' then
    raise EVittixFilterSetError.Create('Filter set file name cannot be empty');
  TargetFile := AFileName;

  // Atomic save: stage next to the target (same volume) so the final
  // swap is a rename, and a crash mid-write cannot corrupt the file.
  if TPath.GetDirectoryName(TargetFile) <> '' then
    ForceDirectories(TPath.GetDirectoryName(TargetFile));
  TempFile := TPath.Combine(TPath.GetDirectoryName(TargetFile),
    '~vittix-' + TPath.GetGUIDFileName(False) + '.tmp');
  BackupFile := TempFile + '.bak';

  Bytes := TEncoding.UTF8.GetBytes(SaveToJSON);
  try
    Stream := TFileStream.Create(TempFile, fmCreate);
    try
      if Length(Bytes) > 0 then
        Stream.WriteBuffer(Bytes[0], Length(Bytes));
    finally
      Stream.Free;
    end;

    if TFile.Exists(TargetFile) then
    begin
      TFile.Replace(TempFile, TargetFile, BackupFile);
      if TFile.Exists(BackupFile) then
        try TFile.Delete(BackupFile) except end;
    end
    else
      TFile.Move(TempFile, TargetFile);
    FFileName := TargetFile;
  except
    // Never leave the staged temp file behind on failure.
    if TFile.Exists(TempFile) then
      try TFile.Delete(TempFile) except end;
    if TFile.Exists(BackupFile) then
      try TFile.Delete(BackupFile) except end;
    raise;
  end;
end;

end.
