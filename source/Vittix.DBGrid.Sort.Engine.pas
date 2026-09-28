unit Vittix.DBGrid.Sort.Engine;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.StrUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  Data.DB,
  Vcl.DBGrids,
  Datasnap.DBClient,
  Vittix.DBGrid.ColumnInfo;

const
  TEMP_CLIENT_DATASET_INDEX = '__VITTIX_SORT__';

type
  // Raised when sorting cannot be applied to the bound dataset (no
  // IndexFieldNames property) instead of silently doing nothing.
  EVittixSortError = class(Exception);

  TFieldValidationEvent = procedure(const FieldName: string; Found: Boolean) of object;

  /// <summary>
  /// Logic-only dataset sorting engine.
  /// Uses IndexFieldNames property (FireDAC / ClientDataSet style).
  /// Supports single and multi-column sorting.
  /// </summary>
  TVittixDBGridSortEngine = class
  private
    FDataSet: TDataSet;
    FColumns: TVittixDBGridColumns;
    FOriginalIndexName: string;
    FOriginalIndexFieldNames: string;
    FOriginalIndexCaptured: Boolean;
    FTempIndexCreated: Boolean;
    FOnFieldValidation: TFieldValidationEvent;

    function DataSetSupportsIndexFieldNames: Boolean;
    function BuildIndexFieldNames: string;
    procedure CaptureOriginalIndexState;
    procedure RestoreOriginalIndexState;
    procedure ApplyClientDataSetSorting;
    procedure NormalizeSortIndices;

  public
    constructor Create(ADataSet: TDataSet; AColumns: TVittixDBGridColumns);

    procedure ApplySorting;
    procedure ClearSorting;
    procedure ToggleSort(AColumn: TColumn; MultiColumn: Boolean = False);
    function GetSortSummaryText: string;

    property OnFieldValidation: TFieldValidationEvent read FOnFieldValidation write FOnFieldValidation;
  end;

implementation

type
  // IndexName is protected on TCustomClientDataSet (public only on
  // TClientDataSet). Cracking lets the engine switch indexes directly on
  // any ClientDataSet descendant (TClientDataSet, TFDMemTable, ...)
  // instead of going through RTTI.
  TVittixCDSAccess = class(TCustomClientDataSet);

{ TVittixDBGridSortEngine }

constructor TVittixDBGridSortEngine.Create(
  ADataSet: TDataSet;
  AColumns: TVittixDBGridColumns);
begin
  inherited Create;
  FDataSet := ADataSet;
  FColumns := AColumns;
  FOriginalIndexCaptured := False;
  FTempIndexCreated := False;
end;

function TVittixDBGridSortEngine.DataSetSupportsIndexFieldNames: Boolean;
begin
  // Check if the dataset has the 'IndexFieldNames' string property.
  // This supports TClientDataSet, TFDMemTable, TFDQuery, etc.
  Result :=
    Assigned(FDataSet) and
    (GetPropInfo(FDataSet.ClassInfo, 'IndexFieldNames') <> nil);
end;

procedure TVittixDBGridSortEngine.NormalizeSortIndices;
var
  Sorted: TList<TVittixDBGridColumnInfo>;
  I: Integer;
begin
  Sorted := TList<TVittixDBGridColumnInfo>.Create;
  try
    // Collect all currently sorted columns
    for I := 0 to FColumns.Count - 1 do
      if FColumns[I].SortOrder <> vsoNone then
        Sorted.Add(FColumns[I]);

    // Sort them by their current SortIndex to maintain relative order
    Sorted.Sort(
      TComparer<TVittixDBGridColumnInfo>.Construct(
        function(const L, R: TVittixDBGridColumnInfo): Integer
        begin
          Result := L.SortIndex - R.SortIndex;
        end
      )
    );

    // Re-assign indices sequentially (0, 1, 2...)
    // This removes gaps if a column was removed from sorting
    for I := 0 to Sorted.Count - 1 do
      Sorted[I].SortIndex := I;
  finally
    Sorted.Free;
  end;
end;

function TVittixDBGridSortEngine.BuildIndexFieldNames: string;
var
  Sorted: TList<TVittixDBGridColumnInfo>;
  Parts: TStringList;
  Info: TVittixDBGridColumnInfo;
  I: Integer;
begin
  Result := '';

  Sorted := TList<TVittixDBGridColumnInfo>.Create;
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ';'; // FireDAC/Midas standard separator
    Parts.StrictDelimiter := True; // Avoid quoting unless necessary

    for I := 0 to FColumns.Count - 1 do
      if FColumns[I].SortOrder <> vsoNone then
        Sorted.Add(FColumns[I]);

    if Sorted.Count = 0 then Exit;

    // Sort by priority
    Sorted.Sort(
      TComparer<TVittixDBGridColumnInfo>.Construct(
        function(const L, R: TVittixDBGridColumnInfo): Integer
        begin
          Result := L.SortIndex - R.SortIndex;
        end
      )
    );

    for Info in Sorted do
    begin
      // Validation: Skip if field doesn't exist in dataset
      if FDataSet.FindField(Info.FieldName) = nil then
      begin
        if Assigned(FOnFieldValidation) then
          FOnFieldValidation(Info.FieldName, False);
        Continue;
      end;

      case Info.SortOrder of
        vsoAsc:
          Parts.Add(Info.FieldName);
        vsoDesc:
          // ":D" is the standard suffix for FireDAC and some Midas versions.
          // If using ADO, this might need to be changed to " DESC" via a property.
          Parts.Add(Info.FieldName + ':D'); 
      end;
    end;

    Result := Parts.DelimitedText;
  finally
    Parts.Free;
    Sorted.Free;
  end;
end;

procedure TVittixDBGridSortEngine.ApplySorting;
var
  IndexFields: string;
  I: Integer;
begin
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;

  CaptureOriginalIndexState;

  try
    if FDataSet is TCustomClientDataSet then
    begin
      ApplyClientDataSetSorting;
      Exit;
    end;

    if not DataSetSupportsIndexFieldNames then
    begin
      // A request to actually sort must fail loudly: silently doing nothing
      // made the grid appear sorted while the data stayed unsorted. Clearing
      // (no sort configured) stays a harmless no-op here.
      for I := 0 to FColumns.Count - 1 do
        if FColumns[I].SortOrder <> vsoNone then
          raise EVittixSortError.CreateFmt(
            '%s does not support index-based sorting (no IndexFieldNames ' +
            'property). Use a dataset that publishes IndexFieldNames, such ' +
            'as TClientDataSet or a FireDAC dataset.', [FDataSet.ClassName]);
      Exit;
    end;

    IndexFields := BuildIndexFieldNames;

    FDataSet.DisableControls;
    try
      // Use RTTI to set the property safely
      SetPropValue(FDataSet, 'IndexFieldNames', IndexFields);
    finally
      FDataSet.EnableControls;
    end;
  except
    RestoreOriginalIndexState;
    raise;
  end;
end;

procedure TVittixDBGridSortEngine.CaptureOriginalIndexState;
begin
  if FOriginalIndexCaptured then
    Exit;

  if Assigned(FDataSet) then
  begin
    if FDataSet is TCustomClientDataSet then
      FOriginalIndexName := TVittixCDSAccess(FDataSet).IndexName;

    if DataSetSupportsIndexFieldNames then
      FOriginalIndexFieldNames := GetPropValue(FDataSet, 'IndexFieldNames', True);
  end;

  FOriginalIndexCaptured := True;
end;

procedure TVittixDBGridSortEngine.RestoreOriginalIndexState;
begin
  if not Assigned(FDataSet) then
    Exit;

  if FDataSet is TCustomClientDataSet then
  begin
    TCustomClientDataSet(FDataSet).DisableControls;
    try
      TVittixCDSAccess(FDataSet).IndexName := FOriginalIndexName;
      TCustomClientDataSet(FDataSet).IndexFieldNames := FOriginalIndexFieldNames;
      if FTempIndexCreated then
      begin
        try
          TCustomClientDataSet(FDataSet).DeleteIndex(TEMP_CLIENT_DATASET_INDEX);
        except
          // Expected when the temp index was never created; anything else
          // (e.g. index busy) still must not break restore.
          on E: EDatabaseError do ;
        end;
      end;
    finally
      TCustomClientDataSet(FDataSet).EnableControls;
    end;
  end
  else if DataSetSupportsIndexFieldNames then
  begin
    FDataSet.DisableControls;
    try
      SetPropValue(FDataSet, 'IndexFieldNames', FOriginalIndexFieldNames);
    finally
      FDataSet.EnableControls;
    end;
  end;
end;

procedure TVittixDBGridSortEngine.ApplyClientDataSetSorting;
var
  Sorted: TList<TVittixDBGridColumnInfo>;
  Fields: TStringList;
  DescFields: TStringList;
  ClientDataSet: TCustomClientDataSet;
  Info: TVittixDBGridColumnInfo;
  I: Integer;
begin
  ClientDataSet := TCustomClientDataSet(FDataSet);
  Sorted := TList<TVittixDBGridColumnInfo>.Create;
  Fields := TStringList.Create;
  DescFields := TStringList.Create;
  try
    Fields.Delimiter := ';';
    Fields.StrictDelimiter := True;
    DescFields.Delimiter := ';';
    DescFields.StrictDelimiter := True;

    for I := 0 to FColumns.Count - 1 do
      if FColumns[I].SortOrder <> vsoNone then
        Sorted.Add(FColumns[I]);

    if Sorted.Count = 0 then
    begin
      TVittixCDSAccess(ClientDataSet).IndexName := '';
      ClientDataSet.IndexFieldNames := '';
      Exit;
    end;

    Sorted.Sort(
      TComparer<TVittixDBGridColumnInfo>.Construct(
        function(const L, R: TVittixDBGridColumnInfo): Integer
        begin
          Result := L.SortIndex - R.SortIndex;
        end
      )
    );

    for Info in Sorted do
    begin
      if FDataSet.FindField(Info.FieldName) = nil then
      begin
        if Assigned(FOnFieldValidation) then
          FOnFieldValidation(Info.FieldName, False);
        Continue;
      end;

      Fields.Add(Info.FieldName);
      if Info.SortOrder = vsoDesc then
        DescFields.Add(Info.FieldName);
    end;

    ClientDataSet.DisableControls;
    try
      TVittixCDSAccess(ClientDataSet).IndexName := '';
      try
        ClientDataSet.DeleteIndex(TEMP_CLIENT_DATASET_INDEX);
      except
        // Expected when the temp index does not exist yet
        on E: EDatabaseError do ;
      end;

      if Fields.Count = 0 then
        Exit;

      ClientDataSet.AddIndex(
        TEMP_CLIENT_DATASET_INDEX,
        Fields.DelimitedText,
        [],
        DescFields.DelimitedText
      );
      TVittixCDSAccess(ClientDataSet).IndexName := TEMP_CLIENT_DATASET_INDEX;
      FTempIndexCreated := True;
    finally
      ClientDataSet.EnableControls;
    end;
  finally
    DescFields.Free;
    Fields.Free;
    Sorted.Free;
  end;
end;

procedure TVittixDBGridSortEngine.ClearSorting;
var
  I: Integer;
begin
  if not Assigned(FColumns) then Exit;

  // Reset internal state
  for I := 0 to FColumns.Count - 1 do
  begin
    FColumns[I].SortOrder := vsoNone;
    FColumns[I].SortIndex := -1;
  end;

  if not Assigned(FDataSet) or not FDataSet.Active then Exit;

  RestoreOriginalIndexState;

  FTempIndexCreated := False;
end;

procedure TVittixDBGridSortEngine.ToggleSort(
  AColumn: TColumn;
  MultiColumn: Boolean);
var
  Info: TVittixDBGridColumnInfo;
  I: Integer;
  SavedOrders: TArray<TVittixSortOrder>;
  SavedIndexes: TArray<Integer>;
begin
  if not Assigned(AColumn) then Exit;

  Info := FColumns.FindByFieldName(AColumn.FieldName);
  if Info = nil then Exit;

  // Snapshot the sort state: when ApplySorting fails (unsupported dataset,
  // dataset error) the column metadata must roll back too, otherwise the
  // grid shows a sort arrow over unsorted data.
  SetLength(SavedOrders, FColumns.Count);
  SetLength(SavedIndexes, FColumns.Count);
  for I := 0 to FColumns.Count - 1 do
  begin
    SavedOrders[I] := FColumns[I].SortOrder;
    SavedIndexes[I] := FColumns[I].SortIndex;
  end;

  try
    // Single-column sort: clear everything else first
    if not MultiColumn then
    begin
      for I := 0 to FColumns.Count - 1 do
        if FColumns[I] <> Info then
        begin
          FColumns[I].SortOrder := vsoNone;
          FColumns[I].SortIndex := -1;
        end;
    end;

    // Tri-state toggle: None -> Asc -> Desc -> None
    case Info.SortOrder of
      vsoNone: Info.SortOrder := vsoAsc;
      vsoAsc:  Info.SortOrder := vsoDesc;
      vsoDesc: Info.SortOrder := vsoNone;
    end;

    // Assign sort index for new sort items
    if Info.SortOrder = vsoNone then
      Info.SortIndex := -1
    else if Info.SortIndex < 0 then
      // Assign temporary high index; Normalize will compact it
      Info.SortIndex := FColumns.Count + 1;

    NormalizeSortIndices;
    ApplySorting;
  except
    for I := 0 to FColumns.Count - 1 do
    begin
      FColumns[I].SortOrder := SavedOrders[I];
      FColumns[I].SortIndex := SavedIndexes[I];
    end;
    raise;
  end;
end;

function TVittixDBGridSortEngine.GetSortSummaryText: string;
var
  Parts: TStringList;
  I: Integer;
begin
  Result := '';
  if not Assigned(FColumns) then Exit;

  Parts := TStringList.Create;
  try
    for I := 0 to FColumns.Count - 1 do
      if FColumns[I].SortOrder <> vsoNone then
        Parts.Add(Format('%s:%s#%d', [
          FColumns[I].FieldName,
          IfThen(FColumns[I].SortOrder = vsoAsc, 'A', 'D'),
          FColumns[I].SortIndex
        ]));
    Result := Parts.CommaText;
  finally
    Parts.Free;
  end;
end;

end.
