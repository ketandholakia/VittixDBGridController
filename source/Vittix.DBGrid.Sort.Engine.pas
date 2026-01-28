unit Vittix.DBGrid.Sort.Engine;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,
  System.Generics.Defaults,
  Data.DB,
  Vcl.DBGrids,
  Vittix.DBGrid.ColumnInfo;

type
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
    FOnFieldValidation: TFieldValidationEvent;

    function DataSetSupportsIndexFieldNames: Boolean;
    function BuildIndexFieldNames: string;
    procedure NormalizeSortIndices;

  public
    constructor Create(ADataSet: TDataSet; AColumns: TVittixDBGridColumns);

    procedure ApplySorting;
    procedure ClearSorting;
    procedure ToggleSort(AColumn: TColumn; MultiColumn: Boolean = False);

    property OnFieldValidation: TFieldValidationEvent read FOnFieldValidation write FOnFieldValidation;
  end;

implementation

{ TVittixDBGridSortEngine }

constructor TVittixDBGridSortEngine.Create(
  ADataSet: TDataSet;
  AColumns: TVittixDBGridColumns);
begin
  inherited Create;
  FDataSet := ADataSet;
  FColumns := AColumns;
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
begin
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;
  if not DataSetSupportsIndexFieldNames then Exit;

  IndexFields := BuildIndexFieldNames;

  FDataSet.DisableControls;
  try
    // Use RTTI to set the property safely
    SetPropValue(FDataSet, 'IndexFieldNames', IndexFields);
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TVittixDBGridSortEngine.ClearSorting;
var
  I: Integer;
begin
  // Reset internal state
  for I := 0 to FColumns.Count - 1 do
  begin
    FColumns[I].SortOrder := vsoNone;
    FColumns[I].SortIndex := -1;
  end;

  if not Assigned(FDataSet) or not FDataSet.Active then Exit;

  // Clear dataset sorting
  if DataSetSupportsIndexFieldNames then
  begin
    FDataSet.DisableControls;
    try
      SetPropValue(FDataSet, 'IndexFieldNames', '');
    finally
      FDataSet.EnableControls;
    end;
  end;
end;

procedure TVittixDBGridSortEngine.ToggleSort(
  AColumn: TColumn;
  MultiColumn: Boolean);
var
  Info: TVittixDBGridColumnInfo;
  I: Integer;
begin
  if not Assigned(AColumn) then Exit;

  Info := FColumns.FindByFieldName(AColumn.FieldName);
  if Info = nil then Exit; 

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
end;

end.