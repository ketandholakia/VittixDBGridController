unit Vittix.DBGrid.Reg;

interface

procedure Register;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  DesignIntf,
  DesignEditors,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Editor;

type
  TVittixAggregationTypeProperty = class(TEnumProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

const
  AggregationNames: array[TVittixAggregationType] of string = (
    'None', 'Count', 'Sum', 'Average', 'Minimum', 'Maximum');

function TVittixAggregationTypeProperty.GetValue: string;
var
  AggType: TVittixAggregationType;
begin
  AggType := TVittixAggregationType(GetOrdValue);
  if (AggType >= Low(TVittixAggregationType)) and
     (AggType <= High(TVittixAggregationType)) then
    Result := AggregationNames[AggType]
  else
    Result := IntToStr(GetOrdValue);
end;

procedure TVittixAggregationTypeProperty.GetValues(Proc: TGetStrProc);
var
  AggType: TVittixAggregationType;
begin
  for AggType := Low(TVittixAggregationType) to High(TVittixAggregationType) do
    Proc(AggregationNames[AggType]);
end;

procedure TVittixAggregationTypeProperty.SetValue(const Value: string);
var
  AggType: TVittixAggregationType;
begin
  for AggType := Low(TVittixAggregationType) to High(TVittixAggregationType) do
    if SameText(Value, AggregationNames[AggType]) or
       SameText(Value, GetEnumName(TypeInfo(TVittixAggregationType), Ord(AggType))) then
    begin
      SetOrdValue(Ord(AggType));
      Exit;
    end;
  raise EPropertyError.CreateFmt('Invalid aggregation type: %s', [Value]);
end;

procedure Register;
begin
  RegisterComponents('Vittix', [TVittixDBGrid]);
  RegisterComponentEditor(TVittixDBGrid, TVittixDBGridEditor);
  RegisterPropertyEditor(TypeInfo(TVittixAggregationType),
    TVittixDBGridColumnInfo, 'AggregationType', TVittixAggregationTypeProperty);

  RegisterPropertiesInCategory(sLayoutCategoryName, TVittixDBGrid,
    ['FrozenColumns']);
  RegisterPropertiesInCategory(sAppearanceCategoryName, TVittixDBGrid,
    ['FooterVisible', 'AlternatingRowColors', 'AlternateRowColor']);
  RegisterPropertiesInCategory(sBehaviorCategoryName, TVittixDBGrid,
    ['IncrementalSearch', 'IncrementalSearchTimeout',
     'CopyIncludeHeaders', 'FilterMode']);
  RegisterPropertiesInCategory('Persistence', TVittixDBGrid,
    ['LayoutStorageFileName', 'ChooserStateFileName',
     'FilterHistoryFileName', 'FilterSetsFileName', 'PersistenceRootPath']);

  RegisterPropertiesInCategory(sLayoutCategoryName, TVittixDBGridColumnInfo,
    ['Pinned', 'TitleCaption']);
  RegisterPropertiesInCategory(sAppearanceCategoryName,
    TVittixDBGridColumnInfo,
    ['DisplayFormat', 'FooterText', 'CellConditions', 'ExportCaption']);
  RegisterPropertiesInCategory(sBehaviorCategoryName,
    TVittixDBGridColumnInfo,
    ['ExportInclude', 'EditorKind', 'PickItems', 'HasMin', 'MinValue',
     'HasMax', 'MaxValue', 'Required', 'DecimalPlaces']);
end;

end.
