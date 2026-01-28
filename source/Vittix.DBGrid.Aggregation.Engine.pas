unit Vittix.DBGrid.Aggregation.Engine;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Data.DB,
  Vittix.DBGrid.ColumnInfo;

type
  /// <summary>
  /// Internal cache map to link a column to its dataset field.
  /// Used to avoid FindField calls inside the record loop.
  /// </summary>
  TAggregationFieldMap = record
    Info: TVittixDBGridColumnInfo;
    Field: TField;
  end;

  TFormatAggregationEvent = procedure(
    Sender: TObject;
    Info: TVittixDBGridColumnInfo;
    AggType: TVittixAggregationType;
    Value: Variant;
    var DisplayText: string
  ) of object;

  /// <summary>
  /// Logic-only aggregation engine.
  /// Calculates per-column aggregations based on dataset content.
  /// Grid-agnostic, filter-safe, footer-safe.
  /// </summary>
  TVittixDBGridAggregationEngine = class
  private
    FDataSet: TDataSet;
    FColumns: TVittixDBGridColumns;
    
    // Performance: Cache active aggregations so we don't iterate 
    // all columns or call FindField during the loop.
    FCache: TArray<TAggregationFieldMap>;

    // Optional callback to decide whether a record participates
    // (filters, selection, etc.)
    FAcceptRecord: TFunc<Boolean>;
    FOnFormatAggregation: TFormatAggregationEvent;

    function IsNumericField(AField: TField): Boolean;
    function IsStringField(AField: TField): Boolean;
    
    procedure BuildCache;

    procedure AggregateField(
      AField: TField;
      var Agg: TVittixAggregation;
      AggType: TVittixAggregationType
    );

  public
    constructor Create(
      ADataSet: TDataSet;
      AColumns: TVittixDBGridColumns
    );

    procedure Clear;
    procedure Recalculate;

    function GetAggregation(
      Info: TVittixDBGridColumnInfo
    ): Variant;

    function GetAggregationDisplayText(
      Info: TVittixDBGridColumnInfo
    ): string;

    property OnAcceptRecord: TFunc<Boolean>
      read FAcceptRecord write FAcceptRecord;

    property OnFormatAggregation: TFormatAggregationEvent
      read FOnFormatAggregation write FOnFormatAggregation;
  end;

implementation

{ TVittixDBGridAggregationEngine }

constructor TVittixDBGridAggregationEngine.Create(
  ADataSet: TDataSet;
  AColumns: TVittixDBGridColumns);
begin
  inherited Create;
  FDataSet := ADataSet;
  FColumns := AColumns;
end;

procedure TVittixDBGridAggregationEngine.Clear;
var
  I: Integer;
begin
  if Assigned(FColumns) then
    for I := 0 to FColumns.Count - 1 do
      FColumns[I].Aggregation.Clear;
end;

procedure TVittixDBGridAggregationEngine.BuildCache;
var
  I: Integer;
  Field: TField;
  Info: TVittixDBGridColumnInfo;
begin
  SetLength(FCache, 0);
  if not Assigned(FDataSet) or not Assigned(FColumns) then Exit;

  // Only cache columns that explicitly require aggregation
  for I := 0 to FColumns.Count - 1 do
  begin
    Info := FColumns[I];
    if Info.AggregationType = vatNone then Continue;

    Field := FDataSet.FindField(Info.FieldName);
    if Assigned(Field) then
    begin
      SetLength(FCache, Length(FCache) + 1);
      FCache[High(FCache)].Info := Info;
      FCache[High(FCache)].Field := Field;
    end;
  end;
end;

function TVittixDBGridAggregationEngine.IsNumericField(
  AField: TField): Boolean;
begin
  // Optimized check
  Result := (AField.DataType in [
    ftSmallint, ftInteger, ftLargeint,
    ftFloat, ftCurrency, ftBCD, ftFMTBcd, ftWord, ftAutoInc
  ]);
end;

function TVittixDBGridAggregationEngine.IsStringField(
  AField: TField): Boolean;
begin
  Result := (AField.DataType in [
    ftString, ftWideString, ftFixedChar,
    ftFixedWideChar, ftMemo, ftWideMemo
  ]);
end;

procedure TVittixDBGridAggregationEngine.AggregateField(
  AField: TField;
  var Agg: TVittixAggregation;
  AggType: TVittixAggregationType);
var
  IntVal: Int64;
  FloatVal: Double;
  CurrVal: Currency;
  StrVal: string;
begin
  case AggType of
    vatCount:
      begin
        Inc(Agg.Count);
        Exit;
      end;

    vatSum, vatAvg, vatMin, vatMax:
      begin
        Inc(Agg.Count);

        // ---------- Numeric ----------
        if IsNumericField(AField) then
        begin
          case AField.DataType of
            ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc:
              begin
                IntVal := AField.AsLargeInt;
                Agg.HasInt := True;
                Agg.SumInt := Agg.SumInt + IntVal;

                if (Agg.Count = 1) or (IntVal < Agg.MinInt) then Agg.MinInt := IntVal;
                if (Agg.Count = 1) or (IntVal > Agg.MaxInt) then Agg.MaxInt := IntVal;
              end;

            ftCurrency:
              begin
                CurrVal := AField.AsCurrency;
                Agg.HasCurrency := True;
                Agg.SumCurrency := Agg.SumCurrency + CurrVal;

                if (Agg.Count = 1) or (CurrVal < Agg.MinCurrency) then Agg.MinCurrency := CurrVal;
                if (Agg.Count = 1) or (CurrVal > Agg.MaxCurrency) then Agg.MaxCurrency := CurrVal;
              end;

            ftFloat, ftBCD, ftFMTBcd:
              begin
                FloatVal := AField.AsFloat;
                Agg.HasFloat := True;
                Agg.SumFloat := Agg.SumFloat + FloatVal;

                if (Agg.Count = 1) or (FloatVal < Agg.MinFloat) then Agg.MinFloat := FloatVal;
                if (Agg.Count = 1) or (FloatVal > Agg.MaxFloat) then Agg.MaxFloat := FloatVal;
              end;
          end;

          Exit;
        end;

        // ---------- String (Min / Max only) ----------
        if IsStringField(AField) and (AggType in [vatMin, vatMax]) then
        begin
          StrVal := AField.AsString;
          Agg.HasString := True;

          if Agg.MinString = '' then
            Agg.MinString := StrVal
          else if StrVal < Agg.MinString then
            Agg.MinString := StrVal;

          if Agg.MaxString = '' then
            Agg.MaxString := StrVal
          else if StrVal > Agg.MaxString then
            Agg.MaxString := StrVal;
        end;
      end;
  end;
end;

procedure TVittixDBGridAggregationEngine.Recalculate;
var
  Bookmark: TBookmark;
  I: Integer;
begin
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;

  // 1. Reset all aggregators
  Clear;

  // 2. Prepare the optimized field list
  BuildCache;
  if Length(FCache) = 0 then Exit; // Nothing to calculate

  // 3. Disable controls and save position
  Bookmark := nil;
  // Safety: If dataset is empty, GetBookmark might fail on some DB components
  if not FDataSet.IsEmpty then
    Bookmark := FDataSet.GetBookmark;

  FDataSet.DisableControls;
  try
    FDataSet.First;
    while not FDataSet.Eof do
    begin
      // Filter / selection awareness
      // (This callback checks the FilterEngine)
      if Assigned(FAcceptRecord) and not FAcceptRecord() then
      begin
        FDataSet.Next;
        Continue;
      end;

      // Iterate ONLY the cached active aggregation fields
      for I := 0 to High(FCache) do
      begin
        if FCache[I].Field.IsNull then
        begin
          Inc(FCache[I].Info.Aggregation.NullCount);
          Continue;
        end;

        AggregateField(
          FCache[I].Field,
          FCache[I].Info.Aggregation,
          FCache[I].Info.AggregationType
        );
      end;

      FDataSet.Next;
    end;
  finally
    if Bookmark <> nil then
    begin
      try
        if FDataSet.BookmarkValid(Bookmark) then
          FDataSet.GotoBookmark(Bookmark);
      finally
        FDataSet.FreeBookmark(Bookmark);
      end;
    end;
    FDataSet.EnableControls;
  end;
end;

function TVittixDBGridAggregationEngine.GetAggregation(
  Info: TVittixDBGridColumnInfo): Variant;
begin
  if Assigned(Info) then
    Result := Info.Aggregation.GetValue(Info.AggregationType)
  else
    Result := Null;
end;

function TVittixDBGridAggregationEngine.GetAggregationDisplayText(
  Info: TVittixDBGridColumnInfo): string;
var
  V: Variant;
begin
  Result := '';
  if not Assigned(Info) then Exit;

  V := GetAggregation(Info);
  if VarIsNull(V) then Exit;

  // Allow custom formatting
  if Assigned(FOnFormatAggregation) then
  begin
    FOnFormatAggregation(Self, Info, Info.AggregationType, V, Result);
    Exit;
  end;

  // You might want to respect TField.DisplayFormat here if possible,
  // but standard formatting is usually fine for footers.
  case Info.AggregationType of
    vatCount: Result := IntToStr(V);
    vatSum:   Result := FormatFloat('#,##0.00', V);
    vatAvg:   Result := FormatFloat('#,##0.00', V);
    vatMin,
    vatMax:   Result := VarToStr(V);
  end;
end;

end.