unit Vittix.DBGrid.Aggregation.Engine;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Diagnostics,
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
    FFullScanCount: Int64;

    // Optional callback to decide whether a record participates
    // (filters, selection, etc.)
    FAcceptRecord: TFunc<Boolean>;
    // Reports whether the filter engine currently owns the dataset's
    // OnFilterRecord hook. With FDataSet.Filtered True, iteration already
    // yields only accepted rows, so the per-record FAcceptRecord callback is
    // redundant and is skipped during the scan.
    FFilterHookActive: TFunc<Boolean>;
    FOnFormatAggregation: TFormatAggregationEvent;

    function IsNumericField(AField: TField): Boolean;
    function IsStringField(AField: TField): Boolean;
    function IsDateField(AField: TField): Boolean;
    
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

    function GetActiveAggregationSummaryText: string;

    /// <summary>Number of full dataset scans performed by this engine.</summary>
    property FullScanCount: Int64 read FFullScanCount;

    property OnAcceptRecord: TFunc<Boolean>
      read FAcceptRecord write FAcceptRecord;

    /// <summary>Optional callback telling the engine that the filter
    /// engine's own OnFilterRecord hook is installed. When it returns True
    /// and the dataset is Filtered, the per-record OnAcceptRecord callback
    /// is skipped because dataset iteration already respects the filter.
    /// </summary>
    property FilterHookActive: TFunc<Boolean>
      read FFilterHookActive write FFilterHookActive;

    property OnFormatAggregation: TFormatAggregationEvent
      read FOnFormatAggregation write FOnFormatAggregation;
  end;

implementation

const
  // Largest magnitude representable by Currency; BCD values inside this
  // range accumulate exactly.
  MAX_CURRENCY = 922337203685477.5807;

function LocalAggregationTypeToString(Value: TVittixAggregationType): string;
begin
  case Value of
    vatCount: Result := 'Count';
    vatSum: Result := 'Sum';
    vatAvg: Result := 'Avg';
    vatMin: Result := 'Min';
    vatMax: Result := 'Max';
  else
    Result := 'None';
  end;
end;

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
  Agg: TVittixAggregation;
begin
  if Assigned(FColumns) then
    for I := 0 to FColumns.Count - 1 do
    begin
      // Shuttle the property-backed record through a local so the
      // mutation lands on the item, not on a compiler temporary
      Agg := FColumns[I].Aggregation;
      Agg.Clear;
      FColumns[I].Aggregation := Agg;
    end;
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
  // Every numeric field type the server-side SQL aggregates cover, so the
  // client SUM/AVG/MIN/MAX match ServerAggregateRefresh exactly.
  // ftSingle/ftExtended take the float path; all integer-like types take
  // the Int64 path (see AggregateField).
  Result := (AField.DataType in [
    ftShortint, ftByte, ftSmallint, ftWord,
    ftInteger, ftLongWord, ftLargeint, ftAutoInc,
    ftFloat, ftSingle, ftExtended, ftCurrency, ftBCD, ftFMTBcd
  ]);
end;

function TVittixDBGridAggregationEngine.IsDateField(
  AField: TField): Boolean;
begin
  Result := (AField.DataType in [
    ftDate, ftTime, ftDateTime, ftTimeStamp
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
            ftShortint, ftByte, ftSmallint, ftWord,
            ftInteger, ftLongWord, ftLargeint, ftAutoInc:
              begin
                IntVal := AField.AsLargeInt;
                Agg.HasInt := True;
                Agg.SumInt := Agg.SumInt + IntVal;
                // Clear() seeds MinInt/MaxInt with proper sentinels, so the
                // plain comparison is correct for every record including the
                // first one.
                if IntVal < Agg.MinInt then Agg.MinInt := IntVal;
                if IntVal > Agg.MaxInt then Agg.MaxInt := IntVal;
              end;

            ftCurrency:
              begin
                CurrVal := AField.AsCurrency;
                Agg.HasCurrency := True;
                Agg.SumCurrency := Agg.SumCurrency + CurrVal;
                if CurrVal < Agg.MinCurrency then Agg.MinCurrency := CurrVal;
                if CurrVal > Agg.MaxCurrency then Agg.MaxCurrency := CurrVal;
              end;

            ftFloat, ftSingle, ftExtended:
              begin
                FloatVal := AField.AsFloat;
                Agg.HasFloat := True;
                Agg.SumFloat := Agg.SumFloat + FloatVal;
                if FloatVal < Agg.MinFloat then Agg.MinFloat := FloatVal;
                if FloatVal > Agg.MaxFloat then Agg.MaxFloat := FloatVal;
              end;

            ftBCD, ftFMTBcd:
              begin
                FloatVal := AField.AsFloat;
                if not Agg.BCDAsFloat then
                begin
                  // Currency is a scaled Int64: 0.1 stays 0.1, so repeated
                  // sums do not drift the way Double does. Fall back to the
                  // Double accumulator only when a value or the running sum
                  // leaves the Currency range.
                  if (FloatVal <= MAX_CURRENCY) and
                     (FloatVal >= -MAX_CURRENCY) and
                     (Abs(Double(Agg.SumBCD) + FloatVal) <= MAX_CURRENCY) then
                  begin
                    CurrVal := AField.AsCurrency;
                    if not Agg.HasBCD then
                    begin
                      Agg.HasBCD := True;
                      Agg.SumBCD := CurrVal;
                      Agg.MinBCD := CurrVal;
                      Agg.MaxBCD := CurrVal;
                    end
                    else
                    begin
                      Agg.SumBCD := Agg.SumBCD + CurrVal;
                      if CurrVal < Agg.MinBCD then Agg.MinBCD := CurrVal;
                      if CurrVal > Agg.MaxBCD then Agg.MaxBCD := CurrVal;
                    end;
                  end
                  else
                    Agg.BCDAsFloat := True;
                end;

                if Agg.BCDAsFloat then
                begin
                  if not Agg.BCDConverted then
                  begin
                    // Fold the exact BCD sum in exactly once.
                    if Agg.HasBCD then
                    begin
                      Agg.HasFloat := True;
                      Agg.SumFloat := Agg.SumFloat + Agg.SumBCD;
                      if Agg.MinBCD < Agg.MinFloat then
                        Agg.MinFloat := Agg.MinBCD;
                      if Agg.MaxBCD > Agg.MaxFloat then
                        Agg.MaxFloat := Agg.MaxBCD;
                    end;
                    Agg.BCDConverted := True;
                  end;
                  Agg.HasFloat := True;
                  Agg.SumFloat := Agg.SumFloat + FloatVal;
                  if FloatVal < Agg.MinFloat then Agg.MinFloat := FloatVal;
                  if FloatVal > Agg.MaxFloat then Agg.MaxFloat := FloatVal;
                end;
              end;
          end;

          Exit;
        end;

        // ---------- Date / time (Min / Max only) ----------
        if IsDateField(AField) and (AggType in [vatMin, vatMax]) then
        begin
          FloatVal := AField.AsDateTime;
          if not Agg.HasDate then
          begin
            Agg.HasDate := True;
            Agg.MinDate := FloatVal;
            Agg.MaxDate := FloatVal;
          end
          else
          begin
            if FloatVal < Agg.MinDate then Agg.MinDate := FloatVal;
            if FloatVal > Agg.MaxDate then Agg.MaxDate := FloatVal;
          end;
          Exit;
        end;

        // ---------- String (Min / Max only) ----------
        if IsStringField(AField) and (AggType in [vatMin, vatMax]) then
        begin
          StrVal := AField.AsString;
          // HasString flags the first record: an empty string is a valid
          // Min/Max value and must not be skipped.
          if not Agg.HasString then
          begin
            Agg.HasString := True;
            Agg.MinString := StrVal;
            Agg.MaxString := StrVal;
          end
          else
          begin
            if StrVal < Agg.MinString then Agg.MinString := StrVal;
            if StrVal > Agg.MaxString then Agg.MaxString := StrVal;
          end;
        end;
      end;
  end;
end;

procedure TVittixDBGridAggregationEngine.Recalculate;
var
  Bookmark: TBookmark;
  I: Integer;
  // One local accumulator per cached column: the property-backed record is
  // copied in once and written back once, instead of on every row.
  Accums: TArray<TVittixAggregation>;
  SkipAccept: Boolean;
{$IFDEF DEBUG}
  Stopwatch: TStopwatch;
{$ENDIF}
begin
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;
  // Navigation would implicitly post an edit or insert before scanning.
  if FDataSet.State in dsEditModes then Exit;

  // Full dataset scan by design. Profile before trying to optimize this path.
{$IFDEF DEBUG}
  Stopwatch := TStopwatch.StartNew;
{$ENDIF}

  // 1. Reset all aggregators
  Clear;

  // 2. Prepare the optimized field list
  BuildCache;
  if Length(FCache) = 0 then Exit; // Nothing to calculate

  SetLength(Accums, Length(FCache));
  for I := 0 to High(Accums) do
    Accums[I].Clear;

  // When the filter engine's own OnFilterRecord hook is installed and the
  // dataset is Filtered, iteration already yields only accepted rows, so the
  // per-record callback is redundant. Otherwise it still runs.
  SkipAccept := Assigned(FFilterHookActive) and FFilterHookActive() and
    FDataSet.Filtered;

  // 3. Disable controls and save position
  Bookmark := nil;
  // Safety: If dataset is empty, GetBookmark might fail on some DB components
  if not FDataSet.IsEmpty then
    Bookmark := FDataSet.GetBookmark;

  FDataSet.DisableControls;
  try
    Inc(FFullScanCount);
    FDataSet.First;
    while not FDataSet.Eof do
    begin
      // Filter / selection awareness
      // (This callback checks the FilterEngine)
      if (not SkipAccept) and Assigned(FAcceptRecord) and
         not FAcceptRecord() then
      begin
        FDataSet.Next;
        Continue;
      end;

      // Iterate ONLY the cached active aggregation fields
      for I := 0 to High(FCache) do
      begin
        if FCache[I].Field.IsNull then
        begin
          Inc(Accums[I].NullCount);
          Continue;
        end;

        AggregateField(
          FCache[I].Field,
          Accums[I],
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

  // Write the finished accumulators back once per column.
  for I := 0 to High(FCache) do
    FCache[I].Info.Aggregation := Accums[I];

{$IFDEF DEBUG}
  OutputDebugString(PChar(Format(
    'Vittix aggregation recalculated in %d ms for %d active aggregation columns',
    [Stopwatch.ElapsedMilliseconds, Length(FCache)]
  )));
{$ENDIF}
end;

function TVittixDBGridAggregationEngine.GetAggregation(
  Info: TVittixDBGridColumnInfo): Variant;
begin
  if Assigned(Info) then
    Result := Info.Aggregation.GetValue(Info.AggregationType)
  else
    Result := Null;
end;

function IsIntegerDataType(AType: TFieldType): Boolean;
begin
  Result := AType in [
    ftShortint, ftByte, ftSmallint, ftWord,
    ftInteger, ftLongWord, ftLargeint, ftAutoInc
  ];
end;

/// <summary>Returns the field's custom display format when it has one.
/// DisplayFormat lives on the concrete numeric and date field classes, not
/// on TField, so this checks the relevant ones.</summary>
function TryFieldDisplayFormat(AField: TField; out AFormat: string): Boolean;
begin
  AFormat := '';
  if AField is TNumericField then
    AFormat := TNumericField(AField).DisplayFormat
  else if AField is TDateTimeField then
    AFormat := TDateTimeField(AField).DisplayFormat
  else if AField is TSQLTimeStampField then
    AFormat := TSQLTimeStampField(AField).DisplayFormat;
  Result := AFormat <> '';
end;

function TVittixDBGridAggregationEngine.GetAggregationDisplayText(
  Info: TVittixDBGridColumnInfo): string;
var
  V: Variant;
  Field: TField;
  Fmt: string;
  HasFmt: Boolean;
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

  Field := nil;
  if Assigned(FDataSet) then
    Field := FDataSet.FindField(Info.FieldName);
  HasFmt := Assigned(Field) and TryFieldDisplayFormat(Field, Fmt);

  case Info.AggregationType of
    vatCount:
      Result := IntToStr(V);

    vatSum, vatAvg:
      begin
        if HasFmt then
          Result := FormatFloat(Fmt, V)
        else if Assigned(Field) and IsIntegerDataType(Field.DataType) then
        begin
          // Integers carry no fractional part; SUM must not add ".00".
          if Info.AggregationType = vatSum then
            Result := FormatFloat('#,##0', V)
          else
            Result := FormatFloat('#,##0.00', V);
        end
        else
          Result := FormatFloat('#,##0.00', V);
      end;

    vatMin, vatMax:
      begin
        if VarType(V) = varDate then
        begin
          if HasFmt then
            Result := FormatDateTime(Fmt, V)
          else
            Result := DateTimeToStr(V);
        end
        else if HasFmt and IsNumericField(Field) then
          Result := FormatFloat(Fmt, V)
        else
          Result := VarToStr(V);
      end;
  end;
end;

function TVittixDBGridAggregationEngine.GetActiveAggregationSummaryText: string;
var
  Parts: TStringList;
  I: Integer;
  Info: TVittixDBGridColumnInfo;
begin
  Result := '';
  if not Assigned(FColumns) then Exit;

  Parts := TStringList.Create;
  try
    for I := 0 to FColumns.Count - 1 do
    begin
      Info := FColumns[I];
      if Info.AggregationType <> vatNone then
        Parts.Add(Format('%s:%s', [Info.FieldName, LocalAggregationTypeToString(Info.AggregationType)]));
    end;
    Result := Parts.CommaText;
  finally
    Parts.Free;
  end;
end;

end.
