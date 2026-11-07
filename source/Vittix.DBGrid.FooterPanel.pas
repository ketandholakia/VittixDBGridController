unit Vittix.DBGrid.FooterPanel;

{$REGION 'Documentation'}
/// <summary>
/// Footer Panel for TVittixDBGrid: renders the aggregation footer under the
/// grid. The controller's GridWindowProc calls SyncLayout for the relevant
/// window messages; this panel owns no hooks itself.
/// </summary>
{$ENDREGION}

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  System.UITypes,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.DBGrids,
  Vcl.Grids,
  Winapi.Messages,
  Vcl.Menus,
  Data.DB,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Aggregation.Engine;

type
  TVittixDBGridFooterPanel = class(TCustomControl)
  private
    // TDBGrid-typed so this unit does not need Vittix.DBGrid in its
    // interface (Vittix.DBGrid.Controller uses this unit in ITS interface).
    // Attach is only called with a TVittixDBGrid; the implementation-side
    // VittixGrid() helper gives typed access to the grid-specific helpers.
    FGrid: TDBGrid;
    FAggregationEngine: TVittixDBGridAggregationEngine;
    FPopup: TPopupMenu;
    FContextColumn: TColumn;
    FSyncingLayout: Boolean;
    // Cached text metrics: SyncLayout runs on every WM_PAINT and scroll
    // message, and recomputing them needs a screen DC each time.
    FCachedHeight: Integer;
    FCachedFontHandle: THandle;
    // Current type-ahead search hint ('' = hidden), pushed by the controller
    FSearchHint: string;
    // Name of the applied filter set ('' = none), pushed by the controller;
    // the search hint takes priority when both are present.
    FFilterSetHint: string;
    // Server-mode status ("Server filter (...)" / "Client filter
    // (fallback)"); priority: search > server > filter set.
    FServerHint: string;
    // Selection aggregates, aligned with the visible columns in order;
    // recomputed per paint while a multi-row selection is active.
    FSelectionFields: TArray<TField>;
    FSelectionAggregates: TArray<TVittixAggregation>;

    procedure BuildPopup;
    procedure PopupClick(Sender: TObject);
    procedure PopupClearClick(Sender: TObject);
    procedure PopupClearAllClick(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure PopupCopyAllClick(Sender: TObject);
    procedure PopupCopySummaryClick(Sender: TObject);
    procedure AggregateSelectionValue(AField: TField; var Agg: TVittixAggregation);
    procedure ComputeSelectionAggregates;
    function SelectionAggregateTextFor(AIndex: Integer): string;
    function VisibleOrdinalOf(AColumn: TColumn): Integer;
    function GetIndicatorOffset: Integer;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function AggregationCaption(Agg: TVittixAggregationType): string;
    function GetPopupShortcutSummaryText: string;
    function GetPopupCaptionSummaryText: string;

    /// <summary>Footer-local rectangle for a column, derived from the
    /// grid's cell geometry. Public for diagnostics and tests.</summary>
    function GetColumnRect(AColumn: TColumn): TRect;

    /// <summary>The data column whose footer cell contains footer-local
    /// pixel X (nil when the point is over no column). Public for hosts
    /// and tests.</summary>
    function HitTestColumn(X: Integer): TColumn;

    /// <summary>Footer-local rectangle of the indicator corner (empty when
    /// the grid has no indicator column). Public for diagnostics and tests.
    /// </summary>
    function GetIndicatorRect: TRect;
    function GetAggregationTextForColumn(AColumn: TColumn): string;
    procedure ClearAggregationAtClientX(X: Integer);
    procedure CopyAggregationAtClientX(X: Integer);
    procedure Attach(
      AGrid: TDBGrid;
      AEngine: TVittixDBGridAggregationEngine
    );
    procedure SyncLayout;
    procedure ClearAggregationForColumn(AColumn: TColumn);
    procedure ClearAllAggregations;
    procedure CopyAggregationForColumn(AColumn: TColumn);
    procedure CopyAllAggregations;
    procedure CopyFooterSummary;

    /// <summary>Shows (or clears, with '') the incremental search buffer
    /// hint in the footer's left corner. Called by the controller.</summary>
    procedure SetSearchHint(const AText: string);

    /// <summary>Shows (or clears, with '') the name of the applied filter
    /// set in the footer's hint area; the search hint takes priority when
    /// both are present. Called by the controller.</summary>
    procedure SetFilterSetHint(const AText: string);

    /// <summary>Shows (or clears, with '') the server-mode status in the
    /// footer's hint area; priority: search > server > filter set. Called
    /// by the controller.</summary>
    procedure SetServerHint(const AText: string);

    /// <summary>True while a multi-row selection is active (dgMultiSelect
    /// with more than one selected row): the footer displays per-column
    /// aggregates of the selection instead of the dataset-wide values, and
    /// the dataset-wide aggregation state is left untouched.</summary>
    function SelectionAggregateActive: Boolean;

    /// <summary>Aggregate display text of the current selection for
    /// AColumn: 'Sum: x' for numeric fields, 'Count: n' otherwise. Empty
    /// when no multi-row selection is active.</summary>
    function GetSelectionAggregateText(AColumn: TColumn): string;
  end;

implementation

uses
  // Implementation-only on purpose: Vittix.DBGrid.Controller (which this
  // unit's interface is used by) requires TVittixDBGrid in its interface.
  System.Math,
  System.Variants,
  Vittix.DBGrid,
  Vittix.DBGrid.Clipboard,
  Vittix.DBGrid.Styles;

function VittixGrid(AGrid: TDBGrid): TVittixDBGrid;
begin
  // Attach is only ever called with a TVittixDBGrid; the guard keeps the
  // helper nil-safe regardless.
  if AGrid is TVittixDBGrid then
    Result := TVittixDBGrid(AGrid)
  else
    Result := nil;
end;

{ TVittixDBGridFooterPanel }

constructor TVittixDBGridFooterPanel.Create(AOwner: TComponent);
begin
  inherited;
  Height := 24;
  ControlStyle := ControlStyle + [csOpaque];

  // OPTIMIZATION: Double buffering prevents flicker during scrolling/resizing
  DoubleBuffered := True;
end;

destructor TVittixDBGridFooterPanel.Destroy;
begin
  // The popup is rebuilt on every open; free the previous one first
  FreeAndNil(FPopup);
  inherited;
end;

procedure TVittixDBGridFooterPanel.Attach(
  AGrid: TDBGrid;
  AEngine: TVittixDBGridAggregationEngine);
begin
  FGrid := AGrid;
  FAggregationEngine := AEngine;

  // DESIGN-TIME SAFETY: AGrid.Parent is nil at design time (grid being placed
  // on a form for the first time). Setting Parent to nil causes AV in bds.exe.
  if not Assigned(AGrid) then Exit;
  if csDesigning in AGrid.ComponentState then Exit;
  if not Assigned(AGrid.Parent) then Exit;

  Parent := AGrid.Parent;
  Align := alNone;
  Anchors := [akLeft, akRight, akBottom];

  SyncLayout;
end;

procedure TVittixDBGridFooterPanel.SyncLayout;
var
  TM: TTextMetric;
  DC: HDC;
  NewLeft: Integer;
  NewTop: Integer;
  NewWidth: Integer;
  NewHeight: Integer;
  Changed: Boolean;
begin
  if not Assigned(FGrid) then Exit;
  if FSyncingLayout then Exit;

  // DESIGN-TIME SAFETY: Do not access GDI handles or ClientWidth in the IDE.
  if csDesigning in FGrid.ComponentState then Exit;

  FSyncingLayout := True;
  try
    // Text metrics only change when the grid font changes, so cache them
    // instead of allocating a screen DC on every WM_PAINT/scroll sync.
    if (FCachedHeight = 0) or (FGrid.Font.Handle <> FCachedFontHandle) then
    begin
      DC := GetDC(0);
      try
        SelectObject(DC, FGrid.Font.Handle);
        GetTextMetrics(DC, TM);
        FCachedHeight := TM.tmHeight + TM.tmExternalLeading +
          Vittix.DBGrid.Styles.ScaleForPPI(8, FGrid);
        FCachedFontHandle := FGrid.Font.Handle;
      finally
        ReleaseDC(0, DC);
      end;
    end;
    NewHeight := FCachedHeight;

    NewLeft := FGrid.Left;
    NewTop := FGrid.Top + FGrid.Height - NewHeight;
    NewWidth := FGrid.Width;

    Changed := (Left <> NewLeft) or (Top <> NewTop) or
               (Width <> NewWidth) or (Height <> NewHeight);

    if (Left <> NewLeft) then
      Left := NewLeft;
    if (Top <> NewTop) then
      Top := NewTop;
    if (Width <> NewWidth) then
      Width := NewWidth;
    if (Height <> NewHeight) then
      Height := NewHeight;

    // Invalidate only when the geometry actually moved; data changes reach
    // the footer through the controller's explicit Invalidate instead, so
    // paint cycles no longer re-request themselves via this path.
    if Changed then
      Invalidate;
  finally
    FSyncingLayout := False;
  end;
end;

function TVittixDBGridFooterPanel.GetIndicatorOffset: Integer;
begin
  Result := 0;
  if VittixGrid(FGrid) <> nil then
    // Use the public helper we added to TVittixDBGrid
    Result := VittixGrid(FGrid).GetIndicatorWidth;
end;

function TVittixDBGridFooterPanel.GetIndicatorRect: TRect;
var
  GridRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if VittixGrid(FGrid) = nil then
    Exit;

  if GetIndicatorOffset <= 0 then
    Exit;

  GridRect := VittixGrid(FGrid).GetCellRect(0, 1);
  Result := Rect(GridRect.Left, 0, GridRect.Right, Height);
end;

function TVittixDBGridFooterPanel.GetColumnRect(AColumn: TColumn): TRect;
var
  GridRect: TRect;
  VisibleIndex: Integer;
  I: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (VittixGrid(FGrid) = nil) or not Assigned(AColumn) then
    Exit;

  VisibleIndex := 0;
  for I := 0 to FGrid.Columns.Count - 1 do
  begin
    if not FGrid.Columns[I].Visible then
      Continue;

    if FGrid.Columns[I] = AColumn then
      Break;

    Inc(VisibleIndex);
  end;

  // CellRect gives the actual painted grid cell geometry, including indicator
  // offset and current grid line spacing. Convert it into footer-local coords.
  GridRect := VittixGrid(FGrid).GetCellRect(
    VisibleIndex + VittixGrid(FGrid).GetIndicatorOffset,
    1
  );
  Result := Rect(
    GridRect.Left,
    0,
    GridRect.Right,
    Height
  );
end;

class function TVittixDBGridFooterPanel.AggregationCaption(
  Agg: TVittixAggregationType): string;
begin
  case Agg of
    vatNone:  Result := 'Clear aggregation';
    vatCount: Result := 'Count';
    vatSum:   Result := 'Sum';
    vatAvg:   Result := 'Average';
    vatMin:   Result := 'Minimum';
    vatMax:   Result := 'Maximum';
  else
    Result := 'Aggregation';
  end;
end;

procedure TVittixDBGridFooterPanel.Paint;
var
  I: Integer;
  R: TRect;
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
  Text: string;
  DrawFlags: Cardinal;
  StartCol: Integer;
    StyleActive: Boolean;
    HasSelectionAgg: Boolean;
    VisIdx: Integer;
    HintText: string;
begin
  if not Assigned(FGrid) or (VittixGrid(FGrid) = nil) then Exit;

  StyleActive := TVittixStylePalette.IsCustomStyleActive(Self);

  Canvas.Font.Assign(FGrid.Font);
  Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  // The grid font color belongs to the unstyled look; under a custom style
  // the chrome text must follow the style instead.
  if StyleActive then
    Canvas.Font.Color := TVittixStylePalette.TextColor(Self);

  // The grid's FixedColor is the unstyled background; a custom style paints
  // the fixed chrome itself, so follow the style face color instead.
  if StyleActive then
    Canvas.Brush.Color := TVittixStylePalette.FaceColor(Self)
  else
    Canvas.Brush.Color := FGrid.FixedColor;
  Canvas.FillRect(ClientRect);

  // Draw the indicator/footer corner cell explicitly so the first data column
  // lines up visually with the footer grid. Without this, the indicator width
  // looks like part of the first column and makes the ID column appear missing.
  R := GetIndicatorRect;
  if not IsRectEmpty(R) then
  begin
    Canvas.FillRect(R);

    Canvas.Pen.Color := TVittixStylePalette.HighlightColor(Self);
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.LineTo(R.Right, R.Top);
    Canvas.MoveTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);

    Canvas.Pen.Color := TVittixStylePalette.ShadowColor(Self);
    Canvas.MoveTo(R.Right - 1, R.Top);
    Canvas.LineTo(R.Right - 1, R.Bottom);
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Right, R.Bottom - 1);
  end;

  // One selection scan per paint (not per column); the visible ordinal in
  // the loop must count columns the loop skipped past (left of StartCol).
  HasSelectionAgg := SelectionAggregateActive;
  if HasSelectionAgg then
    ComputeSelectionAggregates;
  VisIdx := -1;

  // TDBGrid.Columns holds ONLY data columns - the indicator is a fixed
  // column outside that list, so the walk starts at index 0. (The grid's
  // IndicatorOffset lives in fixed-column space; GetColumnRect already
  // adds it to CellRect.) Starting at IndicatorOffset skipped the first
  // data column and shifted the VisIdx/selection-aggregate pairing.
  // Iterate from the first data column, not LeftCol: with a frozen region
  // the columns below LeftCol stay visible and must keep their aggregates
  // aligned under them.
  StartCol := 0;

  for I := StartCol to FGrid.Columns.Count - 1 do
  begin
    Col := FGrid.Columns[I];
    if not Col.Visible then Continue;
    Inc(VisIdx);

    R := GetColumnRect(Col);
    if IsRectEmpty(R) then
      Continue;
    // Scrolled-out (non-frozen) columns report rects left of the client;
    // skip them instead of drawing into clipped space.
    if R.Right <= 0 then
      Continue;

    // Background
    Canvas.FillRect(R);

    // 3D Borders
    Canvas.Pen.Color := TVittixStylePalette.HighlightColor(Self);
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.LineTo(R.Right, R.Top);
    Canvas.MoveTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);

    Canvas.Pen.Color := TVittixStylePalette.ShadowColor(Self);
    Canvas.MoveTo(R.Right - 1, R.Top);
    Canvas.LineTo(R.Right - 1, R.Bottom);
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);

    // Text: selection aggregates shadow the dataset-wide aggregation state
    // while rows are selected (the configured aggregation stays untouched).
    if HasSelectionAgg then
      Text := SelectionAggregateTextFor(VisIdx)
    else
    begin
      Info := VittixGrid(FGrid).ColumnInfoByColumn(Col);
      if Assigned(Info) and Assigned(FAggregationEngine) then
        Text := FAggregationEngine.GetAggregationDisplayText(Info)
      else
        Text := '';

      if Text = '' then
      begin
        Info := VittixGrid(FGrid).ColumnInfoByColumn(Col);
        if Assigned(Info) then
          Text := Info.FooterText;
      end;
    end;

    InflateRect(R, -4, 0);

    DrawFlags := DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS;
    if UseRightToLeftAlignment then
      DrawFlags := DrawFlags or DT_RTLREADING;

    DrawText(
      Canvas.Handle,
      PChar(Text),
      Length(Text),
      R,
      DrawFlags
    );

    if R.Right > ClientWidth then Break;
  end;

  // Type-ahead search hint sits on top in the footer's left corner; the
  // applied filter set name shows in the same spot when no search is active.
  HintText := FSearchHint;
  if HintText = '' then
    HintText := FServerHint;
  if HintText = '' then
    HintText := FFilterSetHint;
  if HintText <> '' then
  begin
    R := Rect(4, 0, Min(240, ClientWidth), Height);
    DrawFlags := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS;
    DrawText(Canvas.Handle, PChar(HintText), Length(HintText), R, DrawFlags);
  end;
end;

function TVittixDBGridFooterPanel.HitTestColumn(X: Integer): TColumn;
var
  I, StartCol: Integer;
  R: TRect;
begin
  Result := nil;
  if (FGrid = nil) or (VittixGrid(FGrid) = nil) then Exit;

  // Hit testing must see the frozen columns too: they stay on screen even
  // when LeftCol has moved past them. Columns is data-only (the indicator
  // is a fixed column outside the list), so the walk starts at index 0.
  StartCol := 0;

  for I := StartCol to FGrid.Columns.Count - 1 do
  begin
    if not FGrid.Columns[I].Visible then Continue;

    R := GetColumnRect(FGrid.Columns[I]);
    if IsRectEmpty(R) then
      Continue;

    if (X >= R.Left) and (X < R.Right) then
      Exit(FGrid.Columns[I]);
    if R.Right > ClientWidth then Break;
  end;
end;

procedure TVittixDBGridFooterPanel.MouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;

  if Button <> mbRight then Exit;

  FContextColumn := HitTestColumn(X);
  if not Assigned(FContextColumn) then Exit;

  BuildPopup;
  P := ClientToScreen(Point(X, Y));
  FPopup.Popup(P.X, P.Y);
end;

procedure TVittixDBGridFooterPanel.DblClick;
begin
  inherited;
  ClearAggregationAtClientX(ScreenToClient(Mouse.CursorPos).X);
end;

function TVittixDBGridFooterPanel.GetAggregationTextForColumn(
  AColumn: TColumn): string;
var
  Info: TVittixDBGridColumnInfo;
begin
  Result := '';
  if not Assigned(AColumn) or not Assigned(FGrid) then Exit;

  Info := VittixGrid(FGrid).ColumnInfoByColumn(AColumn);
  if not Assigned(Info) then Exit;

  if Info.FooterText <> '' then
    Result := Info.FooterText
  else
    Result := AggregationCaption(Info.AggregationType);
end;

procedure TVittixDBGridFooterPanel.ClearAggregationAtClientX(X: Integer);
var
  Col: TColumn;
begin
  Col := HitTestColumn(X);
  if Assigned(Col) then
    ClearAggregationForColumn(Col);
end;

procedure TVittixDBGridFooterPanel.CopyAggregationAtClientX(X: Integer);
var
  Col: TColumn;
begin
  Col := HitTestColumn(X);
  if Assigned(Col) then
    CopyAggregationForColumn(Col);
end;

procedure TVittixDBGridFooterPanel.BuildPopup;
var
  Agg: TVittixAggregationType;
  Item: TMenuItem;
  Info: TVittixDBGridColumnInfo;
begin
  FreeAndNil(FPopup);
  FPopup := TPopupMenu.Create(Self);

  Info := VittixGrid(FGrid).ColumnInfoByColumn(FContextColumn);

  Item := TMenuItem.Create(FPopup);
  Item.Caption := '&Clear aggregation';
  Item.ShortCut := TextToShortCut('Del');
  Item.Tag := Ord(vatNone);
  Item.OnClick := PopupClearClick;
  FPopup.Items.Add(Item);

  Item := TMenuItem.Create(FPopup);
  Item.Caption := 'Clear &all aggregations';
  Item.ShortCut := TextToShortCut('Ctrl+Del');
  Item.OnClick := PopupClearAllClick;
  FPopup.Items.Add(Item);

  Item := TMenuItem.Create(FPopup);
  Item.Caption := '&Copy aggregation';
  Item.ShortCut := TextToShortCut('Ctrl+C');
  Item.OnClick := PopupCopyClick;
  FPopup.Items.Add(Item);

  Item := TMenuItem.Create(FPopup);
  Item.Caption := 'Copy &all aggregations';
  Item.ShortCut := TextToShortCut('Ctrl+Shift+C');
  Item.OnClick := PopupCopyAllClick;
  FPopup.Items.Add(Item);

  Item := TMenuItem.Create(FPopup);
  Item.Caption := 'Copy footer &summary';
  Item.ShortCut := TextToShortCut('Ctrl+Shift+F');
  Item.OnClick := PopupCopySummaryClick;
  FPopup.Items.Add(Item);

  Item := TMenuItem.Create(FPopup);
  Item.Caption := '-';
  FPopup.Items.Add(Item);

  for Agg := Low(TVittixAggregationType) to High(TVittixAggregationType) do
  begin
    Item := TMenuItem.Create(FPopup);
    Item.Caption := AggregationCaption(Agg);
    Item.Tag := Ord(Agg);
    Item.RadioItem := True;
    Item.GroupIndex := 1;
    Item.OnClick := PopupClick;

    if Assigned(Info) and (Info.AggregationType = Agg) then
      Item.Checked := True;

    FPopup.Items.Add(Item);
  end;
end;

function TVittixDBGridFooterPanel.GetPopupShortcutSummaryText: string;
begin
  Result := 'Clear aggregation=Del;Clear all aggregations=Ctrl+Del;Copy aggregation=Ctrl+C;Copy all aggregations=Ctrl+Shift+C;Copy footer summary=Ctrl+Shift+F';
end;

function TVittixDBGridFooterPanel.GetPopupCaptionSummaryText: string;
begin
  Result := '&Clear aggregation|Clear &all aggregations|&Copy aggregation|Copy &all aggregations|Copy footer &summary|-|Count|Sum|Average|Minimum|Maximum';
end;

procedure TVittixDBGridFooterPanel.PopupClearClick(Sender: TObject);
begin
  ClearAggregationForColumn(FContextColumn);
end;

procedure TVittixDBGridFooterPanel.PopupClearAllClick(Sender: TObject);
begin
  ClearAllAggregations;
end;

procedure TVittixDBGridFooterPanel.PopupCopyClick(Sender: TObject);
begin
  CopyAggregationForColumn(FContextColumn);
end;

procedure TVittixDBGridFooterPanel.PopupCopyAllClick(Sender: TObject);
begin
  CopyAllAggregations;
end;

procedure TVittixDBGridFooterPanel.PopupCopySummaryClick(Sender: TObject);
begin
  CopyFooterSummary;
end;

procedure TVittixDBGridFooterPanel.ClearAggregationForColumn(AColumn: TColumn);
var
  Info: TVittixDBGridColumnInfo;
begin
  if not Assigned(AColumn) then Exit;
  if not Assigned(FGrid) then Exit;

  Info := VittixGrid(FGrid).ColumnInfoByColumn(AColumn);
  if not Assigned(Info) then Exit;

  if Info.AggregationType <> vatNone then
  begin
    VittixGrid(FGrid).Controller.SetColumnAggregation(AColumn, vatNone);
    Invalidate;
    FGrid.Invalidate;
  end;
end;

procedure TVittixDBGridFooterPanel.ClearAllAggregations;
var
  I: Integer;
begin
  if not Assigned(FGrid) then Exit;

  for I := 0 to FGrid.Columns.Count - 1 do
    ClearAggregationForColumn(FGrid.Columns[I]);
end;

procedure TVittixDBGridFooterPanel.CopyAggregationForColumn(AColumn: TColumn);
var
  Text: string;
begin
  Text := GetAggregationTextForColumn(AColumn);
  if Text <> '' then
    VittixSetClipboardText(Text);
end;

procedure TVittixDBGridFooterPanel.CopyAllAggregations;
var
  I: Integer;
  Line: string;
  Lines: TStringList;
  Info: TVittixDBGridColumnInfo;
begin
  if not Assigned(FGrid) then Exit;

  Lines := TStringList.Create;
  try
    for I := 0 to FGrid.Columns.Count - 1 do
    begin
      Info := VittixGrid(FGrid).ColumnInfoByColumn(FGrid.Columns[I]);
      if not Assigned(Info) then
        Continue;

      Line := GetAggregationTextForColumn(FGrid.Columns[I]);
      if Line <> '' then
        Lines.Add(FGrid.Columns[I].Title.Caption + ': ' + Line);
    end;

    if Lines.Count > 0 then
      VittixSetClipboardText(TrimRight(Lines.Text));
  finally
    Lines.Free;
  end;
end;

procedure TVittixDBGridFooterPanel.CopyFooterSummary;
var
  I: Integer;
  Parts: TStringList;
  Col: TColumn;
  Text: string;
begin
  if not Assigned(FGrid) then Exit;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := #9;
    Parts.StrictDelimiter := True;

    for I := 0 to FGrid.Columns.Count - 1 do
    begin
      Col := FGrid.Columns[I];
      if not Col.Visible then
        Continue;

      Text := GetAggregationTextForColumn(Col);
      Parts.Add(Text);
    end;

    if Parts.Count > 0 then
      VittixSetClipboardText(Parts.DelimitedText);
  finally
    Parts.Free;
  end;
end;

procedure TVittixDBGridFooterPanel.PopupClick(Sender: TObject);
var
  Agg: TVittixAggregationType;
  Info: TVittixDBGridColumnInfo;
begin
  if not Assigned(FContextColumn) then Exit;

  Agg := TVittixAggregationType(TMenuItem(Sender).Tag);
  Info := VittixGrid(FGrid).ColumnInfoByColumn(FContextColumn);

  if Assigned(Info) then
  begin
    if Info.AggregationType <> Agg then
    begin
      VittixGrid(FGrid).Controller.SetColumnAggregation(FContextColumn, Agg);

      Invalidate;
      FGrid.Invalidate;
    end;
  end;
end;

{ --- Type-ahead search hint --- }

procedure TVittixDBGridFooterPanel.SetSearchHint(const AText: string);
begin
  if FSearchHint = AText then Exit;
  FSearchHint := AText;
  Invalidate;
end;

procedure TVittixDBGridFooterPanel.SetFilterSetHint(const AText: string);
begin
  if FFilterSetHint = AText then Exit;
  FFilterSetHint := AText;
  Invalidate;
end;

procedure TVittixDBGridFooterPanel.SetServerHint(const AText: string);
begin
  if FServerHint = AText then Exit;
  FServerHint := AText;
  Invalidate;
end;

{ --- Selection aggregates --- }

function TVittixDBGridFooterPanel.SelectionAggregateActive: Boolean;
begin
  Result := Assigned(FGrid) and (VittixGrid(FGrid) <> nil) and
    (dgMultiSelect in FGrid.Options) and
    Assigned(FGrid.DataSource) and
    Assigned(FGrid.DataSource.DataSet) and
    FGrid.DataSource.DataSet.Active and
    (FGrid.SelectedRows.Count > 1);
end;

procedure TVittixDBGridFooterPanel.AggregateSelectionValue(
  AField: TField; var Agg: TVittixAggregation);
var
  IntVal: Int64;
  FloatVal: Double;
  CurrVal: Currency;
  StrVal: string;
begin
  // Mirrors the aggregation engine's typed accumulators: integer, currency
  // and float sums keep their native types instead of collapsing to Double.
  case AField.DataType of
    ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc:
      begin
        IntVal := AField.AsLargeInt;
        Agg.HasInt := True;
        Agg.SumInt := Agg.SumInt + IntVal;
        if Agg.Count = 1 then
        begin
          Agg.MinInt := IntVal;
          Agg.MaxInt := IntVal;
        end
        else
        begin
          if IntVal < Agg.MinInt then Agg.MinInt := IntVal;
          if IntVal > Agg.MaxInt then Agg.MaxInt := IntVal;
        end;
      end;

    ftCurrency:
      begin
        CurrVal := AField.AsCurrency;
        Agg.HasCurrency := True;
        Agg.SumCurrency := Agg.SumCurrency + CurrVal;
        if Agg.Count = 1 then
        begin
          Agg.MinCurrency := CurrVal;
          Agg.MaxCurrency := CurrVal;
        end
        else
        begin
          if CurrVal < Agg.MinCurrency then Agg.MinCurrency := CurrVal;
          if CurrVal > Agg.MaxCurrency then Agg.MaxCurrency := CurrVal;
        end;
      end;

    ftFloat, ftBCD, ftFMTBcd:
      begin
        FloatVal := AField.AsFloat;
        Agg.HasFloat := True;
        Agg.SumFloat := Agg.SumFloat + FloatVal;
        if Agg.Count = 1 then
        begin
          Agg.MinFloat := FloatVal;
          Agg.MaxFloat := FloatVal;
        end
        else
        begin
          if FloatVal < Agg.MinFloat then Agg.MinFloat := FloatVal;
          if FloatVal > Agg.MaxFloat then Agg.MaxFloat := FloatVal;
        end;
      end;

    ftString, ftWideString, ftMemo, ftWideMemo, ftFixedChar, ftFixedWideChar:
      begin
        StrVal := AField.AsString;
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

procedure TVittixDBGridFooterPanel.ComputeSelectionAggregates;
var
  I, RowIndex: Integer;
  Dataset: TDataSet;
  Bookmark: TBookmark;
  Agg: TVittixAggregation;
begin
  SetLength(FSelectionFields, 0);
  SetLength(FSelectionAggregates, 0);
  if not SelectionAggregateActive then Exit;

  for I := 0 to FGrid.Columns.Count - 1 do
    if FGrid.Columns[I].Visible and Assigned(FGrid.Columns[I].Field) then
    begin
      SetLength(FSelectionFields, Length(FSelectionFields) + 1);
      SetLength(FSelectionAggregates, Length(FSelectionAggregates) + 1);
      FSelectionFields[High(FSelectionFields)] := FGrid.Columns[I].Field;
      FSelectionAggregates[High(FSelectionAggregates)].Clear;
    end;

  if Length(FSelectionFields) = 0 then Exit;

  Dataset := FGrid.DataSource.DataSet;
  Bookmark := nil;
  // Preserve the cursor and controls state across the selection walk.
  if not Dataset.IsEmpty then
    Bookmark := Dataset.GetBookmark;
  Dataset.DisableControls;
  try
    for RowIndex := 0 to FGrid.SelectedRows.Count - 1 do
    begin
      Dataset.GotoBookmark(FGrid.SelectedRows.Items[RowIndex]);
      for I := 0 to High(FSelectionFields) do
      begin
        Agg := FSelectionAggregates[I];
        Inc(Agg.Count);
        if FSelectionFields[I].IsNull then
          Inc(Agg.NullCount)
        else
          AggregateSelectionValue(FSelectionFields[I], Agg);
        FSelectionAggregates[I] := Agg;
      end;
    end;
  finally
    if Bookmark <> nil then
    begin
      try
        if Dataset.BookmarkValid(Bookmark) then
          Dataset.GotoBookmark(Bookmark);
      finally
        Dataset.FreeBookmark(Bookmark);
      end;
    end;
    Dataset.EnableControls;
  end;
end;

function TVittixDBGridFooterPanel.VisibleOrdinalOf(AColumn: TColumn): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not Assigned(AColumn) or not Assigned(FGrid) then Exit;
  for I := 0 to FGrid.Columns.Count - 1 do
  begin
    if not FGrid.Columns[I].Visible then Continue;
    Inc(Result);
    if FGrid.Columns[I] = AColumn then Exit;
  end;
  Result := -1;
end;

function TVittixDBGridFooterPanel.SelectionAggregateTextFor(
  AIndex: Integer): string;
var
  Agg: TVittixAggregation;
  Value: Variant;
begin
  Result := '';
  if (AIndex < 0) or (AIndex > High(FSelectionAggregates)) then Exit;

  Agg := FSelectionAggregates[AIndex];
  if Agg.Count = 0 then Exit;

  if FSelectionFields[AIndex].DataType in [ftSmallint, ftInteger, ftLargeint,
    ftWord, ftAutoInc, ftFloat, ftCurrency, ftBCD, ftFMTBcd] then
  begin
    Value := Agg.GetValue(vatSum);
    if VarIsNull(Value) then Exit('');
    Result := 'Sum: ' + FormatFloat('#,##0.00', Double(Value));
  end
  else
    Result := 'Count: ' + IntToStr(Agg.Count);
end;

function TVittixDBGridFooterPanel.GetSelectionAggregateText(
  AColumn: TColumn): string;
var
  Ordinal: Integer;
begin
  Result := '';
  if not SelectionAggregateActive then Exit;

  Ordinal := VisibleOrdinalOf(AColumn);
  if Ordinal < 0 then Exit;

  ComputeSelectionAggregates;
  Result := SelectionAggregateTextFor(Ordinal);
end;

end.
