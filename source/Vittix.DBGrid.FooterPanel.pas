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
  Vcl.Clipbrd,
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

    procedure BuildPopup;
    procedure PopupClick(Sender: TObject);
    procedure PopupClearClick(Sender: TObject);
    procedure PopupClearAllClick(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure PopupCopyAllClick(Sender: TObject);
    procedure PopupCopySummaryClick(Sender: TObject);
    function HitTestColumn(X: Integer): TColumn;
    function GetIndicatorOffset: Integer;
    function GetIndicatorRect: TRect;
    function GetColumnRect(AColumn: TColumn): TRect;
    function GetAggregationTextForColumn(AColumn: TColumn): string;
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
  end;

implementation

uses
  // Implementation-only on purpose: Vittix.DBGrid.Controller (which this
  // unit's interface is used by) requires TVittixDBGrid in its interface.
  Vittix.DBGrid;

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
        FCachedHeight := TM.tmHeight + TM.tmExternalLeading + 8;
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
begin
  if not Assigned(FGrid) or (VittixGrid(FGrid) = nil) then Exit;

  Canvas.Font.Assign(FGrid.Font);
  Canvas.Font.Style := Canvas.Font.Style + [fsBold];

  Canvas.Brush.Color := FGrid.FixedColor;
  Canvas.FillRect(ClientRect);

  // Draw the indicator/footer corner cell explicitly so the first data column
  // lines up visually with the footer grid. Without this, the indicator width
  // looks like part of the first column and makes the ID column appear missing.
  R := GetIndicatorRect;
  if not IsRectEmpty(R) then
  begin
    Canvas.Brush.Color := FGrid.FixedColor;
    Canvas.FillRect(R);

    Canvas.Pen.Color := clBtnHighlight;
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.LineTo(R.Right, R.Top);
    Canvas.MoveTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);

    Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(R.Right - 1, R.Top);
    Canvas.LineTo(R.Right - 1, R.Bottom);
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Right, R.Bottom - 1);
  end;

  StartCol := VittixGrid(FGrid).GetLeftCol;

  // Safety check for empty grid or invalid index
  if (StartCol < 0) or (StartCol >= FGrid.Columns.Count) then
    StartCol := 0;

  for I := StartCol to FGrid.Columns.Count - 1 do
  begin
    Col := FGrid.Columns[I];
    if not Col.Visible then Continue;

    R := GetColumnRect(Col);
    if IsRectEmpty(R) then
      Continue;

    // Background
    Canvas.Brush.Color := FGrid.FixedColor;
    Canvas.FillRect(R);

    // 3D Borders
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.LineTo(R.Right, R.Top);
    Canvas.MoveTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);

    Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(R.Right - 1, R.Top);
    Canvas.LineTo(R.Right - 1, R.Bottom);
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);

    // Text
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
end;

function TVittixDBGridFooterPanel.HitTestColumn(X: Integer): TColumn;
var
  I, StartCol: Integer;
  R: TRect;
begin
  Result := nil;
  if (FGrid = nil) or (VittixGrid(FGrid) = nil) then Exit;

  StartCol := VittixGrid(FGrid).GetLeftCol;

  if (StartCol < 0) or (StartCol >= FGrid.Columns.Count) then
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
    Info.AggregationType := vatNone;
    if Assigned(FAggregationEngine) then
      FAggregationEngine.Recalculate;
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
    Clipboard.AsText := Text;
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
      Clipboard.AsText := TrimRight(Lines.Text);
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
      Clipboard.AsText := Parts.DelimitedText;
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
      Info.AggregationType := Agg;

      if Assigned(FAggregationEngine) then
        FAggregationEngine.Recalculate;

      Invalidate;
      FGrid.Invalidate;
    end;
  end;
end;

end.
