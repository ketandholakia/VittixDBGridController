unit Vittix.DBGrid.FooterPanel;

{$REGION 'Documentation'}
/// <summary>
/// FIXED VERSION - Footer Panel for TVittixDBGrid
/// 
/// CRITICAL FIXES APPLIED:
/// 1. Removed TVittixGridHook class (duplicate WindowProc hook)
/// 2. Controller now handles all sync messages via its WindowProc
/// 3. Simplified Attach method
/// 4. Fixed integer overflow protection in Paint
///
/// The Controller's GridWindowProc now calls SyncLayout directly for all
/// necessary messages (WM_SIZE, WM_HSCROLL, etc.), eliminating the need
/// for a separate hook that was causing conflicts.
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
  TypInfo,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Aggregation.Engine;

type
  TVittixDBGridFooterPanel = class(TCustomControl)
  private
    FGrid: TVittixDBGrid;
    FAggregationEngine: TVittixDBGridAggregationEngine;
    FPopup: TPopupMenu;
    FContextColumn: TColumn;

    procedure BuildPopup;
    procedure PopupClick(Sender: TObject);
    function HitTestColumn(X: Integer): TColumn;
    function GetIndicatorOffset: Integer;
    function GetIndicatorRect: TRect;
    function GetColumnRect(AColumn: TColumn): TRect;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Attach(
      AGrid: TVittixDBGrid;
      AEngine: TVittixDBGridAggregationEngine
    );
    procedure SyncLayout;
  end;

implementation

type
  // Cracker class to access protected 'LeftCol' of TCustomGrid/TDBGrid
  TVittixGridAccess = class(TDBGrid);

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
  // FIX: Memory Leak - Popup must be freed
  FreeAndNil(FPopup);
  inherited;
end;

procedure TVittixDBGridFooterPanel.Attach(
  AGrid: TVittixDBGrid;
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
begin
  if not Assigned(FGrid) then Exit;

  // DESIGN-TIME SAFETY: Do not access GDI handles or ClientWidth in the IDE.
  if csDesigning in FGrid.ComponentState then Exit;

  DC := GetDC(0);
  try
    SelectObject(DC, FGrid.Font.Handle);
    GetTextMetrics(DC, TM);
    Height := TM.tmHeight + TM.tmExternalLeading + 8;
  finally
    ReleaseDC(0, DC);
  end;

  Left := FGrid.Left;
  Top := FGrid.Top + FGrid.Height - Height;
  Width := FGrid.Width;
  Invalidate;
end;

function TVittixDBGridFooterPanel.GetIndicatorOffset: Integer;
begin
  Result := 0;
  if Assigned(FGrid) then
    // Use the public helper we added to TVittixDBGrid
    Result := FGrid.GetIndicatorWidth;
end;

function TVittixDBGridFooterPanel.GetIndicatorRect: TRect;
var
  GridRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not Assigned(FGrid) then
    Exit;

  if GetIndicatorOffset <= 0 then
    Exit;

  GridRect := TVittixGridAccess(FGrid).CellRect(0, 1);
  Result := Rect(GridRect.Left, 0, GridRect.Right, Height);
end;

function TVittixDBGridFooterPanel.GetColumnRect(AColumn: TColumn): TRect;
var
  GridRect: TRect;
  VisibleIndex: Integer;
  I: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if not Assigned(FGrid) or not Assigned(AColumn) then
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
  GridRect := TVittixGridAccess(FGrid).CellRect(
    VisibleIndex + TVittixGridAccess(FGrid).IndicatorOffset,
    1
  );
  Result := Rect(
    GridRect.Left,
    0,
    GridRect.Right,
    Height
  );
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
  if not Assigned(FGrid) then Exit;

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

  // FIX: Access protected LeftCol using the cracker class
  StartCol := TVittixGridAccess(FGrid).LeftCol;

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
    Canvas.LineTo(R.Right, R.Bottom - 1);

    // Text
    Info := FGrid.ColumnInfoByColumn(Col);
    if Assigned(Info) and Assigned(FAggregationEngine) then
      Text := FAggregationEngine.GetAggregationDisplayText(Info)
    else
      Text := '';

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
  if not Assigned(FGrid) then Exit;

  // FIX: Access protected LeftCol using the cracker class
  StartCol := TVittixGridAccess(FGrid).LeftCol;

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

procedure TVittixDBGridFooterPanel.BuildPopup;
var
  Agg: TVittixAggregationType;
  Item: TMenuItem;
  Info: TVittixDBGridColumnInfo;
begin
  FreeAndNil(FPopup);
  FPopup := TPopupMenu.Create(Self);

  Info := FGrid.ColumnInfoByColumn(FContextColumn);

  for Agg := Low(TVittixAggregationType) to High(TVittixAggregationType) do
  begin
    Item := TMenuItem.Create(FPopup);
    Item.Caption := GetEnumName(TypeInfo(TVittixAggregationType), Ord(Agg));
    Item.Tag := Ord(Agg);
    Item.RadioItem := True;
    Item.GroupIndex := 1;
    Item.OnClick := PopupClick;

    if Assigned(Info) and (Info.AggregationType = Agg) then
      Item.Checked := True;

    FPopup.Items.Add(Item);
  end;
end;

procedure TVittixDBGridFooterPanel.PopupClick(Sender: TObject);
var
  Agg: TVittixAggregationType;
  Info: TVittixDBGridColumnInfo;
begin
  if not Assigned(FContextColumn) then Exit;

  Agg := TVittixAggregationType(TMenuItem(Sender).Tag);
  Info := FGrid.ColumnInfoByColumn(FContextColumn);

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
