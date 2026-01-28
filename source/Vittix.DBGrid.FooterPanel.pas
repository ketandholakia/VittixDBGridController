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

  Parent := AGrid.Parent;
  Align := alBottom;

  // FIX: No longer creates TVittixGridHook - Controller handles sync now
  SyncLayout;
end;

procedure TVittixDBGridFooterPanel.SyncLayout;
var
  TM: TTextMetric;
  DC: HDC;
begin
  if not Assigned(FGrid) then Exit;

  DC := GetDC(0);
  try
    SelectObject(DC, FGrid.Font.Handle);
    GetTextMetrics(DC, TM);
    Height := TM.tmHeight + TM.tmExternalLeading + 8;
  finally
    ReleaseDC(0, DC);
  end;

  Width := FGrid.ClientWidth;
  Invalidate;
end;

function TVittixDBGridFooterPanel.GetIndicatorOffset: Integer;
begin
  Result := 0;
  if Assigned(FGrid) then
    // Use the public helper we added to TVittixDBGrid
    Result := FGrid.GetIndicatorWidth;
end;

procedure TVittixDBGridFooterPanel.Paint;
var
  I: Integer;
  R: TRect;
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
  Text: string;
  X: Integer;
  DrawFlags: Cardinal;
  StartCol: Integer;
begin
  if not Assigned(FGrid) then Exit;

  Canvas.Font.Assign(FGrid.Font);
  Canvas.Font.Style := Canvas.Font.Style + [fsBold];

  Canvas.Brush.Color := FGrid.FixedColor;
  Canvas.FillRect(ClientRect);

  X := GetIndicatorOffset;

  // FIX: Access protected LeftCol using the cracker class
  StartCol := TVittixGridAccess(FGrid).LeftCol;

  // Safety check for empty grid or invalid index
  if (StartCol < 0) or (StartCol >= FGrid.Columns.Count) then
    StartCol := 0;

  for I := StartCol to FGrid.Columns.Count - 1 do
  begin
    Col := FGrid.Columns[I];
    if not Col.Visible then Continue;

    R := Rect(X, 0, X + Col.Width, Height);

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

    // FIX: CRITICAL ISSUE #3 - Prevent integer overflow
    if (MaxInt - Col.Width < X) then Break;
    Inc(X, Col.Width);
    if X > ClientWidth then Break;
  end;
end;

function TVittixDBGridFooterPanel.HitTestColumn(X: Integer): TColumn;
var
  I, PosX, StartCol: Integer;
begin
  Result := nil;
  if not Assigned(FGrid) then Exit;

  PosX := GetIndicatorOffset;

  // FIX: Access protected LeftCol using the cracker class
  StartCol := TVittixGridAccess(FGrid).LeftCol;

  if (StartCol < 0) or (StartCol >= FGrid.Columns.Count) then
    StartCol := 0;

  for I := StartCol to FGrid.Columns.Count - 1 do
  begin
    if not FGrid.Columns[I].Visible then Continue;

    if (X >= PosX) and (X < PosX + FGrid.Columns[I].Width) then
      Exit(FGrid.Columns[I]);

    Inc(PosX, FGrid.Columns[I].Width);
    if PosX > ClientWidth then Break;
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
