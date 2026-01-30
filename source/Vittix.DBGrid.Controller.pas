unit Vittix.DBGrid.Controller;

{$REGION 'Documentation'}
/// <summary>
/// FIXED VERSION - Controller for TVittixDBGrid
///
/// CRITICAL FIXES APPLIED:
/// 1. Enhanced WindowProc hook to handle all footer sync messages
/// 2. Removed need for duplicate TVittixGridHook in FooterPanel
/// 3. Added proper notification forwarding for DataSource changes
/// 4. Added re-entrance protection in engine operations
///
/// THREAD SAFETY: Not thread-safe. Must be used from main VCL thread only.
/// </summary>
{$ENDREGION}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.Math,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.Forms,
  Data.DB,

  // Vittix
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Sort.Engine,
  Vittix.DBGrid.Filter.Engine,
  Vittix.DBGrid.Aggregation.Engine,
  Vittix.DBGrid.Filter.Popup,
  Vittix.DBGrid.FooterPanel;

const
  DEFAULT_ALTERNATE_ROW_COLOR = $00F7F7F7;
  WM_VITTIX_UPDATE_FIXEDROWS = WM_USER + 1001;

type
  TVittixGridHelper = class(TCustomDBGrid);

  TVittixDBGridController = class(TComponent)
  private
    FGrid: TVittixDBGrid;
    FDataset: TDataSet;

    FActive: Boolean;
    FShowFooter: Boolean;
    FAutoRefresh: Boolean;
    FUpdating: Boolean;  // NEW: Re-entrance guard

    FAlternatingRowColors: Boolean;
    FAlternateRowColor: TColor;

    // Engines (logic only)
    FSortEngine: TVittixDBGridSortEngine;
    FFilterEngine: TVittixDBGridFilterEngine;
    FAggregationEngine: TVittixDBGridAggregationEngine;
    FAggregationDirty: Boolean;
    FEnginesCreated: Boolean;
    FFooterPanel: TVittixDBGridFooterPanel;

    // Event hooks
    FOldTitleClick: TDBGridClickEvent;
    FOldDrawColumnCell: TDrawColumnCellEvent;
    FOldMouseDown: TMouseEvent;
    FOldWindowProc: TWndMethod;
    FOldAfterOpen, FOldAfterClose, FOldAfterScroll,
    FOldAfterPost, FOldAfterDelete: TDataSetNotifyEvent;

    // Internal helpers
    function IsReady: Boolean;
    function FindInfoByColumn(AColumn: TColumn): TVittixDBGridColumnInfo;

    procedure SetGrid(const Value: TVittixDBGrid);
    procedure SetActive(const Value: Boolean);
    procedure SetShowFooter(const Value: Boolean);

    procedure HookGrid;
    procedure UnhookGrid;
    procedure HookDataSource;
    procedure UnhookDataSource;

    procedure CreateEngines;
    procedure DestroyEngines;

    procedure GridWindowProc(var Message: TMessage);

    // Grid events
    procedure GridTitleClick(Column: TColumn);
    procedure GridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    // Dataset events
    procedure DatasetAfterOpen(DataSet: TDataSet);
    procedure DatasetAfterClose(DataSet: TDataSet);
    procedure DatasetAfterScroll(DataSet: TDataSet);
    procedure DatasetAfterPost(DataSet: TDataSet);
    procedure DatasetAfterDelete(DataSet: TDataSet);

    procedure SetAggregationDirty;

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Detach;

    procedure Refresh;
    procedure Clear;
    procedure ApplyState;
    procedure SetGlobalFilter(const Text: string);
    procedure ClearFilters;
    procedure SetColumnAggregation(Column: TColumn; Aggregation: TVittixAggregationType);

    // Called by TVittixDBGrid when its DataSource property changes
    procedure DataSourceChanged;

    property Grid: TVittixDBGrid read FGrid write SetGrid;

  published
    property Active: Boolean read FActive write SetActive default True;
    property ShowFooter: Boolean read FShowFooter write SetShowFooter default True;
    property AutoRefresh: Boolean read FAutoRefresh write FAutoRefresh default True;

    property AlternatingRowColors: Boolean
      read FAlternatingRowColors write FAlternatingRowColors default True;

    property AlternateRowColor: TColor
      read FAlternateRowColor write FAlternateRowColor
      default DEFAULT_ALTERNATE_ROW_COLOR;
  end;

implementation

procedure DebugMsg(const S: string);
begin
  // OutputDebugString(PChar('[Vittix] ' + S));
end;

{ ============================================================================= }
{ LIFECYCLE }
{ ============================================================================= }

constructor TVittixDBGridController.Create(AOwner: TComponent);
begin
  inherited;

  FActive := True;
  FShowFooter := True;
  FAutoRefresh := True;
  FAlternatingRowColors := True;
  FAlternateRowColor := DEFAULT_ALTERNATE_ROW_COLOR;
  FUpdating := False;

  FAggregationDirty := True;
end;

destructor TVittixDBGridController.Destroy;
begin
  UnhookGrid;
  DestroyEngines;
  inherited;
end;

procedure TVittixDBGridController.Detach;
begin
  UnhookGrid;
end;

procedure TVittixDBGridController.Loaded;
begin
  inherited;
  if FActive and Assigned(FGrid) then
    HookGrid;
end;

procedure TVittixDBGridController.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FGrid then
      SetGrid(nil)
    else if AComponent = FDataset then
      UnhookDataSource // FIX: Semicolon removed here
    else if AComponent = FFooterPanel then
      FFooterPanel := nil;
  end;
end;

{ ============================================================================= }
{ DATA SOURCE CHANGE HANDLER }
{ ============================================================================= }

procedure TVittixDBGridController.DataSourceChanged;
begin
  // FIX: Add re-entrance guard
  if FUpdating then Exit;
  FUpdating := True;
  try
    // Fully reset the data connection and engines.
    DestroyEngines;
    UnhookDataSource;
    HookDataSource;

    // If the new dataset is already active, force engine creation immediately.
    if Assigned(FDataset) and FDataset.Active then
    begin
      CreateEngines;
      Refresh;
    end;
  finally
    FUpdating := False;
  end;
end;

{ ============================================================================= }
{ GRID / DATASET HOOKING }
{ ============================================================================= }

procedure TVittixDBGridController.SetGrid(const Value: TVittixDBGrid);
begin
  if FGrid = Value then Exit;

  // Detach from old grid
  UnhookGrid;

  FGrid := Value;

  // Attach to new grid (if not nil)
  if Assigned(FGrid) and FActive then
    HookGrid;
end;

procedure TVittixDBGridController.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;
  FActive := Value;

  if FActive then
    HookGrid
  else
    UnhookGrid;
end;

procedure TVittixDBGridController.SetShowFooter(const Value: Boolean);
begin
  if FShowFooter = Value then Exit;
  FShowFooter := Value;

  if not Assigned(FGrid) then Exit;

  if FShowFooter then
  begin
    if Assigned(FAggregationEngine) and not Assigned(FFooterPanel) then
    begin
      FFooterPanel := TVittixDBGridFooterPanel.Create(Self);
      FFooterPanel.Attach(FGrid, FAggregationEngine);
    end;
  end
  else
    FreeAndNil(FFooterPanel);
end;

procedure TVittixDBGridController.HookGrid;
begin
  if not Assigned(FGrid) then Exit;

  FOldTitleClick := FGrid.OnTitleClick;
  FGrid.OnTitleClick := GridTitleClick;

  FOldDrawColumnCell := FGrid.OnDrawColumnCell;
  FGrid.OnDrawColumnCell := GridDrawColumnCell;

  FOldMouseDown := FGrid.OnMouseDown;
  FGrid.OnMouseDown := GridMouseDown;

  FOldWindowProc := FGrid.WindowProc;
  FGrid.WindowProc := GridWindowProc;

  HookDataSource;
  CreateEngines;
end;

procedure TVittixDBGridController.UnhookGrid;
begin
  UnhookDataSource;

  if not Assigned(FGrid) then Exit;

  FGrid.OnTitleClick := FOldTitleClick;
  FGrid.OnDrawColumnCell := FOldDrawColumnCell;
  FGrid.OnMouseDown := FOldMouseDown;
  FGrid.WindowProc := FOldWindowProc;
end;

procedure TVittixDBGridController.HookDataSource;
begin
  if not Assigned(FGrid) or not Assigned(FGrid.DataSource) then Exit;

  FDataset := FGrid.DataSource.DataSet;
  if not Assigned(FDataset) then Exit;

  FOldAfterOpen := FDataset.AfterOpen;
  FOldAfterClose := FDataset.AfterClose;
  FOldAfterScroll := FDataset.AfterScroll;
  FOldAfterPost := FDataset.AfterPost;
  FOldAfterDelete := FDataset.AfterDelete;

  FDataset.AfterOpen := DatasetAfterOpen;
  FDataset.AfterClose := DatasetAfterClose;
  FDataset.AfterScroll := DatasetAfterScroll;
  FDataset.AfterPost := DatasetAfterPost;
  FDataset.AfterDelete := DatasetAfterDelete;

  // FIX: Force initialization if dataset is already open
  if FDataset.Active then
    DatasetAfterOpen(FDataset);
end;

procedure TVittixDBGridController.UnhookDataSource;
begin
  if not Assigned(FDataset) then Exit;

  FDataset.AfterOpen := FOldAfterOpen;
  FDataset.AfterClose := FOldAfterClose;
  FDataset.AfterScroll := FOldAfterScroll;
  FDataset.AfterPost := FOldAfterPost;
  FDataset.AfterDelete := FOldAfterDelete;

  FDataset := nil;
end;

{ ============================================================================= }
{ ENGINES }
{ ============================================================================= }

procedure TVittixDBGridController.CreateEngines;
begin
  if FEnginesCreated or not IsReady then Exit;

  FSortEngine :=
    TVittixDBGridSortEngine.Create(FDataset, FGrid.ColumnInfo);

  FFilterEngine :=
    TVittixDBGridFilterEngine.Create(FDataset, FGrid.ColumnInfo);

  FAggregationEngine :=
    TVittixDBGridAggregationEngine.Create(FDataset, FGrid.ColumnInfo);

  FAggregationEngine.OnAcceptRecord :=
    function: Boolean
    begin
      Result := not Assigned(FFilterEngine) or
                FFilterEngine.AcceptCurrentRecord;
    end;

  // Create Footer Panel if requested
  if FShowFooter and not Assigned(FFooterPanel) then
  begin
    FFooterPanel := TVittixDBGridFooterPanel.Create(Self);
    FFooterPanel.Attach(FGrid, FAggregationEngine);
  end;

  FEnginesCreated := True;
  SetAggregationDirty;
  Refresh;
end;

procedure TVittixDBGridController.DestroyEngines;
begin
  FreeAndNil(FFooterPanel);
  FreeAndNil(FSortEngine);
  FreeAndNil(FFilterEngine);
  FreeAndNil(FAggregationEngine);
  FEnginesCreated := False;
end;

{ ============================================================================= }
{ DRAWING }
{ ============================================================================= }

procedure TVittixDBGridController.GridWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_VITTIX_UPDATE_FIXEDROWS then
  begin
    Exit;
  end;

  // Call original window proc first
  if Assigned(FOldWindowProc) then
    FOldWindowProc(Message);

  // FIX: CRITICAL ISSUE #1 - Enhanced footer sync to eliminate duplicate hook
  // This replaces the separate TVittixGridHook that was causing hook collision
  // Now handles all necessary messages for proper footer synchronization
  if (Message.Msg = WM_PAINT) or (Message.Msg = WM_SIZE) or
     (Message.Msg = WM_HSCROLL) or (Message.Msg = WM_VSCROLL) or
     (Message.Msg = WM_WINDOWPOSCHANGED) or
     (Message.Msg = CM_FONTCHANGED) or (Message.Msg = CM_VISIBLECHANGED) then
  begin
    if Assigned(FFooterPanel) then
      FFooterPanel.SyncLayout;
  end;
end;

procedure TVittixDBGridController.GridDrawColumnCell(
  Sender: TObject; const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var  IsOddRow: Boolean;
begin
  // Check if we should apply the alternate color
  // We skip:
  // 1. Selected rows (let them be blue/highlighted)
  // 2. Fixed rows (headers)
  if FAlternatingRowColors and
     not (gdSelected in State) and
     not (gdFixed in State) then
  begin
    // Check Dataset Record Number
    if Assigned(FGrid.DataSource) and Assigned(FGrid.DataSource.DataSet) then
    begin
      // Odd returns True for 1, 3, 5...
      // RecNo is usually 1-based.
      IsOddRow := Odd(FGrid.DataSource.DataSet.RecNo);

      if IsOddRow then
        FGrid.Canvas.Brush.Color := FAlternateRowColor;
    end;
  end;

  // Now call the default drawing.
  // It will use the Brush.Color we just set for the background.
  if Assigned(FOldDrawColumnCell) then
    FOldDrawColumnCell(Sender, Rect, DataCol, Column, State)
  else
    FGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

{ ============================================================================= }
{ GRID EVENTS }
{ ============================================================================= }

procedure TVittixDBGridController.GridTitleClick(Column: TColumn);
begin
  if Assigned(FSortEngine) then
  begin
    FSortEngine.ToggleSort(
      Column,
      (GetKeyState(VK_CONTROL) and $8000) <> 0
    );
    SetAggregationDirty;
    Refresh;
  end;

  if Assigned(FOldTitleClick) then
    FOldTitleClick(Column);
end;

procedure TVittixDBGridController.GridMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  G: TVittixGridHelper;
  Coord: TGridCoord;
  ColIndex: Integer;
  Col: TColumn;
begin
  if not Assigned(FGrid) then Exit;

  G := TVittixGridHelper(FGrid);
  Coord := FGrid.MouseCoord(X, Y);

  // Title click for filter
  if (Coord.Y = 0) and (Button = mbRight) then
  begin
    ColIndex := Coord.X - G.IndicatorOffset;
    if (ColIndex >= 0) and (ColIndex < FGrid.Columns.Count) then
    begin
      Col := FGrid.Columns[ColIndex];
      if Assigned(Col) and
         TVittixDBGridFilterPopup.Execute(
           FGrid, FindInfoByColumn(Col)) then
      begin
        FFilterEngine.Active := True;
        SetAggregationDirty;
        Refresh;
      end;
      Exit;
    end;
  end;

  if Assigned(FOldMouseDown) then
    FOldMouseDown(Sender, Button, Shift, X, Y);
end;

{ ============================================================================= }
{ DATASET EVENTS }
{ ============================================================================= }

procedure TVittixDBGridController.DatasetAfterOpen(DataSet: TDataSet);
begin
  CreateEngines;
  Refresh;
end;

procedure TVittixDBGridController.DatasetAfterClose(DataSet: TDataSet);
begin
  DestroyEngines;
end;

procedure TVittixDBGridController.DatasetAfterScroll(DataSet: TDataSet);
begin
  if Assigned(FGrid) then
    FGrid.Invalidate;
end;

procedure TVittixDBGridController.DatasetAfterPost(DataSet: TDataSet);
begin
  SetAggregationDirty;
  Refresh;
end;

procedure TVittixDBGridController.DatasetAfterDelete(DataSet: TDataSet);
begin
  SetAggregationDirty;
  Refresh;
end;

{ ============================================================================= }
{ PUBLIC API }
{ ============================================================================= }

procedure TVittixDBGridController.SetAggregationDirty;
begin
  FAggregationDirty := True;
end;

procedure TVittixDBGridController.Refresh;
begin
  // FIX: Add re-entrance guard for refresh operations
  if FUpdating then Exit;

  if FAggregationDirty and Assigned(FAggregationEngine) then
  begin
    FAggregationEngine.Recalculate;
    FAggregationDirty := False;
  end;

  if Assigned(FGrid) then
    FGrid.Invalidate;
end;

procedure TVittixDBGridController.Clear;
begin
  ClearFilters;
  if Assigned(FSortEngine) then
    FSortEngine.ClearSorting;
  SetAggregationDirty;
  Refresh;
end;

procedure TVittixDBGridController.ApplyState;
begin
  if Assigned(FSortEngine) then
    FSortEngine.ApplySorting;
  SetAggregationDirty;
  Refresh;
end;

procedure TVittixDBGridController.SetGlobalFilter(const Text: string);
begin
  if Assigned(FFilterEngine) then
  begin
    FFilterEngine.GlobalSearchText := Text;
    FFilterEngine.Active := Text <> '';
    SetAggregationDirty;
    Refresh;
  end;
end;

procedure TVittixDBGridController.ClearFilters;
begin
  if Assigned(FFilterEngine) then
  begin
    FFilterEngine.Clear;
    SetAggregationDirty;
    Refresh;
  end;
end;

procedure TVittixDBGridController.SetColumnAggregation(
  Column: TColumn; Aggregation: TVittixAggregationType);
var
  Info: TVittixDBGridColumnInfo;
begin
  Info := FindInfoByColumn(Column);
  if Assigned(Info) then
  begin
    Info.AggregationType := Aggregation;
    SetAggregationDirty;
    Refresh;
  end;
end;

{ ============================================================================= }
{ HELPERS }
{ ============================================================================= }

function TVittixDBGridController.IsReady: Boolean;
begin
  Result :=
    FActive and
    Assigned(FGrid) and
    Assigned(FGrid.DataSource) and
    Assigned(FGrid.DataSource.DataSet) and
    FGrid.DataSource.DataSet.Active;
end;

function TVittixDBGridController.FindInfoByColumn(
  AColumn: TColumn): TVittixDBGridColumnInfo;
var
  I: Integer;
begin
  Result := nil;
  if not Assigned(FGrid) or not Assigned(AColumn) then Exit;

  if AColumn.FieldName = '' then Exit;

  for I := 0 to FGrid.ColumnInfo.Count - 1 do
    if SameText(FGrid.ColumnInfo[I].FieldName, AColumn.FieldName) then
      Exit(FGrid.ColumnInfo[I]);
end;

initialization
  System.Classes.RegisterClass(TVittixDBGridController);

finalization
  System.Classes.UnRegisterClass(TVittixDBGridController);

end.
