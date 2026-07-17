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
  Vittix.DBGrid.ColumnChooser,
  Vittix.DBGrid.Editors,
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
    FOldDblClick: TNotifyEvent;
    FOldKeyDown: TKeyEvent;
    FOldWindowProc: TWndMethod;
    FOldAfterOpen, FOldAfterClose, FOldAfterScroll,
    FOldAfterPost, FOldAfterDelete: TDataSetNotifyEvent;

    // Internal helpers
    function IsReady: Boolean;
    function FindInfoByColumn(AColumn: TColumn): TVittixDBGridColumnInfo;
    function FindColumnByField(AField: TField): TColumn;

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
    procedure GridDblClick(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

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
    procedure InstallWindowProc;

    procedure Refresh;
    procedure Clear;
    procedure ApplyState;
    procedure ShowColumnChooser;
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

procedure TVittixDBGridController.InstallWindowProc;
begin
  // Called from TVittixDBGrid.CreateWnd — only install if we're hooked
  // but the WindowProc was skipped because no handle existed yet.
  if not Assigned(FGrid) then Exit;
  if csDesigning in FGrid.ComponentState then Exit;
  if not FGrid.HandleAllocated then Exit;
  if not FEnginesCreated then Exit; // Only install if fully hooked

  // Only install if not already installed.
  // Compare the stored old proc: if FOldWindowProc is nil we haven't hooked yet.
  if not Assigned(FOldWindowProc) then
  begin
    FOldWindowProc := FGrid.WindowProc;
    FGrid.WindowProc := GridWindowProc;
  end;
end;

procedure TVittixDBGridController.Loaded;
begin
  inherited;
  // DESIGN-TIME SAFETY: Do not hook anything while the IDE is loading.
  if csDesigning in ComponentState then Exit;

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
  // DESIGN-TIME SAFETY: Never touch datasets or engines in the IDE.
  if Assigned(FGrid) and (csDesigning in FGrid.ComponentState) then Exit;

  // Re-entrance guard
  if FUpdating then Exit;
  FUpdating := True;
  try
    DestroyEngines;
    UnhookDataSource;
    HookDataSource;

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

  UnhookGrid;
  FGrid := Value;

  if not Assigned(FGrid) then Exit;

  // PRIMARY GATE: csDesigning is set by Delphi 12.2 before any install
  // callback fires. Also block during streaming (csLoading).
  if csDesigning in FGrid.ComponentState then Exit;
  if csLoading in FGrid.ComponentState then Exit;

  if FActive then
    HookGrid;
end;

procedure TVittixDBGridController.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;
  FActive := Value;

  // DESIGN-TIME SAFETY
  if Assigned(FGrid) and (csDesigning in FGrid.ComponentState) then Exit;

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

  // DESIGN-TIME SAFETY: Never create visual controls at design time.
  if csDesigning in FGrid.ComponentState then Exit;

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

  // PRIMARY GATE: In Delphi 12.2 the IDE sets csDesigning reliably before
  // any component editor or package install callback fires. This is the
  // correct check for all design-time protection.
  if csDesigning in FGrid.ComponentState then Exit;

  // Secondary runtime check: never hook events when not fully constructed.
  if csLoading in FGrid.ComponentState then Exit;

  FOldTitleClick := FGrid.OnTitleClick;
  FGrid.OnTitleClick := GridTitleClick;

  FOldDrawColumnCell := FGrid.OnDrawColumnCell;
  FGrid.OnDrawColumnCell := GridDrawColumnCell;

  FOldMouseDown := FGrid.OnMouseDown;
  FGrid.OnMouseDown := GridMouseDown;

  FOldDblClick := FGrid.OnDblClick;
  FGrid.OnDblClick := GridDblClick;

  FOldKeyDown := FGrid.OnKeyDown;
  FGrid.OnKeyDown := GridKeyDown;

  // WindowProc hooking requires an actual Win32 handle.
  // Only install it if one exists — it will be reinstalled via Loaded otherwise.
  if FGrid.HandleAllocated then
  begin
    FOldWindowProc := FGrid.WindowProc;
    FGrid.WindowProc := GridWindowProc;
  end;

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
  FGrid.OnDblClick := FOldDblClick;
  FGrid.OnKeyDown := FOldKeyDown;
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
    if ssCtrl in Shift then
    begin
      ShowColumnChooser;
      Exit;
    end;

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

procedure TVittixDBGridController.GridDblClick(Sender: TObject);
var
  Field: TField;
  Column: TColumn;
begin
  Field := nil;
  if Assigned(FGrid) then
    Field := FGrid.SelectedField;
  Column := FindColumnByField(Field);

  if Assigned(Column) and Assigned(Field) and
     (Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo, ftDate, ftTime, ftDateTime]) and
     TVittixDBGridEditors.EditField(FGrid, Column) then
  begin
    Refresh;
    Exit;
  end;

  if Assigned(FOldDblClick) then
    FOldDblClick(Sender);
end;

procedure TVittixDBGridController.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Field: TField;
  Column: TColumn;
begin
  if Key = VK_F2 then
  begin
    Field := nil;
    if Assigned(FGrid) then
      Field := FGrid.SelectedField;
    Column := FindColumnByField(Field);

    if Assigned(Column) and Assigned(Field) and
       (Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo, ftDate, ftTime, ftDateTime]) and
       TVittixDBGridEditors.EditField(FGrid, Column) then
    begin
      Refresh;
      Key := 0;
      Exit;
    end;
  end;

  if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

{ ============================================================================= }
{ DATASET EVENTS }
{ ============================================================================= }

procedure TVittixDBGridController.DatasetAfterOpen(DataSet: TDataSet);
begin
  CreateEngines;
  Refresh;

  if Assigned(FOldAfterOpen) then
    FOldAfterOpen(DataSet);
end;

procedure TVittixDBGridController.DatasetAfterClose(DataSet: TDataSet);
begin
  DestroyEngines;

  if Assigned(FOldAfterClose) then
    FOldAfterClose(DataSet);
end;

procedure TVittixDBGridController.DatasetAfterScroll(DataSet: TDataSet);
begin
  if Assigned(FGrid) then
    FGrid.Invalidate;

  if Assigned(FOldAfterScroll) then
    FOldAfterScroll(DataSet);
end;

procedure TVittixDBGridController.DatasetAfterPost(DataSet: TDataSet);
begin
  SetAggregationDirty;
  Refresh;

  if Assigned(FOldAfterPost) then
    FOldAfterPost(DataSet);
end;

procedure TVittixDBGridController.DatasetAfterDelete(DataSet: TDataSet);
begin
  SetAggregationDirty;
  Refresh;

  if Assigned(FOldAfterDelete) then
    FOldAfterDelete(DataSet);
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

function TVittixDBGridController.FindColumnByField(AField: TField): TColumn;
var
  I: Integer;
begin
  Result := nil;
  if not Assigned(FGrid) or not Assigned(AField) then
    Exit;

  for I := 0 to FGrid.Columns.Count - 1 do
    if FGrid.Columns[I].Field = AField then
      Exit(FGrid.Columns[I]);
end;

procedure TVittixDBGridController.ShowColumnChooser;
begin
  if not Assigned(FGrid) then
    Exit;

  if TVittixDBGridColumnChooserForm.Execute(FGrid) then
  begin
    SetAggregationDirty;
    Refresh;
  end;
end;

initialization
  System.Classes.RegisterClass(TVittixDBGridController);

finalization
  System.Classes.UnRegisterClass(TVittixDBGridController);

end.
