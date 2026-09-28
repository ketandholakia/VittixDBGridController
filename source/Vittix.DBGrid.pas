unit Vittix.DBGrid;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.Graphics, // Needed for TColor
  Vcl.Controls,
  Data.DB,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Controller;

type
  TVittixDBGridPersistenceSettings = record
    LayoutStorageFileName: string;
    ChooserStateFileName: string;
    FilterHistoryFileName: string;
    PersistenceRootPath: string;
  end;

  TVittixDBGrid = class(TDBGrid)
  private
    FColumnsInfo: TVittixDBGridColumns;
    FController: TVittixDBGridController;
    FPersistence: TVittixDBGridPersistenceSettings;
    procedure SyncColumnInfo;
    function GetFooterVisible: Boolean;
    procedure SetFooterVisible(const Value: Boolean);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);

    // Delegated to the controller (single source of truth)
    function GetAlternatingRowColors: Boolean;
    procedure SetAlternatingRowColors(const Value: Boolean);
    function GetAlternateRowColor: TColor;
    procedure SetAlternateRowColor(const Value: TColor);
    procedure SetLayoutStorageFileName(const Value: string);
    procedure SetChooserStateFileName(const Value: string);
    procedure SetFilterHistoryFileName(const Value: string);
    procedure SetPersistenceRootPath(const Value: string);

    // Notification events delegate to the controller's storage so assigning
    // through the grid or the controller reaches the same handlers and an
    // operation fires exactly once.
    function GetOnAfterSort: TVittixAfterSortEvent;
    procedure SetOnAfterSort(const Value: TVittixAfterSortEvent);
    function GetOnSortError: TVittixSortErrorEvent;
    procedure SetOnSortError(const Value: TVittixSortErrorEvent);
    function GetOnFilterApplied: TVittixFilterAppliedEvent;
    procedure SetOnFilterApplied(const Value: TVittixFilterAppliedEvent);
    function GetOnAfterApplyLayout: TNotifyEvent;
    procedure SetOnAfterApplyLayout(const Value: TNotifyEvent);
    function GetOnColumnMoved: TVittixColumnMovedEvent;
    procedure SetOnColumnMoved(const Value: TVittixColumnMovedEvent);

  protected
    procedure Loaded; override;
    procedure LayoutChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateWnd; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;

    // Controller integration via virtual overrides instead of event-hooking:
    // application code can freely assign OnTitleClick/OnDrawColumnCell/etc.
    // without silently unhooking sorting, colors, or the filter popup.
    procedure TitleClick(Column: TColumn); override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function ColumnInfoByColumn(Column: TColumn): TVittixDBGridColumnInfo;
    function GetIndicatorWidth: Integer;

    /// <summary>Protected on TCustomDBGrid — exposes the indicator column
    /// offset (0 or 1) without needing a cracker class.</summary>
    function GetIndicatorOffset: Integer;

    /// <summary>Protected on TCustomGrid — exposes the first visible column
    /// index without needing a cracker class.</summary>
    function GetLeftCol: Integer;

    /// <summary>Protected on TCustomGrid — exposes cell pixel geometry
    /// without needing a cracker class.</summary>
    function GetCellRect(ACol, ARow: Integer): TRect;

    /// <summary>Raises OnColumnMoved through the controller. Call after a
    /// column's display position changed (used by the column chooser and
    /// this grid's ColumnMoved override).</summary>
    procedure NotifyColumnMoved(Column: TColumn; OldIndex, NewIndex: Integer);

    property Controller: TVittixDBGridController read FController;
    property ColumnInfo: TVittixDBGridColumns read FColumnsInfo;
  published
    property FooterVisible: Boolean read GetFooterVisible write SetFooterVisible default True;

    property AlternatingRowColors: Boolean
      read GetAlternatingRowColors write SetAlternatingRowColors default True;

    property AlternateRowColor: TColor
      read GetAlternateRowColor write SetAlternateRowColor default $00F7F7F7;

    // Notification events (storage owned by the controller; see private
    // getters/setters above). Sender in the handlers is the controller.
    property OnAfterSort: TVittixAfterSortEvent
      read GetOnAfterSort write SetOnAfterSort;
    property OnSortError: TVittixSortErrorEvent
      read GetOnSortError write SetOnSortError;
    property OnFilterApplied: TVittixFilterAppliedEvent
      read GetOnFilterApplied write SetOnFilterApplied;
    property OnAfterApplyLayout: TNotifyEvent
      read GetOnAfterApplyLayout write SetOnAfterApplyLayout;
    property OnColumnMoved: TVittixColumnMovedEvent
      read GetOnColumnMoved write SetOnColumnMoved;

    property LayoutStorageFileName: string
      read FPersistence.LayoutStorageFileName write SetLayoutStorageFileName;

    property ChooserStateFileName: string
      read FPersistence.ChooserStateFileName write SetChooserStateFileName;

    property FilterHistoryFileName: string
      read FPersistence.FilterHistoryFileName write SetFilterHistoryFileName;

    property PersistenceRootPath: string
      read FPersistence.PersistenceRootPath write SetPersistenceRootPath;

    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property Align;
    property Anchors;
    property Options;
    property Columns;
    property Font;
    property TitleFont;
    property Color;
    property FixedColor;
    property PopupMenu;

    property OnTitleClick;
    property OnDrawColumnCell;
    property OnMouseDown;
    property OnKeyDown;
    property OnKeyUp;
    property OnDblClick;
    property OnColEnter;
    property OnColExit;
  end;

implementation

{ TVittixDBGrid }

constructor TVittixDBGrid.Create(AOwner: TComponent);
var
  Ctrl: TVittixDBGridController;
begin
  inherited;
  FColumnsInfo := TVittixDBGridColumns.Create(Self);

  // Create controller with Self as owner so it is freed automatically.
  // The controller's own constructor defaults (ShowFooter/AlternatingRowColors
  // = True, AlternateRowColor = $00F7F7F7) are the single source of truth for
  // the delegated visual properties — no local copies to keep in sync.
  Ctrl := TVittixDBGridController.Create(Self);
  FController := Ctrl;

  // SetGrid / SetShowFooter must NOT hook anything during construction.
  // The guards inside those methods check csDesigning, but csDesigning is
  // only set AFTER the constructor returns when placed in the IDE.
  // We call them last so all other fields are initialised first,
  // and Loaded will do the actual hooking at the right time.
  Ctrl.Grid := Self;

end;

procedure TVittixDBGrid.BeforeDestruction;
begin
  if Assigned(FController) then
    FController.Detach;

  FreeAndNil(FColumnsInfo);

  inherited;
end;

destructor TVittixDBGrid.Destroy;
begin
  FController := nil;
  inherited;
end;

function TVittixDBGrid.GetDataSource: TDataSource;
begin
  if (csDestroying in ComponentState) then
    Exit(nil);

  if (inherited DataSource <> nil) and
     not (csDestroying in inherited DataSource.ComponentState) then
    Result := inherited DataSource
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetDataSource(Value: TDataSource);
begin
  if inherited DataSource <> Value then
  begin
    inherited DataSource := Value;
    // DESIGN-TIME SAFETY: Object Inspector changes must not trigger engine init.
    if csDesigning in ComponentState then Exit;
    if Assigned(FController) then
      FController.DataSourceChanged;
  end;
end;

function TVittixDBGrid.GetFooterVisible: Boolean;
begin
  if Assigned(FController) then
    Result := FController.ShowFooter
  else
    Result := True; // default when the controller is absent (early teardown)
end;

procedure TVittixDBGrid.SetFooterVisible(const Value: Boolean);
begin
  if GetFooterVisible = Value then Exit;
  if Assigned(FController) then
    FController.ShowFooter := Value;
end;

// --- Delegated visual properties (controller is the source of truth) ---

function TVittixDBGrid.GetAlternatingRowColors: Boolean;
begin
  if Assigned(FController) then
    Result := FController.AlternatingRowColors
  else
    Result := True;
end;

procedure TVittixDBGrid.SetAlternatingRowColors(const Value: Boolean);
begin
  if GetAlternatingRowColors = Value then Exit;
  if Assigned(FController) then
  begin
    FController.AlternatingRowColors := Value;
    Invalidate;
  end;
end;

function TVittixDBGrid.GetAlternateRowColor: TColor;
begin
  if Assigned(FController) then
    Result := FController.AlternateRowColor
  else
    Result := $00F7F7F7;
end;

procedure TVittixDBGrid.SetAlternateRowColor(const Value: TColor);
begin
  if GetAlternateRowColor = Value then Exit;
  if Assigned(FController) then
  begin
    FController.AlternateRowColor := Value;
    if GetAlternatingRowColors then Invalidate;
  end;
end;
// ------------------------

procedure TVittixDBGrid.CreateWnd;
begin
  inherited;
  // Now that a real Win32 window handle exists, install the WindowProc hook
  // if the controller is ready but couldn't hook it earlier (e.g. when
  // HookGrid ran before the handle was allocated at runtime).
  if Assigned(FController) then
  begin
    FController.InstallWindowProc;
    FController.GridLayoutChanged;
  end;
end;

{ Controller integration — virtual overrides instead of event hooking.
  Each override lets the application's own OnXxx handler fire through
  inherited, and gives the controller a stable integration point that
  cannot be overwritten by later event assignments. }

procedure TVittixDBGrid.TitleClick(Column: TColumn);
begin
  inherited TitleClick(Column);
  if Assigned(FController) then
    FController.DoTitleClick(Column);
end;

procedure TVittixDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  // Controller tweaks Brush/Font (alternate rows, cell conditions) before
  // any drawing; the application handler (or the default) paints on top.
  if Assigned(FController) then
    FController.DoDrawColumnCell(
      Rect, DataCol, Column, State);
  if Assigned(OnDrawColumnCell) then
    inherited DrawColumnCell(Rect, DataCol, Column, State)
  else
    DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TVittixDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  // Controller intercepts right-clicks on the title row (filter popup,
  // column chooser). Everything else goes through normal VCL processing.
  if Assigned(FController) and
     FController.DoMouseDown(Button, Shift, X, Y) then
    Exit;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TVittixDBGrid.DblClick;
begin
  // Controller consumes double-clicks on memo/date fields (opens the
  // field editor); anything else fires the application's OnDblClick.
  if Assigned(FController) and
     FController.DoDblClick then
    Exit;
  inherited DblClick;
end;

procedure TVittixDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // Controller consumes F2 (memo/date field editor); everything else
  // goes through normal VCL key processing.
  if Assigned(FController) and
     FController.DoKeyDown(Key, Shift) then
  begin
    Key := 0;
    Exit;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TVittixDBGrid.SetLayoutStorageFileName(const Value: string);
begin
  if FPersistence.LayoutStorageFileName <> Value then
  begin
    FPersistence.LayoutStorageFileName := Value;
    if Assigned(FController) then
      FController.LayoutStorageFileName := Value;
  end;
end;

procedure TVittixDBGrid.SetChooserStateFileName(const Value: string);
begin
  if FPersistence.ChooserStateFileName <> Value then
    FPersistence.ChooserStateFileName := Value;
end;

procedure TVittixDBGrid.SetFilterHistoryFileName(const Value: string);
begin
  if FPersistence.FilterHistoryFileName <> Value then
    FPersistence.FilterHistoryFileName := Value;
end;

procedure TVittixDBGrid.SetPersistenceRootPath(const Value: string);
begin
  if FPersistence.PersistenceRootPath <> Value then
  begin
    FPersistence.PersistenceRootPath := Value;
    if Assigned(FController) then
      FController.PersistenceRootPath := Value;
  end;
end;

procedure TVittixDBGrid.Loaded;
begin
  inherited;
  SyncColumnInfo;

  // DESIGN-TIME SAFETY: Never trigger engine/dataset initialization while
  // the IDE is streaming the DFM. Only do this at true runtime.
  if csDesigning in ComponentState then Exit;

  if Assigned(FController) then
  begin
    FController.RehookGrid;
    FController.DataSourceChanged;
    FController.GridLayoutChanged;
  end;
end;

procedure TVittixDBGrid.LayoutChanged;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    SyncColumnInfo;

    if Assigned(FController) then
      if not FController.IsUpdating then
        FController.GridLayoutChanged;
  end;
end;

procedure TVittixDBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if csDestroying in ComponentState then
    Exit;

  if (Operation = opRemove) and (AComponent = DataSource) then
  begin
    // DESIGN-TIME SAFETY and BUG FIX: The original code set FController := nil
    // here, which orphaned and leaked the controller. At runtime, notify the
    // controller to unhook cleanly. At design time, do nothing at all.
    if csDesigning in ComponentState then Exit;

    if Assigned(FController) then
      FController.DataSourceChanged;
  end;
  if (Operation = opRemove) and (AComponent = FController) then
    FController := nil;
end;

procedure TVittixDBGrid.SyncColumnInfo;
var
  I: Integer;
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
begin
  // CRITICAL: FColumnsInfo may be nil if LayoutChanged is called by
  // TDBGrid.Create (via inherited) before we create FColumnsInfo.
  if not Assigned(FColumnsInfo) then Exit;
  if Columns.Count = 0 then Exit;

  for I := 0 to Columns.Count - 1 do
  begin
    Col := Columns[I];
    if Col.FieldName = '' then Continue;

    Info := FColumnsInfo.FindByFieldName(Col.FieldName);
    if Info = nil then
    begin
      Info := FColumnsInfo.Add;
      Info.FieldName := Col.FieldName;
    end;
  end;
end;

function TVittixDBGrid.ColumnInfoByColumn(Column: TColumn): TVittixDBGridColumnInfo;
begin
  Result := nil;
  if (Column = nil) or (Column.FieldName = '') then Exit;
  Result := FColumnsInfo.FindByFieldName(Column.FieldName);
end;

function TVittixDBGrid.GetIndicatorWidth: Integer;
begin
  if dgIndicator in Options then
    Result := IndicatorWidth
  else
    Result := 0;
end;

function TVittixDBGrid.GetIndicatorOffset: Integer;
begin
  Result := IndicatorOffset;
end;

function TVittixDBGrid.GetLeftCol: Integer;
begin
  Result := LeftCol;
end;

function TVittixDBGrid.GetCellRect(ACol, ARow: Integer): TRect;
begin
  Result := CellRect(ACol, ARow);
end;

{ --- Notification event delegation (storage lives on the controller) --- }

function TVittixDBGrid.GetOnAfterSort: TVittixAfterSortEvent;
begin
  if Assigned(FController) then
    Result := FController.OnAfterSort
  else
    Result := nil;
end;

function TVittixDBGrid.GetOnSortError: TVittixSortErrorEvent;
begin
  if Assigned(FController) then
    Result := FController.OnSortError
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnSortError(const Value: TVittixSortErrorEvent);
begin
  if Assigned(FController) then
    FController.OnSortError := Value;
end;

procedure TVittixDBGrid.SetOnAfterSort(const Value: TVittixAfterSortEvent);
begin
  if Assigned(FController) then
    FController.OnAfterSort := Value;
end;

function TVittixDBGrid.GetOnFilterApplied: TVittixFilterAppliedEvent;
begin
  if Assigned(FController) then
    Result := FController.OnFilterApplied
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnFilterApplied(const Value: TVittixFilterAppliedEvent);
begin
  if Assigned(FController) then
    FController.OnFilterApplied := Value;
end;

function TVittixDBGrid.GetOnAfterApplyLayout: TNotifyEvent;
begin
  if Assigned(FController) then
    Result := FController.OnAfterApplyLayout
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnAfterApplyLayout(const Value: TNotifyEvent);
begin
  if Assigned(FController) then
    FController.OnAfterApplyLayout := Value;
end;

function TVittixDBGrid.GetOnColumnMoved: TVittixColumnMovedEvent;
begin
  if Assigned(FController) then
    Result := FController.OnColumnMoved
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnColumnMoved(const Value: TVittixColumnMovedEvent);
begin
  if Assigned(FController) then
    FController.OnColumnMoved := Value;
end;

{ --- Column move notifications --- }

procedure TVittixDBGrid.ColumnMoved(FromIndex, ToIndex: Longint);
var
  ColIndex: Integer;
begin
  inherited ColumnMoved(FromIndex, ToIndex);

  // Translate raw grid coordinates into column display indexes (the
  // indicator column occupies raw column 0 when shown).
  ColIndex := ToIndex - GetIndicatorOffset;
  if (ColIndex >= 0) and (ColIndex < Columns.Count) then
    NotifyColumnMoved(Columns[ColIndex],
      FromIndex - GetIndicatorOffset, ToIndex - GetIndicatorOffset);
end;

procedure TVittixDBGrid.NotifyColumnMoved(Column: TColumn;
  OldIndex, NewIndex: Integer);
begin
  if Assigned(FController) then
    FController.DoColumnMoved(Column, OldIndex, NewIndex);
end;

end.
