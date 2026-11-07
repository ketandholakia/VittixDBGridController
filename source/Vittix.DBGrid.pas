unit Vittix.DBGrid;

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.Graphics, // Needed for TColor
  Vcl.Controls,
  Data.DB,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Paging.GlobalScroll,
  Vittix.DBGrid.Provider,
  Vittix.DBGrid.Controller;

type
  TVittixDBGridPersistenceSettings = record
    LayoutStorageFileName: string;
    ChooserStateFileName: string;
    FilterHistoryFileName: string;
    FilterSetsFileName: string;
    PersistenceRootPath: string;
  end;

  /// <summary>VCL data grid with controller-owned sorting, filtering,
  /// aggregation, layout persistence, and export support.</summary>
  TVittixDBGrid = class(TDBGrid)
  private
    FColumnsInfo: TVittixDBGridColumns;
    FController: TVittixDBGridController;
    FPersistence: TVittixDBGridPersistenceSettings;
    // Incremental search / copy options (grid-owned settings; the controller
    // reads them through VittixGrid() when handling the routed events)
    FIncrementalSearch: Boolean;
    FIncrementalSearchMode: TVittixIncrementalSearchMode;
    FIncrementalSearchTimeout: Integer;
    FCopyIncludeHeaders: Boolean;
    // Frozen (fixed left) column count over the visible columns; the
    // underlying TCustomGrid.FixedCols is derived, not stored.
    FFrozenColumns: Integer;
    FOnFrozenColumnsChanged: TNotifyEvent;
    // Data routing mode: vcmClient (default) keeps the classic behavior.
    FFilterMode: TVittixFilterMode;
    procedure SyncColumnInfo;
    function GetFooterVisible: Boolean;
    procedure SetFooterVisible(const Value: Boolean);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetFrozenColumns(const Value: Integer);
    procedure ApplyFrozenColumns(ANewCount: Integer; ASyncPins: Boolean);
    function RawFrozenColCount(ANewCount: Integer): Integer;
    function VisibleDataColumnCount: Integer;
    procedure SyncPinnedFlags(ANewCount: Integer);
    procedure SetFilterSetsFileName(const Value: string);
    function GetOnFilterSetApplied: TVittixFilterSetAppliedEvent;
    procedure SetOnFilterSetApplied(const Value: TVittixFilterSetAppliedEvent);
    function GetOnFilterSetsChanged: TNotifyEvent;
    procedure SetOnFilterSetsChanged(const Value: TNotifyEvent);
    function GetOnValidateCell: TVittixValidateCellEvent;
    procedure SetOnValidateCell(const Value: TVittixValidateCellEvent);
    function GetOnProviderFallback: TVittixProviderFallbackEvent;
    procedure SetOnProviderFallback(const Value: TVittixProviderFallbackEvent);
    function GetOnProviderError: TVittixProviderErrorEvent;
    procedure SetOnProviderError(const Value: TVittixProviderErrorEvent);

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
    function GetOnIncrementalSearch: TVittixIncrementalSearchEvent;
    procedure SetOnIncrementalSearch(const Value: TVittixIncrementalSearchEvent);

  protected
    procedure Loaded; override;
    procedure Paint; override;
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
    procedure KeyPress(var Key: Char); override;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    /// <summary>Returns the metadata for a column's field name, or nil when
    /// the column has no matching metadata.</summary>
    function ColumnInfoByColumn(Column: TColumn): TVittixDBGridColumnInfo;
    /// <summary>Width of the row indicator, or zero when it is hidden.</summary>
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

    /// <summary>Freezes all visible columns from the left up to and
    /// including AColumn (inclusive). A nil or hidden column is a no-op.</summary>
    procedure FreezeUpToColumn(Column: TColumn);

    /// <summary>Removes the frozen region and clears every column's Pinned
    /// flag.</summary>
    procedure UnfreezeAll;

    /// <summary>True when the multi-selection contains bookmarks that are
    /// no longer valid for the bound dataset — page-scoped wrappers whose
    /// page was replaced or reloaded, or whose virtual mode is suspended.
    /// The VCL's TBookmarkList.Delete raises only when it reaches a stale
    /// entry, after already deleting every entry processed before it, so a
    /// mixed selection would partially delete. Use
    /// DeleteSelectedRowsSafely for application-driven deletion, or prune
    /// with SelectedRows.Refresh. The grid's Ctrl+Delete path performs
    /// this check automatically.
    /// Always False for datasets that do not implement global page
    /// scrolling.</summary>
    function SelectionHasStaleEntries: Boolean;

    /// <summary>Deletes the selected rows only after every bookmark has
    /// been validated against the bound dataset. Raises EDatabaseError
    /// without deleting anything when the selection contains an invalid
    /// bookmark. An empty selection is a no-op. This protects callers
    /// from the VCL's partial-delete behavior with stale bookmarks.</summary>
    procedure DeleteSelectedRowsSafely;

    /// <summary>Recomputes the frozen region from the per-column Pinned
    /// flags (the leading run of pinned visible columns). Called by the
    /// controller after a layout restore or a column-chooser visibility
    /// change; applications hiding columns directly can call it too.</summary>
    procedure RefreshFrozenColumns;

    /// <summary>Controller that owns the grid's engines and operational
    /// methods. The grid owns its lifetime.</summary>
    property Controller: TVittixDBGridController read FController;
    /// <summary>Per-field sorting, filtering, aggregation, display, and
    /// export settings. Entries are matched by FieldName.</summary>
    property ColumnInfo: TVittixDBGridColumns read FColumnsInfo;
  published
    /// <summary>Shows the aggregation footer at runtime and its static
    /// preview in the form designer. Default True.</summary>
    property FooterVisible: Boolean read GetFooterVisible write SetFooterVisible default True;

    /// <summary>Paints alternating data rows with AlternateRowColor when
    /// enabled. Default True.</summary>
    property AlternatingRowColors: Boolean
      read GetAlternatingRowColors write SetAlternatingRowColors default True;

    /// <summary>Background color for alternating rows.</summary>
    property AlternateRowColor: TColor
      read GetAlternateRowColor write SetAlternateRowColor default $00F7F7F7;

    /// <summary>Enables type-ahead locate: printable characters accumulate
    /// into a search buffer and the grid locates the first record whose
    /// primary sort column (or current column, when unsorted) starts with
    /// the buffer. Default True.</summary>
    property IncrementalSearch: Boolean
      read FIncrementalSearch write FIncrementalSearch default True;

    /// <summary>Idle period in milliseconds after which the incremental
    /// search buffer resets (default 1000). Zero or less expires the buffer
    /// after every keystroke.</summary>
    property IncrementalSearchTimeout: Integer
      read FIncrementalSearchTimeout write FIncrementalSearchTimeout default 1000;

    /// <summary>Controls when IncrementalSearch consumes printable keys.
    /// ismAuto (default) searches only when the grid cannot enter in-place
    /// editing (no dgEditing, or the grid/column/dataset is read-only), so
    /// typing starts an edit on a writable grid. ismAlways is the pre-2.1
    /// behavior (always search while IncrementalSearch is True). ismOff
    /// disables search. IncrementalSearch = False still turns search off
    /// regardless of this mode.</summary>
    property IncrementalSearchMode: TVittixIncrementalSearchMode
      read FIncrementalSearchMode write FIncrementalSearchMode default ismAuto;

    /// <summary>When True, a plain Ctrl+C copies the selection with a TSV
    /// header row of the visible column captions. Ctrl+Shift+C always
    /// includes headers regardless of this setting. Default False.</summary>
    property CopyIncludeHeaders: Boolean
      read FCopyIncludeHeaders write FCopyIncludeHeaders default False;

    /// <summary>Number of leading visible columns kept fixed while the rest
    /// of the grid scrolls horizontally (default 0). The setter clamps to
    /// 0..VisibleColumnCount-1 and marks the frozen columns as Pinned in
    /// ColumnInfo so the layout schema persists them.</summary>
    property FrozenColumns: Integer
      read FFrozenColumns write SetFrozenColumns default 0;

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

    /// <summary>Fired after every incremental search keystroke was
    /// processed. Sender is the controller; SearchText is the active search
    /// buffer ('' after a reset) and Found reports whether it located a
    /// record. Shares storage with the controller's OnIncrementalSearch.</summary>
    property OnIncrementalSearch: TVittixIncrementalSearchEvent
      read GetOnIncrementalSearch write SetOnIncrementalSearch;

    /// <summary>Fired after the frozen region changed (FrozenColumns
    /// assignment, FreezeUpToColumn, UnfreezeAll, or a recompute from the
    /// Pinned flags). Sender is the grid.</summary>
    property OnFrozenColumnsChanged: TNotifyEvent
      read FOnFrozenColumnsChanged write FOnFrozenColumnsChanged;

    /// <summary>Data routing mode. vcmClient (default) evaluates filters
    /// and sorting in the client engine exactly as before; vcmServer
    /// pushes them through the assigned Controller.Provider; vcmAuto uses
    /// the server when the provider can handle the dataset and every
    /// condition is expressible. Old DFMs load as vcmClient.</summary>
    property FilterMode: TVittixFilterMode
      read FFilterMode write FFilterMode default vcmClient;

    /// <summary>Fired when server-side filtering fell back to the client
    /// engine. Shares storage with the controller's OnProviderFallback.
    /// </summary>
    property OnProviderFallback: TVittixProviderFallbackEvent
      read GetOnProviderFallback write SetOnProviderFallback;

    /// <summary>Fired when the provider's re-query raised; set Handled to
    /// swallow. Shares storage with the controller's OnProviderError.
    /// </summary>
    property OnProviderError: TVittixProviderErrorEvent
      read GetOnProviderError write SetOnProviderError;

    /// <summary>Fired after a saved filter set was applied. Shares storage
    /// with the controller's OnFilterSetApplied, so an assignment on either
    /// surface fires exactly once. Sender is the controller.</summary>
    property OnFilterSetApplied: TVittixFilterSetAppliedEvent
      read GetOnFilterSetApplied write SetOnFilterSetApplied;

    /// <summary>Fired after the saved filter sets changed. Shares storage
    /// with the controller's OnFilterSetsChanged.</summary>
    property OnFilterSetsChanged: TNotifyEvent
      read GetOnFilterSetsChanged write SetOnFilterSetsChanged;

    /// <summary>Per-column validation for every Vittix popup editor; runs
    /// after the built-in Required/Min/Max rules. Shares storage with the
    /// controller's OnValidateCell, so an assignment on either surface
    /// fires exactly once.</summary>
    property OnValidateCell: TVittixValidateCellEvent
      read GetOnValidateCell write SetOnValidateCell;

    /// <summary>Layout JSON file used by controller save/load calls when
    /// no explicit file name is passed.</summary>
    property LayoutStorageFileName: string
      read FPersistence.LayoutStorageFileName write SetLayoutStorageFileName;

    /// <summary>File for the column chooser's saved state.</summary>
    property ChooserStateFileName: string
      read FPersistence.ChooserStateFileName write SetChooserStateFileName;

    /// <summary>File for the filter popup's value history.</summary>
    property FilterHistoryFileName: string
      read FPersistence.FilterHistoryFileName write SetFilterHistoryFileName;

    /// <summary>File the saved filter sets persist to. Empty resolves to
    /// PersistenceRootPath\filtersets.json on the controller (per-grid
    /// instance setting, never a process-wide global).</summary>
    property FilterSetsFileName: string
      read FPersistence.FilterSetsFileName write SetFilterSetsFileName;

    /// <summary>Directory used for default layout, chooser, filter history,
    /// and filter set files when their individual paths are empty.</summary>
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

  // Defaults for the grid-owned behavior options; matching `default` clauses
  // in the published section keep existing DFMs streaming the same.
  FIncrementalSearch := True;
  FIncrementalSearchMode := ismAuto;
  FIncrementalSearchTimeout := 1000;
  FCopyIncludeHeaders := False;
  FFrozenColumns := 0;
  FFilterMode := vcmClient;

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
  if csDesigning in ComponentState then
    Invalidate;
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
var
  Paged: IVittixGlobalPageScroll;
begin
  // Controller consumes F2 (memo/date field editor) and the selection copy
  // shortcuts; everything else goes through normal VCL key processing.
  if Assigned(FController) and
     FController.DoKeyDown(Key, Shift) then
  begin
    Key := 0;
    Exit;
  end;
  // Ctrl+Home/Ctrl+End call the non-virtual TDataSet.First/Last, which stay
  // on the loaded page of a paged dataset. Route them through global seeks
  // for datasets that implement global page scrolling.
  if (ssCtrl in Shift) and (Key in [VK_HOME, VK_END]) and (DataSource <> nil)
    and Supports(DataSource.DataSet, IVittixGlobalPageScroll, Paged)
    and Paged.GlobalScrollActive then
  begin
    // Mirror the VCL's movement-key selection reset (its ClearSelection is
    // a private local procedure; SelectedRows is the public equivalent).
    if dgMultiSelect in Options then
      SelectedRows.Clear;
    if Key = VK_HOME then
      Paged.GlobalFirst
    else
      Paged.GlobalLast;
    Key := 0;
    Exit;
  end;
  // Selection delete (Ctrl+VK_DELETE) must be all-or-nothing: the VCL's
  // TBookmarkList.Delete iterates its sorted list from the end and raises
  // only when it reaches a stale page-scoped bookmark, after already
  // deleting every entry processed before it — a mixed selection would
  // partially delete (verified). Validate every entry first and refuse the
  // whole operation before any row is deleted. Datasets without global
  // page scrolling use native bookmark semantics and are untouched.
  if (Key = VK_DELETE) and (ssCtrl in Shift) and
     (dgMultiSelect in Options) and
     (SelectedRows.Count > 0) and (DataSource <> nil) and
     Supports(DataSource.DataSet, IVittixGlobalPageScroll, Paged) and
     SelectionHasStaleEntries then
    raise EDatabaseError.Create(
      'The selection contains rows from pages that are no longer loaded; ' +
      'refresh the selection before deleting.');
  inherited KeyDown(Key, Shift);
end;

procedure TVittixDBGrid.WMVScroll(var Message: TWMVScroll);
var
  Paged: IVittixGlobalPageScroll;
  SI: TScrollInfo;
begin
  // TCustomDBGrid.WMVScroll routes the scrollbar endpoints and edge thumb
  // positions through the non-virtual TDataSet.First/Last, which stay on
  // the loaded page of a paged dataset. Intercept those codes for datasets
  // that implement global page scrolling; line and page scrolling and
  // in-range thumb seeks keep the inherited behavior (the dataset's RecNo
  // setter and MoveBy seek across pages on their own).
  if (DataSource <> nil)
    and Supports(DataSource.DataSet, IVittixGlobalPageScroll, Paged)
    and Paged.GlobalScrollActive then
  begin
    // Mirror the VCL's AcquireFocus before scrolling (private on
    // TCustomDBGrid). Best effort: a disabled or hidden window keeps the
    // old focus and the navigation still runs.
    try
      if CanFocus and not Focused then
        SetFocus;
    except
      on E: EInvalidOperation do;
    end;
    case Message.ScrollCode of
      SB_TOP:
      begin
        Paged.GlobalFirst;
        Exit;
      end;
      SB_BOTTOM:
      begin
        Paged.GlobalLast;
        Exit;
      end;
      SB_THUMBPOSITION, SB_THUMBTRACK:
        if (dgThumbTracking in Options) or
          (Message.ScrollCode = SB_THUMBPOSITION) then
        begin
          SI.cbSize := SizeOf(SI);
          SI.fMask := SIF_ALL;
          GetScrollInfo(Handle, SB_VERT, SI);
          if SI.nTrackPos <= 1 then
            Paged.GlobalFirst
          else if SI.nTrackPos >= Paged.GlobalRecordCount then
            Paged.GlobalLast
          else
          begin
            // The dataset's RecNo setter seeks across pages.
            DataSource.DataSet.RecNo := SI.nTrackPos;
          end;
          Exit;
        end;
    end;
  end;
  inherited;
end;

procedure TVittixDBGrid.KeyPress(var Key: Char);
begin
  // While the in-place editor is open every keystroke belongs to it — the
  // type-ahead search must not steal or consume anything.
  if EditorMode then
  begin
    inherited KeyPress(Key);
    Exit;
  end;

  // Controller consumes printable characters for the incremental search and
  // Esc/Backspace buffer management; everything else falls through.
  if Assigned(FController) and
     FController.DoKeyPress(Key) then
  begin
    Key := #0;
    Exit;
  end;
  inherited KeyPress(Key);
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

procedure TVittixDBGrid.SetFilterSetsFileName(const Value: string);
begin
  if FPersistence.FilterSetsFileName = Value then Exit;
  FPersistence.FilterSetsFileName := Value;
  // The controller owns the filter set list; keep its target in sync.
  if Assigned(FController) then
    FController.FilterSetsFileName := Value;
end;

function TVittixDBGrid.GetOnFilterSetApplied: TVittixFilterSetAppliedEvent;
begin
  if Assigned(FController) then
    Result := FController.OnFilterSetApplied
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnFilterSetApplied(
  const Value: TVittixFilterSetAppliedEvent);
begin
  if Assigned(FController) then
    FController.OnFilterSetApplied := Value;
end;

function TVittixDBGrid.GetOnFilterSetsChanged: TNotifyEvent;
begin
  if Assigned(FController) then
    Result := FController.OnFilterSetsChanged
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnFilterSetsChanged(const Value: TNotifyEvent);
begin
  if Assigned(FController) then
    FController.OnFilterSetsChanged := Value;
end;

function TVittixDBGrid.GetOnValidateCell: TVittixValidateCellEvent;
begin
  if Assigned(FController) then
    Result := FController.OnValidateCell
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnValidateCell(const Value: TVittixValidateCellEvent);
begin
  if Assigned(FController) then
    FController.OnValidateCell := Value;
end;

function TVittixDBGrid.GetOnProviderFallback: TVittixProviderFallbackEvent;
begin
  if Assigned(FController) then
    Result := FController.OnProviderFallback
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnProviderFallback(
  const Value: TVittixProviderFallbackEvent);
begin
  if Assigned(FController) then
    FController.OnProviderFallback := Value;
end;

function TVittixDBGrid.GetOnProviderError: TVittixProviderErrorEvent;
begin
  if Assigned(FController) then
    Result := FController.OnProviderError
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnProviderError(
  const Value: TVittixProviderErrorEvent);
begin
  if Assigned(FController) then
    FController.OnProviderError := Value;
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

procedure TVittixDBGrid.Paint;
var
  FooterRect: TRect;
  FooterHeight: Integer;
  I: Integer;
  CellBounds: TRect;
begin
  inherited;
  if not (csDesigning in ComponentState) or not GetFooterVisible then
    Exit;

  // The IDE needs only a visual indication of the footer. Painting directly
  // on the grid avoids creating a child control or starting runtime engines.
  Canvas.Font.Assign(Font);
  Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  FooterHeight := Canvas.TextHeight('Hg') + 8;
  FooterRect := Rect(0, ClientHeight - FooterHeight, ClientWidth, ClientHeight);
  if FooterRect.Top < 0 then
    FooterRect.Top := 0;

  Canvas.Brush.Color := FixedColor;
  Canvas.FillRect(FooterRect);
  Canvas.Pen.Color := clBtnShadow;
  Canvas.MoveTo(FooterRect.Left, FooterRect.Top);
  Canvas.LineTo(FooterRect.Right, FooterRect.Top);
  if RowCount > 1 then
    for I := 0 to ColCount - 1 do
    begin
      CellBounds := CellRect(I, 1);
      if (CellBounds.Right > FooterRect.Left) and
         (CellBounds.Right < FooterRect.Right) then
      begin
        Canvas.MoveTo(CellBounds.Right - 1, FooterRect.Top);
        Canvas.LineTo(CellBounds.Right - 1, FooterRect.Bottom);
      end;
    end;

  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(FooterRect.Left + 5, FooterRect.Top + 4, 'Footer preview');
  Canvas.Brush.Style := bsSolid;
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

  // TCustomDBGrid resets FixedCols to the bare indicator offset on every
  // layout change; re-derive the frozen region on top of it. Recomputing
  // the count is deliberately NOT done here (a mid-move or mid-layout
  // recompute would fight the caller) — only the raw FixedCols value is
  // made consistent again.
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    if GetIndicatorOffset + FFrozenColumns <> FixedCols then
      ApplyFrozenColumns(FFrozenColumns, False);
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
  InfoIndex: TStringList;
begin
  // CRITICAL: FColumnsInfo may be nil if LayoutChanged is called by
  // TDBGrid.Create (via inherited) before we create FColumnsInfo.
  if not Assigned(FColumnsInfo) then Exit;
  if Columns.Count = 0 then Exit;

  InfoIndex := TStringList.Create;
  try
    InfoIndex.CaseSensitive := False;
    InfoIndex.Sorted := True;
    InfoIndex.Duplicates := dupIgnore;

    // Index existing entries once. FindByFieldName uses SameText and returns
    // the first matching item, so dupIgnore preserves that lookup behavior.
    for I := 0 to FColumnsInfo.Count - 1 do
    begin
      Info := FColumnsInfo[I];
      if Info.FieldName <> '' then
        InfoIndex.AddObject(Info.FieldName, Info);
    end;

    for I := 0 to Columns.Count - 1 do
    begin
      Col := Columns[I];
      if Col.FieldName = '' then Continue;

      if InfoIndex.IndexOf(Col.FieldName) < 0 then
      begin
        Info := FColumnsInfo.Add;
        Info.FieldName := Col.FieldName;
        InfoIndex.AddObject(Info.FieldName, Info);
      end;
    end;
  finally
    InfoIndex.Free;
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

function TVittixDBGrid.GetOnIncrementalSearch: TVittixIncrementalSearchEvent;
begin
  if Assigned(FController) then
    Result := FController.OnIncrementalSearch
  else
    Result := nil;
end;

procedure TVittixDBGrid.SetOnIncrementalSearch(
  const Value: TVittixIncrementalSearchEvent);
begin
  if Assigned(FController) then
    FController.OnIncrementalSearch := Value;
end;

{ --- Frozen (fixed left) columns --- }

function TVittixDBGrid.VisibleDataColumnCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Columns.Count - 1 do
    if Columns[I].Visible then
      Inc(Result);
end;

procedure TVittixDBGrid.SyncPinnedFlags(ANewCount: Integer);
var
  I, Seen: Integer;
  Info: TVittixDBGridColumnInfo;
  Pinned: Boolean;
begin
  // The per-column Pinned flags mirror the frozen region: the first
  // ANewCount visible columns are pinned, everything after is not.
  Seen := 0;
  for I := 0 to Columns.Count - 1 do
  begin
    if Columns[I].Visible then
      Inc(Seen);
    Pinned := (Seen <= ANewCount) and Columns[I].Visible;
    Info := ColumnInfoByColumn(Columns[I]);
    if Assigned(Info) then
      Info.Pinned := Pinned;
  end;
end;

function TVittixDBGrid.RawFrozenColCount(ANewCount: Integer): Integer;
var
  I, Seen: Integer;
begin
  // TCustomGrid.FixedCols counts RAW grid columns (indicator included) and
  // hidden columns keep raw slots, so the count must cover the first
  // ANewCount VISIBLE columns.
  Result := GetIndicatorOffset;
  if ANewCount <= 0 then
    Exit;

  Seen := 0;
  for I := 0 to Columns.Count - 1 do
  begin
    if Columns[I].Visible then
      Inc(Seen);
    if Seen = ANewCount then
      Exit(GetIndicatorOffset + I + 1);
  end;

  // Fewer visible columns than requested: freeze the whole column area.
  Result := GetIndicatorOffset + Columns.Count;
end;

procedure TVittixDBGrid.ApplyFrozenColumns(ANewCount: Integer; ASyncPins: Boolean);
var
  FixedColsValue: Integer;
begin
  // Clamp to 0..VisibleColumnCount-1: freezing every visible column would
  // leave nothing to scroll.
  if ANewCount < 0 then
    ANewCount := 0;
  if ANewCount > VisibleDataColumnCount - 1 then
    ANewCount := VisibleDataColumnCount - 1;
  if ANewCount < 0 then
    ANewCount := 0;

  FixedColsValue := RawFrozenColCount(ANewCount);
  // SetFixedCols raises when the value reaches ColCount; clamp instead so
  // a degenerate column set degrades to "no frozen region".
  if (ColCount > 0) and (FixedColsValue > ColCount - 1) then
    FixedColsValue := ColCount - 1;
  if FixedColsValue < 0 then
    FixedColsValue := 0;
  if FixedColsValue <> FixedCols then
    FixedCols := FixedColsValue;

  // Sync even when the count is unchanged: UnfreezeAll on an already
  // unfrozen grid must still clear any stray Pinned flags.
  if ASyncPins then
    SyncPinnedFlags(ANewCount);

  if ANewCount <> FFrozenColumns then
  begin
    FFrozenColumns := ANewCount;
    if Assigned(FOnFrozenColumnsChanged) then
      FOnFrozenColumnsChanged(Self);
  end;
end;

procedure TVittixDBGrid.SetFrozenColumns(const Value: Integer);
begin
  ApplyFrozenColumns(Value, True);
end;

procedure TVittixDBGrid.FreezeUpToColumn(Column: TColumn);
var
  I, Count: Integer;
begin
  if Column = nil then Exit;

  // Count the visible columns up to and including the target.
  Count := 0;
  for I := 0 to Columns.Count - 1 do
  begin
    if Columns[I].Visible then
      Inc(Count);
    if Columns[I] = Column then
    begin
      // Freezing up to a hidden column has no visual meaning.
      if Column.Visible then
        SetFrozenColumns(Count);
      Exit;
    end;
  end;
end;

procedure TVittixDBGrid.UnfreezeAll;
begin
  SetFrozenColumns(0);
end;

function TVittixDBGrid.SelectionHasStaleEntries: Boolean;
var
  I: Integer;
  Paged: IVittixGlobalPageScroll;
begin
  Result := False;
  if (DataSource = nil) or (DataSource.DataSet = nil) then
    Exit;
  if not Supports(DataSource.DataSet, IVittixGlobalPageScroll, Paged) then
    Exit;
  for I := 0 to SelectedRows.Count - 1 do
    if not DataSource.DataSet.BookmarkValid(SelectedRows[I]) then
      Exit(True);
end;

procedure TVittixDBGrid.DeleteSelectedRowsSafely;
var
  I: Integer;
  DataSet: TDataSet;
begin
  if SelectedRows.Count = 0 then
    Exit;
  if (DataSource = nil) or (DataSource.DataSet = nil) or
    not DataSource.DataSet.Active then
    raise EDatabaseError.Create('Cannot delete selected rows without an active dataset.');
  DataSet := DataSource.DataSet;
  for I := 0 to SelectedRows.Count - 1 do
    if not DataSet.BookmarkValid(SelectedRows[I]) then
      raise EDatabaseError.Create(
        'The selection contains rows that are no longer available; ' +
        'refresh the selection before deleting.');
  SelectedRows.Delete;
end;

procedure TVittixDBGrid.RefreshFrozenColumns;
var
  I, Count: Integer;
  Info: TVittixDBGridColumnInfo;
begin
  // The leading run of pinned VISIBLE columns defines the frozen region;
  // hiding one of them (chooser or application code) shrinks it naturally.
  Count := 0;
  for I := 0 to Columns.Count - 1 do
  begin
    if not Columns[I].Visible then
      Continue;
    Info := ColumnInfoByColumn(Columns[I]);
    if Assigned(Info) and Info.Pinned then
      Inc(Count)
    else
      Break;
  end;
  ApplyFrozenColumns(Count, False);
end;

{ --- Column move notifications --- }

procedure TVittixDBGrid.ColumnMoved(FromIndex, ToIndex: Longint);
var
  FromData, ToData: Integer;
  ColIndex: Integer;
  Frozen: Integer;
begin
  // Translate raw grid coordinates into column display indexes (the
  // indicator column occupies raw column 0 when shown).
  FromData := FromIndex - GetIndicatorOffset;
  ToData := ToIndex - GetIndicatorOffset;
  Frozen := FFrozenColumns;

  inherited ColumnMoved(FromIndex, ToIndex);

  // A frozen column cannot leave the frozen region and a scrolling column
  // cannot enter it. The inherited handler has already moved the column;
  // undo it through the same path (TColumn.SetIndex never re-enters
  // MoveColumn, so this cannot recurse) and report nothing.
  if (Frozen > 0) and
     (((FromData < Frozen) and (ToData >= Frozen)) or
      ((FromData >= Frozen) and (ToData < Frozen))) then
  begin
    if (ToData >= 0) and (ToData < Columns.Count) then
      Columns[ToData].Index := FromData;
    Exit;
  end;

  if (ToData >= 0) and (ToData < Columns.Count) then
    NotifyColumnMoved(Columns[ToData], FromData, ToData);
end;

procedure TVittixDBGrid.NotifyColumnMoved(Column: TColumn;
  OldIndex, NewIndex: Integer);
begin
  if Assigned(FController) then
    FController.DoColumnMoved(Column, OldIndex, NewIndex);
end;

end.
