unit Vittix.DBGrid.Controller;

{$REGION 'Documentation'}
/// <summary>
/// Controller for TVittixDBGrid: wires the grid to the sort, filter and
/// aggregation engines, owns the footer panel and syncs it through the
/// hooked WindowProc.
///
/// THREAD SAFETY: Not thread-safe. Must be used from the main VCL thread only.
/// </summary>
{$ENDREGION}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.Math,
  System.Diagnostics,
  System.TypInfo,
  Winapi.Windows,
  Winapi.Messages,
  System.IOUtils,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.Forms,
  Data.DB,

  // Vittix
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.ColumnChooser,
  Vittix.DBGrid.Editors,
  Vittix.DBGrid.Sort.Engine,
  Vittix.DBGrid.Filter.Engine,
  Vittix.DBGrid.FilterSets,
  Vittix.DBGrid.Provider,
  Vittix.DBGrid.Aggregation.Engine,
  Vittix.DBGrid.Filter.Popup,
  Vittix.DBGrid.FooterPanel,
  Vittix.DBGrid.Layout;

const
  DEFAULT_ALTERNATE_ROW_COLOR = $00F7F7F7;

type
  TVittixDBGridController = class;

  /// <summary>Raised after a sort was applied to the dataset. Column is the
  /// column the user toggled, or nil when sorting was (re)applied wholesale
  /// (ApplyState / Clear / ApplyLayout).</summary>
  TVittixAfterSortEvent = procedure(Sender: TObject; Column: TColumn) of object;

  /// <summary>Raised after a filter was applied (popup commit, global filter,
  /// or clear-all). FieldName is the affected column's field name, or '' for
  /// global/clear operations.</summary>
  TVittixFilterAppliedEvent = procedure(Sender: TObject;
    const FieldName: string) of object;

  /// <summary>Raised after a column's display position changed (column
  /// chooser drag/keyboard moves, layout restore, VCL column moves).</summary>
  TVittixColumnMovedEvent = procedure(Sender: TObject; Column: TColumn;
    OldIndex, NewIndex: Integer) of object;

  /// <summary>Raised when a header-click sort failed (for example a dataset
  /// without IndexFieldNames support). Only fired from interactive title
  /// clicks; the exception is re-raised when no handler is assigned. The
  /// sort state is rolled back before this fires.</summary>
  TVittixSortErrorEvent = procedure(Sender: TObject; Column: TColumn;
    AError: Exception) of object;

  /// <summary>Raised after an incremental (type-ahead) search keystroke was
  /// processed. SearchText is the current search buffer ('' when the buffer
  /// was just reset); Found reports whether the buffer located a record.</summary>
  TVittixIncrementalSearchEvent = procedure(Sender: TObject;
    const SearchText: string; Found: Boolean) of object;

  /// <summary>When type-ahead search takes over printable keystrokes.
  /// ismAuto (default) searches only when the grid is not editable, so
  /// typing can still start in-place editing on a writable grid; ismAlways
  /// is the pre-2.1 behavior (search whenever IncrementalSearch is True);
  /// ismOff disables search like IncrementalSearch = False.</summary>
  TVittixIncrementalSearchMode = (ismAuto, ismAlways, ismOff);

  /// <summary>Raised after a saved filter set was applied
  /// (ApplyFilterSet / layout restore). AName is the applied set.</summary>
  TVittixFilterSetAppliedEvent = procedure(Sender: TObject;
    const AName: string) of object;

  /// <summary>Raised by every Vittix popup editor (memo, date, pick list,
  /// lookup, masked numeric) before a value is committed; the built-in
  /// Required/Min/Max rules run first. Accept = False keeps the editor
  /// open and shows Message. Declared in the editors unit as
  /// TVittixValidateCellEvent and re-exported here.</summary>
  TVittixValidateCellEvent = Vittix.DBGrid.Editors.TVittixValidateCellEvent;

  /// <summary>Fired when server-side filtering fell back to the client
  /// engine: AReason says why (no provider, unwrappable SQL, inexpressible
  /// filter, failing query). vcmAuto fires it too when a provider exists
  /// but declined - applications can log the reason.</summary>
  TVittixProviderFallbackEvent = procedure(Sender: TObject;
    const AReason: string) of object;

  /// <summary>Fired when the provider's re-query raised. Set Handled to
  /// True to swallow it; unhandled errors re-raise after the provider has
  /// restored the original SQL and re-opened the dataset.</summary>
  TVittixProviderErrorEvent = procedure(Sender: TObject; E: Exception;
    var Handled: Boolean) of object;

  TVittixGridDataLink = class(TDataLink)
  private
    FController: TVittixDBGridController;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(AController: TVittixDBGridController);
  end;

  TVittixDBGridController = class(TComponent)
  private
    // FGrid is typed TDBGrid so this unit does not need Vittix.DBGrid in its
    // interface (TVittixDBGrid needs this unit in ITS interface to expose the
    // strongly typed Controller property). SetGrid enforces that the grid is
    // a TVittixDBGrid; VittixGrid gives typed access to its members.
    FGrid: TDBGrid;
    FDataset: TDataSet;
    FDataLink: TVittixGridDataLink;

    FActive: Boolean;
    FShowFooter: Boolean;
    FAutoRefresh: Boolean;
    FUpdating: Boolean;  // Re-entrance guard

    FAlternatingRowColors: Boolean;
    FAlternateRowColor: TColor;
    FLayoutStorageFileName: string;
    FPersistenceRootPath: string;

    // Engines (logic only)
    FSortEngine: TVittixDBGridSortEngine;
    FFilterEngine: TVittixDBGridFilterEngine;
    FAggregationEngine: TVittixDBGridAggregationEngine;
    FAggregationDirty: Boolean;
    FEnginesCreated: Boolean;
    FFooterPanel: TVittixDBGridFooterPanel;
    FAggregationBusy: Boolean;

    // Notification events. Handlers are cached here (assignment must survive
    // engine recreation while datasets close/reopen) and pushed to the
    // engines in CreateEngines / the property setters.
    FOnAfterSort: TVittixAfterSortEvent;
    FOnSortError: TVittixSortErrorEvent;
    FOnFilterApplied: TVittixFilterAppliedEvent;
    FOnAfterApplyLayout: TNotifyEvent;
    FOnColumnMoved: TVittixColumnMovedEvent;
    FOnValidateFilter: TFilterValidationEvent;
    FOnFormatAggregation: TFormatAggregationEvent;
    FOnFieldValidation: TFieldValidationEvent;
    FOnIncrementalSearch: TVittixIncrementalSearchEvent;

    // Incremental (type-ahead) search state. The stopwatch timestamps the
    // last keystroke so the buffer expires after the grid's timeout.
    FSearchBuffer: string;
    FSearchWatch: TStopwatch;

    // Saved filter sets. The list loads lazily from FilterSetsFileName on
    // first use; the active set name survives dataset churn because the
    // engine is re-bound from it in CreateEngines.
    FFilterSetList: TVittixFilterSetList;
    FFilterSetListLoaded: Boolean;
    FFilterSetsFileName: string;
    FActiveFilterSetName: string;
    FOnFilterSetApplied: TVittixFilterSetAppliedEvent;
    FOnFilterSetsChanged: TNotifyEvent;
    FOnValidateCell: TVittixValidateCellEvent;

    // Server-side filter/sort provider state (v2.0). The provider is an
    // application-supplied interface; the server path is only taken when
    // the grid's FilterMode asks for it.
    FProvider: IVittixDataProvider;
    FOnProviderFallback: TVittixProviderFallbackEvent;
    FOnProviderError: TVittixProviderErrorEvent;
    FServerFilterActive: Boolean;
    FServerSortActive: Boolean;
    FPendingServerRoute: Boolean;

    // Last observed multi-select row count; the footer must repaint when the
    // selection changes so its selection aggregates stay current.
    FLastSelectionCount: Integer;

    // Event hooks — only the WindowProc remains; grid input/draw integration
    // happens via virtual overrides on TVittixDBGrid calling the DoXxx methods.
    FOldWindowProc: TWndMethod;

    // Internal helpers
    function IsReady: Boolean;
    function FindInfoByColumn(AColumn: TColumn): TVittixDBGridColumnInfo;
    function FindColumnByField(AField: TField): TColumn;

    /// <summary>Pushes a layout-restore DisplayFormat onto the column's
    /// backing field (numeric and datetime fields only; empty leaves the
    /// field format untouched).</summary>
    procedure ApplyColumnDisplayFormat(AColumn: TColumn; const AFormat: string);

    /// <summary>True when the column carries a configured per-column
    /// editor (EditorKind &lt;&gt; veNone).</summary>
    function ColumnWantsEditor(AColumn: TColumn): Boolean;

    // Incremental search helpers
    function IncrementalSearchFieldName: string;
    function IncrementalSearchTimedOut: Boolean;
    function GridIsEditable: Boolean;
    procedure RunIncrementalSearch;
    procedure ResetSearchBuffer;
    procedure UpdateSearchHint(const AText: string);
    procedure DoIncrementalSearch(const SearchText: string; Found: Boolean);
    function LocateByPrefix(const AFieldName, AText: string): Boolean;
    function FallbackScanForPrefix(const AFieldName, AText: string): Boolean;
    procedure UpdateSelectionFooterSync;

    // Saved filter sets
    procedure EnsureFilterSetListLoaded;
    function GetFilterSetList: TVittixFilterSetList;
    function ResolveFilterSetsFileName: string;
    procedure StoreSetInEngine(ASet: TVittixFilterSet);
    procedure ReapplyActiveFilterSetToEngine;
    procedure UpdateFilterSetHint;
    procedure DoFilterSetApplied(const AName: string);
    procedure DoFilterSetsChanged;

    // Server-side provider routing
    function FilterMode: TVittixFilterMode;
    function CollectFieldInfos: TArray<TVittixSQLFieldInfo>;
    function CollectSortColumns: TArray<TVittixSortColumn>;
    procedure DoProviderFallback(const AReason: string);
    procedure DoProviderError(E: Exception; var Handled: Boolean);
    procedure UpdateServerHint(const AText: string);
    procedure RestoreClientFiltering(const AReason: string;
      AFireFallback: Boolean);
    procedure DeactivateServerState;
    procedure SetProvider(const Value: IVittixDataProvider);
    procedure ApplyClientFiltering;

    procedure SetGrid(const Value: TDBGrid);
    procedure SetActive(const Value: Boolean);
    procedure SetShowFooter(const Value: Boolean);

    procedure HookGrid;
    procedure UnhookGrid;
    procedure HookDataSource;
    procedure UnhookDataSource;

    // Rebinds the FDataset field with matching FreeNotification bookkeeping
    // (the dataset usually outlives the controller's knowledge of it).
    procedure ReplaceDatasetPointer(ADataSet: TDataSet);

    procedure DataLinkActiveChanged;
    procedure DataLinkDataSetChanged;
    procedure DataLinkRecordChanged(Field: TField);

    procedure CreateEngines;
    procedure DestroyEngines;

    procedure GridWindowProc(var Message: TMessage);

    procedure SetAggregationDirty;

    // Event dispatch helpers (no-ops without handlers)
    procedure DoAfterSort(Column: TColumn);
    procedure DoFilterApplied(const FieldName: string);
    procedure DoAfterApplyLayout;

    function GetOnValidateFilter: TFilterValidationEvent;
    procedure SetOnValidateFilter(const Value: TFilterValidationEvent);
    function GetOnFormatAggregation: TFormatAggregationEvent;
    procedure SetOnFormatAggregation(const Value: TFormatAggregationEvent);
    function GetOnFieldValidation: TFieldValidationEvent;
    procedure SetOnFieldValidation(const Value: TFieldValidationEvent);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ExecuteFilterPopup(Column: TColumn): Boolean; virtual;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Recomputes the footer aggregates on the server provider
    /// (one aggregate SELECT against the active server WHERE). Returns
    /// False when the server path was not taken (no provider, no filter,
    /// provider decline/failure) - the caller then falls back to the
    /// client scan. Public so hosts can force a footer refresh after bulk
    /// data changes; also exercised directly by the provider regression
    /// tests.</summary>
    function ServerAggregateRefresh: Boolean;

    // Integration points called from TVittixDBGrid's virtual overrides.
    // DoMouseDown/DoDblClick/DoKeyDown/DoKeyPress return True when the event
    // was consumed and must not reach inherited VCL processing.
    procedure DoTitleClick(Column: TColumn);
    procedure DoDrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState);
    function DoMouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer): Boolean;
    function DoDblClick: Boolean;
    function DoKeyDown(var Key: Word; Shift: TShiftState): Boolean;
    function DoKeyPress(var Key: Char): Boolean;

    /// <summary>True when clipboard shortcuts must not be consumed by the
    /// grid copy because the in-place editor (or a pending dataset edit)
    /// owns them: EditorMode means the editor is up and its native
    /// copy/cut/paste/select-all must win; an edit/insert state without the
    /// editor leaves the keys unconsumed, which is still safer than the
    /// grid copy walking bookmarks (GotoBookmark would implicitly post the
    /// pending edit). Exposed for tests.</summary>
    class function ShouldBypassClipboardShortcut(AEditorMode: Boolean;
      AState: TDataSetState): Boolean; static;

    /// <summary>Current incremental search buffer (type-ahead locate). Empty
    /// when no search is in progress. Exposed for status displays and tests.
    /// </summary>
    property IncrementalSearchBuffer: string read FSearchBuffer;

    // Called by TVittixDBGrid (chooser moves, VCL ColumnMoved) after a
    // column's display position changed; raises OnColumnMoved.
    procedure DoColumnMoved(Column: TColumn; OldIndex, NewIndex: Integer);

    /// <summary>Unhooks the grid before its destruction.</summary>
    procedure Detach;
    /// <summary>Installs the runtime window hook after a handle is created.
    /// Called by the grid; has no effect in the form designer.</summary>
    procedure InstallWindowProc;
    /// <summary>Reattaches runtime hooks after a grid reload.</summary>
    procedure RehookGrid;
    /// <summary>Realigns the runtime footer to the current grid layout.</summary>
    procedure GridLayoutChanged;

    /// <summary>Recalculates dirty footer aggregates when the dataset is
    /// active, then repaints the grid and footer.</summary>
    procedure Refresh;
    /// <summary>Clears filters and sorting, then refreshes aggregates.</summary>
    procedure Clear;
    /// <summary>Applies the current sort state and refreshes aggregates.
    /// Filter changes are routed through RefreshFiltering.</summary>
    procedure ApplyState;
    /// <summary>Opens the column chooser for the attached grid.</summary>
    procedure ShowColumnChooser;
    /// <summary>Sets the global search text used alongside column filters
    /// and refreshes the active filter route.</summary>
    procedure SetGlobalFilter(const Text: string);
    /// <summary>Removes column, global, and saved-set filters.</summary>
    procedure ClearFilters;
    /// <summary>Sets a column's footer aggregation and refreshes its
    /// displayed value. An unknown column has no effect.</summary>
    procedure SetColumnAggregation(Column: TColumn; Aggregation: TVittixAggregationType);
    /// <summary>Copies the current grid layout into State. Nil State is a
    /// no-op.</summary>
    procedure CaptureLayout(State: TVittixDBGridLayoutState);
    /// <summary>Restores a captured layout to the attached grid.</summary>
    procedure ApplyLayout(State: TVittixDBGridLayoutState);
    /// <summary>True while the controller is applying a guarded update.</summary>
    function IsUpdating: Boolean;
    /// <summary>Writes the current layout as JSON to Stream. Nil Stream is
    /// a no-op.</summary>
    procedure SaveLayoutToStream(Stream: TStream);
    /// <summary>Reads layout JSON from Stream and applies it. Nil Stream is
    /// a no-op.</summary>
    procedure LoadLayoutFromStream(Stream: TStream);
    /// <summary>Atomically saves layout JSON. An empty FileName uses
    /// LayoutStorageFileName, then PersistenceRootPath\layout.json; raises
    /// EVittixLayoutError if none is configured.</summary>
    procedure SaveLayoutToFile(const FileName: string = '');
    /// <summary>Loads layout JSON. An empty FileName uses
    /// LayoutStorageFileName, then PersistenceRootPath\layout.json; a missing
    /// file is ignored, but no configured path raises EVittixLayoutError.
    /// </summary>
    procedure LoadLayoutFromFile(const FileName: string = '');
    /// <summary>Restores column order, visibility, and widths, clears
    /// sorting/filtering and footer aggregates, and shows the footer.</summary>
    procedure ResetLayout;

    /// <summary>Finds a grid column by its FieldName; nil when not found.
    /// Public helper used by the export engine and tests.</summary>
    function FindColumnByFieldName(const FieldName: string): TColumn;

    /// <summary>Copies the current cell (or the selected rows when
    /// dgMultiSelect holds more than one row) to the clipboard as TSV.
    /// AIncludeHeaders prepends the visible-column header row regardless of
    /// the grid's CopyIncludeHeaders default.</summary>
    procedure CopySelectionToClipboard(AIncludeHeaders: Boolean);

    /// <summary>Saves the current per-column filters (AND-combined) as one
    /// named filter set and makes it the active set. Persists to the filter
    /// set file when one can be resolved.</summary>
    procedure SaveCurrentFilterAs(const AName: string);

    /// <summary>Applies a saved filter set by (case-insensitive) name.
    /// Returns False when no set carries the name (the layout restore uses
    /// that to skip silently). Conditions within the stored groups are
    /// ANDed, groups are ORed; the first group is mirrored to the per-column
    /// filter texts so titles, popup and exports stay consistent.</summary>
    function ApplyFilterSet(const AName: string): Boolean;

    /// <summary>Deletes a saved filter set; clears the active set name when
    /// it is the deleted one. Returns False when the name is unknown.</summary>
    function DeleteFilterSet(const AName: string): Boolean;

    /// <summary>Names of the saved filter sets, in stored order.</summary>
    function FilterSetNames: TArray<string>;

    /// <summary>Saves the filter set list to the resolved file; a no-op
    /// when no file can be resolved. Called after direct edits to the
    /// FilterSetList (the manager dialog edits it in place).</summary>
    procedure PersistFilterSets;

    /// <summary>Name of the currently applied filter set ('' when none).
    /// Survives dataset close/reopen cycles.</summary>
    property ActiveFilterSetName: string read FActiveFilterSetName;

    /// <summary>Lazily loaded saved filter sets (for the manager dialog).
    /// </summary>
    property FilterSetList: TVittixFilterSetList read GetFilterSetList;

    /// <summary>Application-supplied server provider. Assigning it does
    /// not switch modes by itself: the grid's FilterMode decides. When a
    /// provider is swapped while the server path is active, the old one
    /// restores the dataset's original SQL first.</summary>
    property Provider: IVittixDataProvider read FProvider write SetProvider;

    /// <summary>Routes the currently active filter conditions (column
    /// texts, applied filter set groups, global search) to the server
    /// provider or the client engine according to FilterMode. Called by
    /// every filter entry point; applications changing conditions directly
    /// can call it after their changes.</summary>
    procedure RefreshFiltering;

    /// <summary>Routes the current column sort state to the server
    /// provider or the client sort engine according to FilterMode. Called
    /// by the sort entry points (title click, ApplyState, layout).</summary>
    procedure RefreshSorting;

    /// <summary>True while the server provider owns the WHERE clause.
    /// </summary>
    property ServerFilterActive: Boolean read FServerFilterActive;

    /// <summary>True while the server provider owns the ORDER BY.
    /// </summary>
    property ServerSortActive: Boolean read FServerSortActive;

    /// <summary>Fired when server-side filtering fell back to the client
    /// engine. Shares storage with the grid's OnProviderFallback.</summary>
    property OnProviderFallback: TVittixProviderFallbackEvent
      read FOnProviderFallback write FOnProviderFallback;

    /// <summary>Fired when the provider's re-query raised. Shares storage
    /// with the grid's OnProviderError.</summary>
    property OnProviderError: TVittixProviderErrorEvent
      read FOnProviderError write FOnProviderError;

    /// <summary>File the filter sets persist to. Empty falls back to
    /// PersistenceRootPath\filtersets.json; empty both means in-memory
    /// only.</summary>
    property FilterSetsFileName: string read FFilterSetsFileName
      write FFilterSetsFileName;

    /// <summary>Returns the footer text that should appear for a column
    /// identified by field name: the aggregation display text when an
    /// aggregation is configured, otherwise the column's FooterText, or ''
    /// when neither is set. Used by the export engine when IncludeFooter is
    /// enabled.</summary>
    function FooterDisplayText(const FieldName: string): string;

    // Called by TVittixDBGrid when its DataSource property changes
    procedure DataSourceChanged;

    /// <summary>Active client filter engine, or nil before runtime engine
    /// creation.</summary>
    property FilterEngine: TVittixDBGridFilterEngine read FFilterEngine;
    /// <summary>Active client sort engine, or nil before runtime engine
    /// creation.</summary>
    property SortEngine: TVittixDBGridSortEngine read FSortEngine;
    /// <summary>Footer aggregation engine, or nil before runtime engine
    /// creation.</summary>
    property AggregationEngine: TVittixDBGridAggregationEngine read FAggregationEngine;

    /// <summary>Grid attached to this controller.</summary>
    property Grid: TDBGrid read FGrid write SetGrid;
    /// <summary>Default layout JSON path for SaveLayoutToFile and
    /// LoadLayoutFromFile.</summary>
    property LayoutStorageFileName: string read FLayoutStorageFileName write FLayoutStorageFileName;
    /// <summary>Root directory for default persistence files.</summary>
    property PersistenceRootPath: string read FPersistenceRootPath write FPersistenceRootPath;

    // Notification events. The controller is the implementation owner;
    // TVittixDBGrid re-publishes the first four as convenient surfaces over
    // this same storage (assigning either side reaches the same handlers,
    // so an operation fires exactly once). Sender is the controller.
    property OnAfterSort: TVittixAfterSortEvent
      read FOnAfterSort write FOnAfterSort;
    /// <summary>Handler for failed interactive sorts. Without a handler the
    /// underlying EVittixSortError is re-raised.</summary>
    property OnSortError: TVittixSortErrorEvent
      read FOnSortError write FOnSortError;
    property OnFilterApplied: TVittixFilterAppliedEvent
      read FOnFilterApplied write FOnFilterApplied;
    property OnAfterApplyLayout: TNotifyEvent
      read FOnAfterApplyLayout write FOnAfterApplyLayout;
    property OnColumnMoved: TVittixColumnMovedEvent
      read FOnColumnMoved write FOnColumnMoved;

    /// <summary>Fired after an incremental search keystroke; SearchText is
    /// the active buffer ('' after a reset) and Found reports a locate hit.
    /// The grid re-publishes this as OnIncrementalSearch over the same
    /// storage, so an assignment on either surface fires exactly once.</summary>
    property OnIncrementalSearch: TVittixIncrementalSearchEvent
      read FOnIncrementalSearch write FOnIncrementalSearch;

    /// <summary>Fired after a saved filter set was applied. The grid
    /// re-publishes this as OnFilterSetApplied over the same storage.</summary>
    property OnFilterSetApplied: TVittixFilterSetAppliedEvent
      read FOnFilterSetApplied write FOnFilterSetApplied;

    /// <summary>Fired after the saved filter sets changed (save, delete,
    /// rename, reorder). The grid re-publishes this as
    /// OnFilterSetsChanged over the same storage.</summary>
    property OnFilterSetsChanged: TNotifyEvent
      read FOnFilterSetsChanged write FOnFilterSetsChanged;

    /// <summary>Per-column validation for every Vittix popup editor; runs
    /// after the built-in Required/Min/Max rules. The grid re-publishes
    /// this as OnValidateCell over the same storage.</summary>
    property OnValidateCell: TVittixValidateCellEvent
      read FOnValidateCell write FOnValidateCell;

    // Engine callbacks, previously unreachable (the engines are private).
    // Handlers are cached and pushed to the engines when they (re)appear.
    property OnValidateFilter: TFilterValidationEvent
      read GetOnValidateFilter write SetOnValidateFilter;
    property OnFormatAggregation: TFormatAggregationEvent
      read GetOnFormatAggregation write SetOnFormatAggregation;
    property OnFieldValidation: TFieldValidationEvent
      read GetOnFieldValidation write SetOnFieldValidation;

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

uses
  // Implementation-only on purpose: TVittixDBGrid needs this unit in its
  // interface (strongly typed Controller property), so this unit must not
  // reference Vittix.DBGrid in its own interface section.
  System.Character,
  System.StrUtils,
  Vittix.DBGrid,
  Vittix.DBGrid.Export.Engine;

function VittixGrid(AGrid: TDBGrid): TVittixDBGrid;
begin
  // SetGrid guarantees the type; the guard keeps the helper nil-safe for
  // teardown windows where the grid reference is already cleared.
  if AGrid is TVittixDBGrid then
    Result := TVittixDBGrid(AGrid)
  else
    Result := nil;
end;

function IsAsciiText(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if Ord(S[I]) >= $80 then
      Exit(False);
  Result := True;
end;

function UpperCaseUnicode(const S: string): string;
var
  I: Integer;
begin
  // Per-character TCharHelper.ToUpper applies Unicode simple case mapping —
  // accented Latin, Cyrillic and Greek fold correctly and the caseless
  // Indic scripts pass through unchanged. UpperCase would only fold ASCII.
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    Result[I] := S[I].ToUpper;
end;

{ TVittixGridDataLink }

constructor TVittixGridDataLink.Create(AController: TVittixDBGridController);
begin
  inherited Create;
  FController := AController;
end;

procedure TVittixGridDataLink.ActiveChanged;
begin
  inherited;
  if Assigned(FController) then
    FController.DataLinkActiveChanged;
end;

procedure TVittixGridDataLink.DataSetChanged;
begin
  inherited;
  if Assigned(FController) then
    FController.DataLinkDataSetChanged;
end;

procedure TVittixGridDataLink.RecordChanged(Field: TField);
begin
  inherited;
  if Assigned(FController) then
    FController.DataLinkRecordChanged(Field);
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
  FDataLink := TVittixGridDataLink.Create(Self);
  FLastSelectionCount := -1;
  FSearchWatch := TStopwatch.StartNew;
end;

destructor TVittixDBGridController.Destroy;
begin
  // Release the provider FIRST: its destructor may re-open the dataset,
  // which walks the data links - they must still be alive (and the grid
  // not half-torn-down) when the original SQL is restored.
  FProvider := nil;
  UnhookGrid;
  FreeAndNil(FDataLink);
  // The controller (owned by the grid) can die while the dataset it
  // watches lives on; without this Remove the dataset's later
  // destruction fires a notification into the freed controller.
  if Assigned(FDataset) then
    FDataset.RemoveFreeNotification(Self);
  DestroyEngines;
  FreeAndNil(FFilterSetList);
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

  // First runtime handle creation is the earliest point where the grid has a
  // stable window and client metrics. Force one footer sync here so startup
  // alignment matches the final column layout without waiting for a later
  // resize or interaction.
  GridLayoutChanged;
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
      UnhookDataSource
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
      GridLayoutChanged;
    end;
  finally
    FUpdating := False;
  end;
end;

{ ============================================================================= }
{ GRID / DATASET HOOKING }
{ ============================================================================= }

procedure TVittixDBGridController.SetGrid(const Value: TDBGrid);
begin
  // The controller reaches TVittixDBGrid-specific members (ColumnInfo,
  // footer plumbing, geometry helpers), so reject other grids up front.
  if (Value <> nil) and not (Value is TVittixDBGrid) then
    raise EArgumentException.Create(
      'TVittixDBGridController requires a TVittixDBGrid instance');

  if FGrid = Value then Exit;

  UnhookGrid;
  if Assigned(FGrid) then
    RemoveFreeNotification(FGrid);
  FGrid := Value;

  if not Assigned(FGrid) then Exit;

  // The grid is typically not owned by the controller; without the
  // notification the controller keeps a dangling FGrid pointer when the
  // grid's owner destroys it first.
  FGrid.FreeNotification(Self);

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

  // Secondary runtime check: never hook when not fully constructed.
  if csLoading in FGrid.ComponentState then Exit;

  // Grid input/draw integration no longer hooks events: TVittixDBGrid's
  // virtual overrides call the DoXxx methods directly, so application event
  // assignments can never unhook us.

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
var
  CurrentProc, OurProc: TWndMethod;
begin
  UnhookDataSource;

  if not Assigned(FGrid) then Exit;

  // CRITICAL: Only restore the WindowProc when the hook was actually
  // installed. HookGrid skips the hook when the grid handle is not yet
  // allocated, leaving FOldWindowProc nil. Assigning that nil back here
  // would wipe out the grid's real FWindowProc, so the next Perform
  // (e.g. from TWinControl.Invalidate during a layout change) calls a nil
  // method pointer -> EAccessViolation at address 00000000.
  if Assigned(FOldWindowProc) then
  begin
    CurrentProc := FGrid.WindowProc;
    OurProc := GridWindowProc;
    // A later hook owns the chain; we cannot safely splice its saved method.
    // Such hooks must detach before this controller is destroyed.
    if (TMethod(CurrentProc).Code = TMethod(OurProc).Code) and
       (TMethod(CurrentProc).Data = TMethod(OurProc).Data) then
      FGrid.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
  end;
end;

procedure TVittixDBGridController.RehookGrid;
begin
  if not Assigned(FGrid) then Exit;
  if csDesigning in FGrid.ComponentState then Exit;
  if csLoading in FGrid.ComponentState then Exit;

  UnhookGrid;
  HookGrid;
end;

procedure TVittixDBGridController.ReplaceDatasetPointer(ADataSet: TDataSet);
begin
  if FDataset = ADataSet then Exit;
  if Assigned(FDataset) then
    RemoveFreeNotification(FDataset);
  FDataset := ADataSet;
  if Assigned(FDataset) then
    FDataset.FreeNotification(Self);
end;

procedure TVittixDBGridController.HookDataSource;
begin
  if not Assigned(FGrid) or not Assigned(FGrid.DataSource) then Exit;

  FDataLink.DataSource := FGrid.DataSource;
  ReplaceDatasetPointer(FDataLink.DataSet);
  if not Assigned(FDataset) then Exit;
  if FDataset.Active then
    DataLinkActiveChanged;
end;

procedure TVittixDBGridController.UnhookDataSource;
begin
  if Assigned(FDataLink) then
    FDataLink.DataSource := nil;
  if Assigned(FDataset) then
    RemoveFreeNotification(FDataset);
  FDataset := nil;
end;

procedure TVittixDBGridController.DataLinkActiveChanged;
begin
  if not Assigned(FDataLink) then Exit;
  if FAggregationBusy then Exit;
  ReplaceDatasetPointer(FDataLink.DataSet);
  if Assigned(FDataset) and FDataset.Active then
    CreateEngines
  else
    DestroyEngines;

  if Assigned(FDataset) and FDataset.Active then
  begin
    SetAggregationDirty;
    Refresh;
    GridLayoutChanged;
  end;
end;

procedure TVittixDBGridController.DataLinkDataSetChanged;
begin
  if FAggregationBusy then Exit;
  // deDataSetChange arrives not only for real dataset swaps but also for
  // EnableControls and Filtered changes on the SAME open dataset. Tearing
  // the engines down for those notifications destroyed the engine that just
  // applied a filter (its destructor resets Filtered and unhooks
  // OnFilterRecord). Rebuild only when the dataset identity or state really
  // changed; otherwise the engines self-heal (field-cache stale guard) and
  // the footer just needs a recalculation.
  if Assigned(FDataLink) and Assigned(FDataset) and
     (FDataLink.DataSet = FDataset) and FDataset.Active then
  begin
    SetAggregationDirty;
    Refresh;
    Exit;
  end;

  ReplaceDatasetPointer(nil);
  DestroyEngines;

  if Assigned(FDataLink) then
    ReplaceDatasetPointer(FDataLink.DataSet);

  if Assigned(FDataset) and FDataset.Active then
    DataLinkActiveChanged;
end;

procedure TVittixDBGridController.DataLinkRecordChanged(Field: TField);
begin
  if not Assigned(FDataset) then
    Exit;

  if FDataset.State in dsEditModes then
    Exit;

  if FAggregationBusy then
    Exit;

  SetAggregationDirty;
  Refresh;
  // A server filter change requested during an edit applies now.
  if FPendingServerRoute then
    RefreshFiltering;
end;

{ ============================================================================= }
{ ENGINES }
{ ============================================================================= }

procedure TVittixDBGridController.CreateEngines;
begin
  if FEnginesCreated or not IsReady then Exit;

  FSortEngine :=
    TVittixDBGridSortEngine.Create(FDataset, VittixGrid(FGrid).ColumnInfo);

  FFilterEngine :=
    TVittixDBGridFilterEngine.Create(FDataset, VittixGrid(FGrid).ColumnInfo);

  FAggregationEngine :=
    TVittixDBGridAggregationEngine.Create(FDataset, VittixGrid(FGrid).ColumnInfo);

  // Engines are recreated on dataset churn — re-attach the cached engine
  // event handlers so assignments made earlier are not lost.
  FSortEngine.OnFieldValidation := FOnFieldValidation;
  FFilterEngine.OnValidateFilter := FOnValidateFilter;
  FAggregationEngine.OnFormatAggregation := FOnFormatAggregation;

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
  // The active filter set name survives dataset churn: re-bind its groups
  // to the freshly created engine, then re-route to the server provider
  // when the grid is in a server mode.
  ReapplyActiveFilterSetToEngine;
  RefreshFiltering;
  RefreshSorting;
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
  // Call original window proc first
  if Assigned(FOldWindowProc) then
    FOldWindowProc(Message);

  if (Message.Msg = WM_PAINT) or (Message.Msg = WM_SIZE) or
     (Message.Msg = WM_HSCROLL) or (Message.Msg = WM_VSCROLL) or
     (Message.Msg = WM_WINDOWPOSCHANGED) or
     (Message.Msg = CM_FONTCHANGED) or (Message.Msg = CM_VISIBLECHANGED) then
  begin
    if Assigned(FFooterPanel) then
      FFooterPanel.SyncLayout;
  end;
end;

procedure TVittixDBGridController.DoDrawColumnCell(
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  IsOddRow: Boolean;
  Info: TVittixDBGridColumnInfo;
  Cond: TVittixDBGridCellCondition;
  FieldValue: string;
  I: Integer;
begin
  // Called from TVittixDBGrid.DrawColumnCell BEFORE the application handler
  // or default drawing — only tweaks Brush/Font, never paints.

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
      // FIX BUG (RecNo): RecNo can return -1 for datasets that don't support
      // it (e.g., server-side cursors, some query-based datasets). Odd(-1)
      // returns True, which would incorrectly apply the alternate color to
      // ALL rows. RecNo can also be 0 in some states (BOF/empty). Validate
      // RecNo > 0 before using it.
      if FGrid.DataSource.DataSet.RecNo > 0 then
      begin
        IsOddRow := Odd(FGrid.DataSource.DataSet.RecNo);
        if IsOddRow then
          FGrid.Canvas.Brush.Color := FAlternateRowColor;
      end;
    end;
  end;

  Info := VittixGrid(FGrid).ColumnInfoByColumn(Column);
  if Assigned(Info) and (Info.CellConditions.Count > 0) and
     Assigned(FGrid.DataSource) and Assigned(FGrid.DataSource.DataSet) and
     (not (gdSelected in State)) and (not (gdFixed in State)) then
  begin
    // FIX BUG (FieldByName): FieldByName does a linear search through fields
    // on every cell draw (performance issue) and raises an exception if the
    // field doesn't exist (crash risk). Column.Field is already available and
    // directly references the field object without any lookup.
    if Assigned(Column.Field) then
      FieldValue := Column.Field.AsString
    else
      FieldValue := '';
    for I := 0 to Info.CellConditions.Count - 1 do
    begin
      Cond := Info.CellConditions[I];
      if Cond.Matches(FieldValue) then
      begin
        if Cond.BackgroundColor <> clNone then
          FGrid.Canvas.Brush.Color := Cond.BackgroundColor;
        if Cond.FontColor <> clNone then
          FGrid.Canvas.Font.Color := Cond.FontColor;
        Break;
      end;
    end;
  end;

  // The footer shows selection aggregates when rows are selected; the grid
  // does not notify the footer on selection changes, so piggyback on the
  // cell draws every selection change causes.
  UpdateSelectionFooterSync;
end;

{ ============================================================================= }
{ GRID EVENT INTEGRATION (called from TVittixDBGrid virtual overrides) }
{ ============================================================================= }

procedure TVittixDBGridController.DoTitleClick(Column: TColumn);
begin
  if Assigned(FSortEngine) then
  begin
    // In server/auto modes the client engine never writes IndexFieldNames:
    // a leftover client index would re-sort every re-opened result
    // client-side and silently override the server ORDER BY. ToggleSort
    // still maintains the column state for the UI arrows and fallbacks.
    FSortEngine.ClientApplyEnabled := FilterMode = vcmClient;
    try
      FSortEngine.ToggleSort(
        Column,
        (GetKeyState(VK_CONTROL) and $8000) <> 0
      );
    except
      on E: EVittixSortError do
      begin
        // A header click on a dataset that cannot sort must not crash the
        // application: surface the error to a handler when one is assigned,
        // otherwise keep the failure loud. ToggleSort already rolled the
        // column sort state back.
        if Assigned(FOnSortError) then
        begin
          FOnSortError(Self, Column, E);
          Exit;
        end;
        raise;
      end;
    end;
    if FilterMode <> vcmClient then
      RefreshSorting;
    SetAggregationDirty;
    Refresh;
    // Reached only when ToggleSort applied without raising.
    DoAfterSort(Column);
  end;
end;

function TVittixDBGridController.ExecuteFilterPopup(Column: TColumn): Boolean;
begin
  Result := TVittixDBGridFilterPopup.Execute(
    FGrid, FindInfoByColumn(Column), FOnValidateFilter);
end;

function TVittixDBGridController.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
var
  Coord: TGridCoord;
  ColIndex: Integer;
  Col: TColumn;
begin
  Result := False;
  if not Assigned(FGrid) then Exit;

  Coord := FGrid.MouseCoord(X, Y);

  // Right-click on the title row: filter popup / column chooser
  if (dgTitles in FGrid.Options) and (Coord.Y = 0) and (Button = mbRight) then
  begin
    if ssCtrl in Shift then
    begin
      ShowColumnChooser;
      Exit(True);
    end;

    ColIndex := Coord.X - VittixGrid(FGrid).GetIndicatorOffset;
    if (ColIndex >= 0) and (ColIndex < FGrid.Columns.Count) then
    begin
      Col := FGrid.Columns[ColIndex];
      // Live in-popup validation through the same handler the engine uses
      // at apply time, when one is assigned.
      if Assigned(Col) and
         ExecuteFilterPopup(Col) then
      begin
        if Assigned(FFilterEngine) then
        begin
          RefreshFiltering;
          if not FServerFilterActive then
            FFilterEngine.Active := True;
          SetAggregationDirty;
          Refresh;
          DoFilterApplied(Col.FieldName);
        end;
        Exit(True);
      end;
      Exit(True);
    end;
  end;
end;

function TVittixDBGridController.DoDblClick: Boolean;
var
  Field: TField;
  Column: TColumn;
begin
  Result := False;
  Field := nil;
  if Assigned(FGrid) then
    Field := FGrid.SelectedField;
  Column := FindColumnByField(Field);

  if Assigned(Column) and Assigned(Field) and
     (ColumnWantsEditor(Column) or
      (Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo, ftDate, ftTime, ftDateTime])) and
     TVittixDBGridEditors.EditField(FGrid, Column, FOnValidateCell) then
  begin
    Refresh;
    Result := True; // consumed: the field editor handled it
  end;
end;

class function TVittixDBGridController.ShouldBypassClipboardShortcut(
  AEditorMode: Boolean; AState: TDataSetState): Boolean;
begin
  Result := AEditorMode or (AState in dsEditModes);
end;

function TVittixDBGridController.DoKeyDown(var Key: Word;
  Shift: TShiftState): Boolean;
var
  Field: TField;
  Column: TColumn;
  State: TDataSetState;
begin
  Result := False;

  // Copy selection (see CopySelectionToClipboard): Ctrl+C honors the grid's
  // CopyIncludeHeaders default, Ctrl+Shift+C always includes the header row.
  // While an edit is in progress the clipboard shortcuts belong to the
  // in-place editor's native handling (the focused editor normally consumes
  // them before the grid sees this; the guard covers routed/Perform'ed keys).
  if Assigned(FDataset) then
    State := FDataset.State
  else
    State := dsInactive;
  if Assigned(FGrid) and (Key = Ord('C')) and
     ([ssCtrl] * Shift = [ssCtrl]) and
     not ShouldBypassClipboardShortcut(FGrid.EditorMode, State) then
  begin
    CopySelectionToClipboard(ssShift in Shift);
    Key := 0;
    Exit(True);
  end;

  if Key <> VK_F2 then Exit;

  Field := nil;
  if Assigned(FGrid) then
    Field := FGrid.SelectedField;
  Column := FindColumnByField(Field);

  if Assigned(Column) and Assigned(Field) and
     (ColumnWantsEditor(Column) or
      (Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo, ftDate, ftTime, ftDateTime])) and
     TVittixDBGridEditors.EditField(FGrid, Column, FOnValidateCell) then
  begin
    Refresh;
    Result := True;
  end;
end;

function TVittixDBGridController.ColumnWantsEditor(AColumn: TColumn): Boolean;
var
  Info: TVittixDBGridColumnInfo;
begin
  // A configured per-column editor opens regardless of the field type;
  // the dispatcher itself decides what the editor shows.
  Info := FindInfoByColumn(AColumn);
  Result := Assigned(Info) and (Info.EditorKind <> veNone);
end;

function TVittixDBGridController.GridIsEditable: Boolean;
var
  Grid: TVittixDBGrid;
  Col: TColumn;
begin
  // "Editable" mirrors what TDBGrid needs to enter in-place editing: the
  // editing option, a writable grid/column and a modifiable dataset. Any
  // one of them being read-only means typing cannot start an edit, so
  // ismAuto may run the type-ahead search.
  Result := False;
  Grid := VittixGrid(FGrid);
  if not Assigned(Grid) then Exit;
  if not (dgEditing in Grid.Options) then Exit;
  if Grid.ReadOnly then Exit;
  if Assigned(Grid.SelectedField) then
  begin
    if Grid.SelectedField.ReadOnly then Exit;
    Col := FindColumnByField(Grid.SelectedField);
    if Assigned(Col) and Col.ReadOnly then Exit;
  end;
  if not Assigned(FDataset) or not FDataset.CanModify then Exit;
  Result := True;
end;

function TVittixDBGridController.DoKeyPress(var Key: Char): Boolean;
var
  Grid: TVittixDBGrid;
begin
  Result := False;
  Grid := VittixGrid(FGrid);
  if not Assigned(Grid) or not Grid.IncrementalSearch then Exit;
  if Grid.IncrementalSearchMode = ismOff then Exit;
  // ismAuto keeps printable keys for in-place editing on an editable grid;
  // ismAlways keeps the pre-2.1 behavior of always consuming them.
  if (Grid.IncrementalSearchMode = ismAuto) and GridIsEditable then Exit;

  // A closed dataset must never see a search; any pending buffer is stale.
  if not Assigned(FDataset) or not FDataset.Active then
  begin
    if FSearchBuffer <> '' then
      ResetSearchBuffer;
    Exit;
  end;

  // In-place editing owns the keyboard: never steal characters while the
  // dataset is in an edit/insert state (the grid's KeyPress override skips
  // delegation while the in-place editor itself is open).
  if FDataset.State in dsEditModes then Exit;

  if Key = #27 then
  begin
    // Esc clears the buffer. With nothing accumulated, Esc keeps its default
    // grid behavior (e.g. canceling an edit).
    if FSearchBuffer = '' then Exit;
    ResetSearchBuffer;
    Key := #0;
    Result := True;
    Exit;
  end;

  if Key = #8 then
  begin
    if FSearchBuffer = '' then Exit;
    SetLength(FSearchBuffer, Length(FSearchBuffer) - 1);
    // Backspace granularity matches the Windows EDIT control: one UTF-16
    // code unit per keystroke — except that a trailing low surrogate pulls
    // its high surrogate with it, so half of a surrogate pair never
    // survives in the buffer. Combining marks (Indic matras, virama,
    // nukta) are one Char each by the same rule.
    if (Length(FSearchBuffer) > 0) and
       (Ord(FSearchBuffer[Length(FSearchBuffer)]) >= $D800) and
       (Ord(FSearchBuffer[Length(FSearchBuffer)]) <= $DBFF) then
      SetLength(FSearchBuffer, Length(FSearchBuffer) - 1);
    RunIncrementalSearch;
    Key := #0;
    Result := True;
    Exit;
  end;

  // Printable characters only; control characters (Tab, Enter, Ctrl+letter
  // combos) keep their grid semantics.
  if Key < #32 then Exit;

  // An expired buffer starts a fresh search instead of growing stale text.
  if (FSearchBuffer <> '') and IncrementalSearchTimedOut then
    FSearchBuffer := '';
  FSearchBuffer := FSearchBuffer + Key;
  FSearchWatch := TStopwatch.StartNew;

  // A high surrogate is half a code point: locating with a lone half is a
  // guaranteed miss, so wait for the completing low surrogate (the next
  // WM_CHAR) before searching. BMP characters — including Indic text
  // injected per character by keyboard hooks — search immediately.
  if (Ord(Key) >= $D800) and (Ord(Key) <= $DBFF) then
  begin
    Key := #0;
    Result := True;
    Exit;
  end;

  RunIncrementalSearch;

  // The character was consumed whether or not the search matched; a failed
  // search keeps the buffer so Backspace can recover, and Locate leaves the
  // cursor on the current record.
  Key := #0;
  Result := True;
end;

procedure TVittixDBGridController.ResetSearchBuffer;
begin
  if FSearchBuffer = '' then Exit;
  FSearchBuffer := '';
  UpdateSearchHint('');
  DoIncrementalSearch('', False);
end;

procedure TVittixDBGridController.RunIncrementalSearch;
var
  FieldName: string;
  Found: Boolean;
begin
  if FSearchBuffer = '' then
  begin
    // Fully erased buffer: clear the hint and stop searching.
    UpdateSearchHint('');
    DoIncrementalSearch('', False);
    Exit;
  end;

  FieldName := IncrementalSearchFieldName;
  Found := (FieldName <> '') and LocateByPrefix(FieldName, FSearchBuffer);
  if Found then
    UpdateSearchHint('Find: ' + FSearchBuffer)
  else
    UpdateSearchHint('Find: ' + FSearchBuffer + ' (no match)');
  DoIncrementalSearch(FSearchBuffer, Found);
end;

function TVittixDBGridController.IncrementalSearchFieldName: string;
var
  Grid: TVittixDBGrid;
  Info: TVittixDBGridColumnInfo;
  Best: TVittixDBGridColumnInfo;
  I: Integer;
begin
  Result := '';
  Grid := VittixGrid(FGrid);
  if not Assigned(Grid) then Exit;

  // The primary sort column leads the search: the user reads the data in
  // that order, so type-ahead must locate within it.
  Best := nil;
  if Assigned(Grid.ColumnInfo) then
    for I := 0 to Grid.ColumnInfo.Count - 1 do
    begin
      Info := Grid.ColumnInfo[I];
      if (Info.SortOrder <> vsoNone) and
         ((Best = nil) or (Info.SortIndex < Best.SortIndex)) then
        Best := Info;
    end;

  if Assigned(Best) then
    Exit(Best.FieldName);

  // Unsorted: search the current column.
  if Assigned(Grid.SelectedField) and (Grid.SelectedField.FieldName <> '') then
    Exit(Grid.SelectedField.FieldName);

  // Last resort: the first column bound to a field.
  for I := 0 to Grid.Columns.Count - 1 do
    if Grid.Columns[I].FieldName <> '' then
      Exit(Grid.Columns[I].FieldName);
end;

function TVittixDBGridController.IncrementalSearchTimedOut: Boolean;
var
  Grid: TVittixDBGrid;
begin
  Grid := VittixGrid(FGrid);
  if not Assigned(Grid) then
    Exit(False);
  // A timeout of 0 or less expires every accumulated buffer immediately.
  if Grid.IncrementalSearchTimeout <= 0 then
    Exit(True);
  Result := FSearchWatch.ElapsedMilliseconds > Grid.IncrementalSearchTimeout;
end;

function TVittixDBGridController.LocateByPrefix(const AFieldName,
  AText: string): Boolean;
var
  Field: TField;
  BufferIsUnicode: Boolean;
begin
  Result := False;
  if (AFieldName = '') or (AText = '') then Exit;

  BufferIsUnicode := not IsAsciiText(AText);
  Field := FDataset.FindField(AFieldName);

  // Midas and FireDAC compare ANSI (ftString) values through a codepage
  // conversion that collapses every non-ASCII character to '?', so a
  // non-ASCII buffer can match the WRONG record on such fields (a
  // Devanagari prefix matched a Gujarati row in testing). Wide string
  // fields compare in UTF-16 — Midas keeps strings wide internally and
  // FireDAC.DatS routes dtWideString through CompareStringW with
  // NORM_IGNORECASE (FireDAC.Stan.Util.FDCompareWideStr) — so the fast
  // Locate path is safe there. ASCII buffers are safe on any field.
  if (not BufferIsUnicode) or
     (Assigned(Field) and
      (Field.DataType in [ftWideString, ftFixedWideChar, ftWideMemo])) then
  begin
    try
      // Prefix, case-insensitive. Locate keeps the cursor on the current
      // record when nothing matches.
      Result := FDataset.Locate(AFieldName, AText,
        [loCaseInsensitive, loPartialKey]);
    except
      // Datasets without Locate support raise; fall back to a plain scan
      // that preserves the cursor when nothing matches.
      Result := FallbackScanForPrefix(AFieldName, AText);
    end;
  end
  else
    Result := FallbackScanForPrefix(AFieldName, AText);
end;

function TVittixDBGridController.FallbackScanForPrefix(const AFieldName,
  AText: string): Boolean;
var
  Bookmark: TBookmark;
  Field: TField;
  FoldedBuffer: string;
  BufLen, I: Integer;
  Value: string;
  Matched: Boolean;
begin
  Result := False;
  Field := FDataset.FindField(AFieldName);
  if not Assigned(Field) or FDataset.IsEmpty then Exit;

  // Fold the needle once. Per row, only the first BufLen characters are
  // folded (char-wise, via TCharHelper - no per-row string allocation),
  // which keeps a 50k-row scan in the low milliseconds. Char-wise simple
  // case mapping is equivalent to StartsText over fully folded strings.
  FoldedBuffer := UpperCaseUnicode(AText);
  BufLen := Length(FoldedBuffer);
  Bookmark := FDataset.GetBookmark;
  try
    FDataset.DisableControls;
    try
      FDataset.First;
      while not FDataset.Eof do
      begin
        if not Field.IsNull then
        begin
          Value := Field.AsString;
          if Length(Value) >= BufLen then
          begin
            Matched := True;
            for I := 1 to BufLen do
              if Value[I].ToUpper <> FoldedBuffer[I] then
              begin
                Matched := False;
                Break;
              end;
            if Matched then
              Exit(True); // cursor is already parked on the match
          end;
        end;
        FDataset.Next;
      end;

      // No match: restore the position the search started from.
      if Bookmark <> nil then
      begin
        try
          if FDataset.BookmarkValid(Bookmark) then
            FDataset.GotoBookmark(Bookmark);
        except
        end;
      end;
    finally
      FDataset.EnableControls;
    end;
  finally
    if Bookmark <> nil then
      FDataset.FreeBookmark(Bookmark);
  end;
end;

procedure TVittixDBGridController.UpdateSearchHint(const AText: string);
begin
  if Assigned(FFooterPanel) then
    FFooterPanel.SetSearchHint(AText);
end;

procedure TVittixDBGridController.UpdateSelectionFooterSync;
var
  Count: Integer;
begin
  if not Assigned(FFooterPanel) or not Assigned(FGrid) then Exit;
  Count := FGrid.SelectedRows.Count;
  if Count <> FLastSelectionCount then
  begin
    FLastSelectionCount := Count;
    FFooterPanel.Invalidate;
  end;
end;

procedure TVittixDBGridController.DoIncrementalSearch(const SearchText: string;
  Found: Boolean);
begin
  if Assigned(FOnIncrementalSearch) then
    FOnIncrementalSearch(Self, SearchText, Found);
end;

{ ============================================================================= }
{ DATASET EVENTS }
{ ============================================================================= }

{ ============================================================================= }
{ PUBLIC API }
{ ============================================================================= }

procedure TVittixDBGridController.SetAggregationDirty;
begin
  FAggregationDirty := True;
end;

procedure TVittixDBGridController.Refresh;
begin
  if FUpdating or FAggregationBusy then Exit;

  FAggregationBusy := True;
  try
    if FAggregationDirty and Assigned(FAggregationEngine) and
       Assigned(FDataset) and FDataset.Active and
       not (FDataset.State in dsEditModes) then
    begin
      // Server mode first: one SELECT COUNT/SUM/... against the applied
      // WHERE. Any failure falls back to the client full scan (the
      // correctness baseline).
      if not ServerAggregateRefresh then
        FAggregationEngine.Recalculate;
      FAggregationDirty := False;
    end;
  finally
    FAggregationBusy := False;
  end;

  if Assigned(FGrid) then
  begin
    FGrid.Invalidate;
    // Aggregation values shown in the footer must repaint with the grid:
    // the footer no longer invalidates itself from every grid WM_PAINT.
    if Assigned(FFooterPanel) then
      FFooterPanel.Invalidate;
  end;
end;

procedure TVittixDBGridController.GridLayoutChanged;
begin
  if not Assigned(FGrid) then
    Exit;

  if FUpdating then Exit;
  FUpdating := True;
  try
    if Assigned(FFooterPanel) then
    begin
      FFooterPanel.SyncLayout;
      FFooterPanel.Invalidate;
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TVittixDBGridController.Clear;
begin
  ClearFilters;
  if Assigned(FSortEngine) then
    FSortEngine.ClearSorting;
  if FServerSortActive then
  begin
    if Assigned(FProvider) then
      FProvider.ClearSort;
    FServerSortActive := False;
    FSortEngine.ClientApplyEnabled := not FServerFilterActive;
  end;
  SetAggregationDirty;
  Refresh;
  // ClearFilters already raised OnFilterApplied for the filter side.
  if Assigned(FSortEngine) then
    DoAfterSort(nil);
end;

procedure TVittixDBGridController.ApplyState;
var
  SortingApplied: Boolean;
begin
  SortingApplied := False;
  if Assigned(FSortEngine) then
  begin
    // In server mode this routes the ORDER BY instead of writing
    // IndexFieldNames.
    if FServerSortActive or (FilterMode <> vcmClient) then
      RefreshSorting
    else
      FSortEngine.ApplySorting;
    SortingApplied := True;
  end;
  SetAggregationDirty;
  Refresh;
  if SortingApplied then
    DoAfterSort(nil);
end;

function TVittixDBGridController.IsUpdating: Boolean;
begin
  Result := FUpdating;
end;

procedure TVittixDBGridController.SetGlobalFilter(const Text: string);
begin
  if not Assigned(FFilterEngine) then
    Exit;

  FFilterEngine.GlobalSearchText := Text;
  RefreshFiltering;
  // When the server provider took the WHERE, the client engine stays off;
  // otherwise the client path applies here (which also fixed the pre-2.0
  // no-op when the engine was already active).
  if not FServerFilterActive then
  begin
    if FFilterEngine.Active and Assigned(FDataSet) and FDataSet.Active then
      FFilterEngine.ApplyFilter
    else
      FFilterEngine.Active := Text <> '';
  end;
  SetAggregationDirty;
  Refresh;
  DoFilterApplied('');
end;

procedure TVittixDBGridController.ClearFilters;
begin
  if not Assigned(FFilterEngine) then
    Exit;

  // Clearing also drops any applied filter set (the engine's Clear removes
  // the OR-groups and the per-column texts) and the server WHERE.
  FFilterEngine.Clear;
  FActiveFilterSetName := '';
  if FServerFilterActive then
  begin
    if Assigned(FProvider) then
      FProvider.ClearFilter;
    FServerFilterActive := False;
    UpdateServerHint('');
  end;
  SetAggregationDirty;
  Refresh;
  DoFilterApplied('');
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

  for I := 0 to VittixGrid(FGrid).ColumnInfo.Count - 1 do
    if SameText(VittixGrid(FGrid).ColumnInfo[I].FieldName, AColumn.FieldName) then
      Exit(VittixGrid(FGrid).ColumnInfo[I]);
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

function TVittixDBGridController.FindColumnByFieldName(
  const FieldName: string): TColumn;
var
  I: Integer;
begin
  Result := nil;
  if not Assigned(FGrid) then Exit;
  for I := 0 to FGrid.Columns.Count - 1 do
    if SameText(FGrid.Columns[I].FieldName, FieldName) then
      Exit(FGrid.Columns[I]);
end;

procedure TVittixDBGridController.CaptureLayout(State: TVittixDBGridLayoutState);
var
  I: Integer;
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
  Item: TVittixDBGridLayoutColumnState;
begin
  if (State = nil) or not Assigned(FGrid) then Exit;
  State.Clear;
  State.FooterVisible := VittixGrid(FGrid).FooterVisible;
  State.AlternatingRowColors := VittixGrid(FGrid).AlternatingRowColors;
  State.AlternateRowColor := VittixGrid(FGrid).AlternateRowColor;
  State.ActiveFilterSetName := FActiveFilterSetName;
  for I := 0 to FGrid.Columns.Count - 1 do
  begin
    Col := FGrid.Columns[I];
    if (Col = nil) or (Col.FieldName = '') then Continue;
    Item.FieldName := Col.FieldName;
    Item.DisplayIndex := Col.Index;
    Item.Width := Col.Width;
    Item.Visible := Col.Visible;
    // Schema v2 defaults for columns without ColumnInfo
    Item.Pinned := False;
    Item.TitleCaption := '';
    Item.DisplayFormat := '';
    Item.ExportInclude := True;
    Item.ExportCaption := '';
    Item.EditorKind := 'veNone';
    Item.PickItems := '';
    Item.HasMin := False;
    Item.MinValue := 0;
    Item.HasMax := False;
    Item.MaxValue := 0;
    Item.Required := False;
    Item.DecimalPlaces := -1;
    Info := VittixGrid(FGrid).ColumnInfo.FindByFieldName(Col.FieldName);
    if Assigned(Info) then
    begin
      Item.SortOrder := Info.SortOrder;
      Item.SortIndex := Info.SortIndex;
      Item.AggregationType := Info.AggregationType;
      Item.FooterText := Info.FooterText;
      Item.CellConditionsJson := CellConditionsToJson(Info.CellConditions);
      Item.Pinned := Info.Pinned;
      Item.TitleCaption := Info.TitleCaption;
      Item.DisplayFormat := Info.DisplayFormat;
      Item.ExportInclude := Info.ExportInclude;
      Item.ExportCaption := Info.ExportCaption;
      Item.EditorKind := GetEnumName(TypeInfo(TVittixEditorKind),
        Ord(Info.EditorKind));
      Item.PickItems := Info.PickItems;
      Item.HasMin := Info.HasMin;
      Item.MinValue := Info.MinValue;
      Item.HasMax := Info.HasMax;
      Item.MaxValue := Info.MaxValue;
      Item.Required := Info.Required;
      Item.DecimalPlaces := Info.DecimalPlaces;
    end;
    State.Columns.Add(Item);
  end;
end;

procedure TVittixDBGridController.ApplyLayout(State: TVittixDBGridLayoutState);
var
  I: Integer;
  Item: TVittixDBGridLayoutColumnState;
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
  OldIndex: Integer;
begin
  if (State = nil) or not Assigned(FGrid) then Exit;
  FUpdating := True;
  FGrid.Columns.BeginUpdate;
  try
    for I := 0 to State.Columns.Count - 1 do
    begin
      Item := State.Columns[I];
      Col := FindColumnByFieldName(Item.FieldName);
      if Col = nil then Continue;
      Col.Width := Item.Width;
      Col.Visible := Item.Visible;
      OldIndex := Col.Index;
      if OldIndex <> Item.DisplayIndex then
      begin
        Col.Index := Item.DisplayIndex;
        DoColumnMoved(Col, OldIndex, Item.DisplayIndex);
      end;
      Info := VittixGrid(FGrid).ColumnInfo.FindByFieldName(Item.FieldName);
      if Assigned(Info) then
      begin
        Info.SortOrder := Item.SortOrder;
        Info.SortIndex := Item.SortIndex;
        Info.AggregationType := Item.AggregationType;
        Info.FooterText := Item.FooterText;
        CellConditionsFromJson(Info.CellConditions, Item.CellConditionsJson);
        Info.Pinned := Item.Pinned;
        Info.TitleCaption := Item.TitleCaption;
        Info.DisplayFormat := Item.DisplayFormat;
        Info.ExportInclude := Item.ExportInclude;
        Info.ExportCaption := Item.ExportCaption;
        // Editor configuration; an unknown enum name falls back to veNone.
        case GetEnumValue(TypeInfo(TVittixEditorKind), Item.EditorKind) of
          1: Info.EditorKind := vePickList;
          2: Info.EditorKind := veLookup;
          3: Info.EditorKind := veMaskedNumber;
        else
          Info.EditorKind := veNone;
        end;
        Info.PickItems := Item.PickItems;
        Info.HasMin := Item.HasMin;
        Info.MinValue := Item.MinValue;
        Info.HasMax := Item.HasMax;
        Info.MaxValue := Item.MaxValue;
        Info.Required := Item.Required;
        Info.DecimalPlaces := Item.DecimalPlaces;
        // Push the v2 overrides onto the live objects. An empty
        // TitleCaption/DisplayFormat leaves the current value untouched
        // (a layout cannot know the application's default).
        if Info.TitleCaption <> '' then
          Col.Title.Caption := Info.TitleCaption;
        ApplyColumnDisplayFormat(Col, Info.DisplayFormat);
      end;
    end;
    // Grid visual properties delegate to this controller (single source of
    // truth), so pushing through the grid setters is sufficient.
    VittixGrid(FGrid).FooterVisible := State.FooterVisible;
    VittixGrid(FGrid).AlternatingRowColors := State.AlternatingRowColors;
    VittixGrid(FGrid).AlternateRowColor := State.AlternateRowColor;
    ApplyState;
  finally
    try
      FGrid.Columns.EndUpdate;
    finally
      FUpdating := False;
    end;
  end;
  // Restore the frozen region from the loaded Pinned flags; this must run
  // outside the FUpdating guard so its LayoutChanged can reapply FixedCols.
  VittixGrid(FGrid).RefreshFrozenColumns;
  // Restore the applied filter set when it still exists; unknown names are
  // skipped silently (ApplyFilterSet returns False).
  if State.ActiveFilterSetName <> '' then
    ApplyFilterSet(State.ActiveFilterSetName);
  SetAggregationDirty;
  Refresh;
  GridLayoutChanged;
  // Fire only after the state is fully applied (and FUpdating released) so
  // handlers observe the final layout. ApplyState above already raised
  // OnAfterSort for the restored sort.
  DoAfterApplyLayout;
end;

procedure TVittixDBGridController.SaveLayoutToStream(Stream: TStream);
var
  State: TVittixDBGridLayoutState;
  Storage: IVittixDBGridLayoutStorage;
begin
  if not Assigned(Stream) then Exit;
  State := TVittixDBGridLayoutState.Create;
  try
    CaptureLayout(State);
    Storage := TVittixDBGridLayoutJsonStorage.Create;
    Storage.SaveToStream(State, Stream);
  finally
    State.Free;
  end;
end;

procedure TVittixDBGridController.SaveLayoutToFile(const FileName: string);
var
  State: TVittixDBGridLayoutState;
  Storage: TVittixDBGridLayoutJsonStorage;
  TargetFile: string;
  TempFile: string;
  BackupFile: string;
  Stream: TFileStream;
begin
  if FileName <> '' then
    TargetFile := FileName
  else if FLayoutStorageFileName <> '' then
    TargetFile := FLayoutStorageFileName
  else if FPersistenceRootPath <> '' then
    TargetFile := TPath.Combine(FPersistenceRootPath, 'layout.json')
  else
    raise EVittixLayoutError.Create(
      'No layout file name: pass FileName, set LayoutStorageFileName, or set PersistenceRootPath');

  // Stage next to the target so the final swap is an atomic same-volume
  // rename: a crash mid-write can then never leave a corrupt layout file.
  if TPath.GetDirectoryName(TargetFile) <> '' then
    ForceDirectories(TPath.GetDirectoryName(TargetFile));
  TempFile := TPath.Combine(TPath.GetDirectoryName(TargetFile),
    '~vittix-' + TPath.GetGUIDFileName(False) + '.tmp');
  BackupFile := TempFile + '.bak';

  State := TVittixDBGridLayoutState.Create;
  try
    try
      CaptureLayout(State);
      Storage := TVittixDBGridLayoutJsonStorage.Create;
      try
        Stream := TFileStream.Create(TempFile, fmCreate);
        try
          Storage.SaveToStream(State, Stream);
        finally
          Stream.Free;
        end;
      finally
        Storage.Free;
      end;

      // TFile.Replace needs a backup name (an empty one raises before the
      // swap); the backup is deleted again once the swap succeeded.
      if TFile.Exists(TargetFile) then
      begin
        TFile.Replace(TempFile, TargetFile, BackupFile);
        if TFile.Exists(BackupFile) then
          try TFile.Delete(BackupFile) except end;
      end
      else
        TFile.Move(TempFile, TargetFile);
    except
      // Never leave the staged temp file behind on failure.
      on E: Exception do
      begin
        if TFile.Exists(TempFile) then
          try TFile.Delete(TempFile) except end;
        raise;
      end;
    end;
  finally
    State.Free;
  end;
end;

procedure TVittixDBGridController.LoadLayoutFromStream(Stream: TStream);
var
  State: TVittixDBGridLayoutState;
  Storage: IVittixDBGridLayoutStorage;
begin
  if not Assigned(Stream) then Exit;
  Storage := TVittixDBGridLayoutJsonStorage.Create;
  State := Storage.LoadFromStream(Stream);
  try
    ApplyLayout(State);
  finally
    State.Free;
  end;
end;

procedure TVittixDBGridController.LoadLayoutFromFile(const FileName: string);
var
  Storage: TVittixDBGridLayoutJsonStorage;
  SourceFile: string;
  Stream: TFileStream;
  State: TVittixDBGridLayoutState;
begin
  if FileName <> '' then
    SourceFile := FileName
  else if FLayoutStorageFileName <> '' then
    SourceFile := FLayoutStorageFileName
  else if FPersistenceRootPath <> '' then
    SourceFile := TPath.Combine(FPersistenceRootPath, 'layout.json')
  else
    raise EVittixLayoutError.Create(
      'No layout file name: pass FileName, set LayoutStorageFileName, or set PersistenceRootPath');
  if not FileExists(SourceFile) then Exit;

  Storage := TVittixDBGridLayoutJsonStorage.Create;
  try
    Stream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
    try
      State := Storage.LoadFromStream(Stream);
      try
        ApplyLayout(State);
      finally
        State.Free;
      end;
    finally
      Stream.Free;
    end;
  finally
    Storage.Free;
  end;
end;

procedure TVittixDBGridController.ResetLayout;
var
  I: Integer;
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
begin
  if not Assigned(FGrid) then Exit;

  // Freezing is part of the layout: clear it first so the setter's Pinned
  // sync runs before the column resets below.
  VittixGrid(FGrid).UnfreezeAll;

  FGrid.Columns.BeginUpdate;
  try
    if Assigned(FGrid.DataSource) and Assigned(FGrid.DataSource.DataSet) then
      for I := 0 to FGrid.DataSource.DataSet.Fields.Count - 1 do
      begin
        Col := FindColumnByFieldName(FGrid.DataSource.DataSet.Fields[I].FieldName);
        if Assigned(Col) then Col.Index := Min(I, FGrid.Columns.Count - 1);
      end;
    FGrid.Canvas.Font.Assign(FGrid.Font);
    for I := 0 to FGrid.Columns.Count - 1 do
    begin
      Col := FGrid.Columns[I];
      Col.Visible := True;
      if Assigned(Col.Field) then
        Col.Width := Col.Field.DisplayWidth * FGrid.Canvas.TextWidth('0');
      Info := FindInfoByColumn(Col);
      if Assigned(Info) then
      begin
        Info.AggregationType := vatNone;
        Info.FooterText := '';
        Info.CellConditions.Clear;
      end;
    end;
  finally
    FGrid.Columns.EndUpdate;
  end;
  ShowFooter := True;
  if Assigned(FAggregationEngine) then FAggregationEngine.Clear;
  Clear;
  GridLayoutChanged;
end;

procedure TVittixDBGridController.ShowColumnChooser;
begin
  if not Assigned(FGrid) then
    Exit;

  if TVittixDBGridColumnChooserForm.Execute(FGrid) then
  begin
    // Hiding a frozen column must shrink the frozen region accordingly.
    VittixGrid(FGrid).RefreshFrozenColumns;
    SetAggregationDirty;
    Refresh;
  end;
end;

/// <summary>Layout restores apply DisplayFormat to the backing field.
/// Only numeric and datetime fields carry a DisplayFormat; an empty format
/// leaves the application's own field settings untouched.</summary>
procedure TVittixDBGridController.ApplyColumnDisplayFormat(AColumn: TColumn;
  const AFormat: string);
begin
  if (AFormat = '') or not Assigned(AColumn) or not Assigned(AColumn.Field) then
    Exit;

  if AColumn.Field is TNumericField then
    TNumericField(AColumn.Field).DisplayFormat := AFormat
  else if AColumn.Field is TDateTimeField then
    TDateTimeField(AColumn.Field).DisplayFormat := AFormat;
end;

{ ============================================================================= }
{ SELECTION COPY (Ctrl+C / Ctrl+Shift+C, routed from TVittixDBGrid.KeyDown) }
{ ============================================================================= }

procedure TVittixDBGridController.CopySelectionToClipboard(
  AIncludeHeaders: Boolean);
var
  Grid: TVittixDBGrid;
  Exporter: TVittixDBGridExporter;
begin
  Grid := VittixGrid(FGrid);
  if not Assigned(Grid) then Exit;

  Exporter := TVittixDBGridExporter.Create(Grid);
  try
    // Full-precision invariant values (unless the exporter's locale opt-out
    // is set), visible columns in grid order, tab-delimited with proper
    // quoting — all owned by the exporter so copy and export never drift.
    Exporter.CopySelectionToClipboard(AIncludeHeaders);
  finally
    Exporter.Free;
  end;
end;

{ ============================================================================= }
{ SERVER-SIDE PROVIDER ROUTING (v2.0) }
{ ============================================================================= }

function TVittixDBGridController.FilterMode: TVittixFilterMode;
begin
  // The grid owns the published property; no grid or a design-time grid
  // means the safe default.
  Result := vcmClient;
  if Assigned(FGrid) and not (csDesigning in FGrid.ComponentState) then
    Result := VittixGrid(FGrid).FilterMode;
end;

function TVittixDBGridController.CollectFieldInfos: TArray<TVittixSQLFieldInfo>;
var
  I: Integer;
begin
  SetLength(Result, 0);
  if not Assigned(FDataset) or not FDataset.Active then
    Exit;
  for I := 0 to FDataset.Fields.Count - 1 do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].Name := FDataset.Fields[I].FieldName;
    Result[High(Result)].DataType := FDataset.Fields[I].DataType;
  end;
end;

function TVittixDBGridController.CollectSortColumns: TArray<TVittixSortColumn>;
var
  Infos: TArray<TVittixDBGridColumnInfo>;
  I, J, Count: Integer;
  Info: TVittixDBGridColumnInfo;
begin
  // Same ordering rule as the client sort engine: SortIndex ascending.
  SetLength(Infos, 0);
  if Assigned(FGrid) then
    for I := 0 to VittixGrid(FGrid).ColumnInfo.Count - 1 do
      if VittixGrid(FGrid).ColumnInfo[I].SortOrder <> vsoNone then
      begin
        SetLength(Infos, Length(Infos) + 1);
        Infos[High(Infos)] := VittixGrid(FGrid).ColumnInfo[I];
      end;
  for I := 1 to High(Infos) do
    for J := I downto 1 do
      if Infos[J].SortIndex < Infos[J - 1].SortIndex then
      begin
        Info := Infos[J];
        Infos[J] := Infos[J - 1];
        Infos[J - 1] := Info;
      end;

  SetLength(Result, Length(Infos));
  Count := 0;
  for I := 0 to High(Infos) do
  begin
    Result[Count].FieldName := Infos[I].FieldName;
    Result[Count].Descending := Infos[I].SortOrder = vsoDesc;
    Inc(Count);
  end;
end;

procedure TVittixDBGridController.DoProviderFallback(const AReason: string);
begin
  if Assigned(FOnProviderFallback) then
    FOnProviderFallback(Self, AReason);
end;

procedure TVittixDBGridController.DoProviderError(E: Exception;
  var Handled: Boolean);
begin
  if Assigned(FOnProviderError) then
    FOnProviderError(Self, E, Handled);
end;

procedure TVittixDBGridController.UpdateServerHint(const AText: string);
begin
  if Assigned(FFooterPanel) then
    FFooterPanel.SetServerHint(AText);
end;

procedure TVittixDBGridController.ApplyClientFiltering;
begin
  // The client engine takes over with the same texts (the per-column Info
  // texts and the stored groups stay in place).
  if Assigned(FFilterEngine) and not FFilterEngine.Active and
     Assigned(FDataset) and FDataset.Active then
    FFilterEngine.Active := True;
end;

procedure TVittixDBGridController.RestoreClientFiltering(
  const AReason: string; AFireFallback: Boolean);
begin
  if FServerFilterActive then
  begin
    if Assigned(FProvider) then
      FProvider.ClearFilter;
    FServerFilterActive := False;
  end;
  ApplyClientFiltering;
  SetAggregationDirty;
  Refresh;

  if AReason <> '' then
    UpdateServerHint('Client filter (fallback)')
  else
    UpdateServerHint('');
  if AFireFallback and (AReason <> '') then
    DoProviderFallback(AReason);
end;

procedure TVittixDBGridController.DeactivateServerState;
begin
  // Mode switched back to client (or teardown): everything back to the
  // application's original SQL and the client engine.
  if Assigned(FProvider) then
    FProvider.RestoreOriginal;
  FServerFilterActive := False;
  FServerSortActive := False;
  if Assigned(FSortEngine) then
    FSortEngine.ClientApplyEnabled := True;
  UpdateServerHint('');
end;

procedure TVittixDBGridController.SetProvider(const Value: IVittixDataProvider);
begin
  if FProvider = Value then Exit;

  // A provider swap while the server path is live must not leave the old
  // wrapper SQL on the dataset.
  if FServerFilterActive or FServerSortActive then
  begin
    if Assigned(FProvider) then
      FProvider.RestoreOriginal;
    FServerFilterActive := False;
    FServerSortActive := False;
    if Assigned(FSortEngine) then
      FSortEngine.ClientApplyEnabled := True;
    ApplyClientFiltering;
    UpdateServerHint('');
  end;
  FProvider := Value;
end;

procedure TVittixDBGridController.RefreshFiltering;
var
  Groups, ColumnGroup: TVittixFilterConditionGroups;
  GlobalSearch: string;
  Mode: TVittixFilterMode;
  Reason: string;
  Handled: Boolean;
  HasCondition: Boolean;
  G, C: Integer;
begin
  if FUpdating then Exit;

  Mode := FilterMode;
  if Mode = vcmClient then
  begin
    if FServerFilterActive or FServerSortActive then
    begin
      DeactivateServerState;
      ApplyClientFiltering;
      SetAggregationDirty;
      Refresh;
    end
    else
      // Pure client mode: keep the engine in step with the active
      // conditions (column texts, stored groups, global search).
      ApplyClientFiltering;
    Exit;
  end;

  // A running edit defers the re-query exactly like the aggregation
  // refresh does; DataLinkRecordChanged routes it after the post/cancel.
  if Assigned(FDataset) and (FDataset.State in dsEditModes) then
  begin
    FPendingServerRoute := True;
    Exit;
  end;
  FPendingServerRoute := False;

  if not (Assigned(FDataset) and FDataset.Active) then
    Exit;

  // Conditions: an applied filter set's groups win; otherwise the classic
  // per-column texts form one AND-group.
  Groups := nil;
  GlobalSearch := '';
  if Assigned(FFilterEngine) then
  begin
    Groups := FFilterEngine.GetStoredGroups;
    if Length(Groups) = 0 then
      Groups := FFilterEngine.GetColumnConditions;
    GlobalSearch := FFilterEngine.GlobalSearchText;
  end;

  HasCondition := GlobalSearch <> '';
  if not HasCondition then
    for G := 0 to High(Groups) do
      for C := 0 to High(Groups[G]) do
        if Trim(Groups[G][C].FilterText) <> '' then
        begin
          HasCondition := True;
          Break;
        end;

  if not HasCondition then
  begin
    // Nothing to filter: drop the server WHERE (sort survives) and let
    // the client side be the no-filter baseline.
    if FServerFilterActive then
    begin
      if Assigned(FProvider) then
        FProvider.ClearFilter;
      FServerFilterActive := False;
    end;
    ApplyClientFiltering;
    UpdateServerHint('');
    Exit;
  end;

  if FProvider = nil then
  begin
    if Mode = vcmServer then
      RestoreClientFiltering('no provider assigned', True)
    else
      RestoreClientFiltering('', False);
    Exit;
  end;

  if not FProvider.CanHandle(FDataset) then
  begin
    Reason := FProvider.GetLastFallbackReason;
    if Reason = '' then
      Reason := 'the provider cannot handle this dataset';
    RestoreClientFiltering(Reason, True);
    Exit;
  end;

  // The client validation hook belongs to the client engine; server mode
  // would silently skip it, so those setups stay client-side.
  if Assigned(FOnValidateFilter) then
  begin
    RestoreClientFiltering(
      'an OnValidateFilter hook requires client-side evaluation', True);
    Exit;
  end;

  try
    if FProvider.ApplyFilter(Groups, GlobalSearch, CollectFieldInfos) then
    begin
      // Server owns the WHERE: the client engine must not evaluate too.
      if Assigned(FFilterEngine) and FFilterEngine.Active then
        FFilterEngine.Active := False;
      FServerFilterActive := True;
      UpdateServerHint('Server filter (' + FProvider.GetDialectName + ')');
    end
    else
      RestoreClientFiltering(FProvider.GetLastFallbackReason, True);
  except
    on E: Exception do
    begin
      Handled := False;
      DoProviderError(E, Handled);
      if not Handled then
        raise;
      RestoreClientFiltering('error: ' + E.Message, True);
    end;
  end;
end;

procedure TVittixDBGridController.RefreshSorting;
var
  Columns: TArray<TVittixSortColumn>;
  Fields: TArray<TVittixSQLFieldInfo>;
  Mode: TVittixFilterMode;
  Reason: string;
  Handled: Boolean;
begin
  if FUpdating or not Assigned(FSortEngine) then Exit;

  Mode := FilterMode;
  Columns := CollectSortColumns;
  Fields := CollectFieldInfos;

  if (Mode = vcmClient) or (FProvider = nil) or
     (not FServerSortActive and not FServerFilterActive and
      (Mode = vcmAuto) and (Length(Columns) > 0) and
      not FProvider.CanHandle(FDataset)) then
  begin
    // Client path. An active server sort is torn down first.
    if FServerSortActive then
    begin
      FProvider.ClearSort;
      FServerSortActive := False;
    end;
    FSortEngine.ClientApplyEnabled := True;
    FSortEngine.ApplySorting;
    Exit;
  end;

  // Server path. ApplySorting is a no-op while ClientApplyEnabled is off,
  // so both entry points (ToggleSort, ApplyState) can call here safely.
  if Length(Columns) = 0 then
  begin
    if FServerSortActive then
    begin
      FProvider.ClearSort;
      FServerSortActive := False;
      FSortEngine.ClientApplyEnabled := FServerFilterActive;
    end;
    Exit;
  end;

  // A client sort may have left IndexFieldNames on the dataset; it would
  // re-sort the re-opened result client-side and override the ORDER BY.
  if (GetPropInfo(FDataset.ClassInfo, 'IndexFieldNames') <> nil) and
     (GetPropValue(FDataset, 'IndexFieldNames', True) <> '') then
  begin
    FDataset.DisableControls;
    try
      SetPropValue(FDataset, 'IndexFieldNames', '');
    finally
      FDataset.EnableControls;
    end;
  end;

  try
    if FProvider.ApplySort(Columns, Fields) then
    begin
      FServerSortActive := True;
      FSortEngine.ClientApplyEnabled := False;
    end
    else
    begin
      Reason := FProvider.GetLastFallbackReason;
      FSortEngine.ClientApplyEnabled := True;
      FSortEngine.ApplySorting;
      if Mode = vcmAuto then
        FSortEngine.ClientApplyEnabled := False;
      DoProviderFallback(Reason);
    end;
  except
    on E: Exception do
    begin
      Handled := False;
      DoProviderError(E, Handled);
      if not Handled then
        raise;
      FSortEngine.ClientApplyEnabled := True;
      FSortEngine.ApplySorting;
    end;
  end;
end;

function TVittixDBGridController.ServerAggregateRefresh: Boolean;
var
  Requests: TArray<TVittixAggRequest>;
  Results: TArray<TVittixAggResult>;
  I: Integer;
  Info: TVittixDBGridColumnInfo;
  Agg: TVittixAggregation;
  Field: TField;
begin
  // Server-side footer aggregates: one SELECT COUNT/SUM/... against the
  // same WHERE the provider applied. Any failure falls back to the client
  // scan (the correctness baseline).
  Result := False;
  if (not FServerFilterActive) or (FProvider = nil) then
    Exit;

  SetLength(Requests, 0);
  if Assigned(FGrid) then
    for I := 0 to VittixGrid(FGrid).ColumnInfo.Count - 1 do
    begin
      Info := VittixGrid(FGrid).ColumnInfo[I];
      if Info.AggregationType <> vatNone then
      begin
        SetLength(Requests, Length(Requests) + 1);
        Requests[High(Requests)].FieldName := Info.FieldName;
        Requests[High(Requests)].AggType := Info.AggregationType;
      end;
    end;
  if Length(Requests) = 0 then
    Exit(False);

  try
    if not FProvider.FetchAggregates(Requests, Results) then
      Exit(False);
  except
    Exit(False);
  end;

  for I := 0 to High(Requests) do
  begin
    Info := VittixGrid(FGrid).ColumnInfo.FindByFieldName(Requests[I].FieldName);
    if not Assigned(Info) then
      Continue;
    Agg := Info.Aggregation;
    Agg.Clear;
    if I <= High(Results) then
    begin
      Agg.Count := Results[I].Count;
      Agg.NullCount := Results[I].NullCount;
      Field := FDataset.FindField(Requests[I].FieldName);
      if (Field <> nil) and not VarIsNull(Results[I].Value) then
      begin
        case Field.DataType of
          ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc,
          ftShortint, ftByte, ftLongWord:
            begin
              Agg.HasInt := True;
              case Requests[I].AggType of
                vatSum: Agg.SumInt := Results[I].Value;
                vatAvg: begin
                  Agg.HasFloat := True;
                  Agg.SumFloat := Results[I].Value * Agg.Count;
                end;
                vatMin: Agg.MinInt := Results[I].Value;
                vatMax: Agg.MaxInt := Results[I].Value;
              end;
            end;
          ftCurrency:
            begin
              Agg.HasCurrency := True;
              case Requests[I].AggType of
                vatSum: Agg.SumCurrency := Results[I].Value;
                vatAvg: begin
                  Agg.HasFloat := True;
                  Agg.SumFloat := Results[I].Value * Agg.Count;
                end;
                vatMin: Agg.MinCurrency := Results[I].Value;
                vatMax: Agg.MaxCurrency := Results[I].Value;
              end;
            end;
          ftString, ftWideString, ftMemo, ftWideMemo, ftFixedChar,
          ftFixedWideChar:
            begin
              Agg.HasString := True;
              case Requests[I].AggType of
                vatMin: Agg.MinString := Results[I].Value;
                vatMax: Agg.MaxString := Results[I].Value;
              end;
            end;
        else
          begin
            Agg.HasFloat := True;
            case Requests[I].AggType of
              vatSum: Agg.SumFloat := Results[I].Value;
              vatAvg: begin
                Agg.HasFloat := True;
                Agg.SumFloat := Results[I].Value * Agg.Count;
              end;
              vatMin: Agg.MinFloat := Results[I].Value;
              vatMax: Agg.MaxFloat := Results[I].Value;
            end;
          end;
        end;
      end;
    end;
    Info.Aggregation := Agg;
  end;
  Result := True;
end;

{ ============================================================================= }
{ SAVED FILTER SETS }
{ ============================================================================= }

procedure TVittixDBGridController.EnsureFilterSetListLoaded;
var
  FileName: string;
begin
  if FFilterSetListLoaded then Exit;

  if FFilterSetList = nil then
    FFilterSetList := TVittixFilterSetList.Create;

  // A corrupt file raises EVittixFilterSetError here (on first use) rather
  // than silently dropping the saved sets. The loaded flag is only set
  // after a successful parse, so every later access retries (and keeps
  // raising) until the file is fixed - no save path can overwrite it.
  FileName := ResolveFilterSetsFileName;
  if (FileName <> '') and FileExists(FileName) then
    FFilterSetList.LoadFromFile(FileName);
  FFilterSetListLoaded := True;
end;

function TVittixDBGridController.GetFilterSetList: TVittixFilterSetList;
begin
  EnsureFilterSetListLoaded;
  Result := FFilterSetList;
end;

function TVittixDBGridController.ResolveFilterSetsFileName: string;
begin
  if FFilterSetsFileName <> '' then
    Result := FFilterSetsFileName
  else if FPersistenceRootPath <> '' then
    Result := TPath.Combine(FPersistenceRootPath, 'filtersets.json')
  else
    Result := '';
end;

procedure TVittixDBGridController.StoreSetInEngine(ASet: TVittixFilterSet);
var
  Groups: TVittixFilterConditionGroups;
  G, C: Integer;
begin
  if not Assigned(ASet) or not Assigned(FFilterEngine) then
    Exit;

  SetLength(Groups, ASet.GroupCount);
  for G := 0 to ASet.GroupCount - 1 do
  begin
    SetLength(Groups[G], ASet[G].ConditionCount);
    for C := 0 to ASet[G].ConditionCount - 1 do
      Groups[G][C] := ASet[G][C];
  end;
  FFilterEngine.SetFilterGroups(Groups);
end;

procedure TVittixDBGridController.ReapplyActiveFilterSetToEngine;
var
  ActiveSet: TVittixFilterSet;
begin
  if FActiveFilterSetName = '' then Exit;
  EnsureFilterSetListLoaded;
  ActiveSet := FFilterSetList.FindByName(FActiveFilterSetName);
  if not Assigned(ActiveSet) then
  begin
    // The set vanished (file replaced, set deleted): drop the stale name
    // instead of keeping a filter nobody can see or clear by name.
    FActiveFilterSetName := '';
    UpdateFilterSetHint;
    Exit;
  end;
  StoreSetInEngine(ActiveSet);
  RefreshFiltering;
  if not FServerFilterActive then
  begin
    if Assigned(FFilterEngine) and not FFilterEngine.Active then
      FFilterEngine.Active := True;
  end;
  // The footer panel may have been recreated with the engines: restore its
  // hint for the active set.
  UpdateFilterSetHint;
end;

procedure TVittixDBGridController.SaveCurrentFilterAs(const AName: string);
var
  Set_: TVittixFilterSet;
  Group: TVittixFilterSetGroup;
  I: Integer;
  Info: TVittixDBGridColumnInfo;
  Grid: TVittixDBGrid;
begin
  if Trim(AName) = '' then
    raise EVittixFilterSetError.Create('A filter set needs a name');

  EnsureFilterSetListLoaded;
  Set_ := FFilterSetList.AddOrUpdate(Trim(AName));
  Group := Set_.AddGroup;

  Grid := VittixGrid(FGrid);
  if Assigned(Grid) then
    for I := 0 to Grid.ColumnInfo.Count - 1 do
    begin
      Info := Grid.ColumnInfo[I];
      if Info.HasFilter and (Trim(Info.FilterText) <> '') then
        Group.AddCondition(Info.FieldName, Info.FilterText);
    end;

  if ResolveFilterSetsFileName <> '' then
    FFilterSetList.SaveToFile(ResolveFilterSetsFileName);

  // The current filter state IS this set now, so it becomes the active one
  // without re-applying (the engine state already matches group 1).
  FActiveFilterSetName := Set_.Name;
  UpdateFilterSetHint;
  DoFilterSetsChanged;
end;

function TVittixDBGridController.ApplyFilterSet(const AName: string): Boolean;
var
  Set_: TVittixFilterSet;
  Grid: TVittixDBGrid;
  I, C: Integer;
  Info: TVittixDBGridColumnInfo;
  Col: TColumn;
begin
  Result := False;
  if AName = '' then Exit;

  EnsureFilterSetListLoaded;
  Set_ := FFilterSetList.FindByName(AName);
  if not Assigned(Set_) then Exit;

  Grid := VittixGrid(FGrid);

  // Reset the per-column filters first so removed conditions cannot linger.
  if Assigned(Grid) then
    for I := 0 to Grid.ColumnInfo.Count - 1 do
    begin
      Grid.ColumnInfo[I].FilterText := '';
      Grid.ColumnInfo[I].HasFilter := False;
    end;

  // The first group mirrors into the per-column texts so titles, the filter
  // popup and exports reflect it; extra groups live in the engine only.
  if Set_.GroupCount > 0 then
    for C := 0 to Set_[0].ConditionCount - 1 do
    begin
      if not Assigned(Grid) then
        Break;
      Col := Grid.Controller.FindColumnByFieldName(Set_[0][C].FieldName);
      if not Assigned(Col) then
        Continue;
      Info := Grid.ColumnInfoByColumn(Col);
      if Assigned(Info) then
      begin
        Info.FilterText := Set_[0][C].FilterText;
        Info.HasFilter := True;
      end;
    end;

  if Assigned(FFilterEngine) then
  begin
    StoreSetInEngine(Set_);
    // One routing point: server mode wraps the groups into the WHERE,
    // client mode activates/re-applies the engine.
    RefreshFiltering;
    if not FServerFilterActive then
    begin
      if FFilterEngine.Active and Assigned(FDataSet) and FDataSet.Active then
        FFilterEngine.ApplyFilter
      else
        FFilterEngine.Active := True;
    end;
  end;
  // With a closed dataset the engine is absent; the set name is kept and
  // CreateEngines re-binds the groups when the dataset reopens.

  FActiveFilterSetName := Set_.Name;
  SetAggregationDirty;
  Refresh;
  GridLayoutChanged;
  UpdateFilterSetHint;
  DoFilterSetApplied(Set_.Name);
  Result := True;
end;

function TVittixDBGridController.DeleteFilterSet(const AName: string): Boolean;
begin
  EnsureFilterSetListLoaded;
  Result := FFilterSetList.Delete(AName);
  if not Result then
    Exit;

  if ResolveFilterSetsFileName <> '' then
    FFilterSetList.SaveToFile(ResolveFilterSetsFileName);

  if SameText(FActiveFilterSetName, AName) then
  begin
    FActiveFilterSetName := '';
    UpdateFilterSetHint;
  end;
  DoFilterSetsChanged;
end;

procedure TVittixDBGridController.PersistFilterSets;
begin
  EnsureFilterSetListLoaded;
  if ResolveFilterSetsFileName <> '' then
    FFilterSetList.SaveToFile(ResolveFilterSetsFileName);
end;

function TVittixDBGridController.FilterSetNames: TArray<string>;
var
  I: Integer;
begin
  EnsureFilterSetListLoaded;
  SetLength(Result, FFilterSetList.Count);
  for I := 0 to FFilterSetList.Count - 1 do
    Result[I] := FFilterSetList[I].Name;
end;

procedure TVittixDBGridController.UpdateFilterSetHint;
begin
  if Assigned(FFooterPanel) then
  begin
    if FActiveFilterSetName <> '' then
      FFooterPanel.SetFilterSetHint('Filter set: ' + FActiveFilterSetName)
    else
      FFooterPanel.SetFilterSetHint('');
  end;
end;

procedure TVittixDBGridController.DoFilterSetApplied(const AName: string);
begin
  if Assigned(FOnFilterSetApplied) then
    FOnFilterSetApplied(Self, AName);
end;

procedure TVittixDBGridController.DoFilterSetsChanged;
begin
  if Assigned(FOnFilterSetsChanged) then
    FOnFilterSetsChanged(Self);
end;

{ ============================================================================= }
{ NOTIFICATION EVENTS }
{ ============================================================================= }

procedure TVittixDBGridController.DoAfterSort(Column: TColumn);
begin
  if Assigned(FOnAfterSort) then
    FOnAfterSort(Self, Column);
end;

procedure TVittixDBGridController.DoFilterApplied(const FieldName: string);
begin
  if Assigned(FOnFilterApplied) then
    FOnFilterApplied(Self, FieldName);
end;

procedure TVittixDBGridController.DoAfterApplyLayout;
begin
  if Assigned(FOnAfterApplyLayout) then
    FOnAfterApplyLayout(Self);
end;

procedure TVittixDBGridController.DoColumnMoved(Column: TColumn;
  OldIndex, NewIndex: Integer);
begin
  if Assigned(FOnColumnMoved) then
    FOnColumnMoved(Self, Column, OldIndex, NewIndex);
end;

function TVittixDBGridController.GetOnValidateFilter: TFilterValidationEvent;
begin
  Result := FOnValidateFilter;
end;

procedure TVittixDBGridController.SetOnValidateFilter(
  const Value: TFilterValidationEvent);
begin
  FOnValidateFilter := Value;
  if Assigned(FFilterEngine) then
    FFilterEngine.OnValidateFilter := Value;
end;

function TVittixDBGridController.GetOnFormatAggregation: TFormatAggregationEvent;
begin
  Result := FOnFormatAggregation;
end;

procedure TVittixDBGridController.SetOnFormatAggregation(
  const Value: TFormatAggregationEvent);
begin
  FOnFormatAggregation := Value;
  if Assigned(FAggregationEngine) then
    FAggregationEngine.OnFormatAggregation := Value;
end;

function TVittixDBGridController.GetOnFieldValidation: TFieldValidationEvent;
begin
  Result := FOnFieldValidation;
end;

procedure TVittixDBGridController.SetOnFieldValidation(
  const Value: TFieldValidationEvent);
begin
  FOnFieldValidation := Value;
  if Assigned(FSortEngine) then
    FSortEngine.OnFieldValidation := Value;
end;

function TVittixDBGridController.FooterDisplayText(
  const FieldName: string): string;
var
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
begin
  Result := '';
  if not Assigned(FGrid) then Exit;

  Col := FindColumnByFieldName(FieldName);
  if not Assigned(Col) then Exit;

  Info := VittixGrid(FGrid).ColumnInfoByColumn(Col);
  if not Assigned(Info) then Exit;

  if Info.FooterText <> '' then
    Exit(Info.FooterText);

  if Assigned(FAggregationEngine) then
    Result := FAggregationEngine.GetAggregationDisplayText(Info);
end;

initialization
  System.Classes.RegisterClass(TVittixDBGridController);

finalization
  System.Classes.UnRegisterClass(TVittixDBGridController);

end.
