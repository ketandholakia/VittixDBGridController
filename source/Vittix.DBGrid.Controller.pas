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

    // Event hooks — only the WindowProc remains; grid input/draw integration
    // happens via virtual overrides on TVittixDBGrid calling the DoXxx methods.
    FOldWindowProc: TWndMethod;

    // Internal helpers
    function IsReady: Boolean;
    function FindInfoByColumn(AColumn: TColumn): TVittixDBGridColumnInfo;
    function FindColumnByField(AField: TField): TColumn;

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

    // Integration points called from TVittixDBGrid's virtual overrides.
    // DoMouseDown/DoDblClick/DoKeyDown return True when the event was
    // consumed and must not reach inherited VCL processing.
    procedure DoTitleClick(Column: TColumn);
    procedure DoDrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState);
    function DoMouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer): Boolean;
    function DoDblClick: Boolean;
    function DoKeyDown(var Key: Word; Shift: TShiftState): Boolean;

    // Called by TVittixDBGrid (chooser moves, VCL ColumnMoved) after a
    // column's display position changed; raises OnColumnMoved.
    procedure DoColumnMoved(Column: TColumn; OldIndex, NewIndex: Integer);

    procedure Detach;
    procedure InstallWindowProc;
    procedure RehookGrid;
    procedure GridLayoutChanged;

    procedure Refresh;
    procedure Clear;
    procedure ApplyState;
    procedure ShowColumnChooser;
    procedure SetGlobalFilter(const Text: string);
    procedure ClearFilters;
    procedure SetColumnAggregation(Column: TColumn; Aggregation: TVittixAggregationType);
    procedure CaptureLayout(State: TVittixDBGridLayoutState);
    procedure ApplyLayout(State: TVittixDBGridLayoutState);
    function IsUpdating: Boolean;
    procedure SaveLayoutToStream(Stream: TStream);
    procedure LoadLayoutFromStream(Stream: TStream);
    procedure SaveLayoutToFile(const FileName: string = '');
    procedure LoadLayoutFromFile(const FileName: string = '');
    procedure ResetLayout;

    /// <summary>Finds a grid column by its FieldName; nil when not found.
    /// Public helper used by the export engine and tests.</summary>
    function FindColumnByFieldName(const FieldName: string): TColumn;

    /// <summary>Returns the footer text that should appear for a column
    /// identified by field name: the aggregation display text when an
    /// aggregation is configured, otherwise the column's FooterText, or ''
    /// when neither is set. Used by the export engine when IncludeFooter is
    /// enabled.</summary>
    function FooterDisplayText(const FieldName: string): string;

    // Called by TVittixDBGrid when its DataSource property changes
    procedure DataSourceChanged;

    // Expose engines for testing and advanced scenarios
    property FilterEngine: TVittixDBGridFilterEngine read FFilterEngine;
    property SortEngine: TVittixDBGridSortEngine read FSortEngine;
    property AggregationEngine: TVittixDBGridAggregationEngine read FAggregationEngine;

    property Grid: TDBGrid read FGrid write SetGrid;
    property LayoutStorageFileName: string read FLayoutStorageFileName write FLayoutStorageFileName;
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
  Vittix.DBGrid;

function VittixGrid(AGrid: TDBGrid): TVittixDBGrid;
begin
  // SetGrid guarantees the type; the guard keeps the helper nil-safe for
  // teardown windows where the grid reference is already cleared.
  if AGrid is TVittixDBGrid then
    Result := TVittixDBGrid(AGrid)
  else
    Result := nil;
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
end;

destructor TVittixDBGridController.Destroy;
begin
  UnhookGrid;
  FreeAndNil(FDataLink);
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
end;

{ ============================================================================= }
{ GRID EVENT INTEGRATION (called from TVittixDBGrid virtual overrides) }
{ ============================================================================= }

procedure TVittixDBGridController.DoTitleClick(Column: TColumn);
begin
  if Assigned(FSortEngine) then
  begin
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
     (Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo, ftDate, ftTime, ftDateTime]) and
     TVittixDBGridEditors.EditField(FGrid, Column) then
  begin
    Refresh;
    Result := True; // consumed: the field editor handled it
  end;
end;

function TVittixDBGridController.DoKeyDown(var Key: Word;
  Shift: TShiftState): Boolean;
var
  Field: TField;
  Column: TColumn;
begin
  Result := False;
  if Key <> VK_F2 then Exit;

  Field := nil;
  if Assigned(FGrid) then
    Field := FGrid.SelectedField;
  Column := FindColumnByField(Field);

  if Assigned(Column) and Assigned(Field) and
     (Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo, ftDate, ftTime, ftDateTime]) and
     TVittixDBGridEditors.EditField(FGrid, Column) then
  begin
    Refresh;
    Result := True;
  end;
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
  FFilterEngine.Active := Text <> '';
  SetAggregationDirty;
  Refresh;
  DoFilterApplied('');
end;

procedure TVittixDBGridController.ClearFilters;
begin
  if not Assigned(FFilterEngine) then
    Exit;

  FFilterEngine.Clear;
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
  for I := 0 to FGrid.Columns.Count - 1 do
  begin
    Col := FGrid.Columns[I];
    if (Col = nil) or (Col.FieldName = '') then Continue;
    Item.FieldName := Col.FieldName;
    Item.DisplayIndex := Col.Index;
    Item.Width := Col.Width;
    Item.Visible := Col.Visible;
    Info := VittixGrid(FGrid).ColumnInfo.FindByFieldName(Col.FieldName);
    if Assigned(Info) then
    begin
      Item.SortOrder := Info.SortOrder;
      Item.SortIndex := Info.SortIndex;
      Item.AggregationType := Info.AggregationType;
      Item.FooterText := Info.FooterText;
      Item.CellConditionsJson := CellConditionsToJson(Info.CellConditions);
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
    SetAggregationDirty;
    Refresh;
  end;
end;

{ =============================================================================
  NOTIFICATION EVENTS
  ============================================================================= }

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
