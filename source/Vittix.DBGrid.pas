unit Vittix.DBGrid;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.Graphics, // Needed for TColor
  Vcl.Controls,
  Data.DB,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.ColumnChooser,
  Vittix.DBGrid.Filter.Popup,
  Vittix.DBGrid.Layout;

type
  TVittixDBGrid = class(TDBGrid)
  private
    FColumnsInfo: TVittixDBGridColumns;
    FController: TComponent;
    FFooterVisible: Boolean;
    FLayoutStorageFileName: string;
    FChooserStateFileName: string;
    FFilterHistoryFileName: string;
    FPersistenceRootPath: string;

    // Local storage for design-time properties before Controller is ready
    FAlternatingRowColors: Boolean;
    FAlternateRowColor: TColor;
    procedure SyncColumnInfo;
    procedure SetFooterVisible(const Value: Boolean);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);

    // Accessor methods
    function GetAlternatingRowColors: Boolean;
    procedure SetAlternatingRowColors(const Value: Boolean);
    function GetAlternateRowColor: TColor;
    procedure SetAlternateRowColor(const Value: TColor);
    procedure SetLayoutStorageFileName(const Value: string);
    procedure SetChooserStateFileName(const Value: string);
    procedure SetFilterHistoryFileName(const Value: string);
    procedure SetPersistenceRootPath(const Value: string);

  protected
    procedure Loaded; override;
    procedure LayoutChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function ColumnInfoByColumn(Column: TColumn): TVittixDBGridColumnInfo;
    function GetIndicatorWidth: Integer;

    property Controller: TComponent read FController;
    property ColumnInfo: TVittixDBGridColumns read FColumnsInfo;
  published
    property FooterVisible: Boolean read FFooterVisible write SetFooterVisible default True;

    property AlternatingRowColors: Boolean
      read GetAlternatingRowColors write SetAlternatingRowColors default True;

    property AlternateRowColor: TColor
      read GetAlternateRowColor write SetAlternateRowColor default $00F7F7F7;

    property LayoutStorageFileName: string
      read FLayoutStorageFileName write SetLayoutStorageFileName;

    property ChooserStateFileName: string
      read FChooserStateFileName write SetChooserStateFileName;

    property FilterHistoryFileName: string
      read FFilterHistoryFileName write SetFilterHistoryFileName;

    property PersistenceRootPath: string
      read FPersistenceRootPath write SetPersistenceRootPath;

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

uses
  Vittix.DBGrid.Controller;

{ TVittixDBGrid }

constructor TVittixDBGrid.Create(AOwner: TComponent);
var
  Ctrl: TVittixDBGridController;
begin
  inherited;
  FColumnsInfo := TVittixDBGridColumns.Create(Self);

  // Set Defaults
  FFooterVisible := True;
  FAlternatingRowColors := True;
  FAlternateRowColor := $00F7F7F7;

  // Create controller with Self as owner so it is freed automatically.
  // We pass the property values directly — SetGrid is called inside but
  // HookGrid is fully deferred to Loaded (where csDesigning is reliable).
  Ctrl := TVittixDBGridController.Create(Self);
  FController := Ctrl;

  // These property setters are safe: they only store values, no window access.
  Ctrl.AlternatingRowColors := FAlternatingRowColors;
  Ctrl.AlternateRowColor := FAlternateRowColor;

  // SetGrid / SetShowFooter must NOT hook anything during construction.
  // The guards inside those methods check csDesigning, but csDesigning is
  // only set AFTER the constructor returns when placed in the IDE.
  // We call them last so all other fields are initialised first,
  // and Loaded will do the actual hooking at the right time.
  Ctrl.ShowFooter := FFooterVisible;
  Ctrl.Grid := Self;
end;

procedure TVittixDBGrid.BeforeDestruction;
begin
  if Assigned(FController) and (FController is TVittixDBGridController) then
    TVittixDBGridController(FController).Detach;

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
    if Assigned(FController) and (FController is TVittixDBGridController) then
      TVittixDBGridController(FController).DataSourceChanged;
  end;
end;

procedure TVittixDBGrid.SetFooterVisible(const Value: Boolean);
begin
  if FFooterVisible <> Value then
  begin
    FFooterVisible := Value;
    if Assigned(FController) and (FController is TVittixDBGridController) then
      TVittixDBGridController(FController).ShowFooter := Value;
  end;
end;

// --- Accessor Methods ---

function TVittixDBGrid.GetAlternatingRowColors: Boolean;
begin
  Result := FAlternatingRowColors;
end;

procedure TVittixDBGrid.SetAlternatingRowColors(const Value: Boolean);
begin
  if FAlternatingRowColors <> Value then
  begin
    FAlternatingRowColors := Value;
    if Assigned(FController) and (FController is TVittixDBGridController) then
    begin
      TVittixDBGridController(FController).AlternatingRowColors := Value;
      Invalidate;
    end;
  end;
end;

function TVittixDBGrid.GetAlternateRowColor: TColor;
begin
  Result := FAlternateRowColor;
end;

procedure TVittixDBGrid.SetAlternateRowColor(const Value: TColor);
begin
  if FAlternateRowColor <> Value then
  begin
    FAlternateRowColor := Value;
    if Assigned(FController) and (FController is TVittixDBGridController) then
    begin
      TVittixDBGridController(FController).AlternateRowColor := Value;
      if FAlternatingRowColors then Invalidate;
    end;
  end;
end;
// ------------------------

procedure TVittixDBGrid.CreateWnd;
begin
  inherited;
  // Now that a real Win32 window handle exists, install the WindowProc hook
  // if the controller is ready but couldn't hook it earlier (e.g. when
  // HookGrid ran before the handle was allocated at runtime).
  if Assigned(FController) and (FController is TVittixDBGridController) then
  begin
    TVittixDBGridController(FController).InstallWindowProc;
    TVittixDBGridController(FController).GridLayoutChanged;
  end;
end;

procedure TVittixDBGrid.SetLayoutStorageFileName(const Value: string);
begin
  if FLayoutStorageFileName <> Value then
  begin
    FLayoutStorageFileName := Value;
    TVittixDBGridLayoutJsonStorage.StateFileName := Value;
  end;
end;

procedure TVittixDBGrid.SetChooserStateFileName(const Value: string);
begin
  if FChooserStateFileName <> Value then
  begin
    FChooserStateFileName := Value;
    TVittixDBGridColumnChooserForm.StateFileName := Value;
  end;
end;

procedure TVittixDBGrid.SetFilterHistoryFileName(const Value: string);
begin
  if FFilterHistoryFileName <> Value then
  begin
    FFilterHistoryFileName := Value;
    TVittixDBGridFilterPopup.HistoryFileName := Value;
  end;
end;

procedure TVittixDBGrid.SetPersistenceRootPath(const Value: string);
begin
  if FPersistenceRootPath <> Value then
  begin
    FPersistenceRootPath := Value;
    TVittixDBGridLayoutJsonStorage.RootPath := Value;
    TVittixDBGridColumnChooserForm.RootPath := Value;
    TVittixDBGridFilterPopup.RootPath := Value;
  end;
end;

procedure TVittixDBGrid.Loaded;
begin
  inherited;
  SyncColumnInfo;

  // DESIGN-TIME SAFETY: Never trigger engine/dataset initialization while
  // the IDE is streaming the DFM. Only do this at true runtime.
  if csDesigning in ComponentState then Exit;

  if Assigned(FController) and (FController is TVittixDBGridController) then
  begin
    TVittixDBGridController(FController).DataSourceChanged;
    TVittixDBGridController(FController).GridLayoutChanged;
  end;
end;

procedure TVittixDBGrid.LayoutChanged;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    SyncColumnInfo;

    if Assigned(FController) and (FController is TVittixDBGridController) then
      TVittixDBGridController(FController).GridLayoutChanged;
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

    if Assigned(FController) and (FController is TVittixDBGridController) then
      TVittixDBGridController(FController).DataSourceChanged;
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

end.
