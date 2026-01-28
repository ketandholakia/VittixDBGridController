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
  Vittix.DBGrid.ColumnInfo;

type
  TVittixDBGrid = class(TDBGrid)
  private
    FColumnsInfo: TVittixDBGridColumns;
    FController: TComponent;
    FFooterVisible: Boolean;

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

  protected
    procedure Loaded; override;
    procedure LayoutChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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

  Ctrl := TVittixDBGridController.Create(Self);
  FController := Ctrl;
  
  Ctrl.Grid := Self;
  Ctrl.ShowFooter := FFooterVisible;
  Ctrl.AlternatingRowColors := FAlternatingRowColors;
  Ctrl.AlternateRowColor := FAlternateRowColor;
end;

destructor TVittixDBGrid.Destroy;
begin
  // 1. Destroy Controller FIRST.
  // This ensures the Controller unhooks itself while the Grid is still valid.
  if Assigned(FController) then
  begin
    FController.Free;
    FController := nil;
  end;

  // 2. Destroy metadata.
  FreeAndNil(FColumnsInfo);

  // 3. Destroy VCL Grid (FDataLink will be freed here).
  inherited;
end;

function TVittixDBGrid.GetDataSource: TDataSource;
begin
  Result := inherited DataSource;
end;

procedure TVittixDBGrid.SetDataSource(Value: TDataSource);
begin
  if inherited DataSource <> Value then
  begin
    inherited DataSource := Value;
    // Notify Controller of runtime changes
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

procedure TVittixDBGrid.Loaded;
begin
  inherited;
  SyncColumnInfo;
  
  // Initialize the controller connection if DataSource was set in DFM
  if Assigned(FController) and (FController is TVittixDBGridController) then
    TVittixDBGridController(FController).DataSourceChanged;
end;

procedure TVittixDBGrid.LayoutChanged;
begin
  inherited;
  if not (csLoading in ComponentState) then
    SyncColumnInfo;
end;

procedure TVittixDBGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  
  // FIX: Access Violation on Close.
  // During destruction, TCustomDBGrid frees FDataLink. 
  // Accessing the 'DataSource' property calls GetDataSource, which reads FDataLink.
  // If we are destroying, FDataLink might be nil, causing AV 00000004.
  if csDestroying in ComponentState then Exit;

  if (Operation = opRemove) and (AComponent = DataSource) then
  begin
    // Notify controller BEFORE clearing
    if Assigned(FController) and (FController is TVittixDBGridController) then
      TVittixDBGridController(FController).DataSourceChanged;
    DataSource := nil; 
  end;
end;

procedure TVittixDBGrid.SyncColumnInfo;
var
  I: Integer;
  Col: TColumn;
  Info: TVittixDBGridColumnInfo;
begin
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