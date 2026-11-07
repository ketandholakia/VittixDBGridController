unit Vittix.DBGrid.Paging.DataSet.Controls;

interface

uses
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  Data.DB,
  Vcl.Controls,
  Vcl.DBCtrls,
  Vittix.DBGrid.Paging.Controls.Base,
  Vittix.DBGrid.Paging.DataSet,
  Vittix.DBGrid.Paging.GlobalScroll;

type
  TVittixGlobalNavigator = class;

  TVittixNavigatorLink = class(TDataLink)
  private
    FNavigator: TVittixGlobalNavigator;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  public
    constructor Create(ANavigator: TVittixGlobalNavigator);
  end;

  /// <summary>A DB navigator whose First and Last buttons use global
  /// endpoints when the bound dataset supports active virtual scrolling.
  /// All other actions retain TDBNavigator behavior.</summary>
  TVittixGlobalNavigator = class(TDBNavigator)
  private
    FLink: TVittixNavigatorLink;
    function GetNavigatorDataSource: TDataSource;
    procedure SetNavigatorDataSource(AValue: TDataSource);
    procedure UpdateGlobalButtons;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BtnClick(Index: TNavigateBtn); override;
  published
    property DataSource: TDataSource read GetNavigatorDataSource write SetNavigatorDataSource;
  end;

  /// <summary>Navigation bar for a page-backed dataset. Call
  /// UpdatePageState after navigating the dataset outside this bar.</summary>
  TVittixPagedDataSetBar = class(TVittixPagerBar)
  private
    FDataSet: TVittixPagedDataSet;
    procedure SetDataSet(AValue: TVittixPagedDataSet);
    procedure PageChanged(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Navigate(AAction: TVittixPageAction); override;
  public
    destructor Destroy; override;
    procedure UpdatePageState; override;
    property DataSet: TVittixPagedDataSet read FDataSet write SetDataSet;
  end;

implementation

constructor TVittixNavigatorLink.Create(ANavigator: TVittixGlobalNavigator);
begin
  inherited Create;
  FNavigator := ANavigator;
  VisualControl := True;
end;

procedure TVittixNavigatorLink.ActiveChanged;
begin
  FNavigator.UpdateGlobalButtons;
end;

procedure TVittixNavigatorLink.DataSetChanged;
begin
  FNavigator.UpdateGlobalButtons;
end;

constructor TVittixGlobalNavigator.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TVittixNavigatorLink.Create(Self);
end;

destructor TVittixGlobalNavigator.Destroy;
begin
  FLink.Free;
  inherited;
end;

procedure TVittixGlobalNavigator.SetNavigatorDataSource(AValue: TDataSource);
begin
  inherited DataSource := AValue;
  FLink.DataSource := AValue;
  UpdateGlobalButtons;
end;

function TVittixGlobalNavigator.GetNavigatorDataSource: TDataSource;
begin
  Result := inherited DataSource;
end;

procedure TVittixGlobalNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  UpdateGlobalButtons;
end;

procedure TVittixGlobalNavigator.UpdateGlobalButtons;
var
  Paged: IVittixGlobalPageScroll;
  DataSet: TDataSet;
begin
  if (DataSource = nil) or (DataSource.DataSet = nil) then
    Exit;
  DataSet := DataSource.DataSet;
  if not (Enabled and DataSet.Active and
    Supports(DataSet, IVittixGlobalPageScroll, Paged) and
    Paged.GlobalScrollActive) then
    Exit;
  Buttons[nbFirst].Enabled := DataSet.RecNo > 1;
  Buttons[nbLast].Enabled := DataSet.RecNo < Paged.GlobalRecordCount;
end;

procedure TVittixGlobalNavigator.BtnClick(Index: TNavigateBtn);
var
  Paged: IVittixGlobalPageScroll;
begin
  if (Index in [nbFirst, nbLast]) and (DataSource <> nil) and
    (DataSource.DataSet <> nil) and DataSource.DataSet.Active and
    Supports(DataSource.DataSet, IVittixGlobalPageScroll, Paged) and
    Paged.GlobalScrollActive then
  begin
    if not (csDesigning in ComponentState) and Assigned(BeforeAction) then
      BeforeAction(Self, Index);
    if Index = nbFirst then
      Paged.GlobalFirst
    else
      Paged.GlobalLast;
    if not (csDesigning in ComponentState) and Assigned(OnClick) then
      OnClick(Self, Index);
  end
  else
    inherited BtnClick(Index);
end;

destructor TVittixPagedDataSetBar.Destroy;
begin
  SetDataSet(nil);
  inherited;
end;

procedure TVittixPagedDataSetBar.PageChanged(Sender: TObject);
begin
  UpdatePageState;
end;

procedure TVittixPagedDataSetBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDataSet) then
  begin
    FDataSet := nil;
    UpdatePageState;
  end;
end;

procedure TVittixPagedDataSetBar.SetDataSet(AValue: TVittixPagedDataSet);
begin
  if FDataSet = AValue then
    Exit;
  if Assigned(FDataSet) then
  begin
    FDataSet.RemovePageObserver(PageChanged);
    FDataSet.RemoveFreeNotification(Self);
  end;
  FDataSet := AValue;
  if Assigned(FDataSet) then
  begin
    FDataSet.FreeNotification(Self);
    FDataSet.AddPageObserver(PageChanged);
  end;
  UpdatePageState;
end;

procedure TVittixPagedDataSetBar.UpdatePageState;
var
  State: TVittixPagerState;
begin
  if Assigned(FDataSet) then
  begin
    State := Default(TVittixPagerState);
    State.Connected := True;
    State.Loaded := FDataSet.PageLoaded;
    State.PageIndex := FDataSet.PageIndex;
    State.CanNext := not FDataSet.PageLoaded or FDataSet.HasMore;
    State.TotalKnown := FDataSet.TotalCountKnown;
    State.PageCount := FDataSet.PageCount;
    SetPageState(State);
  end
  else
    SetPageState(TVittixPagerState.Off);
end;

procedure TVittixPagedDataSetBar.Navigate(AAction: TVittixPageAction);
begin
  if not Assigned(FDataSet) then
    Exit;
  case AAction of
    vpaFirst: FDataSet.OpenPage(0);
    vpaPrevious: FDataSet.PreviousPage;
    vpaNext: FDataSet.NextPage;
    vpaLast: FDataSet.LastPage;
  end;
end;

end.
