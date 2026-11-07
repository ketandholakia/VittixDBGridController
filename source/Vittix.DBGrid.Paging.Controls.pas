unit Vittix.DBGrid.Paging.Controls;

interface

uses
  System.Classes,
  Vittix.DBGrid.Paging.Controls.Base,
  Vittix.DBGrid.Paging.FireDAC;

type
  /// <summary>Navigation bar for a FireDAC query pager. The caller owns the
  /// pager. Call UpdatePageState after navigating it outside this bar.</summary>
  TVittixFireDACPagerBar = class(TVittixPagerBar)
  private
    FPager: TVittixFireDACPager;
    procedure SetPager(AValue: TVittixFireDACPager);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Navigate(AAction: TVittixPageAction); override;
  public
    procedure UpdatePageState; override;
    property Pager: TVittixFireDACPager read FPager write SetPager;
  end;

implementation

procedure TVittixFireDACPagerBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPager) then
  begin
    FPager := nil;
    UpdatePageState;
  end;
end;

procedure TVittixFireDACPagerBar.SetPager(AValue: TVittixFireDACPager);
begin
  if FPager = AValue then
    Exit;
  if Assigned(FPager) then
    FPager.RemoveFreeNotification(Self);
  FPager := AValue;
  if Assigned(FPager) then
    FPager.FreeNotification(Self);
  UpdatePageState;
end;

procedure TVittixFireDACPagerBar.UpdatePageState;
var
  State: TVittixPagerState;
begin
  if Assigned(FPager) then
  begin
    State := Default(TVittixPagerState);
    State.Connected := True;
    State.Loaded := FPager.Enabled;
    State.PageIndex := FPager.PageIndex;
    State.CanNext := True;
    // The FireDAC pager has no total count, so TotalKnown stays False and
    // the bar keeps the probe-based look.
    SetPageState(State);
  end
  else
    SetPageState(TVittixPagerState.Off);
end;

procedure TVittixFireDACPagerBar.Navigate(AAction: TVittixPageAction);
begin
  if not Assigned(FPager) then
    Exit;
  case AAction of
    vpaFirst: FPager.OpenPage(0);
    vpaPrevious: FPager.PreviousPage;
    vpaNext: FPager.NextPage;
    vpaLast: ; // no known total count, so the disabled button cannot fire
  end;
end;

end.
