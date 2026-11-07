unit Vittix.DBGrid.Paging.FireDAC;

interface

uses
  System.Classes,
  System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client;

type
  /// <summary>Opt-in row paging for a TFDQuery bound to a DB grid. The query
  /// should have a stable ORDER BY so records do not move between pages.
  /// FireDAC may apply the limit on the client when a DBMS cannot push it
  /// into SQL. The pager leaves other dataset types untouched.</summary>
  TVittixFireDACPager = class(TComponent)
  private
    FQuery: TFDQuery;
    FPageSize: Integer;
    FPageIndex: Integer;
    FEnabled: Boolean;
    FOriginalSkip: Integer;
    FOriginalMax: Integer;
    FOriginalActive: Boolean;
    procedure ReopenWithLimits(ASkip, AMax: Integer; AOpen: Boolean);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AQuery: TFDQuery; APageSize: Integer); reintroduce;
    destructor Destroy; override;

    /// <summary>Opens zero-based page AIndex. Returns False when a page past
    /// the first contains no rows and restores the previous page.</summary>
    function OpenPage(AIndex: Integer): Boolean;
    function NextPage: Boolean;
    function PreviousPage: Boolean;

    /// <summary>Restores the query's original RecsSkip, RecsMax, and open
    /// state. An active edit must be posted or canceled by the caller first.
    /// </summary>
    procedure Disable;

    property Enabled: Boolean read FEnabled;
    property PageIndex: Integer read FPageIndex;
    property PageSize: Integer read FPageSize;
  end;

implementation

constructor TVittixFireDACPager.Create(AQuery: TFDQuery; APageSize: Integer);
begin
  inherited Create(nil);
  if not Assigned(AQuery) then
    raise EArgumentNilException.Create('Paging query is nil');
  if APageSize <= 0 then
    raise EArgumentOutOfRangeException.Create('Page size must be positive');
  FQuery := AQuery;
  FQuery.FreeNotification(Self);
  FPageSize := APageSize;
  FPageIndex := -1;
end;

destructor TVittixFireDACPager.Destroy;
begin
  if Assigned(FQuery) then
  begin
    try
      Disable;
    except
      // Destruction may follow a query/connection teardown. The query's
      // FreeNotification keeps FQuery nil if it was destroyed first.
    end;
    FQuery.RemoveFreeNotification(Self);
  end;
  inherited;
end;

procedure TVittixFireDACPager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FQuery) then
  begin
    FQuery := nil;
    FEnabled := False;
    FPageIndex := -1;
  end;
end;

procedure TVittixFireDACPager.ReopenWithLimits(ASkip, AMax: Integer;
  AOpen: Boolean);
begin
  FQuery.Disconnect; // Unprepares the command before changing fetch limits.
  FQuery.FetchOptions.RecsSkip := ASkip;
  FQuery.FetchOptions.RecsMax := AMax;
  if AOpen then
    FQuery.Open;
end;

function TVittixFireDACPager.OpenPage(AIndex: Integer): Boolean;
var
  OldSkip, OldMax: Integer;
  OldActive: Boolean;
begin
  if not Assigned(FQuery) then
    raise EInvalidOperation.Create('Paging query has been destroyed');
  if AIndex < 0 then
    raise EArgumentOutOfRangeException.Create('Page index must be nonnegative');
  if AIndex > MaxInt div FPageSize then
    raise EArgumentOutOfRangeException.Create('Page offset exceeds Integer range');
  if FQuery.State in dsEditModes then
    raise EInvalidOperation.Create('Post or cancel the edit before changing pages');

  OldSkip := FQuery.FetchOptions.RecsSkip;
  OldMax := FQuery.FetchOptions.RecsMax;
  OldActive := FQuery.Active;
  if not FEnabled then
  begin
    FOriginalSkip := OldSkip;
    FOriginalMax := OldMax;
    FOriginalActive := OldActive;
  end;

  FQuery.DisableControls;
  try
    try
      ReopenWithLimits(AIndex * FPageSize, FPageSize, True);
      if (AIndex > 0) and FQuery.IsEmpty then
      begin
        ReopenWithLimits(OldSkip, OldMax, OldActive);
        Exit(False);
      end;
    except
      try
        ReopenWithLimits(OldSkip, OldMax, OldActive);
      except
        // Keep the original re-query error.
      end;
      raise;
    end;
    FEnabled := True;
    FPageIndex := AIndex;
    Result := True;
  finally
    FQuery.EnableControls;
  end;
end;

function TVittixFireDACPager.NextPage: Boolean;
begin
  if not FEnabled then
    Result := OpenPage(0)
  else
    Result := OpenPage(FPageIndex + 1);
end;

function TVittixFireDACPager.PreviousPage: Boolean;
begin
  if not FEnabled or (FPageIndex <= 0) then
    Exit(False);
  Result := OpenPage(FPageIndex - 1);
end;

procedure TVittixFireDACPager.Disable;
begin
  if not FEnabled or not Assigned(FQuery) then
    Exit;
  if FQuery.State in dsEditModes then
    raise EInvalidOperation.Create('Post or cancel the edit before disabling paging');

  FQuery.DisableControls;
  try
    ReopenWithLimits(FOriginalSkip, FOriginalMax, FOriginalActive);
    FEnabled := False;
    FPageIndex := -1;
  finally
    FQuery.EnableControls;
  end;
end;

end.
