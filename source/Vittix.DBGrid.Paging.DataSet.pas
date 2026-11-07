unit Vittix.DBGrid.Paging.DataSet;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Data.DB,
  Datasnap.DBClient,
  Vittix.DBGrid.Paging.GlobalScroll;

type
  TVittixLoadPageEvent = procedure(Sender: TObject; APageIndex,
    APageSize: Integer; ADataSet: TClientDataSet) of object;

  /// <summary>A page-backed dataset for any source that can load one page
  /// into a TClientDataSet. Define FieldDefs, assign OnLoadPage, then bind
  /// this dataset to the grid. Only the current page is held in memory.</summary>
  TVittixPagedDataSet = class(TClientDataSet, IVittixGlobalPageScroll)
  private
    FPageSize: Integer;
    FPageIndex: Integer;
    FPageLoaded: Boolean;
    FLoading: Boolean;
    FAutoPageOnScroll: Boolean;
    FHasMore: Boolean;
    FTotalCount: Integer;
    FPageGeneration: Integer;
    FVirtualScroll: Boolean;
    FOnLoadPage: TVittixLoadPageEvent;
    FPageObservers: TList<TNotifyEvent>;
    procedure SetPageSize(AValue: Integer);
    procedure SetTotalCount(AValue: Integer);
    procedure PositionToLocalRow(ALocalRec: Integer);
    function GetTotalCountKnown: Boolean;
    function GetPageCount: Integer;
    procedure NotifyPageObservers;
  protected
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddPageObserver(AObserver: TNotifyEvent);
    procedure RemovePageObserver(AObserver: TNotifyEvent);
    function MoveBy(Distance: Integer): Integer; override;
    function OpenPage(AIndex: Integer): Boolean;
    function NextPage: Boolean;
    function PreviousPage: Boolean;
    function LastPage: Boolean;
    function GetBookmark: TBookmark; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    /// <summary>Positions on the first global row (page zero). Equal to
    /// OpenPage(0) unless page zero with its first row is already loaded.</summary>
    procedure GlobalFirst;
    /// <summary>Positions on the last global row, loading the final page and
    /// moving to its last record. Requires a known TotalCount; otherwise it
    /// falls back to the page-local Last.</summary>
    procedure GlobalLast;
    function GlobalScrollActive: Boolean;
    function GlobalRecordCount: Integer;
    /// <summary>Rows currently present in the loaded page. Unlike
    /// RecordCount, this stays page-local while VirtualScroll is active.
    /// Reading it does not move the dataset cursor.</summary>
    function LoadedRecordCount: Integer;
    property PageIndex: Integer read FPageIndex;
    property PageLoaded: Boolean read FPageLoaded;
    property HasMore: Boolean read FHasMore;
    /// <summary>Number of pages implied by TotalCount, or -1 when the total
    /// count is unknown. Never below one: an empty source still has its
    /// single empty page zero.</summary>
    property PageCount: Integer read GetPageCount;
    property TotalCountKnown: Boolean read GetTotalCountKnown;
  published
    property PageSize: Integer read FPageSize write SetPageSize default 100;
    property AutoPageOnScroll: Boolean read FAutoPageOnScroll
      write FAutoPageOnScroll default False;
    /// <summary>Optional total source row count supplied by the caller, for
    /// example from a SELECT COUNT query the loader runs. -1 (default) means
    /// unknown and keeps the probe-based navigation. Callers must keep this
    /// current; a stale count stops navigation at the counted page boundary.
    /// </summary>
    property TotalCount: Integer read FTotalCount write SetTotalCount default -1;
    /// <summary>Exposes the source's total row count as RecordCount and the
    /// global row position as RecNo so the bound TDBGrid scrollbar covers
    /// every row while only one page stays in memory. Requires TotalCount;
    /// without it the dataset keeps page-local RecordCount/RecNo semantics.
    /// RecNo assignment seeks across pages. TDataSet.First/Last (non-virtual)
    /// remain page-local: use RecNo, MoveBy, the navigation bar, or the
    /// grid, which routes its endpoint navigation through GlobalFirst /
    /// GlobalLast. Default False.</summary>
    property VirtualScroll: Boolean read FVirtualScroll
      write FVirtualScroll default False;
    property OnLoadPage: TVittixLoadPageEvent read FOnLoadPage write FOnLoadPage;
  end;

implementation

uses
  System.Variants;

const
  // Page-scoped bookmarks (virtual mode only): a 16-byte tagged array with
  // the magic "D9 01 6B A5" header, the embedded NATIVE CDS bookmark
  // (record identity), the page index, the 1-based local row, and a
  // generation that changes on every successful page load. The native
  // part makes navigation land on the exact record even while a visible
  // grid's window management moves the cursor; the page and generation
  // parts refuse anything captured on a page that is no longer loaded or
  // while the mode is suspended. Anything not matching the magic is a
  // plain CDS bookmark and passes straight through.
  VittixPageBookmarkHeader = 20; // magic + native length + page + row + gen

procedure EncodeDWord(AValue, AOffset: Integer; var Target: TBookmark);
begin
  Target[AOffset] := Byte(AValue);
  Target[AOffset + 1] := Byte(AValue shr 8);
  Target[AOffset + 2] := Byte(AValue shr 16);
  Target[AOffset + 3] := Byte(AValue shr 24);
end;

function DecodeDWord(const Source: TBookmark; AOffset: Integer): Integer;
begin
  Result := Integer(Source[AOffset]) or (Integer(Source[AOffset + 1]) shl 8) or
    (Integer(Source[AOffset + 2]) shl 16) or (Integer(Source[AOffset + 3]) shl 24);
end;

function PageBookmarkFields(const Bookmark: TBookmark;
  out ANative: TBookmark; out APageIndex, ALocalRecNo,
  AGeneration: Integer): Boolean;
var
  NativeLen: Integer;
begin
  Result := (Length(Bookmark) >= VittixPageBookmarkHeader) and
    (Bookmark[0] = $D9) and (Bookmark[1] = $01) and
    (Bookmark[2] = $6B) and (Bookmark[3] = $A5);
  if not Result then
    Exit;
  NativeLen := DecodeDWord(Bookmark, 4);
  Result := Length(Bookmark) = VittixPageBookmarkHeader + NativeLen;
  if not Result then
    Exit;
  SetLength(ANative, NativeLen);
  if NativeLen > 0 then
    Move(Bookmark[8], ANative[0], NativeLen);
  APageIndex := DecodeDWord(Bookmark, 8 + NativeLen);
  ALocalRecNo := DecodeDWord(Bookmark, 12 + NativeLen);
  AGeneration := DecodeDWord(Bookmark, 16 + NativeLen);
end;

constructor TVittixPagedDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FPageSize := 100;
  FPageIndex := -1;
  FTotalCount := -1;
  FPageObservers := TList<TNotifyEvent>.Create;
end;

destructor TVittixPagedDataSet.Destroy;
begin
  FPageObservers.Free;
  inherited;
end;

procedure TVittixPagedDataSet.AddPageObserver(AObserver: TNotifyEvent);
begin
  FPageObservers.Add(AObserver);
end;

procedure TVittixPagedDataSet.RemovePageObserver(AObserver: TNotifyEvent);
var
  I: Integer;
begin
  for I := FPageObservers.Count - 1 downto 0 do
    if (TMethod(FPageObservers[I]).Code = TMethod(AObserver).Code) and
      (TMethod(FPageObservers[I]).Data = TMethod(AObserver).Data) then
      FPageObservers.Delete(I);
end;

procedure TVittixPagedDataSet.NotifyPageObservers;
var
  Observer: TNotifyEvent;
begin
  for Observer in FPageObservers.ToArray do
    Observer(Self);
end;

procedure TVittixPagedDataSet.SetPageSize(AValue: Integer);
begin
  if AValue <= 0 then
    raise EArgumentOutOfRangeException.Create('Page size must be positive');
  if FLoading then
    raise EInvalidOperation.Create('Cannot change page size while loading');
  if FPageLoaded and (AValue <> FPageSize) then
    raise EInvalidOperation.Create('Create a new dataset to change page size');
  FPageSize := AValue;
end;

procedure TVittixPagedDataSet.SetTotalCount(AValue: Integer);
begin
  if AValue < -1 then
    raise EArgumentOutOfRangeException.Create(
      'Total count must be -1 (unknown) or nonnegative');
  if AValue = FTotalCount then
    Exit;
  FTotalCount := AValue;
  // The count becomes the authority for the loaded page's has-more state so
  // count changes take effect without waiting for the next page load.
  if FPageLoaded and not FLoading then
  begin
    if TotalCountKnown then
      FHasMore := FPageIndex < GetPageCount - 1
    else
      FHasMore := inherited GetRecordCount = FPageSize;
  end;
  NotifyPageObservers;
end;

function TVittixPagedDataSet.GetTotalCountKnown: Boolean;
begin
  Result := FTotalCount >= 0;
end;

function TVittixPagedDataSet.GetPageCount: Integer;
begin
  if not TotalCountKnown then
    Exit(-1);
  Result := (FTotalCount - 1) div FPageSize + 1;
  if Result < 1 then
    Result := 1; // an empty source still has its single empty page zero
end;

function TVittixPagedDataSet.OpenPage(AIndex: Integer): Boolean;
var
  Scratch: TClientDataSet;
  WasActive: Boolean;
  Committed: Boolean;
begin
  if AIndex < 0 then
    raise EArgumentOutOfRangeException.Create('Page index must be nonnegative');
  if AIndex > MaxInt div FPageSize then
    raise EArgumentOutOfRangeException.Create('Page offset exceeds Integer range');
  if FLoading then
    raise EInvalidOperation.Create('Page load is already in progress');
  if State in dsEditModes then
    raise EInvalidOperation.Create('Post or cancel the edit before changing pages');
  if not Assigned(FOnLoadPage) then
    raise EInvalidOperation.Create('Assign OnLoadPage before opening a page');

  // Load the page into an internal scratch dataset and adopt its packet
  // through a single atomic Data assignment. The former approach —
  // EmptyDataSet + loader appends into this dataset — churned the
  // TClientDataSet packet under a live visible data link, which displaced
  // MIDAS packet state and broke absolute navigation afterward (verified:
  // SetRecNo/First landed off-target inside delete flows, GetRecNo raised
  // at the cache end, TBookmarkList.Refresh hung). With the atomic swap
  // the loaded page replaces the packet in one step and any loader
  // failure simply discards the scratch, leaving this dataset untouched.
  WasActive := Active;
  if not WasActive then
    CreateDataSet;
  Scratch := TClientDataSet.Create(nil);
  Committed := False;
  FLoading := True;
  DisableControls;
  try
    Scratch.FieldDefs.Assign(FieldDefs);
    Scratch.CreateDataSet;
    FOnLoadPage(Self, AIndex, FPageSize, Scratch);
    if Scratch.RecordCount > FPageSize then
      raise EInvalidOperation.Create('Page loader returned more than PageSize rows');
    if (AIndex > 0) and (Scratch.RecordCount = 0) then
    begin
      // Past the final page: discard the scratch and report failure; this
      // dataset was never touched, so the loaded page stays intact.
      FHasMore := False;
      NotifyPageObservers;
      Exit(False);
    end;
    Data := Scratch.Data;
    First;
    if TotalCountKnown then
      FHasMore := AIndex < GetPageCount - 1
    else
      FHasMore := inherited GetRecordCount = FPageSize;
    Committed := True;
  finally
    FreeAndNil(Scratch);
    EnableControls;
    FLoading := False;
    if not WasActive and not Committed then
      Close;
  end;
  FPageIndex := AIndex;
  FPageLoaded := True;
  Inc(FPageGeneration);
  NotifyPageObservers;
  Result := True;
end;

function TVittixPagedDataSet.MoveBy(Distance: Integer): Integer;
var
  Remaining, Moved: Integer;
begin
  if not (FAutoPageOnScroll or GlobalScrollActive) or FLoading or
    ControlsDisabled or
    not FPageLoaded or
    (Distance = 0) then
    Exit(inherited MoveBy(Distance));

  Result := 0;
  Remaining := Distance;
  while Remaining <> 0 do
  begin
    Moved := inherited MoveBy(Remaining);
    Inc(Result, Moved);
    Dec(Remaining, Moved);
    if Remaining = 0 then
      Break;
    if Remaining > 0 then
    begin
      if not FHasMore or (FPageIndex = MaxInt) or
        not OpenPage(FPageIndex + 1) then
      begin
        inherited MoveBy(1); // Restore EOF after an empty-page rollback.
        Break;
      end;
      Inc(Result);
      Dec(Remaining);
    end
    else
    begin
      if FPageIndex <= 0 then
        Break;
      if not OpenPage(FPageIndex - 1) then
      begin
        inherited MoveBy(-1);
        Break;
      end;
      Last;
      Dec(Result);
      Inc(Remaining);
    end;
  end;
end;

function TVittixPagedDataSet.NextPage: Boolean;
begin
  if not FPageLoaded then
    Result := OpenPage(0)
  else if TotalCountKnown and (FPageIndex >= PageCount - 1) then
    Result := False // the known count ends here; no empty-page probe
  else if FPageIndex = MaxInt then
    Result := False
  else
    Result := OpenPage(FPageIndex + 1);
end;

function TVittixPagedDataSet.PreviousPage: Boolean;
begin
  if not FPageLoaded or (FPageIndex <= 0) then
    Exit(False);
  Result := OpenPage(FPageIndex - 1);
end;

function TVittixPagedDataSet.LastPage: Boolean;
begin
  if not TotalCountKnown then
    Exit(False);
  Result := OpenPage(GetPageCount - 1);
end;

function TVittixPagedDataSet.GlobalScrollActive: Boolean;
begin
  // Client filters are page-local: the global row positions and the total
  // have no truthful meaning for the filtered view, so virtual mode
  // suspends while Filtered is active and resumes when it clears.
  Result := FVirtualScroll and TotalCountKnown and not Filtered;
end;

function TVittixPagedDataSet.GlobalRecordCount: Integer;
begin
  if GlobalScrollActive then
    Result := FTotalCount
  else
    Result := inherited GetRecordCount;
end;

function TVittixPagedDataSet.LoadedRecordCount: Integer;
begin
  Result := inherited GetRecordCount;
end;

procedure TVittixPagedDataSet.GlobalFirst;
begin
  if FPageLoaded and (FPageIndex = 0) then
    First
  else
    OpenPage(0);
end;

procedure TVittixPagedDataSet.GlobalLast;
begin
  if not GlobalScrollActive then
  begin
    // The true last page is unknown; fall back to the page-local end.
    Last;
    Exit;
  end;
  if not (FPageLoaded and (FPageIndex = PageCount - 1)) then
    LastPage;
  Last;
end;

function TVittixPagedDataSet.GetRecordCount: Integer;
begin
  if GlobalScrollActive then
    Result := FTotalCount
  else
    Result := inherited GetRecordCount;
end;

function TVittixPagedDataSet.GetRecNo: Integer;
var
  LocalRec: Integer;
begin
  if GlobalScrollActive and FPageLoaded and not FLoading then
  begin
    // Reading CDS RecNo at the loaded page's cache-end state raises
    // 'At end of table' with a visible grid attached (verified); treat
    // that state as the one-past-last local row so the grid's scrollbar
    // update — which reads RecNo on every paint — cannot raise.
    try
      LocalRec := inherited GetRecNo;
    except
      on E: EDBClient do
        LocalRec := inherited GetRecordCount + 1;
    end;
    Result := FPageIndex * FPageSize + LocalRec;
  end
  else
    Result := inherited GetRecNo;
end;

procedure TVittixPagedDataSet.PositionToLocalRow(ALocalRec: Integer);
var
  Attempts, Current: Integer;
begin
  // Position within the loaded page using relative moves only. Every
  // absolute seek is unreliable with a visible grid attached in some
  // state — TClientDataSet.SetRecNo lands off-target inside disabled-
  // controls delete flows (verified: victims shifted run to run), First
  // + MoveBy mis-lands at packet boundaries — while CDS MoveBy is exact
  // and never crosses packets. Read the current local row, step by the
  // difference, verify, and correct; bounded so a pathological state
  // surfaces as a clear error instead of a silent misposition.
  for Attempts := 1 to FPageSize + 2 do
  begin
    try
      Current := inherited GetRecNo;
    except
      // Cache-end state: one past the loaded rows.
      on E: EDBClient do
        Current := inherited GetRecordCount + 1;
    end;
    if Current = ALocalRec then
      Exit;
    inherited MoveBy(ALocalRec - Current);
  end;
  raise EDatabaseError.CreateFmt(
    'Could not position on local row %d', [ALocalRec]);
end;

procedure TVittixPagedDataSet.SetRecNo(Value: Integer);
var
  TargetPage, LocalRec: Integer;
begin
  if GlobalScrollActive and FPageLoaded and not FLoading then
  begin
    if Value > FTotalCount then
      Value := FTotalCount;
    if Value >= 1 then
    begin
      TargetPage := (Value - 1) div FPageSize;
      LocalRec := Value - TargetPage * FPageSize;
      if TargetPage <> FPageIndex then
        OpenPage(TargetPage); // raises on loader failure and restores the page
      PositionToLocalRow(LocalRec);
    end;
    // Values below 1 name no row and are ignored: the native CDS setter
    // would raise on an empty page instead.
  end
  else
    inherited SetRecNo(Value);
end;

function TVittixPagedDataSet.GetBookmark: TBookmark;
var
  LocalRec, NativeLen: Integer;
  Native: TBookmark;
begin
  // In virtual mode bookmarks are page-scoped wrappers embedding the
  // NATIVE CDS bookmark (record identity). The scope fields exist because
  // a bare native bookmark survives a page replacement and silently
  // repositions to the same local row of whatever page is loaded, so a
  // stale selection would address the wrong rows; validity checks refuse
  // that before the native part is ever used.
  LocalRec := inherited GetRecNo;
  if GlobalScrollActive and FPageLoaded and not FLoading and (LocalRec >= 1) then
  begin
    Native := inherited GetBookmark;
    NativeLen := Length(Native);
    SetLength(Result, VittixPageBookmarkHeader + NativeLen);
    Result[0] := $D9;
    Result[1] := $01;
    Result[2] := $6B;
    Result[3] := $A5;
    EncodeDWord(NativeLen, 4, Result);
    if NativeLen > 0 then
      Move(Native[0], Result[8], NativeLen);
    EncodeDWord(FPageIndex, 8 + NativeLen, Result);
    EncodeDWord(LocalRec, 12 + NativeLen, Result);
    EncodeDWord(FPageGeneration, 16 + NativeLen, Result);
  end
  else
    Result := inherited GetBookmark;
end;

function TVittixPagedDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  Native: TBookmark;
  PageIndex, LocalRec, Generation: Integer;
begin
  if PageBookmarkFields(Bookmark, Native, PageIndex, LocalRec, Generation) then
    // Wrappers are only meaningful under the mode that minted them: a
    // Filtered toggle or VirtualScroll off changes what the stored local
    // row means (a filtered view renumbers the visible rows), so the
    // wrapper must read as stale instead of repositioning onto a
    // different record. Clearing the filter without a reload restores
    // validity because the generation is unchanged.
    Result := GlobalScrollActive and FPageLoaded and
      (PageIndex = FPageIndex) and (LocalRec >= 1) and
      (LocalRec <= inherited GetRecordCount) and
      (Generation = FPageGeneration)
  else
    Result := inherited BookmarkValid(Bookmark);
end;

function TVittixPagedDataSet.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
var
  Native1, Native2: TBookmark;
  Page1, Local1, Generation1, Page2, Local2, Generation2: Integer;
  Wrapper1, Wrapper2: Boolean;
begin
  Wrapper1 := PageBookmarkFields(Bookmark1, Native1, Page1, Local1, Generation1);
  Wrapper2 := PageBookmarkFields(Bookmark2, Native2, Page2, Local2, Generation2);
  if Wrapper1 and Wrapper2 then
  begin
    if Page1 < Page2 then
      Exit(-1);
    if Page1 > Page2 then
      Exit(1);
    if Generation1 < Generation2 then
      Exit(-1);
    if Generation1 > Generation2 then
      Exit(1);
    if Local1 < Local2 then
      Exit(-1);
    if Local1 > Local2 then
      Exit(1);
    Result := 0;
  end
  else if Wrapper1 <> Wrapper2 then
    Result := Ord(Wrapper2) - Ord(Wrapper1) // wrappers sort before natives
  else
    Result := inherited CompareBookmarks(Bookmark1, Bookmark2);
end;

procedure TVittixPagedDataSet.InternalGotoBookmark(Bookmark: TBookmark);
var
  Native: TBookmark;
  PageIndex, LocalRec, Generation: Integer;
begin
  if PageBookmarkFields(Bookmark, Native, PageIndex, LocalRec, Generation) then
  begin
    if not BookmarkValid(Bookmark) then
      raise EDatabaseError.Create(
        'The page-scoped bookmark is no longer valid: its page was ' +
        'replaced, reloaded, or virtual mode is not active');
    // Navigate by the embedded native bookmark (record identity): with a
    // visible grid the cursor is continuously driven by the window
    // management, so position-based seeking lands on whichever row the
    // window service left the cursor on (verified). Record identity is
    // exact in every state; the scope checks above already refused the
    // stale cases where identity alone would silently misposition.
    inherited InternalGotoBookmark(Native);
  end
  else
    inherited InternalGotoBookmark(Bookmark);
end;

end.
