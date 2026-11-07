unit Vittix.Tests.FooterGeometry;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  Data.DB,
  Datasnap.DBClient,
  Vcl.Forms,
  Vcl.DBGrids,
  Vcl.Controls,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.FooterPanel;

type
  /// <summary>Geometry of the footer panel relative to the grid. A real
  /// visible form is used because the horizontal scrollbar and the grid's
  /// client rect are laid out by Windows only once the control is shown.
  ///
  /// Native scrollbars are non-client, so the footer cannot both sit at the
  /// grid's outer bottom edge and leave the scrollbar visible. The reserved
  /// strip is between the data area and the scrollbar: the footer's top is
  /// the client-area bottom and its bottom meets the scrollbar's top.</summary>
  [TestFixture]
  TVittixFooterGeometryTests = class
  private
    FForm: TForm;
    FDataSet: TClientDataSet;
    FSource: TDataSource;
    FGrid: TVittixDBGrid;
    FFooter: TVittixDBGridFooterPanel;
    FFullClientHeight: Integer;
    procedure BuildFixture(AVisible: Boolean; AFooterVisible: Boolean);
    procedure ForceHorizontalScrollbar;
    procedure ForceNoHorizontalScrollbar;
    function FindFooter: TVittixDBGridFooterPanel;
    function DataBottomScreenY: Integer;
    function FooterTopScreenY: Integer;
    function FooterBottomScreenY: Integer;
    function GridWindowRect: TRect;
    function HorizontalScrollBarRect(out AVisible: Boolean): TRect;
    procedure DumpGeometry(const ATag: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure FooterDoesNotCoverLastRow;
    [Test]
    procedure FooterDoesNotCoverHorizontalScrollbar;
    [Test]
    procedure FooterFillsReservedStripWithinGridBounds;
    [Test]
    procedure FooterVisibleFalseRestoresFullClientArea;
    [Test]
    procedure ResizeKeepsFooterBelowTheData;
  end;

implementation

uses
  Vittix.Tests.TestData;

procedure TVittixFooterGeometryTests.BuildFixture(AVisible,
  AFooterVisible: Boolean);
var
  I: Integer;
begin
  FForm := TForm.CreateNew(nil);
  FForm.Position := poDesigned;
  FForm.SetBounds(0, 0, 640, 400);

  FDataSet := CreateSampleDataSet;
  FSource := TDataSource.Create(FForm);
  FSource.DataSet := FDataSet;

  FGrid := TVittixDBGrid.Create(FForm);
  FGrid.Parent := FForm;
  FGrid.Align := alClient;
  FGrid.FooterVisible := AFooterVisible;
  FGrid.DataSource := FSource;

  // Seven 120-pixel columns in a ~640-pixel client area force the native
  // horizontal scrollbar, which is exactly the strip the footer used to
  // cover.
  if FGrid.Columns.Count = 0 then
    for I := 0 to FDataSet.Fields.Count - 1 do
    begin
      FGrid.Columns.Add;
      FGrid.Columns[I].FieldName := FDataSet.Fields[I].FieldName;
    end;
  for I := 0 to FGrid.Columns.Count - 1 do
    FGrid.Columns[I].Width := 120;

  if AVisible then
  begin
    FForm.Show;
    Application.ProcessMessages;
    FGrid.Update;
  end;
  FFooter := FindFooter;
  if Assigned(FFooter) then
    FFullClientHeight := FGrid.ClientHeight + FFooter.Height
  else
    FFullClientHeight := FGrid.ClientHeight;
end;

procedure TVittixFooterGeometryTests.ForceHorizontalScrollbar;
begin
  FForm.ClientWidth := 640;
  Application.ProcessMessages;
  FGrid.Update;
end;

procedure TVittixFooterGeometryTests.ForceNoHorizontalScrollbar;
begin
  FForm.ClientWidth := 1000;
  Application.ProcessMessages;
  FGrid.Update;
end;

function TVittixFooterGeometryTests.FindFooter: TVittixDBGridFooterPanel;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FForm.ControlCount - 1 do
    if FForm.Controls[I] is TVittixDBGridFooterPanel then
      Exit(TVittixDBGridFooterPanel(FForm.Controls[I]));
end;

function TVittixFooterGeometryTests.DataBottomScreenY: Integer;
var
  P: TPoint;
begin
  P := FGrid.ClientToScreen(Point(0, FGrid.ClientHeight));
  Result := P.Y;
end;

function TVittixFooterGeometryTests.FooterTopScreenY: Integer;
var
  P: TPoint;
begin
  P := FForm.ClientToScreen(Point(FFooter.Left, FFooter.Top));
  Result := P.Y;
end;

function TVittixFooterGeometryTests.FooterBottomScreenY: Integer;
begin
  Result := FooterTopScreenY + FFooter.Height;
end;

function TVittixFooterGeometryTests.GridWindowRect: TRect;
begin
  Winapi.Windows.GetWindowRect(FGrid.Handle, Result);
end;

function TVittixFooterGeometryTests.HorizontalScrollBarRect(
  out AVisible: Boolean): TRect;
var
  SBI: TScrollBarInfo;
begin
  ZeroMemory(@SBI, SizeOf(SBI));
  SBI.cbSize := SizeOf(SBI);
  Result := Rect(0, 0, 0, 0);
  AVisible := False;
  if not FGrid.HandleAllocated then Exit;
  if not GetScrollBarInfo(FGrid.Handle, OBJID_HSCROLL, SBI) then Exit;
  AVisible := (SBI.rgstate[0] and STATE_SYSTEM_INVISIBLE) = 0;
  if AVisible then
    Result := SBI.rcScrollBar;
end;

procedure TVittixFooterGeometryTests.DumpGeometry(const ATag: string);
var
  ScrollRect: TRect;
  GridRect: TRect;
  ClientRect: TRect;
  ScrollClient: TRect;
  ScrollVisible: Boolean;
begin
  ScrollRect := HorizontalScrollBarRect(ScrollVisible);
  GridRect := GridWindowRect;
  GetClientRect(FGrid.Handle, ClientRect);
  ScrollClient := ScrollRect;
  MapWindowPoints(HWND_DESKTOP, FGrid.Handle, ScrollClient, 2);
  Writeln(Format('%s gridWin=[%d..%d] clientRect=%dx%d clientOriginY=%d ' +
    'footerScreen=[%d..%d] dataBottom=%d scrollVisible=%s ' +
    'scrollScreen=[%d..%d] scrollClient=[%d..%d]',
    [ATag, GridRect.Top, GridRect.Bottom,
     ClientRect.Right, ClientRect.Bottom, FGrid.ClientToScreen(Point(0, 0)).Y,
     FooterTopScreenY, FooterBottomScreenY, DataBottomScreenY,
     BoolToStr(ScrollVisible, True),
     ScrollRect.Top, ScrollRect.Bottom,
     ScrollClient.Top, ScrollClient.Bottom]));
end;

procedure TVittixFooterGeometryTests.Setup;
begin
  FForm := nil;
  FFooter := nil;
  FGrid := nil;
  // Setup intentionally does nothing: each test builds the shape it needs.
end;

procedure TVittixFooterGeometryTests.TearDown;
begin
  FForm.Free;
  FDataSet.Free;
  FForm := nil;
  FFooter := nil;
  FGrid := nil;
end;

procedure TVittixFooterGeometryTests.FooterDoesNotCoverLastRow;
begin
  BuildFixture(True, True);
  Assert.IsNotNull(FFooter, 'the footer panel was created');
  ForceHorizontalScrollbar;
  DumpGeometry('last-row');

  // The data client area ends at DataBottomScreenY. The footer must start
  // at or below that line, so the last row is never painted under it.
  Assert.IsTrue(FooterTopScreenY >= DataBottomScreenY,
    Format('footer top %d must be >= data bottom %d',
      [FooterTopScreenY, DataBottomScreenY]));
end;

procedure TVittixFooterGeometryTests.FooterDoesNotCoverHorizontalScrollbar;
var
  ScrollRect: TRect;
  ScrollVisible: Boolean;
begin
  BuildFixture(True, True);
  Assert.IsNotNull(FFooter);
  ForceHorizontalScrollbar;
  ScrollRect := HorizontalScrollBarRect(ScrollVisible);
  DumpGeometry('scrollbar');

  Assert.IsTrue(ScrollVisible, 'the fixture shows a horizontal scrollbar');
  // The shrunken client rect moves the native scrollbar up; the footer
  // fills the freed strip below it.
  Assert.IsTrue(FooterTopScreenY >= ScrollRect.Bottom,
    Format('footer top %d must be at/below the scrollbar bottom %d',
      [FooterTopScreenY, ScrollRect.Bottom]));
end;

procedure TVittixFooterGeometryTests.FooterFillsReservedStripWithinGridBounds;
var
  GridRect: TRect;
begin
  BuildFixture(True, True);
  Assert.IsNotNull(FFooter);
  ForceHorizontalScrollbar;
  GridRect := GridWindowRect;
  DumpGeometry('strip');

  // The footer fills the reserved bottom strip and stays inside the grid's
  // outer bounds (the 2px bottom border remains visible below it).
  Assert.IsTrue(FooterBottomScreenY <= GridRect.Bottom,
    Format('footer bottom %d stays inside the grid window %d',
      [FooterBottomScreenY, GridRect.Bottom]));
  Assert.IsTrue(FooterBottomScreenY >= GridRect.Bottom - 6,
    Format('footer bottom %d reaches the grid bottom %d',
      [FooterBottomScreenY, GridRect.Bottom]));
  Assert.AreEqual(FGrid.Left, FFooter.Left, 'footer left aligns with grid');
  Assert.AreEqual(FGrid.Width, FFooter.Width, 'footer spans the grid width');
  Assert.AreEqual<Integer>(FFullClientHeight, FGrid.ClientHeight + FFooter.Height,
    'data area plus footer equals the unreserved client height');
end;

procedure TVittixFooterGeometryTests.FooterVisibleFalseRestoresFullClientArea;
var
  HiddenClientHeight: Integer;
begin
  BuildFixture(True, True);
  Assert.IsNotNull(FFooter);
  ForceHorizontalScrollbar;

  FGrid.FooterVisible := False;
  Application.ProcessMessages;
  FGrid.Update;

  // FooterVisible=False frees the panel; the grid must get the strip back.
  FFooter := FindFooter;
  Assert.IsNull(FFooter, 'the footer panel is gone');
  HiddenClientHeight := FGrid.ClientHeight;
  Writeln(Format('hidden clientHeight=%d (was %d)',
    [HiddenClientHeight, FFullClientHeight]));
  Assert.AreEqual<Integer>(FFullClientHeight, HiddenClientHeight,
    'FooterVisible=False restores the full client area');
end;

procedure TVittixFooterGeometryTests.ResizeKeepsFooterBelowTheData;
begin
  BuildFixture(True, True);
  Assert.IsNotNull(FFooter);
  ForceHorizontalScrollbar;

  FForm.ClientWidth := 900;
  FForm.ClientHeight := 500;
  Application.ProcessMessages;
  FGrid.Update;
  DumpGeometry('resized');

  Assert.IsTrue(FooterTopScreenY >= DataBottomScreenY,
    'after resize the footer is still below the data line');

  // Narrowing the form again brings the horizontal scrollbar back; the
  // footer must still stop above it.
  ForceHorizontalScrollbar;
  Assert.IsTrue(FooterTopScreenY >= DataBottomScreenY);
end;

end.
