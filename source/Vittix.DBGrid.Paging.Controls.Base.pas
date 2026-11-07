unit Vittix.DBGrid.Paging.Controls.Base;

interface

uses
  System.Classes,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TVittixPageAction = (vpaFirst, vpaPrevious, vpaNext, vpaLast);

  /// <summary>Snapshot the bar renders. TotalKnown=False keeps the
  /// probe-based look (no page total, Last disabled) regardless of the
  /// other fields.</summary>
  TVittixPagerState = record
    Connected: Boolean;
    Loaded: Boolean;
    PageIndex: Integer;
    CanNext: Boolean;
    TotalKnown: Boolean;
    PageCount: Integer;
    class function Off: TVittixPagerState; static;
  end;

  /// <summary>Shared VCL controls for explicit page navigation.</summary>
  TVittixPagerBar = class(TPanel)
  private
    FFirstButton: TButton;
    FPreviousButton: TButton;
    FNextButton: TButton;
    FLastButton: TButton;
    FPageLabel: TLabel;
    procedure FirstClick(Sender: TObject);
    procedure PreviousClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
    procedure LastClick(Sender: TObject);
  protected
    procedure Navigate(AAction: TVittixPageAction); virtual; abstract;
    procedure SetPageState(AState: TVittixPagerState);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdatePageState; virtual; abstract;
  end;

implementation

uses
  System.SysUtils;

class function TVittixPagerState.Off: TVittixPagerState;
begin
  Result := Default(TVittixPagerState);
  Result.PageIndex := -1;
end;

constructor TVittixPagerBar.Create(AOwner: TComponent);
begin
  inherited;
  Height := 34;
  BevelOuter := bvNone;

  FFirstButton := TButton.Create(Self);
  FFirstButton.Name := 'FirstButton';
  FFirstButton.Parent := Self;
  FFirstButton.SetBounds(4, 4, 52, 26);
  FFirstButton.Caption := 'First';
  FFirstButton.OnClick := FirstClick;

  FPreviousButton := TButton.Create(Self);
  FPreviousButton.Name := 'PreviousButton';
  FPreviousButton.Parent := Self;
  FPreviousButton.SetBounds(60, 4, 72, 26);
  FPreviousButton.Caption := 'Previous';
  FPreviousButton.OnClick := PreviousClick;

  FNextButton := TButton.Create(Self);
  FNextButton.Name := 'NextButton';
  FNextButton.Parent := Self;
  FNextButton.SetBounds(136, 4, 52, 26);
  FNextButton.Caption := 'Next';
  FNextButton.OnClick := NextClick;

  FLastButton := TButton.Create(Self);
  FLastButton.Name := 'LastButton';
  FLastButton.Parent := Self;
  FLastButton.SetBounds(192, 4, 52, 26);
  FLastButton.Caption := 'Last';
  FLastButton.OnClick := LastClick;

  FPageLabel := TLabel.Create(Self);
  FPageLabel.Name := 'PageLabel';
  FPageLabel.Parent := Self;
  FPageLabel.Left := 256;
  FPageLabel.Top := 10;
  SetPageState(TVittixPagerState.Off);
end;

procedure TVittixPagerBar.SetPageState(AState: TVittixPagerState);
begin
  FFirstButton.Enabled := AState.Connected and AState.Loaded and
    (AState.PageIndex > 0);
  FPreviousButton.Enabled := FFirstButton.Enabled;
  FNextButton.Enabled := AState.Connected and AState.CanNext;
  FLastButton.Enabled := AState.Connected and AState.Loaded and
    AState.TotalKnown and (AState.PageIndex < AState.PageCount - 1);
  if AState.Connected and AState.Loaded then
  begin
    if AState.TotalKnown then
      FPageLabel.Caption := Format('Page %d of %d',
        [AState.PageIndex + 1, AState.PageCount])
    else
      FPageLabel.Caption := Format('Page %d', [AState.PageIndex + 1]);
  end
  else
    FPageLabel.Caption := 'Paging off';
end;

procedure TVittixPagerBar.FirstClick(Sender: TObject);
begin
  Navigate(vpaFirst);
  UpdatePageState;
end;

procedure TVittixPagerBar.PreviousClick(Sender: TObject);
begin
  Navigate(vpaPrevious);
  UpdatePageState;
end;

procedure TVittixPagerBar.NextClick(Sender: TObject);
begin
  Navigate(vpaNext);
  UpdatePageState;
end;

procedure TVittixPagerBar.LastClick(Sender: TObject);
begin
  Navigate(vpaLast);
  UpdatePageState;
end;

end.
