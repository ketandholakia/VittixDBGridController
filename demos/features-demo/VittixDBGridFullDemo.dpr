program VittixDBGridFullDemo;

uses
  Vcl.Forms,
  VittixDBGridForm in 'VittixDBGridForm.pas' {frmVittixDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Vittix DBGrid - Complete Feature Demo';
  Application.CreateForm(TfrmVittixDemo, frmVittixDemo);
  Application.Run;
end.
