program VittixPagedDemo;

uses
  Vcl.Forms,
  VittixPagedDemoForm in 'VittixPagedDemoForm.pas' {PagedDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Vittix DBGrid - paged dataset demo';
  Application.CreateForm(TPagedDemoForm, PagedDemoForm);
  Application.Run;
end.
