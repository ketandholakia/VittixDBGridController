program VittixPagedDemoSelfTest;

{$APPTYPE CONSOLE}

uses
  Vcl.Forms,
  System.SysUtils,
  System.Classes,
  VittixPagedDemoForm in 'VittixPagedDemoForm.pas' {PagedDemoForm};

begin
  // Console companion to the GUI demo: runs the same verification as the
  // interactive demo against a real, visible form. Console subsystem is
  // used because unattended GUI message loops and console allocation are
  // unreliable under automation; the DUnitX suite uses the same model.
  try
    Application.Initialize;
    Application.CreateForm(TPagedDemoForm, PagedDemoForm);
    ExitCode := PagedDemoForm.RunSelfTest;
    // Explicit teardown: without a Run loop the shutdown sequence of an
    // unattended console host otherwise trips an access violation in
    // component finalization.
    Application.ProcessMessages;
    PagedDemoForm.Release;
    Application.ProcessMessages;
  except
    on E: Exception do
    begin
      Writeln('Unhandled: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 99;
    end;
  end;
end.
