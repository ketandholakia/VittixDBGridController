program VittixDBGridTests;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Vcl.Forms,
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.XML.NUnit,
  DUnitX.TestRunner,
  Vittix.Tests.TestData in 'src\Vittix.Tests.TestData.pas',
  Vittix.Tests.ColumnInfo in 'src\Vittix.Tests.ColumnInfo.pas',
  Vittix.Tests.SortEngine in 'src\Vittix.Tests.SortEngine.pas',
  Vittix.Tests.FilterEngine in 'src\Vittix.Tests.FilterEngine.pas',
  Vittix.Tests.AggregationEngine in 'src\Vittix.Tests.AggregationEngine.pas',
  Vittix.Tests.ExportEngine in 'src\Vittix.Tests.ExportEngine.pas',
  Vittix.Tests.Layout in 'src\Vittix.Tests.Layout.pas',
  Vittix.Tests.Controller.Regression in 'src\Vittix.Tests.Controller.Regression.pas';

var
  Runner: ITestRunner;
  Results: IRunResults;
begin
  Application.Initialize;
  Application.ShowMainForm := False;

  try
    TDUnitX.CheckCommandLine;

    Runner := TDUnitX.CreateRunner;
    Runner.UseRTTI := True;
    Runner.FailsOnNoAsserts := True;

    if TDUnitX.Options.ConsoleMode <> TDUnitXConsoleMode.Off then
      Runner.AddLogger(
        TDUnitXConsoleLogger.Create(
          TDUnitX.Options.ConsoleMode = TDUnitXConsoleMode.Quiet
        )
      );

    Runner.AddLogger(TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile));

    Results := Runner.Execute;
    if not Results.AllPassed then
      ExitCode := 1;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
