program VittixDBGridTests;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Vcl.Forms,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.VCLUI.Wait,
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.XML.NUnit,
  DUnitX.TestRunner,
  Vittix.Tests.TestData in 'src\Vittix.Tests.TestData.pas',
  Vittix.Tests.ColumnInfo in 'src\Vittix.Tests.ColumnInfo.pas',
  Vittix.Tests.SortEngine in 'src\Vittix.Tests.SortEngine.pas',
  Vittix.Tests.FilterEngine in 'src\Vittix.Tests.FilterEngine.pas',
  Vittix.Tests.FilterOperators in 'src\Vittix.Tests.FilterOperators.pas',
  Vittix.Tests.FilterSets in 'src\Vittix.Tests.FilterSets.pas',
  Vittix.Tests.Editors in 'src\Vittix.Tests.Editors.pas',
  Vittix.Tests.SQLBuilder in 'src\Vittix.Tests.SQLBuilder.pas',
  Vittix.Tests.ProviderFireDAC in 'src\Vittix.Tests.ProviderFireDAC.pas',
  Vittix.Tests.PagedDataSet in 'src\Vittix.Tests.PagedDataSet.pas',
  Vittix.Tests.AggregationEngine in 'src\Vittix.Tests.AggregationEngine.pas',
  Vittix.Tests.ExportEngine in 'src\Vittix.Tests.ExportEngine.pas',
  Vittix.Tests.Styles in 'src\Vittix.Tests.Styles.pas',
  Vittix.Tests.IncrementalSearch in 'src\Vittix.Tests.IncrementalSearch.pas',
  Vittix.Tests.ClipboardCopy in 'src\Vittix.Tests.ClipboardCopy.pas',
  Vittix.Tests.LayoutSchema in 'src\Vittix.Tests.LayoutSchema.pas',
  Vittix.Tests.FreezeColumns in 'src\Vittix.Tests.FreezeColumns.pas',
  Vittix.Tests.Layout in 'src\Vittix.Tests.Layout.pas',
  Vittix.Tests.Controller.Regression in 'src\Vittix.Tests.Controller.Regression.pas';

var
  Runner: ITestRunner;
  Results: IRunResults;
begin
  Application.Initialize;
  TTestDataCrashReporter.Install;
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
