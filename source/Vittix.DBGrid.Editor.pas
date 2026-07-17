unit Vittix.DBGrid.Editor;

interface

uses
  System.Classes,
  DesignEditors,
  DesignIntf,
  Vcl.Dialogs,
  Vittix.DBGrid;

type
  TVittixDBGridEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  Vittix.DBGrid.Controller;

{ TVittixDBGridEditor }

procedure TVittixDBGridEditor.ExecuteVerb(Index: Integer);
var
  Grid: TVittixDBGrid;
begin
  Grid := Component as TVittixDBGrid;
  if not Assigned(Grid) then Exit;

  case Index of
    0: begin
         // Toggle footer visibility - safe at design time (property only)
         Grid.FooterVisible := not Grid.FooterVisible;
         Designer.Modified;
       end;

    1: begin
         if Assigned(Grid.Controller) and (Grid.Controller is TVittixDBGridController) then
           TVittixDBGridController(Grid.Controller).ShowColumnChooser;
         Designer.Modified;
       end;

    2: begin
         // Recalculate: only call Invalidate at design time (no engines exist).
         // At runtime the Controller handles recalculation automatically.
         Grid.Invalidate;
         Designer.Modified;
       end;

    3: begin
         ShowMessage(
           'Vittix DBGrid'#13#10 +
           'Advanced DBGrid with Footer & Aggregation'
         );
       end;
  end;
end;

function TVittixDBGridEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Toggle Footer';
    1: Result := 'Choose Columns...';
    2: Result := 'Recalculate Aggregates';
    3: Result := 'About Vittix DBGrid...';
  else
    Result := '';
  end;
end;

function TVittixDBGridEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

end.
