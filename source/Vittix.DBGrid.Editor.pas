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
         // Recalculate: only call Invalidate at design time (no engines exist).
         // At runtime the Controller handles recalculation automatically.
         Grid.Invalidate;
         Designer.Modified;
       end;

    2: begin
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
    1: Result := 'Recalculate Aggregates';
    2: Result := 'About Vittix DBGrid...';
  else
    Result := '';
  end;
end;

function TVittixDBGridEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

end.