unit Vittix.DBGrid.Reg;

interface

procedure Register;

implementation

uses
  System.Classes,
  DesignIntf,        // REQUIRED for RegisterComponentEditor
  DesignEditors,
  Vittix.DBGrid,
  Vittix.DBGrid.Editor;

procedure Register;
begin
  RegisterComponents('Vittix', [TVittixDBGrid]);

  RegisterComponentEditor(
    TVittixDBGrid,
    TVittixDBGridEditor
  );
end;

end.

