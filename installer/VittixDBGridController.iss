#define MyAppName "Vittix DBGrid Controller"
#ifndef MyAppVersion
#define MyAppVersion "2.1.0"
#endif
#define MyAppPublisher "Vittix"
#define MyAppURL "https://github.com/ketandholakia/VittixDBGridController"
#define MyAppExeName "VittixDBGridController"
#define DesignPackageName "VittixDBGridControllerD.bpl"
#define RuntimePackageName "VittixDBGridControllerR.bpl"
#define DesignDcpName "VittixDBGridControllerD.dcp"
#define RuntimeDcpName "VittixDBGridControllerR.dcp"
#define FDPackageName "VittixDBGridControllerFD.bpl"
#define FDDcpName "VittixDBGridControllerFD.dcp"

// ---- Supported IDE versions (add a new block here + a matching /d flag) ----
#ifndef HaveDelphi10_3
#define HaveDelphi10_3 0
#endif
#ifndef HaveDelphi12
#define HaveDelphi12 0
#endif

#define PayloadRoot10_3 "payload\\Delphi10_3\\Win32"
#define PayloadRoot12 "payload\\Delphi12Athens\\Win32"
#define PayloadBplDir10_3 PayloadRoot10_3 + "\\Bpl"
#define PayloadDcpDir10_3 PayloadRoot10_3 + "\\Dcp"
#define PayloadBplDir12 PayloadRoot12 + "\\Bpl"
#define PayloadDcpDir12 PayloadRoot12 + "\\Dcp"

[Setup]
AppId={{4D7A49A5-6C2C-442E-95F0-53AE65D5C7C1}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\Vittix\DBGridController
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
LicenseFile=..\LICENSE
InfoBeforeFile=README.md
OutputDir=output
OutputBaseFilename=VittixDBGridControllerSetup
Compression=lzma
SolidCompression=yes
WizardStyle=modern
PrivilegesRequired=admin
UninstallDisplayIcon={app}\docs\ss1.png

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Types]
Name: "full"; Description: "Full installation"
Name: "source"; Description: "Source only"
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "core"; Description: "Source code and Delphi packages"; Types: full source custom; Flags: fixed
Name: "docs"; Description: "Documentation and screenshots"; Types: full custom
Name: "demo"; Description: "Feature demo project"; Types: full custom
#ifdef HaveDelphi10_3
Name: "delphi10_3"; Description: "RAD Studio 10.3 Rio integration files"; Types: full custom
#endif
#ifdef HaveDelphi12
Name: "delphi12"; Description: "RAD Studio 12 Athens integration files"; Types: full custom
#endif

[Tasks]
#ifdef HaveDelphi10_3
Name: "addlib_delphi10_3"; Description: "Add {app}\source to RAD Studio 10.3 Rio Win32 library path"; Components: delphi10_3; Flags: checkedonce
Name: "regpkg_delphi10_3"; Description: "Register design-time package in RAD Studio 10.3 Rio"; Components: delphi10_3; Flags: checkedonce
#endif
#ifdef HaveDelphi12
Name: "addlib_delphi12"; Description: "Add {app}\source to RAD Studio 12 Athens Win32 library path"; Components: delphi12; Flags: checkedonce
Name: "regpkg_delphi12"; Description: "Register design-time package in RAD Studio 12 Athens"; Components: delphi12; Flags: checkedonce
#endif

[Dirs]
Name: "{app}\source"; Components: core
Name: "{app}\packages"; Components: core
Name: "{app}\packages\D10_3"; Components: core
Name: "{app}\assets\palette"; Components: core
Name: "{app}\docs"; Components: docs
Name: "{app}\demos\features-demo"; Components: demo
#ifdef HaveDelphi10_3
Name: "{app}\payload\Delphi10_3\Win32\Bpl"; Components: delphi10_3
Name: "{app}\payload\Delphi10_3\Win32\Dcp"; Components: delphi10_3
#endif
#ifdef HaveDelphi12
Name: "{app}\payload\Delphi12Athens\Win32\Bpl"; Components: delphi12
Name: "{app}\payload\Delphi12Athens\Win32\Dcp"; Components: delphi12
#endif

[Files]
Source: "..\source\*"; DestDir: "{app}\source"; Excludes: "*.dcu,*.bak,*.~*,__history\*"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: core
Source: "..\packages\*"; DestDir: "{app}\packages"; Excludes: "*.bpl,*.dcp,*.dcu,*.local,*.identcache,build\*,__history\*"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: core
Source: "..\packages\D10_3\*"; DestDir: "{app}\packages\D10_3"; Excludes: "*.bpl,*.dcp,*.dcu,*.local,*.identcache,build\*,__history\*"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: core
Source: "..\assets\palette\*.dcr"; DestDir: "{app}\assets\palette"; Flags: ignoreversion; Components: core
Source: "..\README.md"; DestDir: "{app}"; Flags: ignoreversion; Components: core
Source: "..\LICENSE"; DestDir: "{app}"; Flags: ignoreversion; Components: core

Source: "..\docs\*"; DestDir: "{app}\docs"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: docs

Source: "..\demos\features-demo\*"; DestDir: "{app}\demos\features-demo"; Excludes: "*.dcu,*.drc,*.dres,*.exe,*.identcache,*.map,*.mes,*.local,build\*,features-demo-state\*"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: demo

// ---- Payload: Delphi 10.3 Rio (only if built) ----
#ifdef HaveDelphi10_3
#ifexist PayloadBplDir10_3 + "\\" + RuntimePackageName
Source: "{#PayloadBplDir10_3}\{#RuntimePackageName}"; DestDir: "{app}\payload\Delphi10_3\Win32\Bpl"; Flags: ignoreversion; Components: delphi10_3
#endif
#ifexist PayloadBplDir10_3 + "\\" + FDPackageName
Source: "{#PayloadBplDir10_3}\{#FDPackageName}"; DestDir: "{app}\payload\Delphi10_3\Win32\Bpl"; Flags: ignoreversion; Components: delphi10_3
#endif
#ifexist PayloadBplDir10_3 + "\\" + DesignPackageName
Source: "{#PayloadBplDir10_3}\{#DesignPackageName}"; DestDir: "{app}\payload\Delphi10_3\Win32\Bpl"; Flags: ignoreversion; Components: delphi10_3
#endif
#ifexist PayloadDcpDir10_3 + "\\" + RuntimeDcpName
Source: "{#PayloadDcpDir10_3}\{#RuntimeDcpName}"; DestDir: "{app}\payload\Delphi10_3\Win32\Dcp"; Flags: ignoreversion; Components: delphi10_3
#endif
#ifexist PayloadDcpDir10_3 + "\\" + FDDcpName
Source: "{#PayloadDcpDir10_3}\{#FDDcpName}"; DestDir: "{app}\payload\Delphi10_3\Win32\Dcp"; Flags: ignoreversion; Components: delphi10_3
#endif
#ifexist PayloadDcpDir10_3 + "\\" + DesignDcpName
Source: "{#PayloadDcpDir10_3}\{#DesignDcpName}"; DestDir: "{app}\payload\Delphi10_3\Win32\Dcp"; Flags: ignoreversion; Components: delphi10_3
#endif
#endif

// ---- Payload: Delphi 12 Athens (only if built) ----
#ifdef HaveDelphi12
#ifexist PayloadBplDir12 + "\\" + RuntimePackageName
Source: "{#PayloadBplDir12}\{#RuntimePackageName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Bpl"; Flags: ignoreversion; Components: delphi12
#endif
#ifexist PayloadBplDir12 + "\\" + FDPackageName
Source: "{#PayloadBplDir12}\{#FDPackageName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Bpl"; Flags: ignoreversion; Components: delphi12
#endif
#ifexist PayloadBplDir12 + "\\" + DesignPackageName
Source: "{#PayloadBplDir12}\{#DesignPackageName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Bpl"; Flags: ignoreversion; Components: delphi12
#endif
#ifexist PayloadDcpDir12 + "\\" + RuntimeDcpName
Source: "{#PayloadDcpDir12}\{#RuntimeDcpName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Dcp"; Flags: ignoreversion; Components: delphi12
#endif
#ifexist PayloadDcpDir12 + "\\" + FDDcpName
Source: "{#PayloadDcpDir12}\{#FDDcpName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Dcp"; Flags: ignoreversion; Components: delphi12
#endif
#ifexist PayloadDcpDir12 + "\\" + DesignDcpName
Source: "{#PayloadDcpDir12}\{#DesignDcpName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Dcp"; Flags: ignoreversion; Components: delphi12
#endif
#endif

Source: "README.md"; DestDir: "{tmp}"; Flags: deleteafterinstall dontcopy

[Code]
const
  IDE_COUNT = 2;

type
  TIDEInfo = record
    Available: Boolean;
    Detected: Boolean;
    CompName: string;
    TaskLib: string;
    TaskReg: string;
    BDS: string;
    DisplayName: string;
    PayloadFolder: string;
    CompIndex: Integer;
  end;

var
  IDEs: array[0..1] of TIDEInfo;

function FindWindow(lpClassName, lpWindowName: string): HWND;
  external 'FindWindowA@user32.dll stdcall';

function RemoveTrailingBackslash(const Value: string): string;
begin
  Result := Value;
  while (Length(Result) > 0) and (Result[Length(Result)] = '\') do
    Delete(Result, Length(Result), 1);
end;

function NormalizePath(const Value: string): string;
begin
  Result := Lowercase(RemoveTrailingBackslash(ExpandConstant(Value)));
end;

function FindCharFrom(const Value: string; Ch: Char; StartPos: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := StartPos to Length(Value) do
    if Value[I] = Ch then
    begin
      Result := I;
      Exit;
    end;
end;

function NextPathToken(const PathList: string; var StartPos: Integer): string;
var
  DelimPos: Integer;
begin
  if StartPos > Length(PathList) then
  begin
    Result := '';
    Exit;
  end;

  DelimPos := FindCharFrom(PathList, ';', StartPos);
  if DelimPos = 0 then
  begin
    Result := Copy(PathList, StartPos, MaxInt);
    StartPos := Length(PathList) + 1;
  end
  else
  begin
    Result := Copy(PathList, StartPos, DelimPos - StartPos);
    StartPos := DelimPos + 1;
  end;
end;

function ContainsPath(const PathList, TargetPath: string): Boolean;
var
  Item: string;
  StartPos: Integer;
begin
  Result := False;
  if Trim(PathList) = '' then
    Exit;

  StartPos := 1;
  while StartPos <= Length(PathList) do
  begin
    Item := NextPathToken(PathList, StartPos);
    if NormalizePath(Item) = NormalizePath(TargetPath) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function AppendPath(const PathList, TargetPath: string): string;
begin
  if Trim(PathList) = '' then
    Result := TargetPath
  else if ContainsPath(PathList, TargetPath) then
    Result := PathList
  else
    Result := PathList + ';' + TargetPath;
end;

function RemovePath(const PathList, TargetPath: string): string;
var
  Item: string;
  StartPos: Integer;
begin
  Result := '';
  StartPos := 1;
  while StartPos <= Length(PathList) do
  begin
    Item := NextPathToken(PathList, StartPos);
    if Trim(Item) = '' then
      Continue;
    if NormalizePath(Item) = NormalizePath(TargetPath) then
      Continue;

    if Result = '' then
      Result := Item
    else
      Result := Result + ';' + Item;
  end;
end;

function GetDelphiRootDir(const BDS: string): string;
begin
  Result := '';
  if not RegQueryStringValue(HKCU, 'Software\Embarcadero\BDS\' + BDS, 'RootDir', Result) then
    RegQueryStringValue(HKLM, 'Software\Embarcadero\BDS\' + BDS, 'RootDir', Result);
end;

function IDEPayloadBplPath(const IDE: TIDEInfo): string;
begin
  Result := ExpandConstant('{app}\payload\' + IDE.PayloadFolder + '\Win32\Bpl\{#DesignPackageName}');
end;

function IDEPayloadRuntimePath(const IDE: TIDEInfo): string;
begin
  Result := ExpandConstant('{app}\payload\' + IDE.PayloadFolder + '\Win32\Bpl\{#RuntimePackageName}');
end;

function HasCompiledPackages(const IDE: TIDEInfo): Boolean;
begin
  Result :=
    FileExists(IDEPayloadBplPath(IDE)) and
    FileExists(IDEPayloadRuntimePath(IDE));
end;

procedure AddDelphiLibraryPath(const IDE: TIDEInfo);
var
  ExistingValue: string;
  NewValue: string;
  SourcePath: string;
  LibraryKey: string;
begin
  SourcePath := ExpandConstant('{app}\source');
  LibraryKey := 'Software\Embarcadero\BDS\' + IDE.BDS + '\Library\Win32';
  ExistingValue := '';
  RegQueryStringValue(HKCU, LibraryKey, 'Search Path', ExistingValue);
  NewValue := AppendPath(ExistingValue, SourcePath);
  if NewValue <> ExistingValue then
    RegWriteStringValue(HKCU, LibraryKey, 'Search Path', NewValue);
end;

procedure RemoveDelphiLibraryPath(const IDE: TIDEInfo);
var
  ExistingValue: string;
  NewValue: string;
  SourcePath: string;
  LibraryKey: string;
begin
  SourcePath := ExpandConstant('{app}\source');
  LibraryKey := 'Software\Embarcadero\BDS\' + IDE.BDS + '\Library\Win32';
  ExistingValue := '';
  if not RegQueryStringValue(HKCU, LibraryKey, 'Search Path', ExistingValue) then
    Exit;

  NewValue := RemovePath(ExistingValue, SourcePath);
  if NewValue <> ExistingValue then
    RegWriteStringValue(HKCU, LibraryKey, 'Search Path', NewValue);
end;

procedure RegisterDesignPackage(const IDE: TIDEInfo);
var
  KnownKey: string;
  BplPath: string;
begin
  if not HasCompiledPackages(IDE) then
    Exit;

  KnownKey := 'Software\Embarcadero\BDS\' + IDE.BDS + '\Known Packages';
  BplPath := IDEPayloadBplPath(IDE);
  RegWriteStringValue(HKCU, KnownKey, BplPath, '{#MyAppName} Design-Time Package');
end;

procedure UnregisterDesignPackage(const IDE: TIDEInfo);
var
  KnownKey: string;
  BplPath: string;
begin
  KnownKey := 'Software\Embarcadero\BDS\' + IDE.BDS + '\Known Packages';
  BplPath := IDEPayloadBplPath(IDE);
  RegDeleteValue(HKCU, KnownKey, BplPath);
end;

procedure InitIDEs;
begin
  // Delphi 10.3 Rio
{ HaveDelphi10_3}
  IDEs[0].Available := True;
{}
  IDEs[0].Available := False;
{}
  IDEs[0].BDS := '20.0';
  IDEs[0].DisplayName := 'RAD Studio 10.3 Rio';
  IDEs[0].CompName := 'delphi10_3';
  IDEs[0].TaskLib := 'addlib_delphi10_3';
  IDEs[0].TaskReg := 'regpkg_delphi10_3';
  IDEs[0].PayloadFolder := 'Delphi10_3';
  IDEs[0].CompIndex := -1;

  // Delphi 12 Athens
{ HaveDelphi12}
  IDEs[1].Available := True;
{}
  IDEs[1].Available := False;
{}
  IDEs[1].BDS := '23.0';
  IDEs[1].DisplayName := 'RAD Studio 12 Athens';
  IDEs[1].CompName := 'delphi12';
  IDEs[1].TaskLib := 'addlib_delphi12';
  IDEs[1].TaskReg := 'regpkg_delphi12';
  IDEs[1].PayloadFolder := 'Delphi12Athens';
  IDEs[1].CompIndex := -1;
end;
procedure DetectIDEs;
var
  I: Integer;
begin
  for I := 0 to IDE_COUNT - 1 do
  begin
    if not IDEs[I].Available then
    begin
      IDEs[I].Detected := False;
      Continue;
    end;
    IDEs[I].Detected := (GetDelphiRootDir(IDEs[I].BDS) <> '');
  end;
end;

procedure InitializeWizard;
var
  I, J: Integer;
begin
  InitIDEs;
  DetectIDEs;

  // Mark component checked state + "not detected" suffix.
  for I := 0 to WizardForm.ComponentsList.Items.Count - 1 do
  begin
    for J := 0 to IDE_COUNT - 1 do
    begin
      if not IDEs[J].Available then
        Continue;
      if Pos(IDEs[J].DisplayName, WizardForm.ComponentsList.Items[I]) > 0 then
      begin
        IDEs[J].CompIndex := I;
        if not IDEs[J].Detected then
          WizardForm.ComponentsList.Items[I] := WizardForm.ComponentsList.Items[I] + ' (not detected)';
      end;
    end;
  end;
end;

procedure CurPageChanged(CurPageID: Integer);
var
  I: Integer;
begin
  if CurPageID <> wpSelectComponents then
    Exit;

  for I := 0 to IDE_COUNT - 1 do
  begin
    if (IDEs[I].CompIndex < 0) or (not IDEs[I].Available) then
      Continue;
    WizardForm.ComponentsList.Checked[IDEs[I].CompIndex] := IDEs[I].Detected;
  end;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;

  if (CurPageID = wpSelectTasks) then
  begin
    for I := 0 to IDE_COUNT - 1 do
    begin
      if not IDEs[I].Available then
        Continue;
      if WizardIsTaskSelected(IDEs[I].TaskReg) and not HasCompiledPackages(IDEs[I]) then
      begin
        SuppressibleMsgBox(
          'The installer payload does not contain compiled package files for ' +
          IDEs[I].DisplayName + ' (' + '{#DesignPackageName}' + ' and ' +
          '{#RuntimePackageName}' + ').' + #13#10#13#10 +
          'Source files will still be installed, but design-time registration ' +
          'will be skipped.' + #13#10#13#10 +
          'To enable IDE package registration, build the packages and place the ' +
          'BPL/DCP files under installer\payload before compiling this setup.',
          mbInformation, MB_OK, IDOK);
      end;
    end;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  I: Integer;
  RunningIDE: Boolean;
begin
  if CurStep <> ssPostInstall then
    Exit;

  InitIDEs;

  // Warn once if any selected IDE is running.
  RunningIDE := (FindWindow('TAppBuilder', '') <> 0);
  if RunningIDE then
  begin
    for I := 0 to IDE_COUNT - 1 do
    begin
      if not IDEs[I].Available then
        Continue;
      if (IDEs[I].CompIndex >= 0) and WizardForm.ComponentsList.Checked[IDEs[I].CompIndex] and
         (WizardIsTaskSelected(IDEs[I].TaskLib) or WizardIsTaskSelected(IDEs[I].TaskReg)) then
      begin
        SuppressibleMsgBox(
          'One or more selected Delphi IDEs appear to be running. Please close ' +
          'them and restart after installation so the new packages and library ' +
          'paths are picked up.',
          mbInformation, MB_OK, IDOK);
        Break;
      end;
    end;
  end;

  for I := 0 to IDE_COUNT - 1 do
  begin
    if not IDEs[I].Available then
      Continue;
    if (IDEs[I].CompIndex < 0) or (not WizardForm.ComponentsList.Checked[IDEs[I].CompIndex]) then
      Continue;

    if WizardIsTaskSelected(IDEs[I].TaskLib) then
      AddDelphiLibraryPath(IDEs[I]);

    if WizardIsTaskSelected(IDEs[I].TaskReg) then
      RegisterDesignPackage(IDEs[I]);
  end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  I: Integer;
begin
  if CurUninstallStep <> usUninstall then
    Exit;

  InitIDEs;

  for I := 0 to IDE_COUNT - 1 do
  begin
    // Only attempt removal for versions that were actually available in this build.
    if not IDEs[I].Available then
      Continue;
    RemoveDelphiLibraryPath(IDEs[I]);
    UnregisterDesignPackage(IDEs[I]);
  end;
end;
