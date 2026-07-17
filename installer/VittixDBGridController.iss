#define MyAppName "Vittix DBGrid Controller"
#define MyAppVersion "1.0.0"
#define MyAppPublisher "Vittix"
#define MyAppURL "https://github.com/ketan/VittixDBGridController"
#define MyAppExeName "VittixDBGridController"

#define DelphiVersion "23.0"
#define DelphiDisplayName "RAD Studio 12 Athens"
#define DesignPackageName "VittixDBGridControllerD.bpl"
#define RuntimePackageName "VittixDBGridControllerR.bpl"
#define DesignDcpName "VittixDBGridControllerD.dcp"
#define RuntimeDcpName "VittixDBGridControllerR.dcp"

#define PayloadRoot "payload\\Delphi12Athens\\Win32"
#define PayloadBplDir PayloadRoot + "\\Bpl"
#define PayloadDcpDir PayloadRoot + "\\Dcp"

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
Name: "delphi12"; Description: "{#DelphiDisplayName} integration files"; Types: full custom

[Tasks]
Name: "addlibrarypath"; Description: "Add {app}\source to Delphi 12 Win32 library path"; Components: delphi12; Types: full custom; Flags: checkedonce
Name: "registerdesignpkg"; Description: "Register design-time package in Delphi 12"; Components: delphi12; Types: full custom; Flags: checkedonce

[Dirs]
Name: "{app}\source"; Components: core
Name: "{app}\packages"; Components: core
Name: "{app}\docs"; Components: docs
Name: "{app}\demos\features-demo"; Components: demo
Name: "{app}\payload\Delphi12Athens\Win32\Bpl"; Components: delphi12
Name: "{app}\payload\Delphi12Athens\Win32\Dcp"; Components: delphi12

[Files]
Source: "..\source\*"; DestDir: "{app}\source"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: core
Source: "..\packages\*"; DestDir: "{app}\packages"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: core
Source: "..\README.md"; DestDir: "{app}"; Flags: ignoreversion; Components: core
Source: "..\LICENSE"; DestDir: "{app}"; Flags: ignoreversion; Components: core

Source: "..\docs\*"; DestDir: "{app}\docs"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: docs

Source: "..\demos\features-demo\*"; DestDir: "{app}\demos\features-demo"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: demo

#ifexist PayloadBplDir + "\\" + DesignPackageName
Source: "{#PayloadBplDir}\{#DesignPackageName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Bpl"; Flags: ignoreversion; Components: delphi12
#endif
#ifexist PayloadBplDir + "\\" + RuntimePackageName
Source: "{#PayloadBplDir}\{#RuntimePackageName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Bpl"; Flags: ignoreversion; Components: delphi12
#endif
#ifexist PayloadDcpDir + "\\" + DesignDcpName
Source: "{#PayloadDcpDir}\{#DesignDcpName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Dcp"; Flags: ignoreversion; Components: delphi12
#endif
#ifexist PayloadDcpDir + "\\" + RuntimeDcpName
Source: "{#PayloadDcpDir}\{#RuntimeDcpName}"; DestDir: "{app}\payload\Delphi12Athens\Win32\Dcp"; Flags: ignoreversion; Components: delphi12
#endif

Source: "README.md"; DestDir: "{tmp}"; Flags: deleteafterinstall dontcopy

[Code]
const
  DelphiRootKey = 'Software\Embarcadero\BDS\{#DelphiVersion}';
  DelphiKnownPackagesKey = DelphiRootKey + '\Known Packages';
  DelphiLibraryKey = DelphiRootKey + '\Library\Win32';
  DelphiLibraryValueName = 'Search Path';

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

function ContainsPath(const PathList, TargetPath: string): Boolean;
var
  Parts: TArrayOfString;
  I: Integer;
begin
  Result := False;
  if Trim(PathList) = '' then
    Exit;

  Parts := SplitString(PathList, ';');
  for I := 0 to GetArrayLength(Parts) - 1 do
    if NormalizePath(Parts[I]) = NormalizePath(TargetPath) then
    begin
      Result := True;
      Exit;
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
  Parts: TArrayOfString;
  I: Integer;
begin
  Result := '';
  Parts := SplitString(PathList, ';');
  for I := 0 to GetArrayLength(Parts) - 1 do
  begin
    if Trim(Parts[I]) = '' then
      Continue;
    if NormalizePath(Parts[I]) = NormalizePath(TargetPath) then
      Continue;

    if Result = '' then
      Result := Parts[I]
    else
      Result := Result + ';' + Parts[I];
  end;
end;

function GetDelphiRootDir(): string;
begin
  Result := '';
  if not RegQueryStringValue(HKCU, DelphiRootKey, 'RootDir', Result) then
    RegQueryStringValue(HKLM, DelphiRootKey, 'RootDir', Result);
end;

function GetDesignPackagePath(): string;
begin
  Result := ExpandConstant('{app}\payload\Delphi12Athens\Win32\Bpl\{#DesignPackageName}');
end;

function GetRuntimePackagePath(): string;
begin
  Result := ExpandConstant('{app}\payload\Delphi12Athens\Win32\Bpl\{#RuntimePackageName}');
end;

function HasCompiledPackages(): Boolean;
begin
  Result :=
    FileExists(GetDesignPackagePath()) and
    FileExists(GetRuntimePackagePath());
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;

  if (CurPageID = wpSelectTasks) and WizardIsTaskSelected('registerdesignpkg') and not HasCompiledPackages() then
  begin
    SuppressibleMsgBox(
      'The installer payload does not contain compiled Delphi 12 package files (' +
      '{#DesignPackageName} and {#RuntimePackageName}).' + #13#10#13#10 +
      'Source files will still be installed, but design-time registration will be skipped.' + #13#10#13#10 +
      'To enable IDE package registration, place the compiled BPL/DCP files under:' + #13#10 +
      'installer\{#PayloadRoot}\Bpl and installer\{#PayloadRoot}\Dcp before compiling this setup.',
      mbInformation, MB_OK, IDOK);
  end;
end;

procedure AddDelphiLibraryPath();
var
  ExistingValue: string;
  NewValue: string;
  SourcePath: string;
begin
  SourcePath := ExpandConstant('{app}\source');
  ExistingValue := '';
  RegQueryStringValue(HKCU, DelphiLibraryKey, DelphiLibraryValueName, ExistingValue);
  NewValue := AppendPath(ExistingValue, SourcePath);
  if NewValue <> ExistingValue then
    RegWriteStringValue(HKCU, DelphiLibraryKey, DelphiLibraryValueName, NewValue);
end;

procedure RemoveDelphiLibraryPath();
var
  ExistingValue: string;
  NewValue: string;
  SourcePath: string;
begin
  SourcePath := ExpandConstant('{app}\source');
  ExistingValue := '';
  if not RegQueryStringValue(HKCU, DelphiLibraryKey, DelphiLibraryValueName, ExistingValue) then
    Exit;

  NewValue := RemovePath(ExistingValue, SourcePath);
  if NewValue <> ExistingValue then
    RegWriteStringValue(HKCU, DelphiLibraryKey, DelphiLibraryValueName, NewValue);
end;

procedure RegisterDesignPackage();
begin
  if not HasCompiledPackages() then
    Exit;

  RegWriteStringValue(
    HKCU,
    DelphiKnownPackagesKey,
    GetDesignPackagePath(),
    '{#MyAppName} Design-Time Package');
end;

procedure UnregisterDesignPackage();
begin
  RegDeleteValue(HKCU, DelphiKnownPackagesKey, GetDesignPackagePath());
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep <> ssPostInstall then
    Exit;

  if WizardIsTaskSelected('addlibrarypath') then
    AddDelphiLibraryPath();

  if WizardIsTaskSelected('registerdesignpkg') then
    RegisterDesignPackage();
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep <> usUninstall then
    Exit;

  RemoveDelphiLibraryPath();
  UnregisterDesignPackage();
end;
