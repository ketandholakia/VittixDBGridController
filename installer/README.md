Vittix DBGrid Controller Installer
==================================

This folder contains an Inno Setup installer script for the component.
The single setup exe supports both Delphi 10.3 Rio and Delphi 12 Athens
(Win32 only). Each IDE is a separate component that is detected at setup
time and ticked only when that IDE is installed.

What the installer does
-----------------------

- Installs the `source` and `packages` folders (including
  `packages\D10_3`) into `Program Files\Vittix\DBGridController`
- Installs the palette `.dcr` resource needed to rebuild the design package
- Optionally installs documentation and the feature demo
- Per detected IDE:
  - Optionally adds `{app}\source` to that IDE's Win32 library path
  - Optionally registers that IDE's design-time package in Known Packages
- Ships all three packages per IDE: R (runtime), FD (FireDAC provider) and
  D (design-time). Only D is registered in Known Packages; R and FD are
  runtime packages that are loaded through the design package dependency
  and the library/search paths. Their .bpl and .dcp files are placed on
  disk under `{app}\payload\<IDE>\Win32\{Bpl,Dcp}` and the source folder is
  added to the library path, which is sufficient because the IDE resolves
  the runtime packages from the output directory and the DCP files from the
  Dcp folder on the search path.

Compiled package payload
------------------------

The installer can register the design-time package only when these files
are present before compiling the setup, per IDE:

Delphi 10.3 Rio (BDS 20.0):

- `installer\payload\Delphi10_3\Win32\Bpl\VittixDBGridControllerD.bpl`
- `installer\payload\Delphi10_3\Win32\Bpl\VittixDBGridControllerR.bpl`
- `installer\payload\Delphi10_3\Win32\Bpl\VittixDBGridControllerFD.bpl`
- `installer\payload\Delphi10_3\Win32\Dcp\VittixDBGridControllerD.dcp`
- `installer\payload\Delphi10_3\Win32\Dcp\VittixDBGridControllerR.dcp`
- `installer\payload\Delphi10_3\Win32\Dcp\VittixDBGridControllerFD.dcp`

Delphi 12 Athens (BDS 23.0):

- `installer\payload\Delphi12Athens\Win32\Bpl\VittixDBGridControllerD.bpl`
- `installer\payload\Delphi12Athens\Win32\Bpl\VittixDBGridControllerR.bpl`
- `installer\payload\Delphi12Athens\Win32\Bpl\VittixDBGridControllerFD.bpl`
- `installer\payload\Delphi12Athens\Win32\Dcp\VittixDBGridControllerD.dcp`
- `installer\payload\Delphi12Athens\Win32\Dcp\VittixDBGridControllerR.dcp`
- `installer\payload\Delphi12Athens\Win32\Dcp\VittixDBGridControllerFD.dcp`

If the payload for one IDE is missing, that IDE's component is still
listed (source-only copy is useful) but registration is skipped, and the
setup compiles cleanly because every payload file is guarded by `#ifexist`.

How to build the installer
--------------------------

1. Install Inno Setup 6 or 7.
2. Optionally place the compiled Delphi package files in the payload
   folders listed above, or let the helper build them.
3. Open `installer\VittixDBGridController.iss` in Inno Setup and build, or
   run the automated helper.

Automated build helper
----------------------

You can build the package payload and installer with:

```powershell
powershell -ExecutionPolicy Bypass -File .\installer\build-installer.ps1
```

The script takes a version table (Delphi 10.3 Rio = BDS 20.0, Delphi 12
Athens = BDS 23.0) and builds R, then FD, then D for each selected
version, copying the six .bpl/.dcp files into the matching payload folder.

Parameters:

| Parameter | Default | Notes |
|-----------|---------|-------|
| `-Config` | `Release` | Debug or Release |
| `-Platform` | `Win32` | Win32 only; Win64 is rejected |
| `-Version` | `2.1.0` | Installer / release version |
| `-Versions` | `10.3,12` | Comma list of IDEs to build |
| `-BdsRoot` | standard | RAD Studio root; supports a `{bds}` token |
| `-BplDir` | public docs | Override Bpl output dir; supports `{bds}` |
| `-DcpDir` | public docs | Override Dcp output dir; supports `{bds}` |
| `-SkipPackageBuild` | off | Skip msbuild, use existing outputs |
| `-SkipSetupCompile` | off | Skip ISCC |
| `-SkipMissing` | off | Skip IDEs whose BDS root is missing instead of failing |

Environment fallbacks: `VITTIX_BDS_ROOT`, `VITTIX_BPL_DIR`, `VITTIX_DCP_DIR`.

Examples:

```powershell
# Build only Delphi 12
powershell -ExecutionPolicy Bypass -File .\installer\build-installer.ps1 -Versions 12

# Build only Delphi 10.3
powershell -ExecutionPolicy Bypass -File .\installer\build-installer.ps1 -Versions 10.3

# Custom installation root
powershell -ExecutionPolicy Bypass -File .\installer\build-installer.ps1 `
  -BdsRoot "D:\Embarcadero\Studio\{bds}" `
  -BplDir "D:\Embarcadero\Studio\{bds}\Bpl" `
  -DcpDir "D:\Embarcadero\Studio\{bds}\Dcp"
```

The helper checks each package build result before copying the payload or
compiling the setup, and fails if a requested version's IDE is not
installed unless `-SkipMissing` is given. ISCC is run once with one
`/dHaveDelphi<ver>=1` flag per built version plus `/dMyAppVersion`.

GitHub release helper
---------------------

```powershell
$env:GITHUB_TOKEN = "your-token"
powershell -ExecutionPolicy Bypass -File .\installer\release-github.ps1 -Version 2.1.0
```

Passes the new parameters through to `build-installer.ps1`. Useful options:
`-Versions`, `-SkipMissing`, `-BdsRoot`, `-BplDir`, `-DcpDir`,
`-SkipInstallerBuild`, `-SkipTagPush`, `-SkipReleaseCreate`, `-Repository`,
`-Config`, `-Platform`.

Per-version registry keys
-------------------------

Delphi 10.3 Rio (BDS 20.0):

- Library path: `HKCU\Software\Embarcadero\BDS\20.0\Library\Win32`
  value `Search Path`
- Known Packages: `HKCU\Software\Embarcadero\BDS\20.0\Known Packages`
  value name = full path of
  `{app}\payload\Delphi10_3\Win32\Bpl\VittixDBGridControllerD.bpl`

Delphi 12 Athens (BDS 23.0):

- Library path: `HKCU\Software\Embarcadero\BDS\23.0\Library\Win32`
  value `Search Path`
- Known Packages: `HKCU\Software\Embarcadero\BDS\23.0\Known Packages`
  value name = full path of
  `{app}\payload\Delphi12Athens\Win32\Bpl\VittixDBGridControllerD.bpl`

The uninstaller removes exactly what was added for every available version
(library path entry and Known Packages value) and tolerates missing keys.

How to add another Delphi version
---------------------------------

1. Add a row to the `$VersionTable` in `build-installer.ps1` (key, BDS
   number, display name, payload folder, package subfolder).
2. Add a matching `#ifndef HaveDelphiX` / `#define` block, payload root
   defines, component, task, dir and `#ifexist`-guarded file entries in
   `VittixDBGridController.iss`, plus a row in `InitIDEs` in the `[Code]`
   section. The registration/uninstall loop is already generic, so no
   `[Code]` copy-paste is required beyond the table row.
3. Create `packages\<Subdir>\*.dproj` (or reuse `packages\`) for that IDE.
4. Document the new BDS number and registry keys here.

Notes
-----

- The current script targets Delphi 10.3 Rio (BDS 20.0) and Delphi 12
  Athens (BDS 23.0) on Win32.
- Design-time registration is done by writing the package path into
  `Known Packages`.
- Setup detects installed IDEs by reading `RootDir` from
  `HKCU\Software\Embarcadero\BDS\<ver>` (falling back to HKLM). Missing
  IDEs are shown with " (not detected)" and left unticked; the user can
  still tick them manually for a source-only copy.
- If a selected IDE is running (`bds.exe` / `TAppBuilder` window), setup
  shows a non-blocking warning because the IDE must be restarted to pick
  up the new package.
- If no supported IDE is detected, setup still succeeds as a source
  installer.