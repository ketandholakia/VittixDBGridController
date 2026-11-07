Vittix DBGrid Controller Installer
==================================

This folder contains an Inno Setup installer script for the component.
The current payload and registration target Delphi 12 Athens on Win32, matching
the supported build target in the main README.

What the installer does
-----------------------

- Installs the `source` and `packages` folders into `Program Files\Vittix\DBGridController`
- Installs the palette `.dcr` resource needed to rebuild the design package
- Excludes local compiler output and demo state from the setup payload
- Optionally installs documentation and the feature demo
- Optionally adds `{app}\source` to the Delphi 12 Win32 library path
- Optionally registers the Delphi 12 design-time package if compiled package files are bundled

Compiled package payload
------------------------

The installer can register the design-time package only when these files are present before compiling the setup:

- `installer\payload\Delphi12Athens\Win32\Bpl\VittixDBGridControllerD.bpl`
- `installer\payload\Delphi12Athens\Win32\Bpl\VittixDBGridControllerR.bpl`
- `installer\payload\Delphi12Athens\Win32\Dcp\VittixDBGridControllerD.dcp`
- `installer\payload\Delphi12Athens\Win32\Dcp\VittixDBGridControllerR.dcp`

If those files are missing, the installer still works as a source installer and can still add the library path.

How to build the installer
--------------------------

1. Install Inno Setup 6.
2. Optionally place the compiled Delphi 12 package files in the payload folders listed above.
3. Open `installer\VittixDBGridController.iss` in Inno Setup.
4. Build the script.

Automated build helper
----------------------

You can also build the package payload and installer with:

```powershell
powershell -ExecutionPolicy Bypass -File .\installer\build-installer.ps1
```

The script takes optional parameters and environment variables so it does
not depend on machine-specific paths:

| Parameter | Environment fallback | Default |
|-----------|---------------------|---------|
| `-BdsRoot` | `VITTIX_BDS_ROOT` | `C:\Program Files (x86)\Embarcadero\Studio\<DelphiVersion>` |
| `-BplDir` | `VITTIX_BPL_DIR` | `<PUBLIC>\Documents\Embarcadero\Studio\<DelphiVersion>\Bpl` |
| `-DcpDir` | `VITTIX_DCP_DIR` | `<PUBLIC>\Documents\Embarcadero\Studio\<DelphiVersion>\Dcp` |

`-PayloadPlatform` defaults to `-Platform`; the helper rejects a mismatch so
the installer cannot label a Win32 package as another platform. The installer
uses the selected platform in its library-path registry key and task text.
Only Delphi 12 Athens/Win32 is currently built and tested; other combinations
need their own package build and installation verification.
The helper checks the package build result before copying the payload or
compiling the setup.

Example for a custom installation:

```powershell
powershell -ExecutionPolicy Bypass -File .\installer\build-installer.ps1 `
  -BdsRoot "D:\Embarcadero\Studio\23.0" `
  -BplDir "D:\Embarcadero\Studio\23.0\Bpl" `
  -DcpDir "D:\Embarcadero\Studio\23.0\Dcp"
```

What it does:

- Builds `VittixDBGridControllerR.dproj`
- Builds `VittixDBGridControllerD.dproj`
- Copies the resulting `.bpl` and `.dcp` files from the configured source
  directories (the Delphi 12 public-documents output by default) into
  `installer\payload\Delphi12Athens\Win32`
- Runs `ISCC.exe` to compile `installer\VittixDBGridController.iss`

Useful options:

- `-Config Debug`
- `-Version 1.0.4`
- `-DelphiVersion 23.0`
- `-DelphiDisplayName "RAD Studio 12 Athens"`
- `-PayloadRootName Delphi12Athens`
- `-PayloadPlatform Win32`
- `-SkipPackageBuild`
- `-SkipSetupCompile`

GitHub release helper
---------------------

To build the installer, tag the repo, and publish a GitHub release with the installer attached:

```powershell
$env:GITHUB_TOKEN = "your-token"
powershell -ExecutionPolicy Bypass -File .\installer\release-github.ps1 -Version 1.0.4
```

If `gh` is installed and authenticated, the script will prefer `gh release create`. Otherwise it falls back to the GitHub REST API using `GITHUB_TOKEN`.

What it does:

- Verifies the git worktree is clean
- Builds the installer through `installer\build-installer.ps1`
- Creates and pushes a tag that matches the supplied `-Version`
- Creates a GitHub release on the `origin` repository
- Uploads `installer\output\VittixDBGridControllerSetup.exe` as a release asset

Useful options:

- `-ReleaseNotesFile .\release-notes.md`
- `-Draft`
- `-PreRelease`
- `-SkipInstallerBuild`
- `-SkipTagPush`
- `-SkipReleaseCreate`
- `-Repository owner/repo`
- `-DelphiVersion 23.0`
- `-DelphiDisplayName "RAD Studio 12 Athens"`
- `-PayloadRootName Delphi12Athens`
- `-PayloadPlatform Win32`

Notes
-----

- The current script targets Delphi 12 Athens (`BDS 23.0`) on Win32.
- Design-time registration is done by writing the package path into:
  `HKCU\Software\Embarcadero\BDS\23.0\Known Packages`
- The Win32 library path is updated in:
  `HKCU\Software\Embarcadero\BDS\23.0\Library\Win32`
