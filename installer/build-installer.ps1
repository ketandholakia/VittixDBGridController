param(
  [ValidateSet("Debug", "Release")]
  [string]$Config = "Release",
  [ValidateSet("Win32", "Win64")]
  [string]$Platform = "Win32",
  [string]$Version = "1.0.0",
  [string]$DelphiVersion = "23.0",
  [string]$DelphiDisplayName = "RAD Studio 12 Athens",
  [string]$PayloadRootName = "Delphi12Athens",
  [string]$PayloadPlatform = "",
  # RAD Studio root (contains bin/rsvars.bat). Falls back to the
  # VITTIX_BDS_ROOT environment variable, then to the standard location.
  [string]$BdsRoot = "",
  # Where the package build left the .bpl/.dcp files. Falls back to the
  # VITTIX_BPL_DIR / VITTIX_DCP_DIR environment variables, then to the
  # default public-documents output for the given Delphi version.
  [string]$BplDir = "",
  [string]$DcpDir = "",
  [switch]$SkipPackageBuild,
  [switch]$SkipSetupCompile
)

$ErrorActionPreference = "Stop"

if (-not $PayloadPlatform) { $PayloadPlatform = $Platform }
if ($PayloadPlatform -ne $Platform) {
  throw "PayloadPlatform ($PayloadPlatform) must match the package build Platform ($Platform)."
}

function Resolve-ToolPath {
  param([string[]]$Candidates)

  foreach ($candidate in $Candidates) {
    if (Test-Path $candidate) {
      return $candidate
    }
  }

  return $null
}

function Ensure-Directory {
  param([string]$Path)

  if (-not (Test-Path $Path)) {
    New-Item -ItemType Directory -Path $Path | Out-Null
  }
}

$repoRoot = Split-Path -Parent $PSScriptRoot
$packagesDir = Join-Path $repoRoot "packages"
$installerDir = $PSScriptRoot
$payloadRoot = Join-Path $installerDir ("payload\" + $PayloadRootName + "\" + $PayloadPlatform)
$payloadBplDir = Join-Path $payloadRoot "Bpl"
$payloadDcpDir = Join-Path $payloadRoot "Dcp"
if ($BdsRoot -eq "") { $BdsRoot = $env:VITTIX_BDS_ROOT }
if ($BdsRoot -eq "") {
  $BdsRoot = "C:\Program Files (x86)\Embarcadero\Studio\$DelphiVersion"
}
$studioRoot = $BdsRoot
$publicDocs = Join-Path $env:PUBLIC "Documents\Embarcadero\Studio\$DelphiVersion"

if ($BplDir -eq "") { $BplDir = $env:VITTIX_BPL_DIR }
if ($BplDir -eq "") { $BplDir = Join-Path $publicDocs "Bpl" }
if ($DcpDir -eq "") { $DcpDir = $env:VITTIX_DCP_DIR }
if ($DcpDir -eq "") { $DcpDir = Join-Path $publicDocs "Dcp" }

$rsvars = Resolve-ToolPath @(
  (Join-Path $studioRoot "bin\rsvars.bat")
)

$msbuild = Resolve-ToolPath @(
  (Join-Path $studioRoot "bin\msbuild.exe"),
  "C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe"
)

$iscc = Resolve-ToolPath @(
  "C:\Program Files (x86)\Inno Setup 6\ISCC.exe",
  "C:\Program Files\Inno Setup 6\ISCC.exe"
)

if (-not $SkipPackageBuild) {
  if (-not $rsvars) {
    throw "rsvars.bat not found. Update installer\build-installer.ps1 for your RAD Studio installation."
  }

  if (-not $msbuild) {
    throw "MSBuild not found. Install RAD Studio/MSBuild or update installer\build-installer.ps1."
  }

  Write-Host "Building runtime and design-time packages for Delphi $DelphiVersion..."

  $runtimeProj = Join-Path $packagesDir "VittixDBGridControllerR.dproj"
  $designProj = Join-Path $packagesDir "VittixDBGridControllerD.dproj"

  $buildCmd = 'call "' + $rsvars + '" >nul && "' + $msbuild +
    '" "' + $runtimeProj + '" /t:Build /p:Config=' + $Config +
    ' /p:Platform=' + $Platform + ' /v:minimal /nologo && "' +
    $msbuild + '" "' + $designProj + '" /t:Build /p:Config=' +
    $Config + ' /p:Platform=' + $Platform + ' /v:minimal /nologo'

  & cmd.exe /d /c $buildCmd
  if ($LASTEXITCODE -ne 0) {
    throw "Package build failed."
  }
}

$sourceBplDir = $BplDir
$sourceDcpDir = $DcpDir

Ensure-Directory -Path $payloadBplDir
Ensure-Directory -Path $payloadDcpDir

$filesToCopy = @(
  @{ Source = Join-Path $sourceBplDir "VittixDBGridControllerR.bpl"; Dest = Join-Path $payloadBplDir "VittixDBGridControllerR.bpl" },
  @{ Source = Join-Path $sourceBplDir "VittixDBGridControllerD.bpl"; Dest = Join-Path $payloadBplDir "VittixDBGridControllerD.bpl" },
  @{ Source = Join-Path $sourceDcpDir "VittixDBGridControllerR.dcp"; Dest = Join-Path $payloadDcpDir "VittixDBGridControllerR.dcp" },
  @{ Source = Join-Path $sourceDcpDir "VittixDBGridControllerD.dcp"; Dest = Join-Path $payloadDcpDir "VittixDBGridControllerD.dcp" }
)

foreach ($entry in $filesToCopy) {
  if (-not (Test-Path $entry.Source)) {
    throw "Expected build output not found: $($entry.Source)"
  }

  Copy-Item -LiteralPath $entry.Source -Destination $entry.Dest -Force
}

Write-Host "Copied compiled package payload into installer\payload."

if (-not $SkipSetupCompile) {
  if (-not $iscc) {
    throw "ISCC.exe not found. Install Inno Setup 6 or rerun with -SkipSetupCompile."
  }

  $issFile = Join-Path $installerDir "VittixDBGridController.iss"
  Write-Host "Compiling Inno Setup installer..."
  & $iscc $issFile "/dMyAppVersion=$Version" "/dDelphiVersion=$DelphiVersion" "/dDelphiDisplayName=$DelphiDisplayName" "/dPayloadRoot=payload\$PayloadRootName\$PayloadPlatform" "/dPayloadRootName=$PayloadRootName" "/dPayloadPlatform=$PayloadPlatform"
  if ($LASTEXITCODE -ne 0) {
    throw "Inno Setup compilation failed."
  }
}

Write-Host "Installer build completed."
