param(
  [string]$Config = "Release",
  [string]$Platform = "Win32",
  [switch]$SkipPackageBuild,
  [switch]$SkipSetupCompile
)

$ErrorActionPreference = "Stop"

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
$payloadRoot = Join-Path $installerDir "payload\Delphi12Athens\Win32"
$payloadBplDir = Join-Path $payloadRoot "Bpl"
$payloadDcpDir = Join-Path $payloadRoot "Dcp"

$rsvars = Resolve-ToolPath @(
  "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
)

$msbuild = Resolve-ToolPath @(
  "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\msbuild.exe",
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

  Write-Host "Building runtime and design-time packages for Delphi 12..."

  $runtimeProj = Join-Path $packagesDir "VittixDBGridControllerR.dproj"
  $designProj = Join-Path $packagesDir "VittixDBGridControllerD.dproj"

  $buildCmd = @"
call "$rsvars"
"$msbuild" "$runtimeProj" /t:Build /p:Config=$Config /p:Platform=$Platform
if errorlevel 1 exit /b 1
"$msbuild" "$designProj" /t:Build /p:Config=$Config /p:Platform=$Platform
if errorlevel 1 exit /b 1
"@

  & cmd.exe /c $buildCmd
  if ($LASTEXITCODE -ne 0) {
    throw "Package build failed."
  }
}

$publicDocs = Join-Path $env:PUBLIC "Documents\Embarcadero\Studio\23.0"
$sourceBplDir = Join-Path $publicDocs "Bpl"
$sourceDcpDir = Join-Path $publicDocs "Dcp"

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
  & $iscc $issFile
  if ($LASTEXITCODE -ne 0) {
    throw "Inno Setup compilation failed."
  }
}

Write-Host "Installer build completed."
