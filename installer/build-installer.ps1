param(
  [ValidateSet("Debug", "Release")]
  [string]$Config = "Release",
  [ValidateSet("Win32")]
  [string]$Platform = "Win32",
  [string]$Version = "2.1.0",
  [string[]]$Versions = @("10.3", "12"),
  # RAD Studio root (contains bin/rsvars.bat). Falls back to the
  # VITTIX_BDS_ROOT environment variable, then to the standard location.
  # Accepts a {bds} token that is replaced per version.
  [string]$BdsRoot = "",
  # Where the package build left the .bpl/.dcp files. Falls back to the
  # VITTIX_BPL_DIR / VITTIX_DCP_DIR environment variables, then to the
  # default public-documents output for the given Delphi version. Accepts a
  # {bds} token that is replaced per version.
  [string]$BplDir = "",
  [string]$DcpDir = "",
  [switch]$SkipPackageBuild,
  [switch]$SkipSetupCompile,
  [switch]$SkipMissing
)

$ErrorActionPreference = "Stop"

if ($Platform -ne "Win32") {
  throw "Only Win32 is supported. Platform '$Platform' is not allowed."
}

# Version table: key, BDS number, display name, payload folder, package subdir.
$VersionTable = @(
  @{ Key = "10.3"; BDS = "20.0"; DisplayName = "RAD Studio 10.3 Rio"; PayloadFolder = "Delphi10_3"; PackageSubdir = "packages\D10_3" },
  @{ Key = "12";   BDS = "23.0"; DisplayName = "RAD Studio 12 Athens"; PayloadFolder = "Delphi12Athens"; PackageSubdir = "packages" }
)

function Normalize-VersionKey {
  param([string]$Value)
  $v = $Value.Trim().ToLower()
  if ($v -in @("10.3", "10_3", "delphi10_3", "rio")) { return "10.3" }
  if ($v -in @("12", "12.0", "delphi12", "athens", "23.0")) { return "12" }
  return $Value
}

function Resolve-ToolPath {
  param([string[]]$Candidates)
  foreach ($candidate in $Candidates) {
    if (Test-Path $candidate) { return $candidate }
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

$rsvars = $null
$msbuild = Resolve-ToolPath @(
  "C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe"
)

$iscc = Resolve-ToolPath @(
  "C:\Program Files\Inno Setup 7\ISCC.exe",
  "C:\Program Files (x86)\Inno Setup 6\ISCC.exe",
  "C:\Program Files\Inno Setup 6\ISCC.exe"
)

# Resolve selected versions against the table.
$selectedKeys = @()
foreach ($v in $Versions) {
  foreach ($part in ($v -split ",")) {
    $p = $part.Trim()
    if (-not $p) { continue }
    $k = Normalize-VersionKey $p
    $entry = $VersionTable | Where-Object { $_.Key -eq $k }
    if (-not $entry) {
      throw "Unknown version '$p'. Supported: 10.3, 12."
    }
    $selectedKeys += $k
  }
}
$selectedKeys = $selectedKeys | Select-Object -Unique

$builtVersions = @()

foreach ($key in $selectedKeys) {
  $vt = $VersionTable | Where-Object { $_.Key -eq $key } | Select-Object -First 1
  $bds = $vt.BDS
  $displayName = $vt.DisplayName
  $payloadFolder = $vt.PayloadFolder
  $packageSubdir = $vt.PackageSubdir

  $studioRoot = $BdsRoot
  if (-not $studioRoot) { $studioRoot = $env:VITTIX_BDS_ROOT }
  if (-not $studioRoot) { $studioRoot = "C:\Program Files (x86)\Embarcadero\Studio\$bds" }
  $studioRoot = $studioRoot.Replace("{bds}", $bds)

  $rsvarsPath = Join-Path $studioRoot "bin\rsvars.bat"
  if (-not (Test-Path $rsvarsPath)) {
    if ($SkipMissing) {
      Write-Warning "IDE not installed for $displayName (BDS $bds): $studioRoot. Skipping."
      continue
    }
    throw "IDE not installed for $displayName (BDS $bds): rsvars.bat not found at $rsvarsPath."
  }

  $publicDocs = Join-Path $env:PUBLIC "Documents\Embarcadero\Studio\$bds"
  $bplDir = $BplDir
  if (-not $bplDir) { $bplDir = $env:VITTIX_BPL_DIR }
  if (-not $bplDir) { $bplDir = Join-Path $publicDocs "Bpl" }
  $bplDir = $bplDir.Replace("{bds}", $bds)

  $dcpDir = $DcpDir
  if (-not $dcpDir) { $dcpDir = $env:VITTIX_DCP_DIR }
  if (-not $dcpDir) { $dcpDir = Join-Path $publicDocs "Dcp" }
  $dcpDir = $dcpDir.Replace("{bds}", $bds)

  $payloadRoot = Join-Path $installerDir ("payload\" + $payloadFolder + "\Win32")
  $payloadBplDir = Join-Path $payloadRoot "Bpl"
  $payloadDcpDir = Join-Path $payloadRoot "Dcp"
  Ensure-Directory -Path $payloadBplDir
  Ensure-Directory -Path $payloadDcpDir

  $packagesVersionDir = Join-Path $repoRoot $packageSubdir

  if (-not $SkipPackageBuild) {
    $rsvars = Resolve-ToolPath @($rsvarsPath)
    if (-not $rsvars) { throw "rsvars.bat not found for $displayName." }
    if (-not $msbuild) { throw "MSBuild not found." }

    Write-Host "Building runtime, FireDAC and design-time packages for $displayName (BDS $bds)..."

    $runtimeProj = Join-Path $packagesVersionDir "VittixDBGridControllerR.dproj"
    $fdProj = Join-Path $packagesVersionDir "VittixDBGridControllerFD.dproj"
    $designProj = Join-Path $packagesVersionDir "VittixDBGridControllerD.dproj"

    foreach ($proj in @($runtimeProj, $fdProj, $designProj)) {
      if (-not (Test-Path $proj)) { throw "Project not found: $proj" }
    }

    $buildCmd = 'call "' + $rsvars + '" >nul && ' +
      '"' + $msbuild + '" "' + $runtimeProj + '" /t:Build /p:Config=' + $Config +
      ' /p:Platform=' + $Platform + ' /v:minimal /nologo && ' +
      '"' + $msbuild + '" "' + $fdProj + '" /t:Build /p:Config=' + $Config +
      ' /p:Platform=' + $Platform + ' /v:minimal /nologo && ' +
      '"' + $msbuild + '" "' + $designProj + '" /t:Build /p:Config=' + $Config +
      ' /p:Platform=' + $Platform + ' /v:minimal /nologo'

    & cmd.exe /d /c $buildCmd
    if ($LASTEXITCODE -ne 0) {
      throw "Package build failed for $displayName."
    }
  }

  $filesToCopy = @(
    @{ Source = Join-Path $bplDir "VittixDBGridControllerR.bpl"; Dest = Join-Path $payloadBplDir "VittixDBGridControllerR.bpl" },
    @{ Source = Join-Path $bplDir "VittixDBGridControllerFD.bpl"; Dest = Join-Path $payloadBplDir "VittixDBGridControllerFD.bpl" },
    @{ Source = Join-Path $bplDir "VittixDBGridControllerD.bpl"; Dest = Join-Path $payloadBplDir "VittixDBGridControllerD.bpl" },
    @{ Source = Join-Path $dcpDir "VittixDBGridControllerR.dcp"; Dest = Join-Path $payloadDcpDir "VittixDBGridControllerR.dcp" },
    @{ Source = Join-Path $dcpDir "VittixDBGridControllerFD.dcp"; Dest = Join-Path $payloadDcpDir "VittixDBGridControllerFD.dcp" },
    @{ Source = Join-Path $dcpDir "VittixDBGridControllerD.dcp"; Dest = Join-Path $payloadDcpDir "VittixDBGridControllerD.dcp" }
  )

  foreach ($entry in $filesToCopy) {
    if (-not (Test-Path $entry.Source)) {
      throw "Expected build output not found for $displayName : $($entry.Source)"
    }
    Copy-Item -LiteralPath $entry.Source -Destination $entry.Dest -Force
  }

  Write-Host "Copied compiled package payload for $displayName into installer\payload\$payloadFolder."
  $builtVersions += $key
}

# ISCC flags: one /dHaveDelphiX=1 per built version.
$haveFlags = @()
foreach ($key in $builtVersions) {
  $safe = $key.Replace(".", "_")
  $haveFlags += "/dHaveDelphi$safe=1"
}

if (-not $SkipSetupCompile) {
  if (-not $iscc) {
    throw "ISCC.exe not found. Install Inno Setup or rerun with -SkipSetupCompile."
  }

  $issFile = Join-Path $installerDir "VittixDBGridController.iss"
  Write-Host "Compiling Inno Setup installer..."
  $isccArgs = @($issFile, "/dMyAppVersion=$Version") + $haveFlags
  & $iscc @isccArgs
  if ($LASTEXITCODE -ne 0) {
    throw "Inno Setup compilation failed."
  }
}

Write-Host "Installer build completed. Built versions: $($builtVersions -join ', ')"