param(
  [Parameter(Mandatory = $true)]
  [string]$Version,

  [string]$Repository = "",
  [string]$ReleaseTitle = "",
  [string]$ReleaseNotesFile = "",
  [string]$TargetBranch = "main",
  [string]$Config = "Release",
  [string]$Platform = "Win32",
  [string]$DelphiVersion = "23.0",
  [string]$DelphiDisplayName = "RAD Studio 12 Athens",
  [string]$PayloadFolder = "payload\Delphi12Athens\Win32",
  [switch]$Draft,
  [switch]$PreRelease,
  [switch]$SkipInstallerBuild,
  [switch]$SkipTagPush,
  [switch]$SkipReleaseCreate
)

$ErrorActionPreference = "Stop"

function Require-Command {
  param([string]$Name)

  if (-not (Get-Command $Name -ErrorAction SilentlyContinue)) {
    throw "Required command not found: $Name"
  }
}

function Test-Command {
  param([string]$Name)

  return [bool](Get-Command $Name -ErrorAction SilentlyContinue)
}

function Get-GitOutput {
  param([string[]]$GitArgs)

  $output = & git @GitArgs
  if ($LASTEXITCODE -ne 0) {
    throw "git $($GitArgs -join ' ') failed."
  }

  return ($output | Out-String).Trim()
}

function Ensure-CleanWorktree {
  $status = Get-GitOutput -GitArgs @("status", "--short")
  if ($status) {
    throw "Git worktree is not clean. Commit or stash changes before creating a release."
  }
}

function Normalize-VersionTag {
  param([string]$InputVersion)

  if ($InputVersion.StartsWith("v")) {
    return $InputVersion
  }

  return "v$InputVersion"
}

function Get-RepositoryFromRemote {
  $remoteUrl = Get-GitOutput -GitArgs @("remote", "get-url", "origin")

  if ($remoteUrl -match 'github\.com[:/](?<owner>[^/]+)/(?<repo>[^/.]+?)(?:\.git)?$') {
    return "$($Matches.owner)/$($Matches.repo)"
  }

  throw "Could not infer GitHub repository from origin remote: $remoteUrl"
}

function Ensure-TagDoesNotExist {
  param([string]$Tag)

  $existingLocal = Get-GitOutput -GitArgs @("tag", "--list", $Tag)
  if ($existingLocal -eq $Tag) {
    throw "Git tag already exists locally: $Tag"
  }

  $existingRemote = Get-GitOutput -GitArgs @("ls-remote", "--tags", "origin", $Tag)
  if ($existingRemote) {
    throw "Git tag already exists on origin: $Tag"
  }
}

function Read-ReleaseNotes {
  param(
    [string]$Tag,
    [string]$NotesPath
  )

  if ($NotesPath) {
    if (-not (Test-Path $NotesPath)) {
      throw "Release notes file not found: $NotesPath"
    }

    return Get-Content -LiteralPath $NotesPath -Raw
  }

  return @"
Release $Tag

- Built with the repository installer/release automation
"@
}

function Invoke-GitHubApi {
  param(
    [string]$Method,
    [string]$Uri,
    $Body = $null,
    [string]$ContentType = "application/json"
  )

  $token = $env:GITHUB_TOKEN
  if (-not $token) {
    throw "GITHUB_TOKEN environment variable is required for GitHub release publishing."
  }

  $headers = @{
    Authorization = "Bearer $token"
    Accept = "application/vnd.github+json"
    "User-Agent" = "VittixDBGridController-ReleaseScript"
    "X-GitHub-Api-Version" = "2022-11-28"
  }

  if ($null -ne $Body -and $ContentType -eq "application/json") {
    $Body = $Body | ConvertTo-Json -Depth 10
  }

  return Invoke-RestMethod -Method $Method -Uri $Uri -Headers $headers -Body $Body -ContentType $ContentType
}

function New-GitHubRelease {
  param(
    [string]$Repo,
    [string]$Tag,
    [string]$Title,
    [string]$BodyText,
    [bool]$IsDraft,
    [bool]$IsPreRelease,
    [string]$Branch
  )

  $payload = @{
    tag_name = $Tag
    target_commitish = $Branch
    name = $Title
    body = $BodyText
    draft = $IsDraft
    prerelease = $IsPreRelease
  }

  return Invoke-GitHubApi -Method POST -Uri "https://api.github.com/repos/$Repo/releases" -Body $payload
}

function Upload-GitHubReleaseAsset {
  param(
    [string]$UploadUrlTemplate,
    [string]$AssetPath
  )

  $token = $env:GITHUB_TOKEN
  $assetName = [System.IO.Path]::GetFileName($AssetPath)
  $uploadUrl = ($UploadUrlTemplate -replace '\{\?name,label\}$', '') + "?name=$([Uri]::EscapeDataString($assetName))"

  $headers = @{
    Authorization = "Bearer $token"
    Accept = "application/vnd.github+json"
    "User-Agent" = "VittixDBGridController-ReleaseScript"
    "X-GitHub-Api-Version" = "2022-11-28"
  }

  Invoke-RestMethod `
    -Method POST `
    -Uri $uploadUrl `
    -Headers $headers `
    -InFile $AssetPath `
    -ContentType "application/octet-stream" | Out-Null
}

function New-GitHubReleaseWithGh {
  param(
    [string]$Repo,
    [string]$Tag,
    [string]$Title,
    [string]$BodyFile,
    [string]$AssetPath,
    [bool]$IsDraft,
    [bool]$IsPreRelease,
    [string]$Branch
  )

  $args = @(
    "release", "create", $Tag,
    $AssetPath,
    "--repo", $Repo,
    "--target", $Branch,
    "--title", $Title,
    "--notes-file", $BodyFile
  )

  if ($IsDraft) {
    $args += "--draft"
  }

  if ($IsPreRelease) {
    $args += "--prerelease"
  }

  & gh @args
  if ($LASTEXITCODE -ne 0) {
    throw "gh release create failed."
  }
}

Require-Command git

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

$tag = Normalize-VersionTag -InputVersion $Version
$releaseName = if ($ReleaseTitle) { $ReleaseTitle } else { $tag }

if (-not $Repository) {
  $Repository = Get-RepositoryFromRemote
}

Ensure-CleanWorktree
if (-not $SkipTagPush) {
  Ensure-TagDoesNotExist -Tag $tag
}

if (-not $SkipInstallerBuild) {
  & powershell -ExecutionPolicy Bypass -File (Join-Path $PSScriptRoot "build-installer.ps1") -Config $Config -Platform $Platform -Version $Version -DelphiVersion $DelphiVersion -DelphiDisplayName $DelphiDisplayName -PayloadFolder $PayloadFolder
  if ($LASTEXITCODE -ne 0) {
    throw "Installer build failed."
  }
}

$installerExe = Join-Path $PSScriptRoot "output\VittixDBGridControllerSetup.exe"
if (-not (Test-Path $installerExe)) {
  throw "Installer output not found: $installerExe"
}

$releaseNotes = Read-ReleaseNotes -Tag $tag -NotesPath $ReleaseNotesFile
$tempNotesFile = Join-Path $env:TEMP "vittix-release-notes-$tag.txt"
Set-Content -LiteralPath $tempNotesFile -Value $releaseNotes -Encoding UTF8

if (-not $SkipTagPush) {
  Get-GitOutput -GitArgs @("tag", "-a", $tag, "-m", $releaseName)
  Get-GitOutput -GitArgs @("push", "origin", $TargetBranch)
  Get-GitOutput -GitArgs @("push", "origin", $tag)
}

try {
  if (-not $SkipReleaseCreate) {
    if (Test-Command gh) {
      New-GitHubReleaseWithGh `
        -Repo $Repository `
        -Tag $tag `
        -Title $releaseName `
        -BodyFile $tempNotesFile `
        -AssetPath $installerExe `
        -IsDraft ([bool]$Draft) `
        -IsPreRelease ([bool]$PreRelease) `
        -Branch $TargetBranch

      Write-Host "GitHub release created with gh for $tag"
    }
    else {
      $release = New-GitHubRelease `
        -Repo $Repository `
        -Tag $tag `
        -Title $releaseName `
        -BodyText $releaseNotes `
        -IsDraft ([bool]$Draft) `
        -IsPreRelease ([bool]$PreRelease) `
        -Branch $TargetBranch

      Upload-GitHubReleaseAsset -UploadUrlTemplate $release.upload_url -AssetPath $installerExe
      Write-Host "GitHub release created: $($release.html_url)"
    }
  }
  else {
    Write-Host "Tag created and pushed: $tag"
    Write-Host "Skipped GitHub release creation."
  }
}
finally {
  if (Test-Path $tempNotesFile) {
    Remove-Item -LiteralPath $tempNotesFile -Force
  }
}
