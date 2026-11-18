$ErrorActionPreference = "Stop"

$Root = (Get-Location).Path
$Required = @("rSpacing", "apsim")
$Created = @("workflows", "runs", "results", "logs")

foreach ($name in $Required) {
    $path = Join-Path $Root $name
    if (-not (Test-Path $path -PathType Container)) {
        throw "Required directory is missing: $path"
    }
}

foreach ($name in $Created) {
    $path = Join-Path $Root $name
    New-Item -ItemType Directory -Path $path -Force | Out-Null
}

$gitignore = @"
/runs/
/results/
/logs/
/.Rproj.user/
.Rhistory
.RData
"@

$gitignorePath = Join-Path $Root ".gitignore"
if (-not (Test-Path $gitignorePath)) {
    Set-Content -Path $gitignorePath -Value $gitignore -Encoding UTF8
}

Write-Host "Workspace ready: $Root"
Write-Host "Next step: run source('environment_check.R') from R."
