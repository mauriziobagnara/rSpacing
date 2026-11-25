# rSpacing / Spacing.Tool2 workspace setup (step T01)
# Run from the workspace root:
#   powershell -ExecutionPolicy Bypass -File .\setup_workspace.ps1
# Creates workflows/, runs/, results/ and logs/, and copies the R Markdown
# workflows shipped with rSpacing into workflows/ (existing files are kept).

$ErrorActionPreference = "Stop"
$Root = (Get-Location).Path

foreach ($name in @("rSpacing", "apsim")) {
    $path = Join-Path $Root $name
    if (-not (Test-Path $path -PathType Container)) {
        throw "Required directory is missing: $path. Run this script from the workspace root."
    }
}

foreach ($name in @("workflows", "runs", "results", "logs")) {
    New-Item -ItemType Directory -Path (Join-Path $Root $name) -Force | Out-Null
}

$gitignorePath = Join-Path $Root ".gitignore"
if (-not (Test-Path $gitignorePath)) {
    Set-Content -Path $gitignorePath -Encoding UTF8 -Value @"
/runs/
/results/
/logs/
/.Rproj.user/
.Rhistory
.RData
"@
}

$source = Join-Path $Root "rSpacing\inst\rmarkdown"
$copied = 0; $kept = 0
foreach ($file in Get-ChildItem -Path $source -Filter *.Rmd) {
    $target = Join-Path (Join-Path $Root "workflows") $file.Name
    if (Test-Path $target) { $kept++ } else { Copy-Item $file.FullName $target; $copied++ }
}

Write-Host "Workspace ready: $Root"
Write-Host "Workflows copied to workflows\: $copied new, $kept already present."
Write-Host "Next step (T02): powershell -ExecutionPolicy Bypass -File .\build_tool2.ps1"
