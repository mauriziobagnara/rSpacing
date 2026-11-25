# Build Spacing.Tool2 (step T02)
# Run from the workspace root:
#   powershell -ExecutionPolicy Bypass -File .\build_tool2.ps1
# The first build also compiles the APSIM source in apsim\apsim source\ and can
# take several minutes. Logs: logs\dotnet_restore.log and logs\dotnet_build.log.

$Root = (Get-Location).Path
$Project = Join-Path $Root "apsim\src\Spacing.Tool2\Spacing.Tool2.csproj"
$Dll = Join-Path $Root "apsim\src\Spacing.Tool2\bin\Debug\net8.0\spacing-tool2.dll"
if (-not (Test-Path $Project)) { throw "Project not found: $Project. Run this script from the workspace root." }
if (-not (Get-Command dotnet -ErrorAction SilentlyContinue)) { throw "dotnet was not found. Install the .NET 8 SDK and open a new PowerShell window." }
New-Item -ItemType Directory -Path (Join-Path $Root "logs") -Force | Out-Null

Write-Host "Restoring NuGet packages..."
# No build servers left in the background (they would keep the log open after the build)
$env:MSBUILDDISABLENODEREUSE = "1"; $env:DOTNET_CLI_USE_MSBUILD_SERVER = "0"
& dotnet restore $Project -nodeReuse:false *>&1 | Tee-Object -FilePath (Join-Path $Root "logs\dotnet_restore.log")
if ($LASTEXITCODE -ne 0) { throw "dotnet restore failed (exit $LASTEXITCODE). See logs\dotnet_restore.log." }

Write-Host "Building Spacing.Tool2 (Debug, net8.0)..."
& dotnet build $Project --configuration Debug --framework net8.0 --no-restore -nodeReuse:false -p:UseSharedCompilation=false *>&1 | Tee-Object -FilePath (Join-Path $Root "logs\dotnet_build.log")
$BuildExit = $LASTEXITCODE
& dotnet build-server shutdown *> $null
if ($BuildExit -ne 0) { throw "dotnet build failed (exit $BuildExit). See logs\dotnet_build.log." }
if (-not (Test-Path $Dll)) { throw "Build finished but the DLL is missing: $Dll" }

Write-Host "Spacing.Tool2 built: $Dll"
Write-Host "Next step (T03): Rscript install_rspacing.R"
