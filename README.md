# rSpacing / Spacing.Tool2 test workspace

This workspace is the stable local layout for testing and future development. The R package and C# solution remain in two independent Git repositories.

## Required layout

```text
rSpacing_workspace/
├── README_FIRST.md
├── TEST_PROTOCOL.md
├── environment_check.R
├── setup_workspace.ps1
├── rSpacing/          # clone of the R package repository, version 0.10.05
├── apsim/             # clone of the C# repository, Spacing.Tool2 version 0.09.05
├── workflows/         # writable copies of operational Rmd files
├── runs/              # temporary APSIMx runs; not versioned
├── results/           # persistent tables, figures, fixtures, rankings
└── logs/              # environment, build and test logs
```

Do not put `runs`, `results`, or `logs` inside either repository.

## Repositories to record before testing

```text
rSpacing repository URL: https://github.com/mauriziobagnara/rSpacing
rSpacing branch: development-Maurizio
rSpacing tag or commit: <TO_FILL>
Expected package version: 0.10.05

Spacing.Tool2 repository URL: https://github.com/mauriziobagnara/APSIMxSpacing
Spacing.Tool2 branch: main
Spacing.Tool2 tag or commit: <TO_FILL>
Expected runtime version: 0.09.05
```

## First use

1. Place or clone the R repository as `rSpacing`.
2. Place or clone the C# repository as `apsim`.
3. Open PowerShell in the workspace root.
4. Run:

```powershell
powershell -ExecutionPolicy Bypass -File .\setup_workspace.ps1
```

5. Start R from the workspace root and run:

```r
source("environment_check.R")
```

6. Do not continue if `logs/environment_check.csv` contains a failed required check.

## Compile Spacing.Tool2

```powershell
dotnet restore .\apsim\src\Spacing.Tool2\Spacing.Tool2.csproj 2>&1 |
  Tee-Object .\logs\dotnet_restore.log

dotnet build .\apsim\src\Spacing.Tool2\Spacing.Tool2.csproj `
  --configuration Debug `
  --framework net8.0 2>&1 |
  Tee-Object .\logs\dotnet_build.log
```

Expected DLL:

```text
apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll
```

The environment checker also accepts a Release build at:

```text
apsim/src/Spacing.Tool2/bin/Release/net8.0/spacing-tool2.dll
```

## Copy operational workflows

The package distributes the operational R Markdown files under `rSpacing/inst/rmarkdown/`. Copy them to the writable `workflows/` directory before execution:

```r
source_dir <- file.path("rSpacing", "inst", "rmarkdown")
files <- list.files(source_dir, pattern = "[.]Rmd$", full.names = TRUE)
stopifnot(length(files) > 0L)
ok <- file.copy(files, "workflows", overwrite = FALSE)
if (!all(ok)) stop("One or more workflows already exist or could not be copied.")
```

Do not execute the Rmd files inside the package source tree or installed R library.

## Test order

1. Environment check.
2. C# restore and build.
3. R package documentation, tests, vignette and check.
4. Full-function manual with model execution disabled.
5. Full-function manual with model execution enabled.
6. Five-year workflow.
7. Morris smoke test with 2 trajectories.
8. Input-uncertainty smoke test with 30 samples and 50 bootstrap replicates.
9. Full analyses only after all smoke tests pass.

Follow `TEST_PROTOCOL.md` and do not change R or C# source code during the first test cycle.
