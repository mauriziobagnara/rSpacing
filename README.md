# rSpacing / Spacing.Tool2 test workspace

This workspace is the stable local layout for testing and future development. The R package and C# solution remain in two independent Git repositories.

## Required layout

```text
rSpacing_workspace/
├── README.md
├── TEST_PROTOCOL.md
├── environment_check.R
├── setup_workspace.ps1
├── rSpacing/          # rSpacing/ folder in the R package repository, version 0.10.05
├── apsim/             # apsim/ folder in the APSIMxSpacing C# repository, Spacing.Tool2 version 0.09.05
├── workflows/         # writable copies of operational Rmd files (created during setup)
├── runs/              # temporary APSIMx runs; not versioned (created during setup)
├── results/           # persistent tables, figures, fixtures, and rankings (created during setup)
└── logs/              # environment, build, and test logs (created during setup)
```

Do not place `runs/`, `results/`, or `logs/` inside either repository.

## Repository references

```text
rSpacing repository URL: https://github.com/mauriziobagnara/rSpacing
rSpacing branch: development-Maurizio
rSpacing tag or commit:
Expected package version: 0.10.05

Spacing.Tool2 repository URL: https://github.com/mauriziobagnara/APSIMxSpacing
Spacing.Tool2 branch: main
Spacing.Tool2 tag or commit: 
Expected runtime version: 0.09.05
```

## First use

1. Place or clone the R repository as `rSpacing`.
2. Place or clone the C# repository as `apsim`.
3. Open PowerShell in the workspace root.
4. Prepare the workspace:

```powershell
powershell -ExecutionPolicy Bypass -File .\setup_workspace.ps1
```

5. Start R from the workspace root and run:

```r
source("environment_check.R")
```

6. Do not continue if a check marked `required = TRUE` fails in:

```text
logs/environment_check.csv
```

## Compile Spacing.Tool2

Open PowerShell in the workspace root and run:

```powershell
dotnet restore .\apsim\src\Spacing.Tool2\Spacing.Tool2.csproj 2>&1 |
  Tee-Object .\logs\dotnet_restore.log

dotnet build .\apsim\src\Spacing.Tool2\Spacing.Tool2.csproj `
  --configuration Debug `
  --framework net8.0 2>&1 |
  Tee-Object .\logs\dotnet_build.log
```

Expected Debug DLL:

```text
apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll
```

The workspace also accepts a Release build at:

```text
apsim/src/Spacing.Tool2/bin/Release/net8.0/spacing-tool2.dll
```

## Install rSpacing and build the vignette

The `.Rproj` file is not installed. It only opens the workspace as an RStudio project and sets the working directory.

From the workspace root, install directly from the package source with vignette construction enabled:

```r
workspace_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

package_root <- file.path(
  workspace_root,
  "rSpacing"
)

stopifnot(
  file.exists(
    file.path(package_root, "DESCRIPTION")
  )
)

devtools::install(
  pkg = package_root,
  build = TRUE,
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = FALSE
)
```

Restart R and verify:

```r
library(rSpacing)

packageVersion("rSpacing")
vignette(package = "rSpacing")
vignette(
  "rSpacing-workflow_EN",
  package = "rSpacing"
)
```

Expected package version:

```text
0.10.05 or 0.10.5
```

### Recommended clean installation from a source tarball

For the final delivery test, build and install the source tarball:

```r
workspace_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

package_root <- file.path(
  workspace_root,
  "rSpacing"
)

tarball <- devtools::build(
  pkg = package_root,
  vignettes = TRUE,
  manual = FALSE
)

install.packages(
  tarball,
  repos = NULL,
  type = "source"
)
```

Restart R and verify:

```r
library(rSpacing)

stopifnot(
  packageVersion("rSpacing") ==
    numeric_version("0.10.05")
)

vignette(package = "rSpacing")
vignette(
  "rSpacing-workflow_EN",
  package = "rSpacing"
)
```

### Build only the vignette

```r
tools::buildVignettes(
  dir = file.path(
    normalizePath(
      ".",
      winslash = "/",
      mustWork = TRUE
    ),
    "rSpacing"
  )
)
```

### Diagnose a missing installed vignette

```r
pkg <- find.package("rSpacing")

file.exists(
  file.path(pkg, "Meta", "vignette.rds")
)

list.files(
  file.path(pkg, "doc"),
  recursive = TRUE,
  full.names = TRUE
)
```

The expected result is:

- `Meta/vignette.rds` exists;
- the installed `doc/` directory contains the rendered vignette;
- `vignette("rSpacing-workflow_EN", package = "rSpacing")` opens the manual.

## Copy operational workflows

The package distributes operational R Markdown files under `rSpacing/inst/rmarkdown/`. Copy them to the writable `workflows/` directory before execution:

```r
workspace_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

source_dir <- file.path(
  workspace_root,
  "rSpacing",
  "inst",
  "rmarkdown"
)

target_dir <- file.path(
  workspace_root,
  "workflows"
)

files <- list.files(
  source_dir,
  pattern = "[.]Rmd$",
  full.names = TRUE
)

stopifnot(length(files) > 0L)
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

ok <- file.copy(
  files,
  target_dir,
  overwrite = FALSE
)

if (!all(ok)) {
  stop(
    "One or more workflows already exist or could not be copied."
  )
}
```

Do not execute operational Rmd files inside the package source tree or installed R library.

## Test order

1. Environment check.
2. C# restore and build.
3. R package documentation, tests, vignette, and check.
4. Clean source-tarball installation and installed vignette.
5. Full-function manual without model execution.
6. Full-function manual with model execution.
7. Five-year workflow.
8. Morris smoke test, then full Morris analysis.
9. Input-uncertainty smoke test, then full uncertainty analysis.
10. Final test report.

The detailed criteria, evidence requirements, and stop conditions are defined in `TEST_PROTOCOL.md`.
