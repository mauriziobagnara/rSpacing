# rSpacing / Spacing.Tool2 workspace

**rSpacing 0.10.09** (R package) and **Spacing.Tool2 0.09.10** (C# runner of APSIM Next Generation), with their workflows and tests.

This repository is the workspace: it holds the R package in `rSpacing/`, the scripts that build and test everything, and the test protocol. The C# solution comes from the separate repository APSIMxSpacing and is copied into `apsim/` (step T00); it is not tracked here. `runs/`, `results/` and `logs/` are created locally and are not tracked either.

## Branches of this version

| Repository | Branch | Content |
|---|---|---|
| [rSpacing](https://github.com/mauriziobagnara/rSpacing) | `release-0.10.09` | this workspace and rSpacing 0.10.09 in `rSpacing/` |
| [APSIMxSpacing](https://github.com/mauriziobagnara/APSIMxSpacing) | `release-0.09.10` | Spacing.Tool2 0.09.10 in `apsim/` |

The previous stable versions (rSpacing 0.10.05, Spacing.Tool2 0.09.05) are on `development-Maurizio` and `main`.

## Layout

```text
<workspace>/
├── README.md               # this file
├── TEST_PROTOCOL.md        # what each test does and its PASS criteria
├── setup_workspace.ps1     # step T01
├── build_tool2.ps1         # step T02
├── install_rspacing.R      # step T03
├── environment_check.R     # step T04
├── run_test.R              # steps T05-T14, one at a time
├── run_all.R               # all steps, unattended
├── tools/                  # helpers used by the scripts above
├── rSpacing/               # R package source, version 0.10.09
├── reference_results/      # results the tests are compared with
├── apsim/                  # copied in T00 from APSIMxSpacing (not tracked): spacing.sln, Spacing.Tool2 0.09.10, APSIM source
├── workflows/              # created by T01: writable copies of the R Markdown workflows
├── runs/                   # created by T01: APSIM runs (large, temporary)
├── results/                # created by T01: tables, figures, reports
└── logs/                   # created by T01: build, check and test logs
```

## Requirements

| Software | Version | Notes |
|---|---|---|
| Windows | 10 or 11 | PowerShell is included |
| .NET SDK | 8.x | `dotnet --info` must list an 8.0 SDK |
| R | 4.3 or newer | |
| RStudio | recent | provides pandoc, needed to render the reports |
| Visual Studio 2026 | optional | to open and edit `apsim/spacing.sln`; the build below uses only the .NET SDK |
| Internet | at T03 | to install missing CRAN packages, once |

Keep the workspace path short, for example `C:\spacing\workspace`: very long Windows paths can break APSIM's SQLite output.

## Step by step

### T00. Get the code

With GitHub Desktop:

1. Clone `mauriziobagnara/rSpacing` and `mauriziobagnara/APSIMxSpacing` (*File > Clone repository*). A short local path avoids problems with long Windows paths, for example `C:\spacing\GitHub\`.
2. In each repository, switch to the branch of this version (*Current branch*): `release-0.10.09` for rSpacing, `release-0.09.10` for APSIMxSpacing.
3. Copy the C# solution into the workspace. In PowerShell, with the two clone folders of step 1:

```powershell
robocopy "<clones>\APSIMxSpacing\apsim" "<clones>\rSpacing\apsim" /MIR /XD bin obj .vs /NFL /NDL
```

The rSpacing clone is now the workspace: run all the following steps from its root folder. Repeat the copy (and step T02) whenever APSIMxSpacing changes. Without Git, download the two branches as ZIP files from GitHub (*Code > Download ZIP*) and arrange them the same way.

### T01. Create the workspace folders

Open **PowerShell** in the workspace folder (in File Explorer: *File > Open Windows PowerShell*, or `cd` to the folder) and run:

```powershell
powershell -ExecutionPolicy Bypass -File .\setup_workspace.ps1
```

It creates `workflows/`, `runs/`, `results/` and `logs/`, and copies the six R Markdown workflows from `rSpacing/inst/rmarkdown/` into `workflows/`.

### T02. Build Spacing.Tool2

In the same PowerShell window:

```powershell
powershell -ExecutionPolicy Bypass -File .\build_tool2.ps1
```

This runs `dotnet restore` and `dotnet build` (Debug, net8.0) and writes `logs/dotnet_restore.log` and `logs/dotnet_build.log`. The first build compiles APSIM from `apsim/apsim source/` and takes several minutes. Result:

```text
apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll
```

Building in Visual Studio 2026 (open `apsim/spacing.sln`, configuration Debug, *Build > Build Solution*) gives the same DLL. Rebuild after any change to the C# code.

### T03. Install rSpacing

Open **RStudio**. Set the workspace folder as working directory (*Session > Set Working Directory > Choose Directory...*), then in the console:

```r
source("install_rspacing.R")
```

It installs missing CRAN packages, builds rSpacing from `rSpacing/` with its vignette, and installs it. Then restart R (*Session > Restart R*) and set the working directory again if needed.

### T04. Check the environment

```r
source("environment_check.R")
```

It checks versions, the Tool2 build, R packages, pandoc, the workflows and write access, and saves `logs/environment_check.csv`. **Do not continue if it stops with a failed check**: the detail column says what to do.

### T05-T14. Run the tests

```r
source("run_test.R")
run_test("T05")     # then T06, T07, ... T12; T13 and T14 are the long analyses
test_status()       # latest PASS / FAIL of each test
```

Each test runs in a separate R process, checks its own result against its PASS criterion, opens its HTML report, and records the outcome in `logs/test_status.csv`. The console output of a test is in `logs/tests/<ID>.log`.

| Test | What | Time |
|---|---|---|
| T05 | rSpacing tests (with real Tool2 runs) and `R CMD check` | about 5 min |
| T06 | operational manual, no model run | 1 min |
| T07 | operational manual, one model run | 2 min |
| T08 | five-year standard rotation report | 1 min |
| T09 | cookbook: every scenario option, run and checked | 2 min |
| T10 | scenario comparison: yield, soil C, nitrate leaching, greenhouse gases | 3 min |
| T11 | Morris sensitivity, smoke test (2 trajectories, 75 runs) | 10 min |
| T12 | input uncertainty, smoke test (30 samples, 51 runs) | 8 min |
| T13 | Morris sensitivity, full analysis (10 trajectories, 275 runs) | about 45 min |
| T14 | input uncertainty, full analysis (500 samples, 521 runs) | about 75 min |

Run T13 and T14 only after T05-T12 have passed. Details and PASS criteria are in `TEST_PROTOCOL.md`.

## Unattended run of everything

`run_all.R` does T00-T14 by itself: it creates the folders, builds Spacing.Tool2, installs rSpacing, checks the environment and runs every test, stopping at the first failure. With the full analyses it takes about 2.5 hours (about 40 minutes with `full=FALSE`). From PowerShell in the workspace root (use the path of your R version):

```powershell
& "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" run_all.R
& "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" run_all.R full=FALSE apsim_clone="C:/Users/<you>/Documents/GitHub/APSIMxSpacing"
```

or in RStudio: open `run_all.R`, set the workspace root as working directory, and choose *Source as Background Job*. Options (`key=value`): `apsim_clone` (refresh `apsim/` from the APSIMxSpacing clone first, i.e. T00), `build`, `install`, `full` (T13-T14), `stop_on_fail`; defaults are at the top of the script.

Keep the computer awake while it runs. At the end, `logs/run_all_summary.csv` lists every step with PASS / FAIL / SKIPPED, time and detail; each step's console output is in `logs/run_all/` and `logs/tests/`, and the reports are in `results/test_results/`.

## Using the workflows for your own work

The rendered reports are in `results/test_results/`. To change a scenario, edit the copy in `workflows/` (never the files in `rSpacing/inst/rmarkdown/`), save it under the next version number, and render it from RStudio with *Knit*, or with:

```r
rmarkdown::render("workflows/<file>.Rmd", params = list(workspace_root = ".."))
```

All workflows find the workspace from `workspace_root` (`..` = the parent of `workflows/`), so they contain no path of a specific computer. Every run uses mock weather and the demonstration soil unless changed in the workflow.

- **Learning the R API:** start from `rSpacing_cookbook_v1.00_EN.Rmd` (one recipe per option) and the package help (`?spacing_crop`, `help(package = "rSpacing")`).
- **Standard rotation** of all workflows: Soybean Generic_MG1 → winter wheat Claire → maize B_130, Gualtieri (RE), 2020-2024; wheat N at sowing, stem elongation and flag leaf.
- **Running Tool2 without R:** see `apsim/RSPACING_INTEGRATION.md` and the requests in `apsim/examples/`.

## Versions and changes

| Component | Version | Changes |
|---|---|---|
| rSpacing | 0.10.09 | `rSpacing/NEWS.md` |
| Spacing.Tool2 | 0.09.10 | `apsim/CHANGELOG.md` |
| Workflows | manual rev10, cookbook v1.00, scenario comparison v1.00, five-year v1.08, Morris v1.18, uncertainty v1.06 | `rSpacing/inst/rmarkdown/README.md` |

The full differences from the previous stable versions are the differences between the branches on GitHub: `development-Maurizio` → `release-0.10.09` in rSpacing, `main` → `release-0.09.10` in APSIMxSpacing.

When the workspace is a Git clone, `environment_check.R` records its branch and commit in `logs/`.
