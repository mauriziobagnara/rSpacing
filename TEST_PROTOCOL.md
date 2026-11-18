# rSpacing v0.10.05 / Spacing.Tool2 v0.09.05 test protocol

## 1. Purpose

The first test cycle verifies installation, build reproducibility, package quality, vignette availability, and end-to-end integration. The tester must not modify R or C# source code during this cycle.

## 2. Required versions

| Component | Required reference |
|---|---|
| R package | rSpacing 0.10.05 |
| C# runtime | Spacing.Tool2 0.09.05 |
| .NET | 8.x |
| Five-year workflow | v1.05 EN workspace version |
| Morris workflow | v1.14 workspace version; v1.12 remains the stable execution reference |
| Input uncertainty | v1.04 workspace version |
| Full-function manual | rev09; `run_models: false` by default |
| Package vignette | `rSpacing-workflow_EN` |

Record repository URLs, branches, tags, and commit hashes before testing.

## 3. PASS, FAIL, BLOCKED

- **PASS:** observed result matches the stated criterion.
- **FAIL:** command completed but the result is wrong, incomplete, or raises an unexpected error.
- **BLOCKED:** a prerequisite is missing, so the test cannot be executed.

Do not convert a FAIL into a PASS by editing source files. Record the evidence and stop at the relevant gate.

## 4. Test evidence

Store all evidence under `logs/` or `results/test_results/`:

```text
logs/
├── environment_check.csv
├── sessionInfo.txt
├── dotnet_info.txt
├── dotnet_restore.log
├── dotnet_build.log
├── rSpacing_commit.txt
├── apsim_commit.txt
├── rSpacing_status.txt
└── apsim_status.txt

results/test_results/
├── package/
├── manual_rev09/
├── five_year/
├── morris_smoke/
└── uncertainty_smoke/
```

## 5. T00: environment gate

From the workspace root:

```r
source("environment_check.R")
```

**PASS criteria:** all required checks in `logs/environment_check.csv` are `TRUE`.

## 6. T01: C# restore and build

From PowerShell in the workspace root:

```powershell
dotnet restore .\apsim\src\Spacing.Tool2\Spacing.Tool2.csproj 2>&1 |
  Tee-Object .\logs\dotnet_restore.log

dotnet build .\apsim\src\Spacing.Tool2\Spacing.Tool2.csproj `
  --configuration Debug `
  --framework net8.0 2>&1 |
  Tee-Object .\logs\dotnet_build.log
```

**PASS criteria:** build exit code 0 and DLL exists at `apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll`.

## 7. T02: package documentation and tests

Start a clean R session from the workspace root:

```r
setwd("rSpacing")

devtools::document()
devtools::test()
tools::buildVignettes(dir = ".")
devtools::check(document = FALSE, manual = FALSE)
```

Save console output. **PASS criteria:** no FAIL, no ERROR, vignette build succeeds, and warnings are individually recorded and classified.

## 8. T03: source tarball and installed vignette

```r
tarball <- devtools::build(pkg = ".", vignettes = TRUE, manual = FALSE)
install.packages(tarball, repos = NULL, type = "source")
```

Restart R:

```r
library(rSpacing)
packageVersion("rSpacing")
vignette(package = "rSpacing")
vignette("rSpacing-workflow_EN", package = "rSpacing")
```

**PASS criteria:** version 0.10.05 and the vignette opens. No tar warning for paths longer than 100 bytes.

## 9. T04: full-function manual without model execution

Render rev09 with `run_models: false`.

**PASS criteria:** the document completes without requiring dotnet or producing APSIMx run folders.

## 10. T05: full-function manual with model execution

Render rev09 with `run_models: true` and `workspace_root: ".."` from the `workflows/` directory.

**PASS criteria:** one model run completes, outputs are written under workspace `runs/` and `results/`, and no personal hard-coded path appears in the run configuration.

## 11. T06: five-year workflow

Render the workspace version of the five-year workflow with a fresh result directory.

**PASS criteria:** model success, canonical fixture created, yearly rotation and management tables populated, output inventory produced, and standard error contains no unhandled exception.

## 12. T07: Morris smoke test

Use:

```yaml
trajectories: 2
levels: 4
grid_jump: 2
```

**PASS criteria:** baseline succeeds; parameter preflight audit is written; biomass uses `Wheat_Biomass_kgha`, `Maize_Biomass_kgha`, and `Soybean_Biomass_kgha`; all smoke-design runs complete; `morris_indices.csv` is created.

Expected total when 16 factors are accepted: 1 baseline + 24 preflight + 34 Morris = 59 runs.

## T07A: full Morris analysis

Run this test only after T07 has passed.

Use:

```yaml
analysis_profile: "full"
trajectories: 10
levels: 4
grid_jump: 2
output_subdir: "morris_v114_full"
```

## 13. T08: input-uncertainty smoke test

Use:

```yaml
samples: 30
bootstrap_replicates: 50
```

**PASS criteria:** baseline and all 16 preflight tests succeed; 30 LHS scenarios complete or satisfy the documented completion threshold; final Soil C and cumulative N2O-N, CO2-C, and nitrate-N leaching are available; PRCC files and non-empty figures are created.

Expected total: 1 baseline + 16 preflight + 30 LHS = 47 runs.

## T08A: full environmental input-uncertainty analysis

Run T08A only after the T08 smoke test has passed.

T08A is the complete input-uncertainty analysis. It uses 500 constrained Latin Hypercube scenarios and 500 bootstrap replicates. The four environmental outputs are analysed separately:

1. final `SoilCtot_kgCha`;
2. cumulative `N2O_total_kgNha`;
3. cumulative `CO2C_total_kgCha`;
4. cumulative `LeachNO3_kgNha`.

Yield and biomass are not included.

Use:

```yaml
analysis_profile: "full"
samples: 500
bootstrap_replicates: 500
output_subdir: "uncertainty_v104_full"
```
The complete analysis contains: 1 baseline run+ 16 input-preflight runs + 500 Latin Hypercube runs = 517 APSIMx runs


## 14. T09: final report

For every test, record:

| Field | Required content |
|---|---|
| Test ID | T00-T09 |
| Component | R package, C#, workflow, or integration |
| Version | File version, repository tag, commit |
| Command | Exact command executed |
| Expected | Criterion from this protocol |
| Observed | Actual result |
| Status | PASS / FAIL / BLOCKED |
| Evidence | Relative path to log or output |
| Notes | No source-code changes |

## 15. Stop conditions

Stop and report before continuing if:

- environment gate fails;
- C# build fails;
- package tests have FAIL or check has ERROR;
- vignette is absent from the installed source package;
- baseline model execution fails;
- preflight rejects every factor;
- crop-specific biomass columns are duplicated or missing;
- uncertainty environmental outputs are absent.
