# rSpacing v0.10.05 / Spacing.Tool2 v0.09.05 test protocol

## 1. Purpose

This protocol verifies installation, build reproducibility, package quality, vignette availability, and end-to-end integration. The tester must not modify R or C# source code during the first test cycle.

Operational setup commands are maintained in `README.md`. This protocol avoids repeating those instructions and defines the validation evidence and PASS criteria.

## 2. Required versions

| Component | Required reference |
|---|---|
| R package | rSpacing 0.10.05 |
| C# runtime | Spacing.Tool2 0.09.05 |
| .NET | 8.x or a newer SDK capable of targeting `net8.0` |
| Five-year workflow | v1.05 EN workspace version |
| Morris workflow | v1.14 workspace version; v1.12 remains the stable execution reference |
| Input uncertainty | v1.04 workspace version |
| Full-function manual | rev09; `run_models: false` by default |
| Package vignette | `rSpacing-workflow_EN` |

Record repository URLs, branches, tags, and commit hashes before final delivery.

## 3. PASS, FAIL, and BLOCKED

- **PASS:** observed result matches every stated criterion.
- **FAIL:** the command runs but the result is wrong, incomplete, or raises an unexpected error.
- **BLOCKED:** a prerequisite is missing, so the test cannot be executed.

Do not convert a FAIL into a PASS by editing source files during the test. Record the evidence and stop at the relevant gate.

## 4. Test evidence

Store evidence under `logs/` or `results/test_results/`:

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
├── morris_full/
├── uncertainty_smoke/
└── uncertainty_full/
```

## 5. T00: environment gate

Complete `README.md`, section **First use**.

### Evidence

- `logs/environment_check.csv`
- `logs/sessionInfo.txt`
- `logs/dotnet_info.txt`

### PASS criteria

- every row marked `required = TRUE` has `passed = TRUE`;
- optional Git checks may be false during local preparation but must be resolved before final delivery;
- `rSpacing` is version 0.10.05;
- the C# project, configuration, and Debug or Release DLL are found;
- `workflows/`, `runs/`, `results/`, and `logs/` are writable.

## 6. T01: C# restore and build

Complete `README.md`, section **Compile Spacing.Tool2**.

### Evidence

- `logs/dotnet_restore.log`
- `logs/dotnet_build.log`

### PASS criteria

- restore exits with status 0;
- build exits with status 0;
- the expected Debug or Release DLL exists;
- no C# source file is modified during the test.

## 7. T02: package documentation and tests

Start a clean R session from the workspace root:

```r
package_root <- file.path(
  normalizePath(
    ".",
    winslash = "/",
    mustWork = TRUE
  ),
  "rSpacing"
)

devtools::document(package_root)
devtools::test(package_root)
tools::buildVignettes(dir = package_root)
devtools::check(
  package_root,
  document = FALSE,
  manual = FALSE
)
```

### Evidence

Save the console output or redirect it to files under `results/test_results/package/`.

### PASS criteria

- `devtools::document()` completes;
- tests contain no FAIL;
- vignette construction completes;
- `devtools::check()` contains no ERROR;
- warnings are individually recorded and classified.

## 8. T03: source tarball and installed vignette

Complete `README.md`, section **Recommended clean installation from a source tarball**.

### Evidence

- generated source tarball;
- package installation output;
- installed package version;
- vignette listing.

### PASS criteria

- installed version is 0.10.05 or 0.10.5;
- `rSpacing-workflow_EN` is registered and opens;
- no tar warning reports a path longer than 100 bytes;
- the installed package contains `Meta/vignette.rds` and a rendered file under `doc/`.

## 9. T04: full-function manual without model execution

Render rev09 with:

```yaml
run_models: false
```

### PASS criteria

- the document completes;
- APSIMx is not executed;
- no APSIMx run directory is created;
- `runtime_configuration.csv` records `run_models = FALSE`;
- the rendered manual is written under `results/test_results/manual_rev09/`.

## 10. T05: full-function manual with model execution

Render rev09 from `workflows/` with:

```yaml
workspace_root: ".."
run_models: true
```

### PASS criteria

- one APSIMx model run completes;
- `mock = TRUE` remains active;
- outputs are written under workspace `runs/`, `results/`, and `logs/`;
- no personal hard-coded path appears in `runtime_configuration.csv`;
- the computation-time table reports one planned APSIMx run and measured elapsed time.

## 11. T06: five-year workflow

Render the v1.05 EN workflow with a fresh output subdirectory and:

```yaml
build_fixture: true
```

### PASS criteria

- the five-year model run succeeds;
- the canonical fixture is created;
- yearly rotation and management tables are populated;
- the output inventory is created;
- standard error contains no unhandled exception;
- `computation_time.csv` and `document_elapsed_time.csv` are created.

## 12. T07: Morris smoke test

Use:

```yaml
analysis_profile: "smoke"
trajectories: 2
levels: 4
grid_jump: 2
output_subdir: "morris_v114_smoke"
```

### Expected workload

With 24 candidate factors and 16 accepted factors:

```text
1 baseline + 24 preflight + 2 × (16 + 1) = 59 APSIMx runs
```

The actual total must be recalculated from the preflight audit:

```r
candidate_factors <- nrow(preflight)
accepted_factors <- sum(preflight$accepted %in% TRUE)
trajectories <- 2L

expected_morris_runs <- trajectories * (
  accepted_factors + 1L
)

expected_total_runs <- 1L +
  candidate_factors +
  expected_morris_runs
```

### PASS criteria

- baseline succeeds;
- parameter preflight audit is written;
- at least one factor is accepted;
- the Morris run count matches the validated design;
- all smoke-run response vectors used in the analysis are complete;
- biomass sources are exactly:
  - `Wheat_Biomass_kgha`;
  - `Maize_Biomass_kgha`;
  - `Soybean_Biomass_kgha`;
- no biomass source is duplicated;
- `morris_indices.csv` is created;
- timing and workload tables are created.

T07 is a technical smoke test. Its ranking is diagnostic only.

## 13. T07A: full Morris analysis

Run only after T07 passes.

Use:

```yaml
analysis_profile: "full"
trajectories: 10
levels: 4
grid_jump: 2
output_subdir: "morris_v114_full"
```

### Expected workload

With 24 candidate factors and 16 accepted factors:

```text
1 baseline + 24 preflight + 10 × (16 + 1) = 195 APSIMx runs
```

The actual total must be recalculated from the full-analysis preflight using the same formula as T07 with `trajectories <- 10L`.

### PASS criteria

- all T07 criteria pass for the full design;
- all `mu`, `mu_star`, and `sigma` values are finite;
- one index row exists for each accepted factor;
- the global ranking and `mu_star`-`sigma` plot are non-empty;
- no T07 result is reused;
- `computation_time_estimate.csv` and `document_elapsed_time.csv` are created.

## 14. T08: input-uncertainty smoke test

Use:

```yaml
analysis_profile: "smoke"
samples: 30
bootstrap_replicates: 50
output_subdir: "uncertainty_v104_smoke"
```

### Expected workload

```text
1 baseline + 16 preflight + 30 LHS = 47 APSIMx runs
```

Bootstrap replicates do not add APSIMx runs.

### PASS criteria

- baseline succeeds;
- all 16 preflight tests are accepted;
- 30 LHS scenarios are attempted;
- at least 20 scenarios are complete;
- complete scenarios contain finite final Soil C and cumulative N2O-N, CO2-C, and nitrate-N leaching;
- PRCC files and non-empty figures are created for all four outputs;
- timing and completion tables are created.

T08 is a technical smoke test. Its PRCC values and rankings are diagnostic only.

## 15. T08A: full environmental input-uncertainty analysis

Run only after T08 passes.

Use:

```yaml
analysis_profile: "full"
samples: 500
bootstrap_replicates: 500
output_subdir: "uncertainty_v104_full"
```

### Expected workload

```text
1 baseline + 16 preflight + 500 LHS = 517 APSIMx runs
```

The four outputs are analysed separately:

1. final `SoilCtot_kgCha`;
2. cumulative `N2O_total_kgNha`;
3. cumulative `CO2C_total_kgCha`;
4. cumulative `LeachNO3_kgNha`.

Yield and biomass are excluded.

### PASS criteria

- baseline succeeds and all four baseline outputs are finite;
- all 16 preflight tests are accepted;
- the LHS design contains 500 unique sample identifiers;
- generated soil and management scenarios pass the physical-validity audit;
- the run inventory contains 500 scenarios;
- failures, if any, are documented and evaluated for concentration within the sampled domain;
- the completion threshold defined by the workflow is satisfied;
- four output summaries and four PRCC rankings are created;
- all reported PRCC, interval, and standardised-regression values are finite;
- PRCC intervals are ordered correctly around their point estimates;
- `computation_time_estimate.csv` and `document_elapsed_time.csv` are created;
- the completion report records 517 planned APSIMx runs.

## 16. T09: final report

For every test, record:

| Field | Required content |
|---|---|
| Test ID | T00-T09 |
| Component | R package, C#, workflow, or integration |
| Version | File version, repository tag, and commit |
| Command | Exact command executed |
| Expected | Criterion from this protocol |
| Observed | Actual result |
| Status | PASS / FAIL / BLOCKED |
| Evidence | Relative path to log or output |
| Notes | Confirmation that no source code was changed during testing |

## 17. Stop conditions

Stop and report before continuing if:

- environment gate fails;
- C# build fails;
- package tests have FAIL or package check has ERROR;
- the vignette is absent from the installed source package;
- a baseline model execution fails;
- preflight rejects every factor;
- crop-specific biomass columns are duplicated or missing;
- uncertainty environmental outputs are absent;
- required timing or provenance files are not created.
