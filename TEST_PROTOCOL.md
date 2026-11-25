# rSpacing 0.10.09 / Spacing.Tool2 0.09.10 test protocol

## 1. Purpose

The test cycle checks installation, the build, package quality, and every workflow from start to end. The tester runs the steps in order and does **not** change R or C# source code during the cycle. The commands are in `README.md`; this document says what each test does and when it passes.

## 2. Versions under test

| Component | Version |
|---|---|
| rSpacing | 0.10.09 (`rSpacing/DESCRIPTION`; R shows `0.10.9`) |
| Spacing.Tool2 | 0.09.10 (`apsim/VERSION`) |
| .NET | 8.x |
| Operational manual | rev10 |
| Cookbook | v1.00 |
| Scenario comparison | v1.00 |
| Five-year workflow | v1.08 |
| Morris workflow | v1.18 |
| Input-uncertainty workflow | v1.06 |

The code comes from the branches `release-0.10.09` of rSpacing (this workspace) and `release-0.09.10` of APSIMxSpacing (copied into `apsim/`), as described in README step T00. Before testing, record the two branches and their commits; `environment_check.R` saves them in `logs/` when Git is available.

## 3. PASS, FAIL, BLOCKED

- **PASS:** the observed result meets the criterion.
- **FAIL:** the step ran but the result is wrong, incomplete, or an unexpected error occurred.
- **BLOCKED:** a prerequisite is missing, so the step cannot run.

Do not turn a FAIL into a PASS by editing source files. Record the evidence and stop (section 7).

The whole cycle can also run unattended with `run_all.R` (README, *Unattended run of everything*); it applies the same criteria and stop conditions and writes `logs/run_all_summary.csv`. For T05-T14, `run_test()` applies the criterion itself and writes PASS or FAIL, with a one-line explanation, to `logs/test_status.csv`. `test_status()` shows the latest result of each test.

## 4. Evidence

```text
logs/
├── dotnet_restore.log, dotnet_build.log     # T02
├── environment_check.csv, dotnet_info.txt   # T04
├── sessionInfo.txt
├── <repo>_commit.txt, _branch.txt, _status.txt   # when the folders are Git clones
├── tests/T05.log ... T14.log                # console output of each test
└── test_status.csv                          # PASS / FAIL of each test
results/
├── package/rSpacing_0.10.09.tar.gz          # T03
└── test_results/                            # reports and check output of T05-T14
```

Running a test again moves its earlier outputs to `runs/_previous/` and `results/_previous/` (nothing is deleted). These folders can be deleted by hand when no longer needed.

## 5. Tests

### T01. Workspace setup

`setup_workspace.ps1` (README, T01).

**PASS:** the script prints "Workspace ready"; `workflows/` contains the six `.Rmd` files of `rSpacing/inst/rmarkdown/`.

### T02. Build of Spacing.Tool2

`build_tool2.ps1` (README, T02).

**PASS:** the script prints "Spacing.Tool2 built"; `apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll` exists; `logs/dotnet_build.log` ends with `0 Error(s)`. Record the number of warnings: they come from the APSIM source and are not suppressed.

### T03. Installation of rSpacing

`source("install_rspacing.R")` in RStudio (README, T03).

**PASS:** the script prints "rSpacing 0.10.9 installed ... with the vignette"; `results/package/rSpacing_0.10.09.tar.gz` exists. After restarting R, `vignette("rSpacing-workflow_EN", package = "rSpacing")` opens the manual.

### T04. Environment gate

`source("environment_check.R")`.

**PASS:** the script ends with "Environment check passed"; every required check in `logs/environment_check.csv` is `TRUE`. Optional checks (`git_on_PATH`, `workspace_in_git`, `apsim_in_git`, `R_package_ggrepel`) may be `FALSE`: record them.

### T05. Package tests and R CMD check

`run_test("T05")`: runs the unit tests of rSpacing, including three integration tests that run the built Tool2, then `R CMD check` (examples and tests included).

**PASS:** no failed test, no test error, no check error. About 347 tests pass and 1 is skipped (an older integration test that compiles Tool2 with `dotnet run`). Record the check warnings and notes listed in `results/test_results/T05_package/check_summary.txt`. Expected notes: suggested packages `apsimx`, `leaflet` or `maps` not installed; installed size (reference data of about 34 MB).

### T06. Operational manual without model runs

`run_test("T06")`: renders the manual rev10 with `run_models = false`; it uses the 77 core functions of rSpacing on bundled data.

**PASS:** the report `results/test_results/T06_manual_no_run.html` is created; no `.NET` process is started.

### T07. Operational manual with a model run

`run_test("T07")`: the same manual with `run_models = true`; one mock run of Tool2.

**PASS:** the report is created and at least one APSIM output database is written under `runs/manual_rev10_run/`. No path of another computer appears in `logs/manual_rev10_run/runtime_configuration.csv`.

### T08. Five-year standard rotation

`run_test("T08")`: renders the five-year workflow v1.08. The rotation is Soybean Generic_MG1 → winter wheat Claire → maize B_130, Gualtieri (RE), 2020-2024, mock weather, demonstration silt loam. Wheat N is applied at sowing, at `MaximumSpikeletPrimordia` and at `FlagLeafAppearance`.

**PASS:** crops, sowing and harvest dates, and yields (within 1%) match `reference_results/T08_five_year_crop_summary.csv`:

| Crop | Sown | Harvested | Yield (kg/ha) |
|---|---|---|---|
| Soybean | 2020-05-01 | 2020-10-02 | 5374 |
| Wheat | 2020-10-10 | 2021-06-30 | 3326 |
| Maize | 2022-04-01 | 2022-09-06 | 13431 |
| Soybean | 2023-05-01 | 2023-10-01 | 5182 |
| Wheat | 2023-10-10 | 2024-06-29 | 4062 |

In the report, the fertilisation plan shows the two stage-triggered wheat doses about 160 and 195 days after sowing. Wheat yields are low because wheat is short of nitrogen on the demonstration soil; this is a known property of the demonstration case, not a test failure.

### T09. Cookbook

`run_test("T09")`: renders the cookbook v1.00. Ten recipes run every scenario option of rSpacing with Tool2 and check the result, for example that a stage-triggered fertiliser appears in the daily output the day after the stage. The options are soil, sowing window, fixed date and sowing rule, every fertiliser, irrigation, organic and tillage trigger, residue removal, repeated crops, cover crop, intercrop, perennial crop, own weather file, parameter overrides, and invalid inputs.

**PASS:** all checks pass (57 of 57), listed in `results/cookbook_v100/cookbook_checks.csv`. The document stops by itself if a check fails.

### T10. Scenario comparison

`run_test("T10")`: renders the scenario comparison v1.00. It runs nine management scenarios on the standard rotation (baseline, N -50% and +50%, rainfed, no-till, ploughing, wheat straw removed, oats cover crop, manure instead of mineral N at sowing). It compares yield, stable soil organic carbon (whole profile and 0-30 cm), nitrate leaching, N2O, and the net soil greenhouse-gas balance.

**PASS:** all nine scenarios run, and every indicator matches `reference_results/T10_scenario_indicators.csv` within 1%.

### T11. Morris smoke test

`run_test("T11")`: Morris v1.18, 2 trajectories, 4 levels, grid jump 2. Runs: 1 baseline + 24 parameter preflight + 2 × 25 Morris = 75.

**PASS:** all 24 candidate parameters are accepted at preflight; all 50 Morris runs succeed; `morris_indices.csv` and the global ranking are written under `results/morris_v118_smoke/`. Biomass uses `Wheat_Biomass_kgha`, `Maize_Biomass_kgha` and `Soybean_Biomass_kgha` (shown in the report).

### T12. Input-uncertainty smoke test

`run_test("T12")`: uncertainty v1.06, 30 Latin hypercube samples, 50 bootstrap replicates. Runs: 1 baseline + 20 input preflight + 30 = 51.

**PASS:** all 20 inputs are accepted at preflight; all 30 samples complete with the four outputs (final soil C, cumulative N2O-N, CO2-C and nitrate-N leaching); PRCC tables and figures are created under `results/uncertainty_v106_smoke/`.

### T13. Morris full analysis

Run only after T05-T12 have passed. `run_test("T13")`: 10 trajectories, 275 runs, about 45 minutes.

**PASS:** 24/24 parameters accepted and 250/250 Morris runs successful. The top three parameters by mu* are reported next to those of `reference_results/T13_morris_global_ranking.csv` (Soybean RUE, Maize RUE, Soybean grain number). The reference report is `reference_results/T13_morris_full_v1.18.html`.

### T14. Input-uncertainty full analysis

Run only after T05-T12 have passed. `run_test("T14")`: 500 samples, 500 bootstrap replicates, 521 runs, about 75 minutes.

**PASS:** 20/20 inputs accepted and 500/500 samples complete. Compare the output summary and input ranking of the report with `reference_results/T14_uncertainty_output_summary.csv` and `T14_uncertainty_input_ranking.csv` (reference report `T14_uncertainty_full_v1.06.html`). With the same seed and mock weather, the numbers are expected to agree to the last digits, apart from rounding differences between computers.

### T15. Test report

For every test record:

| Field | Content |
|---|---|
| Test ID | T01-T14 |
| Component | C#, R package, workflow |
| Version | from section 2, plus repository commit |
| Command | as in README |
| Expected | criterion from this protocol |
| Observed | from `logs/test_status.csv` or the console |
| Status | PASS / FAIL / BLOCKED |
| Evidence | relative path of the log or report |
| Notes | warnings, notes, optional checks that are FALSE |

## 6. Re-running

A test can be run again at any time with `run_test("<ID>")`; earlier outputs are set aside as described in section 4. After a change to the C# code, run T02 again; after a change to the R package, run T03 again; then T04 and the affected tests.

## 7. Stop conditions

Stop and report before continuing if:

- the build (T02) or the installation (T03) fails;
- the environment gate (T04) fails;
- T05 reports a failed test, a test error or a check error;
- a workflow test (T06-T12) is FAIL;
- in T11 or T13, preflight rejects any parameter, or crop biomass columns are missing;
- in T12 or T14, any of the four environmental outputs is missing.
