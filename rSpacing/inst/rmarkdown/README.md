# rSpacing APSIMx workflow scripts

This directory contains complete, executable R Markdown workflows distributed with the **rSpacing** package. They are operational analyses, not package vignettes. They are installed as reference scripts and are **not** compiled or executed during `R CMD build`, `R CMD check`, or package installation.

## Included workflows

| File | Purpose | Default APSIMx workload |
|---|---|---:|
| `rSpacing_APSIMx_5year_workflow_report_v1.04_EN.Rmd` | Runs and documents the 2020–2024 Wheat–Maize–Soybean rotation, including requested and applied management, yield, Soil C, soil water, output inventories, tables, and figures. | 1 run |
| `rSpacing_APSIMx_Morris_multivariate_yield_biomass_all_cultivars_v1.13.Rmd` | Performs parameter preflight followed by a multivariate Morris analysis for crop-specific yield and total biomass. | 1 baseline + 24 preflight runs + `r × (k + 1)` Morris runs |
| `rSpacing_APSIMx_input_uncertainty_environmental_outputs_v1.03.Rmd` | Performs a constrained Latin Hypercube input-uncertainty analysis for final Soil C, cumulative N2O-N, cumulative CO2-C, and cumulative nitrate-N leaching. | 1 baseline + 16 preflight runs + 500 LHS runs = 517 runs |

In the Morris expression, `r` is the number of trajectories and `k` is the number of parameters accepted by preflight. With the default 10 trajectories and 16 accepted parameters, the complete workflow requires 195 new APSIMx runs: 1 baseline, 24 preflight runs, and 170 Morris runs.

## Important distinction between operational and stable versions

The scripts in this directory contain expanded explanations intended to support presentation and interpretation of the work.

- The documented five-year workflow is `v1.04_EN`.
- The documented Morris workflow is `v1.13`.
- The documented input-uncertainty workflow is `v1.03`.
- The verified operational baseline for future Morris development remains `rSpacing_APSIMx_Morris_multivariate_yield_biomass_all_cultivars_v1.12.Rmd`. The v1.13 script preserves the same method but adds presentation guidance and uses its own fresh execution paths.

Do not overwrite an existing version. Create the next progressive version for any change.

## System requirements

These workflows require more than the installed R package alone.

1. **R** and **RStudio** or another R Markdown rendering environment.
2. **Pandoc**, normally supplied with RStudio.
3. **.NET 8 SDK or runtime**, with `dotnet` available on `PATH`.
4. A compiled `Spacing.Tool2` solution compatible with the workflow request schema.
5. Write access to the selected working and output directories.
6. The R packages used by the selected workflow.

Typical R dependencies include:

```r
install.packages(c(
  "jsonlite",
  "ggplot2",
  "rmarkdown",
  "knitr",
  "sensitivity",
  "DBI",
  "RSQLite"
))
```

`DBI` and `RSQLite` are used to inspect APSIMx SQLite DataStore outputs. The Morris workflow requires `sensitivity`.

## Retrieve the scripts from an installed package

Locate the installed directory with:

```r
system.file("scripts", package = "rSpacing")
```

List the available R Markdown files with:

```r
workflow_dir <- system.file("scripts", package = "rSpacing")
list.files(workflow_dir, pattern = "[.]Rmd$", full.names = TRUE)
```

## Copy before running

Do not render the workflows directly inside the installed package library. Installed package directories may be read-only, and these analyses create JSON, CSV, RDS, PNG, log, SQLite, and run-directory outputs.

Copy the scripts to a writable project directory first:

```r
source_dir <- system.file("scripts", package = "rSpacing")
target_dir <- file.path(getwd(), "rSpacing_workflows")

dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(
  source_dir,
  pattern = "[.]Rmd$",
  full.names = TRUE
)

ok <- file.copy(
  from = files,
  to = file.path(target_dir, basename(files)),
  overwrite = FALSE
)

if (!all(ok)) {
  stop("One or more workflow files could not be copied.")
}

normalizePath(target_dir, winslash = "/", mustWork = TRUE)
```

Open the copied Rmd in RStudio and render it from the writable project directory.

## Configure `SPACING_ROOT`

Before execution, update the path near the beginning of each Rmd:

```r
SPACING_ROOT <- "C:/Users/your.name/Documents/spacing-main/apsim"
```

The path must point to the local APSIMx/rSpacing development tree expected by the workflow. The scripts verify the presence of both:

```text
src/Spacing.Tool2/Spacing.Tool2.csproj
src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll
```

If the solution was compiled in `Release`, update `BUILD_CONFIGURATION` accordingly.

Test the external runtime before starting a long analysis:

```r
Sys.which("dotnet")
system2("dotnet", "--info")
```

## Execution policy

The current workflows use:

```r
USE_MOCK_WEATHER <- TRUE
```

Keep mock weather enabled unless the workflow is deliberately being changed and the external weather-data path has been tested separately.

The Morris and uncertainty workflows deliberately use fresh execution directories and do not reuse results from earlier versions. If the selected execution directory already contains files, the workflow stops rather than mixing old and new runs.

Before starting a long ensemble:

1. render the five-year workflow once;
2. confirm the baseline outputs and variable inventory;
3. run the preflight section;
4. inspect the preflight audit;
5. only then allow the full Morris or LHS ensemble to proceed.

## Workflow 1: five-year rotation report

### Scope

The five-year workflow runs the complete 2020–2024 rotation with:

- Wheat, cultivar `Hartog`;
- Maize, cultivar `B_110`;
- Soybean, cultivar `Generic_MG1`;
- a six-layer demonstration soil profile;
- crop-specific sowing, fertilisation, tillage, and irrigation rules.

### Main outputs

The report includes:

- rotation and cultivar tables;
- requested management;
- requested and applied fertilisation;
- requested and applied irrigation;
- sowing and harvest evidence;
- annual yield evidence;
- annual and layered Soil C evidence;
- initial soil-water profile;
- previews of daily, control, harvest, crop-summary, and time-series tables;
- an inventory of exported variables;
- variable-coverage checks;
- `Spacing.Tool2` diagnostics.

Requested management and applied APSIMx events are reported separately. A requested sowing window or fertilisation event is not automatically treated as proof that APSIMx applied the event.

### Fixture mode

The five-year Rmd can build and save a canonical fixture. Once the fixture exists, use the report's non-execution mode to regenerate presentation tables and figures without rerunning APSIMx. Keep the Rmd and its matching fixture together.

## Workflow 2: Morris sensitivity analysis

### Responses

The Morris workflow uses six crop-specific responses:

- Wheat yield;
- Maize yield;
- Soybean yield;
- `Wheat_Biomass_kgha`;
- `Maize_Biomass_kgha`;
- `Soybean_Biomass_kgha`.

The biomass columns are matched explicitly to their crops. They must not be replaced by a first matching or generic biomass column.

### Target

The six responses are converted to deviations from a common baseline and combined with an equally weighted root-mean-square target. The target measures the magnitude of joint change. It does **not** indicate whether a change is agronomically beneficial, because positive and negative deviations both increase the score.

### Preflight

Each candidate parameter is tested individually before the Morris design is constructed. A factor is accepted only if:

- the APSIMx override path is valid;
- `Spacing.Tool2` completes successfully;
- all three yields are available;
- all three crop-specific biomass values are available;
- the multivariate target is finite.

Inspect:

```text
parameter_preflight_audit.csv
parameter_preflight_rejected.csv
```

Only accepted parameters enter the final design.

### Interpretation

The principal Morris indices are:

- `mu`: signed mean elementary effect;
- `mu_star`: mean absolute elementary effect and primary screening measure;
- `sigma`: variation of elementary effects, indicating non-linearity, interactions, or state dependence.

The ranking is global for the combined yield-biomass target. It is not a percentage decomposition of variance and does not provide separate parameter rankings for each crop response.

## Workflow 3: environmental input uncertainty

### Scope

This workflow varies soil and management input variables using a constrained Latin Hypercube Sampling design. Cultivar parameters are not varied.

The default design contains 500 scenarios and 16 uncertain inputs covering:

- initial organic-carbon stock and vertical gradient;
- bulk density;
- `LL15`, `DUL`, and `SAT` relationships;
- initial soil water;
- initial nitrate and ammonium;
- pH;
- mineral, organic, and split nitrogen applications;
- irrigation threshold, target, and maximum event depth.

Hydraulic values are reconstructed so that:

```text
LL15 < DUL < SAT
```

Initial water is constrained between `LL15` and `DUL`.

### Environmental outputs

The four outputs are analysed separately:

1. final `SoilCtot_kgCha` on the last simulated date;
2. cumulative `N2O_total_kgNha` over the five-year rotation;
3. cumulative `CO2C_total_kgCha` over the five-year rotation;
4. cumulative `LeachNO3_kgNha` over the five-year rotation.

`N2O_total_kgNha` is N contained in N2O, not total N2O mass and not CO2 equivalent. `CO2C_total_kgCha` is C contained in CO2. `LeachNO3_kgNha` is nitrate-N leaving the simulated soil profile; it is not a direct estimate of nitrate concentration or mass reaching the groundwater table.

### Interpretation

For each output, the workflow produces:

- descriptive uncertainty statistics;
- 5th, 25th, 50th, 75th, and 95th percentiles;
- Partial Rank Correlation Coefficients, PRCC;
- bootstrap 95% intervals for PRCC;
- standardised regression coefficients;
- a separate input-importance ranking.

A positive PRCC indicates that the output tends to increase with the input after controlling for the other sampled inputs. A negative PRCC indicates a decreasing tendency. An interval crossing zero identifies an effect that is weak or unresolved in the current 500-scenario ensemble.

## Output directories and disk use

The workflows create separate execution and analysis directories. The execution tree may contain one APSIMx DataStore per run and can become large, especially for the 500-scenario uncertainty analysis.

Before execution, confirm:

- sufficient free disk space;
- write permissions;
- a short enough Windows path;
- that the execution directory is empty;
- that the computer will remain available for the full run.

Retain, at minimum:

- the exact Rmd version;
- baseline diagnostics;
- preflight audit;
- run inventory;
- final indices or rankings;
- canonical fixture, where produced;
- package and `Spacing.Tool2` version information.

Temporary per-run folders can be archived separately after the analysis is complete and verified.

## Reproducibility and limitations

- The default soil profile is a synthetic demonstration profile, not an observed site calibration.
- Mock weather is used by default.
- Uniform input ranges in the uncertainty workflow are assumptions, not measured probability distributions.
- Results are conditional on the selected parameter ranges, input ranges, cultivar definitions, management, weather mode, and five-year rotation.
- Morris indices are screening statistics, not variance fractions.
- PRCC describes monotonic conditional association within the sampled domain and does not establish causality outside that domain.
- Do not compare results produced by different script versions without checking changes to the baseline, factors, ranges, outputs, and execution engine.

## Recommended archival record

For every completed analysis, record:

```text
Rmd file name and version
rSpacing package version
Spacing.Tool2 version or source revision
.NET version
R version and package versions
execution date
mock or real weather mode
baseline diagnostics
accepted and rejected preflight factors
number of requested, completed, and failed runs
paths to the final fixture and result tables
```

Use `sessionInfo()` at the end of an analysis session to preserve the R environment:

```r
sessionInfo()
```

## Support and troubleshooting

When reporting a failure, include the smallest relevant set of files:

1. the exact Rmd version;
2. the baseline or preflight audit;
3. the affected row from the run inventory;
4. the corresponding `stderr_path` log;
5. the request JSON for the failed run;
6. `sessionInfo()` and `dotnet --info` output.

Do not send all run directories unless the problem cannot be isolated from the audits and logs.
