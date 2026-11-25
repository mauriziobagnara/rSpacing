# rSpacing

An R interface to **Spacing.Tool2**, the command-line runner of APSIM Next Generation for the SPACING project. With rSpacing you describe a site, a soil and a crop rotation with its management in R, run it with Tool2, and read, summarise and plot the APSIM outputs.

**Version:** 0.10.09 (`packageVersion("rSpacing")` shows `0.10.9`). Model runs need **Spacing.Tool2 >= 0.09.10** and .NET 8.

## Install

From the root of the source package, build and install the tarball with the vignette:

```r
install.packages(c("devtools", "knitr", "rmarkdown"))
tarball <- devtools::build(pkg = ".", vignettes = TRUE, manual = FALSE)
install.packages(tarball, repos = NULL, type = "source")
```

Restart R, then check:

```r
library(rSpacing)
packageVersion("rSpacing")                          # 0.10.9
vignette("rSpacing-workflow_EN", package = "rSpacing")
```

If the vignette does not appear, run `tools::buildVignettes(dir = ".")` from the package root to see the error.

## Quick start: a scenario run

A scenario is a site, a period, an optional soil profile and an ordered list of crops, each with its own management. Paths below are relative to a workspace that holds the C# solution in `apsim/` (see the workspace `README.md`).

```r
library(rSpacing)

crops <- list(
  spacing_crop("Soybean", "Generic_MG1", c("1-may", "15-jun"), 35, 35, 450,
               tillage = spacing_tillage("no-till")),
  spacing_crop("Wheat", "Claire", c("10-oct", "20-nov"), 180, 35, 180,
               fertilisation = spacing_fertilisation(30, list(
                 spacing_fertiliser_application(70, trigger = "Stage", stage = "MaximumSpikeletPrimordia"),
                 spacing_fertiliser_application(50, trigger = "Stage", stage = "FlagLeafAppearance"))),
               tillage = spacing_tillage("minimum"), organic = spacing_organic_n(20)),
  spacing_crop("Maize", "B_130", c("1-apr", "10-may"), 7.5, 50, 750,
               fertilisation = spacing_fertilisation(50, list(
                 spacing_fertiliser_application(100, trigger = "DaysAfterSowing", days_after_sowing = 35))),
               irrigation = spacing_irrigation(TRUE, 0.50, 0.85, 35, "1-jun", "31-aug")))

scenario <- new_spacing_scenario("my_rotation", crops, latitude = 44.90, longitude = 10.63,
                                 start_year = 2020, end_year = 2024, cycles = 2)

cfg <- spacing_config(apsim_dir = "apsim", use_compiled = TRUE,
                      executable = "apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll")
run <- run_spacing(cfg, scenario)
result <- read_spacing(run)

run$result$yields                 # mean yield by crop (kg/ha)
spacing_crop_summary(result)      # one row per crop: sowing, harvest, yield, soil C, N2O, leaching
plot_spacing_yield(result)
```

`write_spacing_scenario()` saves the request JSON, which can also be given to Tool2 directly with `--request`.

## What the package covers

| Area | Main functions |
|---|---|
| Crops and management | `spacing_crop()`, `spacing_fertilisation()`, `spacing_fertiliser_application()`, `spacing_irrigation()`, `spacing_irrigation_event()`, `spacing_tillage()`, `spacing_tillage_operation()`, `spacing_organic_n()`, `spacing_organic_application()`, `spacing_sowing_rule()` |
| Rotations | `new_spacing_scenario()` (repeated crops, `cycles`), `spacing_termination()` (cover crops), `spacing_intercrop()`, `spacing_perennial()`, `spacing_residue_removal()`, `spacing_surface_residue()` |
| Soil | `spacing_soil_profile()`, `as_spacing_soil_profile()` |
| APSIM names | `spacing_cultivars()`, `spacing_crop_stages()`, `spacing_fertiliser_types()`, `spacing_residue_types()` |
| Running Tool2 | `spacing_config()`, `run_spacing()`, `write_spacing_scenario()`, `new_spacing_request()` (single-crop requests) |
| Outputs | `read_spacing()`, `spacing_daily()`, `spacing_report()`, `spacing_crop_summary()`, `spacing_units()`, `summary()`, `spacing_variable_dictionary()`, `spacing_report_spec()` (extra output variables) |
| Plots | `plot_spacing_yield()`, `plot_spacing_daily_panel()`, `plot_spacing_report_panel()`, `plot_spacing_soil_profile()`, `plot_spacing_soil_carbon()`, `plot_spacing_fertilisation()`, `plot_spacing_irrigation()`, `plot_spacing_comparison()`, `plot_spacing_map()`, `plot_spacing_met()` |
| Weather files | `read_spacing_met()`, `new_spacing_met()`, `prepare_spacing_met()`, `set_spacing_met_column()`, `write_spacing_met()` |
| Run specifications | `new_spacing_run_spec()`, `spacing_parameter_catalogue()`, `write_spacing_run_spec()`, `read_spacing_run_spec()`, `update_spacing_parameters()` |

Every function has a help page with runnable examples (`?spacing_crop`). The help index groups them by family.

## Learning material

- **Cookbook** (`inst/rmarkdown/rSpacing_cookbook_v1.00_EN.Rmd`): every scenario option, run with Tool2 and checked automatically. Start here.
- **Vignette** `rSpacing-workflow_EN`: reading, summarising and plotting results from a bundled reference run, with no model run needed.
- **Examples** in `inst/examples/`: `scenario_tool2_request.R`, `scenario_management_events.R`, `scenario_rotation_options.R`, `calibration_run_spec.R`.
- **Workflows** in `inst/rmarkdown/`: operational manual, five-year rotation report, scenario comparison, Morris sensitivity and input-uncertainty analyses. See `inst/rmarkdown/README.md`. Copy them to a writable folder before rendering.

## Development checks

```r
roxygen2::roxygenise()
testthat::test_local()
rcmdcheck::rcmdcheck(args = "--no-manual")
```

Unit tests need neither APSIM nor .NET. The integration tests run Tool2 and are skipped unless these variables are set:

```r
Sys.setenv(RSPACING_INTEGRATION = "true",
           RSPACING_APSIM_DIR   = "<workspace>/apsim",
           RSPACING_TOOL2_DLL   = "<workspace>/apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll")
testthat::test_local()
```

Changes by version are listed in `NEWS.md`.
