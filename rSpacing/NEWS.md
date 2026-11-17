# rSpacing 0.10.05

- Added `plot_spacing_atlas()` for automatic daily, Report, and soil plot collections.
- Added `plot_spacing_yield()` for crop/cultivar yield summaries.
- Added `plot_spacing_irrigation()` and `plot_spacing_fertilisation()` for applied-management outputs.
- Added `plot_spacing_soil_carbon()` for stock, change, percentage change, profile, and time-depth views.
- Added `plot_spacing_comparison()` for comparing variables across runs.
- No C# source or execution API was changed. The stable C# reference remains Spacing.Tool2 0.09.05.

## Packaging and test corrections

- Serialise parameter catalogues as row-oriented JSON records and restore scalar or null values robustly.
- Restore the optional `run_spec` argument in `build_spacing_args()` and validate the JSON before adding `--run-spec`.
- Preserve the long Windows path spelling returned by `normalizePath()` instead of allowing `fs::path_abs()` to emit an equivalent 8.3 path.
- Replace deprecated `ggplot2::aes_string()` calls with tidy-evaluation mappings.
- Remove row-name recycling warnings from layered soil-profile plots.


# rSpacing 0.10.04

- Corrected `collect_spacing()` for CSV data frames with zero rows.
- Provenance columns now use `rep(value, nrow(table))`, preserving empty tables.
- CSV schemas from different runs are aligned before row binding; missing fields
  are filled with `NA`.
- Added regression tests for empty tables, populated tables, non-coincident tables,
  differing columns, and empty input.
- Updated the complete real-model-run Rmd and removed its local workaround.
- No C# source or dependency was changed.

# rSpacing 0.10.03

- Corrected `read_spacing_run_spec()` so `ParameterCatalogue` is reconstructed
  correctly from both row-oriented and column-oriented JSON structures.
- Added one-row and two-row catalogue round-trip regression tests, including the
  catalogue used by the complete workflow Rmd.
- Updated the complete workflow to revision 04 and package version 0.10.03.
- No C# source or dependency was changed.

# rSpacing 0.10.02

- Corrected `prepare_spacing_config_set()` so every required property from the
  base `spacing_config_values` object is retained when a scenario overrides
  only selected fields such as `MaxParallelism`.
- Added a regression test matching the complete workflow Rmd.
- Updated the complete workflow Rmd to 0.10.02.
- No C# source or dependency was changed.

# rSpacing 0.10.01

- Corrected `new_spacing_rotation_spec()`: the formal argument `repeat`, which is
  a reserved R keyword and prevented package documentation/install, is now
  `repeat_rotation`.
- Updated the shipped manual page and complete workflow Rmd accordingly.
- No C# source or package dependency was changed.

# rSpacing 0.10

- Consolidated release based on the complete rSpacing 0.08 API.
- Preserves all v0.08 source files, exports, documentation, and tests.
- Imports the validated run-specification model from experimental v0.09.
- Adds `spacing_irrigation()` and `spacing_apply_irrigation()`.
- Adds structured `UreaN` fertilisation functions.
- Adds verified Wheat/Hartog, Maize/B_110, and Soybean/Generic_MG1 catalogue entries.
- Includes the complete five-year APSIMx workflow example under `inst/examples`.
- Morris integration is deliberately deferred until the consolidated package passes the base API checks.
- Does not modify or bundle the C# solution.

# Historical note

rSpacing 0.09 was an experimental reduced API branch. It is not used as the
structural base of this release.

## Vignette

- Added `vignettes/rSpacing-workflow.Rmd`, an executable manual covering the primary and advanced APIs.
- Result sections use a verified APSIMx fixture or an explicitly requested live run.
- Added `inst/scripts/build_reference_fixture.R` to generate the real reference fixture on the verified Windows workstation.

## Vignette weather correction

- The weather subsetting example now reconstructs dates from APSIM `year` and day-of-year columns instead of referring to a non-existent `data$date` field.
