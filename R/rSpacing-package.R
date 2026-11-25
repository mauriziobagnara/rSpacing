#' rSpacing: an R interface to Spacing.Tool2 and APSIM Next Generation
#'
#' `rSpacing` provides a validated, reproducible interface between R and the
#' custom C# `Spacing.Tool2` workflow. It builds command-line requests, executes
#' the external process, records provenance, discovers generated files, and
#' imports APSIM SQLite and CSV outputs.
#'
#' The package deliberately keeps model construction and simulation in C# and
#' APSIMx. R owns orchestration, validation, provenance, result collection, and
#' downstream analysis.
#'
#' @section Main workflow:
#' 1. Create a project configuration with [spacing_config()] (or write one
#'    with [prepare_spacing_runtime()]).
#' 2. Describe the simulation, either as a simple request
#'    ([new_spacing_request()]) or as a complete scenario
#'    ([new_spacing_scenario()]) built from [spacing_crop()] and
#'    [spacing_soil_profile()].
#' 3. Execute it with [run_spacing()].
#' 4. Import outputs with [read_spacing()], or read the Tool2 CSV files named
#'    in `run$result` for scenarios.
#' 5. Inspect and plot outputs with [inventory_spacing_result()],
#'    [summary.spacing_result()] and [plot_spacing_result()].
#' 6. Map simulation coordinates with [plot_spacing_location()] or [plot_spacing_map()].
#' 7. Combine several results with [collect_spacing()].
#'
#' @section Crop management in scenarios:
#' - Fertilisation: [spacing_fertilisation()], [spacing_fertiliser_application()].
#' - Irrigation: [spacing_irrigation()] (automatic), [spacing_irrigation_event()] (scheduled).
#' - Tillage and organic inputs: [spacing_tillage()], [spacing_tillage_operation()],
#'   [spacing_organic_n()], [spacing_organic_application()].
#' - Sowing: [spacing_sowing_rule()] or a fixed `sowing_date` in [spacing_crop()].
#' - Crop types: [spacing_intercrop()], [spacing_termination()] (cover crops),
#'   [spacing_perennial()]; residues: [spacing_residue_removal()],
#'   [spacing_surface_residue()].
#' - Outputs: [spacing_report_spec()].
#' - APSIM names: [spacing_cultivars()], [spacing_crop_stages()],
#'   [spacing_fertiliser_types()], [spacing_residue_types()].
#'
#' @section Weather and run specifications:
#' APSIM `.met` files are built and edited with [new_spacing_met()],
#' [read_spacing_met()] and [write_spacing_met()]. Run specifications for
#' calibration and sensitivity workflows are built with
#' [new_spacing_run_spec()].
#'
#' @section External software:
#' A real model run requires the custom `Spacing.Tool2` project and either the
#' `dotnet` executable or a compiled Tool2 executable (a framework-dependent
#' `spacing-tool2.dll` needs only the .NET runtime). Scenario features need
#' Spacing.Tool2 >= 0.09.07; management events >= 0.09.09; rotation options
#' (repeated crops, sowing rules, scheduled irrigation, cover crops,
#' intercrops, perennials, residue removal, report variables) >= 0.09.10.
#' Unit tests do not require APSIMx or .NET.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats setNames
#' @importFrom utils head tail
NULL

# Column names used inside ggplot2::aes() (non-standard evaluation).
utils::globalVariables(c(
  ".colour_group", "DOY", "Year", "anomaly", "depth_bottom_mm", "depth_mid_mm",
  "depth_mm", "depth_top_mm", "direction", "group", "high", "irrigation_mm",
  "lat", "latitude", "long", "longitude", "low", "metric", "nitrogen_kg_ha",
  "scenario_id", "series", "thickness_mm", "value", "yield_kg_ha"
))
