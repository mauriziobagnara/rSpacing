# APSIM definitions used to validate scenarios in R before Spacing.Tool2 runs.
# The tables in inst/extdata/apsim were exported from the resources embedded in
# the APSIM Models.dll shipped with Spacing.Tool2 0.09.09 (ApsimResources class);
# Tool2 repeats every check against the APSIM version it actually runs.

#' Read one of the bundled APSIM tables
#' @param name Character scalar. Table name: `"fert_types"`,
#'   `"residue_types"`, `"stages"` or `"cultivars"` (file
#'   `inst/extdata/apsim/<name>.csv`).
#' @return A data frame.
#' @noRd
.spacing_apsim_table <- function(name) {
  path <- system.file("extdata", "apsim", paste0(name, ".csv"), package = "rSpacing")
  if (!nzchar(path)) stop("APSIM table `", name, "` not found in rSpacing.", call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' APSIM fertiliser types usable for an N dose
#'
#' Fertiliser types of the APSIM `Fertiliser` resource that contain mineral N,
#' with the N fraction of the product (NO3 + NH4 + urea). Doses in rSpacing are
#' always kg N/ha; Tool2 (>= 0.09.09) applies `dose / n_fraction` kg/ha of
#' product. Types without N (e.g. `RockP`, `CalciteCA`) are not listed and are
#' rejected.
#'
#' The table was exported from the APSIM `Models.dll` of Spacing.Tool2 0.09.10;
#' Tool2 repeats the check against the APSIM version it runs.
#'
#' @return A data frame with columns `fertiliser` (APSIM name) and
#'   `n_fraction` (kg N per kg of product).
#' @family APSIM names
#' @export
#' @examples
#' spacing_fertiliser_types()
spacing_fertiliser_types <- function() .spacing_apsim_table("fert_types")

#' APSIM surface residue types
#'
#' Residue types of the APSIM `SurfaceOrganicMatter` resource, with the carbon
#' fraction of the fresh mass. rSpacing and Tool2 convert an organic N dose
#' into applied mass as `mass = N * C:N / carbon_fraction`; e.g. `manure` and
#' `fym` 0.08, `cm` (chicken manure) 0.277, crop residues 0.4.
#'
#' @return A data frame with columns `residue_type` (APSIM name) and
#'   `carbon_fraction`.
#' @family APSIM names
#' @export
#' @examples
#' head(spacing_residue_types())
#' subset(spacing_residue_types(), carbon_fraction < 0.4)
spacing_residue_types <- function() .spacing_apsim_table("residue_types")

#' APSIM phenological stage names
#'
#' Stage names usable for `"Stage"` triggers in
#' [spacing_fertiliser_application()], [spacing_irrigation_event()] and
#' [spacing_termination()]: the start stages of the phenological phases of the
#' APSIM crop model, in phenological order. A stage-triggered operation
#' happens on the first day after the crop has reached the stage.
#'
#' @param crop Optional APSIM crop model name (case-insensitive); `NULL`
#'   returns all crops. A crop without a table returns zero rows.
#' @return A data frame with columns `crop` and `stage`.
#' @family APSIM names
#' @export
#' @examples
#' spacing_crop_stages("Wheat")
#' spacing_crop_stages("Maize")$stage
spacing_crop_stages <- function(crop = NULL) {
  x <- .spacing_apsim_table("stages")
  if (!is.null(crop)) x <- x[tolower(x$crop) == tolower(crop), , drop = FALSE]
  rownames(x) <- NULL
  x
}

#' Validate a residue type against the APSIM table
#' @param residue_type Character scalar (case-insensitive).
#' @param argument Character scalar. Argument name for the error message.
#' @return The type as spelled in APSIM.
#' @noRd
.spacing_residue_type <- function(residue_type, argument = "residue_type") {
  residue_type <- .spacing_optional_text(residue_type, argument)
  types <- spacing_residue_types()$residue_type
  hit <- types[tolower(types) == tolower(residue_type %||% "")]
  if (!length(hit))
    stop("`", argument, "` = \"", residue_type, "\" is not an APSIM residue type; ",
         "see spacing_residue_types().", call. = FALSE)
  hit[[1]]
}

#' Validate a stage name for a crop
#' @param crop APSIM crop model name.
#' @param stage Stage name (case-insensitive).
#' @return The stage as spelled in APSIM; `stage` unchanged when the crop has
#'   no stage table (Tool2 then validates it).
#' @noRd
.spacing_check_stage <- function(crop, stage) {
  known <- spacing_crop_stages(crop)
  if (!nrow(known)) return(stage)   # crop not tabulated: Tool2 validates it
  hit <- known$stage[tolower(known$stage) == tolower(stage)]
  if (!length(hit))
    stop(crop, ": stage \"", stage, "\" is not an APSIM stage of this crop. Available: ",
         paste(known$stage, collapse = ", "), ".", call. = FALSE)
  hit[[1]]
}
