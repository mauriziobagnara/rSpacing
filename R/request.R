#' Create a simple Spacing.Tool2 simulation request
#'
#' A request describes a run through Tool2 command-line arguments: a location,
#' a rotation string (e.g. `"Maize"` or `"Wheat,Maize"`), mock or file weather
#' and optional calibration settings. Management, soil and cultivars are then
#' the Tool2 defaults. For full control of the rotation, management and soil
#' use [new_spacing_scenario()] instead.
#'
#' Referenced files are checked at construction and stored as absolute paths.
#'
#' @param latitude,longitude Numeric scalars. Coordinates in decimal degrees
#'   (latitude in `[-90, 90]`, longitude in `[-180, 180]`).
#' @param rotation Character scalar. Crop name or comma-separated crop
#'   sequence passed to Tool2 `--rotation`.
#' @param mock Logical. Add `--mock`: Tool2 uses synthetic weather and soil
#'   instead of the database API.
#' @param met_file Optional path of an existing APSIM `.met` weather file
#'   (`--met-file`).
#' @param calibrate Logical. Add `--calibrate` to run the Tool2 calibration
#'   before the simulation.
#' @param observations Optional path of an existing observations CSV
#'   (`--observations`). Required when `calibrate = TRUE`.
#' @param yield_weight,lai_weight Optional numeric weights of the yield and LAI
#'   terms of the calibration objective (`--yield-weight`, `--lai-weight`).
#' @param simplexes Optional integer. Number of multi-start simplexes of the
#'   calibration (`--simplexes`).
#' @param scenario_id Optional character identifier stored with the run.
#'   Default `"scenario-<timestamp>"`.
#' @param extra_args Character vector of additional Tool2 arguments appended
#'   unchanged.
#' @return An object of class `spacing_request`: a list with the arguments
#'   above (files as absolute paths, logical flags as `TRUE`/`FALSE`).
#' @seealso [run_spacing()], [new_spacing_scenario()] for complete scenarios.
#' @family running Tool2
#' @export
#' @examples
#' req <- new_spacing_request(45.4, 9.2, "Maize", mock = TRUE)
#' req
#' new_spacing_request(44.9, 10.63, "Wheat,Maize", mock = TRUE, scenario_id = "demo")$rotation
new_spacing_request <- function(latitude, longitude, rotation, mock = FALSE,
                                met_file = NULL, calibrate = FALSE,
                                observations = NULL, yield_weight = NULL,
                                lai_weight = NULL, simplexes = NULL,
                                scenario_id = NULL, extra_args = character()) {
  if (!is.null(met_file)) met_file <- .normalise_existing(met_file, "met_file", "file")
  if (!is.null(observations)) observations <- .normalise_existing(observations, "observations", "file")
  x <- structure(list(latitude=latitude, longitude=longitude, rotation=rotation,
                      mock=isTRUE(mock), met_file=met_file,
                      calibrate=isTRUE(calibrate), observations=observations,
                      yield_weight=yield_weight, lai_weight=lai_weight,
                      simplexes=simplexes,
                      scenario_id=scenario_id %||% paste0("scenario-", format(Sys.time(), "%Y%m%d-%H%M%S")),
                      extra_args=as.character(extra_args)), class="spacing_request")
  validate_spacing_request(x)
  x
}

#' Validate a simple simulation request
#'
#' Checks coordinates, the rotation string and that observations are given
#' when calibration is requested.
#'
#' @param x A `spacing_request` object from [new_spacing_request()].
#' @return `x`, invisibly. Problems raise errors of class
#'   `spacing_validation_error`.
#' @family running Tool2
#' @export
#' @examples
#' validate_spacing_request(new_spacing_request(45.4, 9.2, "Maize", mock = TRUE))
validate_spacing_request <- function(x) {
  if (!inherits(x, "spacing_request")) .rspacing_abort("x is not a spacing_request.", "spacing_validation_error")
  if (!is.numeric(x$latitude) || length(x$latitude)!=1L || !is.finite(x$latitude) || x$latitude < -90 || x$latitude > 90)
    .rspacing_abort("latitude must be finite and between -90 and 90.", "spacing_validation_error")
  if (!is.numeric(x$longitude) || length(x$longitude)!=1L || !is.finite(x$longitude) || x$longitude < -180 || x$longitude > 180)
    .rspacing_abort("longitude must be finite and between -180 and 180.", "spacing_validation_error")
  if (!is.character(x$rotation) || length(x$rotation)!=1L || !nzchar(x$rotation))
    .rspacing_abort("rotation must be one non-empty string.", "spacing_validation_error")
  if (x$calibrate && is.null(x$observations))
    .rspacing_abort("observations is required when calibrate=TRUE.", "spacing_validation_error")
  invisible(x)
}

#' @describeIn new_spacing_request Print location, rotation and the mock flag.
#' @param x A `spacing_request` object.
#' @param ... Ignored.
#' @export
print.spacing_request <- function(x, ...) {
  cat("<spacing_request>\n", sprintf("  location: %.6f, %.6f\n", x$latitude, x$longitude),
      "  rotation: ", x$rotation, "\n", "  mock:     ", x$mock, "\n", sep="")
  invisible(x)
}
