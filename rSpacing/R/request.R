#' Create one Spacing.Tool2 simulation request
#'
#' A request contains the scientific inputs which map directly to Tool2 command
#' line arguments. Construction validates coordinates and referenced files.
#'
#' @param latitude,longitude Numeric scalars in decimal degrees.
#' @param rotation Character scalar identifying a crop or rotation.
#' @param mock Logical. Add the `--mock` flag.
#' @param met_file Optional existing APSIM `.met` file.
#' @param calibrate Logical. Add the `--calibrate` flag.
#' @param observations Optional existing observations CSV. Required when
#'   `calibrate = TRUE`.
#' @param yield_weight,lai_weight Optional numeric objective weights.
#' @param simplexes Optional integer number of optimisation simplexes.
#' @param scenario_id Optional stable scenario identifier.
#' @param extra_args Character vector of additional Tool2 arguments.
#' @return An S3 object of class `spacing_request`.
#' @export
#' @examples
#' req <- new_spacing_request(45.4, 9.2, "Maize", mock = TRUE)
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

#' Validate a simulation request
#' @param x A `spacing_request` object.
#' @return `x`, invisibly. Errors have class `spacing_validation_error`.
#' @export
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

#' @export
print.spacing_request <- function(x, ...) {
  cat("<spacing_request>\n", sprintf("  location: %.6f, %.6f\n", x$latitude, x$longitude),
      "  rotation: ", x$rotation, "\n", "  mock:     ", x$mock, "\n", sep="")
  invisible(x)
}
