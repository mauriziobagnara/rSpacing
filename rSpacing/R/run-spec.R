# Helpers for data-driven APSIM run specifications.

.assert_scalar_character <- function(x, name, allow_empty = FALSE) {
  ok <- is.character(x) && length(x) == 1L && !is.na(x)
  if (ok && !allow_empty) ok <- nzchar(x)
  if (!ok) .rspacing_abort(sprintf("'%s' must be one %scharacter value.", name,
                                   if (allow_empty) "" else "non-empty "),
                            "spacing_validation_error")
  invisible(x)
}

.assert_numeric_vector <- function(x, name, n = NULL, finite = TRUE,
                                   lower = -Inf, upper = Inf) {
  ok <- is.numeric(x) && length(x) > 0L
  if (ok && !is.null(n)) ok <- length(x) == n
  if (ok && finite) ok <- all(is.finite(x))
  if (ok) ok <- all(x >= lower & x <= upper)
  if (!ok) {
    suffix <- if (is.null(n)) "" else sprintf(" of length %d", n)
    .rspacing_abort(sprintf("'%s' must be a valid numeric vector%s.", name, suffix),
                    "spacing_validation_error")
  }
  invisible(x)
}

.named_list <- function(x, name) {
  if (is.null(x) || length(x) == 0L) return(list())
  if (is.atomic(x) && !is.null(names(x))) x <- as.list(x)
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x))))
    .rspacing_abort(sprintf("'%s' must be a named list or named atomic vector.", name),
                    "spacing_validation_error")
  x
}

#' Create a layered APSIM soil specification
#'
#' Builds a data-only soil profile which can be serialised in a run
#' specification and translated by `Spacing.Tool2` into APSIM `Physical`,
#' `Water`, `Organic`, `Chemical`, and solute nodes. No C# recompilation is
#' required when only these values change.
#'
#' @param profile_id Stable soil-profile identifier.
#' @param thickness,bd,air_dry,ll15,dul,sat,ks Layered physical and hydraulic
#'   values. Thickness is in mm and bulk density in g/cm3.
#' @param carbon,soil_cn_ratio,ph Layered organic-carbon and chemical values.
#' @param no3,nh4,urea Initial mineral-N values by layer, in the units expected
#'   by the receiving `Spacing.Tool2` schema.
#' @param initial_water Optional layered initial water values.
#' @param crop_parameters Optional named list by crop. Each crop entry may
#'   contain layered `LL`, `KL`, and `XF` vectors.
#' @param metadata Optional named list containing source, licence, date,
#'   analytical method, or other provenance fields.
#' @return An S3 object of class `spacing_soil_spec`.
#' @export
new_spacing_soil_spec <- function(profile_id, thickness, bd, air_dry, ll15,
                                  dul, sat, ks, carbon, soil_cn_ratio, ph,
                                  no3, nh4, urea = rep(0, length(thickness)),
                                  initial_water = NULL,
                                  crop_parameters = list(), metadata = list()) {
  .assert_scalar_character(profile_id, "profile_id")
  n <- length(thickness)
  .assert_numeric_vector(thickness, "thickness", lower = .Machine$double.eps)
  for (nm in c("bd", "air_dry", "ll15", "dul", "sat", "ks", "carbon",
               "soil_cn_ratio", "ph", "no3", "nh4", "urea")) {
    .assert_numeric_vector(get(nm), nm, n = n)
  }
  if (!is.null(initial_water)) .assert_numeric_vector(initial_water, "initial_water", n = n)
  if (any(air_dry > ll15 | ll15 > dul | dul > sat))
    .rspacing_abort("Soil water limits must satisfy AirDry <= LL15 <= DUL <= SAT in every layer.",
                    "spacing_validation_error")
  if (any(bd <= 0) || any(ks < 0) || any(carbon < 0) || any(no3 < 0) ||
      any(nh4 < 0) || any(urea < 0) || any(ph <= 0 | ph >= 14))
    .rspacing_abort("Soil values contain physically invalid negative or out-of-range values.",
                    "spacing_validation_error")
  crop_parameters <- .named_list(crop_parameters, "crop_parameters")
  crop_parameters <- lapply(names(crop_parameters), function(crop) {
    cp <- crop_parameters[[crop]]
    if (!is.list(cp)) .rspacing_abort(sprintf("Crop parameters for '%s' must be a list.", crop),
                                      "spacing_validation_error")
    for (nm in intersect(c("LL", "KL", "XF"), names(cp)))
      .assert_numeric_vector(cp[[nm]], sprintf("crop_parameters[['%s']]$%s", crop, nm), n = n)
    if (!all(c("LL", "KL", "XF") %in% names(cp)))
      .rspacing_abort(sprintf("Crop parameters for '%s' must contain LL, KL, and XF.", crop),
                      "spacing_validation_error")
    if (any(cp$KL < 0) || any(cp$XF < 0 | cp$XF > 1) || any(cp$LL < air_dry | cp$LL > dul))
      .rspacing_abort(sprintf("Invalid LL, KL, or XF values for crop '%s'.", crop),
                      "spacing_validation_error")
    cp
  }) |> stats::setNames(names(crop_parameters))
  metadata <- .named_list(metadata, "metadata")
  structure(list(
    ProfileId = profile_id, Thickness = as.numeric(thickness), BD = as.numeric(bd),
    AirDry = as.numeric(air_dry), LL15 = as.numeric(ll15), DUL = as.numeric(dul),
    SAT = as.numeric(sat), KS = as.numeric(ks), Carbon = as.numeric(carbon),
    SoilCNRatio = as.numeric(soil_cn_ratio), PH = as.numeric(ph),
    NO3 = as.numeric(no3), NH4 = as.numeric(nh4), Urea = as.numeric(urea),
    InitialWater = if (is.null(initial_water)) NULL else as.numeric(initial_water),
    CropParameters = crop_parameters, Metadata = metadata
  ), class = c("spacing_soil_spec", "list"))
}

#' Create an APSIM cultivar override specification
#'
#' Cultivar parameters are represented as named APSIM paths and values. The C#
#' builder can translate them into `Models.PMF.Cultivar.Command` entries at
#' runtime, avoiding changes to crop resource files and recompilation.
#'
#' @param crop Crop model name, for example `"Maize"`.
#' @param name Cultivar name passed to the APSIM sowing operation.
#' @param parameters Named list or vector. Names are approved APSIM property
#'   paths or parameter identifiers resolved through a parameter catalogue.
#' @param aliases Optional character vector of alternative cultivar names.
#' @param metadata Optional named provenance list.
#' @return An S3 object of class `spacing_cultivar_spec`.
#' @export
new_spacing_cultivar_spec <- function(crop, name, parameters,
                                      aliases = character(), metadata = list()) {
  .assert_scalar_character(crop, "crop")
  .assert_scalar_character(name, "name")
  parameters <- .named_list(parameters, "parameters")
  if (!length(parameters)) .rspacing_abort("At least one cultivar parameter is required.",
                                           "spacing_validation_error")
  if (!is.character(aliases) || anyNA(aliases))
    .rspacing_abort("'aliases' must be a character vector.", "spacing_validation_error")
  structure(list(Crop = crop, Name = name, Parameters = parameters,
                 Aliases = unname(aliases), Metadata = .named_list(metadata, "metadata")),
            class = c("spacing_cultivar_spec", "list"))
}

.normalise_rotation_phases <- function(phases) {
  if (is.data.frame(phases)) phases <- split(phases, seq_len(nrow(phases)))
  if (!is.list(phases) || !length(phases))
    .rspacing_abort("'phases' must contain at least one crop phase.", "spacing_validation_error")
  lapply(seq_along(phases), function(i) {
    p <- phases[[i]]
    if (is.data.frame(p)) p <- as.list(p[1L, , drop = FALSE])
    if (!is.list(p)) .rspacing_abort(sprintf("Rotation phase %d must be a list.", i),
                                     "spacing_validation_error")
    required <- c("Crop", "Cultivar")
    missing <- setdiff(required, names(p))
    if (length(missing)) .rspacing_abort(sprintf("Rotation phase %d is missing: %s", i,
                                                 paste(missing, collapse = ", ")),
                                         "spacing_validation_error")
    .assert_scalar_character(p$Crop, sprintf("phases[[%d]]$Crop", i))
    .assert_scalar_character(p$Cultivar, sprintf("phases[[%d]]$Cultivar", i))
    defaults <- list(SowingWindow = NULL, Population = NULL, Depth = NULL,
                     RowSpacing = NULL, HarvestRule = "IsReadyForHarvesting",
                     ResidueFractionRetained = 1)
    p <- utils::modifyList(defaults, p)
    if (!is.null(p$SowingWindow)) {
      if (!is.list(p$SowingWindow) || !all(c("Start", "End") %in% names(p$SowingWindow)))
        .rspacing_abort(sprintf("Rotation phase %d has an invalid SowingWindow.", i),
                        "spacing_validation_error")
    }
    for (nm in c("Population", "Depth", "RowSpacing", "ResidueFractionRetained")) {
      if (!is.null(p[[nm]]) && (!is.numeric(p[[nm]]) || length(p[[nm]]) != 1L || !is.finite(p[[nm]])))
        .rspacing_abort(sprintf("Rotation phase %d has an invalid %s.", i, nm),
                        "spacing_validation_error")
    }
    if (p$ResidueFractionRetained < 0 || p$ResidueFractionRetained > 1)
      .rspacing_abort(sprintf("Rotation phase %d residue fraction must be between 0 and 1.", i),
                      "spacing_validation_error")
    p
  })
}

#' Create a crop-rotation specification
#'
#' @param name Stable rotation name, also suitable for matching the CLI request.
#' @param phases Data frame or list of crop-phase lists. Each phase requires
#'   `Crop` and `Cultivar` and may define `SowingWindow`, `Population`, `Depth`,
#'   `RowSpacing`, `HarvestRule`, and `ResidueFractionRetained`.
#' @param repeat_rotation Logical. Repeat the sequence after its final phase.
#' @param metadata Optional named provenance list.
#' @return An S3 object of class `spacing_rotation_spec`.
#' @export
new_spacing_rotation_spec <- function(name, phases, repeat_rotation = TRUE, metadata = list()) {
  .assert_scalar_character(name, "name")
  structure(list(Name = name, Repeat = isTRUE(repeat_rotation),
                 Phases = .normalise_rotation_phases(phases),
                 Metadata = .named_list(metadata, "metadata")),
            class = c("spacing_rotation_spec", "list"))
}

.validate_management_events <- function(x, type) {
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) x <- split(x, seq_len(nrow(x)))
  if (!is.list(x)) .rspacing_abort(sprintf("'%s' events must be a list or data frame.", type),
                                   "spacing_validation_error")
  lapply(seq_along(x), function(i) {
    e <- x[[i]]
    if (is.data.frame(e)) e <- as.list(e[1L, , drop = FALSE])
    if (!is.list(e)) .rspacing_abort(sprintf("%s event %d must be a list.", type, i),
                                     "spacing_validation_error")
    e
  })
}

#' Create a management specification
#'
#' Stores data-driven fertilisation, irrigation, tillage, and residue events.
#' Manager logic remains compiled and stable; values and schedules vary by run.
#'
#' @param fertilisation,irrigation,tillage,residues Lists or data frames of
#'   management events. Event fields are preserved for validation by the C#
#'   run-spec reader.
#' @param metadata Optional named provenance list.
#' @return An S3 object of class `spacing_management_spec`.
#' @export
new_spacing_management_spec <- function(fertilisation = list(), irrigation = list(),
                                        tillage = list(), residues = list(),
                                        metadata = list()) {
  structure(list(
    Fertilisation = .validate_management_events(fertilisation, "fertilisation"),
    Irrigation = .validate_management_events(irrigation, "irrigation"),
    Tillage = .validate_management_events(tillage, "tillage"),
    Residues = .validate_management_events(residues, "residue"),
    Metadata = .named_list(metadata, "metadata")
  ), class = c("spacing_management_spec", "list"))
}

#' Build and validate a calibration parameter catalogue
#'
#' The catalogue maps stable R-facing parameter identifiers to approved APSIM
#' property paths. It supplies units, bounds, transformations, and the stage at
#' which each override is applied.
#'
#' @param data Optional data frame containing the catalogue.
#' @param parameter_id,crop,scope,apsim_path,type,unit,lower_bound,upper_bound,
#'   transform,application_stage Columns used when `data` is `NULL`.
#' @return A data frame of class `spacing_parameter_catalogue`.
#' @export
spacing_parameter_catalogue <- function(data = NULL, parameter_id = character(),
                                        crop = character(), scope = character(),
                                        apsim_path = character(), type = "double",
                                        unit = NA_character_, lower_bound = -Inf,
                                        upper_bound = Inf, transform = "identity",
                                        application_stage = "cultivar") {
  if (is.null(data)) {
    n <- length(parameter_id)
    recycle <- function(x) if (length(x) == 1L && n != 1L) rep(x, n) else x
    data <- data.frame(parameter_id = parameter_id, crop = recycle(crop),
                       scope = recycle(scope), apsim_path = recycle(apsim_path),
                       type = recycle(type), unit = recycle(unit),
                       lower_bound = recycle(lower_bound), upper_bound = recycle(upper_bound),
                       transform = recycle(transform),
                       application_stage = recycle(application_stage),
                       stringsAsFactors = FALSE)
  }
  required <- c("parameter_id", "crop", "scope", "apsim_path", "type", "unit",
                "lower_bound", "upper_bound", "transform", "application_stage")
  missing <- setdiff(required, names(data))
  if (length(missing)) .rspacing_abort(sprintf("Parameter catalogue is missing: %s",
                                               paste(missing, collapse = ", ")),
                                       "spacing_validation_error")
  if (anyDuplicated(data$parameter_id) || any(!nzchar(data$parameter_id)) ||
      any(!nzchar(data$apsim_path)) || any(data$lower_bound > data$upper_bound))
    .rspacing_abort("Parameter catalogue contains duplicate identifiers, empty paths, or invalid bounds.",
                    "spacing_validation_error")
  class(data) <- c("spacing_parameter_catalogue", "data.frame")
  data
}

#' Validate calibration parameter values against a catalogue
#'
#' @param values Named numeric vector or named list.
#' @param catalogue A `spacing_parameter_catalogue`.
#' @param require_all Logical. Require every catalogue parameter to be supplied.
#' @return A named list of validated values.
#' @export
validate_spacing_parameters <- function(values, catalogue, require_all = FALSE) {
  values <- .named_list(values, "values")
  if (!inherits(catalogue, "spacing_parameter_catalogue"))
    catalogue <- spacing_parameter_catalogue(catalogue)
  unknown <- setdiff(names(values), catalogue$parameter_id)
  if (length(unknown)) .rspacing_abort(sprintf("Unknown parameter identifiers: %s",
                                               paste(unknown, collapse = ", ")),
                                       "spacing_validation_error")
  if (require_all) {
    missing <- setdiff(catalogue$parameter_id, names(values))
    if (length(missing)) .rspacing_abort(sprintf("Missing parameter values: %s",
                                                 paste(missing, collapse = ", ")),
                                         "spacing_validation_error")
  }
  for (id in names(values)) {
    row <- catalogue[catalogue$parameter_id == id, , drop = FALSE]
    value <- values[[id]]
    if (row$type == "double") {
      if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
          value < row$lower_bound || value > row$upper_bound)
        .rspacing_abort(sprintf("Parameter '%s' is outside its allowed numeric bounds.", id),
                        "spacing_validation_error")
    }
  }
  values
}

#' Create a complete data-driven APSIM run specification
#'
#' @param run_id Stable run or calibration-iteration identifier.
#' @param latitude,longitude Decimal-degree coordinates.
#' @param start_year,end_year Simulation years.
#' @param weather_file Existing APSIM `.met` file or `NULL` when the C# workflow
#'   resolves weather from its configured source.
#' @param soil A `spacing_soil_spec`.
#' @param rotation A `spacing_rotation_spec`.
#' @param cultivars List of `spacing_cultivar_spec` objects.
#' @param management A `spacing_management_spec`.
#' @param parameter_values Optional named values validated against `catalogue`.
#' @param catalogue Optional `spacing_parameter_catalogue`.
#' @param reports Optional named report-definition list.
#' @param metadata Optional named provenance list.
#' @return An S3 object of class `spacing_run_spec`.
#' @export
new_spacing_run_spec <- function(run_id, latitude, longitude, start_year, end_year,
                                 weather_file = NULL, soil, rotation,
                                 cultivars = list(),
                                 management = new_spacing_management_spec(),
                                 parameter_values = list(), catalogue = NULL,
                                 reports = list(), metadata = list()) {
  if (!is.null(weather_file)) weather_file <- .normalise_existing(weather_file, "weather_file", "file")
  x <- structure(list(
    SchemaVersion = "1.0", RunId = run_id,
    Coordinates = list(Latitude = latitude, Longitude = longitude),
    Period = list(StartYear = as.integer(start_year), EndYear = as.integer(end_year)),
    Weather = list(File = weather_file), Soil = soil, Rotation = rotation,
    Cultivars = cultivars, Management = management,
    ParameterValues = parameter_values,
    ParameterCatalogue = if (is.null(catalogue)) NULL else unclass(catalogue),
    Reports = reports, Metadata = .named_list(metadata, "metadata")
  ), class = c("spacing_run_spec", "list"))
  validate_spacing_run_spec(x)
}

#' Validate a complete APSIM run specification
#'
#' @param x A `spacing_run_spec`.
#' @return `x`, invisibly.
#' @export
validate_spacing_run_spec <- function(x) {
  if (!inherits(x, "spacing_run_spec"))
    .rspacing_abort("x is not a spacing_run_spec.", "spacing_validation_error")
  .assert_scalar_character(x$RunId, "RunId")
  lat <- x$Coordinates$Latitude; lon <- x$Coordinates$Longitude
  if (!is.numeric(lat) || length(lat) != 1L || !is.finite(lat) || lat < -90 || lat > 90 ||
      !is.numeric(lon) || length(lon) != 1L || !is.finite(lon) || lon < -180 || lon > 180)
    .rspacing_abort("Run-spec coordinates are invalid.", "spacing_validation_error")
  if (length(x$Period$StartYear) != 1L || length(x$Period$EndYear) != 1L ||
      is.na(x$Period$StartYear) || is.na(x$Period$EndYear) ||
      x$Period$StartYear > x$Period$EndYear)
    .rspacing_abort("Run-spec period is invalid.", "spacing_validation_error")
  if (!inherits(x$Soil, "spacing_soil_spec"))
    .rspacing_abort("Run spec requires a spacing_soil_spec.", "spacing_validation_error")
  if (!inherits(x$Rotation, "spacing_rotation_spec"))
    .rspacing_abort("Run spec requires a spacing_rotation_spec.", "spacing_validation_error")
  if (!inherits(x$Management, "spacing_management_spec"))
    .rspacing_abort("Run spec requires a spacing_management_spec.", "spacing_validation_error")
  if (!is.list(x$Cultivars) || any(!vapply(x$Cultivars, inherits, logical(1), "spacing_cultivar_spec")))
    .rspacing_abort("Every cultivar must be a spacing_cultivar_spec.", "spacing_validation_error")
  if (!is.null(x$ParameterCatalogue)) {
    catalogue <- spacing_parameter_catalogue(as.data.frame(x$ParameterCatalogue,
                                                           stringsAsFactors = FALSE))
    validate_spacing_parameters(x$ParameterValues, catalogue)
  } else if (length(x$ParameterValues)) {
    .rspacing_abort("ParameterValues require a ParameterCatalogue.", "spacing_validation_error")
  }
  invisible(x)
}

.run_spec_as_list <- function(x) {
  unclass(x)
}

#' Write an APSIM run specification to JSON
#'
#' @param x A validated `spacing_run_spec`.
#' @param path Destination JSON path.
#' @param overwrite Logical. Overwrite an existing file.
#' @return The absolute output path, invisibly.
#' @export
write_spacing_run_spec <- function(x, path, overwrite = FALSE) {
  validate_spacing_run_spec(x)
  path <- as.character(fs::path_abs(path))
  if (fs::file_exists(path) && !isTRUE(overwrite))
    .rspacing_abort(sprintf("Run-spec file already exists: %s", path),
                    "spacing_validation_error")
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  tmp <- tempfile(pattern = "run-spec-", tmpdir = fs::path_dir(path), fileext = ".json")
  on.exit(if (fs::file_exists(tmp)) fs::file_delete(tmp), add = TRUE)
  jsonlite::write_json(.run_spec_as_list(x), tmp, pretty = TRUE,
                       auto_unbox = TRUE, null = "null", digits = NA)
  if (fs::file_exists(path)) fs::file_delete(path)
  fs::file_move(tmp, path)
  invisible(path)
}

.restore_parameter_catalogue <- function(x) {
  if (is.null(x)) return(NULL)

  required <- c(
    "parameter_id", "crop", "scope", "apsim_path", "type", "unit",
    "lower_bound", "upper_bound", "transform", "application_stage"
  )

  if (is.data.frame(x)) {
    return(spacing_parameter_catalogue(x))
  }

  if (!is.list(x)) {
    .rspacing_abort(
      "ParameterCatalogue in run-spec JSON must be an object or an array of records.",
      "spacing_validation_error"
    )
  }

  # Column-oriented JSON object: each required name is a vector/list column.
  if (all(required %in% names(x))) {
    columns <- lapply(x[required], function(column) {
      if (is.list(column)) unlist(column, recursive = FALSE, use.names = FALSE)
      else column
    })
    lengths <- vapply(columns, length, integer(1))
    if (!length(lengths) || any(lengths != lengths[[1L]])) {
      .rspacing_abort(
        "ParameterCatalogue columns have inconsistent lengths after JSON import.",
        "spacing_validation_error"
      )
    }
    data <- as.data.frame(columns, stringsAsFactors = FALSE,
                          optional = TRUE, check.names = FALSE)
    return(spacing_parameter_catalogue(data))
  }

  # Row-oriented JSON array: each element is one parameter record.
  records_are_valid <- length(x) > 0L && all(vapply(
    x,
    function(record) is.list(record) && all(required %in% names(record)),
    logical(1)
  ))
  if (records_are_valid) {
    rows <- lapply(x, function(record) {
      values <- lapply(record[required], function(value) {
        if (is.list(value)) unlist(value, recursive = FALSE, use.names = FALSE)
        else value
      })
      as.data.frame(values, stringsAsFactors = FALSE,
                    optional = TRUE, check.names = FALSE)
    })
    data <- do.call(rbind, rows)
    rownames(data) <- NULL
    return(spacing_parameter_catalogue(data))
  }

  .rspacing_abort(
    paste0(
      "ParameterCatalogue in run-spec JSON cannot be reconstructed. Required fields: ",
      paste(required, collapse = ", "), "."
    ),
    "spacing_validation_error"
  )
}

#' Read an APSIM run specification from JSON
#'
#' @param path Existing run-spec JSON path.
#' @param validate Logical. Validate the reconstructed object.
#' @return A `spacing_run_spec`.
#' @export
read_spacing_run_spec <- function(path, validate = TRUE) {
  path <- .normalise_existing(path, "run_spec", "file")
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  class(x$Soil) <- c("spacing_soil_spec", "list")
  class(x$Rotation) <- c("spacing_rotation_spec", "list")
  class(x$Management) <- c("spacing_management_spec", "list")
  x$Cultivars <- lapply(x$Cultivars %||% list(), function(z) {
    class(z) <- c("spacing_cultivar_spec", "list"); z
  })
  x$ParameterCatalogue <- .restore_parameter_catalogue(x$ParameterCatalogue)
  class(x) <- c("spacing_run_spec", "list")
  if (isTRUE(validate)) validate_spacing_run_spec(x)
  x
}

#' Update calibration parameters in a run specification
#'
#' @param run_spec A `spacing_run_spec`.
#' @param values Named values keyed by catalogue `parameter_id`.
#' @param catalogue Optional catalogue. Defaults to the catalogue embedded in
#'   `run_spec`.
#' @return An updated and validated `spacing_run_spec`.
#' @export
update_spacing_parameters <- function(run_spec, values, catalogue = NULL) {
  validate_spacing_run_spec(run_spec)
  if (is.null(catalogue)) {
    if (is.null(run_spec$ParameterCatalogue))
      .rspacing_abort("No parameter catalogue is available.", "spacing_validation_error")
    catalogue <- spacing_parameter_catalogue(as.data.frame(run_spec$ParameterCatalogue,
                                                           stringsAsFactors = FALSE))
  }
  values <- validate_spacing_parameters(values, catalogue)
  run_spec$ParameterValues <- utils::modifyList(run_spec$ParameterValues, values)
  run_spec$ParameterCatalogue <- unclass(catalogue)
  validate_spacing_run_spec(run_spec)
}

#' @export
print.spacing_run_spec <- function(x, ...) {
  cat("<spacing_run_spec>\n",
      "  run_id:    ", x$RunId, "\n",
      "  site:      ", x$Coordinates$Latitude, ", ", x$Coordinates$Longitude, "\n",
      "  period:    ", x$Period$StartYear, "-", x$Period$EndYear, "\n",
      "  soil:      ", x$Soil$ProfileId, "\n",
      "  rotation:  ", x$Rotation$Name, "\n",
      "  cultivars: ", length(x$Cultivars), "\n",
      sep = "")
  invisible(x)
}
