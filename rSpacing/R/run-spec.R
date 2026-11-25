# Helpers for data-driven APSIM run specifications.

#' Check a single string
#' @param x Value to check.
#' @param name Character scalar. Name for the error message.
#' @param allow_empty Logical. Accept `""`.
#' @return `x`, invisibly; a `spacing_validation_error` otherwise.
#' @noRd
.assert_scalar_character <- function(x, name, allow_empty = FALSE) {
  ok <- is.character(x) && length(x) == 1L && !is.na(x)
  if (ok && !allow_empty) ok <- nzchar(x)
  if (!ok) .rspacing_abort(sprintf("'%s' must be one %scharacter value.", name,
                                   if (allow_empty) "" else "non-empty "),
                            "spacing_validation_error")
  invisible(x)
}

#' Check a numeric vector
#' @param x Value to check.
#' @param name Character scalar. Name for the error message.
#' @param n Optional required length.
#' @param finite Logical. Require finite values.
#' @param lower,upper Inclusive bounds for every element.
#' @return `x`, invisibly; a `spacing_validation_error` otherwise.
#' @noRd
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

#' Coerce to a fully named list
#' @param x `NULL`, a named list or a named atomic vector.
#' @param name Character scalar. Name for the error message.
#' @return A named list (empty for `NULL` or length 0).
#' @noRd
.named_list <- function(x, name) {
  if (is.null(x) || length(x) == 0L) return(list())
  if (is.atomic(x) && !is.null(names(x))) x <- as.list(x)
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x))))
    .rspacing_abort(sprintf("'%s' must be a named list or named atomic vector.", name),
                    "spacing_validation_error")
  x
}

#' Create a layered soil specification for a run specification
#'
#' Builds the `Soil` section of a [new_spacing_run_spec()]: a data-only soil
#' profile that `Spacing.Tool2` (>= 0.09.07) translates into the APSIM
#' `Physical`, `Water`, `Organic`, `Chemical` and solute nodes when the run
#' specification is passed with `--run-spec`. It replaces the soil of the
#' request or scenario. To use it in a scenario, convert it with
#' [as_spacing_soil_profile()].
#'
#' Checks: equal lengths, `AirDry <= LL15 <= DUL <= SAT` in every layer,
#' positive bulk density, non-negative KS, carbon and mineral N, pH in
#' `(0, 14)`, and for crop parameters `KL >= 0`, `XF` in `[0, 1]` and `LL`
#' within `[AirDry, DUL]`.
#'
#' @param profile_id Character scalar. Soil profile identifier.
#' @param thickness Numeric vector. Layer thickness (mm), one value per layer.
#' @param bd Bulk density (g/cm3) by layer.
#' @param air_dry,ll15,dul,sat Volumetric water contents (m3/m3) by layer:
#'   air-dry, lower limit (15 bar), drained upper limit, saturation.
#' @param ks Saturated hydraulic conductivity (mm/day) by layer.
#' @param carbon Organic carbon (\%) by layer.
#' @param soil_cn_ratio Soil organic matter C:N ratio by layer.
#' @param ph Soil pH by layer.
#' @param no3,nh4,urea Initial nitrate, ammonium and urea (kg N/ha) by layer;
#'   `urea` defaults to 0.
#' @param initial_water Optional initial volumetric water content (m3/m3) by
#'   layer; `NULL` lets Tool2 start at DUL.
#' @param crop_parameters Optional named list by crop (e.g. `Maize`), each a
#'   list with layered `LL` (m3/m3), `KL` (/day) and `XF` (0-1); all three are
#'   required for a crop.
#' @param metadata Optional named list of provenance fields (source, licence,
#'   date, method, ...).
#' @return An object of class `spacing_soil_spec`: a list with `ProfileId`,
#'   `Thickness`, `BD`, `AirDry`, `LL15`, `DUL`, `SAT`, `KS`, `Carbon`,
#'   `SoilCNRatio`, `PH`, `NO3`, `NH4`, `Urea`, `InitialWater`,
#'   `CropParameters` and `Metadata`.
#' @family run specifications
#' @export
#' @examples
#' soil <- new_spacing_soil_spec("demo", thickness = c(150, 150),
#'   bd = c(1.30, 1.40), air_dry = c(.07, .08), ll15 = c(.14, .16), dul = c(.31, .33),
#'   sat = c(.45, .44), ks = c(100, 80), carbon = c(1.2, .8), soil_cn_ratio = c(12, 12),
#'   ph = c(7.2, 7.4), no3 = c(20, 8), nh4 = c(2, 1))
#' soil$DUL
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

#' Create a cultivar override specification
#'
#' Describes a cultivar as named APSIM parameter paths and values, for the
#' `Cultivars` section of a [new_spacing_run_spec()]. In scenarios the same
#' overrides are given with `parameter_overrides` in [spacing_crop()], which
#' Tool2 turns into a cultivar built on the base cultivar.
#'
#' Spacing.Tool2 (up to 0.09.10) reads only the `Soil` section of a run
#' specification passed with `--run-spec`; the other sections are stored for
#' provenance and for R workflows. Complete rotations and management are sent
#' to Tool2 with [new_spacing_scenario()].
#'
#' @param crop Character scalar. APSIM crop model name, e.g. `"Maize"`.
#' @param name Character scalar. Name of the cultivar used at sowing.
#' @param parameters Named list or named vector (at least one element). Names
#'   are APSIM property paths without leading brackets (e.g.
#'   `"Leaf.Photosynthesis.RUE.FixedValue"`) or catalogue identifiers.
#' @param aliases Optional character vector of other names of the cultivar.
#' @param metadata Optional named list of provenance fields.
#' @return An object of class `spacing_cultivar_spec`: a list with `Crop`,
#'   `Name`, `Parameters`, `Aliases` and `Metadata`.
#' @family run specifications
#' @export
#' @examples
#' new_spacing_cultivar_spec("Maize", "B_130_cal",
#'   list(Leaf.Photosynthesis.RUE.FixedValue = 2.1, Phenology.Juvenile.Target.FixedValue = 240))
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

#' Validate and complete rotation phases
#' @param phases Data frame or list of phase lists, each with `Crop` and
#'   `Cultivar`.
#' @return A list of phase lists with defaults filled in (`HarvestRule =
#'   "IsReadyForHarvesting"`, `ResidueFractionRetained = 1`, others `NULL`).
#' @noRd
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

#' Create a rotation specification
#'
#' Describes a crop sequence for the `Rotation` section of a
#' [new_spacing_run_spec()].
#'
#' Spacing.Tool2 (up to 0.09.10) reads only the `Soil` section of a run
#' specification passed with `--run-spec`; the other sections are stored for
#' provenance and for R workflows. Complete rotations and management are sent
#' to Tool2 with [new_spacing_scenario()].
#'
#' @param name Character scalar. Rotation name.
#' @param phases Data frame (one row per phase) or list of phase lists. Each
#'   phase needs `Crop` and `Cultivar` and may set `SowingWindow`
#'   (`list(Start = "d-mmm", End = "d-mmm")`), `Population` (plants/m2),
#'   `Depth` (mm), `RowSpacing` (mm), `HarvestRule` (default
#'   `"IsReadyForHarvesting"`) and `ResidueFractionRetained` (0-1, default 1).
#' @param repeat_rotation Logical. Repeat the sequence after its last phase.
#' @param metadata Optional named list of provenance fields.
#' @return An object of class `spacing_rotation_spec`: a list with `Name`,
#'   `Repeat`, `Phases` and `Metadata`.
#' @family run specifications
#' @export
#' @examples
#' new_spacing_rotation_spec("wheat-maize", list(
#'   list(Crop = "Wheat", Cultivar = "Claire",
#'        SowingWindow = list(Start = "10-oct", End = "20-nov")),
#'   list(Crop = "Maize", Cultivar = "B_130", Population = 7.5)))$Phases[[2]]
new_spacing_rotation_spec <- function(name, phases, repeat_rotation = TRUE, metadata = list()) {
  .assert_scalar_character(name, "name")
  structure(list(Name = name, Repeat = isTRUE(repeat_rotation),
                 Phases = .normalise_rotation_phases(phases),
                 Metadata = .named_list(metadata, "metadata")),
            class = c("spacing_rotation_spec", "list"))
}

#' Normalise a list of management events
#' @param x `NULL`, a data frame (one row per event) or a list of event lists.
#' @param type Character scalar. Event type for error messages.
#' @return A list of event lists.
#' @noRd
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
#' Stores fertilisation, irrigation, tillage and residue events for the
#' `Management` section of a [new_spacing_run_spec()]. Event fields are kept as
#' given.
#'
#' Spacing.Tool2 (up to 0.09.10) reads only the `Soil` section of a run
#' specification passed with `--run-spec`; the other sections are stored for
#' provenance and for R workflows. Complete rotations and management are sent
#' to Tool2 with [new_spacing_scenario()]. Management for scenarios is built with [spacing_crop()]
#' and its options.
#'
#' @param fertilisation,irrigation,tillage,residues Data frames (one row per
#'   event) or lists of event lists.
#' @param metadata Optional named list of provenance fields.
#' @return An object of class `spacing_management_spec`: a list with
#'   `Fertilisation`, `Irrigation`, `Tillage`, `Residues` (lists of events) and
#'   `Metadata`.
#' @family run specifications
#' @export
#' @examples
#' new_spacing_management_spec(
#'   fertilisation = data.frame(Crop = "Maize", Trigger = "Sowing", AmountKgNHa = 50))
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

#' Build and validate a parameter catalogue
#'
#' A catalogue maps stable R-facing parameter identifiers to APSIM property
#' paths, with units, bounds, a transformation and the stage at which each
#' override applies. It is used to validate calibration and sensitivity values
#' ([validate_spacing_parameters()]) and is stored in run specifications.
#'
#' @param data Optional data frame with all catalogue columns. When `NULL`, the
#'   catalogue is built from the vector arguments below (length-one values are
#'   recycled).
#' @param parameter_id Character vector. Unique identifiers, e.g.
#'   `"maize_rue"`.
#' @param crop Character vector. APSIM crop model names.
#' @param scope Character vector. Where the parameter applies, e.g.
#'   `"cultivar"` or `"soil"`.
#' @param apsim_path Character vector. APSIM property paths without leading
#'   brackets, e.g. `"Leaf.Photosynthesis.RUE.FixedValue"`.
#' @param type Character vector. Value type; `"double"` values are checked
#'   against the bounds.
#' @param unit Character vector. Units (informative).
#' @param lower_bound,upper_bound Numeric vectors. Allowed range
#'   (`lower_bound <= upper_bound`).
#' @param transform Character vector. Transformation used by calibration
#'   algorithms, e.g. `"identity"` or `"log"` (informative).
#' @param application_stage Character vector. When the override is applied,
#'   e.g. `"cultivar"` (informative).
#' @return A data frame of class `spacing_parameter_catalogue` with the ten
#'   columns `parameter_id`, `crop`, `scope`, `apsim_path`, `type`, `unit`,
#'   `lower_bound`, `upper_bound`, `transform`, `application_stage`.
#'   Duplicate identifiers, empty paths or reversed bounds raise an error.
#' @family run specifications
#' @export
#' @examples
#' catalogue <- spacing_parameter_catalogue(parameter_id = "maize_rue", crop = "Maize",
#'   scope = "cultivar", apsim_path = "Leaf.Photosynthesis.RUE.FixedValue",
#'   unit = "g/MJ", lower_bound = 1.2, upper_bound = 3.2)
#' catalogue
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

#' Validate parameter values against a catalogue
#'
#' Checks that every value has a catalogue identifier and, for `"double"`
#' parameters, is a single finite number within the bounds.
#'
#' @param values Named numeric vector or named list of values, keyed by
#'   `parameter_id`.
#' @param catalogue A `spacing_parameter_catalogue`, or a data frame accepted
#'   by [spacing_parameter_catalogue()].
#' @param require_all Logical. Require a value for every catalogue parameter.
#' @return The values as a named list. Unknown, missing (with `require_all`)
#'   or out-of-range values raise a `spacing_validation_error`.
#' @family run specifications
#' @export
#' @examples
#' catalogue <- spacing_parameter_catalogue(parameter_id = "maize_rue", crop = "Maize",
#'   scope = "cultivar", apsim_path = "Leaf.Photosynthesis.RUE.FixedValue",
#'   unit = "g/MJ", lower_bound = 1.2, upper_bound = 3.2)
#' validate_spacing_parameters(c(maize_rue = 2.0), catalogue)
#' try(validate_spacing_parameters(c(maize_rue = 5), catalogue))
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

#' Create a complete run specification
#'
#' Assembles location, period, weather, soil, rotation, cultivars, management,
#' parameter values and report definitions into one validated, data-only
#' object that can be written to JSON with [write_spacing_run_spec()].
#'
#' Spacing.Tool2 (up to 0.09.10) reads only the `Soil` section of a run
#' specification passed with `--run-spec`; the other sections are stored for
#' provenance and for R workflows. Complete rotations and management are sent
#' to Tool2 with [new_spacing_scenario()].
#'
#' @param run_id Character scalar. Run or calibration-iteration identifier.
#' @param latitude,longitude Numeric scalars. Coordinates in decimal degrees.
#' @param start_year,end_year Integers. Simulation years.
#' @param weather_file Existing APSIM `.met` file, or `NULL` when Tool2 takes
#'   the weather from its configured source.
#' @param soil A `spacing_soil_spec` ([new_spacing_soil_spec()]).
#' @param rotation A `spacing_rotation_spec` ([new_spacing_rotation_spec()]).
#' @param cultivars List of `spacing_cultivar_spec` objects
#'   ([new_spacing_cultivar_spec()]).
#' @param management A `spacing_management_spec`
#'   ([new_spacing_management_spec()]).
#' @param parameter_values Named values validated against `catalogue`.
#' @param catalogue Optional `spacing_parameter_catalogue`; required when
#'   `parameter_values` is not empty.
#' @param reports Optional named list of report definitions.
#' @param metadata Optional named list of provenance fields.
#' @return An object of class `spacing_run_spec`: a list with
#'   `SchemaVersion` ("1.0"), `RunId`, `Coordinates`, `Period`, `Weather`,
#'   `Soil`, `Rotation`, `Cultivars`, `Management`, `ParameterValues`,
#'   `ParameterCatalogue`, `Reports` and `Metadata`.
#' @family run specifications
#' @export
#' @examples
#' soil <- new_spacing_soil_spec("demo", thickness = c(150, 150),
#'   bd = c(1.30, 1.40), air_dry = c(.07, .08), ll15 = c(.14, .16), dul = c(.31, .33),
#'   sat = c(.45, .44), ks = c(100, 80), carbon = c(1.2, .8), soil_cn_ratio = c(12, 12),
#'   ph = c(7.2, 7.4), no3 = c(20, 8), nh4 = c(2, 1))
#' rotation <- new_spacing_rotation_spec("maize", list(list(Crop = "Maize", Cultivar = "B_130")))
#' catalogue <- spacing_parameter_catalogue(parameter_id = "maize_rue", crop = "Maize",
#'   scope = "cultivar", apsim_path = "Leaf.Photosynthesis.RUE.FixedValue",
#'   unit = "g/MJ", lower_bound = 1.2, upper_bound = 3.2)
#' spec <- new_spacing_run_spec("run-001", 44.9, 10.63, 2020, 2024, soil = soil,
#'   rotation = rotation, parameter_values = list(maize_rue = 2.0), catalogue = catalogue)
#' spec
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

#' Validate a run specification
#'
#' Checks the identifier, coordinates, period, the classes of the soil,
#' rotation, management and cultivar sections, and the parameter values
#' against the embedded catalogue.
#'
#' @param x A `spacing_run_spec` object.
#' @return `x`, invisibly. Problems raise a `spacing_validation_error`.
#' @family run specifications
#' @export
#' @examples
#' soil <- new_spacing_soil_spec("demo", thickness = c(150, 150),
#'   bd = c(1.30, 1.40), air_dry = c(.07, .08), ll15 = c(.14, .16), dul = c(.31, .33),
#'   sat = c(.45, .44), ks = c(100, 80), carbon = c(1.2, .8), soil_cn_ratio = c(12, 12),
#'   ph = c(7.2, 7.4), no3 = c(20, 8), nh4 = c(2, 1))
#' rotation <- new_spacing_rotation_spec("maize", list(list(Crop = "Maize", Cultivar = "B_130")))
#' catalogue <- spacing_parameter_catalogue(parameter_id = "maize_rue", crop = "Maize",
#'   scope = "cultivar", apsim_path = "Leaf.Photosynthesis.RUE.FixedValue",
#'   unit = "g/MJ", lower_bound = 1.2, upper_bound = 3.2)
#' spec <- new_spacing_run_spec("run-001", 44.9, 10.63, 2020, 2024, soil = soil,
#'   rotation = rotation, parameter_values = list(maize_rue = 2.0), catalogue = catalogue)
#' validate_spacing_run_spec(spec)
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

#' Prepare a run specification for JSON
#' @param x A `spacing_run_spec`.
#' @return A plain list with the parameter catalogue as an array of records.
#' @noRd
.run_spec_as_list <- function(x) {
  out <- unclass(x)
  if (!is.null(x$ParameterCatalogue)) {
    catalogue <- as.data.frame(
      x$ParameterCatalogue,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    out$ParameterCatalogue <- lapply(seq_len(nrow(catalogue)), function(i) {
      as.list(catalogue[i, , drop = FALSE])
    })
  }
  out
}

#' Write a run specification to JSON
#'
#' Validates the specification and writes it atomically (temporary file moved
#' into place). Numbers keep full precision.
#'
#' @param x A `spacing_run_spec` object.
#' @param path Character scalar. Destination JSON file.
#' @param overwrite Logical. Replace an existing file.
#' @return The absolute path of the file, invisibly.
#' @family run specifications
#' @export
#' @examples
#' soil <- new_spacing_soil_spec("demo", thickness = c(150, 150),
#'   bd = c(1.30, 1.40), air_dry = c(.07, .08), ll15 = c(.14, .16), dul = c(.31, .33),
#'   sat = c(.45, .44), ks = c(100, 80), carbon = c(1.2, .8), soil_cn_ratio = c(12, 12),
#'   ph = c(7.2, 7.4), no3 = c(20, 8), nh4 = c(2, 1))
#' rotation <- new_spacing_rotation_spec("maize", list(list(Crop = "Maize", Cultivar = "B_130")))
#' catalogue <- spacing_parameter_catalogue(parameter_id = "maize_rue", crop = "Maize",
#'   scope = "cultivar", apsim_path = "Leaf.Photosynthesis.RUE.FixedValue",
#'   unit = "g/MJ", lower_bound = 1.2, upper_bound = 3.2)
#' spec <- new_spacing_run_spec("run-001", 44.9, 10.63, 2020, 2024, soil = soil,
#'   rotation = rotation, parameter_values = list(maize_rue = 2.0), catalogue = catalogue)
#' path <- write_spacing_run_spec(spec, tempfile(fileext = ".json"))
#' read_spacing_run_spec(path)
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

#' Rebuild a parameter catalogue read from JSON
#'
#' Accepts a data frame, a column-oriented object or a row-oriented array of
#' records (as written by [write_spacing_run_spec()]).
#'
#' @param x The `ParameterCatalogue` element of a parsed run-spec JSON.
#' @return A `spacing_parameter_catalogue`, or `NULL` when `x` is `NULL`.
#' @noRd
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
      if (is.null(column)) return(NA)
      if (is.list(column)) {
        return(vapply(column, function(value) {
          if (is.null(value) || !length(value)) NA
          else value[[1L]]
        }, FUN.VALUE = if (is.numeric(unlist(column))) numeric(1) else character(1)))
      }
      column
    })
    lengths <- vapply(columns, length, integer(1))
    target_length <- max(lengths)
    columns <- lapply(columns, function(column) {
      if (length(column) == target_length) column
      else if (length(column) == 1L) rep(column, target_length)
      else .rspacing_abort(
        "ParameterCatalogue columns have inconsistent lengths after JSON import.",
        "spacing_validation_error"
      )
    })
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
        if (is.null(value) || !length(value)) return(NA)
        if (is.list(value)) value <- unlist(value, recursive = FALSE, use.names = FALSE)
        value[[1L]]
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

#' Turn layered soil fields read from JSON back into numeric vectors
#' @param soil The `Soil` element of a parsed run-spec JSON (lists of scalars).
#' @return The same list with layered fields as numeric vectors, including the
#'   `LL`, `KL` and `XF` of each crop in `CropParameters`.
#' @noRd
.restore_soil_vectors <- function(soil) {
  if (is.null(soil)) return(soil)
  as_num <- function(v) if (is.list(v) && length(v) && all(vapply(v, function(e) is.numeric(e) && length(e) == 1L, logical(1))))
    as.numeric(unlist(v)) else v
  layered <- c("Thickness", "BD", "AirDry", "LL15", "DUL", "SAT", "KS", "Carbon", "SoilCNRatio",
               "PH", "NO3", "NH4", "Urea", "InitialWater")
  for (nm in intersect(layered, names(soil))) soil[[nm]] <- as_num(soil[[nm]])
  if (length(soil$CropParameters))
    soil$CropParameters <- lapply(soil$CropParameters, function(cp) lapply(cp, as_num))
  soil
}

#' Read a run specification from JSON
#'
#' Reads a file written by [write_spacing_run_spec()] and restores the classes
#' of the soil, rotation, management, cultivar and catalogue sections.
#'
#' @param path Character scalar. Existing run-spec JSON file.
#' @param validate Logical. Validate the result with
#'   [validate_spacing_run_spec()].
#' @return A `spacing_run_spec` object.
#' @family run specifications
#' @export
#' @examples
#' soil <- new_spacing_soil_spec("demo", thickness = c(150, 150),
#'   bd = c(1.30, 1.40), air_dry = c(.07, .08), ll15 = c(.14, .16), dul = c(.31, .33),
#'   sat = c(.45, .44), ks = c(100, 80), carbon = c(1.2, .8), soil_cn_ratio = c(12, 12),
#'   ph = c(7.2, 7.4), no3 = c(20, 8), nh4 = c(2, 1))
#' rotation <- new_spacing_rotation_spec("maize", list(list(Crop = "Maize", Cultivar = "B_130")))
#' catalogue <- spacing_parameter_catalogue(parameter_id = "maize_rue", crop = "Maize",
#'   scope = "cultivar", apsim_path = "Leaf.Photosynthesis.RUE.FixedValue",
#'   unit = "g/MJ", lower_bound = 1.2, upper_bound = 3.2)
#' spec <- new_spacing_run_spec("run-001", 44.9, 10.63, 2020, 2024, soil = soil,
#'   rotation = rotation, parameter_values = list(maize_rue = 2.0), catalogue = catalogue)
#' path <- write_spacing_run_spec(spec, tempfile(fileext = ".json"))
#' read_spacing_run_spec(path)$ParameterValues
read_spacing_run_spec <- function(path, validate = TRUE) {
  path <- .normalise_existing(path, "run_spec", "file")
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  x$Soil <- .restore_soil_vectors(x$Soil)
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

#' Change parameter values in a run specification
#'
#' Validates new values against the catalogue (the embedded one by default)
#' and merges them into `ParameterValues`, e.g. at each iteration of a
#' calibration.
#'
#' @param run_spec A `spacing_run_spec` object.
#' @param values Named values keyed by catalogue `parameter_id`.
#' @param catalogue Optional `spacing_parameter_catalogue`; defaults to the
#'   catalogue stored in `run_spec` (an error when there is none).
#' @return The updated and validated `spacing_run_spec`.
#' @family run specifications
#' @export
#' @examples
#' soil <- new_spacing_soil_spec("demo", thickness = c(150, 150),
#'   bd = c(1.30, 1.40), air_dry = c(.07, .08), ll15 = c(.14, .16), dul = c(.31, .33),
#'   sat = c(.45, .44), ks = c(100, 80), carbon = c(1.2, .8), soil_cn_ratio = c(12, 12),
#'   ph = c(7.2, 7.4), no3 = c(20, 8), nh4 = c(2, 1))
#' rotation <- new_spacing_rotation_spec("maize", list(list(Crop = "Maize", Cultivar = "B_130")))
#' catalogue <- spacing_parameter_catalogue(parameter_id = "maize_rue", crop = "Maize",
#'   scope = "cultivar", apsim_path = "Leaf.Photosynthesis.RUE.FixedValue",
#'   unit = "g/MJ", lower_bound = 1.2, upper_bound = 3.2)
#' spec <- new_spacing_run_spec("run-001", 44.9, 10.63, 2020, 2024, soil = soil,
#'   rotation = rotation, parameter_values = list(maize_rue = 2.0), catalogue = catalogue)
#' update_spacing_parameters(spec, list(maize_rue = 2.4))$ParameterValues
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

#' @describeIn new_spacing_run_spec Print identifier, site, period, soil,
#'   rotation and number of cultivars.
#' @param x A `spacing_run_spec` object.
#' @param ... Ignored.
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
