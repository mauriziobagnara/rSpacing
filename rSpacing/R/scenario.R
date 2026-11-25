# Complete Spacing.Tool2 scenarios (rotation, management, soil) sent with --request.
# Added in rSpacing 0.10.07 for Spacing.Tool2 >= 0.09.07; management events
# (dates, stages, tillage operations, organic inputs, initial residue) added in
# rSpacing 0.10.08 for Spacing.Tool2 >= 0.09.09.

.TILLAGE_TYPES <- c("conventional", "minimum", "no-till")

#' Define a tillage operation
#'
#' Tillage is applied by `Spacing.Tool2` (>= 0.09.07) on the sowing day of the
#' crop, after any organic input and before sowing. It incorporates a fraction
#' of the surface residues to a depth and temporarily lowers the runoff curve
#' number (CN2).
#'
#' Arguments left `NULL` take the Tool2 assumptions for `type`:
#' \tabular{lllll}{
#'   type \tab fraction \tab depth (mm) \tab CN2 reduction \tab until rain (mm)\cr
#'   conventional \tab 0.90 \tab 250 \tab 20 \tab 100\cr
#'   minimum \tab 0.50 \tab 120 \tab 10 \tab 75\cr
#'   no-till \tab 0 \tab 0 \tab 0 \tab 0
#' }
#'
#' @param type One of `"conventional"`, `"minimum"`, `"no-till"`.
#' @param incorporation_fraction Fraction of surface residues incorporated, in `[0, 1]`.
#' @param depth_mm Incorporation depth (mm).
#' @param cn_reduction Reduction of the runoff curve number CN2.
#' @param cn_rain Cumulative rain (mm) after which the CN2 reduction ends.
#' @return An object of class `spacing_tillage`: a list with `type` and the
#'   four parameters (`NULL` when Tool2 uses the default of `type`).
#' @seealso [spacing_tillage_operation()] for tillage on other dates.
#' @family crop management
#' @export
#' @examples
#' spacing_tillage("minimum")
#' spacing_tillage("conventional", depth_mm = 300)
spacing_tillage <- function(type = c("conventional", "minimum", "no-till"),
                            incorporation_fraction = NULL, depth_mm = NULL,
                            cn_reduction = NULL, cn_rain = NULL) {
  type <- match.arg(type)
  opt <- function(x, name, upper = Inf) {
    if (is.null(x)) return(NULL)
    .spacing_assert_number(x, name, lower = 0, upper = upper)
  }
  x <- list(type = type,
            incorporation_fraction = opt(incorporation_fraction, "incorporation_fraction", 1),
            depth_mm = opt(depth_mm, "depth_mm"),
            cn_reduction = opt(cn_reduction, "cn_reduction"),
            cn_rain = opt(cn_rain, "cn_rain"))
  if (isTRUE(x$incorporation_fraction > 0) && identical(x$depth_mm, 0))
    stop("`depth_mm` must be > 0 when `incorporation_fraction` > 0.", call. = FALSE)
  structure(x, class = "spacing_tillage")
}

#' Define an organic nitrogen input on the sowing day
#'
#' The input is applied by `Spacing.Tool2` (>= 0.09.07) on the sowing day,
#' before tillage, as an APSIM surface residue. The applied mass is
#' `amount_kg_n_ha * cn_ratio / carbon_fraction` kg/ha, with the carbon fraction
#' of the residue type in the APSIM resource (see [spacing_residue_types()];
#' `manure` 0.08). With no-till it stays on the surface. For inputs on other
#' dates use [spacing_organic_application()].
#'
#' @param amount_kg_n_ha Organic N (kg N/ha). Zero means no input.
#' @param residue_type APSIM residue type, one of
#'   `spacing_residue_types()$residue_type`. Types other than `"manure"`
#'   require Tool2 >= 0.09.09.
#' @param cn_ratio C:N ratio of the material (default 15, mature cattle manure).
#' @return An object of class `spacing_organic_n`: a list with
#'   `amount_kg_n_ha`, `residue_type` (APSIM spelling) and `cn_ratio`.
#' @seealso [spacing_organic_application()] for inputs on other dates,
#'   [spacing_residue_types()].
#' @family crop management
#' @export
#' @examples
#' spacing_organic_n(25)
#' spacing_organic_n(30, residue_type = "fym", cn_ratio = 18)
spacing_organic_n <- function(amount_kg_n_ha = 0, residue_type = "manure", cn_ratio = 15) {
  amount_kg_n_ha <- .spacing_assert_number(amount_kg_n_ha, "amount_kg_n_ha", lower = 0)
  residue_type <- .spacing_residue_type(residue_type)
  cn_ratio <- .spacing_assert_number(cn_ratio, "cn_ratio", lower = 0, lower_inclusive = FALSE)
  structure(list(amount_kg_n_ha = amount_kg_n_ha, residue_type = residue_type,
                 cn_ratio = cn_ratio), class = "spacing_organic_n")
}

.OPERATION_TRIGGERS <- c("Sowing", "DaysAfterSowing", "Date", "DaysAfterPreviousHarvest")

#' Timing fields of an operation, validated for its trigger
#' @param trigger One of the operation triggers.
#' @param days Integer for the `Days*` triggers.
#' @param date `d-mmm` date for `"Date"`.
#' @return A list with `days` (integer, 0 when unused) and `date` (`NULL` when
#'   unused).
#' @noRd
.spacing_operation_timing <- function(trigger, days, date) {
  if (trigger %in% c("DaysAfterSowing", "DaysAfterPreviousHarvest")) {
    if (!is.numeric(days) || length(days) != 1L || is.na(days) || days < 0 || days != as.integer(days))
      stop("`days` must be one non-negative integer.", call. = FALSE)
    return(list(days = as.integer(days), date = NULL))
  }
  if (trigger == "Date") {
    if (is.null(date)) stop("`date` is required with trigger = \"Date\".", call. = FALSE)
    return(list(days = 0L, date = .spacing_normalize_day_month(date, "date")))
  }
  list(days = 0L, date = NULL)
}

#' Define an organic input with its own timing
#'
#' Organic inputs (manure, slurry, compost, residues) applied by `Spacing.Tool2`
#' (>= 0.09.09) as APSIM surface residue on a trigger of their own. Each input
#' belongs to the crop of [spacing_crop()] it is given to; its season runs from
#' the end of the previous crop (or the start of the simulation) to the end of
#' this crop, so it can fall in the fallow before sowing or during the crop:
#' \describe{
#'   \item{`"Sowing"`}{on the sowing day, before tillage.}
#'   \item{`"DaysAfterSowing"`}{`days` days after sowing.}
#'   \item{`"Date"`}{on `date` (`d-mmm`).}
#'   \item{`"DaysAfterPreviousHarvest"`}{`days` days after the end of the
#'     previous crop (after the start of the simulation for the first crop).}
#' }
#' On the same day organic inputs come before tillage. An input whose trigger is
#' not reached before the crop ends is skipped and reported as an APSIM warning.
#' The `organic` argument of [spacing_crop()] (sowing-day input) is added to
#' these inputs unless one of them already has `trigger = "Sowing"`.
#'
#' @inheritParams spacing_organic_n
#' @param trigger One of `"Sowing"`, `"DaysAfterSowing"`, `"Date"`,
#'   `"DaysAfterPreviousHarvest"`.
#' @param days Non-negative integer for the `Days*` triggers.
#' @param date Calendar date `d-mmm` for `trigger = "Date"`.
#' @return An object of class `spacing_organic_application`: the fields of
#'   [spacing_organic_n()] plus `trigger`, `days` (integer) and `date`
#'   (`d-mmm` or `NULL`).
#' @family crop management
#' @export
#' @examples
#' spacing_organic_application(40, "cm", cn_ratio = 10,
#'   trigger = "DaysAfterPreviousHarvest", days = 30)
spacing_organic_application <- function(amount_kg_n_ha, residue_type = "manure", cn_ratio = 15,
                                        trigger = c("Sowing", "DaysAfterSowing", "Date",
                                                    "DaysAfterPreviousHarvest"),
                                        days = 0L, date = NULL) {
  trigger <- match.arg(trigger)
  base <- spacing_organic_n(amount_kg_n_ha, residue_type, cn_ratio)
  timing <- .spacing_operation_timing(trigger, days, date)
  structure(c(unclass(base), list(trigger = trigger), timing), class = "spacing_organic_application")
}

#' Define a tillage operation with its own timing
#'
#' A tillage operation applied by `Spacing.Tool2` (>= 0.09.09) on its own
#' trigger, with the same triggers and season as [spacing_organic_application()]
#' (e.g. autumn ploughing on `"15-nov"` before a spring crop). Parameters left
#' `NULL` take the assumptions for `type` documented in [spacing_tillage()].
#' Give a list of operations to `tillage_operations` in [spacing_crop()]; it
#' replaces the single sowing-day `tillage`, and an empty list means no tillage.
#'
#' @inheritParams spacing_tillage
#' @inheritParams spacing_organic_application
#' @return An object of class `spacing_tillage_operation`: the fields of
#'   [spacing_tillage()] plus `trigger`, `days` and `date`.
#' @family crop management
#' @export
#' @examples
#' spacing_tillage_operation("conventional", trigger = "Date", date = "15-nov")
spacing_tillage_operation <- function(type = c("conventional", "minimum", "no-till"),
                                      trigger = c("Sowing", "DaysAfterSowing", "Date",
                                                  "DaysAfterPreviousHarvest"),
                                      days = 0L, date = NULL,
                                      incorporation_fraction = NULL, depth_mm = NULL,
                                      cn_reduction = NULL, cn_rain = NULL) {
  type <- match.arg(type)
  trigger <- match.arg(trigger)
  base <- spacing_tillage(type, incorporation_fraction, depth_mm, cn_reduction, cn_rain)
  timing <- .spacing_operation_timing(trigger, days, date)
  structure(c(unclass(base), list(trigger = trigger), timing), class = "spacing_tillage_operation")
}

#' Define the initial surface residue
#'
#' Surface organic matter present at the start of the simulation
#' (`Spacing.Tool2` >= 0.09.09). Without it Tool2 uses 500 kg/ha of wheat
#' stubble with C:N 80, all lying on the surface.
#'
#' @param type APSIM residue type (see [spacing_residue_types()]).
#' @param mass_kg_ha Residue mass (kg/ha).
#' @param cn_ratio C:N ratio of the residue.
#' @param standing_fraction Fraction of the residue standing, in `[0, 1]`.
#' @param cp_ratio C:P ratio (0 = not simulated).
#' @param name Name of the residue pool; default `"<type>_residue"`.
#' @return An object of class `spacing_surface_residue`: a list with `name`,
#'   `type`, `mass_kg_ha`, `cn_ratio`, `standing_fraction` and `cp_ratio`
#'   (the fields of the Tool2 `surface_residue` object).
#' @family scenarios
#' @export
#' @examples
#' spacing_surface_residue("maize", 3000, cn_ratio = 60)
spacing_surface_residue <- function(type = "wheat", mass_kg_ha = 500, cn_ratio = 80,
                                    standing_fraction = 0, cp_ratio = 0, name = NULL) {
  type <- .spacing_residue_type(type, "type")
  structure(list(name = .spacing_optional_text(name, "name") %||% paste0(type, "_residue"),
                 type = type,
                 mass_kg_ha = .spacing_assert_number(mass_kg_ha, "mass_kg_ha", lower = 0),
                 cn_ratio = .spacing_assert_number(cn_ratio, "cn_ratio", lower = 0, lower_inclusive = FALSE),
                 standing_fraction = .spacing_assert_number(standing_fraction, "standing_fraction", 0, 1),
                 cp_ratio = .spacing_assert_number(cp_ratio, "cp_ratio", lower = 0)),
            class = "spacing_surface_residue")
}

#' Define one crop of a rotation
#'
#' Collects cultivar, sowing rule, fertilisation, irrigation, tillage, organic
#' input and optional cultivar parameter overrides for one crop. The crops of a
#' [new_spacing_scenario()] are sown by Tool2 in list order, one at a time, at
#' the first date of each sowing window after the previous harvest.
#'
#' @param name APSIM crop model name, e.g. `"Wheat"`, `"Maize"`, `"Soybean"`,
#'   `"Barley"`.
#' @param cultivar APSIM cultivar name.
#' @param sowing_window Character vector of length 2, start and end in
#'   `d-mmm` format, e.g. `c("10-oct", "20-nov")`.
#' @param density Sowing population (plants/m2).
#' @param depth_mm Sowing depth (mm).
#' @param row_spacing_mm Row spacing (mm).
#' @param fertilisation A [spacing_fertilisation()] object.
#' @param irrigation A [spacing_irrigation()] object; the default disables
#'   automatic irrigation.
#' @param tillage A [spacing_tillage()] object: one tillage on the sowing day.
#' @param organic A [spacing_organic_n()] object: organic input on the sowing day.
#' @param organic_applications List of [spacing_organic_application()] objects
#'   (Tool2 >= 0.09.09).
#' @param sowing_date Fixed sowing date `d-mmm` (Tool2 >= 0.09.10): the crop
#'   is sown on that day (the first one after the previous crop has ended) and
#'   `sowing_window` may be `NULL`.
#' @param sowing_rule A [spacing_sowing_rule()] checked on each day of the
#'   sowing window (Tool2 >= 0.09.10).
#' @param irrigation_schedule List of [spacing_irrigation_event()] objects
#'   (Tool2 >= 0.09.10).
#' @param intercrop List of [spacing_intercrop()] objects sown with this crop
#'   (Tool2 >= 0.09.10).
#' @param termination A [spacing_termination()]: the crop is a cover crop
#'   ended without harvest (Tool2 >= 0.09.10).
#' @param perennial A [spacing_perennial()]: the crop stays for several years
#'   (Tool2 >= 0.09.10).
#' @param residue_removal A [spacing_residue_removal()] applied when the crop
#'   ends (Tool2 >= 0.09.10).
#' @param tillage_operations `NULL` (use `tillage`) or a list of
#'   [spacing_tillage_operation()] objects that replaces `tillage`; `list()`
#'   means no tillage at all (Tool2 >= 0.09.09).
#' @param parameter_overrides Named list or named numeric vector of APSIM
#'   cultivar parameters (paths without leading brackets, e.g.
#'   `"Leaf.Photosynthesis.RUE.FixedValue"`). Tool2 applies them on top of the
#'   base cultivar.
#' @param verify_cultivar Logical. When `TRUE` and the crop has an APSIM
#'   cultivar catalogue ([spacing_cultivars()]), the cultivar (and those of
#'   intercropped crops) must be defined there; the name is returned with the
#'   APSIM spelling. Crops without a catalogue are passed unverified; Tool2
#'   (>= 0.09.10) checks every cultivar before running.
#' @return An object of class `spacing_crop`: a list with every argument after
#'   validation (dates normalised to `d-mmm`, stage names in APSIM spelling,
#'   single option objects wrapped in lists).
#' @seealso [new_spacing_scenario()], which assembles crops into a rotation.
#' @family scenarios
#' @export
#' @examples
#' spacing_crop("Maize", "B_130", c("1-apr", "10-may"), density = 7.5,
#'              depth_mm = 50, row_spacing_mm = 750,
#'              fertilisation = spacing_fertilisation(50),
#'              tillage = spacing_tillage("conventional"),
#'              organic = spacing_organic_n(25))
spacing_crop <- function(name, cultivar, sowing_window, density, depth_mm, row_spacing_mm,
                         fertilisation = spacing_fertilisation(),
                         irrigation = spacing_irrigation(enabled = FALSE),
                         tillage = spacing_tillage("conventional"),
                         organic = spacing_organic_n(0),
                         parameter_overrides = list(),
                         verify_cultivar = TRUE,
                         organic_applications = list(),
                         tillage_operations = NULL,
                         sowing_date = NULL, sowing_rule = NULL, irrigation_schedule = list(),
                         intercrop = list(), termination = NULL, perennial = NULL,
                         residue_removal = NULL) {
  name <- .spacing_optional_text(name, "name")
  cultivar <- .spacing_optional_text(cultivar, "cultivar")
  if (is.null(name) || is.null(cultivar)) stop("`name` and `cultivar` are required.", call. = FALSE)
  if (isTRUE(verify_cultivar) && tolower(name) %in% tolower(spacing_cultivars()$crop))
    cultivar <- spacing_validate_cultivar(name, cultivar)
  if (!is.null(sowing_date)) sowing_date <- .spacing_normalize_day_month(sowing_date, "sowing_date")
  if (is.null(sowing_window) && is.null(sowing_date))
    stop("Give `sowing_window` or `sowing_date`.", call. = FALSE)
  if (!is.null(sowing_window)) {
    if (!is.character(sowing_window) || length(sowing_window) != 2L || anyNA(sowing_window))
      stop("`sowing_window` must be a character vector of length 2 (start, end).", call. = FALSE)
    sowing_window <- c(.spacing_normalize_day_month(sowing_window[[1]], "sowing_window[1]"),
                       .spacing_normalize_day_month(sowing_window[[2]], "sowing_window[2]"))
  }
  density <- .spacing_assert_number(density, "density", lower = 0, lower_inclusive = FALSE)
  depth_mm <- .spacing_assert_number(depth_mm, "depth_mm", lower = 0, lower_inclusive = FALSE)
  row_spacing_mm <- .spacing_assert_number(row_spacing_mm, "row_spacing_mm", lower = 0, lower_inclusive = FALSE)
  checks <- list(fertilisation = "spacing_fertilisation", irrigation = "spacing_irrigation",
                 tillage = "spacing_tillage", organic = "spacing_organic_n")
  for (arg in names(checks)) if (!inherits(get(arg), checks[[arg]]))
    stop("`", arg, "` must be created with ", checks[[arg]], "().", call. = FALSE)
  .list_of <- function(x, cls, arg) {
    if (inherits(x, cls)) x <- list(x)
    if (!is.list(x) || !all(vapply(x, inherits, logical(1), cls)))
      stop("`", arg, "` must be a list of ", cls, "() objects.", call. = FALSE)
    unname(x)
  }
  organic_applications <- .list_of(organic_applications, "spacing_organic_application", "organic_applications")
  if (!is.null(tillage_operations))
    tillage_operations <- .list_of(tillage_operations, "spacing_tillage_operation", "tillage_operations")
  # v0.10.09 options
  opt <- list(sowing_rule = "spacing_sowing_rule", termination = "spacing_termination",
              perennial = "spacing_perennial", residue_removal = "spacing_residue_removal")
  for (arg in names(opt)) if (!is.null(get(arg)) && !inherits(get(arg), opt[[arg]]))
    stop("`", arg, "` must be NULL or created with ", opt[[arg]], "().", call. = FALSE)
  if (!is.null(sowing_date) && !is.null(sowing_rule))
    warning("`sowing_rule` is ignored when `sowing_date` is given.", call. = FALSE)
  if (!is.null(termination) && !is.null(perennial))
    stop("`termination` and `perennial` cannot be used together.", call. = FALSE)
  irrigation_schedule <- .list_of(irrigation_schedule, "spacing_irrigation_event", "irrigation_schedule")
  intercrop <- .list_of(intercrop, "spacing_intercrop", "intercrop")
  ic_names <- vapply(intercrop, function(z) z$name, character(1))
  if (any(tolower(ic_names) == tolower(name)) || anyDuplicated(tolower(ic_names)))
    stop("Intercropped crops must be different APSIM models from each other and from the main crop.", call. = FALSE)
  if (isTRUE(verify_cultivar))
    for (k in seq_along(intercrop)) if (tolower(intercrop[[k]]$name) %in% tolower(spacing_cultivars()$crop))
      intercrop[[k]]$cultivar <- spacing_validate_cultivar(intercrop[[k]]$name, intercrop[[k]]$cultivar)
  for (k in seq_along(irrigation_schedule))
    if (identical(irrigation_schedule[[k]]$trigger, "Stage"))
      irrigation_schedule[[k]]$stage <- .spacing_check_stage(name, irrigation_schedule[[k]]$stage)
  if (!is.null(termination) && identical(termination$trigger, "Stage"))
    termination$stage <- .spacing_check_stage(name, termination$stage)
  # stage names of fertiliser applications are checked against the crop model
  for (i in seq_along(fertilisation$applications)) {
    a <- fertilisation$applications[[i]]
    if (identical(a$trigger, "Stage"))
      fertilisation$applications[[i]]$stage <- .spacing_check_stage(name, a$stage)
  }
  parameter_overrides <- .spacing_overrides(parameter_overrides)
  structure(list(name = name, cultivar = cultivar, sowing_window = sowing_window,
                 density = density, depth_mm = depth_mm, row_spacing_mm = row_spacing_mm,
                 fertilisation = fertilisation, irrigation = irrigation, tillage = tillage,
                 organic = organic, parameter_overrides = parameter_overrides,
                 organic_applications = organic_applications,
                 tillage_operations = tillage_operations,
                 sowing_date = sowing_date, sowing_rule = sowing_rule,
                 irrigation_schedule = irrigation_schedule, intercrop = intercrop,
                 termination = termination, perennial = perennial,
                 residue_removal = residue_removal),
            class = "spacing_crop")
}

.SOIL_REQUIRED <- c("DepthTop", "DepthBottom", "BD", "LL15", "DUL", "SAT", "OC", "PH", "NO3", "NH4")
.SOIL_OPTIONAL <- c("SW", "AirDry", "KS", "SWCON", "SoilCNRatio", "FBiom", "FInert", "FOM", "Urea",
                    "Sand", "Silt", "Clay")
.SOIL_WATER <- c("SummerDate", "SummerU", "SummerCona", "WinterDate", "WinterU", "WinterCona",
                 "DiffusConst", "DiffusSlope", "Salb", "CN2Bare", "CNRed", "CNCov", "FOMCNRatio")

#' Define a layered soil profile for Spacing.Tool2
#'
#' Every column is passed to APSIM. Optional columns (`SW`, `AirDry`, `KS`,
#' `SWCON`, `SoilCNRatio`, `FBiom`, `FInert`, `FOM`, `Urea`) must be complete
#' when present; when absent Tool2 uses its historic default and reports it
#' (`inputs.soil_defaults_applied` in `result.json`). The checks applied by Tool2
#' are repeated here so that invalid profiles fail in R:
#' `AirDry <= LL15 <= DUL < SAT`, `SAT <= 1 - BD/2.65`, `SW` within `[AirDry, SAT]`,
#' crop `LL` within `[AirDry, DUL]`.
#'
#' @param layers Data frame, one row per layer. Required columns: `DepthTop`,
#'   `DepthBottom` (cm), `BD` (g/cm3), `LL15`, `DUL`, `SAT` (m3/m3), `OC` (\%),
#'   `PH`, `NO3`, `NH4`. Optional: `SW`, `AirDry`, `KS` (mm/d), `SWCON`,
#'   `SoilCNRatio`, `FBiom`, `FInert`, `FOM` (kg/ha), `Urea`, `Sand`, `Silt`, `Clay`.
#' @param profile_id Profile identifier.
#' @param crop_parameters Named list by crop; each element a list with layered
#'   `LL`, `KL`, `XF` (any may be omitted).
#' @param water Named list of WaterBalance parameters: `SummerDate`, `SummerU`,
#'   `SummerCona`, `WinterDate`, `WinterU`, `WinterCona`, `DiffusConst`,
#'   `DiffusSlope`, `Salb`, `CN2Bare`, `CNRed`, `CNCov`, and `FOMCNRatio`.
#' @param initial_n_units `"kgha"` (default) or `"ppm"`, for `NO3`, `NH4`, `Urea`.
#' @param soil_type,data_source Descriptive text.
#' @return An object of class `spacing_soil_profile`: a list with
#'   `ProfileId`, `SoilType`, `DataSource`, `AreaFraction` (1),
#'   `InitialNUnits`, `Layers` (data frame, empty optional columns dropped),
#'   `CropParameters` and `Water`, i.e. the fields of the Tool2
#'   `soil_profile` object.
#' @seealso [as_spacing_soil_profile()] to convert a run-spec soil.
#' @family scenarios
#' @export
#' @examples
#' layers <- data.frame(DepthTop = c(0, 30), DepthBottom = c(30, 60),
#'   BD = c(1.30, 1.40), LL15 = c(.14, .16), DUL = c(.31, .33),
#'   SAT = c(.45, .44), OC = c(1.2, .7), PH = c(7.2, 7.4),
#'   NO3 = c(20, 8), NH4 = c(2, 1))
#' spacing_soil_profile(layers, "demo")
spacing_soil_profile <- function(layers, profile_id, crop_parameters = list(), water = list(),
                                 initial_n_units = c("kgha", "ppm"),
                                 soil_type = "", data_source = "rSpacing") {
  initial_n_units <- match.arg(initial_n_units)
  profile_id <- .spacing_optional_text(profile_id, "profile_id")
  if (!is.data.frame(layers) || !nrow(layers)) stop("`layers` must be a non-empty data frame.", call. = FALSE)
  miss <- setdiff(.SOIL_REQUIRED, names(layers))
  if (length(miss)) stop("Missing soil columns: ", paste(miss, collapse = ", "), call. = FALSE)
  unknown <- setdiff(names(layers), c(.SOIL_REQUIRED, .SOIL_OPTIONAL))
  if (length(unknown)) stop("Unknown soil columns: ", paste(unknown, collapse = ", "), call. = FALSE)
  for (nm in names(layers)) {
    v <- layers[[nm]]
    if (!is.numeric(v)) stop("Soil column `", nm, "` must be numeric.", call. = FALSE)
    if (anyNA(v) && !all(is.na(v)))
      stop("Soil column `", nm, "` must be given for every layer or for none.", call. = FALSE)
  }
  layers <- layers[, vapply(layers, function(v) !all(is.na(v)), logical(1)), drop = FALSE]
  n <- nrow(layers)
  if (any(layers$DepthBottom <= layers$DepthTop)) stop("DepthBottom must exceed DepthTop in every layer.", call. = FALSE)
  if (n > 1L && any(abs(layers$DepthTop[-1] - layers$DepthBottom[-n]) > 1e-9))
    stop("Layers must be contiguous (DepthTop[i] = DepthBottom[i-1]).", call. = FALSE)
  air <- if (is.null(layers$AirDry)) 0.5 * layers$LL15 else layers$AirDry
  sw <- if (is.null(layers$SW)) layers$DUL else layers$SW
  bad <- which(!(air <= layers$LL15 & layers$LL15 <= layers$DUL & layers$DUL < layers$SAT))
  if (length(bad)) stop("AirDry <= LL15 <= DUL < SAT violated in layer(s) ", paste(bad, collapse = ", "), ".", call. = FALSE)
  if (any(layers$BD <= 0 | layers$BD >= 2.65)) stop("BD must be in (0, 2.65).", call. = FALSE)
  bad <- which(layers$SAT > 1 - layers$BD / 2.65 + 1e-6)
  if (length(bad)) stop("SAT exceeds total porosity 1 - BD/2.65 in layer(s) ", paste(bad, collapse = ", "), ".", call. = FALSE)
  bad <- which(sw < air - 1e-9 | sw > layers$SAT + 1e-9)
  if (length(bad)) stop("SW outside [AirDry, SAT] in layer(s) ", paste(bad, collapse = ", "), ".", call. = FALSE)
  if (any(layers$NO3 < 0 | layers$NH4 < 0) || (!is.null(layers$Urea) && any(layers$Urea < 0)))
    stop("Initial NO3, NH4 and Urea must be non-negative.", call. = FALSE)
  crop_parameters <- .named_list(crop_parameters, "crop_parameters")
  for (crop in names(crop_parameters)) {
    cp <- crop_parameters[[crop]]
    unknown <- setdiff(names(cp), c("LL", "KL", "XF"))
    if (length(unknown)) stop("crop_parameters$", crop, ": unknown entries ", paste(unknown, collapse = ", "), call. = FALSE)
    for (k in names(cp)) .assert_numeric_vector(cp[[k]], sprintf("crop_parameters$%s$%s", crop, k), n = n)
    if (!is.null(cp$LL) && any(cp$LL < air - 1e-9 | cp$LL > layers$DUL + 1e-9))
      stop("crop_parameters$", crop, "$LL must be within [AirDry, DUL].", call. = FALSE)
    if (!is.null(cp$KL) && any(cp$KL < 0)) stop("crop_parameters$", crop, "$KL must be >= 0.", call. = FALSE)
    if (!is.null(cp$XF) && any(cp$XF < 0 | cp$XF > 1)) stop("crop_parameters$", crop, "$XF must be in [0, 1].", call. = FALSE)
    crop_parameters[[crop]] <- lapply(cp, as.numeric)
  }
  water <- .named_list(water, "water")
  unknown <- setdiff(names(water), .SOIL_WATER)
  if (length(unknown)) stop("Unknown water parameters: ", paste(unknown, collapse = ", "), call. = FALSE)
  structure(list(ProfileId = profile_id, SoilType = soil_type, DataSource = data_source,
                 AreaFraction = 1, InitialNUnits = initial_n_units,
                 Layers = layers, CropParameters = crop_parameters, Water = water),
            class = "spacing_soil_profile")
}

#' Convert a run-spec soil specification to a Tool2 soil profile
#'
#' @param x A `spacing_soil_spec` from [new_spacing_soil_spec()] (thickness in
#'   mm, mineral N in kg/ha).
#' @param ... Optional extra layered columns (`SWCON`, `FBiom`, `FInert`,
#'   `FOM`) and `water` passed to [spacing_soil_profile()].
#' @return A `spacing_soil_profile` (see [spacing_soil_profile()]): depths in
#'   cm from the cumulative thickness, `OC` from `Carbon`, initial N in kg/ha.
#' @family scenarios
#' @export
#' @examples
#' spec <- new_spacing_soil_spec("demo", thickness = c(150, 150),
#'   bd = c(1.30, 1.40), air_dry = c(.07, .08), ll15 = c(.14, .16), dul = c(.31, .33),
#'   sat = c(.45, .44), ks = c(100, 80), carbon = c(1.2, .8), soil_cn_ratio = c(12, 12),
#'   ph = c(7.2, 7.4), no3 = c(20, 8), nh4 = c(2, 1))
#' as_spacing_soil_profile(spec)$Layers
as_spacing_soil_profile <- function(x, ...) {
  if (!inherits(x, "spacing_soil_spec")) stop("`x` must be a spacing_soil_spec.", call. = FALSE)
  extra <- list(...); water <- extra$water %||% list(); extra$water <- NULL
  bottom <- cumsum(x$Thickness) / 10
  layers <- data.frame(DepthTop = c(0, utils::head(bottom, -1)), DepthBottom = bottom,
                       BD = x$BD, AirDry = x$AirDry, LL15 = x$LL15, DUL = x$DUL, SAT = x$SAT,
                       KS = x$KS, OC = x$Carbon, SoilCNRatio = x$SoilCNRatio, PH = x$PH,
                       NO3 = x$NO3, NH4 = x$NH4, Urea = x$Urea)
  if (!is.null(x$InitialWater)) layers$SW <- x$InitialWater
  for (k in names(extra)) layers[[k]] <- extra[[k]]
  spacing_soil_profile(layers, x$ProfileId, crop_parameters = x$CropParameters,
                       water = water, initial_n_units = "kgha",
                       data_source = x$Metadata$source %||% "rSpacing soil spec")
}

#' Define a complete Spacing.Tool2 scenario
#'
#' A scenario holds everything `Spacing.Tool2` (>= 0.09.07) reads from a
#' `--request` file: location, period, weather, soil and an ordered rotation
#' with full management for each crop. Run it with [run_spacing()] or write it
#' with [write_spacing_scenario()].
#'
#' Since Tool2 0.09.10 a crop may appear more than once in the sequence (each
#' slot has its own management), and a slot may include intercropped crops,
#' a cover crop termination or a perennial crop. Checked here: tillage is one of
#' three types (parameters can be changed); fertiliser, residue and stage names
#' must exist in APSIM.
#'
#' @param id Scenario identifier (also used as APSIM simulation name).
#' @param crops List of [spacing_crop()] objects, in rotation order. Names, if
#'   any, are ignored.
#' @param soil A [spacing_soil_profile()]; `NULL` lets Tool2 use the mock/API soil.
#' @param latitude,longitude Coordinates (decimal degrees).
#' @param start_year,end_year Simulation period.
#' @param cycles Number of rotation cycles (repetitions of the crop sequence).
#' @param mock Logical. Use the synthetic Tool2 weather (and mock soil if `soil` is `NULL`).
#' @param weather_file Optional existing APSIM `.met` file; overrides mock weather.
#' @param cell_id Optional cell identifier.
#' @param name Optional descriptive name.
#' @param metadata Optional named list stored in the Tool2 manifest.
#' @param max_crop_days Days a crop may stay in the field before Tool2 ends it
#'   (`EndReason = "max_days"`). Tool2 >= 0.09.08.
#' @param stall_days Consecutive days without phenological progress before Tool2
#'   ends the crop (`EndReason = "stalled"`); 0 disables the check. Tool2 >= 0.09.08.
#'   Crops ended by the crop model itself (e.g. frost, loss of leaf area) are always
#'   recorded as `EndReason = "failed"` with zero yield, and the rotation continues.
#' @param surface_residue Optional [spacing_surface_residue()] present at the
#'   start of the simulation (Tool2 >= 0.09.09); `NULL` keeps the Tool2 default.
#' @param report Optional [spacing_report_spec()] with extra output variables
#'   (Tool2 >= 0.09.10).
#' @return An object of class `spacing_scenario`: a list with `id`, `name`,
#'   `latitude`, `longitude`, `cell_id`, `start_year`, `end_year`, `cycles`,
#'   `max_crop_days`, `stall_days`, `mock`, `weather_file`, `soil`, `crops`,
#'   `metadata`, `surface_residue` and `report`.
#' @seealso [run_spacing()] to run it, [write_spacing_scenario()] to save the
#'   Tool2 request, `inst/examples/scenario_tool2_request.R` and
#'   `inst/examples/scenario_rotation_options.R` for complete examples.
#' @family scenarios
#' @export
#' @examples
#' soy <- spacing_crop("Soybean", "Generic_MG1", c("1-may", "15-jun"), 35, 35, 450,
#'                     tillage = spacing_tillage("no-till"))
#' wheat <- spacing_crop("Wheat", "Claire", c("10-oct", "20-nov"), 180, 35, 180,
#'   fertilisation = spacing_fertilisation(30, list(
#'     spacing_fertiliser_application(70, trigger = "Stage", stage = "MaximumSpikeletPrimordia"),
#'     spacing_fertiliser_application(50, trigger = "Stage", stage = "FlagLeafAppearance"))),
#'   tillage = spacing_tillage("minimum"), organic = spacing_organic_n(20))
#' sc <- new_spacing_scenario("demo", list(soy, wheat), latitude = 44.9, longitude = 10.63,
#'                            start_year = 2020, end_year = 2022)
#' sc
#' \dontrun{
#' run <- run_spacing(cfg, sc)
#' run$result$yields
#' }
new_spacing_scenario <- function(id, crops, soil = NULL, latitude, longitude,
                                 start_year, end_year, cycles = 1L, mock = TRUE,
                                 weather_file = NULL, cell_id = NULL, name = id,
                                 metadata = list(), max_crop_days = 400, stall_days = 150,
                                 surface_residue = NULL, report = NULL) {
  id <- .spacing_optional_text(id, "id")
  if (is.null(id) || grepl("[^A-Za-z0-9_.-]", id))
    stop("`id` must contain only letters, digits, '_', '-' or '.'.", call. = FALSE)
  if (inherits(crops, "spacing_crop")) crops <- list(crops)
  if (!is.list(crops) || !length(crops) || !all(vapply(crops, inherits, logical(1), "spacing_crop")))
    stop("`crops` must be a non-empty list of spacing_crop() objects.", call. = FALSE)
  # Names are for the user only: a named list would be written as a JSON object.
  crops <- unname(crops)
  # Tool2 >= 0.09.10: the same crop may appear in several slots of the sequence
  crop_names <- unique(unlist(lapply(crops, function(z) c(z$name, vapply(z$intercrop, function(i) i$name, character(1))))))
  if (!is.null(soil) && !inherits(soil, "spacing_soil_profile"))
    stop("`soil` must be NULL or a spacing_soil_profile().", call. = FALSE)
  if (!is.null(soil)) {
    extra <- setdiff(names(soil$CropParameters), crop_names)
    if (length(extra)) warning("Soil crop parameters for crops not in the rotation are ignored: ",
                               paste(extra, collapse = ", "), call. = FALSE)
  }
  latitude <- .spacing_assert_number(latitude, "latitude", -90, 90)
  longitude <- .spacing_assert_number(longitude, "longitude", -180, 180)
  start_year <- as.integer(.spacing_assert_number(start_year, "start_year", 1800, 2300))
  end_year <- as.integer(.spacing_assert_number(end_year, "end_year", start_year, 2300))
  cycles <- as.integer(.spacing_assert_number(cycles, "cycles", 1, 1000))
  max_crop_days <- as.integer(.spacing_assert_number(max_crop_days, "max_crop_days", 1, 3650))
  stall_days <- as.integer(.spacing_assert_number(stall_days, "stall_days", 0, 3650))
  if (!is.null(weather_file)) weather_file <- .normalise_existing(weather_file, "weather_file", "file")
  if (!is.null(surface_residue) && !inherits(surface_residue, "spacing_surface_residue"))
    stop("`surface_residue` must be NULL or a spacing_surface_residue().", call. = FALSE)
  if (!is.null(report) && !inherits(report, "spacing_report_spec"))
    stop("`report` must be NULL or a spacing_report_spec().", call. = FALSE)
  structure(list(id = id, name = name, latitude = latitude, longitude = longitude,
                 cell_id = cell_id, start_year = start_year, end_year = end_year,
                 cycles = cycles, max_crop_days = max_crop_days, stall_days = stall_days,
                 mock = isTRUE(mock), weather_file = weather_file,
                 soil = soil, crops = crops, metadata = .named_list(metadata, "metadata"),
                 surface_residue = surface_residue, report = report),
            class = "spacing_scenario")
}

#' Convert a scenario to the Spacing.Tool2 request structure
#'
#' @param x A [new_spacing_scenario()] object.
#' @param run_id Run identifier written in the request.
#' @param result_file,manifest_file Optional paths where Tool2 writes its JSON
#'   result and manifest.
#' @return A named list with the field names of the Tool2 request contract
#'   (see `RSPACING_INTEGRATION.md` of Spacing.Tool2): `id`, `name`, `run_id`,
#'   `metadata`, `mock`, `calibrate`, `lat`, `lon`, `cell_id`, `start_year`,
#'   `end_year`, `weather_file`, `soil_profile`, `result_file`,
#'   `manifest_file`, `crops` (each with `name`, `management` and the optional
#'   `parameter_overrides`, `intercrop`, `termination`, `perennial`,
#'   `residue_removal`), `cycles`, `rotation_safety`, `surface_residue` and
#'   `report`. `NULL` top-level fields are dropped.
#' @family scenarios
#' @export
#' @examples
#' sc <- new_spacing_scenario("demo",
#'   spacing_crop("Maize", "B_130", c("1-apr", "10-may"), 7.5, 50, 750,
#'                fertilisation = spacing_fertilisation(50)),
#'   latitude = 44.9, longitude = 10.63, start_year = 2020, end_year = 2021)
#' str(as_spacing_tool2_request(sc)$crops[[1]]$management)
as_spacing_tool2_request <- function(x, run_id = x$id, result_file = NULL, manifest_file = NULL) {
  if (!inherits(x, "spacing_scenario")) stop("`x` must be a spacing_scenario.", call. = FALSE)
  crop_json <- lapply(x$crops, function(cr) {
    m <- list(cultivar = cr$cultivar, tillage_type = cr$tillage$type,
              sowing_window_start = cr$sowing_window[1], sowing_window_end = cr$sowing_window[2],
              sowing_density = cr$density, sowing_depth = cr$depth_mm, row_spacing = cr$row_spacing_mm)
    m <- utils::modifyList(m, as.list(cr$fertilisation))
    if (!length(m$split_fertiliser)) m$split_fertiliser <- NULL
    m <- utils::modifyList(m, as.list(cr$irrigation))
    till <- list(tillage_incorporation_fraction = cr$tillage$incorporation_fraction,
                 tillage_depth_mm = cr$tillage$depth_mm,
                 tillage_cn_reduction = cr$tillage$cn_reduction,
                 tillage_cn_rain = cr$tillage$cn_rain)
    m <- c(m, Filter(Negate(is.null), till))
    m$nitrogen_organic_kg_ha <- cr$organic$amount_kg_n_ha
    if (cr$organic$amount_kg_n_ha > 0) {
      m$organic_residue_type <- cr$organic$residue_type
      m$organic_cn_ratio <- cr$organic$cn_ratio
    }
    # Tool2 >= 0.09.09: events with their own timing (written only when used)
    if (length(cr$organic_applications))
      m$organic_applications <- lapply(cr$organic_applications, function(a) Filter(Negate(is.null), list(
        trigger = a$trigger, days = a$days, date = a$date, amount_kg_n_ha = a$amount_kg_n_ha,
        residue_type = a$residue_type, cn_ratio = a$cn_ratio)))
    if (!is.null(cr$tillage_operations))
      m$tillage_operations <- lapply(cr$tillage_operations, function(o) Filter(Negate(is.null), list(
        type = o$type, trigger = o$trigger, days = o$days, date = o$date,
        incorporation_fraction = o$incorporation_fraction, depth_mm = o$depth_mm,
        cn_reduction = o$cn_reduction, cn_rain = o$cn_rain)))
    # Tool2 >= 0.09.10 (written only when used)
    if (!is.null(cr$sowing_date)) m$sowing_date <- cr$sowing_date
    if (!is.null(cr$sowing_rule)) m$sowing_rule <- unclass(cr$sowing_rule)
    if (length(cr$irrigation_schedule)) m$irrigation_schedule <- lapply(cr$irrigation_schedule, unclass)
    if (is.null(cr$sowing_window)) m$sowing_window_start <- m$sowing_window_end <- NULL
    out <- list(name = cr$name, management = m)
    if (length(cr$parameter_overrides)) out$parameter_overrides <- cr$parameter_overrides
    if (length(cr$intercrop))
      out$intercrop <- lapply(cr$intercrop, function(i) {
        z <- list(name = i$name, cultivar = i$cultivar, sowing_density = i$density,
                  sowing_depth = i$depth_mm, row_spacing = i$row_spacing_mm)
        if (length(i$parameter_overrides)) z$parameter_overrides <- i$parameter_overrides
        z
      })
    if (!is.null(cr$termination)) out$termination <- unclass(cr$termination)
    if (!is.null(cr$perennial)) out$perennial <- unclass(cr$perennial)
    if (!is.null(cr$residue_removal)) out$residue_removal <- unclass(cr$residue_removal)
    out
  })
  soil <- NULL
  if (!is.null(x$soil)) {
    s <- unclass(x$soil)
    s$CellId <- x$cell_id %||% x$id
    s$Layers <- lapply(seq_len(nrow(s$Layers)), function(i) as.list(s$Layers[i, , drop = FALSE]))
    if (!length(s$CropParameters)) s$CropParameters <- NULL
    if (!length(s$Water)) s$Water <- NULL
    soil <- s
  }
  req <- list(id = x$id, name = x$name, run_id = run_id,
              metadata = c(list(source = "rSpacing", rspacing_version = as.character(utils::packageVersion("rSpacing"))),
                           x$metadata),
              mock = x$mock, calibrate = FALSE, lat = x$latitude, lon = x$longitude,
              cell_id = x$cell_id, start_year = x$start_year, end_year = x$end_year,
              weather_file = x$weather_file, soil_profile = soil,
              result_file = result_file, manifest_file = manifest_file,
              crops = crop_json, cycles = x$cycles,
              rotation_safety = list(max_crop_days = x$max_crop_days %||% 400L,
                                     stall_days = x$stall_days %||% 150L),
              surface_residue = if (!is.null(x$surface_residue)) unclass(x$surface_residue),
              report = if (!is.null(x$report)) .spacing_report_json(x$report))
  Filter(Negate(is.null), req)
}

#' Write a scenario as a Spacing.Tool2 request file
#'
#' @param x A [new_spacing_scenario()] object.
#' @param path Destination JSON path.
#' @param overwrite Logical. Overwrite an existing file.
#' @param ... Passed to [as_spacing_tool2_request()].
#' @return The absolute path of the JSON file, invisibly.
#' @family scenarios
#' @export
#' @examples
#' sc <- new_spacing_scenario("demo",
#'   spacing_crop("Maize", "B_130", c("1-apr", "10-may"), 7.5, 50, 750),
#'   latitude = 44.9, longitude = 10.63, start_year = 2020, end_year = 2021)
#' path <- write_spacing_scenario(sc, tempfile(fileext = ".json"))
#' cat(head(readLines(path), 15), sep = "\n")
write_spacing_scenario <- function(x, path, overwrite = FALSE, ...) {
  path <- as.character(fs::path_abs(path))
  if (fs::file_exists(path) && !isTRUE(overwrite))
    stop("Request file already exists: ", path, call. = FALSE)
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  jsonlite::write_json(as_spacing_tool2_request(x, ...), path, auto_unbox = TRUE,
                       pretty = TRUE, digits = NA, null = "null")
  invisible(path)
}

#' @describeIn new_spacing_scenario Print location, period, weather, soil,
#'   residue, report options and one line per crop.
#' @param x A `spacing_scenario` object.
#' @param ... Ignored.
#' @export
print.spacing_scenario <- function(x, ...) {
  cat("<spacing_scenario> ", x$id, "\n", sep = "")
  cat("  location: ", x$latitude, ", ", x$longitude, "  period: ", x$start_year, "-", x$end_year,
      "  cycles: ", x$cycles, "\n", sep = "")
  cat("  weather:  ", if (!is.null(x$weather_file)) x$weather_file else if (x$mock) "mock" else "API", "\n", sep = "")
  cat("  soil:     ", if (is.null(x$soil)) "Tool2 mock/API" else
      sprintf("%s (%d layers, N in %s)", x$soil$ProfileId, nrow(x$soil$Layers), x$soil$InitialNUnits), "\n", sep = "")
  if (!is.null(x$surface_residue))
    cat("  residue:  ", x$surface_residue$mass_kg_ha, " kg/ha ", x$surface_residue$type,
        " (C:N ", x$surface_residue$cn_ratio, ")\n", sep = "")
  if (!is.null(x$report))
    cat("  report:   +", length(x$report$harvest_variables), " harvest, +", length(x$report$daily_variables),
        " daily, -", length(x$report$daily_exclude), " daily variables",
        if (!is.null(x$report$daily_event)) paste0(", event ", x$report$daily_event), "\n", sep = "")
  cat("  rotation:\n")
  for (cr in x$crops) {
    till <- if (is.null(cr$tillage_operations)) cr$tillage$type else
      if (!length(cr$tillage_operations)) "none" else paste0(length(cr$tillage_operations), " ops")
    sow <- if (!is.null(cr$sowing_date)) paste0("on ", cr$sowing_date) else
      paste0(cr$sowing_window[1], "..", cr$sowing_window[2], if (!is.null(cr$sowing_rule)) " (rule)" else "")
    extra <- c(if (length(cr$intercrop)) paste0("+", vapply(cr$intercrop, function(i) i$name, "")),
               if (!is.null(cr$termination)) "cover", if (!is.null(cr$perennial)) paste0("perennial ", cr$perennial$years, "y"),
               if (!is.null(cr$residue_removal)) paste0("residue ", cr$residue_removal$method),
               if (length(cr$irrigation_schedule)) paste0(length(cr$irrigation_schedule), " irr events"))
    cat(sprintf("    %-8s %-12s sow %s  till %-12s N %g+%d split  org %g+%d  irr %s%s\n",
                cr$name, cr$cultivar, sow, till,
                cr$fertilisation$at_sowing_kg_n_ha, length(cr$fertilisation$applications),
                cr$organic$amount_kg_n_ha, length(cr$organic_applications),
                if (isTRUE(cr$irrigation$enabled)) "auto" else "no",
                if (length(extra)) paste0("  [", paste(extra, collapse = ", "), "]") else ""))
  }
  invisible(x)
}

#' @describeIn spacing_crop Print the crop and cultivar.
#' @param x A `spacing_crop` object.
#' @param ... Ignored.
#' @export
print.spacing_crop <- function(x, ...) {
  cat("<spacing_crop> ", x$name, " / ", x$cultivar, "\n", sep = "")
  invisible(x)
}

#' Tool2 `report` object of a scenario
#' @param r A `spacing_report_spec`.
#' @return A list with the variable vectors wrapped in [I()] (so that single
#'   values stay JSON arrays) and `daily_event` when set.
#' @noRd
.spacing_report_json <- function(r) {
  out <- list(harvest_variables = I(r$harvest_variables), daily_variables = I(r$daily_variables),
              daily_exclude = I(r$daily_exclude), daily_event = r$daily_event)
  Filter(Negate(is.null), out)
}
