# Rotation options of Spacing.Tool2 >= 0.09.10 (rSpacing 0.10.09): sowing rules,
# scheduled irrigation, intercrops, cover crops, perennials, residue removal and
# configurable report variables.

#' Check a single integer within bounds
#' @param x Value to check.
#' @param argument Character scalar. Argument name for the error message.
#' @param lower,upper Numeric bounds (inclusive).
#' @return `x` as an integer.
#' @noRd
.spacing_count <- function(x, argument, lower = 0, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != as.integer(x) || x < lower || x > upper)
    stop("`", argument, "` must be one integer in [", lower, ", ", upper, "].", call. = FALSE)
  as.integer(x)
}

#' Define sowing conditions
#'
#' Conditions checked by `Spacing.Tool2` (>= 0.09.10) on each day of the sowing
#' window; the crop is sown on the first day on which all of them are met. A
#' criterion set to 0 is not applied.
#'
#' @param min_esw_mm Minimum extractable soil water (SW - LL15, mm) in the top
#'   `esw_depth_mm` of the profile.
#' @param esw_depth_mm Depth (mm) used for `min_esw_mm`; 0 = whole profile.
#' @param min_rain_mm Minimum rain (mm) over the last `rain_days` days, today included.
#' @param rain_days Days (1-60) for `min_rain_mm`.
#' @param min_soil_temp_c Minimum mean temperature (°C) of the top soil layer
#'   over the previous `temp_days` days (APSIM soil temperature model).
#' @param temp_days Days (1-60) for `min_soil_temp_c`.
#' @param sow_at_window_end Logical. Sow on the last day of the window when the
#'   conditions were never met; otherwise sowing moves to the next year's window.
#' @return An object of class `spacing_sowing_rule`.
#' @export
#' @examples
#' spacing_sowing_rule(min_esw_mm = 40, min_rain_mm = 5, rain_days = 5,
#'                     min_soil_temp_c = 12, sow_at_window_end = TRUE)
#' @family crop management
spacing_sowing_rule <- function(min_esw_mm = 0, esw_depth_mm = 0, min_rain_mm = 0, rain_days = 3,
                                min_soil_temp_c = 0, temp_days = 3, sow_at_window_end = FALSE) {
  x <- list(min_esw_mm = .spacing_assert_number(min_esw_mm, "min_esw_mm", lower = 0),
            esw_depth_mm = .spacing_assert_number(esw_depth_mm, "esw_depth_mm", lower = 0),
            min_rain_mm = .spacing_assert_number(min_rain_mm, "min_rain_mm", lower = 0),
            rain_days = .spacing_count(rain_days, "rain_days", 1, 60),
            min_soil_temp_c = .spacing_assert_number(min_soil_temp_c, "min_soil_temp_c", lower = 0),
            temp_days = .spacing_count(temp_days, "temp_days", 1, 60),
            sow_at_window_end = isTRUE(sow_at_window_end))
  structure(x, class = "spacing_sowing_rule")
}

#' Define one scheduled irrigation
#'
#' A fixed irrigation applied by `Spacing.Tool2` (>= 0.09.10), in addition to
#' or instead of automatic irrigation ([spacing_irrigation()]). Triggers are
#' those of [spacing_fertiliser_application()]: `"Sowing"`, `"DaysAfterSowing"`,
#' `"Date"` and `"Stage"` apply while the crop is in the field;
#' `"DaysAfterPreviousHarvest"` can fall before sowing.
#'
#' @param amount_mm Irrigation amount (mm), > 0.
#' @param trigger One of `"Date"`, `"DaysAfterSowing"`, `"Stage"`, `"Sowing"`,
#'   `"DaysAfterPreviousHarvest"`.
#' @param date Calendar date `d-mmm` for `trigger = "Date"`.
#' @param days Days for the `Days*` triggers.
#' @param stage APSIM stage name for `trigger = "Stage"` (see [spacing_crop_stages()]).
#' @return An object of class `spacing_irrigation_event`.
#' @export
#' @examples
#' spacing_irrigation_event(40, "Stage", stage = "Flowering")
#' @family crop management
spacing_irrigation_event <- function(amount_mm,
                                     trigger = c("Date", "DaysAfterSowing", "Stage", "Sowing",
                                                 "DaysAfterPreviousHarvest"),
                                     date = NULL, days = 0L, stage = NULL) {
  trigger <- match.arg(trigger)
  amount_mm <- .spacing_assert_number(amount_mm, "amount_mm", lower = 0, lower_inclusive = FALSE)
  x <- list(trigger = trigger, amount_mm = amount_mm)
  if (trigger %in% c("DaysAfterSowing", "DaysAfterPreviousHarvest")) x$days <- .spacing_count(days, "days")
  if (trigger == "Date") {
    if (is.null(date)) stop("`date` is required with trigger = \"Date\".", call. = FALSE)
    x$date <- .spacing_normalize_day_month(date, "date")
  }
  if (trigger == "Stage") {
    x$stage <- .spacing_optional_text(stage, "stage")
    if (is.null(x$stage)) stop("`stage` is required with trigger = \"Stage\".", call. = FALSE)
  }
  structure(x, class = "spacing_irrigation_event")
}

#' Define an intercropped crop
#'
#' A crop sown together with the main crop of a [spacing_crop()] slot
#' (`Spacing.Tool2` >= 0.09.10). Each crop of the slot ends on its own
#' (harvest, failure, ...); the next slot starts when all have ended. Two crops
#' of the same slot must be different APSIM models. Fertilisation, irrigation,
#' tillage and organic inputs of the slot apply to the whole field; `"Stage"`
#' triggers refer to the main crop.
#'
#' @param name APSIM crop model name.
#' @param cultivar APSIM cultivar name.
#' @param density Sowing population (plants/m2).
#' @param depth_mm Sowing depth (mm).
#' @param row_spacing_mm Row spacing (mm).
#' @param parameter_overrides Named numeric overrides, as in [spacing_crop()].
#' @return An object of class `spacing_intercrop`.
#' @export
#' @examples
#' spacing_intercrop("Maize", "B_130", density = 3.5, depth_mm = 50, row_spacing_mm = 750)
#' @family crop management
spacing_intercrop <- function(name, cultivar, density, depth_mm, row_spacing_mm, parameter_overrides = list()) {
  name <- .spacing_optional_text(name, "name"); cultivar <- .spacing_optional_text(cultivar, "cultivar")
  if (is.null(name) || is.null(cultivar)) stop("`name` and `cultivar` are required.", call. = FALSE)
  x <- list(name = name, cultivar = cultivar,
            density = .spacing_assert_number(density, "density", lower = 0, lower_inclusive = FALSE),
            depth_mm = .spacing_assert_number(depth_mm, "depth_mm", lower = 0, lower_inclusive = FALSE),
            row_spacing_mm = .spacing_assert_number(row_spacing_mm, "row_spacing_mm", lower = 0, lower_inclusive = FALSE),
            parameter_overrides = .spacing_overrides(parameter_overrides))
  structure(x, class = "spacing_intercrop")
}

#' Define the termination of a cover crop
#'
#' The crop is ended by `Spacing.Tool2` (>= 0.09.10) without harvest: its
#' biomass stays in the field as residue. The end is recorded with
#' `EndReason = "terminated"` and is excluded from the mean yields.
#'
#' @param trigger One of `"Date"`, `"DaysAfterSowing"`, `"Stage"`.
#' @param date Calendar date `d-mmm` for `trigger = "Date"`.
#' @param days Days after sowing (>= 1) for `trigger = "DaysAfterSowing"`.
#' @param stage APSIM stage name for `trigger = "Stage"`.
#' @return An object of class `spacing_termination`.
#' @export
#' @examples
#' spacing_termination("Date", date = "15-mar")
#' @family crop management
spacing_termination <- function(trigger = c("Date", "DaysAfterSowing", "Stage"),
                                date = NULL, days = NULL, stage = NULL) {
  trigger <- match.arg(trigger)
  x <- list(trigger = trigger)
  if (trigger == "Date") {
    if (is.null(date)) stop("`date` is required with trigger = \"Date\".", call. = FALSE)
    x$date <- .spacing_normalize_day_month(date, "date")
  }
  if (trigger == "DaysAfterSowing") {
    if (is.null(days)) stop("`days` is required with trigger = \"DaysAfterSowing\".", call. = FALSE)
    x$days <- .spacing_count(days, "days", lower = 1)
  }
  if (trigger == "Stage") {
    x$stage <- .spacing_optional_text(stage, "stage")
    if (is.null(x$stage)) stop("`stage` is required with trigger = \"Stage\".", call. = FALSE)
  }
  structure(x, class = "spacing_termination")
}

#' Define a perennial crop
#'
#' The crop stays in the field for `years` years from sowing
#' (`Spacing.Tool2` >= 0.09.10). Every year on `harvest_date` the harvested
#' organ (Grain or, e.g. for Grapevine, Berry) is removed and recorded with
#' `EndReason = "harvested"`; the crop is not ended by the harvest. After
#' `years` years it is ended (`EndReason = "perennial_end"`). Stall and
#' maximum-days checks do not apply to perennials.
#'
#' @param years Number of years in the field (>= 1).
#' @param harvest_date Annual harvest date `d-mmm`.
#' @return An object of class `spacing_perennial`.
#' @export
#' @examples
#' spacing_perennial(10, harvest_date = "15-sep")
#' @family crop management
spacing_perennial <- function(years, harvest_date) {
  structure(list(years = .spacing_count(years, "years", lower = 1),
                 harvest_date = .spacing_normalize_day_month(harvest_date, "harvest_date")),
            class = "spacing_perennial")
}

#' Define residue removal or burning after harvest
#'
#' At the end of the crop (after harvest), `Spacing.Tool2` (>= 0.09.10) removes
#' `fraction` of the surface residue present at that moment (all residue pools).
#' APSIM does not model burning separately: in both cases the C and N removed
#' leave the system. The daily output column `ResidueRemoved_kgha` reports it.
#'
#' @param fraction Fraction of the surface residue removed, in `[0, 1]`.
#' @param method `"removal"` (e.g. baling) or `"burn"`.
#' @return An object of class `spacing_residue_removal`.
#' @export
#' @examples
#' spacing_residue_removal(0.7)
#' @family crop management
spacing_residue_removal <- function(fraction, method = c("removal", "burn")) {
  method <- match.arg(method)
  structure(list(fraction = .spacing_assert_number(fraction, "fraction", 0, 1), method = method),
            class = "spacing_residue_removal")
}

#' Configure report variables
#'
#' Extra APSIM report variables and the daily report event
#' (`Spacing.Tool2` >= 0.09.10). Variables use the APSIM Report syntax, e.g.
#' `"[Wheat].Grain.N * 10 as Wheat_GrainN_kgha"`. Invalid variables make the
#' APSIM run fail with the APSIM message.
#'
#' @param harvest_variables Variables added to the harvest `Report` (one row per crop end).
#' @param daily_variables Variables added to the `DailyReport`.
#' @param daily_exclude Default daily variables to drop, by alias (e.g. `"VP_hPa"`)
#'   or expression.
#' @param daily_event Event of the `DailyReport`, e.g. `"[Clock].EndOfMonth"`;
#'   `NULL` keeps `"[Clock].EndOfDay"`. With an event other than daily,
#'   `crop_summary_*.csv` cannot be built.
#' @return An object of class `spacing_report_spec`.
#' @export
#' @examples
#' spacing_report_spec(harvest_variables = "[Wheat].Grain.N * 10 as Wheat_GrainN_kgha",
#'                daily_variables = "[Weather].MeanT as Tmean", daily_exclude = "VP_hPa")
#' @family scenarios
spacing_report_spec <- function(harvest_variables = character(), daily_variables = character(),
                           daily_exclude = character(), daily_event = NULL) {
  chk <- function(x, arg) {
    if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x))) || any(grepl("[\"\n]", x)))
      stop("`", arg, "` must be a character vector of non-empty APSIM variables.", call. = FALSE)
    alias <- trimws(sub("^.*\\s+as\\s+", "", x, ignore.case = TRUE))
    if (anyDuplicated(tolower(alias))) stop("`", arg, "` has repeated aliases.", call. = FALSE)
    as.character(x)
  }
  if (!is.null(daily_event)) {
    daily_event <- .spacing_optional_text(daily_event, "daily_event")
    if (!grepl("^\\[[A-Za-z0-9_.]+\\]\\.[A-Za-z0-9_.]+$", daily_event))
      stop("`daily_event` must look like \"[Clock].EndOfMonth\".", call. = FALSE)
  }
  structure(list(harvest_variables = chk(harvest_variables, "harvest_variables"),
                 daily_variables = chk(daily_variables, "daily_variables"),
                 daily_exclude = chk(daily_exclude, "daily_exclude"),
                 daily_event = daily_event),
            class = "spacing_report_spec")
}

#' Validate cultivar parameter overrides
#' @param parameter_overrides Named list or named numeric vector; names are
#'   APSIM paths without leading brackets.
#' @return A named list of single finite numbers (empty list when none).
#' @noRd
.spacing_overrides <- function(parameter_overrides) {
  if (!length(parameter_overrides)) return(list())
  if (is.null(names(parameter_overrides)) || any(!nzchar(names(parameter_overrides))))
    stop("`parameter_overrides` must be named.", call. = FALSE)
  ok <- vapply(parameter_overrides, function(v) is.numeric(v) && length(v) == 1L && is.finite(v), logical(1))
  if (!all(ok)) stop("Every parameter override must be one finite number.", call. = FALSE)
  as.list(parameter_overrides)
}
