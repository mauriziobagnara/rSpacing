#' Define one mineral fertiliser application
#'
#' One N application of a [spacing_fertilisation()] programme. The dose is
#' always in kg N/ha: `Spacing.Tool2` (>= 0.09.09) applies
#' `amount_kg_n_ha / n_fraction` kg/ha of product, using the N fraction of the
#' fertiliser type ([spacing_fertiliser_types()]). For example 50 kg N/ha as
#' `"DAP"` (18 \% N) is 277.8 kg/ha of product.
#'
#' Triggers:
#' \describe{
#'   \item{`"Sowing"`}{on the sowing day, after sowing.}
#'   \item{`"DaysAfterSowing"`}{`days_after_sowing` days after sowing, while
#'     the crop is in the field.}
#'   \item{`"Date"`}{on the calendar date `date` (`d-mmm`), once per season,
#'     only while the crop is in the field (Tool2 >= 0.09.09).}
#'   \item{`"Stage"`}{when the main crop reaches the APSIM phenological stage
#'     `stage` ([spacing_crop_stages()]); applied on the first day after the
#'     stage is reached (Tool2 >= 0.09.09).}
#'   \item{`"DaysAfterPreviousHarvest"`}{`days_after_sowing` days after the
#'     end of the previous crop, also before sowing (Tool2 >= 0.09.10).}
#' }
#' An application whose trigger is not reached before the crop ends is skipped
#' and reported as a warning in the APSIM summary.
#'
#' @param amount_kg_n_ha Numeric scalar >= 0. Mineral N dose (kg N/ha).
#' @param fertiliser Character scalar. APSIM fertiliser type containing mineral
#'   N, one of `spacing_fertiliser_types()$fertiliser` (case-insensitive);
#'   default `"UreaN"`.
#' @param trigger One of `"Sowing"`, `"DaysAfterSowing"`, `"Date"`, `"Stage"`,
#'   `"DaysAfterPreviousHarvest"`.
#' @param days_after_sowing Non-negative integer. Days after sowing for
#'   `"DaysAfterSowing"`, days after the previous crop for
#'   `"DaysAfterPreviousHarvest"`; ignored (set to 0) otherwise.
#' @param date Character scalar `d-mmm` (e.g. `"1-mar"`), required for
#'   `"Date"`.
#' @param stage Character scalar, required for `"Stage"`: an APSIM stage name.
#'   It is checked against the crop when the application is passed to
#'   [spacing_crop()].
#' @param depth_mm Numeric scalar >= 0. Application depth (mm); 0 = surface.
#'   APSIM places the fertiliser in the soil layer that contains this depth.
#' @return An object of class `spacing_fertiliser_application`: a list with
#'   `trigger`, `days_after_sowing`, `amount_kg_n_ha`, `fert_type` (the APSIM
#'   spelling of the type), `date`, `stage` and `depth_mm`.
#' @seealso [spacing_fertilisation()], [spacing_fertiliser_types()],
#'   [spacing_crop_stages()].
#' @family crop management
#' @export
#' @examples
#' spacing_fertiliser_application(100, trigger = "DaysAfterSowing", days_after_sowing = 35)
#' spacing_fertiliser_application(40, "UAN_N", trigger = "Stage",
#'                                stage = "FlagLeafAppearance", depth_mm = 50)
#' spacing_fertiliser_application(60, "NH4NO3N", trigger = "Date", date = "1-mar")
spacing_fertiliser_application <- function(
    amount_kg_n_ha,
    fertiliser = "UreaN",
    trigger = c("Sowing", "DaysAfterSowing", "Date", "Stage", "DaysAfterPreviousHarvest"),
    days_after_sowing = 0L,
    date = NULL,
    stage = NULL,
    depth_mm = 0) {
  trigger <- match.arg(trigger)
  amount_kg_n_ha <- .spacing_assert_number(
    amount_kg_n_ha, "amount_kg_n_ha", lower = 0
  )
  fertiliser <- .spacing_fertiliser_type(fertiliser)
  if (!is.numeric(days_after_sowing) || length(days_after_sowing) != 1L ||
      is.na(days_after_sowing) || days_after_sowing < 0 ||
      days_after_sowing != as.integer(days_after_sowing)) {
    stop("`days_after_sowing` must be one non-negative integer.", call. = FALSE)
  }
  depth_mm <- .spacing_assert_number(depth_mm, "depth_mm", lower = 0)
  if (!trigger %in% c("DaysAfterSowing", "DaysAfterPreviousHarvest")) days_after_sowing <- 0L
  if (trigger == "Date") {
    if (is.null(date)) stop("`date` is required with trigger = \"Date\".", call. = FALSE)
    date <- .spacing_normalize_day_month(date, "date")
  } else date <- NULL
  if (trigger == "Stage") {
    stage <- .spacing_optional_text(stage, "stage")
    if (is.null(stage)) stop("`stage` is required with trigger = \"Stage\".", call. = FALSE)
  } else stage <- NULL
  structure(
    list(
      trigger = trigger,
      days_after_sowing = as.integer(days_after_sowing),
      amount_kg_n_ha = amount_kg_n_ha,
      fert_type = fertiliser,
      date = date,
      stage = stage,
      depth_mm = depth_mm
    ),
    class = "spacing_fertiliser_application"
  )
}

#' Define a crop fertilisation programme
#'
#' A sowing-day dose plus any number of later applications, for the
#' `fertilisation` argument of [spacing_crop()]. In the Tool2 request the
#' sowing dose becomes `nitrogen_mineral_kg_ha` and the applications become
#' `split_fertiliser`. The sowing dose is added to the applications unless one
#' of them already has `trigger = "Sowing"`.
#'
#' The fields `basal_fertiliser_type` and `basal_fertiliser_depth_mm` (Tool2
#' >= 0.09.09) are written only when they differ from the defaults (`"UreaN"`,
#' 0 mm), so programmes without them give the same request as rSpacing
#' 0.10.07.
#'
#' @param at_sowing_kg_n_ha Numeric scalar >= 0. Mineral N applied on the
#'   sowing day (kg N/ha).
#' @param applications A list of [spacing_fertiliser_application()] objects, or
#'   a single one.
#' @param fertiliser Character scalar. Fertiliser type of the sowing dose
#'   ([spacing_fertiliser_types()]).
#' @param depth_mm Numeric scalar >= 0. Depth (mm) of the sowing dose.
#' @return An object of class `spacing_fertilisation`: a list with
#'   `at_sowing_kg_n_ha`, `fertiliser`, `depth_mm` and `applications`.
#' @seealso [spacing_apply_fertilisation()] to merge it into a management list.
#' @family crop management
#' @export
#' @examples
#' wheat_n <- spacing_fertilisation(30, list(
#'   spacing_fertiliser_application(70, trigger = "Stage", stage = "MaximumSpikeletPrimordia"),
#'   spacing_fertiliser_application(50, trigger = "Stage", stage = "FlagLeafAppearance")))
#' wheat_n
#' as.list(wheat_n)
#' as.list(spacing_fertilisation(50, fertiliser = "DAP", depth_mm = 50))
spacing_fertilisation <- function(
    at_sowing_kg_n_ha = 0,
    applications = list(),
    fertiliser = "UreaN",
    depth_mm = 0) {
  at_sowing_kg_n_ha <- .spacing_assert_number(
    at_sowing_kg_n_ha, "at_sowing_kg_n_ha", lower = 0
  )
  if (inherits(applications, "spacing_fertiliser_application")) applications <- list(applications)
  if (!is.list(applications)) {
    stop("`applications` must be a list.", call. = FALSE)
  }
  valid <- vapply(
    applications,
    inherits,
    logical(1),
    what = "spacing_fertiliser_application"
  )
  if (length(valid) && !all(valid)) {
    stop(
      "Every element of `applications` must be created with ",
      "spacing_fertiliser_application().",
      call. = FALSE
    )
  }
  fertiliser <- .spacing_fertiliser_type(fertiliser)
  depth_mm <- .spacing_assert_number(depth_mm, "depth_mm", lower = 0)
  structure(
    list(
      at_sowing_kg_n_ha = at_sowing_kg_n_ha,
      fertiliser = fertiliser,
      depth_mm = depth_mm,
      applications = applications
    ),
    class = "spacing_fertilisation"
  )
}

#' Add a fertilisation programme to a management list
#'
#' Merges the Tool2 fields of a [spacing_fertilisation()] object
#' (`nitrogen_mineral_kg_ha`, `split_fertiliser` and, when set,
#' `basal_fertiliser_type`, `basal_fertiliser_depth_mm`) into a crop
#' management list built by hand, leaving the other fields unchanged.
#' [spacing_crop()] does this automatically.
#'
#' @param management Named list of Tool2 crop management fields.
#' @param fertilisation A `spacing_fertilisation` object.
#' @return The updated management list.
#' @family crop management
#' @export
#' @examples
#' m <- list(cultivar = "B_130", sowing_window_start = "1-apr", sowing_window_end = "10-may")
#' str(spacing_apply_fertilisation(m, spacing_fertilisation(50)))
spacing_apply_fertilisation <- function(management, fertilisation) {
  if (!is.list(management) || is.null(names(management))) {
    stop("`management` must be a named list.", call. = FALSE)
  }
  if (!inherits(fertilisation, "spacing_fertilisation")) {
    stop(
      "`fertilisation` must be created with spacing_fertilisation().",
      call. = FALSE
    )
  }
  utils::modifyList(management, as.list(fertilisation), keep.null = TRUE)
}

#' @describeIn spacing_fertilisation The Tool2 request fields of the programme.
#' @param x A `spacing_fertilisation` object.
#' @param ... Ignored.
#' @export
as.list.spacing_fertilisation <- function(x, ...) {
  out <- list(
    nitrogen_mineral_kg_ha = x$at_sowing_kg_n_ha,
    split_fertiliser = lapply(x$applications, as.list)
  )
  # Fields added in Tool2 0.09.09 are written only when they differ from the
  # defaults, so requests without them are identical to rSpacing 0.10.07.
  if (!identical(x$fertiliser %||% "UreaN", "UreaN")) out$basal_fertiliser_type <- x$fertiliser
  if (isTRUE((x$depth_mm %||% 0) > 0)) out$basal_fertiliser_depth_mm <- x$depth_mm
  out
}

#' @describeIn spacing_fertiliser_application The Tool2 request fields of the
#'   application (`date`, `stage` and `depth_mm` only when set).
#' @param x A `spacing_fertiliser_application` object.
#' @param ... Ignored.
#' @export
as.list.spacing_fertiliser_application <- function(x, ...) {
  out <- list(trigger = x$trigger, days_after_sowing = x$days_after_sowing,
              amount_kg_n_ha = x$amount_kg_n_ha, fert_type = x$fert_type)
  if (!is.null(x$date)) out$date <- x$date
  if (!is.null(x$stage)) out$stage <- x$stage
  if (isTRUE((x$depth_mm %||% 0) > 0)) out$depth_mm <- x$depth_mm
  out
}

#' @describeIn spacing_fertilisation Print the sowing dose and the timing of each
#'   application.
#' @export
print.spacing_fertilisation <- function(x, ...) {
  cat("<spacing_fertilisation>\n")
  cat("  at sowing:   ", x$at_sowing_kg_n_ha, " kg N/ha as ", x$fertiliser,
      if (isTRUE(x$depth_mm > 0)) paste0(" at ", x$depth_mm, " mm"), "\n", sep = "")
  cat("  later events:", length(x$applications), "\n")
  for (a in x$applications) {
    when <- switch(a$trigger, Sowing = "sowing", DaysAfterSowing = paste(a$days_after_sowing, "DAS"),
                   Date = a$date, Stage = paste("stage", a$stage))
    cat(sprintf("    %g kg N/ha %s at %s\n", a$amount_kg_n_ha, a$fert_type, when))
  }
  invisible(x)
}

#' Validate a fertiliser type against the APSIM table
#' @param fertiliser Character scalar (case-insensitive).
#' @return The type as spelled in APSIM; an error lists the N fertilisers.
#' @noRd
.spacing_fertiliser_type <- function(fertiliser) {
  fertiliser <- .spacing_optional_text(fertiliser, "fertiliser")
  types <- spacing_fertiliser_types()
  hit <- types$fertiliser[tolower(types$fertiliser) == tolower(fertiliser %||% "")]
  if (!length(hit)) {
    stop(
      "Unsupported fertiliser `", fertiliser,
      "`. APSIM fertiliser types containing mineral N: ",
      paste(types$fertiliser, collapse = ", "), ".",
      call. = FALSE
    )
  }
  hit[[1]]
}
