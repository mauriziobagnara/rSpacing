#' Define automatic (threshold) irrigation
#'
#' Irrigation applied by `Spacing.Tool2` when the fraction of plant-available
#' water of the whole soil profile, `(SW - LL15) / (DUL - LL15)`, falls below
#' `threshold`. Each event brings the fraction back to `target`, capped at
#' `max_application_mm`, and events happen only while the crop is in the field
#' and inside the calendar window. For irrigation on fixed dates or stages use
#' [spacing_irrigation_event()]; the two can be combined.
#'
#' The object maps to the Tool2 fields `auto_irrigate`,
#' `irrigation_threshold`, `irrigation_target`, `max_irrigation_mm`,
#' `irrigation_window_start` and `irrigation_window_end`. When
#' `enabled = FALSE`, [as.list()] returns only `list(auto_irrigate = FALSE)`,
#' so inactive settings cannot be mistaken for an active strategy. Without a
#' window, Tool2 uses its default sowing window of the crop.
#'
#' Irrigation efficiency, method, wetted fraction and water quality are not
#' part of the Tool2 contract and are not represented.
#'
#' @param enabled Logical scalar. `FALSE` disables automatic irrigation.
#' @param threshold Numeric scalar in `[0, 1)`. Fraction of plant-available
#'   water below which irrigation starts.
#' @param target Numeric scalar in `(0, 1]`, greater than `threshold`.
#'   Fraction of plant-available water restored by each event.
#' @param max_application_mm Positive numeric scalar. Maximum depth of one
#'   event (mm).
#' @param window_start,window_end Optional character scalars in `d-mmm`,
#'   `dd-mmm`, `d-mm` or `dd-mm` format (e.g. `"1-jun"`, `"31-aug"`),
#'   normalised to `d-mmm`. Give both or neither; windows crossing the end of
#'   the year are allowed.
#' @param label Optional character label kept by rSpacing, not sent to Tool2.
#' @return An object of class `spacing_irrigation`: a list with `enabled`,
#'   `threshold`, `target`, `max_application_mm`, `window_start`,
#'   `window_end` and `label` (`NULL` values when disabled).
#' @seealso [spacing_apply_irrigation()], [spacing_irrigation_event()].
#' @family crop management
#' @export
#' @examples
#' maize_irrigation <- spacing_irrigation(threshold = 0.50, target = 0.85,
#'   max_application_mm = 35, window_start = "1-jun", window_end = "31-aug",
#'   label = "Maize summer irrigation")
#' maize_irrigation
#' as.list(maize_irrigation)
#' summary(maize_irrigation)
#' as.list(spacing_irrigation(enabled = FALSE, label = "Rainfed"))
spacing_irrigation <- function(
    enabled = TRUE,
    threshold = 0.50,
    target = 0.85,
    max_application_mm = 35,
    window_start = NULL,
    window_end = NULL,
    label = NULL) {

  enabled <- .spacing_assert_flag(enabled, "enabled")
  label <- .spacing_optional_text(label, "label")

  if (!enabled) {
    return(structure(
      list(
        enabled = FALSE,
        threshold = NULL,
        target = NULL,
        max_application_mm = NULL,
        window_start = NULL,
        window_end = NULL,
        label = label
      ),
      class = "spacing_irrigation"
    ))
  }

  threshold <- .spacing_assert_number(
    threshold, "threshold", lower = 0, upper = 1,
    lower_inclusive = TRUE, upper_inclusive = FALSE
  )
  target <- .spacing_assert_number(
    target, "target", lower = 0, upper = 1,
    lower_inclusive = FALSE, upper_inclusive = TRUE
  )
  max_application_mm <- .spacing_assert_number(
    max_application_mm, "max_application_mm", lower = 0,
    lower_inclusive = FALSE
  )

  if (target <= threshold) {
    stop(
      "`target` must be greater than `threshold`; received target = ",
      target, " and threshold = ", threshold, ".",
      call. = FALSE
    )
  }

  if (xor(is.null(window_start), is.null(window_end))) {
    stop(
      "`window_start` and `window_end` must either both be supplied or both be NULL.",
      call. = FALSE
    )
  }

  if (!is.null(window_start)) {
    window_start <- .spacing_normalize_day_month(window_start, "window_start")
    window_end <- .spacing_normalize_day_month(window_end, "window_end")
  }

  structure(
    list(
      enabled = TRUE,
      threshold = threshold,
      target = target,
      max_application_mm = max_application_mm,
      window_start = window_start,
      window_end = window_end,
      label = label
    ),
    class = "spacing_irrigation"
  )
}

#' Add an irrigation strategy to a management list
#'
#' Merges the Tool2 fields of a [spacing_irrigation()] object into a crop
#' management list built by hand, leaving the other fields unchanged.
#' [spacing_crop()] does this automatically.
#'
#' @param management Named list of Tool2 crop management fields.
#' @param irrigation A `spacing_irrigation` object.
#' @return The updated management list.
#' @family crop management
#' @export
#' @examples
#' wheat <- list(cultivar = "Claire")
#' str(spacing_apply_irrigation(wheat, spacing_irrigation(threshold = 0.35, target = 0.70,
#'   max_application_mm = 25, window_start = "1-mar", window_end = "31-may")))
spacing_apply_irrigation <- function(management, irrigation) {
  if (!is.list(management) || is.null(names(management))) {
    stop("`management` must be a named list.", call. = FALSE)
  }
  if (!inherits(irrigation, "spacing_irrigation")) {
    stop(
      "`irrigation` must be created with spacing_irrigation().",
      call. = FALSE
    )
  }

  utils::modifyList(
    management,
    as.list(irrigation),
    keep.null = TRUE
  )
}

#' @describeIn spacing_irrigation The Tool2 request fields.
#' @param x A `spacing_irrigation` object.
#' @param ... Ignored.
#' @export
as.list.spacing_irrigation <- function(x, ...) {
  if (!inherits(x, "spacing_irrigation")) {
    stop("`x` is not a spacing_irrigation object.", call. = FALSE)
  }

  if (!isTRUE(x$enabled)) {
    return(list(auto_irrigate = FALSE))
  }

  list(
    auto_irrigate = TRUE,
    irrigation_threshold = x$threshold,
    irrigation_target = x$target,
    max_irrigation_mm = x$max_application_mm,
    irrigation_window_start = x$window_start,
    irrigation_window_end = x$window_end
  )
}

#' @describeIn spacing_irrigation Print the strategy.
#' @export
print.spacing_irrigation <- function(x, ...) {
  cat("<spacing_irrigation>\n")
  if (!isTRUE(x$enabled)) {
    cat("  status: disabled\n")
    if (!is.null(x$label)) cat("  label:  ", x$label, "\n", sep = "")
    return(invisible(x))
  }

  cat("  status:                 enabled\n")
  cat("  threshold:              ", format(x$threshold), "\n", sep = "")
  cat("  target:                 ", format(x$target), "\n", sep = "")
  cat("  maximum application:    ", format(x$max_application_mm), " mm\n", sep = "")
  cat(
    "  irrigation window:      ",
    if (is.null(x$window_start)) "unrestricted" else
      paste(x$window_start, "to", x$window_end),
    "\n", sep = ""
  )
  if (!is.null(x$label)) cat("  label:                  ", x$label, "\n", sep = "")
  invisible(x)
}

#' @describeIn spacing_irrigation One-row data frame with the settings (`NA`
#'   when disabled).
#' @param object A `spacing_irrigation` object.
#' @export
summary.spacing_irrigation <- function(object, ...) {
  data.frame(
    enabled = isTRUE(object$enabled),
    threshold = if (isTRUE(object$enabled)) object$threshold else NA_real_,
    target = if (isTRUE(object$enabled)) object$target else NA_real_,
    max_application_mm = if (isTRUE(object$enabled)) {
      object$max_application_mm
    } else {
      NA_real_
    },
    window_start = if (is.null(object$window_start)) NA_character_ else object$window_start,
    window_end = if (is.null(object$window_end)) NA_character_ else object$window_end,
    label = if (is.null(object$label)) NA_character_ else object$label,
    stringsAsFactors = FALSE
  )
}

#' Check a single logical value
#' @param x Value to check.
#' @param argument Character scalar. Argument name for the error message.
#' @return `x`; an error when it is not one non-missing logical.
#' @noRd
.spacing_assert_flag <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", argument, "` must be one non-missing logical value.", call. = FALSE)
  }
  x
}

#' Check a single finite number within bounds
#' @param x Value to check.
#' @param argument Character scalar. Argument name for the error message.
#' @param lower,upper Numeric bounds.
#' @param lower_inclusive,upper_inclusive Logical. Whether the bounds are
#'   allowed.
#' @return `x` as a double; an error states the allowed interval.
#' @noRd
.spacing_assert_number <- function(
    x, argument, lower = -Inf, upper = Inf,
    lower_inclusive = TRUE, upper_inclusive = TRUE) {

  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop("`", argument, "` must be one finite numeric value.", call. = FALSE)
  }

  lower_ok <- if (lower_inclusive) x >= lower else x > lower
  upper_ok <- if (upper_inclusive) x <= upper else x < upper
  if (!lower_ok || !upper_ok) {
    left <- if (lower_inclusive) "[" else "("
    right <- if (upper_inclusive) "]" else ")"
    stop(
      "`", argument, "` must be in ", left, lower, ", ", upper, right,
      "; received ", x, ".",
      call. = FALSE
    )
  }
  as.numeric(x)
}

#' Check an optional single string
#' @param x `NULL` or a character scalar.
#' @param argument Character scalar. Argument name for the error message.
#' @return `NULL`, or `x` with surrounding white space removed; an error for
#'   `NA`, empty strings or vectors.
#' @noRd
.spacing_optional_text <- function(x, argument) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`", argument, "` must be NULL or one non-empty character value.",
         call. = FALSE)
  }
  trimws(x)
}

#' Validate and normalise a day-month date
#'
#' Accepts `d-mmm`, `dd-mmm`, `d-mm`, `dd-mm` (also with `/`) and checks that
#' the day exists in the month (29 February is allowed).
#'
#' @param x Character scalar, e.g. `"01-JUN"` or `"1/6"`.
#' @param argument Character scalar. Argument name for the error message.
#' @return Character scalar in APSIM `d-mmm` form, e.g. `"1-jun"`.
#' @noRd
.spacing_normalize_day_month <- function(x, argument) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("`", argument, "` must be one non-missing character value.",
         call. = FALSE)
  }

  value <- tolower(trimws(x))
  match <- regexec(
    "^([0-9]{1,2})[-/]([a-z]{3}|[0-9]{1,2})$",
    value,
    perl = TRUE
  )
  parts <- regmatches(value, match)[[1L]]
  if (length(parts) != 3L) {
    stop(
      "`", argument, "` must use d-mmm, dd-mmm, d-mm, or dd-mm format; ",
      "for example '1-jun' or '31-aug'.",
      call. = FALSE
    )
  }

  day <- as.integer(parts[2L])
  month_token <- parts[3L]
  month_names <- c(
    jan = 1L, feb = 2L, mar = 3L, apr = 4L, may = 5L, jun = 6L,
    jul = 7L, aug = 8L, sep = 9L, oct = 10L, nov = 11L, dec = 12L
  )
  month <- if (grepl("^[0-9]+$", month_token)) {
    as.integer(month_token)
  } else {
    unname(month_names[[month_token]])
  }

  if (length(month) == 0L || is.na(month) || month < 1L || month > 12L) {
    stop("Invalid month in `", argument, "`: ", x, ".", call. = FALSE)
  }

  days_in_month <- c(31L, 29L, 31L, 30L, 31L, 30L,
                     31L, 31L, 30L, 31L, 30L, 31L)
  if (day < 1L || day > days_in_month[month]) {
    stop("Invalid day in `", argument, "`: ", x, ".", call. = FALSE)
  }

  paste0(day, "-", names(month_names)[month])
}
