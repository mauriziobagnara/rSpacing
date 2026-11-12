#' Define an APSIMx threshold-based irrigation strategy
#'
#' Creates and validates the irrigation component used by rSpacing when building
#' a crop management request for `Spacing.Tool2`. The returned object maps
#' exactly to the irrigation fields currently supported by the C# interface:
#' `auto_irrigate`, `irrigation_threshold`, `irrigation_target`,
#' `max_irrigation_mm`, `irrigation_window_start`, and
#' `irrigation_window_end`.
#'
#' Irrigation is triggered when the fraction of plant-available water in the
#' soil profile falls below `threshold`. The application attempts to restore
#' the fraction to `target`, subject to `max_application_mm`, and only inside
#' the optional calendar window.
#'
#' @param enabled Logical scalar. If `FALSE`, automatic irrigation is disabled.
#' @param threshold Numeric scalar in `[0, 1)`. Fraction of plant-available
#'   water below which irrigation is triggered.
#' @param target Numeric scalar in `(0, 1]`. Fraction of plant-available water
#'   to restore after irrigation. Must be greater than `threshold`.
#' @param max_application_mm Positive numeric scalar. Maximum irrigation depth,
#'   in mm, allowed for one event.
#' @param window_start Optional character scalar in `d-mmm` or `dd-mmm` format,
#'   for example `"1-jun"` or `"15-jun"`.
#' @param window_end Optional character scalar in `d-mmm` or `dd-mmm` format,
#'   for example `"31-aug"`. `window_start` and `window_end` must be supplied
#'   together. Windows crossing the end of the calendar year are allowed.
#' @param label Optional human-readable label retained by rSpacing but not sent
#'   to `Spacing.Tool2`.
#'
#' @return An object of class `spacing_irrigation`. Use `as.list()` to obtain
#'   the exact JSON-ready fields for `management`.
#'
#' @details
#' The function intentionally exposes only irrigation controls actually
#' supported by the current rSpacing/C# contract. It does not silently invent
#' unsupported properties such as irrigation efficiency, application method,
#' wetted fraction, salinity, or source-water composition.
#'
#' When `enabled = FALSE`, `as.list()` returns only
#' `list(auto_irrigate = FALSE)`, preventing inactive numerical settings from
#' being mistaken for an active strategy.
#'
#' @examples
#' maize_irrigation <- spacing_irrigation(
#'   enabled = TRUE,
#'   threshold = 0.50,
#'   target = 0.85,
#'   max_application_mm = 35,
#'   window_start = "1-jun",
#'   window_end = "31-aug",
#'   label = "Maize B_110 summer irrigation"
#' )
#'
#' management <- list(
#'   cultivar = "B_110",
#'   sowing_window_start = "1-apr",
#'   sowing_window_end = "10-may"
#' )
#' management <- spacing_apply_irrigation(management, maize_irrigation)
#'
#' jsonlite::toJSON(management, auto_unbox = TRUE, pretty = TRUE)
#'
#' rainfed <- spacing_irrigation(enabled = FALSE, label = "Rainfed")
#' as.list(rainfed)
#'
#' @export
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

#' Add an irrigation strategy to crop management
#'
#' Merges a validated `spacing_irrigation` object into a crop management list
#' without changing any unrelated management setting.
#'
#' @param management A named list containing crop management fields.
#' @param irrigation A `spacing_irrigation` object.
#'
#' @return The updated management list, ready to be included under
#'   `crops[[i]]$management` in a `Spacing.Tool2` request.
#'
#' @examples
#' wheat_management <- list(cultivar = "Hartog")
#' wheat_management <- spacing_apply_irrigation(
#'   wheat_management,
#'   spacing_irrigation(
#'     threshold = 0.35,
#'     target = 0.70,
#'     max_application_mm = 25,
#'     window_start = "1-mar",
#'     window_end = "31-may"
#'   )
#' )
#'
#' @export
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

.spacing_assert_flag <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", argument, "` must be one non-missing logical value.", call. = FALSE)
  }
  x
}

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

.spacing_optional_text <- function(x, argument) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`", argument, "` must be NULL or one non-empty character value.",
         call. = FALSE)
  }
  trimws(x)
}

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
