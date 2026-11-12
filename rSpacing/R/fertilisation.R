#' Define one APSIM fertiliser application
#'
#' @param amount_kg_n_ha Non-negative nitrogen amount in kg N/ha.
#' @param fertiliser Character scalar. APSIM fertiliser type. The current
#'   Spacing.Tool2 workflow supports `"UreaN"`.
#' @param trigger Either `"Sowing"` or `"DaysAfterSowing"`.
#' @param days_after_sowing Non-negative integer used with
#'   `trigger = "DaysAfterSowing"`.
#' @return A `spacing_fertiliser_application` object.
#' @export
spacing_fertiliser_application <- function(
    amount_kg_n_ha,
    fertiliser = "UreaN",
    trigger = c("Sowing", "DaysAfterSowing"),
    days_after_sowing = 0L) {
  trigger <- match.arg(trigger)
  amount_kg_n_ha <- .spacing_assert_number(
    amount_kg_n_ha, "amount_kg_n_ha", lower = 0
  )
  fertiliser <- .spacing_optional_text(fertiliser, "fertiliser")
  supported <- "UreaN"
  if (!fertiliser %in% supported) {
    stop(
      "Unsupported fertiliser `", fertiliser,
      "`. Current Spacing.Tool2 contract supports: ",
      paste(supported, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!is.numeric(days_after_sowing) || length(days_after_sowing) != 1L ||
      is.na(days_after_sowing) || days_after_sowing < 0 ||
      days_after_sowing != as.integer(days_after_sowing)) {
    stop("`days_after_sowing` must be one non-negative integer.", call. = FALSE)
  }
  if (trigger == "Sowing") days_after_sowing <- 0L
  structure(
    list(
      trigger = trigger,
      days_after_sowing = as.integer(days_after_sowing),
      amount_kg_n_ha = amount_kg_n_ha,
      fert_type = fertiliser
    ),
    class = "spacing_fertiliser_application"
  )
}

#' Define a crop fertilisation programme
#'
#' @param at_sowing_kg_n_ha Non-negative amount applied at sowing.
#' @param applications A list of objects created by
#'   [spacing_fertiliser_application()].
#' @param fertiliser Fertiliser type used for the sowing application.
#' @return A `spacing_fertilisation` object.
#' @export
spacing_fertilisation <- function(
    at_sowing_kg_n_ha = 0,
    applications = list(),
    fertiliser = "UreaN") {
  at_sowing_kg_n_ha <- .spacing_assert_number(
    at_sowing_kg_n_ha, "at_sowing_kg_n_ha", lower = 0
  )
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
  fertiliser <- .spacing_optional_text(fertiliser, "fertiliser")
  if (!fertiliser %in% "UreaN") {
    stop("Current Spacing.Tool2 contract supports only `UreaN`.", call. = FALSE)
  }
  structure(
    list(
      at_sowing_kg_n_ha = at_sowing_kg_n_ha,
      fertiliser = fertiliser,
      applications = applications
    ),
    class = "spacing_fertilisation"
  )
}

#' Add fertilisation to crop management
#'
#' @param management Named management list.
#' @param fertilisation A `spacing_fertilisation` object.
#' @return Updated JSON-ready management list.
#' @export
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

#' @export
as.list.spacing_fertilisation <- function(x, ...) {
  list(
    nitrogen_mineral_kg_ha = x$at_sowing_kg_n_ha,
    split_fertiliser = lapply(x$applications, unclass)
  )
}

#' @export
print.spacing_fertilisation <- function(x, ...) {
  cat("<spacing_fertilisation>\n")
  cat("  at sowing:   ", x$at_sowing_kg_n_ha, " kg N/ha\n", sep = "")
  cat("  fertiliser:  ", x$fertiliser, "\n", sep = "")
  cat("  later events:", length(x$applications), "\n")
  invisible(x)
}
