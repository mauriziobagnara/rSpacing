#' Plot one daily output variable
#'
#' @param result A `spacing_result`.
#' @param variable Character scalar. A numeric column of the daily table
#'   ([spacing_daily()]).
#' @param x Character. Horizontal scale: `"date"` (full series), `"year"`
#'   (annual summary) or `"doy"` (multi-year mean by day of year).
#' @param summary Character. Annual summary for `x = "year"`: `"auto"`,
#'   `"mean"`, `"sum"`, `"last"` or `"max"`. `"auto"` uses
#'   [classify_spacing_variables()] (fluxes are summed, states averaged).
#' @param ribbon Logical. For `x = "doy"`, show +/- 1 standard deviation
#'   around the multi-year mean.
#' @param base_size Numeric. Base font size.
#' @return A `ggplot` object.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' plot_spacing_daily(result, "Maize_LAI")
#' plot_spacing_daily(result, "Maize_LAI", x = "doy")
plot_spacing_daily <- function(result, variable, x = c("date","year","doy"),
                               summary = c("auto","mean","sum","last","max"),
                               ribbon = TRUE, base_size = 12) {
  x <- match.arg(x); summary <- match.arg(summary)
  data <- spacing_daily(result, strict=TRUE, allow_empty=FALSE)
  if (!variable %in% names(data)) .rspacing_abort(sprintf("Variable '%s' is not in the daily table.", variable), "spacing_schema_error")
  if (!is.numeric(data[[variable]])) .rspacing_abort(sprintf("Variable '%s' is not numeric.", variable), "spacing_schema_error")
  labels <- .spacing_variable_labels(result, variable, source = "daily")
  unit <- spacing_variable_unit(result, variable, table="DailyReport", default="")
  ylab <- labels$y
  if (x == "date") {
    date_col <- names(data)[tolower(names(data)) %in% c("clock.today","date","today")][1L]
    if (!is.na(date_col)) {
      dates <- as.Date(
        sub(" .*", "", data[[date_col]]),
        tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
      )
    } else if (all(c("Year", "DOY") %in% names(data))) {
      dates <- as.Date(paste0(as.integer(data$Year), "-01-01")) +
        as.integer(data$DOY) - 1L
    } else {
      .rspacing_abort(
        "No date column or Year/DOY combination is available in the daily table.",
        "spacing_schema_error"
      )
    }
    d <- data.frame(date = dates, value = data[[variable]])
    p <- ggplot2::ggplot(d, ggplot2::aes(x=date, y=value)) + ggplot2::geom_line(linewidth=.55, colour="#1F4E79", na.rm=TRUE)
  } else if (x == "year") {
    if (!"Year" %in% names(data)) .rspacing_abort("The daily table has no Year column.", "spacing_schema_error")
    method <- summary
    if (method == "auto") {
      method <- labels$summary
      if (!method %in% c("mean", "sum", "last", "max")) {
        method <- classify_spacing_variables(variable)$recommended_summary[[1L]]
      }
    }
    spl <- split(data[[variable]], data$Year)
    fun <- switch(method, mean=function(z) mean(z,na.rm=TRUE), sum=function(z) sum(z,na.rm=TRUE), last=function(z) tail(z[is.finite(z)],1L), max=function(z) max(z,na.rm=TRUE))
    vals <- vapply(spl, function(z) { z <- z[is.finite(z)]; if (!length(z)) NA_real_ else fun(z) }, numeric(1))
    d <- data.frame(Year=as.integer(names(vals)), value=as.numeric(vals))
    p <- ggplot2::ggplot(d, ggplot2::aes(x=Year, y=value)) + ggplot2::geom_line(linewidth=.65, colour="#1F4E79", na.rm=TRUE) + ggplot2::geom_point(size=1.8, colour="#1F4E79", na.rm=TRUE)
  } else {
    if (!"DOY" %in% names(data)) .rspacing_abort("The daily table has no DOY column.", "spacing_schema_error")
    spl <- split(data[[variable]], data$DOY)
    meanv <- vapply(spl, function(z) mean(z,na.rm=TRUE), numeric(1)); sdv <- vapply(spl, function(z) stats::sd(z,na.rm=TRUE), numeric(1))
    d <- data.frame(DOY=as.integer(names(meanv)), value=as.numeric(meanv), low=meanv-sdv, high=meanv+sdv)
    p <- ggplot2::ggplot(d, ggplot2::aes(x=DOY, y=value))
    if (isTRUE(ribbon)) p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin=low,ymax=high), fill="#9ECAE1", alpha=.35, na.rm=TRUE)
    p <- p + ggplot2::geom_line(linewidth=.7, colour="#08519C", na.rm=TRUE)
  }
  p + ggplot2::labs(title=labels$title, subtitle=labels$subtitle, x=switch(x,date="Date",year="Year",doy="Day of year (DOY)"), y=ylab) +
    ggplot2::theme_minimal(base_size=base_size) + ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title.position="plot")
}

#' Keep the report rows of the crop a variable belongs to
#'
#' Since Tool2 0.09.09 the harvest `Report` has one row per crop end, with a
#' `HarvestedCrop` column; a variable such as `Maize_GrainYield_kgha` is 0 on
#' the rows of the other crops. Those rows are dropped.
#' @param data Report data frame.
#' @param variable Column name.
#' @return `data`, restricted to the rows of the variable's crop when the
#'   prefix of `variable` is one of the harvested crops.
#' @noRd
.spacing_report_rows_for <- function(data, variable) {
  if (!"HarvestedCrop" %in% names(data)) return(data)
  crop <- sub("_.*$", "", variable)
  keep <- as.character(data$HarvestedCrop) == crop
  if (!identical(crop, variable) && any(keep, na.rm = TRUE)) data[which(keep), , drop = FALSE] else data
}

#' Plot one annual report variable
#'
#' @param result A `spacing_result`.
#' @param variable Character scalar. A numeric column of the report table
#'   ([spacing_report()]).
#' @param base_size Numeric. Base font size.
#' @return A `ggplot` object (values by harvest year). When the report has a
#'   `HarvestedCrop` column, a crop variable such as `Maize_GrainYield_kgha` is
#'   plotted only for the harvests of that crop.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' plot_spacing_report(result, "Maize_GrainYield_kgha")
plot_spacing_report <- function(result, variable, base_size = 12) {
  data <- spacing_report(result, strict=TRUE, allow_empty=FALSE)
  data <- .spacing_report_rows_for(data, variable)
  if (!"Year" %in% names(data)) .rspacing_abort("The report table has no Year column.", "spacing_schema_error")
  if (!variable %in% names(data)) .rspacing_abort(sprintf("Variable '%s' is not in the report table.", variable), "spacing_schema_error")
  if (!is.numeric(data[[variable]])) .rspacing_abort(sprintf("Variable '%s' is not numeric.", variable), "spacing_schema_error")
  labels <- .spacing_variable_labels(result, variable, source = "report")
  unit <- spacing_variable_unit(result, variable, table="Report", default="")
  ylab <- labels$y
  d <- data.frame(Year=data$Year, value=data[[variable]])
  ggplot2::ggplot(d, ggplot2::aes(x=Year,y=value)) +
    ggplot2::geom_line(linewidth=.7, colour="#7A1F5C", na.rm=TRUE) +
    ggplot2::geom_point(size=2, colour="#7A1F5C", na.rm=TRUE) +
    ggplot2::labs(title=labels$title, subtitle=labels$subtitle, x="Year", y=ylab) +
    ggplot2::theme_minimal(base_size=base_size) + ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title.position="plot")
}

#' Plot a spacing result
#'
#' Entry point for quick plots: weather ([plot_spacing_met()]), a daily
#' variable or a report variable, as a multi-figure panel or a single figure.
#'
#' @param result A `spacing_result`.
#' @param source Character. `"weather"`, `"daily"` or `"report"`.
#' @param variable Character scalar. Variable for daily or report output
#'   (required for those sources).
#' @param panel Logical. Return a multi-figure panel for daily and report
#'   variables; `FALSE` gives the single figure of [plot_spacing_daily()] or
#'   [plot_spacing_report()].
#' @param ... Further arguments passed to the selected plot function.
#' @return A `ggplot` or `patchwork` object.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' plot_spacing_result(result, "report", "Maize_GrainYield_kgha", panel = FALSE)
#' plot(result, "daily", "Maize_LAI")
plot_spacing_result <- function(
    result,
    source = c("weather", "daily", "report"),
    variable = NULL,
    panel = TRUE,
    ...) {
  source <- match.arg(source)
  .validate_spacing_result(result)
  if (source == "weather") return(plot_spacing_met(result, ...))
  if (is.null(variable) || length(variable) != 1L) {
    .rspacing_abort(
      "Provide one 'variable' for daily or report plots.",
      "spacing_validation_error"
    )
  }
  if (isTRUE(panel)) {
    return(plot_spacing_variable_panel(result, variable, source, ...))
  }
  if (source == "daily") {
    plot_spacing_daily(result, variable, ...)
  } else {
    plot_spacing_report(result, variable, ...)
  }
}

#' @describeIn plot_spacing_result `plot()` method; same as
#'   `plot_spacing_result(x, ...)`.
#' @param x A `spacing_result`.
#' @export
plot.spacing_result <- function(x, ...) plot_spacing_result(x, ...)
