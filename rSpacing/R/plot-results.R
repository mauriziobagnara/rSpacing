#' Plot daily outputs from a spacing result
#'
#' @param result A `spacing_result`.
#' @param variable One numeric column in the daily table.
#' @param x Horizontal scale: `"date"`, `"year"`, or `"doy"`.
#' @param summary Summary used for `x = "year"`: `"auto"`, `"mean"`,
#'   `"sum"`, `"last"`, or `"max"`. `"auto"` uses
#'   [classify_spacing_variables()].
#' @param ribbon Logical. For `x = "doy"`, show plus or minus one standard
#'   deviation around the multi-year mean.
#' @param base_size Base font size.
#' @return A `ggplot` object.
#' @export
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
  p + ggplot2::labs(title=labels$title, subtitle=labels$subtitle, x=switch(x,date="Data",year="Anno",doy="Giorno dell'anno (DOY)"), y=ylab) +
    ggplot2::theme_minimal(base_size=base_size) + ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title.position="plot")
}

#' Plot annual APSIM report output
#'
#' @param result A `spacing_result`.
#' @param variable One numeric column in the annual report table.
#' @param base_size Base font size.
#' @return A `ggplot` object.
#' @export
plot_spacing_report <- function(result, variable, base_size = 12) {
  data <- spacing_report(result, strict=TRUE, allow_empty=FALSE)
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
    ggplot2::labs(title=labels$title, subtitle=labels$subtitle, x="Anno", y=ylab) +
    ggplot2::theme_minimal(base_size=base_size) + ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title.position="plot")
}

#' Plot a spacing result
#'
#' @param result A `spacing_result`.
#' @param source Data source: `"weather"`, `"daily"`, or `"report"`.
#' @param variable Variable to plot for daily or report outputs.
#' @param panel Logical. Return a multi-figure panel for daily and report
#'   variables. Set to `FALSE` for the historical single-figure behaviour.
#' @param ... Additional arguments passed to the selected plot function.
#' @return A `ggplot` or `patchwork` object.
#' @export
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

#' @export
plot.spacing_result <- function(x, ...) plot_spacing_result(x, ...)
