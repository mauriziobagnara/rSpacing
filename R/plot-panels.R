#' Multi-figure panel for one daily variable
#'
#' One panel for a daily APSIM output variable: the full date series, the
#' annual summary and the multi-year day-of-year climatology (the three views
#' of [plot_spacing_daily()]).
#'
#' @param result A `spacing_result`.
#' @param variable Character scalar. A numeric column of the daily table
#'   ([spacing_daily()]).
#' @param year_summary Character. Annual summary: `"auto"` (chosen by
#'   [classify_spacing_variables()]), `"mean"`, `"sum"`, `"last"` or `"max"`.
#' @param ribbon Logical. Show the +/- 1 standard deviation ribbon in the
#'   day-of-year panel.
#' @param base_size Numeric. Base font size.
#' @param title Optional panel title; by default the variable label and unit.
#'
#' @return A `patchwork` object with three ggplot figures.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' plot_spacing_daily_panel(result, "Maize_LAI", year_summary = "max")
plot_spacing_daily_panel <- function(
    result,
    variable,
    year_summary = c("auto", "mean", "sum", "last", "max"),
    ribbon = TRUE,
    base_size = 12,
    title = NULL) {
  year_summary <- match.arg(year_summary)
  .validate_spacing_result(result)
  labels <- .spacing_variable_labels(result, variable, source = "daily")
  if (is.null(title)) title <- labels$title

  p_date <- plot_spacing_daily(
    result,
    variable = variable,
    x = "date",
    base_size = base_size
  ) + ggplot2::labs(subtitle = "Full daily series")

  p_year <- plot_spacing_daily(
    result,
    variable = variable,
    x = "year",
    summary = year_summary,
    base_size = base_size
  ) + ggplot2::labs(subtitle = "Annual summary")

  p_doy <- plot_spacing_daily(
    result,
    variable = variable,
    x = "doy",
    ribbon = ribbon,
    base_size = base_size
  ) + ggplot2::labs(subtitle = "Day-of-year climatology")

  patchwork::wrap_plots(
    p_date,
    p_year,
    p_doy,
    ncol = 2,
    heights = c(1, 1)
  ) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = title,
      subtitle = paste0(labels$description, " | APSIM daily output | Recommended annual summary: ", labels$summary),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = base_size + 4),
        plot.subtitle = ggplot2::element_text(size = base_size + 1)
      )
    )
}

#' Multi-figure panel for one annual report variable
#'
#' One panel for a numeric APSIM Report variable: the annual series, the
#' empirical distribution and the anomalies from the long-term mean.
#' When the report has a `HarvestedCrop` column, a crop variable such as
#' `Maize_GrainYield_kgha` uses only the harvests of that crop.
#'
#' @param result A `spacing_result`.
#' @param variable Character scalar. A numeric column of the report table
#'   ([spacing_report()]).
#' @param base_size Numeric. Base font size.
#' @param title Optional panel title; by default the variable label and unit.
#'
#' @return A `patchwork` object with three ggplot figures.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' plot_spacing_report_panel(result, "Maize_GrainYield_kgha")
plot_spacing_report_panel <- function(
    result,
    variable,
    base_size = 12,
    title = NULL) {
  .validate_spacing_result(result)
  labels <- .spacing_variable_labels(result, variable, source = "report")
  if (is.null(title)) title <- labels$title
  data <- spacing_report(result, strict = TRUE, allow_empty = FALSE)
  data <- .spacing_report_rows_for(data, variable)

  if (!"Year" %in% names(data)) {
    .rspacing_abort("The report table has no Year column.", "spacing_schema_error")
  }
  if (!variable %in% names(data)) {
    .rspacing_abort(
      sprintf("Variable '%s' is not in the report table.", variable),
      "spacing_schema_error"
    )
  }
  if (!is.numeric(data[[variable]])) {
    .rspacing_abort(
      sprintf("Variable '%s' is not numeric.", variable),
      "spacing_schema_error"
    )
  }

  unit <- spacing_variable_unit(
    result,
    variable,
    table = "Report",
    default = ""
  )
  y_label <- if (nzchar(unit)) sprintf("%s (%s)", variable, unit) else variable

  d <- data.frame(
    Year = as.integer(data$Year),
    value = as.numeric(data[[variable]])
  )
  d <- d[is.finite(d$Year) & is.finite(d$value), , drop = FALSE]
  if (!nrow(d)) {
    .rspacing_abort(
      sprintf("Variable '%s' has no finite report values.", variable),
      "spacing_data_error"
    )
  }

  long_mean <- mean(d$value)
  d$anomaly <- d$value - long_mean
  d$direction <- ifelse(d$anomaly >= 0, "Above the mean", "Below the mean")

  p_year <- plot_spacing_report(
    result,
    variable = variable,
    base_size = base_size
  ) + ggplot2::labs(subtitle = "Annual series")

  bins <- max(5L, min(15L, floor(sqrt(nrow(d)))))
  p_distribution <- ggplot2::ggplot(d, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      bins = bins,
      fill = "#4C78A8",
      colour = "white",
      alpha = 0.85
    ) +
    ggplot2::geom_vline(
      xintercept = long_mean,
      colour = "#B22222",
      linetype = 2,
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = variable,
      subtitle = "Distribution across years",
      x = y_label,
      y = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  p_anomaly <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = Year, y = anomaly, fill = direction)
  ) +
    ggplot2::geom_col(width = 0.82, show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = c(
      "Above the mean" = "#2E8B57",
      "Below the mean" = "#C44E52"
    )) +
    ggplot2::labs(
      title = variable,
      subtitle = "Anomaly from the long-term mean",
      x = "Year",
      y = if (nzchar(unit)) sprintf("Anomaly (%s)", unit) else "Anomaly"
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  patchwork::wrap_plots(
    p_year,
    p_distribution,
    p_anomaly,
    ncol = 2
  ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = paste0(labels$description, " | Event: ", labels$event, " | Series, distribution and anomalies"),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = base_size + 4),
        plot.subtitle = ggplot2::element_text(size = base_size + 1)
      )
    )
}

#' Multi-figure panel for one variable
#'
#' Calls [plot_spacing_daily_panel()] or [plot_spacing_report_panel()]
#' according to `source`.
#'
#' @param result A `spacing_result`.
#' @param variable Character scalar. Numeric variable to plot.
#' @param source Character. `"daily"` or `"report"`.
#' @param ... Further arguments passed to the panel function.
#'
#' @return A `patchwork` object.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' plot_spacing_variable_panel(result, "Maize_Biomass_kgha", source = "report")
plot_spacing_variable_panel <- function(
    result,
    variable,
    source = c("daily", "report"),
    ...) {
  source <- match.arg(source)
  if (source == "daily") {
    plot_spacing_daily_panel(result, variable = variable, ...)
  } else {
    plot_spacing_report_panel(result, variable = variable, ...)
  }
}

#' Multi-figure panels for several variables
#'
#' Builds one panel per selected numeric variable. With `variables = "all"`,
#' empty, constant and identifier columns are skipped, and with
#' `exclude_weather = TRUE` weather columns too (plot them with
#' [plot_spacing_met()]).
#'
#' @param result A `spacing_result`.
#' @param source Character. `"daily"` or `"report"`.
#' @param variables Character vector of column names, or `"all"`.
#' @param exclude_weather Logical. Skip weather variables in daily panels.
#' @param base_size Numeric. Base font size.
#' @param ... Further arguments passed to the panel function.
#'
#' @return A named list of `patchwork` objects, one per variable.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' panels <- plot_spacing_panels(result, source = "report")
#' names(panels)
#' panels[[1]]
plot_spacing_panels <- function(
    result,
    source = c("daily", "report"),
    variables = "all",
    exclude_weather = TRUE,
    base_size = 12,
    ...) {
  source <- match.arg(source)
  .validate_spacing_result(result)

  data <- if (source == "daily") {
    spacing_daily(result, strict = TRUE, allow_empty = FALSE)
  } else {
    spacing_report(result, strict = TRUE, allow_empty = FALSE)
  }

  numeric_variables <- names(data)[vapply(data, function(x) {
    if (!is.numeric(x) && !is.integer(x)) return(FALSE)
    finite <- x[is.finite(x)]
    length(finite) > 0L && length(unique(finite)) > 1L
  }, logical(1))]

  identifiers <- c(
    "Year", "DOY", "CheckpointID", "SimulationID", "MessageType"
  )
  candidates <- setdiff(numeric_variables, identifiers)

  if (source == "daily" && isTRUE(exclude_weather)) {
    classification <- classify_spacing_variables(candidates)
    candidates <- classification$variable[classification$group != "weather"]
  }

  if (length(variables) == 1L && identical(variables, "all")) {
    selected <- candidates
  } else {
    selected <- intersect(as.character(variables), candidates)
    missing <- setdiff(as.character(variables), names(data))
    if (length(missing)) {
      warning(
        "Variables not found in ", source, " output: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
  }

  panels <- lapply(selected, function(variable) {
    plot_spacing_variable_panel(
      result,
      variable = variable,
      source = source,
      base_size = base_size,
      ...
    )
  })
  names(panels) <- selected
  panels
}
