#' Multi-figure panel for one daily variable
#'
#' Creates one coherent panel for a daily APSIM output variable. The panel
#' combines the full date series, the annual summary, and the multi-year DOY
#' climatology. The component plots remain available through
#' [plot_spacing_daily()].
#'
#' @param result A `spacing_result`.
#' @param variable One numeric column in the daily output table.
#' @param year_summary Annual summary passed to [plot_spacing_daily()].
#' @param ribbon Logical. Show the standard-deviation ribbon in the DOY panel.
#' @param base_size Base font size.
#' @param title Optional panel title. By default, the variable name is used.
#'
#' @return A `patchwork` object containing three ggplot figures.
#' @export
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
  ) + ggplot2::labs(subtitle = "Serie giornaliera completa")

  p_year <- plot_spacing_daily(
    result,
    variable = variable,
    x = "year",
    summary = year_summary,
    base_size = base_size
  ) + ggplot2::labs(subtitle = "Riepilogo annuale")

  p_doy <- plot_spacing_daily(
    result,
    variable = variable,
    x = "doy",
    ribbon = ribbon,
    base_size = base_size
  ) + ggplot2::labs(subtitle = "Climatologia per giorno dell'anno")

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
      subtitle = paste0(labels$description, " | Output giornaliero APSIM | Sintesi annuale consigliata: ", labels$summary),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = base_size + 4),
        plot.subtitle = ggplot2::element_text(size = base_size + 1)
      )
    )
}

#' Multi-figure panel for one annual report variable
#'
#' Creates one panel for a numeric APSIM Report variable. The panel combines
#' the annual time series, the empirical distribution, and annual anomalies
#' from the long-term mean.
#'
#' @param result A `spacing_result`.
#' @param variable One numeric column in the Report table.
#' @param base_size Base font size.
#' @param title Optional panel title.
#'
#' @return A `patchwork` object containing three ggplot figures.
#' @export
plot_spacing_report_panel <- function(
    result,
    variable,
    base_size = 12,
    title = NULL) {
  .validate_spacing_result(result)
  labels <- .spacing_variable_labels(result, variable, source = "report")
  if (is.null(title)) title <- labels$title
  data <- spacing_report(result, strict = TRUE, allow_empty = FALSE)

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
  d$direction <- ifelse(d$anomaly >= 0, "Sopra la media", "Sotto la media")

  p_year <- plot_spacing_report(
    result,
    variable = variable,
    base_size = base_size
  ) + ggplot2::labs(subtitle = "Andamento annuale")

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
      subtitle = "Distribuzione fra gli anni",
      x = y_label,
      y = "Frequenza"
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
      "Sopra la media" = "#2E8B57",
      "Sotto la media" = "#C44E52"
    )) +
    ggplot2::labs(
      title = variable,
      subtitle = "Anomalia dalla media pluriennale",
      x = "Anno",
      y = if (nzchar(unit)) sprintf("Anomalia (%s)", unit) else "Anomalia"
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
      subtitle = paste0(labels$description, " | Evento: ", labels$event, " | Andamento, distribuzione e anomalie"),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = base_size + 4),
        plot.subtitle = ggplot2::element_text(size = base_size + 1)
      )
    )
}

#' Multi-figure panel for one spacing variable
#'
#' Dispatches to [plot_spacing_daily_panel()] or
#' [plot_spacing_report_panel()] according to the selected source.
#'
#' @param result A `spacing_result`.
#' @param variable Numeric variable to plot.
#' @param source Data source: `"daily"` or `"report"`.
#' @param ... Additional arguments passed to the selected panel function.
#'
#' @return A `patchwork` object.
#' @export
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

#' Create multi-figure panels for multiple spacing variables
#'
#' Produces a named list containing one multi-figure panel for each selected
#' numeric variable. Empty, constant, identifier, and weather variables can be
#' removed automatically. Weather remains available through
#' [plot_spacing_met()].
#'
#' @param result A `spacing_result`.
#' @param source Data source: `"daily"` or `"report"`.
#' @param variables Character vector of variables or `"all"`.
#' @param exclude_weather Logical. Exclude weather variables from daily panels.
#' @param base_size Base font size.
#' @param ... Additional arguments passed to the selected panel function.
#'
#' @return A named list of `patchwork` objects.
#' @export
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
