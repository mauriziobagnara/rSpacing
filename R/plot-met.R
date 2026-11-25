#' Plot an APSIM weather object
#'
#' Four-panel weather diagnostic: maximum and minimum temperature, daily rain
#' and solar radiation over time.
#'
#' @param x A `spacing_met` object, a `spacing_result` (its weather file is
#'   read) or the path of an existing `.met` file.
#' @param start_date,end_date Optional inclusive date limits (`Date` or
#'   strings coercible to `Date`).
#' @param title Optional plot title; by default the site and period.
#' @param colours Named character vector of colours for `maxt`, `mint`,
#'   `rain` and `radn`.
#' @param base_size Numeric. Base font size passed to
#'   `ggplot2::theme_minimal()`.
#' @return A `patchwork`/`ggplot` object.
#' @family weather files
#' @family plots
#' @export
#' @examples
#' met <- read_spacing_met(system.file("extdata", "minimal.met", package = "rSpacing"),
#'                         validate = FALSE)
#' plot_spacing_met(met)
#' plot(met)
plot_spacing_met <- function(x, start_date = NULL, end_date = NULL,
                             title = NULL,
                             colours = c(maxt = "#C0392B", mint = "#2471A3",
                                         rain = "#2E86C1", radn = "#D68910"),
                             base_size = 11) {
  if (inherits(x, "spacing_result")) {
    if (!is.character(x$weather) || length(x$weather) != 1L || !file.exists(x$weather)) {
      .rspacing_abort("The spacing_result does not reference one existing .met file.", "spacing_met_error")
    }
    x <- x$weather
  }
  if (is.character(x) && length(x) == 1L) x <- read_spacing_met(x)
  if (!inherits(x, "spacing_met")) {
    .rspacing_abort("x must be a spacing_met object, a spacing_result, or one .met file path.",
                    "spacing_met_error")
  }
  required <- c("maxt", "mint", "rain", "radn")
  missing <- setdiff(required, names(x$data))
  if (length(missing)) .rspacing_abort(
    paste("Cannot plot; missing weather columns:", paste(missing, collapse = ", ")),
    "spacing_met_error")
  dates <- .met_dates(x$data)
  keep <- rep(TRUE, length(dates))
  if (!is.null(start_date)) keep <- keep & dates >= as.Date(start_date)
  if (!is.null(end_date)) keep <- keep & dates <= as.Date(end_date)
  if (!any(keep)) .rspacing_abort("The requested plot period contains no rows.", "spacing_met_error")
  d <- x$data[keep, , drop = FALSE]
  dates <- dates[keep]
  long <- rbind(
    data.frame(date = dates, variable = "Maximum temperature", series = "Maximum", value = d$maxt),
    data.frame(date = dates, variable = "Minimum temperature", series = "Minimum", value = d$mint),
    data.frame(date = dates, variable = "Rainfall", series = "Rainfall", value = d$rain),
    data.frame(date = dates, variable = "Solar radiation", series = "Solar radiation", value = d$radn)
  )
  long$variable <- factor(long$variable,
    levels = c("Maximum temperature", "Minimum temperature", "Rainfall", "Solar radiation"),
    labels = c("Maximum temperature (deg C)", "Minimum temperature (deg C)",
               "Rainfall (mm)", "Solar radiation (MJ/m2)"))
  site <- x$constants$site$value %||% "APSIM weather"
  if (is.null(title)) title <- paste0(site, " | ", min(dates), " to ", max(dates))
  cols <- c(Maximum = colours[["maxt"]], Minimum = colours[["mint"]],
            Rainfall = colours[["rain"]], `Solar radiation` = colours[["radn"]])
  line_data <- long[long$series != "Rainfall", , drop = FALSE]
  rain_data <- long[long$series == "Rainfall", , drop = FALSE]
  ggplot2::ggplot(long, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_col(data = rain_data, ggplot2::aes(fill = series), width = 1, show.legend = FALSE) +
    ggplot2::geom_line(data = line_data, ggplot2::aes(colour = series), linewidth = 0.45, na.rm = TRUE) +
    ggplot2::facet_grid(variable ~ ., scales = "free_y", switch = "y") +
    ggplot2::scale_colour_manual(values = cols, name = NULL) +
    ggplot2::scale_fill_manual(values = cols, name = NULL) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1),
                   legend.position = "top",
                   plot.title.position = "plot")
}

#' @describeIn plot_spacing_met `plot()` method; same as `plot_spacing_met(x, ...)`.
#' @param ... Further arguments passed to `plot_spacing_met()`.
#' @export
plot.spacing_met <- function(x, ...) plot_spacing_met(x, ...)
