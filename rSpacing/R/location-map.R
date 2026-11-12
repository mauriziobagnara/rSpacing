#' Extract simulation coordinates and location metadata
#'
#' Standardises spatial metadata from one or more rSpacing objects. Supported
#' inputs include `spacing_request`, `spacing_run`, `spacing_result`, a data
#' frame with latitude and longitude columns, and lists containing any of these
#' objects.
#'
#' @param x An rSpacing object, data frame, or list of supported objects.
#' @param label Optional label used when `x` represents a single location.
#' @return A data frame with scenario, cell, latitude, longitude, rotation,
#'   mock mode, status, and label.
#' @export
spacing_coordinates <- function(x, label = NULL) {
  normalise_names <- function(d) {
    names(d) <- tolower(names(d))
    aliases <- list(
      latitude = c("latitude", "lat"),
      longitude = c("longitude", "lon", "lng", "long"),
      scenario_id = c("scenario_id", "scenario", "run_id"),
      rotation = c("rotation", "crop"),
      mock = c("mock"),
      status = c("status"),
      cell_id = c("cell_id", "cell"),
      label = c("label", "site", "location", "name")
    )
    for (target in names(aliases)) {
      hit <- aliases[[target]][aliases[[target]] %in% names(d)][1L]
      if (!is.na(hit) && nzchar(hit) && !target %in% names(d)) d[[target]] <- d[[hit]]
    }
    d
  }

  one <- function(object, object_label = NULL) {
    if (inherits(object, "spacing_result")) {
      object <- object$run
    }
    if (inherits(object, "spacing_run")) {
      request <- object$request %||% list()
      d <- data.frame(
        latitude = request$latitude %||% NA_real_,
        longitude = request$longitude %||% NA_real_,
        scenario_id = request$scenario_id %||% object$run_id %||% NA_character_,
        rotation = request$rotation %||% NA_character_,
        mock = request$mock %||% NA,
        status = object$status %||% NA,
        stringsAsFactors = FALSE
      )
    } else if (inherits(object, "spacing_request")) {
      d <- data.frame(
        latitude = object$latitude,
        longitude = object$longitude,
        scenario_id = object$scenario_id %||% NA_character_,
        rotation = object$rotation %||% NA_character_,
        mock = object$mock %||% NA,
        status = NA,
        stringsAsFactors = FALSE
      )
    } else if (is.data.frame(object)) {
      d <- normalise_names(as.data.frame(object, stringsAsFactors = FALSE))
      if (!all(c("latitude", "longitude") %in% names(d))) {
        .rspacing_abort(
          "A location data frame must contain latitude/longitude or lat/lon columns.",
          "spacing_location_error"
        )
      }
    } else if (is.list(object) && !is.null(object$latitude) && !is.null(object$longitude)) {
      d <- normalise_names(as.data.frame(object, stringsAsFactors = FALSE))
    } else {
      .rspacing_abort(
        "Unsupported object. Supply a spacing_request, spacing_run, spacing_result, location data frame, or list of these objects.",
        "spacing_location_error"
      )
    }

    d <- normalise_names(d)
    n <- nrow(d)
    if (n == 0L) {
      .rspacing_abort("No simulation locations were supplied.", "spacing_location_error")
    }
    add_default <- function(name, value) if (!name %in% names(d)) d[[name]] <<- rep(value, n)
    add_default("scenario_id", NA_character_)
    add_default("rotation", NA_character_)
    add_default("mock", NA)
    add_default("status", NA)
    add_default("cell_id", NA_character_)
    add_default("label", NA_character_)

    d$latitude <- suppressWarnings(as.numeric(d$latitude))
    d$longitude <- suppressWarnings(as.numeric(d$longitude))
    invalid <- !is.finite(d$latitude) | !is.finite(d$longitude) |
      d$latitude < -90 | d$latitude > 90 |
      d$longitude < -180 | d$longitude > 180
    if (any(invalid)) {
      .rspacing_abort(
        "Location coordinates must be finite decimal degrees within valid latitude and longitude ranges.",
        "spacing_location_error"
      )
    }

    make_cell <- function(lat, lon) {
      lat_code <- sprintf("%03d", round(abs(lat) * 10))
      lon_code <- sprintf("%03d", round(abs(lon) * 10))
      prefix_lat <- ifelse(lat < 0, "S", "")
      prefix_lon <- ifelse(lon < 0, "W", "")
      paste0("CELL_", prefix_lat, lat_code, "_", prefix_lon, lon_code)
    }
    missing_cell <- is.na(d$cell_id) | !nzchar(as.character(d$cell_id))
    d$cell_id[missing_cell] <- make_cell(d$latitude[missing_cell], d$longitude[missing_cell])

    if (!is.null(object_label) && length(object_label) == 1L) d$label <- object_label
    missing_label <- is.na(d$label) | !nzchar(as.character(d$label))
    d$label[missing_label] <- ifelse(
      !is.na(d$scenario_id[missing_label]) & nzchar(as.character(d$scenario_id[missing_label])),
      as.character(d$scenario_id[missing_label]),
      d$cell_id[missing_label]
    )

    d[c("scenario_id", "cell_id", "latitude", "longitude", "rotation", "mock", "status", "label")]
  }

  if (is.list(x) && !inherits(x, c("spacing_request", "spacing_run", "spacing_result")) &&
      !is.data.frame(x) && is.null(x$latitude)) {
    out <- lapply(seq_along(x), function(i) {
      item_label <- if (!is.null(names(x)) && nzchar(names(x)[i])) names(x)[i] else NULL
      one(x[[i]], item_label)
    })
    result <- do.call(rbind, out)
    rownames(result) <- NULL
    return(result)
  }
  one(x, label)
}

#' Plot simulation locations on a static or interactive map
#'
#' Creates a publication-friendly static map or an interactive Leaflet map from
#' simulation coordinates. Static maps work with ggplot2 alone; when the
#' optional `maps` package is installed, a geographic basemap is added.
#' Interactive maps require the optional `leaflet` package and Internet access
#' when web tiles are requested.
#'
#' @param x An object accepted by [spacing_coordinates()].
#' @param type Output type: `"static"` or `"interactive"`.
#' @param label Optional single-location label.
#' @param colour_by Metadata column used to colour points on static maps.
#' @param basemap Logical. Add a geographic basemap or web tiles when possible.
#' @param zoom Initial Leaflet zoom for a single point.
#' @param point_size Static-map point size.
#' @param label_points Logical. Show labels on the static map.
#' @param title Optional map title.
#' @return A `ggplot` object for static maps or a `leaflet` htmlwidget for
#'   interactive maps.
#' @export
plot_spacing_map <- function(
    x,
    type = c("static", "interactive"),
    label = NULL,
    colour_by = c("rotation", "status", "mock", "scenario_id"),
    basemap = TRUE,
    zoom = 10,
    point_size = 4,
    label_points = TRUE,
    title = NULL) {
  type <- match.arg(type)
  colour_by <- match.arg(colour_by)
  locations <- spacing_coordinates(x, label = label)
  if (is.null(title)) title <- if (nrow(locations) == 1L) {
    paste0("Localizzazione della simulazione: ", locations$label[[1L]])
  } else {
    "Localizzazione delle simulazioni"
  }

  if (type == "interactive") {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      .rspacing_abort(
        "The optional package 'leaflet' is required for interactive maps. Install it with install.packages('leaflet').",
        "spacing_location_dependency_error"
      )
    }
    popup <- paste0(
      "<strong>", locations$label, "</strong><br>",
      "Cell: ", locations$cell_id, "<br>",
      "Latitude: ", format(locations$latitude, digits = 8), "<br>",
      "Longitude: ", format(locations$longitude, digits = 8), "<br>",
      "Rotation: ", ifelse(is.na(locations$rotation), "", locations$rotation), "<br>",
      "Scenario: ", ifelse(is.na(locations$scenario_id), "", locations$scenario_id), "<br>",
      "Mock: ", ifelse(is.na(locations$mock), "", locations$mock), "<br>",
      "Status: ", ifelse(is.na(locations$status), "", locations$status)
    )
    map <- leaflet::leaflet(locations)
    if (isTRUE(basemap)) map <- leaflet::addTiles(map)
    map <- leaflet::addCircleMarkers(
      map,
      lng = ~longitude,
      lat = ~latitude,
      radius = 7,
      stroke = TRUE,
      weight = 2,
      color = "#FFFFFF",
      fillColor = "#C62828",
      fillOpacity = 0.9,
      popup = popup,
      label = ~label
    )
    if (nrow(locations) == 1L) {
      map <- leaflet::setView(map, lng = locations$longitude[[1L]], lat = locations$latitude[[1L]], zoom = zoom)
    } else {
      pad_lon <- max(diff(range(locations$longitude)) * 0.08, 0.05)
      pad_lat <- max(diff(range(locations$latitude)) * 0.08, 0.05)
      map <- leaflet::fitBounds(
        map,
        lng1 = min(locations$longitude) - pad_lon,
        lat1 = min(locations$latitude) - pad_lat,
        lng2 = max(locations$longitude) + pad_lon,
        lat2 = max(locations$latitude) + pad_lat
      )
    }
    return(map)
  }

  p <- ggplot2::ggplot()
  if (isTRUE(basemap) && requireNamespace("maps", quietly = TRUE)) {
    world <- ggplot2::map_data("world")
    p <- p + ggplot2::geom_polygon(
      data = world,
      ggplot2::aes(x = long, y = lat, group = group),
      fill = "#F2EFE9",
      colour = "#A6A6A6",
      linewidth = 0.25
    )
  }

  locations$.colour_group <- as.factor(locations[[colour_by]])
  p <- p +
    ggplot2::geom_point(
      data = locations,
      ggplot2::aes(x = longitude, y = latitude, colour = .colour_group),
      size = point_size,
      alpha = 0.95
    )

  if (isTRUE(label_points)) {
    p <- p + ggplot2::geom_text(
      data = locations,
      ggplot2::aes(x = longitude, y = latitude, label = label),
      nudge_y = max(diff(range(locations$latitude)), 0.1) * 0.045,
      check_overlap = TRUE,
      size = 3.5,
      fontface = "bold"
    )
  }

  if (nrow(locations) == 1L) {
    x_pad <- 2.5
    y_pad <- 2.0
  } else {
    x_pad <- max(diff(range(locations$longitude)) * 0.2, 1)
    y_pad <- max(diff(range(locations$latitude)) * 0.2, 1)
  }

  p +
    ggplot2::coord_quickmap(
      xlim = range(locations$longitude) + c(-x_pad, x_pad),
      ylim = range(locations$latitude) + c(-y_pad, y_pad),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = paste0(
        "Coordinate WGS84 | ", nrow(locations),
        ifelse(nrow(locations) == 1L, " simulazione", " simulazioni")
      ),
      x = "Longitudine",
      y = "Latitudine",
      colour = gsub("_", " ", colour_by)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      legend.position = if (nrow(locations) > 1L) "right" else "none"
    )
}

#' Plot one simulation location
#'
#' Convenience wrapper around [plot_spacing_map()] for one request, run, result,
#' or explicit coordinate record.
#'
#' @inheritParams plot_spacing_map
#' @return A static ggplot map or interactive Leaflet widget.
#' @export
plot_spacing_location <- function(x, ...) {
  locations <- spacing_coordinates(x)
  if (nrow(locations) != 1L) {
    .rspacing_abort(
      "plot_spacing_location() requires exactly one location; use plot_spacing_map() for multiple simulations.",
      "spacing_location_error"
    )
  }
  plot_spacing_map(x, ...)
}
