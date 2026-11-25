#' Default units of common APSIM weather variables
#'
#' Units used when [new_spacing_met()] or [set_spacing_met_column()] get no
#' unit for a known column: `year`, `day`, `date`, `day_of_month`, `month`
#' `()`, `radn` `(MJ/m^2)`, `maxt`/`mint` `(oC)`, `rain` `(mm)`, `wind`
#' `(m/s)`, `vp` `(hPa)`, `pan` `(mm)`, `co2` `(ppm)`.
#'
#' @return A named character vector (names are column names, values are units
#'   in the parentheses required by the APSIM `.met` format).
#' @family weather files
#' @export
#' @examples
#' spacing_met_default_units()[c("radn", "rain")]
spacing_met_default_units <- function() {
  c(
    year = "()", day = "()", date = "()",
    day_of_month = "()", month = "()",
    radn = "(MJ/m^2)", maxt = "(oC)", mint = "(oC)",
    rain = "(mm)", wind = "(m/s)", vp = "(hPa)",
    pan = "(mm)", co2 = "(ppm)"
  )
}

#' Create an APSIM weather object
#'
#' Creates a validated in-memory representation of an APSIM `.met` file. Daily
#' data may identify time with `year` and `day` (day of year), with `year`,
#' `month` and `day_of_month`, or with a `date` column coercible to `Date`.
#' The object is validated with [validate_spacing_met()] before it is
#' returned and is written with [write_spacing_met()].
#'
#' @param data Data frame of daily weather, one row per day. APSIM needs at
#'   least `radn` (MJ/m2), `maxt` and `mint` (degC) and `rain` (mm); other
#'   columns (`wind`, `vp`, `co2`, ...) are kept.
#' @param latitude Numeric scalar. Latitude in decimal degrees (-90 to 90).
#' @param longitude Optional numeric scalar. Longitude in decimal degrees.
#' @param site Optional character scalar. Site name, written as the `site`
#'   constant.
#' @param tav,amp Optional numeric scalars. Annual average temperature and
#'   annual amplitude of mean monthly temperature (degC). When `NULL` they are
#'   calculated from `maxt` and `mint`.
#' @param units Named character vector with the unit of each data column, with
#'   or without parentheses. Missing units of known columns come from
#'   [spacing_met_default_units()]; others default to `()`.
#' @param constants Named list of additional APSIM constants. Each element is a
#'   scalar or `list(value = ..., unit = ..., comment = ...)`. Names are
#'   lower-cased.
#' @param section Character scalar written as the section line.
#' @param comments Character vector written as leading `!` comment lines.
#' @param add_year_day Logical. When a `date` column is present, add or replace
#'   the `year` and `day` columns.
#'
#' @return An object of class `spacing_met`: a list with `section`
#'   (character), `comments` (character), `constants` (named list of
#'   `list(value, unit, comment)`, always with `latitude`, `tav` and `amp`),
#'   `units` (named character, one per column, with parentheses) and `data`
#'   (data frame of daily values).
#' @family weather files
#' @export
#' @examples
#' wx <- data.frame(date = seq(as.Date("2020-01-01"), by = "day", length.out = 5),
#'   radn = c(5, 6, 4, 7, 6), maxt = c(8, 9, 7, 10, 9), mint = c(-1, 0, -2, 1, 0),
#'   rain = c(0, 2.5, 0, 0, 1))
#' met <- new_spacing_met(wx, latitude = 44.9, longitude = 10.6, site = "demo")
#' met
#' met$constants$tav$value
new_spacing_met <- function(
    data,
    latitude,
    longitude = NULL,
    site = NULL,
    tav = NULL,
    amp = NULL,
    units = NULL,
    constants = list(),
    section = "[weather.met.weather]",
    comments = character(),
    add_year_day = TRUE) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    .rspacing_abort("data must be a non-empty data frame.", "spacing_met_error")
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  names(data) <- tolower(trimws(names(data)))
  if (anyDuplicated(names(data))) {
    .rspacing_abort("Weather column names must be unique.", "spacing_met_error")
  }
  if ("date" %in% names(data)) {
    parsed_date <- as.Date(data$date)
    if (anyNA(parsed_date)) {
      .rspacing_abort("The date column contains unparseable values.", "spacing_met_error")
    }
    data$date <- parsed_date
    if (isTRUE(add_year_day)) {
      data$year <- as.integer(format(parsed_date, "%Y"))
      data$day <- as.integer(format(parsed_date, "%j"))
      preferred <- c("year", "day")
      data <- data[c(preferred, setdiff(names(data), c(preferred, "date")))]
    }
  }
  units <- .complete_met_units(data, units)
  calculated <- .calculate_tav_amp(data)
  if (is.null(tav)) tav <- calculated$tav
  if (is.null(amp)) amp <- calculated$amp
  standard_constants <- list(
    latitude = .met_constant(latitude, "(DECIMAL DEGREES)"),
    tav = .met_constant(tav, "(oC)", "annual average ambient temperature"),
    amp = .met_constant(amp, "(oC)", "annual amplitude in mean monthly temperature")
  )
  if (!is.null(longitude)) {
    standard_constants <- append(
      standard_constants,
      list(longitude = .met_constant(longitude, "(DECIMAL DEGREES)")),
      after = 1L
    )
  }
  if (!is.null(site)) {
    standard_constants <- append(
      list(site = .met_constant(site, NULL)),
      standard_constants
    )
  }
  additional <- .normalise_met_constants(constants)
  standard_constants[names(additional)] <- additional
  x <- structure(
    list(
      section = section,
      comments = as.character(comments),
      constants = standard_constants,
      units = units,
      data = data
    ),
    class = "spacing_met"
  )
  validate_spacing_met(x)
  x
}

#' Prepare an APSIM weather object from a raw data frame
#'
#' Convenience wrapper that renames source columns to APSIM names, converts
#' units and then calls [new_spacing_met()]. No gap filling or interpolation is
#' performed: missing days make validation fail.
#'
#' @param data Input data frame with the source columns.
#' @param column_map Optional named character vector: names are APSIM column
#'   names and values are source column names, e.g.
#'   `c(maxt = "tmax", mint = "tmin")`.
#' @param conversions Named list keyed by APSIM column name, applied after
#'   renaming. Each element is a function of the column or
#'   `list(multiplier = ..., offset = ...)` (value * multiplier + offset).
#' @inheritParams new_spacing_met
#' @return A `spacing_met` object (see [new_spacing_met()]).
#' @family weather files
#' @export
#' @examples
#' raw <- data.frame(date = seq(as.Date("2020-06-01"), by = "day", length.out = 3),
#'   srad_kj = c(25000, 27000, 22000), tmax = c(28, 30, 27), tmin = c(15, 16, 14),
#'   prcp = c(0, 0, 4))
#' prepare_spacing_met(raw, latitude = 44.9,
#'   column_map = c(radn = "srad_kj", maxt = "tmax", mint = "tmin", rain = "prcp"),
#'   conversions = list(radn = list(multiplier = 0.001)))$data
prepare_spacing_met <- function(
    data,
    latitude,
    longitude = NULL,
    site = NULL,
    column_map = NULL,
    conversions = list(),
    units = NULL,
    constants = list(),
    section = "[weather.met.weather]",
    comments = character(),
    add_year_day = TRUE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.null(column_map)) {
    if (is.null(names(column_map)) || any(!nzchar(names(column_map)))) {
      .rspacing_abort("column_map must be named with APSIM column names.",
                      "spacing_met_error")
    }
    missing <- setdiff(unname(column_map), names(data))
    if (length(missing)) {
      .rspacing_abort(
        paste("Source weather columns not found:", paste(missing, collapse = ", ")),
        "spacing_met_error"
      )
    }
    for (target in names(column_map)) {
      names(data)[names(data) == column_map[[target]]] <- target
    }
  }
  names(data) <- tolower(names(data))
  if (length(conversions)) {
    if (is.null(names(conversions)) || any(!nzchar(names(conversions)))) {
      .rspacing_abort("conversions must be a named list.", "spacing_met_error")
    }
    for (nm in names(conversions)) {
      if (!nm %in% names(data)) {
        .rspacing_abort(sprintf("Conversion column not found: %s", nm),
                        "spacing_met_error")
      }
      rule <- conversions[[nm]]
      if (is.function(rule)) {
        data[[nm]] <- rule(data[[nm]])
      } else if (is.list(rule)) {
        multiplier <- rule$multiplier %||% 1
        offset <- rule$offset %||% 0
        data[[nm]] <- data[[nm]] * multiplier + offset
      } else {
        .rspacing_abort(
          sprintf("Conversion for '%s' must be a function or list.", nm),
          "spacing_met_error"
        )
      }
    }
  }
  new_spacing_met(
    data = data,
    latitude = latitude,
    longitude = longitude,
    site = site,
    units = units,
    constants = constants,
    section = section,
    comments = comments,
    add_year_day = add_year_day
  )
}

#' Validate an APSIM weather object
#'
#' Checks required columns and constants, units, dates, daily continuity,
#' duplicate dates, finite values, temperature ordering (`maxt >= mint`) and
#' non-negative radiation and rain.
#'
#' @param x A `spacing_met` object.
#' @param require_continuous Logical. Require one row for every calendar day.
#' @param allow_missing Logical. Accept missing values in weather columns.
#' @param require_core Logical. Require `radn`, `maxt`, `mint` and `rain`.
#' @param check_physical Logical. Apply basic physical-range checks.
#' @param start_date,end_date Optional dates (or strings coercible to `Date`)
#'   that the data must cover.
#'
#' @return Invisibly, a data frame of issues with columns `severity`,
#'   `code` and `message` (no rows when all is well). When any issue is an error, a `spacing_met_validation_error` is
#'   raised instead, with the issues in its `issues` field.
#' @family weather files
#' @export
#' @examples
#' wx <- data.frame(date = seq(as.Date("2020-01-01"), by = "day", length.out = 5),
#'   radn = c(5, 6, 4, 7, 6), maxt = c(8, 9, 7, 10, 9), mint = c(-1, 0, -2, 1, 0),
#'   rain = c(0, 2.5, 0, 0, 1))
#' met <- new_spacing_met(wx, latitude = 44.9, longitude = 10.6, site = "demo")
#' validate_spacing_met(met)
#' try(validate_spacing_met(met, end_date = "2020-12-31"))
validate_spacing_met <- function(
    x,
    require_continuous = TRUE,
    allow_missing = FALSE,
    require_core = TRUE,
    check_physical = TRUE,
    start_date = NULL,
    end_date = NULL) {
  if (!inherits(x, "spacing_met")) {
    .rspacing_abort("x is not a spacing_met object.", "spacing_met_error")
  }
  issues <- list()
  add_issue <- function(severity, code, message) {
    issues[[length(issues) + 1L]] <<- data.frame(
      severity = severity, code = code, message = message,
      stringsAsFactors = FALSE
    )
  }
  data <- x$data
  if (!is.data.frame(data) || nrow(data) == 0L) {
    add_issue("error", "empty_data", "Weather data are empty.")
  }
  if (!identical(names(x$units), names(data))) {
    add_issue("error", "units_mismatch",
              "Units must be named and ordered exactly like weather columns.")
  }
  core <- c("radn", "maxt", "mint", "rain")
  if (require_core) {
    missing <- setdiff(core, names(data))
    if (length(missing)) add_issue(
      "error", "missing_core",
      paste("Missing core columns:", paste(missing, collapse = ", "))
    )
  }
  dates <- tryCatch(.met_dates(data), error = function(e) {
    add_issue("error", "invalid_time", conditionMessage(e)); as.Date(character())
  })
  if (length(dates)) {
    if (anyNA(dates)) add_issue("error", "missing_dates", "Dates contain NA values.")
    if (anyDuplicated(dates)) add_issue("error", "duplicate_dates", "Duplicate weather dates found.")
    if (is.unsorted(dates, strictly = FALSE)) add_issue("error", "unsorted_dates", "Weather dates are not increasing.")
    if (require_continuous && length(dates) > 1L && any(diff(dates) != 1)) {
      gaps <- dates[-length(dates)][diff(dates) != 1]
      add_issue("error", "date_gaps",
                paste("Weather series has gaps after:", paste(head(gaps, 5), collapse = ", ")))
    }
    if (!is.null(start_date) && min(dates) > as.Date(start_date)) {
      add_issue("error", "late_start", "Weather data begin after the expected start date.")
    }
    if (!is.null(end_date) && max(dates) < as.Date(end_date)) {
      add_issue("error", "early_end", "Weather data end before the expected end date.")
    }
  }
  numeric_weather <- intersect(core, names(data))
  for (nm in numeric_weather) {
    if (!is.numeric(data[[nm]])) add_issue("error", "non_numeric", paste(nm, "must be numeric."))
    if (!allow_missing && anyNA(data[[nm]])) add_issue("error", "missing_values", paste(nm, "contains missing values."))
    if (any(is.infinite(data[[nm]]), na.rm = TRUE)) add_issue("error", "infinite_values", paste(nm, "contains infinite values."))
  }
  if (check_physical) {
    if (all(c("maxt", "mint") %in% names(data)) &&
        any(data$maxt < data$mint, na.rm = TRUE)) {
      add_issue("error", "temperature_order", "Some maxt values are lower than mint.")
    }
    if ("rain" %in% names(data) && any(data$rain < 0, na.rm = TRUE)) {
      add_issue("error", "negative_rain", "Rain contains negative values.")
    }
    if ("radn" %in% names(data) && any(data$radn < 0, na.rm = TRUE)) {
      add_issue("error", "negative_radiation", "Radn contains negative values.")
    }
  }
  constants <- x$constants
  for (nm in c("latitude", "tav", "amp")) {
    if (is.null(constants[[nm]]) || length(constants[[nm]]$value) != 1L ||
        is.na(constants[[nm]]$value)) {
      add_issue("error", "missing_constant", paste("Missing or invalid constant:", nm))
    }
  }
  result <- if (length(issues)) do.call(rbind, issues) else data.frame(
    severity = character(), code = character(), message = character(),
    stringsAsFactors = FALSE
  )
  if (any(result$severity == "error")) {
    .rspacing_abort(
      paste(result$message[result$severity == "error"], collapse = " "),
      "spacing_met_validation_error",
      issues = result
    )
  }
  invisible(result)
}

#' Write an APSIM `.met` file
#'
#' Writes comments, section, constants, column names, units and daily values in
#' the APSIM `.met` layout. The `date`, `month` and `day_of_month` helper
#' columns are written as they are, after `year` and `day`.
#'
#' @param x A `spacing_met` object.
#' @param path Character scalar. Destination file.
#' @param overwrite Logical. Replace an existing file.
#' @param digits Integer. Significant digits for numeric daily values.
#' @param na Character scalar written for missing values.
#' @param validate Logical. Validate with [validate_spacing_met()] before
#'   writing.
#'
#' @return The absolute path of the file, invisibly.
#' @family weather files
#' @export
#' @examples
#' wx <- data.frame(date = seq(as.Date("2020-01-01"), by = "day", length.out = 5),
#'   radn = c(5, 6, 4, 7, 6), maxt = c(8, 9, 7, 10, 9), mint = c(-1, 0, -2, 1, 0),
#'   rain = c(0, 2.5, 0, 0, 1))
#' met <- new_spacing_met(wx, latitude = 44.9, longitude = 10.6, site = "demo")
#' path <- write_spacing_met(met, tempfile(fileext = ".met"))
#' readLines(path, n = 10)
write_spacing_met <- function(x, path, overwrite = FALSE, digits = 10,
                              na = "?", validate = TRUE) {
  if (validate) validate_spacing_met(x)
  path <- as.character(fs::path_abs(path))
  if (tolower(fs::path_ext(path)) != "met") {
    path <- paste0(path, ".met")
  }
  if (fs::file_exists(path) && !overwrite) {
    .rspacing_abort(paste("MET file already exists; set overwrite=TRUE:", path),
                    "spacing_met_error")
  }
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  lines <- character()
  if (length(x$comments)) {
    lines <- c(lines, paste0("! ", sub("^!\\s*", "", x$comments)))
  }
  lines <- c(lines, x$section)
  for (nm in names(x$constants)) {
    constant <- x$constants[[nm]]
    value <- .format_met_scalar(constant$value, digits)
    unit <- if (is.null(constant$unit) || !nzchar(constant$unit)) "" else paste0(" ", .normalise_met_unit(constant$unit))
    comment <- if (is.null(constant$comment) || !nzchar(constant$comment)) "" else paste0(" ! ", constant$comment)
    lines <- c(lines, paste0(nm, " = ", value, unit, comment))
  }
  column_names <- names(x$data)
  lines <- c(lines, paste(column_names, collapse = " "))
  lines <- c(lines, paste(unname(x$units[column_names]), collapse = " "))
  data_lines <- apply(x$data, 1L, function(row) {
    paste(vapply(seq_along(row), function(i) {
      value <- row[[i]]
      if (is.na(value)) return(na)
      if (inherits(x$data[[i]], "Date")) return(format(as.Date(value), "%Y/%m/%d"))
      if (is.numeric(x$data[[i]])) return(format(signif(as.numeric(value), digits), scientific = FALSE, trim = TRUE))
      as.character(value)
    }, character(1)), collapse = " ")
  })
  lines <- c(lines, data_lines)
  temp <- tempfile(pattern = paste0(fs::path_file(path), "."),
                   tmpdir = fs::path_dir(path), fileext = ".tmp")
  on.exit(if (fs::file_exists(temp)) fs::file_delete(temp), add = TRUE)
  writeLines(lines, temp, useBytes = TRUE)
  if (fs::file_exists(path)) fs::file_delete(path)
  fs::file_move(temp, path)
  invisible(path)
}

#' Read an APSIM `.met` file
#'
#' Reads comments, section, constants, units and daily values. Unknown columns
#' and constants are kept. Constant names are lower-cased, so files using
#' `Latitude`, `TAV` or `AMP` are read the same way.
#'
#' @param path Character scalar. Existing `.met` file.
#' @param validate Logical. Validate the result with [validate_spacing_met()].
#' @param na.strings Character vector of values read as missing.
#'
#' @return A `spacing_met` object (see [new_spacing_met()]).
#' @family weather files
#' @export
#' @examples
#' met <- read_spacing_met(system.file("extdata", "minimal.met", package = "rSpacing"))
#' met
#' met$units
read_spacing_met <- function(path, validate = TRUE,
                             na.strings = c("?", "NA", "NaN", "*")) {
  path <- .normalise_existing(path, "MET file", "file")
  raw <- readLines(path, warn = FALSE)
  raw <- sub("^\\ufeff", "", raw)
  trimmed <- trimws(raw)
  comments <- sub("^!\\s*", "", trimmed[grepl("^!", trimmed)])
  content_idx <- which(nzchar(trimmed) & !grepl("^!", trimmed))
  if (!length(content_idx)) .rspacing_abort("MET file is empty.", "spacing_met_error")
  section <- NULL
  first <- content_idx[[1]]
  if (grepl("^\\[.*\\]$", trimmed[[first]]) ||
      identical(tolower(trimmed[[first]]), "weather.met.weather")) {
    section <- trimmed[[first]]
    content_idx <- content_idx[-1]
  }
  constants <- list()
  heading_pos <- NA_integer_
  for (idx in content_idx) {
    line <- trimmed[[idx]]
    if (grepl("^[A-Za-z_][A-Za-z0-9_.-]*\\s*=", line)) {
      parsed <- .parse_met_constant(line)
      constants[[tolower(parsed$name)]] <- parsed$value
    } else {
      heading_pos <- idx
      break
    }
  }
  if (is.na(heading_pos) || heading_pos >= length(raw)) {
    .rspacing_abort("Cannot locate MET headings and units lines.", "spacing_met_error")
  }
  unit_pos <- which(seq_along(raw) > heading_pos & nzchar(trimws(raw)) &
                    !grepl("^!", trimws(raw)))[1]
  if (is.na(unit_pos)) .rspacing_abort("Cannot locate MET units line.", "spacing_met_error")
  headings <- strsplit(trimws(raw[[heading_pos]]), "\\s+")[[1]]
  units <- .split_met_units(trimws(raw[[unit_pos]]))
  if (length(units) != length(headings)) {
    .rspacing_abort("Number of MET units does not match headings.", "spacing_met_error")
  }
  data_lines <- raw[seq.int(unit_pos + 1L, length(raw))]
  data_lines <- data_lines[nzchar(trimws(data_lines)) & !grepl("^!", trimws(data_lines))]
  if (!length(data_lines)) .rspacing_abort("MET file contains no daily data.", "spacing_met_error")
  text_connection <- textConnection(data_lines)
  on.exit(close(text_connection), add = TRUE)
  data <- utils::read.table(
    text_connection, header = FALSE, col.names = headings,
    stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = na.strings, fill = FALSE, comment.char = ""
  )
  names(data) <- tolower(names(data))
  names(units) <- names(data)
  if ("date" %in% names(data)) {
    candidate <- suppressWarnings(as.Date(data$date, tryFormats = c("%Y/%m/%d", "%Y-%m-%d", "%d/%m/%Y")))
    if (!all(is.na(candidate))) data$date <- candidate
  }
  x <- structure(
    list(
      section = section %||% "[weather.met.weather]",
      comments = comments,
      constants = constants,
      units = units,
      data = data,
      source_file = path
    ),
    class = "spacing_met"
  )
  if (validate) validate_spacing_met(x)
  x
}

#' Add or replace a daily weather column
#'
#' @param x A `spacing_met` object.
#' @param name Character scalar. APSIM column name, e.g. `"co2"`.
#' @param values Vector with one value per row, or one value recycled.
#' @param unit Character scalar. Unit, with or without parentheses.
#' @param position Optional integer. Column position of a new column (default:
#'   last). A replaced column keeps its position.
#' @param validate Logical. Validate the result.
#'
#' @return The updated `spacing_met` object.
#' @family weather files
#' @export
#' @examples
#' met <- read_spacing_met(system.file("extdata", "minimal.met", package = "rSpacing"))
#' met <- set_spacing_met_column(met, "co2", 420, unit = "ppm")
#' met$units
set_spacing_met_column <- function(x, name, values, unit = "()",
                                   position = NULL, validate = TRUE) {
  if (!inherits(x, "spacing_met")) .rspacing_abort("x is not a spacing_met object.", "spacing_met_error")
  name <- tolower(trimws(name))
  if (!nzchar(name) || length(name) != 1L) .rspacing_abort("name must be one non-empty string.", "spacing_met_error")
  if (length(values) == 1L) values <- rep(values, nrow(x$data))
  if (length(values) != nrow(x$data)) .rspacing_abort("values length must equal the number of weather rows.", "spacing_met_error")
  existed <- name %in% names(x$data)
  x$data[[name]] <- values
  x$units[[name]] <- .normalise_met_unit(unit)
  if (!existed && !is.null(position)) {
    position <- as.integer(position)
    if (!is.finite(position) || position < 1L || position > ncol(x$data)) {
      .rspacing_abort("position is outside the available column range.", "spacing_met_error")
    }
    order <- append(setdiff(names(x$data), name), name, after = position - 1L)
    x$data <- x$data[order]
    x$units <- x$units[order]
  } else {
    x$units <- x$units[names(x$data)]
  }
  if (validate) validate_spacing_met(x)
  x
}

#' Remove a daily weather column
#'
#' @param x A `spacing_met` object.
#' @param name Character scalar. Column to remove (an error when absent).
#' @param validate Logical. Validate the result (removing a core column then
#'   fails).
#' @return The updated `spacing_met` object.
#' @family weather files
#' @export
#' @examples
#' met <- read_spacing_met(system.file("extdata", "minimal.met", package = "rSpacing"))
#' met <- set_spacing_met_column(met, "wind", 2, unit = "m/s")
#' names(remove_spacing_met_column(met, "wind")$data)
remove_spacing_met_column <- function(x, name, validate = TRUE) {
  name <- tolower(name)
  if (!name %in% names(x$data)) .rspacing_abort(paste("Weather column not found:", name), "spacing_met_error")
  x$data[[name]] <- NULL
  x$units <- x$units[names(x$data)]
  if (validate) validate_spacing_met(x)
  x
}

#' Set or replace an APSIM weather constant
#'
#' @param x A `spacing_met` object.
#' @param name Character scalar. Constant name (lower-cased), e.g. `"tav"`.
#' @param value Scalar value.
#' @param unit Optional unit, written in parentheses after the value.
#' @param comment Optional comment, written after `!`.
#' @param validate Logical. Validate the result.
#' @return The updated `spacing_met` object.
#' @family weather files
#' @export
#' @examples
#' met <- read_spacing_met(system.file("extdata", "minimal.met", package = "rSpacing"))
#' met <- set_spacing_met_constant(met, "station", "Example-01", comment = "WMO id")
#' met$constants$station
set_spacing_met_constant <- function(x, name, value, unit = NULL,
                                     comment = NULL, validate = TRUE) {
  name <- trimws(name)
  if (!nzchar(name) || length(value) != 1L) .rspacing_abort("Constant name and value must be scalar.", "spacing_met_error")
  x$constants[[name]] <- .met_constant(value, unit, comment)
  if (validate) validate_spacing_met(x)
  x
}

#' Restrict weather data to a date interval
#'
#' @param x A `spacing_met` object.
#' @param start_date,end_date Inclusive limits as `Date` or strings coercible
#'   to `Date`; `NULL` keeps that end open.
#' @param recalculate_tav_amp Logical. Recalculate `tav` and `amp` from the
#'   retained period.
#' @return The updated and validated `spacing_met` object.
#' @family weather files
#' @export
#' @examples
#' wx <- data.frame(date = seq(as.Date("2020-01-01"), by = "day", length.out = 5),
#'   radn = c(5, 6, 4, 7, 6), maxt = c(8, 9, 7, 10, 9), mint = c(-1, 0, -2, 1, 0),
#'   rain = c(0, 2.5, 0, 0, 1))
#' met <- new_spacing_met(wx, latitude = 44.9, longitude = 10.6, site = "demo")
#' subset_spacing_met(met, "2020-01-02", "2020-01-04")$data
subset_spacing_met <- function(x, start_date = NULL, end_date = NULL,
                               recalculate_tav_amp = TRUE) {
  dates <- .met_dates(x$data)
  keep <- rep(TRUE, length(dates))
  if (!is.null(start_date)) keep <- keep & dates >= as.Date(start_date)
  if (!is.null(end_date)) keep <- keep & dates <= as.Date(end_date)
  if (!any(keep)) .rspacing_abort("Date subset contains no weather rows.", "spacing_met_error")
  x$data <- x$data[keep, , drop = FALSE]
  if (recalculate_tav_amp) {
    values <- .calculate_tav_amp(x$data)
    x <- set_spacing_met_constant(x, "tav", values$tav, "(oC)",
                                  "annual average ambient temperature", validate = FALSE)
    x <- set_spacing_met_constant(x, "amp", values$amp, "(oC)",
                                  "annual amplitude in mean monthly temperature", validate = FALSE)
  }
  validate_spacing_met(x)
  x
}

#' Point the Weather models of an `.apsimx` file to a `.met` file
#'
#' Recursively finds `Models.Climate.Weather` nodes and replaces their
#' `FileName`, so that a template `.apsimx` uses a generated `.met` file,
#' without the external `apsimx` R package. Tool2 users do not need this:
#' Tool2 writes the weather path itself.
#'
#' @param apsimx_file Character scalar. Existing `.apsimx` JSON file.
#' @param met_file Character scalar. Existing `.met` file; its absolute path is
#'   written.
#' @param output Optional destination. By default a copy named
#'   `<name>_weather.apsimx` next to `apsimx_file`.
#' @param weather_name Optional Weather node `Name`; `NULL` updates every
#'   Weather node.
#' @param overwrite Logical. Replace an existing destination.
#'
#' @return The absolute path of the written `.apsimx` file, invisibly. An error
#'   is raised when no matching Weather node exists.
#' @family weather files
#' @export
#' @examples
#' tmp <- tempfile(); dir.create(tmp)
#' apsimx <- file.path(tmp, "template.apsimx")
#' writeLines('{"$type": "Models.Core.Simulations, Models", "Name": "Simulations",
#'   "Children": [{"$type": "Models.Climate.Weather, Models", "Name": "Weather",
#'   "FileName": "old.met", "Children": []}]}', apsimx)
#' met <- system.file("extdata", "minimal.met", package = "rSpacing")
#' out <- set_apsimx_weather_file(apsimx, met)
#' jsonlite::fromJSON(out)$Children$FileName
set_apsimx_weather_file <- function(
    apsimx_file,
    met_file,
    output = NULL,
    weather_name = NULL,
    overwrite = FALSE) {
  apsimx_file <- .normalise_existing(apsimx_file, "APSIMX file", "file")
  met_file <- .normalise_existing(met_file, "MET file", "file")
  if (is.null(output)) {
    output <- fs::path(
      fs::path_dir(apsimx_file),
      paste0(fs::path_ext_remove(fs::path_file(apsimx_file)), "_weather.apsimx")
    )
  }
  output <- as.character(fs::path_abs(output))
  if (fs::file_exists(output) && !overwrite) .rspacing_abort("Output APSIMX file exists; set overwrite=TRUE.", "spacing_met_error")
  tree <- jsonlite::fromJSON(apsimx_file, simplifyVector = FALSE)
  changed <- 0L
  update_node <- function(node) {
    if (!is.list(node)) return(node)
    type <- node[["$type"]] %||% ""
    name <- node[["Name"]] %||% ""
    if (grepl("Models\\.Climate\\.Weather", type) &&
        (is.null(weather_name) || identical(name, weather_name))) {
      node[["FileName"]] <- met_file
      changed <<- changed + 1L
    }
    for (nm in names(node)) {
      child <- node[[nm]]
      if (is.list(child)) {
        if (!is.null(child[["$type"]])) {
          node[[nm]] <- update_node(child)
        } else {
          node[[nm]] <- lapply(child, update_node)
        }
      }
    }
    node
  }
  tree <- update_node(tree)
  if (changed == 0L) .rspacing_abort("No matching Weather model found in APSIMX file.", "spacing_met_error")
  fs::dir_create(fs::path_dir(output), recurse = TRUE)
  jsonlite::write_json(tree, output, pretty = TRUE, auto_unbox = TRUE,
                       null = "null", digits = NA)
  invisible(output)
}

#' @describeIn new_spacing_met Print rows, columns, period and site.
#' @param x A `spacing_met` object.
#' @param ... Ignored.
#' @export
print.spacing_met <- function(x, ...) {
  dates <- tryCatch(.met_dates(x$data), error = function(e) as.Date(character()))
  cat(
    "<spacing_met>\n",
    "  rows:    ", nrow(x$data), "\n",
    "  columns: ", paste(names(x$data), collapse = ", "), "\n",
    if (length(dates)) paste0("  period:  ", min(dates), " to ", max(dates), "\n") else "",
    "  site:    ", x$constants$site$value %||% "", "\n",
    sep = ""
  )
  invisible(x)
}

#' Complete units for all columns
#' @param data Weather data frame.
#' @param units Named unit vector supplied by the user, or `NULL`.
#' @return Named unit vector in column order, with parentheses.
#' @noRd
.complete_met_units <- function(data, units) {
  defaults <- spacing_met_default_units()
  if (is.null(units)) units <- character()
  if (is.null(names(units)) && length(units)) {
    if (length(units) != ncol(data)) .rspacing_abort("Unnamed units must have one value per weather column.", "spacing_met_error")
    names(units) <- names(data)
  }
  result <- setNames(rep("()", ncol(data)), names(data))
  common <- intersect(names(data), names(defaults)); result[common] <- defaults[common]
  common <- intersect(names(data), names(units)); result[common] <- units[common]
  result <- vapply(result, .normalise_met_unit, character(1))
  result
}

#' Add parentheses to a unit string
#' @param x Character vector of units.
#' @return Units wrapped in `()`.
#' @noRd
.normalise_met_unit <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return("()")
  x <- trimws(as.character(x))
  if (!startsWith(x, "(")) x <- paste0("(", x)
  if (!endsWith(x, ")")) x <- paste0(x, ")")
  x
}

#' Build a constant record
#' @param value,unit,comment Constant value, unit and comment.
#' @return `list(value, unit, comment)`.
#' @noRd
.met_constant <- function(value, unit = NULL, comment = NULL) {
  list(value = value, unit = if (is.null(unit)) NULL else .normalise_met_unit(unit),
       comment = comment)
}

#' Normalise user constants
#' @param constants Named list of scalars or constant records.
#' @return Named list of constant records with lower-case names.
#' @noRd
.normalise_met_constants <- function(constants) {
  if (!is.list(constants) || (length(constants) &&
      (is.null(names(constants)) || any(!nzchar(names(constants)))))) {
    .rspacing_abort("constants must be a fully named list.", "spacing_met_error")
  }
  lapply(constants, function(x) {
    if (is.list(x) && "value" %in% names(x)) {
      .met_constant(x$value, x$unit %||% NULL, x$comment %||% NULL)
    } else {
      .met_constant(x)
    }
  })
}

#' Derive dates from weather data
#' @param data Data frame with `date`, `year`+`day` or `year`+`month`+`day_of_month`.
#' @return A `Date` vector, one per row.
#' @noRd
.met_dates <- function(data) {
  if ("date" %in% names(data)) {
    result <- as.Date(data$date)
  } else if (all(c("year", "day") %in% names(data))) {
    result <- as.Date(paste0(as.integer(data$year), "-01-01")) + as.integer(data$day) - 1L
    if (any(as.integer(format(result, "%Y")) != as.integer(data$year))) stop("Invalid year/day combinations.")
  } else if (all(c("year", "month", "day_of_month") %in% names(data))) {
    result <- as.Date(sprintf("%04d-%02d-%02d", as.integer(data$year), as.integer(data$month), as.integer(data$day_of_month)))
  } else {
    stop("Time columns must be date, year/day, or year/month/day_of_month.")
  }
  result
}

#' Calculate tav and amp
#' @param data Data frame with dates, `maxt` and `mint`.
#' @return `list(tav, amp)`: mean daily temperature, and range of the
#'   monthly climatology (degC).
#' @noRd
.calculate_tav_amp <- function(data) {
  if (!all(c("maxt", "mint") %in% names(data))) {
    .rspacing_abort("maxt and mint are required to calculate tav and amp.", "spacing_met_error")
  }
  dates <- .met_dates(data)
  daily_mean <- (as.numeric(data$maxt) + as.numeric(data$mint)) / 2
  if (anyNA(daily_mean)) .rspacing_abort("Cannot calculate tav/amp with missing temperatures.", "spacing_met_error")
  monthly <- tapply(daily_mean, format(dates, "%Y-%m"), mean)
  month_number <- as.integer(substr(names(monthly), 6, 7))
  monthly_climatology <- tapply(as.numeric(monthly), month_number, mean)
  list(tav = mean(daily_mean), amp = max(monthly_climatology) - min(monthly_climatology))
}

#' Format a constant value
#' @param x Scalar value.
#' @param digits Significant digits.
#' @return Character scalar.
#' @noRd
.format_met_scalar <- function(x, digits) {
  if (is.numeric(x)) format(signif(x, digits), scientific = FALSE, trim = TRUE) else as.character(x)
}

#' Parse a constant line
#' @param line Character scalar `name = value (unit) ! comment`.
#' @return `list(name, value)` with a constant record, or `NULL`.
#' @noRd
.parse_met_constant <- function(line) {
  parts <- strsplit(line, "!", fixed = TRUE)[[1]]
  body <- trimws(parts[[1]])
  comment <- if (length(parts) > 1L) {
    trimws(paste(parts[-1], collapse = "!"))
  } else {
    NULL
  }
  equal <- regexpr("=", body, fixed = TRUE)[[1]]
  if (equal < 2L) {
    .rspacing_abort(paste("Invalid MET constant:", line), "spacing_met_error")
  }
  name <- trimws(substr(body, 1L, equal - 1L))
  remainder <- trimws(substr(body, equal + 1L, nchar(body)))
  unit_match <- regexpr("\\s+\\([^)]*\\)\\s*$", remainder, perl = TRUE)
  if (unit_match[[1]] > 0L) {
    unit <- trimws(regmatches(remainder, unit_match))
    raw_value <- trimws(substr(remainder, 1L, unit_match[[1]] - 1L))
  } else {
    unit <- NULL
    raw_value <- remainder
  }
  numeric_value <- suppressWarnings(as.numeric(raw_value))
  value <- if (!is.na(numeric_value) &&
               grepl("^[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$",
                     raw_value, perl = TRUE)) {
    numeric_value
  } else {
    raw_value
  }
  list(name = name, value = .met_constant(value, unit, comment))
}

#' Split the units line
#' @param line Character scalar with parenthesised units.
#' @return Character vector of units.
#' @noRd
.split_met_units <- function(line) {
  units <- regmatches(line, gregexpr("\\([^)]*\\)", line))[[1]]
  if (identical(units, character(0))) strsplit(line, "\\s+")[[1]] else units
}
