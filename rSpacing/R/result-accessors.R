#' Access the standard tables of a spacing result
#'
#' Return the daily table (`DailyReport`), the annual report table (`Report`)
#' or the crop summary (one row per harvest written by Tool2) of a
#' `spacing_result`. CSV exports are used first, then DataStore tables;
#' file-specific suffixes in table names are ignored. Empty tables are
#' returned by default, so that an empty result can be told from a missing
#' table.
#'
#' @param result A `spacing_result` returned by [read_spacing()].
#' @param strict Logical. `TRUE`: a missing table raises an error. `FALSE`:
#'   a missing table returns `NULL` with a warning.
#' @param allow_empty Logical. `FALSE`: a table with zero rows is treated as
#'   missing.
#' @return A data frame (one row per day, per report event or per harvest), or
#'   `NULL` when the table is missing and `strict = FALSE`.
#' @family results
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' dim(spacing_daily(result))
#' spacing_report(result)[1:3, c("Year", "Maize_GrainYield_kgha")]
#' spacing_crop_summary(result)
#' suppressWarnings(spacing_crop_summary(result, strict = FALSE, allow_empty = FALSE))
#' @name spacing_result_tables
NULL

#' Check the class of a result
#' @param result Object to check.
#' @return `result`, invisibly; an error otherwise.
#' @noRd
.validate_spacing_result <- function(result) {
  if (!inherits(result, "spacing_result")) {
    .rspacing_abort(
      "'result' must be a spacing_result returned by read_spacing().",
      "spacing_result_error"
    )
  }
  invisible(result)
}

#' Find a standard table in a result
#' @param result A `spacing_result`.
#' @param prefix Table prefix (`daily`, `report`, `crop_summary`).
#' @param label Description for messages.
#' @param strict,allow_empty See `spacing_result_tables`.
#' @return A data frame, or `NULL`.
#' @noRd
.find_spacing_table <- function(result, prefix, label, strict, allow_empty) {
  .validate_spacing_result(result)
  csv <- result$csv
  matches <- integer()
  if (is.list(csv) && length(csv)) {
    nms <- names(csv)
    if (is.null(nms)) nms <- rep("", length(csv))
    matches <- grep(paste0("^", prefix), nms, ignore.case = TRUE)
  }
  if (!length(matches)) {
    msg <- sprintf("The %s table was not found in result$csv.", label)
    if (isTRUE(strict)) .rspacing_abort(msg, "spacing_schema_error")
    warning(msg, call. = FALSE)
    return(NULL)
  }
  candidates <- csv[matches]
  valid <- which(vapply(candidates, is.data.frame, logical(1)))
  if (!length(valid)) {
    msg <- sprintf("The %s output exists but is not a data frame.", label)
    if (isTRUE(strict)) .rspacing_abort(msg, "spacing_schema_error")
    warning(msg, call. = FALSE)
    return(NULL)
  }
  candidates <- candidates[valid]
  nonempty <- which(vapply(candidates, nrow, integer(1)) > 0L)
  value <- if (length(nonempty)) candidates[[nonempty[[1L]]]] else candidates[[1L]]
  if (!isTRUE(allow_empty) && nrow(value) == 0L) {
    msg <- sprintf("The %s table is present but has zero rows.", label)
    if (isTRUE(strict)) .rspacing_abort(msg, "spacing_empty_table_error")
    warning(msg, call. = FALSE)
    return(NULL)
  }
  value
}

#' @rdname spacing_result_tables
#' @export
spacing_daily <- function(result, strict = TRUE, allow_empty = TRUE) {
  .find_spacing_table(result, "daily_", "daily", strict, allow_empty)
}

#' @rdname spacing_result_tables
#' @export
spacing_report <- function(result, strict = TRUE, allow_empty = TRUE) {
  .find_spacing_table(result, "report_", "report", strict, allow_empty)
}

#' @rdname spacing_result_tables
#' @export
spacing_crop_summary <- function(result, strict = TRUE, allow_empty = TRUE) {
  .find_spacing_table(result, "crop_summary_", "crop summary", strict, allow_empty)
}

#' Extract the APSIM units table
#'
#' Returns the `_Units` table of the APSIM DataStore read with the result.
#'
#' @param result A `spacing_result`.
#' @param database Optional database name or index when the result holds
#'   several DataStores.
#' @return A data frame with `TableName`, `ColumnHeading` and `Units`, or an
#'   empty data frame when no units table is available.
#' @family results
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' head(spacing_units(result))
spacing_units <- function(result, database = NULL) {
  .validate_spacing_result(result)
  units <- result$units
  if (!is.list(units) || !length(units)) {
    return(data.frame(TableName=character(), ColumnHeading=character(), Units=character()))
  }
  if (is.null(database)) {
    pieces <- Filter(is.data.frame, units)
  } else {
    if (is.numeric(database)) database <- names(units)[[as.integer(database[[1L]])]]
    pieces <- list(units[[database]])
    pieces <- Filter(is.data.frame, pieces)
  }
  if (!length(pieces)) {
    return(data.frame(TableName=character(), ColumnHeading=character(), Units=character()))
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

#' Unit of an output variable
#'
#' @param result A `spacing_result`.
#' @param variable Character scalar. Output column name.
#' @param table Optional APSIM report table name, e.g. `"DailyReport"` or
#'   `"Report"`.
#' @param default Value returned when no unit is recorded.
#' @return A character scalar.
#' @family results
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' spacing_variable_unit(result, "Rain_mm")
#' spacing_variable_unit(result, "NotAColumn", default = NA)
spacing_variable_unit <- function(result, variable, table = NULL, default = "") {
  u <- spacing_units(result)
  if (!nrow(u) || !all(c("ColumnHeading", "Units") %in% names(u))) return(default)
  keep <- u$ColumnHeading == variable
  if (!is.null(table) && "TableName" %in% names(u)) keep <- keep & u$TableName == table
  values <- unique(u$Units[keep & !is.na(u$Units) & nzchar(u$Units)])
  if (!length(values)) default else paste(values, collapse = ", ")
}

#' Classify output variables
#'
#' Assigns each variable a scientific group, a temporal role and the annual
#' summary to use, from exact names and anchored patterns (so that `Grain`
#' is not taken for rain, nor `CO2C` for atmospheric CO2). Used by the plot
#' functions when `summary = "auto"`.
#'
#' @param variables Character vector of variable names.
#' @return A data frame with one row per unique variable and columns
#'   `variable`, `group` (`"weather"`, `"water_balance"`, `"soil_water"`,
#'   `"soil_nitrogen"`, `"nitrogen_loss"`, `"soil_carbon"`, `"carbon_flux"`,
#'   `"crop"` or `"other"`), `temporal_role` (`"daily"`, `"state"`, `"flux"`,
#'   `"model_output"` or `"unknown"`) and `recommended_summary` (`"sum"` or
#'   `"mean"`).
#' @family results
#' @export
#' @examples
#' classify_spacing_variables(c("Rain_mm", "SWmm(1)", "N2O_kgNha(2)", "Maize_LAI"))
classify_spacing_variables <- function(variables) {
  variables <- unique(as.character(variables))
  classify_one <- function(v) {
    if (v %in% c("Tx","Tn","Rain_mm","Radn_MJm2","Wind_ms","VP_hPa","ET0_mm")) {
      annual <- if (v %in% c("Rain_mm", "ET0_mm")) "sum" else "mean"
      return(c("weather","daily",annual))
    }
    if (grepl("^(PotEvapSoil|ActEvapSoil|Runoff|Perc|Irrig)_mm$", v)) return(c("water_balance","daily","sum"))
    if (grepl("^SWmm\\(|^SoilSWtot_mm$", v)) return(c("soil_water","state","mean"))
    if (grepl("^(NO3|NH4)_kgha\\(|^(MinerN|NitrN)_kgNha$|^SoilNO3_kgha$", v)) return(c("soil_nitrogen","state","mean"))
    if (grepl("^LeachNO3_kgNha$|^N2O_", v)) return(c("nitrogen_loss","flux","sum"))
    if (grepl("^SoilC", v)) return(c("soil_carbon","state","mean"))
    if (grepl("^CO2C_", v)) return(c("carbon_flux","flux","sum"))
    if (grepl("^[A-Za-z0-9]+_", v)) return(c("crop","model_output","mean"))
    c("other","unknown","mean")
  }
  values <- t(vapply(variables, classify_one, character(3)))
  data.frame(variable=variables, group=values[,1], temporal_role=values[,2], recommended_summary=values[,3], stringsAsFactors=FALSE)
}

#' List the tables of a result
#'
#' @param result A `spacing_result`.
#' @return A data frame with one row per table (CSV exports and DataStore
#'   tables) and columns `source`, `table`, `class`, `rows`, `columns`,
#'   `has_date`, `has_year`, `has_doy`, `numeric_variables` (comma-separated)
#'   and `empty`.
#' @family results
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' inventory_spacing_result(result)[, c("table", "rows", "columns", "empty")]
inventory_spacing_result <- function(result) {
  .validate_spacing_result(result)
  rows <- list(); k <- 0L
  add <- function(source, table, value) {
    k <<- k + 1L
    is_df <- is.data.frame(value)
    nms <- if (is_df) names(value) else character()
    rows[[k]] <<- data.frame(
      source=source, table=table, class=paste(class(value), collapse="/"),
      rows=if (is_df) nrow(value) else NA_integer_,
      columns=if (is_df) ncol(value) else NA_integer_,
      has_date=any(tolower(nms) %in% c("date","clock.today","today","datetime")),
      has_year=any(tolower(nms) == "year"),
      has_doy=any(tolower(nms) %in% c("doy","dayofyear","day_of_year")),
      numeric_variables=if (is_df) paste(nms[vapply(value, is.numeric, logical(1))], collapse=", ") else "",
      empty=is_df && nrow(value) == 0L,
      stringsAsFactors=FALSE
    )
  }
  if (is.list(result$csv)) for (nm in names(result$csv)) add("csv", nm, result$csv[[nm]])
  if (is.list(result$datastore)) {
    for (db in names(result$datastore)) {
      x <- result$datastore[[db]]
      if (is.list(x)) for (nm in names(x)) add(paste0("datastore:", db), nm, x[[nm]])
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}
