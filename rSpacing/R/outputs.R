#' Discover files produced by Spacing.Tool2
#'
#' Lists APSIM artefacts in the Tool2 output and weather directories of a
#' configuration, grouped by type. With `before`, only files that are new or
#' were modified after the snapshot are returned; [run_spacing()] uses this to
#' attach the outputs of a run.
#'
#' Files with extensions `apsimx`, `db`, `csv`, `met`, `json` and `log` are
#' considered.
#'
#' @param config A `spacing_config` object from [spacing_config()].
#' @param before Optional named numeric vector of modification times (seconds
#'   since the epoch) named by file path, as taken before a run. `NULL` returns
#'   every matching file.
#' @return A named list of character vectors of absolute paths: `apsimx`
#'   (APSIM simulation files), `database` (SQLite DataStores), `csv` (CSV
#'   outputs), `weather` (`.met` files) and `other` (`json`, `log`).
#' @family results
#' @export
#' @examples
#' root <- tempfile("spacing")
#' dir.create(file.path(root, "apsim", "src", "Spacing.Tool2"), recursive = TRUE)
#' jsonlite::write_json(list(OutputDir = "output", TempWeatherDir = "weather"),
#'                      file.path(root, "apsim", "spacing.config.json"), auto_unbox = TRUE)
#' cfg <- spacing_config(repository = root)
#' writeLines("a,b\n1,2", file.path(cfg$output_dir, "report_demo.csv"))
#' discover_spacing_outputs(cfg)$csv
discover_spacing_outputs <- function(config, before = NULL) {
  exts <- c("apsimx", "db", "csv", "met", "json", "log")
  dirs <- unique(c(config$output_dir, config$temp_weather_dir))
  files <- unlist(lapply(dirs, function(d) if (fs::dir_exists(d)) fs::dir_ls(d, recurse=TRUE, type="file") else character()))
  files <- files[tolower(fs::path_ext(files)) %in% exts]
  if (!is.null(before) && length(files)) {
    info <- fs::file_info(files); old <- before[as.character(files)]
    files <- files[is.na(old) | as.numeric(info$modification_time) > old]
  }
  files <- as.character(files)
  list(apsimx=files[tolower(fs::path_ext(files))=="apsimx"],
       database=files[tolower(fs::path_ext(files))=="db"],
       csv=files[tolower(fs::path_ext(files))=="csv"],
       weather=files[tolower(fs::path_ext(files))=="met"],
       other=files[!tolower(fs::path_ext(files)) %in% c("apsimx","db","csv","met")])
}

#' Read an APSIM SQLite DataStore
#'
#' Reads tables of an APSIM `.db` file (e.g. `Report`, `DailyReport`,
#' `_Messages`, `_Units`) into data frames.
#'
#' @param path Character scalar. Path of an existing `.db` file.
#' @param tables Character vector of table names, or `NULL` (default) for all
#'   tables. Unknown names raise a `spacing_schema_error`.
#' @return A named list of data frames, one per table.
#' @family results
#' @export
#' @examples
#' db <- tempfile(fileext = ".db")
#' con <- DBI::dbConnect(RSQLite::SQLite(), db)
#' DBI::dbWriteTable(con, "Report", data.frame(Year = 2020:2021, Yield = c(9.1, 8.7)))
#' DBI::dbDisconnect(con)
#' read_spacing_datastore(db)$Report
read_spacing_datastore <- function(path, tables = NULL) {
  path <- .normalise_existing(path, "DataStore", "file")
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con), add=TRUE)
  available <- DBI::dbListTables(con); selected <- tables %||% available
  missing <- setdiff(selected, available)
  if (length(missing)) .rspacing_abort(paste("Missing DataStore tables:", paste(missing, collapse=", ")), "spacing_schema_error")
  setNames(lapply(selected, function(tab) DBI::dbReadTable(con, tab)), selected)
}

#' Read a Spacing.Tool2 CSV output
#'
#' Reads a CSV written by Tool2 (`report_*`, `daily_*`, `crop_summary_*`)
#' keeping the original column names, e.g. `Clock.Today` or `NO3_kgha(1)`.
#'
#' @param path Character scalar. Path of an existing CSV file.
#' @param ... Further arguments passed to [utils::read.csv()].
#' @return A data frame; strings are not converted to factors and column names
#'   are not altered (`check.names = FALSE`).
#' @family results
#' @export
#' @examples
#' path <- system.file("extdata", "reference_maize", "tables", "report.csv", package = "rSpacing")
#' head(read_spacing_csv(path))
read_spacing_csv <- function(path, ...) {
  path <- .normalise_existing(path, "CSV output", "file")
  utils::read.csv(path, stringsAsFactors=FALSE, check.names=FALSE, ...)
}

#' Read all outputs of a run
#'
#' Imports every DataStore and CSV listed in `run$outputs` into one
#' `spacing_result`, the object used by the result accessors and plots.
#'
#' @param run A `spacing_run` object returned by [run_spacing()].
#' @param datastore_tables Optional character vector of DataStore tables to
#'   read (passed to [read_spacing_datastore()]); `NULL` reads all.
#' @return An object of class `spacing_result`: a list with `run` (the
#'   `spacing_run`), `datastore` (named list per `.db` file of named lists of
#'   tables), `csv` (named list of data frames), `apsimx` and `weather` (paths),
#'   `messages` (the `_Messages` tables) and `units` (the `_Units` tables).
#' @seealso [spacing_daily()], [spacing_report()], [summary.spacing_result()],
#'   [plot_spacing_result()].
#' @family results
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds", package = "rSpacing"))
#' result
#' \dontrun{
#' result <- read_spacing(run_spacing(cfg, request))
#' }
read_spacing <- function(run, datastore_tables = NULL) {
  if (!inherits(run, "spacing_run")) .rspacing_abort("run is not a spacing_run.", "spacing_validation_error")
  db <- setNames(lapply(run$outputs$database, read_spacing_datastore, tables=datastore_tables), basename(run$outputs$database))
  csv <- setNames(lapply(run$outputs$csv, read_spacing_csv), basename(run$outputs$csv))
  structure(list(run=run, datastore=db, csv=csv, apsimx=run$outputs$apsimx,
                 weather=run$outputs$weather, messages=.extract_messages(db),
                 units=.extract_units(db)), class="spacing_result")
}
#' APSIM message tables of a list of DataStores
#' @param db_list Named list of DataStores (lists of data frames).
#' @return Named list of `_Messages` data frames (DataStores without the table
#'   are dropped).
#' @noRd
.extract_messages <- function(db_list) Filter(Negate(is.null), lapply(db_list, function(x) x[["_Messages"]] %||% NULL))
#' APSIM unit tables of a list of DataStores
#' @param db_list Named list of DataStores (lists of data frames).
#' @return Named list of `_Units` data frames.
#' @noRd
.extract_units <- function(db_list) Filter(Negate(is.null), lapply(db_list, function(x) x[["_Units"]] %||% NULL))

#' Combine the CSV outputs of several results
#'
#' Row-binds, file by file, the CSV tables of several `spacing_result`
#' objects, e.g. the runs of a sensitivity analysis, adding provenance columns.
#'
#' Tables are matched by file name. Columns missing in some runs are filled with
#' `NA` before binding. Zero-row tables are kept, with zero-length provenance
#' columns.
#'
#' @param results A `spacing_result` or a list of them.
#' @return A named list (one element per CSV file name) of data frames with the
#'   extra columns `.run_id` and `.scenario_id`. An empty list when the results
#'   contain no CSV.
#' @family results
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds", package = "rSpacing"))
#' combined <- collect_spacing(list(result, result))
#' names(combined)
#' table(combined[[1]]$.run_id)
collect_spacing <- function(results) {
  if (inherits(results, "spacing_result")) results <- list(results)
  if (!is.list(results) || !length(results)) {
    .rspacing_abort(
      "`results` must contain at least one spacing_result object.",
      "spacing_validation_error"
    )
  }
  if (!all(vapply(results, inherits, logical(1), "spacing_result"))) {
    .rspacing_abort(
      "All elements must be spacing_result objects.",
      "spacing_validation_error"
    )
  }

  csv_names <- unique(unlist(
    lapply(results, function(x) names(x$csv)),
    use.names = FALSE
  ))
  if (!length(csv_names)) return(setNames(list(), character()))

  combined <- lapply(csv_names, function(nm) {
    chunks <- lapply(results, function(x) {
      z <- x$csv[[nm]]
      if (is.null(z)) return(NULL)
      if (!is.data.frame(z)) {
        .rspacing_abort(
          paste0("CSV output `", nm, "` is not a data frame."),
          "spacing_validation_error"
        )
      }

      # rep(..., nrow(z)) is length zero for a zero-row table. This preserves
      # the empty table and avoids `$<-.data.frame` assigning one value to zero
      # rows, while retaining character provenance columns.
      z$.run_id <- rep(as.character(x$run$run_id %||% NA_character_), nrow(z))
      z$.scenario_id <- rep(
        as.character(x$run$request$scenario_id %||% NA_character_),
        nrow(z)
      )
      z
    })
    chunks <- Filter(Negate(is.null), chunks)
    if (!length(chunks)) return(NULL)

    # Align columns across runs before row-binding. Missing columns are added
    # with NA and every data frame is reordered consistently.
    all_columns <- unique(unlist(lapply(chunks, names), use.names = FALSE))
    chunks <- lapply(chunks, function(z) {
      missing_columns <- setdiff(all_columns, names(z))
      if (length(missing_columns)) {
        for (column in missing_columns) z[[column]] <- rep(NA, nrow(z))
      }
      z[all_columns]
    })
    out <- do.call(rbind, chunks)
    rownames(out) <- NULL
    out
  })
  names(combined) <- csv_names
  combined
}

#' @describeIn read_spacing Print the number of DataStores, CSV files and
#'   APSIM files.
#' @param x A `spacing_result` object.
#' @param ... Ignored.
#' @export
print.spacing_result <- function(x, ...) {
  cat("<spacing_result>\n", "  databases: ", length(x$datastore), "\n",
      "  csv files:  ", length(x$csv), "\n", "  apsimx:     ", length(x$apsimx), "\n", sep="")
  invisible(x)
}
