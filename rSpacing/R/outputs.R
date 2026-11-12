#' Discover generated Spacing.Tool2 artefacts
#'
#' @param config A `spacing_config` object.
#' @param before Optional named numeric vector of modification times generated
#'   by the internal pre-run snapshot. When supplied, only new or modified
#'   files are returned.
#' @return A named list containing `apsimx`, `database`, `csv`, `weather`, and
#'   `other` character vectors.
#' @export
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
#' @param path Existing `.db` file.
#' @param tables Character vector of table names or `NULL` for all tables.
#' @return A named list of data frames.
#' @export
read_spacing_datastore <- function(path, tables = NULL) {
  path <- .normalise_existing(path, "DataStore", "file")
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con), add=TRUE)
  available <- DBI::dbListTables(con); selected <- tables %||% available
  missing <- setdiff(selected, available)
  if (length(missing)) .rspacing_abort(paste("Missing DataStore tables:", paste(missing, collapse=", ")), "spacing_schema_error")
  setNames(lapply(selected, function(tab) DBI::dbReadTable(con, tab)), selected)
}

#' Read a Spacing.Tool2 CSV output
#' @param path Existing CSV file.
#' @param ... Additional arguments passed to [utils::read.csv()].
#' @return A data frame preserving original column names.
#' @export
read_spacing_csv <- function(path, ...) {
  path <- .normalise_existing(path, "CSV output", "file")
  utils::read.csv(path, stringsAsFactors=FALSE, check.names=FALSE, ...)
}

#' Read all outputs associated with a run
#' @param run A `spacing_run` object.
#' @param datastore_tables Optional table selection passed to
#'   [read_spacing_datastore()].
#' @return An S3 object of class `spacing_result`.
#' @export
read_spacing <- function(run, datastore_tables = NULL) {
  if (!inherits(run, "spacing_run")) .rspacing_abort("run is not a spacing_run.", "spacing_validation_error")
  db <- setNames(lapply(run$outputs$database, read_spacing_datastore, tables=datastore_tables), basename(run$outputs$database))
  csv <- setNames(lapply(run$outputs$csv, read_spacing_csv), basename(run$outputs$csv))
  structure(list(run=run, datastore=db, csv=csv, apsimx=run$outputs$apsimx,
                 weather=run$outputs$weather, messages=.extract_messages(db),
                 units=.extract_units(db)), class="spacing_result")
}
.extract_messages <- function(db_list) Filter(Negate(is.null), lapply(db_list, function(x) x[["_Messages"]] %||% NULL))
.extract_units <- function(db_list) Filter(Negate(is.null), lapply(db_list, function(x) x[["_Units"]] %||% NULL))

#' Combine CSV outputs from several results
#' @param results A `spacing_result` or a list of them.
#' @return A named list of row-bound data frames. `.run_id` and `.scenario_id`
#'   provenance columns are added. Zero-row tables are retained with zero-length
#'   character provenance columns. Columns missing from individual runs are filled
#'   with `NA` before row-binding.
#' @export
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

#' @export
print.spacing_result <- function(x, ...) {
  cat("<spacing_result>\n", "  databases: ", length(x$datastore), "\n",
      "  csv files:  ", length(x$csv), "\n", "  apsimx:     ", length(x$apsimx), "\n", sep="")
  invisible(x)
}
