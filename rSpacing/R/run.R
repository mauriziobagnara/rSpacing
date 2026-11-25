#' Run Spacing.Tool2 for a request or a complete scenario
#'
#' Starts the C# application, captures its standard output and standard error,
#' writes a JSON manifest and lists the APSIM files created or changed in the
#' Tool2 output and weather directories.
#'
#' Each call creates a run directory `<run_root>/<run_id>` with `stdout.log`,
#' `stderr.log` and `manifest.json`. For a `spacing_scenario`, the scenario is
#' written there as `request.json` and passed with `--request`; Tool2 writes
#' `result.json` and `tool2_manifest.json` in the same directory, and the
#' parsed `result.json` is returned in `$result`.
#'
#' **Errors.** With `error_on_status = TRUE` a `spacing_process_error` is
#' raised when the process status is not 0. Tool2 (>= 0.09.08) exit codes are
#' interpreted: 2 = the APSIM simulation failed, 3 = invalid request or
#' unexpected error; the first `ERRORE:` line of Tool2 is included in the
#' message. For scenarios, `result.json` must also report `success = TRUE`.
#' The condition carries the complete run object in `$run`.
#'
#' **Warnings.** For scenarios, a warning lists crops that ended without a
#' normal harvest (`ForcedEnd = 1`: `EndReason` `failed`, `stalled` or
#' `max_days`). Cover-crop terminations and the end of a perennial are not
#' forced ends.
#'
#' @param config A `spacing_config` from [spacing_config()]. It is validated,
#'   including the `dotnet` check.
#' @param request A `spacing_request` from [new_spacing_request()], or a
#'   complete `spacing_scenario` from [new_spacing_scenario()].
#' @param error_on_status Logical. Raise a `spacing_process_error` on a
#'   non-zero or missing process status (and, for scenarios, on
#'   `success = FALSE` in `result.json`).
#' @param echo Logical. Stream the Tool2 output to the R console while it runs.
#' @return An object of class `spacing_run`: the manifest fields (`run_id`,
#'   `started`, `ended`, `duration_seconds`, `command`, `arguments`, `status`,
#'   `timeout`, `repository`, `config_file`, `config_sha256`, `request`,
#'   `outputs`) plus `stdout`, `stderr`, `run_dir` and `result` (the parsed
#'   Tool2 `result.json` for scenarios, otherwise `NULL`).
#' @seealso [read_spacing()] to import the outputs, [build_spacing_args()],
#'   [new_spacing_scenario()].
#' @family running Tool2
#' @export
#' @examples
#' \dontrun{
#' cfg <- spacing_config(apsim_dir = "apsim", use_compiled = TRUE,
#'   executable = "apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll")
#' run <- run_spacing(cfg, new_spacing_request(45.4, 9.2, "Maize", mock = TRUE))
#' run$status
#' result <- read_spacing(run)
#' }
run_spacing <- function(config, request, error_on_status = TRUE, echo = TRUE) {
  validate_spacing_config(config, check_dotnet = TRUE)
  is_scenario <- inherits(request, "spacing_scenario")
  if (!is_scenario) validate_spacing_request(request)
  run_id <- paste(format(Sys.time(), "%Y%m%dT%H%M%S"),
                  digest::digest(list(request, stats::runif(1)), algo="xxhash64"), sep="-")
  run_dir <- as.character(fs::path(config$run_root, run_id))
  fs::dir_create(run_dir, recurse=TRUE)
  request_file <- NULL
  if (is_scenario) {
    request_file <- write_spacing_scenario(
      request, fs::path(run_dir, "request.json"), run_id = run_id,
      result_file = normalizePath(fs::path(run_dir, "result.json"), winslash = "/", mustWork = FALSE),
      manifest_file = normalizePath(fs::path(run_dir, "tool2_manifest.json"), winslash = "/", mustWork = FALSE))
  }
  before <- .snapshot_files(config)
  process <- .build_process_command(config, request, request_file = request_file)
  started <- Sys.time()
  result <- tryCatch(
    processx::run(process$command, process$args, wd=config$repository,
                  timeout=config$timeout, echo=echo, error_on_status=FALSE,
                  windows_verbatim_args=FALSE),
    error=function(e) list(status=NA_integer_, stdout="", stderr=conditionMessage(e), timeout=FALSE)
  )
  ended <- Sys.time()
  writeLines(result$stdout %||% "", fs::path(run_dir, "stdout.log"), useBytes=TRUE)
  writeLines(result$stderr %||% "", fs::path(run_dir, "stderr.log"), useBytes=TRUE)
  outputs <- discover_spacing_outputs(config, before=before)
  manifest <- list(schema_version=1L, package="rSpacing", run_id=run_id,
                   started=.format_time(started), ended=.format_time(ended),
                   duration_seconds=as.numeric(difftime(ended,started,units="secs")),
                   command=process$command, arguments=process$args, status=result$status,
                   timeout=isTRUE(result$timeout), repository=config$repository,
                   config_file=config$config_file, config_sha256=config$config_sha256,
                   request=if (is_scenario) list(scenario=request$id, request_file=request_file) else unclass(request),
                   outputs=outputs)
  jsonlite::write_json(manifest, fs::path(run_dir, "manifest.json"), pretty=TRUE, auto_unbox=TRUE, null="null")
  tool2_result <- NULL
  result_json <- fs::path(run_dir, "result.json")
  if (is_scenario && fs::file_exists(result_json))
    tool2_result <- tryCatch(jsonlite::read_json(result_json, simplifyVector = TRUE), error = function(e) NULL)
  run <- structure(c(manifest, list(stdout=result$stdout %||% "", stderr=result$stderr %||% "",
                                    run_dir=run_dir, result=tool2_result)), class="spacing_run")
  if (error_on_status && (is.na(run$status) || run$status != 0L)) {
    # Tool2 >= 0.09.08 exit codes: 2 = APSIM simulation failed, 3 = invalid request or
    # unexpected error. The first "ERRORE:" line of stderr carries the reason.
    detail <- grep("^ERRORE", strsplit(run$stderr, "\r?\n")[[1]], value = TRUE)
    kind <- switch(as.character(run$status), "2" = "APSIM simulation failed",
                   "3" = "invalid request or unexpected error", "Spacing.Tool2 failed")
    .rspacing_abort(sprintf("%s (status %s)%s. See %s", kind, run$status,
                            if (length(detail)) paste0(": ", sub("^ERRORE:\\s*", "", detail[[1]])) else "",
                            run_dir),
                    "spacing_process_error", run=run)
  }
  if (is_scenario) {
    # Spacing.Tool2 0.09.07 exits with status 0 even when the APSIM simulation fails
    # (0.09.08 exits with 2); result.json is checked in both cases.
    if (error_on_status && !isTRUE(tool2_result$success))
      .rspacing_abort(sprintf("APSIM simulation failed: %s. See %s",
                              trimws(tool2_result$error %||% "no result.json written"), run_dir),
                      "spacing_process_error", run=run)
    .warn_forced_crop_ends(tool2_result)
  }
  run
}

#' Format a time stamp for manifests
#'
#' @param x A `POSIXct` time.
#' @return Character scalar in ISO 8601 form with milliseconds and UTC offset.
#' @noRd
.format_time <- function(x) format(x, "%Y-%m-%dT%H:%M:%OS3%z")

#' Modification times of the files in the Tool2 output directories
#'
#' Used before a run, so that [discover_spacing_outputs()] can list only new or
#' changed files.
#'
#' @param config A `spacing_config`.
#' @return Named numeric vector: modification times (seconds since the epoch)
#'   named by absolute file path.
#' @noRd
.snapshot_files <- function(config) {
  dirs <- unique(c(config$output_dir, config$temp_weather_dir))
  files <- unlist(lapply(dirs, function(d) if (fs::dir_exists(d)) fs::dir_ls(d, recurse=TRUE, type="file") else character()))
  stats <- if (length(files)) fs::file_info(files) else data.frame()
  setNames(as.numeric(stats$modification_time), as.character(files))
}

#' @describeIn run_spacing Print the run identifier, status, duration and run
#'   directory.
#' @param x A `spacing_run` object.
#' @param ... Ignored.
#' @export
print.spacing_run <- function(x, ...) {
  cat("<spacing_run>\n", "  run_id: ", x$run_id, "\n", "  status: ", x$status,
      "\n", "  duration: ", round(x$duration_seconds, 3), " s\n", "  run_dir: ", x$run_dir, "\n", sep="")
  invisible(x)
}

#' Warn about crops that ended without a normal harvest
#'
#' Reads the harvest report named in `result.json` and warns when rows have
#' `ForcedEnd = 1`. Tool2 >= 0.09.08 gives the reason in `EndReason`
#' (`failed`, `stalled`, `max_days`); Tool2 0.09.07 has only `ForcedEnd`.
#'
#' @param tool2_result The parsed Tool2 `result.json` (a list).
#' @return The forced-end rows (data frame), invisibly, or `NULL`.
#' @noRd
.warn_forced_crop_ends <- function(tool2_result) {
  path <- tool2_result$tradeoffs$control_csv_path
  if (is.null(path) || !file.exists(path)) return(invisible(NULL))
  report <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(report) || !all(c("ForcedEnd", "HarvestedCrop") %in% names(report))) return(invisible(NULL))
  forced <- report[report$ForcedEnd %in% 1, , drop = FALSE]
  if (nrow(forced)) {
    reason <- if ("EndReason" %in% names(forced)) forced$EndReason else "max_days"
    warning(sprintf("%d crop(s) did not end with a normal harvest: %s",
                    nrow(forced), paste0(forced$HarvestedCrop, " ", forced$Clock.Today, " (", reason, ")", collapse = ", ")),
            call. = FALSE)
  }
  invisible(forced)
}
