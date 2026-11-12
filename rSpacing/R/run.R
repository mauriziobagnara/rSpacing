#' Execute one Spacing.Tool2 request
#'
#' Runs the custom C# application, captures standard output and errors, records
#' a JSON manifest, and discovers new or modified APSIM artefacts.
#'
#' @param config A validated `spacing_config`.
#' @param request A validated `spacing_request`.
#' @param error_on_status Logical. Raise a `spacing_process_error` on a non-zero
#'   or missing process status. The condition contains the complete run object.
#' @param echo Logical. Stream process output to the R console.
#' @return An S3 object of class `spacing_run`.
#' @export
#' @examples
#' \dontrun{
#' run <- run_spacing(cfg, new_spacing_request(45.4, 9.2, "Maize", mock=TRUE))
#' }
run_spacing <- function(config, request, error_on_status = TRUE, echo = TRUE) {
  validate_spacing_config(config, check_dotnet = TRUE)
  validate_spacing_request(request)
  run_id <- paste(format(Sys.time(), "%Y%m%dT%H%M%S"),
                  digest::digest(list(request, runif(1)), algo="xxhash64"), sep="-")
  run_dir <- as.character(fs::path(config$run_root, run_id))
  fs::dir_create(run_dir, recurse=TRUE)
  before <- .snapshot_files(config)
  process <- .build_process_command(config, request)
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
                   request=unclass(request), outputs=outputs)
  jsonlite::write_json(manifest, fs::path(run_dir, "manifest.json"), pretty=TRUE, auto_unbox=TRUE, null="null")
  run <- structure(c(manifest, list(stdout=result$stdout %||% "", stderr=result$stderr %||% "",
                                    run_dir=run_dir)), class="spacing_run")
  if (error_on_status && (is.na(run$status) || run$status != 0L))
    .rspacing_abort(sprintf("Spacing.Tool2 failed with status %s. See %s", run$status, run_dir),
                    "spacing_process_error", run=run)
  run
}

.format_time <- function(x) format(x, "%Y-%m-%dT%H:%M:%OS3%z")

.snapshot_files <- function(config) {
  dirs <- unique(c(config$output_dir, config$temp_weather_dir))
  files <- unlist(lapply(dirs, function(d) if (fs::dir_exists(d)) fs::dir_ls(d, recurse=TRUE, type="file") else character()))
  stats <- if (length(files)) fs::file_info(files) else data.frame()
  setNames(as.numeric(stats$modification_time), as.character(files))
}

#' @export
print.spacing_run <- function(x, ...) {
  cat("<spacing_run>\n", "  run_id: ", x$run_id, "\n", "  status: ", x$status,
      "\n", "  duration: ", round(x$duration_seconds, 3), " s\n", "  run_dir: ", x$run_dir, "\n", sep="")
  invisible(x)
}
