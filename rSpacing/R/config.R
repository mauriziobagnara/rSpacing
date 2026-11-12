#' Create a Spacing.Tool2 project configuration
#'
#' Reads `spacing.config.json`, resolves all important paths, creates output
#' directories when necessary, and records a SHA-256 hash of the configuration.
#' This function never silently substitutes another JSON file.
#'
#' @param repository Character scalar or `NULL`. Root directory of the `spacing`
#'   repository. May be omitted when `apsim_dir` is supplied.
#' @param apsim_dir Character scalar or `NULL`. Direct path to the `apsim`
#'   directory containing `spacing.config.json` and `src/Spacing.Tool2`.
#' @param project Character scalar. Path to the `Spacing.Tool2` project,
#'   absolute or relative to `repository`.
#' @param config_file Character scalar. Path to `spacing.config.json`, absolute
#'   or relative to `repository`.
#' @param dotnet Character scalar. Name or path of the `dotnet` executable.
#' @param timeout Numeric scalar. Maximum execution time passed to
#'   [processx::run()]. Use `Inf` for no timeout.
#' @param run_root Character scalar or `NULL`. Directory in which rSpacing
#'   stores per-run logs and manifests. The default is
#'   `<OutputDir>/rSpacing-runs`.
#' @param use_compiled Logical. If `TRUE`, call an already compiled executable
#'   rather than `dotnet run`.
#' @param executable Character scalar or `NULL`. Compiled Tool2 executable;
#'   required when `use_compiled = TRUE`.
#'
#' @return An S3 object of class `spacing_config`.
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- spacing_config(
#'   repository = "C:/Users/user/Documents/spacing",
#'   project = "apsim/src/Spacing.Tool2",
#'   config_file = "apsim/spacing.config.json"
#' )
#' print(cfg)
#' }
spacing_config <- function(repository = NULL, apsim_dir = NULL,
                           project = NULL, config_file = NULL,
                           dotnet = "dotnet", timeout = Inf, run_root = NULL,
                           use_compiled = FALSE, executable = NULL) {
  if (is.null(repository) && is.null(apsim_dir)) {
    .rspacing_abort("Supply either repository or apsim_dir.", "spacing_validation_error")
  }

  if (!is.null(apsim_dir)) {
    apsim_dir <- .normalise_existing(apsim_dir, "apsim_dir", "dir")
    if (is.null(repository)) repository <- fs::path_dir(apsim_dir)
  }
  repository <- .normalise_existing(repository, "repository", "dir")
  if (is.null(apsim_dir)) apsim_dir <- fs::path(repository, "apsim")
  apsim_dir <- .normalise_existing(apsim_dir, "apsim_dir", "dir")

  if (is.null(project)) project <- fs::path(apsim_dir, "src", "Spacing.Tool2")
  if (is.null(config_file)) config_file <- fs::path(apsim_dir, "spacing.config.json")
  project <- if (fs::is_absolute_path(project)) project else fs::path(repository, project)
  config_file <- if (fs::is_absolute_path(config_file)) config_file else fs::path(repository, config_file)
  project <- .normalise_existing(project, "Spacing.Tool2 project", "dir")
  config_file <- .normalise_existing(config_file, "configuration file", "file")
  raw <- .read_json(config_file)
  output_dir <- raw$OutputDir %||% .rspacing_abort("JSON property 'OutputDir' is required.", "spacing_config_error")
  weather_dir <- raw$TempWeatherDir %||% .rspacing_abort("JSON property 'TempWeatherDir' is required.", "spacing_config_error")
  output_dir <- as.character(fs::path_abs(output_dir, start = fs::path_dir(config_file)))
  weather_dir <- as.character(fs::path_abs(weather_dir, start = fs::path_dir(config_file)))
  fs::dir_create(output_dir, recurse = TRUE)
  fs::dir_create(weather_dir, recurse = TRUE)
  if (is.null(run_root)) run_root <- fs::path(output_dir, "rSpacing-runs")
  run_root <- as.character(fs::path_abs(run_root))
  fs::dir_create(run_root, recurse = TRUE)
  if (use_compiled) executable <- .normalise_existing(executable, "compiled executable", "file")
  structure(list(repository=repository, apsim_dir=apsim_dir, project=project, config_file=config_file,
                 config=raw, output_dir=output_dir, temp_weather_dir=weather_dir,
                 dotnet=dotnet, timeout=timeout, run_root=run_root,
                 use_compiled=isTRUE(use_compiled), executable=executable,
                 config_sha256=.hash_file(config_file)), class="spacing_config")
}

#' Validate a Spacing.Tool2 configuration
#'
#' @param x A `spacing_config` object.
#' @param check_dotnet Logical. Check whether `dotnet` is available when a
#'   compiled executable is not being used.
#' @return `x`, invisibly. Errors have class `spacing_validation_error` or
#'   `spacing_runtime_error`.
#' @export
validate_spacing_config <- function(x, check_dotnet = TRUE) {
  if (!inherits(x, "spacing_config")) .rspacing_abort("x is not a spacing_config.", "spacing_validation_error")
  .normalise_existing(x$project, "Spacing.Tool2 project", "dir")
  .normalise_existing(x$config_file, "configuration file", "file")
  if (!fs::dir_exists(x$output_dir) || !fs::is_dir(x$output_dir))
    .rspacing_abort("OutputDir is unavailable.", "spacing_validation_error")
  if (check_dotnet && !x$use_compiled && !nzchar(Sys.which(x$dotnet)))
    .rspacing_abort(sprintf("dotnet executable not found: %s", x$dotnet), "spacing_runtime_error")
  invisible(x)
}

#' @export
print.spacing_config <- function(x, ...) {
  cat("<spacing_config>\n", "  project: ", x$project, "\n", "  config:  ", x$config_file,
      "\n", "  output:  ", x$output_dir, "\n", "  weather: ", x$temp_weather_dir, "\n", sep="")
  invisible(x)
}
