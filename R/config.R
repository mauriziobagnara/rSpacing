#' Create a Spacing.Tool2 project configuration
#'
#' Locates the `Spacing.Tool2` project and its `spacing.config.json`, resolves
#' all paths, creates the output, weather and run directories when they are
#' missing, and records a SHA-256 hash of the configuration file for
#' provenance. The function never substitutes another JSON file silently: a
#' missing file or a missing `OutputDir`/`TempWeatherDir` property is an error.
#'
#' Relative paths inside `spacing.config.json` (`OutputDir`,
#' `TempWeatherDir`) are resolved against the directory of the JSON file.
#'
#' @param repository Character scalar or `NULL`. Root of the `spacing`
#'   repository (the directory that contains `apsim/`). May be omitted when
#'   `apsim_dir` is given; it then defaults to the parent of `apsim_dir`.
#' @param apsim_dir Character scalar or `NULL`. The `apsim` directory that
#'   contains `spacing.config.json` and `src/Spacing.Tool2`. Defaults to
#'   `<repository>/apsim`.
#' @param project Character scalar or `NULL`. Path of the `Spacing.Tool2`
#'   project directory, absolute or relative to `repository`. Default
#'   `<apsim_dir>/src/Spacing.Tool2`.
#' @param config_file Character scalar or `NULL`. Path of
#'   `spacing.config.json`, absolute or relative to `repository`. Default
#'   `<apsim_dir>/spacing.config.json`.
#' @param dotnet Character scalar. Name or path of the `dotnet` executable.
#' @param timeout Numeric scalar. Maximum run time in seconds passed to
#'   [processx::run()]; `Inf` (default) means no limit.
#' @param run_root Character scalar or `NULL`. Directory where rSpacing writes
#'   one folder per run (logs, request, manifest). Default
#'   `<OutputDir>/rSpacing-runs`.
#' @param use_compiled Logical. `TRUE` runs an already compiled Tool2 given in
#'   `executable` instead of `dotnet run` (which needs the .NET SDK and a
#'   NuGet restore).
#' @param executable Character scalar or `NULL`. Compiled Tool2, required when
#'   `use_compiled = TRUE`: either a native executable or the
#'   framework-dependent `spacing-tool2.dll` (e.g.
#'   `src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll`), which is run as
#'   `dotnet <dll>` and needs only the .NET runtime.
#' @return An object of class `spacing_config`: a list with `repository`,
#'   `apsim_dir`, `project`, `config_file`, `config` (the parsed JSON),
#'   `output_dir`, `temp_weather_dir`, `dotnet`, `timeout`, `run_root`,
#'   `use_compiled`, `executable` and `config_sha256`.
#' @seealso [validate_spacing_config()], [run_spacing()],
#'   [prepare_spacing_runtime()] to create the configuration file.
#' @family configuration
#' @export
#' @examples
#' root <- tempfile("spacing")
#' dir.create(file.path(root, "apsim", "src", "Spacing.Tool2"), recursive = TRUE)
#' jsonlite::write_json(list(OutputDir = "output", TempWeatherDir = "weather"),
#'                      file.path(root, "apsim", "spacing.config.json"), auto_unbox = TRUE)
#' cfg <- spacing_config(repository = root)
#' cfg
#' cfg$output_dir
#' \dontrun{
#' # A compiled DLL, run with `dotnet <dll>`
#' cfg <- spacing_config(apsim_dir = "apsim", use_compiled = TRUE,
#'   executable = "apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll")
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
#' Checks that the project directory, the configuration file and the output
#' directory still exist and, optionally, that the `dotnet` command is
#' available (needed for `dotnet run` and for compiled `.dll` builds).
#'
#' @param x A `spacing_config` object from [spacing_config()].
#' @param check_dotnet Logical. If `TRUE`, verify with [Sys.which()] that
#'   `x$dotnet` can be found when it is needed.
#' @return `x`, invisibly. Problems raise errors of class
#'   `spacing_validation_error` (paths) or `spacing_runtime_error` (`dotnet`
#'   missing).
#' @family configuration
#' @export
#' @examples
#' root <- tempfile("spacing")
#' dir.create(file.path(root, "apsim", "src", "Spacing.Tool2"), recursive = TRUE)
#' jsonlite::write_json(list(OutputDir = "output", TempWeatherDir = "weather"),
#'                      file.path(root, "apsim", "spacing.config.json"), auto_unbox = TRUE)
#' cfg <- spacing_config(repository = root)
#' validate_spacing_config(cfg, check_dotnet = FALSE)
validate_spacing_config <- function(x, check_dotnet = TRUE) {
  if (!inherits(x, "spacing_config")) .rspacing_abort("x is not a spacing_config.", "spacing_validation_error")
  .normalise_existing(x$project, "Spacing.Tool2 project", "dir")
  .normalise_existing(x$config_file, "configuration file", "file")
  if (!fs::dir_exists(x$output_dir) || !fs::is_dir(x$output_dir))
    .rspacing_abort("OutputDir is unavailable.", "spacing_validation_error")
  needs_dotnet <- !x$use_compiled || grepl("[.]dll$", x$executable %||% "", ignore.case = TRUE)
  if (check_dotnet && needs_dotnet && !nzchar(Sys.which(x$dotnet)))
    .rspacing_abort(sprintf("dotnet executable not found: %s", x$dotnet), "spacing_runtime_error")
  invisible(x)
}

#' @describeIn spacing_config Print the project, configuration, output and
#'   weather paths.
#' @param x A `spacing_config` object.
#' @param ... Ignored.
#' @export
print.spacing_config <- function(x, ...) {
  cat("<spacing_config>\n", "  project: ", x$project, "\n", "  config:  ", x$config_file,
      "\n", "  output:  ", x$output_dir, "\n", "  weather: ", x$temp_weather_dir, "\n", sep="")
  invisible(x)
}
