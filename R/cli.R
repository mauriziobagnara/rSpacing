#' Build the Spacing.Tool2 command-line arguments
#'
#' Translates a request or a scenario into the arguments read by
#' `Spacing.Tool2`. Only Tool2 arguments are returned: when Tool2 is started
#' with `dotnet run`, [run_spacing()] adds the `dotnet run --project ... --`
#' prefix itself.
#'
#' For a [new_spacing_request()] object, location, rotation string, weather
#' file and calibration options become individual arguments (`--lat`, `--lon`,
#' `--rotation`, `--met-file`, `--calibrate`, `--observations`,
#' `--yield-weight`, `--lai-weight`, `--simplexes`). For a
#' [new_spacing_scenario()] object, everything is in the request file and only
#' `--request` (plus `--mock`) is passed.
#'
#' @param config A `spacing_config` object from [spacing_config()]. It is
#'   validated without checking for `dotnet`.
#' @param request A `spacing_request` from [new_spacing_request()], or a
#'   `spacing_scenario` from [new_spacing_scenario()] (rSpacing >= 0.10.07,
#'   Spacing.Tool2 >= 0.09.07).
#' @param run_spec Optional path to a run-specification JSON file
#'   ([write_spacing_run_spec()]). It is read and validated, then passed as
#'   `--run-spec`. Tool2 uses only its `Soil` section, which replaces the soil
#'   of the request or scenario.
#' @param request_file Path of the Tool2 request JSON written for a
#'   `spacing_scenario` (see [write_spacing_scenario()]). Required for
#'   scenarios, ignored for requests.
#' @return A character vector of arguments beginning with `"--config"`, e.g.
#'   `c("--config", "<path>", "--request", "<path>", "--mock")`. Extra
#'   arguments stored in `request$extra_args` are appended.
#' @seealso [run_spacing()], which builds and executes the command.
#' @family running Tool2
#' @export
#' @examples
#' # A throw-away repository layout with the configuration file Tool2 expects
#' root <- tempfile("spacing")
#' dir.create(file.path(root, "apsim", "src", "Spacing.Tool2"), recursive = TRUE)
#' jsonlite::write_json(list(OutputDir = file.path(root, "out"),
#'                           TempWeatherDir = file.path(root, "weather")),
#'                      file.path(root, "apsim", "spacing.config.json"), auto_unbox = TRUE)
#' cfg <- spacing_config(repository = root)
#' build_spacing_args(cfg, new_spacing_request(45.4, 9.2, "Maize", mock = TRUE))
build_spacing_args <- function(config, request, run_spec = NULL, request_file = NULL) {
  validate_spacing_config(config, check_dotnet = FALSE)
  if (inherits(request, "spacing_scenario")) {
    # Complete scenario: location, period, weather, soil, rotation and management
    # are all read by Tool2 from the request file.
    if (is.null(request_file))
      .rspacing_abort("A spacing_scenario requires 'request_file' (see write_spacing_scenario()).",
                      "spacing_validation_error")
    request_file <- .normalise_existing(request_file, "request_file", "file")
    tool_args <- c("--config", config$config_file, "--request", request_file)
    if (request$mock) tool_args <- c(tool_args, "--mock")
  } else {
  validate_spacing_request(request)
  tool_args <- c("--config", config$config_file,
                 "--lat", format(request$latitude, scientific=FALSE, trim=TRUE, digits=15),
                 "--lon", format(request$longitude, scientific=FALSE, trim=TRUE, digits=15),
                 "--rotation", request$rotation)
  if (request$mock) tool_args <- c(tool_args, "--mock")
  if (!is.null(request$met_file)) tool_args <- c(tool_args, "--met-file", request$met_file)
  if (request$calibrate) tool_args <- c(tool_args, "--calibrate")
  if (!is.null(request$observations)) tool_args <- c(tool_args, "--observations", request$observations)
  if (!is.null(request$yield_weight)) tool_args <- c(tool_args, "--yield-weight", as.character(request$yield_weight))
  if (!is.null(request$lai_weight)) tool_args <- c(tool_args, "--lai-weight", as.character(request$lai_weight))
  if (!is.null(request$simplexes)) tool_args <- c(tool_args, "--simplexes", as.character(request$simplexes))
  }
  if (!is.null(run_spec)) {
    if (!is.character(run_spec) || length(run_spec) != 1L ||
        is.na(run_spec) || !nzchar(run_spec)) {
      .rspacing_abort(
        "'run_spec' must be one non-empty path.",
        "spacing_validation_error"
      )
    }
    if (!file.exists(run_spec)) {
      .rspacing_abort(
        sprintf("run_spec not found: %s", run_spec),
        "spacing_validation_error",
        path = run_spec
      )
    }
    run_spec <- normalizePath(
      run_spec,
      winslash = "/",
      mustWork = TRUE
    )
    read_spacing_run_spec(run_spec, validate = TRUE)
    tool_args <- c(tool_args, "--run-spec", run_spec)
  }
  c(tool_args, request$extra_args)
}

#' Build the full process command for Spacing.Tool2
#'
#' Combines [build_spacing_args()] with the launcher: `dotnet run --project
#' <project> -- <args>` for source projects, `dotnet <dll> <args>` for a
#' compiled framework-dependent DLL, or `<executable> <args>` for a native
#' executable.
#'
#' @param config A `spacing_config`.
#' @param request A `spacing_request` or `spacing_scenario`.
#' @param request_file Request JSON path (scenarios only).
#' @return A list with `command` (character scalar) and `args` (character
#'   vector), ready for [processx::run()].
#' @noRd
.build_process_command <- function(config, request, request_file = NULL) {
  args <- build_spacing_args(config, request, request_file = request_file)
  if (config$use_compiled) {
    # 0.10.07: a framework-dependent build (spacing-tool2.dll) is run with `dotnet <dll>`,
    # which needs only the .NET runtime (no SDK, no NuGet restore).
    if (grepl("[.]dll$", config$executable, ignore.case = TRUE))
      return(list(command=config$dotnet, args=c(config$executable, args)))
    return(list(command=config$executable, args=args))
  }
  list(command=config$dotnet, args=c("run", "--project", config$project, "--", args))
}
