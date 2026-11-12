#' Build Tool2 command-line arguments
#'
#' Produces only arguments consumed by `Spacing.Tool2`. For a `dotnet run`
#' invocation, the internal process adapter inserts the required separator
#' before these arguments.
#'
#' @param config A `spacing_config` object.
#' @param request A `spacing_request` object.
#' @return A character vector beginning with `--config`.
#' @export
#' @examples
#' \dontrun{
#' build_spacing_args(cfg, new_spacing_request(45.4, 9.2, "Maize", mock=TRUE))
#' }
build_spacing_args <- function(config, request) {
  validate_spacing_config(config, check_dotnet = FALSE)
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
  c(tool_args, request$extra_args)
}

.build_process_command <- function(config, request) {
  args <- build_spacing_args(config, request)
  if (config$use_compiled) return(list(command=config$executable, args=args))
  list(command=config$dotnet, args=c("run", "--project", config$project, "--", args))
}
