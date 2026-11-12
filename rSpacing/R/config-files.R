#' Create values for a Spacing.Tool2 JSON configuration
#'
#' Constructs an in-memory representation of `spacing.config.json`. The object
#' contains the properties currently used by `SpacingConfig.cs`, while
#' `additional` preserves custom properties required by a modified C# build.
#'
#' @param db_api_base_url Character scalar. Base URL used by the database API.
#' @param db_api_key Character scalar. API credential. The default is empty.
#' @param temp_weather_dir Character scalar. Directory in which generated
#'   APSIM weather files are written.
#' @param output_dir Character scalar. Directory for APSIMX, SQLite and CSV
#'   outputs.
#' @param start_year,end_year Integer simulation-year limits.
#' @param max_parallelism Positive integer number of concurrent workers.
#' @param additional Named list of custom JSON properties. These are appended
#'   after the standard properties and may override them only when
#'   `allow_override = TRUE`.
#' @param allow_override Logical. Permit `additional` to replace standard
#'   properties.
#'
#' @return An S3 object of class `spacing_config_values`.
#' @export
#'
#' @examples
#' values <- new_spacing_config_values(
#'   db_api_base_url = "http://localhost:8000/api/v1",
#'   temp_weather_dir = "C:/spacing/work/weather",
#'   output_dir = "C:/spacing/results",
#'   start_year = 1990,
#'   end_year = 2023,
#'   max_parallelism = 4
#' )
new_spacing_config_values <- function(
    db_api_base_url,
    db_api_key = "",
    temp_weather_dir,
    output_dir,
    start_year,
    end_year,
    max_parallelism = 1L,
    additional = list(),
    allow_override = FALSE) {
  if (!is.list(additional) ||
      (length(additional) > 0L &&
       (is.null(names(additional)) || any(!nzchar(names(additional)))))) {
    .rspacing_abort("additional must be a fully named list.", "spacing_config_error")
  }
  standard <- list(
    DbApiBaseUrl = db_api_base_url,
    DbApiKey = db_api_key,
    TempWeatherDir = temp_weather_dir,
    OutputDir = output_dir,
    StartYear = as.integer(start_year),
    EndYear = as.integer(end_year),
    MaxParallelism = as.integer(max_parallelism)
  )
  duplicated_names <- intersect(names(standard), names(additional))
  if (length(duplicated_names) && !isTRUE(allow_override)) {
    .rspacing_abort(
      paste("additional attempts to override standard properties:",
            paste(duplicated_names, collapse = ", ")),
      "spacing_config_error"
    )
  }
  if (length(additional)) standard[names(additional)] <- additional
  x <- structure(standard, class = c("spacing_config_values", "list"))
  validate_spacing_config_values(x)
  x
}

#' Validate values intended for spacing.config.json
#'
#' @param x A `spacing_config_values` object or compatible named list.
#' @param check_directories Logical. If `TRUE`, require `TempWeatherDir` and
#'   `OutputDir` to exist.
#' @return `x`, invisibly. Errors have class `spacing_config_error`.
#' @export
validate_spacing_config_values <- function(x, check_directories = FALSE) {
  if (!is.list(x)) {
    .rspacing_abort("Configuration values must be a named list.", "spacing_config_error")
  }
  required <- c(
    "DbApiBaseUrl", "DbApiKey", "TempWeatherDir", "OutputDir",
    "StartYear", "EndYear", "MaxParallelism"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    .rspacing_abort(
      paste("Missing configuration properties:", paste(missing, collapse = ", ")),
      "spacing_config_error"
    )
  }
  scalar_character <- c("DbApiBaseUrl", "DbApiKey", "TempWeatherDir", "OutputDir")
  failures <- vapply(scalar_character, function(nm) {
    !is.character(x[[nm]]) || length(x[[nm]]) != 1L || is.na(x[[nm]])
  }, logical(1))
  if (any(failures)) {
    .rspacing_abort(
      paste("Properties must be character scalars:",
            paste(scalar_character[failures], collapse = ", ")),
      "spacing_config_error"
    )
  }
  if (!nzchar(x$TempWeatherDir) || !nzchar(x$OutputDir)) {
    .rspacing_abort("TempWeatherDir and OutputDir cannot be empty.", "spacing_config_error")
  }
  years <- c(x$StartYear, x$EndYear)
  if (length(years) != 2L || any(!is.finite(years)) || any(years != as.integer(years))) {
    .rspacing_abort("StartYear and EndYear must be finite integers.", "spacing_config_error")
  }
  if (x$StartYear > x$EndYear) {
    .rspacing_abort("StartYear cannot be greater than EndYear.", "spacing_config_error")
  }
  if (length(x$MaxParallelism) != 1L || !is.finite(x$MaxParallelism) ||
      x$MaxParallelism < 1 || x$MaxParallelism != as.integer(x$MaxParallelism)) {
    .rspacing_abort("MaxParallelism must be a positive integer.", "spacing_config_error")
  }
  if (check_directories) {
    for (nm in c("TempWeatherDir", "OutputDir")) {
      if (!fs::dir_exists(x[[nm]])) {
        .rspacing_abort(
          sprintf("Directory configured by %s does not exist: %s", nm, x[[nm]]),
          "spacing_config_error"
        )
      }
    }
  }
  invisible(x)
}

#' Read spacing.config.json without creating a runtime configuration
#'
#' Unlike [spacing_config()], this function only reads and validates JSON
#' values. It is useful when preparing or updating files before the repository
#' and runtime are configured.
#'
#' @param path Existing JSON file.
#' @param validate Logical. Validate standard properties.
#' @param normalise_paths Logical. Resolve relative output and weather paths
#'   against the JSON file directory.
#'
#' @return A `spacing_config_values` object.
#' @export
read_spacing_config_file <- function(path, validate = TRUE,
                                     normalise_paths = TRUE) {
  path <- .normalise_existing(path, "configuration file", "file")
  x <- .read_json(path)
  if (!is.list(x)) {
    .rspacing_abort("The JSON root must be an object.", "spacing_config_error")
  }
  if (normalise_paths) {
    for (nm in intersect(c("TempWeatherDir", "OutputDir"), names(x))) {
      x[[nm]] <- as.character(fs::path_abs(x[[nm]], start = fs::path_dir(path)))
    }
  }
  class(x) <- c("spacing_config_values", "list")
  if (validate) validate_spacing_config_values(x)
  attr(x, "source_file") <- path
  x
}

#' Write spacing.config.json
#'
#' Validates values, optionally creates runtime directories, and writes an
#' atomic JSON file. Existing files are not replaced unless `overwrite = TRUE`.
#'
#' @param x A `spacing_config_values` object or compatible named list.
#' @param path Destination JSON file.
#' @param overwrite Logical. Replace an existing destination.
#' @param create_directories Logical. Create `OutputDir` and `TempWeatherDir`.
#' @param pretty Logical. Write human-readable indented JSON.
#' @param relative_to Optional directory. When supplied, output and weather
#'   paths are written relative to this directory where possible.
#'
#' @return The absolute destination path, invisibly.
#' @export
write_spacing_config_file <- function(
    x,
    path,
    overwrite = FALSE,
    create_directories = TRUE,
    pretty = TRUE,
    relative_to = NULL) {
  validate_spacing_config_values(x)
  path <- as.character(fs::path_abs(path))
  if (fs::file_exists(path) && !isTRUE(overwrite)) {
    .rspacing_abort(
      paste("Configuration file already exists; set overwrite=TRUE:", path),
      "spacing_config_error"
    )
  }
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  # Keep the names: removing all attributes also removes names and corrupts JSON.
  values <- as.list(x)
  class(values) <- NULL
  for (nm in c("TempWeatherDir", "OutputDir")) {
    absolute <- as.character(fs::path_abs(values[[nm]], start = fs::path_dir(path)))
    if (create_directories) fs::dir_create(absolute, recurse = TRUE)
    values[[nm]] <- absolute
  }
  if (!is.null(relative_to)) {
    base <- as.character(fs::path_abs(relative_to))
    values$TempWeatherDir <- .path_relative_if_within(values$TempWeatherDir, base)
    values$OutputDir <- .path_relative_if_within(values$OutputDir, base)
  }
  temporary <- tempfile(
    pattern = paste0(fs::path_file(path), "."),
    tmpdir = fs::path_dir(path),
    fileext = ".tmp"
  )
  on.exit(if (fs::file_exists(temporary)) fs::file_delete(temporary), add = TRUE)
  jsonlite::write_json(
    values,
    path = temporary,
    pretty = pretty,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
  if (fs::file_exists(path)) fs::file_delete(path)
  fs::file_move(temporary, path)
  invisible(path)
}

#' Update an existing spacing.config.json
#'
#' Reads all current properties, updates named values, preserves unknown custom
#' properties, validates the result, and writes either in place or to `output`.
#'
#' @param path Existing JSON configuration.
#' @param ... Named properties to update.
#' @param values Optional named list of properties to update.
#' @param output Optional destination. `NULL` updates `path` in place.
#' @param create_directories Logical. Create configured directories.
#' @param backup Logical. When updating in place, copy the original file to a
#'   timestamped `.bak` file before replacement.
#'
#' @return The updated absolute file path, invisibly.
#' @export
update_spacing_config_file <- function(
    path,
    ...,
    values = list(),
    output = NULL,
    create_directories = TRUE,
    backup = TRUE) {
  dots <- list(...)
  changes <- c(values, dots)
  if (!length(changes) || is.null(names(changes)) || any(!nzchar(names(changes)))) {
    .rspacing_abort("Supply at least one named configuration value.", "spacing_config_error")
  }
  path <- .normalise_existing(path, "configuration file", "file")
  current <- read_spacing_config_file(path, validate = FALSE,
                                      normalise_paths = TRUE)
  current[names(changes)] <- changes
  class(current) <- c("spacing_config_values", "list")
  validate_spacing_config_values(current)
  destination <- if (is.null(output)) path else as.character(fs::path_abs(output))
  if (is.null(output) && isTRUE(backup)) {
    backup_path <- paste0(
      path, ".", format(Sys.time(), "%Y%m%dT%H%M%S"), ".bak"
    )
    fs::file_copy(path, backup_path, overwrite = FALSE)
  }
  write_spacing_config_file(
    current,
    destination,
    overwrite = TRUE,
    create_directories = create_directories
  )
  invisible(destination)
}

#' Prepare a set of configuration files for several runs
#'
#' Creates one isolated JSON configuration per row of a scenario table. Each
#' configuration receives a dedicated `OutputDir` and `TempWeatherDir`, which
#' prevents parallel runs from overwriting one another.
#'
#' @param base A `spacing_config_values` object, or path to an existing JSON.
#' @param scenarios Data frame containing a unique identifier column and
#'   optional columns whose names match JSON properties.
#' @param directory Destination root.
#' @param id_column Name of the unique scenario identifier column.
#' @param overwrite Logical. Replace existing scenario JSON files.
#' @param create_directories Logical. Create scenario directories.
#'
#' @return A data frame mapping scenario identifiers to configuration, output,
#'   and weather paths.
#' @export
prepare_spacing_config_set <- function(
    base,
    scenarios,
    directory,
    id_column = "scenario_id",
    overwrite = FALSE,
    create_directories = TRUE) {
  if (is.character(base) && length(base) == 1L) {
    base <- read_spacing_config_file(base)
  }
  validate_spacing_config_values(base)
  if (!is.data.frame(scenarios) || !id_column %in% names(scenarios)) {
    .rspacing_abort(
      sprintf("scenarios must be a data frame with column '%s'.", id_column),
      "spacing_config_error"
    )
  }
  ids <- as.character(scenarios[[id_column]])
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    .rspacing_abort("Scenario identifiers must be non-empty and unique.",
                    "spacing_config_error")
  }
  root <- as.character(fs::path_abs(directory))
  if (create_directories) fs::dir_create(root, recurse = TRUE)
  output <- vector("list", length(ids))
  # Convert the validated S3 configuration to a plain named list without
  # relying on unclass().  This preserves every required property when the
  # object carries multiple classes (spacing_config_values, list).
  base_values <- as.list(base)
  names(base_values) <- names(base)
  standard_names <- names(base_values)
  override_names <- intersect(setdiff(names(scenarios), id_column),
                              standard_names)
  for (i in seq_along(ids)) {
    safe_id <- .safe_file_id(ids[[i]])
    scenario_root <- fs::path(root, safe_id)
    values <- base_values
    for (nm in override_names) {
      value <- scenarios[[nm]][[i]]
      if (!is.na(value)) values[[nm]] <- value
    }
    values$OutputDir <- as.character(fs::path(scenario_root, "output"))
    values$TempWeatherDir <- as.character(fs::path(scenario_root, "weather"))
    class(values) <- c("spacing_config_values", "list")
    config_path <- as.character(fs::path(scenario_root, "spacing.config.json"))
    write_spacing_config_file(
      values,
      config_path,
      overwrite = overwrite,
      create_directories = create_directories
    )
    output[[i]] <- data.frame(
      scenario_id = ids[[i]],
      config_file = config_path,
      output_dir = values$OutputDir,
      temp_weather_dir = values$TempWeatherDir,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, output)
}

#' Prepare configuration and runtime objects for an executable run
#'
#' Writes `spacing.config.json` when values are supplied, then returns both a
#' validated [spacing_config()] object and the configuration path. This is the
#' highest-level setup function for a single executable workflow.
#'
#' @param repository Root of the C# repository.
#' @param config Either configuration values or a path to an existing JSON.
#' @param config_path Destination used when `config` is an in-memory object.
#' @param project,dotnet,timeout,use_compiled,executable Passed to
#'   [spacing_config()].
#' @param overwrite Logical. Replace `config_path` when writing values.
#'
#' @return A list containing `config_file`, `runtime`, and `values`.
#' @export
prepare_spacing_runtime <- function(
    repository,
    config,
    config_path = fs::path(repository, "apsim", "spacing.config.json"),
    project = "apsim/src/Spacing.Tool2",
    dotnet = "dotnet",
    timeout = Inf,
    use_compiled = FALSE,
    executable = NULL,
    overwrite = FALSE) {
  if (is.character(config) && length(config) == 1L) {
    config_file <- .normalise_existing(config, "configuration file", "file")
    values <- read_spacing_config_file(config_file)
  } else {
    validate_spacing_config_values(config)
    config_file <- write_spacing_config_file(
      config,
      config_path,
      overwrite = overwrite,
      create_directories = TRUE
    )
    values <- read_spacing_config_file(config_file)
  }
  runtime <- spacing_config(
    repository = repository,
    project = project,
    config_file = config_file,
    dotnet = dotnet,
    timeout = timeout,
    use_compiled = use_compiled,
    executable = executable
  )
  structure(
    list(config_file = config_file, runtime = runtime, values = values),
    class = "spacing_runtime_setup"
  )
}

#' @export
print.spacing_config_values <- function(x, ...) {
  cat(
    "<spacing_config_values>\n",
    "  API:         ", x$DbApiBaseUrl, "\n",
    "  years:       ", x$StartYear, "-", x$EndYear, "\n",
    "  parallelism: ", x$MaxParallelism, "\n",
    "  output:      ", x$OutputDir, "\n",
    "  weather:     ", x$TempWeatherDir, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.spacing_runtime_setup <- function(x, ...) {
  cat("<spacing_runtime_setup>\n", "  config: ", x$config_file, "\n", sep = "")
  print(x$runtime)
  invisible(x)
}

.path_relative_if_within <- function(path, base) {
  absolute <- as.character(fs::path_abs(path))
  base <- as.character(fs::path_abs(base))
  relative <- as.character(fs::path_rel(absolute, start = base))
  if (identical(relative, "..") || startsWith(relative, paste0("..", .Platform$file.sep))) {
    absolute
  } else {
    relative
  }
}

.safe_file_id <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) .rspacing_abort("Scenario identifier is not file-safe.",
                                  "spacing_config_error")
  x
}
