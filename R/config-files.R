#' Create the values of a spacing.config.json file
#'
#' Builds, in memory, the content of `spacing.config.json`, the file read by
#' `SpacingConfig.cs` in Spacing.Tool2. The standard properties are
#' `DbApiBaseUrl`, `DbApiKey`, `TempWeatherDir`, `OutputDir`, `StartYear`,
#' `EndYear` and `MaxParallelism`; `additional` keeps custom properties needed
#' by a modified C# build. Write it with [write_spacing_config_file()].
#'
#' @param db_api_base_url Character scalar. Base URL of the soil/weather
#'   database API (not used in mock runs, but required by Tool2).
#' @param db_api_key Character scalar. API key; empty by default.
#' @param temp_weather_dir Character scalar. Directory where Tool2 writes the
#'   generated APSIM `.met` files.
#' @param output_dir Character scalar. Directory for the `.apsimx`, SQLite and
#'   CSV outputs.
#' @param start_year,end_year Integers. Default simulation years of Tool2
#'   (requests and scenarios can set their own period). `start_year` must not
#'   exceed `end_year`.
#' @param max_parallelism Positive integer. Maximum number of concurrent APSIM
#'   simulations in Tool2.
#' @param additional Named list of custom JSON properties, appended after the
#'   standard ones.
#' @param allow_override Logical. Allow `additional` to replace standard
#'   properties (otherwise an error).
#' @return An object of class `spacing_config_values` (a named list with the
#'   JSON properties), validated with [validate_spacing_config_values()].
#' @family configuration
#' @export
#' @examples
#' values <- new_spacing_config_values(
#'   db_api_base_url = "http://localhost:8000/api/v1",
#'   temp_weather_dir = file.path(tempdir(), "weather"),
#'   output_dir = file.path(tempdir(), "output"),
#'   start_year = 2020, end_year = 2024)
#' values
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

#' Validate the values of a spacing.config.json file
#'
#' Checks that all standard properties are present and well formed:
#' character scalars for the API URL/key and the directories, non-empty
#' directories, integer years with `StartYear <= EndYear`, and a positive
#' integer `MaxParallelism`.
#'
#' @param x A `spacing_config_values` object or a compatible named list.
#' @param check_directories Logical. Also require `TempWeatherDir` and
#'   `OutputDir` to exist.
#' @return `x`, invisibly. Problems raise errors of class
#'   `spacing_config_error`.
#' @family configuration
#' @export
#' @examples
#' values <- new_spacing_config_values(
#'   db_api_base_url = "http://localhost:8000/api/v1",
#'   temp_weather_dir = file.path(tempdir(), "weather"),
#'   output_dir = file.path(tempdir(), "output"),
#'   start_year = 2020, end_year = 2024)
#' validate_spacing_config_values(values)
#' try(validate_spacing_config_values(modifyList(unclass(values), list(EndYear = 2010L))))
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

#' Read a spacing.config.json file
#'
#' Reads and validates the JSON values only. Unlike [spacing_config()], it does
#' not need the Tool2 project and creates no directory, so it can be used to
#' inspect or prepare files before the runtime is set up.
#'
#' @param path Character scalar. Path of an existing JSON file.
#' @param validate Logical. Validate the standard properties with
#'   [validate_spacing_config_values()].
#' @param normalise_paths Logical. Resolve relative `OutputDir` and
#'   `TempWeatherDir` against the directory of the JSON file.
#' @return A `spacing_config_values` object with the attribute `source_file`
#'   (absolute path of the file read). Custom properties are kept.
#' @family configuration
#' @export
#' @examples
#' values <- new_spacing_config_values(
#'   db_api_base_url = "http://localhost:8000/api/v1",
#'   temp_weather_dir = file.path(tempdir(), "weather"),
#'   output_dir = file.path(tempdir(), "output"),
#'   start_year = 2020, end_year = 2024)
#' path <- write_spacing_config_file(values, file.path(tempdir(), "spacing.config.json"),
#'                                   overwrite = TRUE)
#' read_spacing_config_file(path)
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

#' Write a spacing.config.json file
#'
#' Validates the values, optionally creates the output and weather
#' directories, and writes the JSON atomically (to a temporary file that is
#' then moved into place). An existing file is replaced only with
#' `overwrite = TRUE`.
#'
#' Directory properties are written as absolute paths (relative values are
#' resolved against the destination directory) unless `relative_to` is given.
#'
#' @param x A `spacing_config_values` object or compatible named list.
#' @param path Character scalar. Destination JSON file.
#' @param overwrite Logical. Replace an existing destination.
#' @param create_directories Logical. Create `OutputDir` and `TempWeatherDir`.
#' @param pretty Logical. Write indented, human-readable JSON.
#' @param relative_to Optional directory. When given, the directory properties
#'   are written relative to it when they lie inside it (absolute otherwise),
#'   which keeps the file portable between computers.
#' @return The absolute destination path, invisibly.
#' @family configuration
#' @export
#' @examples
#' values <- new_spacing_config_values(
#'   db_api_base_url = "http://localhost:8000/api/v1",
#'   temp_weather_dir = file.path(tempdir(), "weather"),
#'   output_dir = file.path(tempdir(), "output"),
#'   start_year = 2020, end_year = 2024)
#' path <- write_spacing_config_file(values, file.path(tempdir(), "cfg", "spacing.config.json"),
#'                                   overwrite = TRUE, relative_to = tempdir())
#' cat(readLines(path), sep = "\n")
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

#' Update an existing spacing.config.json file
#'
#' Reads all current properties, replaces the named ones, keeps unknown custom
#' properties, validates the result and writes it in place (with an optional
#' timestamped backup) or to `output`.
#'
#' @param path Character scalar. Existing JSON configuration.
#' @param ... Named properties to change, e.g. `EndYear = 2025L`.
#' @param values Optional named list of properties to change (combined with
#'   `...`; `...` wins on duplicates).
#' @param output Optional destination path. `NULL` updates `path` in place.
#' @param create_directories Logical. Create the configured directories.
#' @param backup Logical. When updating in place, first copy the original to
#'   `<path>.<timestamp>.bak`.
#' @return The absolute path of the updated file, invisibly.
#' @family configuration
#' @export
#' @examples
#' values <- new_spacing_config_values(
#'   db_api_base_url = "http://localhost:8000/api/v1",
#'   temp_weather_dir = file.path(tempdir(), "weather"),
#'   output_dir = file.path(tempdir(), "output"),
#'   start_year = 2020, end_year = 2024)
#' path <- write_spacing_config_file(values, file.path(tempdir(), "spacing.config.json"),
#'                                   overwrite = TRUE)
#' update_spacing_config_file(path, EndYear = 2025L, backup = FALSE)
#' read_spacing_config_file(path)$EndYear
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

#' Prepare one configuration file per scenario
#'
#' Creates an isolated `spacing.config.json` for each row of a scenario table,
#' each with its own `OutputDir` and `TempWeatherDir`
#' (`<directory>/<scenario_id>/output` and `.../weather`), so that runs
#' executed in parallel cannot overwrite each other. Columns of `scenarios`
#' named like standard JSON properties (e.g. `StartYear`) override the base
#' value for that scenario; `NA` keeps the base value.
#'
#' @param base A `spacing_config_values` object, or the path of an existing
#'   JSON configuration.
#' @param scenarios Data frame with a unique identifier column and optional
#'   property columns.
#' @param directory Character scalar. Root directory of the scenario folders.
#' @param id_column Character scalar. Name of the identifier column; values
#'   are made file-safe for folder names.
#' @param overwrite Logical. Replace existing scenario JSON files.
#' @param create_directories Logical. Create the scenario directories.
#' @return A data frame with one row per scenario and columns `scenario_id`,
#'   `config_file`, `output_dir` and `temp_weather_dir`.
#' @family configuration
#' @export
#' @examples
#' values <- new_spacing_config_values(
#'   db_api_base_url = "http://localhost:8000/api/v1",
#'   temp_weather_dir = file.path(tempdir(), "weather"),
#'   output_dir = file.path(tempdir(), "output"),
#'   start_year = 2020, end_year = 2024)
#' prepare_spacing_config_set(values,
#'   data.frame(scenario_id = c("base", "late"), EndYear = c(NA, 2025L)),
#'   directory = file.path(tempdir(), "scenarios"), overwrite = TRUE)
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

#' Prepare the configuration file and the runtime in one step
#'
#' Writes `spacing.config.json` when `config` holds values (or reads it when
#' `config` is a path), then creates the [spacing_config()] runtime object. It
#' is the highest-level set-up function for a single workflow.
#'
#' @param repository Character scalar. Root of the C# `spacing` repository.
#' @param config Either a `spacing_config_values` object or the path of an
#'   existing JSON configuration.
#' @param config_path Character scalar. Destination of the JSON file when
#'   `config` is an in-memory object. Default
#'   `<repository>/apsim/spacing.config.json`.
#' @param project,dotnet,timeout,use_compiled,executable Passed to
#'   [spacing_config()].
#' @param overwrite Logical. Replace `config_path` when writing values.
#' @return An object of class `spacing_runtime_setup`: a list with
#'   `config_file` (path), `runtime` (the `spacing_config`) and `values` (the
#'   `spacing_config_values` read back from disk).
#' @family configuration
#' @export
#' @examples
#' root <- tempfile("spacing")
#' dir.create(file.path(root, "apsim", "src", "Spacing.Tool2"), recursive = TRUE)
#' values <- new_spacing_config_values("http://localhost:8000/api/v1",
#'   temp_weather_dir = "weather", output_dir = "output",
#'   start_year = 2020, end_year = 2024)
#' setup <- prepare_spacing_runtime(root, values)
#' setup
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

#' @describeIn new_spacing_config_values Print API URL, years, parallelism and
#'   directories.
#' @param x A `spacing_config_values` object.
#' @param ... Ignored.
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

#' @describeIn prepare_spacing_runtime Print the configuration file and the
#'   runtime paths.
#' @param x A `spacing_runtime_setup` object.
#' @param ... Ignored.
#' @export
print.spacing_runtime_setup <- function(x, ...) {
  cat("<spacing_runtime_setup>\n", "  config: ", x$config_file, "\n", sep = "")
  print(x$runtime)
  invisible(x)
}

#' Express a path relative to a base when it lies inside it
#' @param path Character scalar. Path to convert.
#' @param base Character scalar. Base directory.
#' @return The relative path when `path` is inside `base`, otherwise the
#'   absolute path.
#' @noRd
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

#' Make an identifier safe for file and folder names
#' @param x Character scalar.
#' @return `x` with runs of characters other than letters, digits, `.`, `_`
#'   and `-` replaced by `_` and leading/trailing `_` removed. An empty result
#'   raises a `spacing_config_error`.
#' @noRd
.safe_file_id <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) .rspacing_abort("Scenario identifier is not file-safe.",
                                  "spacing_config_error")
  x
}
