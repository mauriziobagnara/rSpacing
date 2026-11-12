`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

.rspacing_abort <- function(message, class = "rspacing_error", ..., call. = FALSE) {
  condition <- structure(
    c(list(message = message, call = if (call.) sys.call(-1L) else NULL), list(...)),
    class = c(class, "error", "condition")
  )
  stop(condition)
}

.normalise_existing <- function(path, name, type = c("file", "dir")) {
  type <- match.arg(type)
  if (!is.character(path) || length(path) != 1L || !nzchar(path))
    .rspacing_abort(sprintf("'%s' must be one non-empty path.", name), "spacing_validation_error")
  path <- fs::path_abs(path)
  ok <- if (type == "file") fs::file_exists(path) else fs::dir_exists(path)
  if (!ok) .rspacing_abort(sprintf("%s not found: %s", name, path), "spacing_validation_error", path = path)
  as.character(path)
}

.read_json <- function(path) {
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
           error = function(e) .rspacing_abort(sprintf("Invalid JSON '%s': %s", path, conditionMessage(e)),
                                                "spacing_config_error", path = path))
}

.hash_file <- function(path) if (fs::file_exists(path)) digest::digest(file = path, algo = "sha256") else NA_character_

.utc_now <- function() format(Sys.time(), tz = "UTC", usetz = TRUE)
