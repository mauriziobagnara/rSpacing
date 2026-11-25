#' Default for NULL or empty values
#'
#' Internal infix helper: returns `y` when `x` is `NULL` or has length zero,
#' otherwise `x`.
#'
#' @param x Any R object.
#' @param y Value returned when `x` is `NULL` or empty.
#' @return `x` or `y`.
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

#' Signal a classed rSpacing error
#'
#' Internal helper that raises an error condition with a specific class, so
#' callers can catch rSpacing errors selectively with [tryCatch()].
#'
#' @param message Character scalar. Error message.
#' @param class Character vector. Condition class(es) placed before `"error"`
#'   and `"condition"`, e.g. `"spacing_validation_error"`.
#' @param ... Extra named fields stored in the condition (e.g. `path`, `run`).
#' @param call. Logical. Record the calling expression in the condition.
#' @return Never returns; always signals an error.
#' @noRd
.rspacing_abort <- function(message, class = "rspacing_error", ..., call. = FALSE) {
  condition <- structure(
    c(list(message = message, call = if (call.) sys.call(-1L) else NULL), list(...)),
    class = c(class, "error", "condition")
  )
  stop(condition)
}

#' Check that a path exists and return it as an absolute path
#'
#' @param path Character scalar. File or directory path.
#' @param name Character scalar. Human-readable label used in error messages.
#' @param type `"file"` or `"dir"`: what `path` must be.
#' @return The absolute path as a character scalar. A
#'   `spacing_validation_error` is raised when the path is invalid or missing.
#' @noRd
.normalise_existing <- function(path, name, type = c("file", "dir")) {
  type <- match.arg(type)
  if (!is.character(path) || length(path) != 1L || !nzchar(path))
    .rspacing_abort(sprintf("'%s' must be one non-empty path.", name), "spacing_validation_error")
  path <- fs::path_abs(path)
  ok <- if (type == "file") fs::file_exists(path) else fs::dir_exists(path)
  if (!ok) .rspacing_abort(sprintf("%s not found: %s", name, path), "spacing_validation_error", path = path)
  as.character(path)
}

#' Read a JSON file with a classed error on failure
#'
#' @param path Character scalar. JSON file path.
#' @return The parsed object ([jsonlite::fromJSON()] with
#'   `simplifyVector = TRUE`). Invalid JSON raises a `spacing_config_error`.
#' @noRd
.read_json <- function(path) {
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
           error = function(e) .rspacing_abort(sprintf("Invalid JSON '%s': %s", path, conditionMessage(e)),
                                                "spacing_config_error", path = path))
}

#' SHA-256 hash of a file
#'
#' @param path Character scalar. File path.
#' @return Character scalar with the SHA-256 digest, or `NA` when the file
#'   does not exist.
#' @noRd
.hash_file <- function(path) if (fs::file_exists(path)) digest::digest(file = path, algo = "sha256") else NA_character_

#' Current time as a UTC string
#'
#' @return Character scalar, e.g. `"2026-11-24 14:00:00 UTC"`.
#' @noRd
.utc_now <- function() format(Sys.time(), tz = "UTC", usetz = TRUE)
