# Helpers shared by install_rspacing.R, environment_check.R and run_test.R.
# They assume the working directory is the workspace root.

WORKSPACE_EXPECTED <- list(rspacing = "0.10.9", tool2 = "0.09.10")

ws_root <- function() normalizePath(".", winslash = "/", mustWork = TRUE)

ws_check_root <- function(root = ws_root()) {
  if (!dir.exists(file.path(root, "rSpacing")) || !dir.exists(file.path(root, "apsim")))
    stop("Run this from the workspace root (the folder that contains rSpacing/ and apsim/). ",
         "Current folder: ", root, call. = FALSE)
  invisible(root)
}

# Find pandoc: RStudio provides it inside a session; outside RStudio we look in the
# usual Windows install folders and set RSTUDIO_PANDOC for rmarkdown.
ws_find_pandoc <- function() {
  if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) return(TRUE)
  candidates <- c(
    Sys.glob("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/x86_64"),
    Sys.glob("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"),
    "C:/Program Files/RStudio/bin/pandoc", "C:/Program Files/RStudio/bin/quarto/bin/tools",
    "C:/Program Files/Pandoc", file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"),
    Sys.glob("C:/Program Files/Quarto/bin/tools*"),
    Sys.glob(file.path(Sys.getenv("LOCALAPPDATA"), "Programs/RStudio/resources/app/bin/quarto/bin/tools*")),
    Sys.glob(file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin/tools*")))
  exe <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  hit <- candidates[file.exists(file.path(candidates, exe))]
  if (length(hit)) Sys.setenv(RSTUDIO_PANDOC = hit[[1]])
  requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()
}

ws_tool2_dll <- function(root = ws_root()) {
  candidates <- file.path(root, "apsim", "src", "Spacing.Tool2", "bin", c("Debug", "Release"),
                          "net8.0", "spacing-tool2.dll")
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) normalizePath(hit[[1]], winslash = "/") else NA_character_
}

# TRUE when the DLL is newer than every C# source file of Spacing.Tool2 and Spacing.Core.
ws_tool2_up_to_date <- function(root = ws_root()) {
  dll <- ws_tool2_dll(root)
  if (is.na(dll)) return(FALSE)
  src <- list.files(file.path(root, "apsim", "src", c("Spacing.Tool2", "Spacing.Core")),
                    pattern = "[.](cs|csproj)$", recursive = TRUE, full.names = TRUE)
  src <- src[!grepl("/(bin|obj)/", src)]
  !length(src) || file.mtime(dll) >= max(file.mtime(src))
}

ws_rscript <- function() {
  exe <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  file.path(R.home("bin"), exe)
}

# CRAN packages used by the package, its checks and the workflows.
WORKSPACE_PACKAGES <- c(
  "DBI", "RSQLite", "jsonlite", "processx", "fs", "digest", "ggplot2", "rlang", "patchwork",
  "devtools", "testthat", "knitr", "rmarkdown", "roxygen2", "rcmdcheck",
  "sensitivity", "scales", "ggrepel")

# Plain, console-safe text: no ANSI colours, no carriage returns or other control
# characters, ASCII only, limited width. Printing raw R CMD check output can crash RStudio.
ws_clean <- function(x, width = 150) {
  x <- as.character(x)
  x <- gsub("\033\\[[0-9;?]*[A-Za-z]", "", x, perl = TRUE, useBytes = TRUE)
  x <- gsub("[^\\x20-\\x7E\t]", "", x, perl = TRUE, useBytes = TRUE)
  ifelse(nchar(x) > width, paste0(substr(x, 1, width - 3), "..."), x)
}

# The last n non-empty lines of a log, cleaned for the console
ws_log_tail <- function(log, n = 25, width = 150) {
  if (!file.exists(log)) return(character())
  lines <- tryCatch(readLines(log, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  lines <- ws_clean(lines, width)
  utils::tail(lines[nzchar(trimws(lines))], n)
}

# Run a command, write its output (stdout and stderr) to `log`, and return the exit status.
# The log is appended line by line and closed after each write, so it can be opened
# (Notepad, Get-Content) while the command runs. Every `heartbeat` seconds a line with
# the elapsed time and the last line of output is printed to the console.
ws_run <- function(command, args = character(), log, label = basename(log), heartbeat = 60) {
  dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
  cat("", file = log)
  if (!requireNamespace("processx", quietly = TRUE)) {        # fallback: log locked while running
    quoted <- ifelse(grepl("[[:space:]]", args), shQuote(args), args)
    return(system2(command, quoted, stdout = log, stderr = log))
  }
  p <- processx::process$new(command, args, stdout = "|", stderr = "2>&1", windows_hide_window = TRUE)
  started <- last_beat <- Sys.time(); last_line <- ""
  write_lines <- function(lines) {
    if (!length(lines)) return()
    cat(lines, file = log, sep = "\n", append = TRUE)
    lines <- trimws(lines[nzchar(trimws(lines))])
    if (length(lines)) last_line <<- utils::tail(lines, 1)
  }
  while (p$is_alive()) {
    p$poll_io(2000)
    write_lines(p$read_output_lines())
    if (as.numeric(difftime(Sys.time(), last_beat, units = "secs")) >= heartbeat) {
      cat(format(Sys.time(), "%H:%M:%S"), " ", label, " still running (",
          round(as.numeric(difftime(Sys.time(), started, units = "mins"))), " min). Last output: ",
          ws_clean(last_line, 110), "\n", sep = "")
      last_beat <- Sys.time()
    }
  }
  # Drain the remaining output for a few seconds only: a background process started by the
  # command (e.g. a dotnet build server) may keep the pipe open after the command has ended.
  for (i in 1:5) { p$poll_io(500); write_lines(p$read_output_lines()) }
  p$wait()
  status <- p$get_exit_status()
  if (is.null(status) || is.na(status)) 1L else status
}
