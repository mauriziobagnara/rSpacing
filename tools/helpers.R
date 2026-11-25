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
