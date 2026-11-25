# Install rSpacing from the workspace (step T03)
#
# In RStudio, with the workspace root as working directory:
#   source("install_rspacing.R")
#
# 1. installs the CRAN packages that are missing (internet needed once);
# 2. builds rSpacing from rSpacing/ as a source tarball with its vignette;
# 3. installs the tarball and checks the installed version.
# The tarball is kept in results/package/. Nothing in rSpacing/ is changed.

source(file.path("tools", "helpers.R"))
root <- ws_check_root()

missing <- WORKSPACE_PACKAGES[!vapply(WORKSPACE_PACKAGES, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
still_missing <- WORKSPACE_PACKAGES[!vapply(WORKSPACE_PACKAGES, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing)) stop("Could not install: ", paste(still_missing, collapse = ", "), call. = FALSE)

if (!ws_find_pandoc()) stop("pandoc not found: run this from RStudio, or install pandoc.", call. = FALSE)

out_dir <- file.path(root, "results", "package")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
message("Building rSpacing with its vignette (about 1 minute)...")
tarball <- devtools::build(pkg = file.path(root, "rSpacing"), path = out_dir,
                           vignettes = TRUE, manual = FALSE, quiet = TRUE)
if ("rSpacing" %in% loadedNamespaces()) try(unloadNamespace("rSpacing"), silent = TRUE)
install.packages(tarball, repos = NULL, type = "source")

installed <- as.character(utils::packageVersion("rSpacing", lib.loc = .libPaths()))
vig <- utils::vignette(package = "rSpacing")$results
if (!identical(installed, WORKSPACE_EXPECTED$rspacing))
  stop("Installed rSpacing is ", installed, ", expected ", WORKSPACE_EXPECTED$rspacing, call. = FALSE)
if (!"rSpacing-workflow_EN" %in% vig[, "Item"]) stop("The vignette was not installed.", call. = FALSE)

message("rSpacing ", installed, " installed from ", basename(tarball), ", with the vignette.")
message("Restart R (Session > Restart R), then run step T04: source(\"environment_check.R\")")
