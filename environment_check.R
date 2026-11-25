# rSpacing / Spacing.Tool2 workspace environment check (step T04)
#
# In RStudio, with the workspace root as working directory:
#   source("environment_check.R")
#
# Checks the layout, versions, the Tool2 build, R packages, pandoc and write access.
# Writes logs/environment_check.csv and stops if a required check fails.
# It changes no source file.

source(file.path("tools", "helpers.R"))
workspace_root <- ws_check_root()
log_root <- file.path(workspace_root, "logs")
dir.create(log_root, recursive = TRUE, showWarnings = FALSE)

check_rows <- list()
add_check <- function(id, required, passed, detail) {
  check_rows[[length(check_rows) + 1L]] <<- data.frame(
    check_id = id, required = isTRUE(required), passed = isTRUE(passed),
    detail = paste(detail, collapse = " | "), stringsAsFactors = FALSE)
}
capture_command <- function(command, args = character(), file) {
  output <- tryCatch(system2(command, args, stdout = TRUE, stderr = TRUE),
                     error = function(e) paste("ERROR:", conditionMessage(e)))
  writeLines(output, file.path(log_root, file), useBytes = TRUE)
  output
}
is_writable_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  probe <- tempfile("write_test_", tmpdir = path)
  ok <- dir.exists(path) && suppressWarnings(file.create(probe))
  if (isTRUE(ok)) unlink(probe, force = TRUE)
  isTRUE(ok)
}

r_root  <- file.path(workspace_root, "rSpacing")
cs_root <- file.path(workspace_root, "apsim")

# Layout and versions of the sources
add_check("R_version_4_3_or_newer", TRUE, getRversion() >= "4.3.0", R.version.string)
desc <- tryCatch(read.dcf(file.path(r_root, "DESCRIPTION")), error = function(e) NULL)
src_version <- if (!is.null(desc)) desc[1, "Version"] else NA
add_check("rSpacing_source_0.10.09", TRUE, identical(unname(src_version), "0.10.09"),
          paste("rSpacing/DESCRIPTION Version:", src_version))
tool2_version <- tryCatch(trimws(readLines(file.path(cs_root, "VERSION"), warn = FALSE)[1]), error = function(e) NA)
add_check("Tool2_source_0.09.10", TRUE, identical(tool2_version, WORKSPACE_EXPECTED$tool2),
          paste("apsim/VERSION:", tool2_version))
add_check("Tool2_project", TRUE, file.exists(file.path(cs_root, "src", "Spacing.Tool2", "Spacing.Tool2.csproj")),
          "apsim/src/Spacing.Tool2/Spacing.Tool2.csproj")
add_check("Tool2_config", TRUE, file.exists(file.path(cs_root, "spacing.config.json")), "apsim/spacing.config.json")

# .NET and the Tool2 build
dotnet <- Sys.which("dotnet")
add_check("dotnet_on_PATH", TRUE, nzchar(dotnet), if (nzchar(dotnet)) dotnet else "dotnet not found")
dotnet_info <- if (nzchar(dotnet)) capture_command(dotnet, "--info", "dotnet_info.txt") else character()
add_check("dotnet_8", TRUE, any(grepl("(^|[^0-9])8[.]0[.]", dotnet_info)),
          if (any(grepl("8[.]0[.]", dotnet_info))) "a .NET 8 SDK/runtime is installed" else "no .NET 8 found")
dll <- ws_tool2_dll(workspace_root)
add_check("Tool2_DLL_built", TRUE, !is.na(dll),
          if (is.na(dll)) "not found: run build_tool2.ps1 (step T02)" else dll)
add_check("Tool2_DLL_up_to_date", TRUE, ws_tool2_up_to_date(workspace_root),
          "DLL newer than the C# sources; if FALSE run build_tool2.ps1 again")

# Installed rSpacing and R packages
installed <- tryCatch(as.character(utils::packageVersion("rSpacing")), error = function(e) NA)
add_check("rSpacing_installed_0.10.9", TRUE, identical(installed, WORKSPACE_EXPECTED$rspacing),
          paste("installed:", installed, "- if wrong run install_rspacing.R (step T03)"))
vig <- tryCatch(utils::vignette(package = "rSpacing")$results[, "Item"], error = function(e) character())
add_check("rSpacing_vignette_installed", TRUE, "rSpacing-workflow_EN" %in% vig, "rSpacing-workflow_EN")
for (pkg in WORKSPACE_PACKAGES)
  add_check(paste0("R_package_", pkg), !pkg %in% c("ggrepel"), requireNamespace(pkg, quietly = TRUE), pkg)
add_check("pandoc", TRUE, ws_find_pandoc(), "needed to render the workflows (RStudio includes it)")

# Workspace folders and workflows
for (name in c("workflows", "runs", "results", "logs"))
  add_check(paste0(name, "_writable"), TRUE, is_writable_dir(file.path(workspace_root, name)), name)
shipped <- list.files(file.path(r_root, "inst", "rmarkdown"), pattern = "[.]Rmd$")
present <- shipped %in% list.files(file.path(workspace_root, "workflows"), pattern = "[.]Rmd$")
add_check("workflows_copied", TRUE, length(shipped) > 0 && all(present),
          if (all(present)) paste(length(shipped), "workflows in workflows/") else
            paste("missing in workflows/:", paste(shipped[!present], collapse = ", "), "- run setup_workspace.ps1"))

# Git provenance (optional): the workspace is normally a clone of the rSpacing repository;
# apsim/ is a copy of the apsim/ folder of APSIMxSpacing, or a clone of it.
git <- Sys.which("git")
add_check("git_on_PATH", FALSE, nzchar(git), if (nzchar(git)) git else "git not found")
for (repo in list(c("workspace", "."), c("apsim", "apsim"))) {
  path <- file.path(workspace_root, repo[2])
  is_repo <- nzchar(git) && dir.exists(file.path(path, ".git"))   # apsim/ inside the workspace is normally a copy
  add_check(paste0(repo[1], "_in_git"), FALSE, is_repo, if (is_repo) path else paste("not in a Git clone:", path))
  if (is_repo) {
    capture_command(git, c("-C", shQuote(path), "rev-parse", "HEAD"), paste0(repo[1], "_commit.txt"))
    capture_command(git, c("-C", shQuote(path), "branch", "--show-current"), paste0(repo[1], "_branch.txt"))
    capture_command(git, c("-C", shQuote(path), "status", "--short"), paste0(repo[1], "_status.txt"))
  }
}

checks <- do.call(rbind, check_rows)
utils::write.csv(checks, file.path(log_root, "environment_check.csv"), row.names = FALSE)
writeLines(utils::capture.output(utils::sessionInfo()), file.path(log_root, "sessionInfo.txt"), useBytes = TRUE)
print(checks[, c("check_id", "required", "passed")], row.names = FALSE)
cat("\nDetails: ", file.path(log_root, "environment_check.csv"), "\n", sep = "")

failed <- checks$required & !checks$passed
if (any(failed))
  stop("Environment check failed for: ", paste(checks$check_id[failed], collapse = ", "),
       ". See the detail column of logs/environment_check.csv.", call. = FALSE)
message("Environment check passed. Next: source(\"run_test.R\"); run_test(\"T05\")")
