# rSpacing / Spacing.Tool2 workspace environment check
# Run from the workspace root. This script changes no source files.

workspace_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
log_root <- file.path(workspace_root, "logs")
dir.create(log_root, recursive = TRUE, showWarnings = FALSE)

check_rows <- list()
add_check <- function(id, required, passed, detail) {
  check_rows[[length(check_rows) + 1L]] <<- data.frame(
    check_id = id,
    required = isTRUE(required),
    passed = isTRUE(passed),
    detail = paste(detail, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

capture_command <- function(command, args = character(), file) {
  output <- tryCatch(
    system2(command, args, stdout = TRUE, stderr = TRUE),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
  writeLines(output, file.path(log_root, file), useBytes = TRUE)
  output
}

is_writable_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) return(FALSE)
  probe <- tempfile("write_test_", tmpdir = path)
  ok <- suppressWarnings(file.create(probe))
  if (isTRUE(ok)) unlink(probe, force = TRUE)
  isTRUE(ok)
}

r_root <- file.path(workspace_root, "rSpacing")
cs_root <- file.path(workspace_root, "apsim")
project <- file.path(cs_root, "src", "Spacing.Tool2", "Spacing.Tool2.csproj")
dll_debug <- file.path(cs_root, "src", "Spacing.Tool2", "bin", "Debug", "net8.0", "spacing-tool2.dll")
dll_release <- file.path(cs_root, "src", "Spacing.Tool2", "bin", "Release", "net8.0", "spacing-tool2.dll")

add_check("workspace_root", TRUE, dir.exists(workspace_root), workspace_root)
add_check("R_version_4_3_or_newer", TRUE, getRversion() >= "4.3.0", R.version.string)
add_check("r_package_DESCRIPTION", TRUE, file.exists(file.path(r_root, "DESCRIPTION")), file.path(r_root, "DESCRIPTION"))
add_check("csharp_project", TRUE, file.exists(project), project)
add_check("spacing_config", TRUE, file.exists(file.path(cs_root, "spacing.config.json")), file.path(cs_root, "spacing.config.json"))

if (file.exists(file.path(r_root, "DESCRIPTION"))) {
  desc <- tryCatch(read.dcf(file.path(r_root, "DESCRIPTION")), error = function(e) NULL)
  version <- if (!is.null(desc) && "Version" %in% colnames(desc)) desc[1, "Version"] else NA_character_
  add_check("rSpacing_version_0_10_05", TRUE, identical(as.character(version), "0.10.05"), paste("Version:", version))
}

dotnet <- Sys.which("dotnet")
add_check("dotnet_on_PATH", TRUE, nzchar(dotnet), if (nzchar(dotnet)) dotnet else "dotnet not found")
if (nzchar(dotnet)) {
  dotnet_info <- capture_command(dotnet, "--info", "dotnet_info.txt")
  dotnet_8_lines <- dotnet_info[grepl("8[.]0", dotnet_info)]
  add_check(
    "dotnet_8",
    TRUE,
    length(dotnet_8_lines) > 0L,
    if (length(dotnet_8_lines)) head(dotnet_8_lines, 5L) else "No .NET 8 SDK/runtime detected"
  )
} else {
  writeLines("dotnet not found", file.path(log_root, "dotnet_info.txt"))
  add_check("dotnet_8", TRUE, FALSE, "dotnet not found")
}

add_check("tool2_DLL_Debug_or_Release", TRUE, file.exists(dll_debug) || file.exists(dll_release), c(dll_debug, dll_release))
for (name in c("workflows", "runs", "results", "logs")) {
  path <- file.path(workspace_root, name)
  add_check(paste0(name, "_writable"), TRUE, is_writable_dir(path), path)
}

required_packages <- c("devtools", "testthat", "knitr", "rmarkdown", "jsonlite", "ggplot2", "sensitivity", "DBI", "RSQLite")
for (pkg in required_packages) {
  add_check(paste0("R_package_", pkg), TRUE, requireNamespace(pkg, quietly = TRUE), pkg)
}

# Git provenance is recorded when the sibling directories are Git clones.
# Local copied directories remain acceptable for the environment gate, but the
# missing provenance is reported as an optional check and must be resolved
# before final delivery to the intern.
git <- Sys.which("git")
add_check("git_on_PATH", FALSE, nzchar(git), if (nzchar(git)) git else "git not found")
if (nzchar(git)) {
  for (repo in c("rSpacing", "apsim")) {
    path <- file.path(workspace_root, repo)
    is_repo <- dir.exists(file.path(path, ".git"))
    add_check(
      paste0(repo, "_is_git_clone"),
      FALSE,
      is_repo,
      if (is_repo) path else paste("Not a Git clone:", path)
    )
    if (is_repo) {
      commit <- capture_command(git, c("-C", path, "rev-parse", "HEAD"), paste0(repo, "_commit.txt"))
      branch <- capture_command(git, c("-C", path, "branch", "--show-current"), paste0(repo, "_branch.txt"))
      status <- capture_command(git, c("-C", path, "status", "--short"), paste0(repo, "_status.txt"))
      add_check(paste0(repo, "_git_commit"), FALSE, length(commit) == 1L && grepl("^[0-9a-fA-F]{40}$", commit), commit)
      add_check(paste0(repo, "_git_status_recorded"), FALSE, file.exists(file.path(log_root, paste0(repo, "_status.txt"))), if (length(status)) status else "clean")
      add_check(paste0(repo, "_git_branch_recorded"), FALSE, length(branch) >= 1L && nzchar(branch[1]), branch)
    } else {
      writeLines("Not a Git clone", file.path(log_root, paste0(repo, "_commit.txt")))
      writeLines("Not a Git clone", file.path(log_root, paste0(repo, "_branch.txt")))
      writeLines("Not a Git clone", file.path(log_root, paste0(repo, "_status.txt")))
    }
  }
}

checks <- do.call(rbind, check_rows)
write.csv(checks, file.path(log_root, "environment_check.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(log_root, "sessionInfo.txt"), useBytes = TRUE)
display_checks <- checks[, c("check_id", "required", "passed"), drop = FALSE]
print(display_checks, row.names = FALSE)
cat("\nFull details: ", file.path(log_root, "environment_check.csv"), "\n", sep = "")

failed <- checks$required & !checks$passed
if (any(failed)) {
  stop(
    "Environment check failed for: ",
    paste(checks$check_id[failed], collapse = ", "),
    ". See logs/environment_check.csv.",
    call. = FALSE
  )
}

message("Environment check passed. Evidence is in: ", log_root)
