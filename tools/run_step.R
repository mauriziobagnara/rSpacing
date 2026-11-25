# One test step, run in a fresh R process by run_test(). Not meant to be called directly:
#   Rscript tools/run_step.R <ID> <workspace root>

args <- commandArgs(trailingOnly = TRUE)
id <- args[[1]]
setwd(args[[2]])
source(file.path("tools", "helpers.R"))
root <- ws_check_root()
options(warn = 1, width = 120)
`%||%` <- function(a, b) if (is.null(a)) b else a
cat("=== ", id, " started ", format(Sys.time()), " ===\n", sep = "")
cat("Workspace: ", root, "\nR: ", R.version.string, "\n", sep = "")
ws_find_pandoc()

wf_dir  <- file.path(root, "workflows")
rep_dir <- file.path(root, "results", "test_results")
ref_dir <- file.path(root, "reference_results")
dir.create(rep_dir, recursive = TRUE, showWarnings = FALSE)
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

record <- function(status, detail, report = "") {
  f <- file.path(root, "logs", "test_status.csv")
  row <- data.frame(test = id, status = status, detail = gsub("[\r\n]+", " ", detail), report = report,
                    finished = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), stringsAsFactors = FALSE)
  utils::write.table(row, f, sep = ",", row.names = FALSE, col.names = !file.exists(f),
                     append = file.exists(f), qmethod = "double")
  cat("\n=== ", id, " ", status, ": ", detail, " ===\n", sep = "")
}

# Move the outputs of an earlier run of the same test out of the way (never deleted).
set_aside <- function(...) {
  for (rel in c(...)) {
    path <- file.path(root, rel)
    if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE))) {
      dest <- file.path(dirname(path), "_previous", stamp, basename(path))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (!file.rename(path, dest)) stop("Could not move ", path, " to ", dest)
      cat("Earlier outputs moved to ", dest, "\n", sep = "")
    }
  }
}

render <- function(file, report, params) {
  src <- file.path(wf_dir, file)
  if (!file.exists(src)) stop("Workflow not found: ", src, ". Run setup_workspace.ps1 (step T01).")
  rmarkdown::render(src, output_format = "html_document", output_dir = rep_dir, output_file = report,
                    params = params, envir = new.env(), intermediates_dir = tempdir(), quiet = FALSE)
}

within_pct <- function(x, ref, pct = 1) {
  x <- as.numeric(x); ref <- as.numeric(ref)
  all(abs(x - ref) <= pct / 100 * pmax(abs(ref), 1e-9) + 1e-6)
}
read_ref <- function(name) utils::read.csv(file.path(ref_dir, name), stringsAsFactors = FALSE)
latest <- function(pattern, dir) {
  f <- list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (!length(f)) stop("No file matching ", pattern, " under ", dir)
  f[which.max(file.mtime(f))]
}

result <- tryCatch({
  switch(id,

  T05 = {
    Sys.setenv(RSPACING_INTEGRATION = "true", RSPACING_APSIM_DIR = file.path(root, "apsim"),
               RSPACING_TOOL2_DLL = ws_tool2_dll(root))
    out <- file.path(rep_dir, "T05_package"); dir.create(out, showWarnings = FALSE)
    cat("\n--- tests (including real Tool2 runs) ---\n")
    tr <- as.data.frame(devtools::test(pkg = file.path(root, "rSpacing"), stop_on_failure = FALSE,
                                       reporter = testthat::ProgressReporter$new(show_praise = FALSE)))
    utils::write.csv(tr[, intersect(names(tr), c("file", "test", "nb", "failed", "skipped", "error", "warning"))],
                     file.path(out, "test_results.csv"), row.names = FALSE)
    cat("\n--- R CMD check ---\n")
    chk <- devtools::check(pkg = file.path(root, "rSpacing"), document = FALSE, manual = FALSE, cran = FALSE,
                           error_on = "never", check_dir = out)
    complete <- any(grepl("checking tests", chk$stdout %||% "")) && isTRUE(chk$status == 0L)
    if (!complete && !length(chk$errors)) chk$errors <- "R CMD check did not complete (see the log)"
    writeLines(c("ERRORS:", chk$errors, "", "WARNINGS:", chk$warnings, "", "NOTES:", chk$notes),
               file.path(out, "check_summary.txt"))
    detail <- sprintf("tests: %d passed, %d failed, %d errors, %d skipped; check: %d errors, %d warnings, %d notes",
                      sum(tr$nb) - sum(tr$failed) - sum(tr$skipped), sum(tr$failed), sum(tr$error),
                      sum(tr$skipped), length(chk$errors), length(chk$warnings), length(chk$notes))
    ok <- sum(tr$failed) == 0 && sum(tr$error) == 0 && !length(chk$errors)
    list(ok, paste0(detail, " (warnings and notes are listed in results/test_results/T05_package/check_summary.txt)"), "")
  },

  T06 = {
    set_aside("runs/manual_rev10", "results/manual_rev10")
    r <- render("rSpacing_v0.10.09_complete_operational_workflow_rev10_model_run_EN.Rmd", "T06_manual_no_run.html",
                list(workspace_root = "..", run_models = FALSE, output_subdir = "manual_rev10"))
    list(TRUE, "manual rendered without model runs", r)
  },

  T07 = {
    set_aside("runs/manual_rev10_run", "results/manual_rev10_run")
    r <- render("rSpacing_v0.10.09_complete_operational_workflow_rev10_model_run_EN.Rmd", "T07_manual_with_run.html",
                list(workspace_root = "..", run_models = TRUE, output_subdir = "manual_rev10_run"))
    dbs <- list.files(file.path(root, "runs", "manual_rev10_run"), pattern = "[.]db$", recursive = TRUE)
    list(length(dbs) > 0, sprintf("manual rendered; APSIM output databases written: %d", length(dbs)), r)
  },

  T08 = {
    set_aside("runs/five_year_v108", "results/five_year_v108")
    r <- render("rSpacing_APSIMx_5year_workflow_report_v1.08_EN.Rmd", "T08_five_year_v1.08.html",
                list(workspace_root = "..", output_subdir = "five_year_v108"))
    cs <- utils::read.csv(latest("^crop_summary_.*[.]csv$", file.path(root, "runs", "five_year_v108")))
    ref <- read_ref("T08_five_year_crop_summary.csv")
    cs <- cs[order(cs$SowDate), ]; ref <- ref[order(ref$SowDate), ]
    ok <- nrow(cs) == nrow(ref) && identical(cs$Crop, ref$Crop) && identical(cs$SowDate, ref$SowDate) &&
      identical(cs$HarvestDate, ref$HarvestDate) && within_pct(cs$Yield_kgha, ref$Yield_kgha)
    list(ok, paste0("crops ", paste(sprintf("%s %s %.0f kg/ha", cs$Crop, substr(cs$HarvestDate, 1, 4), cs$Yield_kgha),
                                     collapse = ", "),
                    if (ok) " (match the reference)" else " (DIFFER from reference_results/T08_five_year_crop_summary.csv)"), r)
  },

  T09 = {
    r <- render("rSpacing_cookbook_v1.00_EN.Rmd", "T09_cookbook.html",
                list(workspace_root = "..", stop_on_failed_check = TRUE))
    chk <- utils::read.csv(file.path(root, "results", "cookbook_v100", "cookbook_checks.csv"))
    list(all(chk$passed) && nrow(chk) >= 57, sprintf("%d of %d checks passed", sum(chk$passed), nrow(chk)), r)
  },

  T10 = {
    r <- render("rSpacing_scenario_comparison_v1.00_EN.Rmd", "T10_scenario_comparison.html",
                list(workspace_root = "..", rerun = TRUE))
    ind <- utils::read.csv(file.path(root, "results", "scenario_comparison_v100", "scenario_indicators.csv"))
    ref <- read_ref("T10_scenario_indicators.csv")
    cols <- setdiff(intersect(names(ref), names(ind)), "scenario")
    ok <- identical(ind$scenario, ref$scenario) &&
      all(vapply(cols, function(cc) within_pct(ind[[cc]], ref[[cc]]), logical(1)))
    list(ok, sprintf("%d scenarios run; indicators %s the reference", nrow(ind), if (ok) "match" else "DIFFER from"), r)
  },

  T11 = , T13 = {
    full <- id == "T13"; traj <- if (full) 10 else 2
    sub <- if (full) "morris_v118_full" else "morris_v118_smoke"
    set_aside(paste0("runs/m118_", traj, "t"), file.path("results", sub))
    r <- render("rSpacing_APSIMx_Morris_multivariate_yield_biomass_all_cultivars_v1.18_EN.Rmd",
                if (full) "T13_morris_full_v1.18.html" else "T11_morris_smoke_v1.18.html",
                list(workspace_root = "..", analysis_profile = if (full) "full" else "smoke",
                     trajectories = traj, levels = 4, grid_jump = 2, output_subdir = sub))
    res_dir <- file.path(root, "results", sub)
    pre <- utils::read.csv(latest("^parameter_preflight_audit[.]csv$", res_dir))
    runs <- utils::read.csv(latest("^multivariate_runs[.]csv$", res_dir))
    rank <- utils::read.csv(latest("^morris_multivariate_global_ranking[.]csv$", res_dir))
    expected_runs <- traj * (sum(pre$accepted) + 1)
    ok <- sum(pre$accepted) == 24 && nrow(runs) == expected_runs && all(runs$model_success)
    top <- paste(head(paste(rank$crop, rank$role), 3), collapse = "; ")
    if (full) top <- paste0(top, " (reference: ", paste(head(paste(read_ref("T13_morris_global_ranking.csv")$crop,
                                                                      read_ref("T13_morris_global_ranking.csv")$role), 3), collapse = "; "), ")")
    list(ok, sprintf("%d/24 parameters accepted; %d/%d Morris runs successful; top 3: %s",
                     sum(pre$accepted), sum(runs$model_success), expected_runs, top), r)
  },

  T12 = , T14 = {
    full <- id == "T14"; n <- if (full) 500 else 30
    sub <- if (full) "uncertainty_v106_full" else "uncertainty_v106_smoke"
    set_aside(paste0("runs/u106_", n, "n"), file.path("results", sub))
    r <- render("rSpacing_APSIMx_input_uncertainty_environmental_outputs_v1.06_EN.Rmd",
                if (full) "T14_uncertainty_full_v1.06.html" else "T12_uncertainty_smoke_v1.06.html",
                list(workspace_root = "..", analysis_profile = if (full) "full" else "smoke",
                     samples = n, bootstrap_replicates = if (full) 500 else 50, output_subdir = sub))
    res_dir <- file.path(root, "results", sub)
    pre <- utils::read.csv(latest("^input_preflight_audit[.]csv$", res_dir))
    runs <- utils::read.csv(latest("^uncertainty_runs[.]csv$", res_dir))
    ok <- all(pre$accepted) && nrow(runs) == n && all(runs$model_success & runs$outputs_available)
    list(ok, sprintf("%d/%d preflight inputs accepted; %d/%d samples complete",
                     sum(pre$accepted), nrow(pre), sum(runs$model_success & runs$outputs_available), n), r)
  },
  stop("unknown test ", id))
}, error = function(e) list(FALSE, paste("ERROR:", conditionMessage(e)), ""))

record(if (isTRUE(result[[1]])) "PASS" else "FAIL", result[[2]], result[[3]])
quit(status = if (isTRUE(result[[1]])) 0 else 1)
