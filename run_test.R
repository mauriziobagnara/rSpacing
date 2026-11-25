# Run one test of TEST_PROTOCOL.md (steps T05 to T14)
#
# In RStudio, with the workspace root as working directory:
#   source("run_test.R")
#   run_test("T05")          # then T06, T07, ... in order
#   test_status()            # PASS / FAIL table of the tests run so far
#
# Each test runs in a separate R process, so the RStudio session is not affected.
# Console output goes to logs/tests/<ID>.log, rendered reports to
# results/test_results/, and the result to logs/test_status.csv.
# Outputs of an earlier run of the same test are moved (not deleted) to
# runs/_previous/<date-time>/ and results/_previous/<date-time>/.

source(file.path("tools", "helpers.R"))

TESTS <- data.frame(
  id = sprintf("T%02d", 5:14),
  what = c("rSpacing tests and R CMD check",
           "Operational manual, no model run",
           "Operational manual, with one model run",
           "Five-year standard rotation report",
           "Cookbook (all scenario options, self-checking)",
           "Scenario comparison (yield, soil C, leaching, GHG)",
           "Morris smoke test (2 trajectories)",
           "Input-uncertainty smoke test (30 samples)",
           "Morris full analysis (10 trajectories)",
           "Input-uncertainty full analysis (500 samples)"),
  minutes = c("5", "1", "2", "1", "2", "3", "10", "8", "45", "75"),
  stringsAsFactors = FALSE)

run_test <- function(id, open_report = interactive()) {
  root <- ws_check_root()
  id <- toupper(id)
  if (!id %in% TESTS$id) stop("Unknown test ", id, ". Available: ", paste(TESTS$id, collapse = ", "), call. = FALSE)
  if (!ws_find_pandoc()) stop("pandoc not found: run from RStudio.", call. = FALSE)
  log_dir <- file.path(root, "logs", "tests"); dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(log_dir, paste0(id, ".log"))
  row <- TESTS[TESTS$id == id, ]
  message(id, ": ", row$what, " (about ", row$minutes, " min). Log: ", log_file)
  started <- Sys.time()
  status <- system2(ws_rscript(), c("--no-save", "--no-restore", shQuote(file.path(root, "tools", "run_step.R")),
                                    id, shQuote(root)),
                    stdout = log_file, stderr = log_file)   # the child inherits RSTUDIO_PANDOC
  minutes <- as.numeric(difftime(Sys.time(), started, units = "mins"))
  res <- test_status()
  res <- res[res$test == id, , drop = FALSE]
  if (!nrow(res) || status != 0) {
    message(id, " FAIL after ", round(minutes, 1), " min. Last lines of the log:")
    cat(utils::tail(readLines(log_file, warn = FALSE), 25), sep = "\n")
    return(invisible(FALSE))
  }
  message(id, " ", res$status, " (", round(minutes, 1), " min): ", res$detail)
  report <- res$report
  if (isTRUE(open_report) && !is.na(report) && nzchar(report) && file.exists(report)) utils::browseURL(report)
  invisible(res$status == "PASS")
}

# Latest result of each test; test_status(all = TRUE) shows every run.
test_status <- function(all = FALSE) {
  f <- file.path(ws_root(), "logs", "test_status.csv")
  if (!file.exists(f)) return(data.frame(test = character(), status = character(), detail = character(),
                                          report = character(), finished = character()))
  x <- utils::read.csv(f, stringsAsFactors = FALSE)
  if (!all) x <- x[!duplicated(x$test, fromLast = TRUE), , drop = FALSE]
  x[order(x$test), , drop = FALSE]
}

message("Tests available: run_test(\"T05\") ... run_test(\"T14\"). See TEST_PROTOCOL.md.")
