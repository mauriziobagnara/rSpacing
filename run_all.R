# Run the whole README, unattended: T00 (optional) to T14.
#
# Start it from the workspace root and leave it running (about 2.5 hours with the
# full analyses, about 40 minutes without):
#
#   PowerShell:  & "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" run_all.R
#                (use your R version; add  full=FALSE  to skip T13-T14)
#   RStudio:     open run_all.R and click "Source as Background Job" (Jobs tab),
#                with the workspace root as working directory.
#
# Settings can be changed below or given on the command line as key=value, e.g.
#   Rscript run_all.R full=FALSE apsim_clone="C:/Users/me/Documents/GitHub/APSIMxSpacing"
#
# Each step runs in its own R process. The run stops at the first failed step
# (as TEST_PROTOCOL.md requires) and writes:
#   logs/run_all/<step>.log    console output of every step (readable while the step runs)
#   logs/run_all_summary.csv   one line per step: PASS / FAIL / SKIPPED, time, detail
#   logs/test_status.csv       the usual PASS / FAIL of T05-T14
# Keep the computer awake (Windows: Settings > System > Power > Sleep: Never) while it runs.

SETTINGS <- list(
  apsim_clone   = "",     # path of the APSIMxSpacing clone: refresh apsim/ from it first (T00). "" = use apsim/ as it is
  build         = TRUE,   # T02: build Spacing.Tool2 with dotnet
  install       = TRUE,   # T03: build and install rSpacing (installs missing CRAN packages: needs internet)
  full          = TRUE,   # T13 and T14: full Morris and uncertainty analyses (about 2 hours)
  stop_on_fail  = TRUE    # stop at the first failed step
)
for (arg in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(arg, "=", fixed = TRUE)[[1]]
  if (length(kv) != 2 || !kv[1] %in% names(SETTINGS)) stop("Unknown argument: ", arg, call. = FALSE)
  SETTINGS[[kv[1]]] <- if (is.logical(SETTINGS[[kv[1]]])) as.logical(toupper(kv[2])) else kv[2]
}

source(file.path("tools", "helpers.R"))
root <- ws_check_root()
log_dir <- file.path(root, "logs", "run_all")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
summary_file <- file.path(root, "logs", "run_all_summary.csv")
started_all <- Sys.time()
steps <- data.frame(step = character(), what = character(), status = character(), minutes = numeric(),
                    detail = character(), stringsAsFactors = FALSE)

say <- function(...) cat(format(Sys.time(), "%H:%M:%S"), " ", ..., "\n", sep = "")
record <- function(step, what, status, minutes, detail) {
  steps[nrow(steps) + 1L, ] <<- list(step, what, status, round(as.numeric(minutes), 1), detail)
  utils::write.csv(steps, summary_file, row.names = FALSE)
  say(step, " ", status, " (", round(as.numeric(minutes), 1), " min): ", detail)
}
finish <- function() {
  total <- as.numeric(difftime(Sys.time(), started_all, units = "mins"))
  say("Finished in ", round(total, 1), " min. Summary: ", summary_file)
  print(steps[, c("step", "status", "minutes", "detail")], row.names = FALSE, right = FALSE)
  if (any(steps$status == "FAIL")) {
    say("A step failed: see its log in logs/run_all/ (and logs/tests/ for T05-T14).")
    quit(save = "no", status = 1)
  }
  quit(save = "no", status = 0)
}
# Run a workspace script in a separate R process; output to logs/run_all/<step>.log
run_script <- function(step, script) {
  log <- file.path(log_dir, paste0(step, ".log"))
  status <- ws_run(ws_rscript(), c("--no-save", "--no-restore", file.path(root, script)), log, label = step)
  list(ok = identical(as.integer(status), 0L), log = log)
}
tail_of <- function(log) paste(ws_log_tail(log, 3, 120), collapse = " | ")
step_failed <- function() isTRUE(SETTINGS$stop_on_fail) && any(steps$status == "FAIL")

# processx keeps the logs readable while a step runs and gives a progress line every minute
if (!requireNamespace("processx", quietly = TRUE))
  try(utils::install.packages("processx", repos = "https://cloud.r-project.org"), silent = TRUE)

say("Workspace: ", root)
say("Settings: ", paste(names(SETTINGS), unlist(SETTINGS), sep = "=", collapse = ", "))

# ---- T00: refresh apsim/ from the APSIMxSpacing clone (optional) -----------------------------
if (nzchar(SETTINGS$apsim_clone)) {
  t0 <- Sys.time()
  src <- file.path(SETTINGS$apsim_clone, "apsim")
  log <- file.path(log_dir, "T00.log")
  if (!file.exists(file.path(src, "spacing.sln"))) {
    record("T00", "Copy apsim/ from APSIMxSpacing", "FAIL", 0, paste("spacing.sln not found in", src))
  } else {
    status <- if (.Platform$OS.type == "windows") {
      system2("robocopy", c(shQuote(normalizePath(src)), shQuote(file.path(root, "apsim")),
                            "/MIR", "/XD", "bin", "obj", ".vs", "/NFL", "/NDL", "/NP"), stdout = log, stderr = log)
    } else {
      system2("rsync", c("-a", "--delete", "--exclude", "bin/", "--exclude", "obj/", paste0(src, "/"),
                         file.path(root, "apsim")), stdout = log, stderr = log)
    }
    ok <- if (.Platform$OS.type == "windows") status < 8 else status == 0   # robocopy: 0-7 = success
    record("T00", "Copy apsim/ from APSIMxSpacing", if (ok) "PASS" else "FAIL",
           difftime(Sys.time(), t0, units = "mins"), if (ok) paste("apsim/ refreshed from", src) else tail_of(log))
  }
  if (step_failed()) finish()
} else {
  record("T00", "Copy apsim/ from APSIMxSpacing", "SKIPPED", 0, "apsim/ used as it is (setting apsim_clone empty)")
}

# ---- T01: workspace folders and workflows --------------------------------------------------
t0 <- Sys.time()
for (d in c("workflows", "runs", "results", "logs")) dir.create(file.path(root, d), showWarnings = FALSE)
shipped <- list.files(file.path(root, "rSpacing", "inst", "rmarkdown"), pattern = "[.]Rmd$", full.names = TRUE)
target <- file.path(root, "workflows", basename(shipped))
new <- !file.exists(target)
invisible(file.copy(shipped[new], target[new]))
differs <- basename(shipped)[!new & tools::md5sum(shipped) != tools::md5sum(target)]
record("T01", "Workspace folders and workflows", if (all(file.exists(target))) "PASS" else "FAIL",
       difftime(Sys.time(), t0, units = "mins"),
       paste0(sum(new), " workflows copied, ", sum(!new), " already present",
              if (length(differs)) paste0("; NOTE: edited copies kept in workflows/ (differ from rSpacing/inst/rmarkdown): ",
                                          paste(differs, collapse = ", ")) else ""))
if (step_failed()) finish()

# ---- T02: build Spacing.Tool2 --------------------------------------------------------------
if (isTRUE(SETTINGS$build)) {
  t0 <- Sys.time()
  dotnet <- Sys.which("dotnet")
  project <- file.path(root, "apsim", "src", "Spacing.Tool2", "Spacing.Tool2.csproj")
  if (!nzchar(dotnet) || !file.exists(project)) {
    record("T02", "Build Spacing.Tool2", "FAIL", 0, if (!nzchar(dotnet)) "dotnet not found" else paste("not found:", project))
  } else {
    say("T02 building Spacing.Tool2 (the first build also compiles APSIM: 15-30 minutes; later builds are faster).")
    say("    Progress: logs/dotnet_restore.log, then logs/dotnet_build.log (readable while it runs).")
    # No build servers left running in the background: they would keep the log open and
    # make this step wait after the build has finished.
    Sys.setenv(MSBUILDDISABLENODEREUSE = "1", DOTNET_CLI_USE_MSBUILD_SERVER = "0", UseSharedCompilation = "false")
    no_servers <- c("-nodeReuse:false", "-p:UseSharedCompilation=false")
    s1 <- ws_run(dotnet, c("restore", project, no_servers), file.path(root, "logs", "dotnet_restore.log"),
                 label = "T02 restore")
    s2 <- if (identical(as.integer(s1), 0L))
      ws_run(dotnet, c("build", project, "--configuration", "Debug", "--framework", "net8.0", "--no-restore", no_servers),
             file.path(root, "logs", "dotnet_build.log"), label = "T02 build") else NA
    system2(dotnet, c("build-server", "shutdown"), stdout = FALSE, stderr = FALSE)
    ok <- identical(as.integer(s1), 0L) && identical(as.integer(s2), 0L) && !is.na(ws_tool2_dll(root))
    record("T02", "Build Spacing.Tool2", if (ok) "PASS" else "FAIL", difftime(Sys.time(), t0, units = "mins"),
           if (ok) paste("built:", ws_tool2_dll(root))
           else if (!identical(s1, 0L)) "dotnet restore failed: see logs/dotnet_restore.log"
           else "dotnet build failed: see logs/dotnet_build.log")
  }
  if (step_failed()) finish()
} else {
  record("T02", "Build Spacing.Tool2", "SKIPPED", 0, "setting build = FALSE")
}

# ---- T03: install rSpacing ------------------------------------------------------------------
if (isTRUE(SETTINGS$install)) {
  t0 <- Sys.time()
  say("T03 installing rSpacing...")
  r <- run_script("T03", "install_rspacing.R")
  record("T03", "Install rSpacing", if (r$ok) "PASS" else "FAIL", difftime(Sys.time(), t0, units = "mins"),
         if (r$ok) "rSpacing 0.10.9 installed with the vignette" else tail_of(r$log))
  if (step_failed()) finish()
} else {
  record("T03", "Install rSpacing", "SKIPPED", 0, "setting install = FALSE")
}

# ---- T04: environment check -----------------------------------------------------------------
t0 <- Sys.time()
r <- run_script("T04", "environment_check.R")
record("T04", "Environment check", if (r$ok) "PASS" else "FAIL", difftime(Sys.time(), t0, units = "mins"),
       if (r$ok) "all required checks passed (logs/environment_check.csv)" else tail_of(r$log))
if (step_failed()) finish()

# ---- T05-T14: tests -------------------------------------------------------------------------
source("run_test.R")
ids <- c(sprintf("T%02d", 5:12), if (isTRUE(SETTINGS$full)) c("T13", "T14"))
for (id in ids) {
  if (id %in% c("T13", "T14") && any(steps$status[steps$step %in% sprintf("T%02d", 5:12)] != "PASS")) {
    record(id, TESTS$what[TESTS$id == id], "SKIPPED", 0, "run only after T05-T12 have passed"); next
  }
  t0 <- Sys.time()
  say(id, " ", TESTS$what[TESTS$id == id], " (about ", TESTS$minutes[TESTS$id == id], " min)...")
  ok <- isTRUE(run_test(id, open_report = FALSE))
  res <- test_status(); res <- res[res$test == id, ]
  record(id, TESTS$what[TESTS$id == id], if (ok) "PASS" else "FAIL", difftime(Sys.time(), t0, units = "mins"),
         if (nrow(res)) res$detail else "see logs/tests/")
  if (step_failed() && !id %in% c("T13", "T14")) finish()
}
if (!isTRUE(SETTINGS$full)) {
  record("T13", TESTS$what[TESTS$id == "T13"], "SKIPPED", 0, "setting full = FALSE")
  record("T14", TESTS$what[TESTS$id == "T14"], "SKIPPED", 0, "setting full = FALSE")
}
finish()
