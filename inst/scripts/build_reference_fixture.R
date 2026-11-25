# Generate a new APSIM reference fixture like the one used by the rSpacing vignette
# (inst/extdata/reference_maize/): a single-crop mock Maize run of Spacing.Tool2.
#
# Run from the workspace root (the folder with rSpacing/ and apsim/), after
# build_tool2.ps1 and install_rspacing.R:
#   source("rSpacing/inst/scripts/build_reference_fixture.R")
# The fixture is written to rSpacing/inst/extdata/reference_run/. To use it in the
# vignette, review it and then replace inst/extdata/reference_maize/ with it.

spacing_root <- normalizePath(Sys.getenv("RSPACING_ROOT", "apsim"), winslash = "/", mustWork = TRUE)
package_source <- normalizePath(Sys.getenv("RSPACING_PACKAGE_SOURCE", "rSpacing"), winslash = "/", mustWork = TRUE)
dll <- file.path(spacing_root, "src", "Spacing.Tool2", "bin", c("Debug", "Release"), "net8.0", "spacing-tool2.dll")
dll <- dll[file.exists(dll)][1]
if (is.na(dll)) stop("spacing-tool2.dll not found: build Spacing.Tool2 first (build_tool2.ps1).")
fixture_dir <- file.path(package_source, "inst", "extdata", "reference_run")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

library(rSpacing)

cfg <- spacing_config(apsim_dir = spacing_root, use_compiled = TRUE, executable = dll,
                      run_root = file.path(tempdir(), "reference_fixture_runs"))
validate_spacing_config(cfg, check_dotnet = TRUE)

request <- new_spacing_request(
  latitude = 44.9,
  longitude = 10.63,
  rotation = "Maize",
  mock = TRUE,
  scenario_id = "vignette_reference_maize"
)
validate_spacing_request(request)

run <- run_spacing(cfg, request, error_on_status = TRUE, echo = TRUE)
result <- read_spacing(run)
stopifnot(inherits(result, "spacing_result"))

saveRDS(result, file.path(fixture_dir, "reference_result.rds"), version = 3)
jsonlite::write_json(
  unclass(request),
  file.path(fixture_dir, "request.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)
jsonlite::write_json(
  unclass(run),
  file.path(fixture_dir, "run_manifest.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

for (entry in names(result$csv)) {
  if (is.data.frame(result$csv[[entry]])) {
    utils::write.csv(
      result$csv[[entry]],
      file.path(fixture_dir, paste0("table_", gsub("[^A-Za-z0-9._-]", "_", entry), ".csv")),
      row.names = FALSE, na = ""
    )
  }
}

files <- list.files(fixture_dir, full.names = TRUE)
files <- files[basename(files) != "SHA256SUMS.csv"]
checksums <- data.frame(
  file = basename(files),
  bytes = file.info(files)$size,
  md5 = unname(tools::md5sum(files)),
  stringsAsFactors = FALSE
)
utils::write.csv(checksums, file.path(fixture_dir, "SHA256SUMS.csv"), row.names = FALSE)

message("Reference fixture written to: ", normalizePath(fixture_dir, winslash = "/"))
