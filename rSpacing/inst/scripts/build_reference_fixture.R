# Generate a real APSIMx reference fixture for the rSpacing vignette.
# Run only on the verified Windows workstation with Spacing.Tool2 v0.09.05.

spacing_root <- Sys.getenv(
  "RSPACING_ROOT",
  "C:/Users/maurizio.bagnara/Documents/spacing-main/apsim"
)
package_source <- Sys.getenv("RSPACING_PACKAGE_SOURCE", unset = getwd())
fixture_dir <- file.path(package_source, "inst", "extdata", "reference_run")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

library(rSpacing)

cfg <- spacing_config(spacing_root)
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
