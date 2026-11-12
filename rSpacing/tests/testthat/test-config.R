
test_that("configuration resolves directories and hashes JSON", {
  td <- make_test_repository(); cfg <- spacing_config(td)
  expect_s3_class(cfg, "spacing_config")
  expect_true(dir.exists(cfg$output_dir)); expect_true(dir.exists(cfg$temp_weather_dir))
  expect_match(cfg$config_sha256, "^[0-9a-f]+$")
})

test_that("missing JSON properties fail explicitly", {
  td <- tempfile(); dir.create(td); dir.create(file.path(td,"apsim","src","Spacing.Tool2"), recursive=TRUE)
  dir.create(file.path(td,"apsim"), showWarnings=FALSE)
  jsonlite::write_json(list(OutputDir=file.path(td,"out")), file.path(td,"apsim","spacing.config.json"), auto_unbox=TRUE)
  expect_error(spacing_config(td), class="spacing_config_error")
})


test_that("configuration accepts the apsim directory directly", {
  td <- make_test_repository()
  cfg <- spacing_config(apsim_dir = file.path(td, "apsim"))
  expect_s3_class(cfg, "spacing_config")
  expect_equal(normalizePath(cfg$apsim_dir, winslash = "/"), normalizePath(file.path(td, "apsim"), winslash = "/"))
  expect_equal(normalizePath(cfg$repository, winslash = "/"), normalizePath(td, winslash = "/"))
})
