test_that("weather object adds year and day and calculates constants", {
  met <- new_spacing_met(
    make_test_met_data(), latitude = 45.4, longitude = 9.2,
    site = "Test site"
  )
  expect_s3_class(met, "spacing_met")
  expect_named(met$data, c("year", "day", "radn", "maxt", "mint", "rain"))
  expect_true(is.finite(met$constants$tav$value))
  expect_true(is.finite(met$constants$amp$value))
})

test_that("MET round trip preserves data, units and custom constants", {
  td <- tempfile(); dir.create(td)
  met <- new_spacing_met(
    make_test_met_data(), 45.4,
    constants = list(source = list(value = "synthetic", comment = "test"))
  )
  path <- write_spacing_met(met, file.path(td, "site.met"))
  read_back <- read_spacing_met(path)
  expect_equal(nrow(read_back$data), nrow(met$data))
  expect_equal(names(read_back$data), names(met$data))
  expect_equal(read_back$units, met$units)
  expect_equal(read_back$constants$source$value, "synthetic")
})

test_that("validation detects weather errors", {
  bad <- make_test_met_data()
  bad$maxt[[3]] <- 5
  expect_error(new_spacing_met(bad, 45.4),
               class = "spacing_met_validation_error")
  gap <- make_test_met_data(); gap <- gap[-3, ]
  expect_error(new_spacing_met(gap, 45.4),
               class = "spacing_met_validation_error")
})

test_that("columns, constants and subsets can be modified", {
  met <- new_spacing_met(make_test_met_data(), 45.4)
  met <- set_spacing_met_column(met, "wind", 2.5, "m/s")
  expect_equal(met$data$wind, rep(2.5, 10))
  expect_equal(met$units[["wind"]], "(m/s)")
  met <- set_spacing_met_constant(met, "co2", 420, "ppm")
  expect_equal(met$constants$co2$value, 420)
  short <- subset_spacing_met(met, "2000-01-03", "2000-01-05")
  expect_equal(nrow(short$data), 3L)
  met <- remove_spacing_met_column(met, "wind")
  expect_false("wind" %in% names(met$data))
})

test_that("raw unit conversions are applied", {
  raw <- make_test_met_data()
  raw$radn <- raw$radn * 1e6
  raw$maxt <- raw$maxt + 273.15
  raw$mint <- raw$mint + 273.15
  met <- prepare_spacing_met(
    raw, latitude = 45.4,
    conversions = list(
      radn = list(multiplier = 1e-6),
      maxt = list(offset = -273.15),
      mint = list(offset = -273.15)
    )
  )
  expect_equal(met$data$radn[[1]], 12)
  expect_equal(met$data$maxt[[1]], 20, tolerance = 1e-10)
})

test_that("APSIMX Weather FileName is updated in a copy", {
  td <- tempfile(); dir.create(td)
  met <- new_spacing_met(make_test_met_data(), 45.4)
  met_path <- write_spacing_met(met, file.path(td, "site.met"))
  apsimx <- list(
    `$type` = "Models.Core.Simulations, Models",
    Children = list(list(
      `$type` = "Models.Climate.Weather, Models",
      Name = "Weather", FileName = "old.met"
    ))
  )
  input <- file.path(td, "model.apsimx")
  jsonlite::write_json(apsimx, input, auto_unbox = TRUE, pretty = TRUE)
  output <- set_apsimx_weather_file(input, met_path)
  changed <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  expect_equal(changed$Children[[1]]$FileName, met_path)
})

test_that("meteorological plot is a four-panel ggplot", {
  skip_if_not_installed("ggplot2")
  met <- new_spacing_met(make_test_met_data(), 45.4, site = "Test site")
  p <- plot_spacing_met(met)
  expect_s3_class(p, "ggplot")
  expect_equal(length(levels(p$data$variable)), 4L)
  expect_s3_class(plot(met), "ggplot")
})

test_that("MET constants are read case-insensitively", {
  src <- system.file("extdata", "minimal.met", package = "rSpacing")
  skip_if(!nzchar(src))
  lines <- readLines(src, warn = FALSE)
  lines <- sub("^latitude\\s*=", "Latitude =", lines, ignore.case = TRUE)
  lines <- sub("^tav\\s*=", "TAV =", lines, ignore.case = TRUE)
  lines <- sub("^amp\\s*=", "AMP =", lines, ignore.case = TRUE)
  path <- tempfile(fileext = ".met")
  writeLines(lines, path)
  met <- read_spacing_met(path)
  expect_true(all(c("latitude", "tav", "amp") %in% names(met$constants)))
  expect_s3_class(plot_spacing_met(met), "ggplot")
})
