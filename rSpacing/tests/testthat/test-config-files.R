test_that("configuration values validate core fields", {
  x <- new_spacing_config_values(
    "http://localhost:8000/api/v1", "",
    tempfile("weather-"), tempfile("output-"),
    1990, 2023, 4
  )
  expect_s3_class(x, "spacing_config_values")
  expect_equal(x$StartYear, 1990L)
  expect_error(
    new_spacing_config_values("", "", "w", "o", 2024, 2023, 1),
    class = "spacing_config_error"
  )
})

test_that("write and read preserve standard and custom properties", {
  td <- tempfile(); dir.create(td)
  x <- new_spacing_config_values(
    "http://localhost", "secret",
    file.path(td, "weather"), file.path(td, "output"),
    2000, 2010, 2,
    additional = list(CustomMode = "research")
  )
  path <- file.path(td, "spacing.config.json")
  write_spacing_config_file(x, path)
  expect_true(file.exists(path))
  expect_true(dir.exists(file.path(td, "weather")))
  y <- read_spacing_config_file(path)
  expect_equal(y$CustomMode, "research")
  expect_equal(y$DbApiBaseUrl, "http://localhost")
  expect_equal(y$DbApiKey, "secret")
  expect_equal(y$StartYear, 2000L)
  expect_equal(y$EndYear, 2010L)
  expect_equal(y$MaxParallelism, 2L)
  expect_true(all(c(
    "DbApiBaseUrl", "DbApiKey", "TempWeatherDir", "OutputDir",
    "StartYear", "EndYear", "MaxParallelism"
  ) %in% names(y)))
})

test_that("update preserves unknown properties and makes backup", {
  td <- tempfile(); dir.create(td)
  x <- new_spacing_config_values(
    "http://localhost", "", file.path(td,"w"), file.path(td,"o"),
    1990, 2000, additional = list(CustomMode="keep")
  )
  path <- file.path(td,"spacing.config.json")
  write_spacing_config_file(x,path)
  update_spacing_config_file(path, EndYear=2010L)
  y <- read_spacing_config_file(path)
  expect_equal(y$EndYear,2010L)
  expect_equal(y$CustomMode,"keep")
  expect_true(length(Sys.glob(paste0(path,".*.bak"))) == 1L)
})

test_that("scenario set creates isolated directories", {
  td <- tempfile(); dir.create(td)
  base <- new_spacing_config_values(
    "http://localhost", "", file.path(td,"w"), file.path(td,"o"),
    1990, 2000, 1
  )
  scenarios <- data.frame(
    scenario_id=c("maize base","wheat-highN"),
    StartYear=c(1991L,1992L),
    stringsAsFactors=FALSE
  )
  map <- prepare_spacing_config_set(base, scenarios, file.path(td,"sets"))
  expect_equal(nrow(map),2L)
  expect_true(all(file.exists(map$config_file)))
  expect_false(identical(map$output_dir[[1]],map$output_dir[[2]]))
  expect_equal(read_spacing_config_file(map$config_file[[1]])$StartYear,1991L)
})


test_that("scenario set preserves required base properties with partial overrides", {
  td <- tempfile(); dir.create(td)
  base <- new_spacing_config_values(
    db_api_base_url = "http://localhost:8000/api/v1",
    db_api_key = "",
    temp_weather_dir = file.path(td, "weather"),
    output_dir = file.path(td, "output"),
    start_year = 2020,
    end_year = 2024,
    max_parallelism = 1L
  )
  scenarios <- data.frame(
    scenario_id = c("baseline", "parallel2"),
    MaxParallelism = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  map <- prepare_spacing_config_set(
    base, scenarios, file.path(td, "config-set"), overwrite = TRUE
  )
  first <- read_spacing_config_file(map$config_file[[1]])
  second <- read_spacing_config_file(map$config_file[[2]])
  expect_equal(first$DbApiBaseUrl, "http://localhost:8000/api/v1")
  expect_equal(first$DbApiKey, "")
  expect_equal(first$StartYear, 2020L)
  expect_equal(first$EndYear, 2024L)
  expect_equal(first$MaxParallelism, 1L)
  expect_equal(second$MaxParallelism, 2L)
})
