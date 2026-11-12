
test_that("SQLite reader imports selected tables", {
  db <- tempfile(fileext=".db"); con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(con, "DailyReport", data.frame(Day=1:2, Yield=c(10,20)))
  DBI::dbWriteTable(con, "_Units", data.frame(ColumnHeading="Yield", Units="kg/ha"))
  DBI::dbDisconnect(con)
  x <- read_spacing_datastore(db, c("DailyReport","_Units"))
  expect_named(x, c("DailyReport","_Units")); expect_equal(nrow(x$DailyReport), 2)
  expect_error(read_spacing_datastore(db, "Missing"), class="spacing_schema_error")
})

test_that("output discovery classifies file extensions", {
  td <- make_test_repository(); cfg <- spacing_config(td)
  file.create(file.path(cfg$output_dir,"x.apsimx")); file.create(file.path(cfg$output_dir,"x.db")); file.create(file.path(cfg$output_dir,"daily.csv")); file.create(file.path(cfg$temp_weather_dir,"site.met"))
  x <- discover_spacing_outputs(cfg)
  expect_length(x$apsimx,1); expect_length(x$database,1); expect_length(x$csv,1); expect_length(x$weather,1)
})


make_collect_result <- function(run_id, scenario_id, csv) {
  structure(
    list(
      run = list(
        run_id = run_id,
        request = list(scenario_id = scenario_id)
      ),
      csv = csv
    ),
    class = "spacing_result"
  )
}

test_that("collect_spacing retains zero-row CSV tables", {
  empty <- data.frame(Date = as.Date(character()), Yield = numeric())
  result <- make_collect_result("run-empty", "scenario-empty", list(daily = empty))

  collected <- collect_spacing(result)

  expect_named(collected, "daily")
  expect_s3_class(collected$daily, "data.frame")
  expect_equal(nrow(collected$daily), 0L)
  expect_identical(collected$daily$.run_id, character())
  expect_identical(collected$daily$.scenario_id, character())
})

test_that("collect_spacing adds provenance to non-empty tables", {
  table <- data.frame(Date = as.Date("2020-01-01"), Yield = 100)
  result <- make_collect_result("run-1", "scenario-1", list(daily = table))

  collected <- collect_spacing(list(result))

  expect_equal(nrow(collected$daily), 1L)
  expect_identical(collected$daily$.run_id, "run-1")
  expect_identical(collected$daily$.scenario_id, "scenario-1")
})

test_that("collect_spacing combines different tables and aligns columns", {
  first <- make_collect_result(
    "run-1", "scenario-1",
    list(
      daily = data.frame(Date = as.Date("2020-01-01"), Yield = 100),
      empty = data.frame(Value = numeric())
    )
  )
  second <- make_collect_result(
    "run-2", "scenario-2",
    list(
      daily = data.frame(Date = as.Date("2020-01-02"), Biomass = 250),
      harvest = data.frame(Crop = "Maize", Yield = 9000)
    )
  )

  collected <- collect_spacing(list(first, second))

  expect_named(collected, c("daily", "empty", "harvest"), ignore.order = FALSE)
  expect_equal(nrow(collected$daily), 2L)
  expect_true(all(c("Yield", "Biomass", ".run_id", ".scenario_id") %in%
                  names(collected$daily)))
  expect_equal(collected$daily$.run_id, c("run-1", "run-2"))
  expect_equal(nrow(collected$empty), 0L)
  expect_equal(nrow(collected$harvest), 1L)
  expect_identical(collected$harvest$.scenario_id, "scenario-2")
})

test_that("collect_spacing rejects empty input", {
  expect_error(collect_spacing(list()), "at least one")
})
