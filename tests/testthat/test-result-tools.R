make_spacing_result <- function(weather = NULL) {
  daily <- data.frame(
    Year = rep(2000:2001, each = 3),
    DOY = rep(1:3, 2),
    Rain_mm = c(1, 2, 0, 3, 0, 4),
    SoilSWtot_mm = c(100, 101, 99, 102, 103, 101),
    Maize_LAI = c(0, 1, 2, 0, 1.5, 2.5)
  )
  report <- data.frame(
    Year = 2000:2001,
    Maize_GrainYield_kgha = c(7000, 8000),
    Maize_LAI = c(2, 2.5)
  )
  units <- data.frame(
    TableName = c("DailyReport", "Report"),
    ColumnHeading = c("Rain_mm", "Maize_LAI"),
    Units = c("mm", "m^2/m^2")
  )
  structure(list(
    run = structure(list(run_id="test", request=list(scenario_id="test")), class="spacing_run"),
    datastore = list(),
    csv = list(
      crop_summary_test.csv = data.frame(Crop=character(), Yield_kgha=numeric()),
      daily_test.csv = daily,
      report_test.csv = report
    ),
    apsimx = character(), weather = weather,
    messages = list(), units = list(test.db = units)
  ), class = "spacing_result")
}

test_that("standard result tables are found and empty tables are explicit", {
  x <- make_spacing_result()
  expect_equal(nrow(spacing_daily(x)), 6)
  expect_equal(nrow(spacing_report(x)), 2)
  expect_equal(nrow(spacing_crop_summary(x)), 0)
  expect_error(spacing_crop_summary(x, allow_empty=FALSE), class="spacing_empty_table_error")
})

test_that("units, classification, and inventory are available", {
  x <- make_spacing_result()
  expect_equal(spacing_variable_unit(x, "Rain_mm", "DailyReport"), "mm")
  cls <- classify_spacing_variables(c("Rain_mm", "CO2C_total_kgCha", "Maize_GrainWt_kgha"))
  expect_equal(cls$group, c("weather", "carbon_flux", "crop"))
  inv <- inventory_spacing_result(x)
  expect_true(any(inv$table == "crop_summary_test.csv" & inv$empty))
  expect_true(any(inv$table == "daily_test.csv" & inv$has_doy))
})

test_that("daily and annual plotting returns ggplot objects", {
  x <- make_spacing_result()
  expect_s3_class(plot_spacing_daily(x, "SoilSWtot_mm", x="doy"), "ggplot")
  expect_s3_class(plot_spacing_daily(x, "Rain_mm", x="year", summary="sum"), "ggplot")
  expect_s3_class(plot_spacing_report(x, "Maize_GrainYield_kgha"), "ggplot")
  expect_s3_class(plot_spacing_result(x, "daily", "Maize_LAI", panel=FALSE, x="doy"), "ggplot")
})

test_that("weather plot accepts spacing_result", {
  path <- system.file("extdata", "minimal.met", package="rSpacing")
  skip_if(!nzchar(path))
  x <- make_spacing_result(path)
  expect_s3_class(plot_spacing_met(x), "ggplot")
  expect_s3_class(plot(x, source="weather"), "ggplot")
})

test_that("multi-figure panels are returned for daily and report variables", {
  x <- make_spacing_result()
  daily_panel <- plot_spacing_daily_panel(x, "SoilSWtot_mm")
  report_panel <- plot_spacing_report_panel(x, "Maize_GrainYield_kgha")
  expect_s3_class(daily_panel, "patchwork")
  expect_s3_class(report_panel, "patchwork")
  expect_s3_class(
    plot_spacing_result(x, "daily", "Maize_LAI"),
    "patchwork"
  )
  expect_s3_class(
    plot_spacing_result(x, "daily", "Maize_LAI", panel = FALSE, x = "doy"),
    "ggplot"
  )
})

test_that("multiple panels are created as a named list", {
  x <- make_spacing_result()
  panels <- plot_spacing_panels(
    x,
    source = "daily",
    variables = c("SoilSWtot_mm", "Maize_LAI")
  )
  expect_named(panels, c("SoilSWtot_mm", "Maize_LAI"))
  expect_true(all(vapply(panels, inherits, logical(1), what = "patchwork")))
})
