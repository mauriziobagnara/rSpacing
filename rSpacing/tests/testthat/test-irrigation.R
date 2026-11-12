testthat::test_that("spacing_irrigation creates a JSON-ready active strategy", {
  irrigation <- spacing_irrigation(
    threshold = 0.50,
    target = 0.85,
    max_application_mm = 35,
    window_start = "1-jun",
    window_end = "31-aug",
    label = "Maize B_110"
  )

  testthat::expect_s3_class(irrigation, "spacing_irrigation")
  testthat::expect_equal(
    as.list(irrigation),
    list(
      auto_irrigate = TRUE,
      irrigation_threshold = 0.50,
      irrigation_target = 0.85,
      max_irrigation_mm = 35,
      irrigation_window_start = "1-jun",
      irrigation_window_end = "31-aug"
    )
  )
})

testthat::test_that("disabled irrigation emits only auto_irrigate", {
  irrigation <- spacing_irrigation(enabled = FALSE, label = "Rainfed")
  testthat::expect_equal(as.list(irrigation), list(auto_irrigate = FALSE))
})

testthat::test_that("irrigation can be merged into management", {
  management <- list(cultivar = "Hartog", sowing_density = 180)
  irrigation <- spacing_irrigation(
    threshold = 0.35,
    target = 0.70,
    max_application_mm = 25,
    window_start = "01/03",
    window_end = "31/05"
  )
  combined <- spacing_apply_irrigation(management, irrigation)

  testthat::expect_equal(combined$cultivar, "Hartog")
  testthat::expect_true(combined$auto_irrigate)
  testthat::expect_equal(combined$irrigation_window_start, "1-mar")
  testthat::expect_equal(combined$irrigation_window_end, "31-may")
})

testthat::test_that("invalid strategies are rejected", {
  testthat::expect_error(
    spacing_irrigation(threshold = 0.8, target = 0.7),
    "greater than"
  )
  testthat::expect_error(
    spacing_irrigation(window_start = "1-jun", window_end = NULL),
    "both"
  )
  testthat::expect_error(
    spacing_irrigation(max_application_mm = 0),
    "must be in"
  )
  testthat::expect_error(
    spacing_irrigation(window_start = "31-feb", window_end = "1-mar"),
    "Invalid day"
  )
})
