# rSpacing 0.10.09: rotation options of Spacing.Tool2 0.09.10

wheat_crop <- function(...) spacing_crop("Wheat", "Hartog", c("10-oct", "20-nov"), 180, 35, 180, ...)

test_that("option constructors validate their arguments", {
  r <- spacing_sowing_rule(min_esw_mm = 40, min_rain_mm = 5, rain_days = 5, min_soil_temp_c = 12, sow_at_window_end = TRUE)
  expect_equal(r$rain_days, 5L); expect_true(r$sow_at_window_end)
  expect_error(spacing_sowing_rule(rain_days = 0))
  expect_error(spacing_sowing_rule(min_esw_mm = -1))
  expect_equal(spacing_irrigation_event(30, "Date", date = "15-JUL")$date, "15-jul")
  expect_equal(spacing_irrigation_event(30, "DaysAfterSowing", days = 40)$days, 40L)
  expect_error(spacing_irrigation_event(0, "Date", date = "1-jul"))
  expect_error(spacing_irrigation_event(30, "Stage"), "stage")
  expect_error(spacing_termination("DaysAfterSowing"), "days")
  expect_error(spacing_termination("DaysAfterSowing", days = 0))
  expect_equal(spacing_perennial(3, "15-sep")$years, 3L)
  expect_error(spacing_perennial(0, "15-sep"))
  expect_error(spacing_residue_removal(1.5))
  expect_equal(spacing_residue_removal(.5, "burn")$method, "burn")
  expect_error(spacing_report_spec(daily_event = "EndOfDay"), "Clock")
  expect_error(spacing_report_spec(daily_variables = c("[A].x as X", "[B].y as X")), "repeated")
  expect_equal(spacing_fertiliser_application(20, trigger = "DaysAfterPreviousHarvest", days_after_sowing = 30)$days_after_sowing, 30L)
})

test_that("crops check intercrops, stages and option combinations", {
  expect_error(wheat_crop(intercrop = spacing_intercrop("Wheat", "Hartog", 100, 30, 180)), "different")
  expect_error(wheat_crop(termination = spacing_termination("Date", date = "1-mar"),
                          perennial = spacing_perennial(2, "1-jul")), "together")
  expect_error(wheat_crop(termination = spacing_termination("Stage", stage = "Tasseling")), "not an APSIM stage")
  cr <- wheat_crop(irrigation_schedule = spacing_irrigation_event(30, "Stage", stage = "anthesis"))
  expect_equal(cr$irrigation_schedule[[1]]$stage, "Anthesis")
  expect_error(spacing_crop("Maize", "B_110", NULL, 7.5, 50, 750), "sowing_date")
  expect_s3_class(spacing_crop("Maize", "B_110", NULL, 7.5, 50, 750, sowing_date = "20-apr"), "spacing_crop")
  expect_warning(spacing_crop("Maize", "B_110", c("1-apr", "1-jun"), 7.5, 50, 750, sowing_date = "20-apr",
                              sowing_rule = spacing_sowing_rule(min_rain_mm = 5)), "ignored")
})

test_that("rotation options reach the Tool2 request", {
  w1 <- wheat_crop(residue_removal = spacing_residue_removal(.7))
  oats <- spacing_crop("Oats", "Drummond", c("1-aug", "30-sep"), 250, 30, 180, verify_cultivar = FALSE,
                       tillage = spacing_tillage("no-till"), termination = spacing_termination("Date", date = "15-mar"))
  maize <- spacing_crop("Maize", "B_110", c("1-apr", "10-may"), 7.5, 50, 750,
                        sowing_rule = spacing_sowing_rule(min_esw_mm = 40, min_soil_temp_c = 12, sow_at_window_end = TRUE),
                        irrigation_schedule = list(spacing_irrigation_event(30, "DaysAfterSowing", days = 40),
                                                   spacing_irrigation_event(40, "Stage", stage = "Flowering")))
  w2 <- wheat_crop(parameter_overrides = c("Leaf.Photosynthesis.RUE.FixedValue" = 1.3))
  soy <- spacing_crop("Soybean", "Generic_MG1", NULL, 35, 35, 450, sowing_date = "10-may",
                      intercrop = spacing_intercrop("Maize", "B_110", 3.5, 50, 750))
  vine <- spacing_crop("Grapevine", "SauvignonBlanc", c("1-jul", "31-jul"), .4, 300, 2500, verify_cultivar = FALSE,
                       perennial = spacing_perennial(3, "15-sep"))
  sc <- new_spacing_scenario("opts", list(w1, oats, maize, w2, soy, vine), latitude = 45, longitude = 10,
                             start_year = 2020, end_year = 2030,
                             report = spacing_report_spec(harvest_variables = "[Wheat].Grain.N * 10 as Wheat_GrainN_kgha",
                                                          daily_exclude = "VP_hPa"))
  out <- capture.output(print(sc))
  expect_true(any(grepl("cover", out))); expect_true(any(grepl("\\+Maize", out))); expect_true(any(grepl("perennial 3y", out)))
  req <- jsonlite::read_json(write_spacing_scenario(sc, tempfile(fileext = ".json")))
  expect_equal(vapply(req$crops, `[[`, "", "name"), c("Wheat", "Oats", "Maize", "Wheat", "Soybean", "Grapevine"))
  expect_equal(req$crops[[1]]$residue_removal$fraction, .7)
  expect_equal(req$crops[[2]]$termination$date, "15-mar")
  expect_equal(req$crops[[3]]$management$sowing_rule$min_soil_temp_c, 12)
  expect_true(req$crops[[3]]$management$sowing_rule$sow_at_window_end)
  expect_equal(req$crops[[3]]$management$irrigation_schedule[[2]]$stage, "Flowering")
  expect_equal(req$crops[[4]]$parameter_overrides$Leaf.Photosynthesis.RUE.FixedValue, 1.3)
  expect_equal(req$crops[[5]]$management$sowing_date, "10-may")
  expect_null(req$crops[[5]]$management$sowing_window_start)
  expect_equal(req$crops[[5]]$intercrop[[1]]$name, "Maize")
  expect_equal(req$crops[[5]]$intercrop[[1]]$sowing_density, 3.5)
  expect_equal(req$crops[[6]]$perennial$harvest_date, "15-sep")
  expect_equal(unlist(req$report$harvest_variables), "[Wheat].Grain.N * 10 as Wheat_GrainN_kgha")
  expect_equal(unlist(req$report$daily_exclude), "VP_hPa")
  expect_null(req$crops[[1]]$intercrop); expect_null(req$crops[[1]]$management$sowing_rule)
})
