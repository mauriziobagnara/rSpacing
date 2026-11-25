test_that("real Tool2 mock run succeeds", {
  skip_if(Sys.getenv("RSPACING_INTEGRATION") != "true")
  repository <- Sys.getenv("RSPACING_REPOSITORY")
  skip_if(repository == "")
  cfg <- spacing_config(repository)
  req <- new_spacing_request(45.4, 9.2, "Maize", mock = TRUE,
                             scenario_id = "ci-mock")
  run <- run_spacing(cfg, req, echo = FALSE)
  expect_equal(run$status, 0L)
  expect_true(length(run$outputs$apsimx) >= 1L)
  expect_true(length(run$outputs$database) >= 1L)
})

test_that("real Tool2 run of a complete scenario (Tool2 >= 0.09.07)", {
  skip_if(Sys.getenv("RSPACING_INTEGRATION") != "true")
  apsim_dir <- Sys.getenv("RSPACING_APSIM_DIR"); dll <- Sys.getenv("RSPACING_TOOL2_DLL")
  skip_if(apsim_dir == "" || dll == "")
  cfg <- spacing_config(apsim_dir = apsim_dir, use_compiled = TRUE, executable = dll)
  layers <- data.frame(DepthTop = c(0, 30), DepthBottom = c(30, 60), BD = c(1.3, 1.4),
                       LL15 = c(.14, .16), DUL = c(.31, .33), SAT = c(.45, .44), OC = c(1.2, .7),
                       PH = c(7.2, 7.4), NO3 = c(20, 8), NH4 = c(2, 1))
  sc <- new_spacing_scenario("ci_scenario",
    list(spacing_crop("Wheat", "Hartog", c("10-oct", "20-nov"), 180, 35, 180,
                      fertilisation = spacing_fertilisation(30), tillage = spacing_tillage("minimum")),
         spacing_crop("Maize", "B_110", c("1-apr", "10-may"), 7.5, 50, 750,
                      fertilisation = spacing_fertilisation(50), organic = spacing_organic_n(25))),
    soil = spacing_soil_profile(layers, "CI"), latitude = 44.9, longitude = 10.63,
    start_year = 2020, end_year = 2022)
  run <- run_spacing(cfg, sc, echo = FALSE)
  expect_true(isTRUE(run$result$success))
  expect_setequal(names(run$result$yields), c("Wheat", "Maize"))
})

test_that("real Tool2 run with management events (Tool2 >= 0.09.09)", {
  skip_if(Sys.getenv("RSPACING_INTEGRATION") != "true")
  apsim_dir <- Sys.getenv("RSPACING_APSIM_DIR"); dll <- Sys.getenv("RSPACING_TOOL2_DLL")
  skip_if(apsim_dir == "" || dll == "")
  cfg <- spacing_config(apsim_dir = apsim_dir, use_compiled = TRUE, executable = dll)
  wheat <- spacing_crop("Wheat", "Hartog", c("10-oct", "20-nov"), 180, 35, 180,
    fertilisation = spacing_fertilisation(30, list(
      spacing_fertiliser_application(60, "NH4NO3N", trigger = "Date", date = "1-mar"),
      spacing_fertiliser_application(40, "UAN_N", trigger = "Stage", stage = "FlagLeafAppearance", depth_mm = 50))))
  maize <- spacing_crop("Maize", "B_110", c("1-apr", "10-may"), 7.5, 50, 750,
    fertilisation = spacing_fertilisation(50, fertiliser = "DAP", depth_mm = 50),
    organic_applications = list(spacing_organic_application(40, "cm", 10, "DaysAfterPreviousHarvest", days = 30)),
    tillage_operations = list(spacing_tillage_operation("conventional", "Date", date = "15-nov"),
                              spacing_tillage_operation("minimum")))
  sc <- new_spacing_scenario("ci_events", list(wheat, maize), latitude = 44.9, longitude = 10.63,
                             start_year = 2020, end_year = 2022,
                             surface_residue = spacing_surface_residue("maize", 3000, 60))
  run <- run_spacing(cfg, sc, echo = FALSE)
  expect_true(isTRUE(run$result$success))
  daily <- utils::read.csv(run$result$tradeoffs$daily_csv_path)
  expect_true("FertMineralN_kgNha" %in% names(daily))
  expect_equal(sum(daily$FertMineralN_kgNha), sum(c(30, 60, 40, 50)), tolerance = 1e-6)
})

test_that("real Tool2 run with rotation options (Tool2 >= 0.09.10)", {
  skip_if(Sys.getenv("RSPACING_INTEGRATION") != "true")
  apsim_dir <- Sys.getenv("RSPACING_APSIM_DIR"); dll <- Sys.getenv("RSPACING_TOOL2_DLL")
  skip_if(apsim_dir == "" || dll == "")
  cfg <- spacing_config(apsim_dir = apsim_dir, use_compiled = TRUE, executable = dll)
  w1 <- spacing_crop("Wheat", "Hartog", c("10-oct", "20-nov"), 180, 35, 180,
                     fertilisation = spacing_fertilisation(30), residue_removal = spacing_residue_removal(.5))
  oats <- spacing_crop("Oats", "Drummond", c("1-aug", "30-sep"), 250, 30, 180, verify_cultivar = FALSE,
                       tillage = spacing_tillage("no-till"), termination = spacing_termination("Date", date = "15-mar"))
  maize <- spacing_crop("Maize", "B_110", c("1-apr", "10-may"), 7.5, 50, 750,
                        fertilisation = spacing_fertilisation(50),
                        sowing_rule = spacing_sowing_rule(min_soil_temp_c = 12, temp_days = 5, sow_at_window_end = TRUE),
                        irrigation_schedule = spacing_irrigation_event(40, "Stage", stage = "Flowering"))
  w2 <- spacing_crop("Wheat", "Hartog", c("10-oct", "20-nov"), 220, 35, 180, fertilisation = spacing_fertilisation(60))
  sc <- new_spacing_scenario("ci_options", list(w1, oats, maize, w2), latitude = 44.9, longitude = 10.63,
                             start_year = 2020, end_year = 2023,
                             report = spacing_report_spec(daily_variables = "[Weather].MeanT as Tmean"))
  run <- run_spacing(cfg, sc, echo = FALSE)
  expect_true(isTRUE(run$result$success))
  rep <- utils::read.csv(run$result$tradeoffs$control_csv_path)
  expect_equal(rep$HarvestedCrop, c("Wheat", "Oats", "Maize", "Wheat"))
  expect_equal(rep$RotationSlot, 1:4)
  expect_equal(rep$EndReason[2], "terminated")
  daily <- utils::read.csv(run$result$tradeoffs$daily_csv_path)
  expect_true("Tmean" %in% names(daily)); expect_true(sum(daily$ResidueRemoved_kgha) > 0)
  expect_equal(sum(daily$Irrig_mm), 40)
})
