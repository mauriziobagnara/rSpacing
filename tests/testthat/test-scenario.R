demo_layers <- function() {
  data.frame(DepthTop = c(0, 15, 30), DepthBottom = c(15, 30, 60),
             BD = c(1.28, 1.33, 1.39), LL15 = c(.14, .15, .16), DUL = c(.31, .32, .33),
             SAT = c(.46, .45, .44), SW = c(.28, .29, .30), OC = c(1.45, 1.15, .75),
             PH = c(7.2, 7.3, 7.4), NO3 = c(18, 12, 8), NH4 = c(2, 1.5, 1))
}

demo_crops <- function() {
  list(
    spacing_crop("Soybean", "Generic_MG1", c("1-may", "15-jun"), 35, 35, 450,
                 tillage = spacing_tillage("no-till"),
                 irrigation = spacing_irrigation(threshold = .45, target = .8, max_application_mm = 30,
                                                 window_start = "15-jun", window_end = "31-aug")),
    spacing_crop("Wheat", "Hartog", c("10-oct", "20-nov"), 180, 35, 180,
                 fertilisation = spacing_fertilisation(30, list(
                   spacing_fertiliser_application(70, trigger = "DaysAfterSowing", days_after_sowing = 100))),
                 tillage = spacing_tillage("minimum"), organic = spacing_organic_n(20)),
    spacing_crop("Maize", "B_110", c("1-apr", "10-may"), 7.5, 50, 750,
                 fertilisation = spacing_fertilisation(50),
                 tillage = spacing_tillage("conventional", depth_mm = 300),
                 organic = spacing_organic_n(25),
                 parameter_overrides = c("Leaf.Photosynthesis.RUE.FixedValue" = 2.1)))
}

test_that("tillage and organic inputs validate their arguments", {
  expect_equal(spacing_tillage("minimum")$type, "minimum")
  expect_null(spacing_tillage("minimum")$depth_mm)
  expect_error(spacing_tillage("plough"))
  expect_error(spacing_tillage("conventional", incorporation_fraction = 1.2))
  expect_error(spacing_tillage("conventional", incorporation_fraction = .5, depth_mm = 0))
  expect_error(spacing_organic_n(-1))
  expect_error(spacing_organic_n(10, residue_type = "compost"), "not an APSIM residue type")
  expect_equal(spacing_organic_n(10, residue_type = "FYM")$residue_type, "fym")
})

test_that("crops validate sowing, objects, overrides and verified cultivars", {
  expect_s3_class(demo_crops()[[1]], "spacing_crop")
  expect_error(spacing_crop("Wheat", "NotACultivar", c("1-oct", "1-nov"), 180, 35, 180), "catalogue")
  expect_s3_class(spacing_crop("Wheat", "NotACultivar", c("1-oct", "1-nov"), 180, 35, 180,
                               verify_cultivar = FALSE), "spacing_crop")
  expect_s3_class(spacing_crop("Barley", "Grimmett", c("1-oct", "15-nov"), 150, 30, 180), "spacing_crop")
  expect_error(spacing_crop("Wheat", "Hartog", "1-oct", 180, 35, 180), "length 2")
  expect_error(spacing_crop("Wheat", "Hartog", c("1-oct", "1-nov"), 0, 35, 180))
  expect_error(spacing_crop("Wheat", "Hartog", c("1-oct", "1-nov"), 180, 35, 180, tillage = "minimum"),
               "spacing_tillage")
  expect_error(spacing_crop("Wheat", "Hartog", c("1-oct", "1-nov"), 180, 35, 180,
                            parameter_overrides = list(1.5)), "named")
})

test_that("soil profiles repeat the Tool2 physical checks", {
  sp <- spacing_soil_profile(demo_layers(), "S1")
  expect_s3_class(sp, "spacing_soil_profile")
  expect_equal(sp$InitialNUnits, "kgha")
  bad <- demo_layers(); bad$SAT[2] <- 0.60
  expect_error(spacing_soil_profile(bad, "S1"), "porosity")
  bad <- demo_layers(); bad$DUL[1] <- 0.12
  expect_error(spacing_soil_profile(bad, "S1"), "LL15 <= DUL")
  bad <- demo_layers(); bad$SWCON <- c(.3, NA, .3)
  expect_error(spacing_soil_profile(bad, "S1"), "every layer")
  bad <- demo_layers(); bad$DepthTop[2] <- 16
  expect_error(spacing_soil_profile(bad, "S1"), "contiguous")
  bad <- demo_layers(); bad$Foo <- 1
  expect_error(spacing_soil_profile(bad, "S1"), "Unknown soil columns")
  expect_error(spacing_soil_profile(demo_layers(), "S1",
                                    crop_parameters = list(Wheat = list(LL = c(.05, .15, .16)))), "AirDry, DUL")
  expect_error(spacing_soil_profile(demo_layers(), "S1", water = list(Foo = 1)), "Unknown water")
})

test_that("run-spec soil converts to a Tool2 soil profile", {
  spec <- new_spacing_soil_spec("RS", thickness = c(150, 150), bd = c(1.3, 1.35),
    air_dry = c(.06, .1), ll15 = c(.14, .15), dul = c(.31, .32), sat = c(.45, .44),
    ks = c(100, 80), carbon = c(1.2, .9), soil_cn_ratio = c(10, 11), ph = c(7, 7.2),
    no3 = c(10, 5), nh4 = c(1, .5))
  sp <- as_spacing_soil_profile(spec, SWCON = c(.3, .25))
  expect_equal(sp$Layers$DepthBottom, c(15, 30))
  expect_equal(sp$Layers$OC, c(1.2, .9))
  expect_equal(sp$Layers$SWCON, c(.3, .25))
})

test_that("scenarios validate their arguments (Tool2 0.09.10)", {
  crops <- demo_crops()
  # since Tool2 0.09.10 a crop may appear in several slots
  rep_sc <- new_spacing_scenario("x", list(crops[[2]], crops[[3]], crops[[2]]),
                                 latitude = 45, longitude = 10, start_year = 2020, end_year = 2024)
  expect_equal(vapply(rep_sc$crops, `[[`, "", "name"), c("Wheat", "Maize", "Wheat"))
  expect_error(new_spacing_scenario("bad id", crops, latitude = 45, longitude = 10,
                                    start_year = 2020, end_year = 2024), "letters")
  expect_error(new_spacing_scenario("x", crops, latitude = 45, longitude = 10,
                                    start_year = 2024, end_year = 2020))
  expect_error(new_spacing_scenario("x", "Wheat", latitude = 45, longitude = 10,
                                    start_year = 2020, end_year = 2024), "spacing_crop")
})

test_that("the Tool2 request uses the field names read by Spacing.Tool2", {
  sc <- new_spacing_scenario("rot1", demo_crops(),
    soil = spacing_soil_profile(demo_layers(), "S1",
                                crop_parameters = list(Wheat = list(KL = c(.06, .05, .04)))),
    latitude = 44.9, longitude = 10.63, start_year = 2020, end_year = 2024, cycles = 2)
  path <- write_spacing_scenario(sc, tempfile(fileext = ".json"), result_file = "r.json")
  req <- jsonlite::read_json(path)
  expect_equal(vapply(req$crops, `[[`, "", "name"), c("Soybean", "Wheat", "Maize"))
  expect_equal(req$cycles, 2L); expect_true(req$mock); expect_equal(req$result_file, "r.json")
  expect_equal(req$rotation_safety$max_crop_days, 400L); expect_equal(req$rotation_safety$stall_days, 150L)
  w <- req$crops[[2]]$management
  expect_equal(w$tillage_type, "minimum")
  expect_equal(w$nitrogen_mineral_kg_ha, 30)
  expect_equal(w$split_fertiliser[[1]]$trigger, "DaysAfterSowing")
  expect_equal(w$split_fertiliser[[1]]$days_after_sowing, 100L)
  expect_equal(w$nitrogen_organic_kg_ha, 20); expect_equal(w$organic_residue_type, "manure")
  expect_equal(w$sowing_window_start, "10-oct"); expect_false(w$auto_irrigate)
  m <- req$crops[[3]]
  expect_equal(m$management$tillage_depth_mm, 300)
  expect_null(m$management$tillage_incorporation_fraction)
  expect_equal(m$parameter_overrides$Leaf.Photosynthesis.RUE.FixedValue, 2.1)
  s <- req$crops[[1]]$management
  expect_true(s$auto_irrigate); expect_equal(s$irrigation_threshold, .45)
  expect_null(s$split_fertiliser); expect_equal(s$nitrogen_organic_kg_ha, 0); expect_null(s$organic_cn_ratio)
  expect_equal(length(req$soil_profile$Layers), 3L)
  expect_equal(req$soil_profile$Layers[[1]]$BD, 1.28)
  expect_equal(req$soil_profile$InitialNUnits, "kgha")
  expect_equal(unlist(req$soil_profile$CropParameters$Wheat$KL), c(.06, .05, .04))
  expect_error(write_spacing_scenario(sc, path), "already exists")
})

test_that("scenario arguments use --request and a compiled dll runs through dotnet", {
  root <- tempfile("rsp"); dir.create(root)
  apsim <- file.path(root, "apsim"); dir.create(file.path(apsim, "src", "Spacing.Tool2"), recursive = TRUE)
  cfg_file <- file.path(apsim, "spacing.config.json")
  jsonlite::write_json(list(OutputDir = file.path(root, "out"), TempWeatherDir = file.path(root, "w")),
                       cfg_file, auto_unbox = TRUE)
  dll <- file.path(root, "spacing-tool2.dll"); writeLines("", dll)
  cfg <- spacing_config(apsim_dir = apsim, use_compiled = TRUE, executable = dll)
  sc <- new_spacing_scenario("rot1", demo_crops(), latitude = 44.9, longitude = 10.63,
                             start_year = 2020, end_year = 2024)
  req_file <- write_spacing_scenario(sc, file.path(root, "q.json"))
  args <- build_spacing_args(cfg, sc, request_file = req_file)
  expect_equal(args[1:4], c("--config", cfg$config_file, "--request", normalizePath(req_file, winslash = "/")))
  expect_true("--mock" %in% args)
  expect_false("--rotation" %in% args)
  expect_error(build_spacing_args(cfg, sc), "request_file")
  cmd <- rSpacing:::.build_process_command(cfg, sc, request_file = req_file)
  expect_equal(cmd$command, "dotnet")
  expect_equal(cmd$args[1], cfg$executable)
})

test_that("rotation safety thresholds are validated", {
  expect_error(new_spacing_scenario("x", demo_crops(), latitude = 45, longitude = 10,
                                    start_year = 2020, end_year = 2024, stall_days = -1))
  sc <- new_spacing_scenario("x", demo_crops(), latitude = 45, longitude = 10,
                             start_year = 2020, end_year = 2024, max_crop_days = 200, stall_days = 0)
  req <- as_spacing_tool2_request(sc)
  expect_equal(req$rotation_safety, list(max_crop_days = 200L, stall_days = 0L))
})

test_that("a named list of crops is written as a JSON array", {
  crops <- list(first = spacing_crop("Maize", "B_130", c("1-apr", "10-may"), 7.5, 50, 750),
                second = spacing_crop("Wheat", "Claire", c("10-oct", "20-nov"), 180, 35, 180))
  sc <- new_spacing_scenario("named", crops, latitude = 44.9, longitude = 10.6,
                             start_year = 2020, end_year = 2021)
  path <- write_spacing_scenario(sc, tempfile(fileext = ".json"))
  json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_null(names(json$crops))
  expect_equal(vapply(json$crops, function(z) z$name, ""), c("Maize", "Wheat"))
})
