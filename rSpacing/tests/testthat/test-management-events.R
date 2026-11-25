# rSpacing 0.10.08: management events of Spacing.Tool2 0.09.09

test_that("APSIM tables are available", {
  ft <- spacing_fertiliser_types()
  expect_true(all(c("UreaN", "DAP", "UAN_N", "NH4NO3N") %in% ft$fertiliser))
  expect_equal(ft$n_fraction[ft$fertiliser == "DAP"], .18)
  rt <- spacing_residue_types()
  expect_equal(rt$carbon_fraction[rt$residue_type == "manure"], .08)
  expect_true("FlagLeafAppearance" %in% spacing_crop_stages("Wheat")$stage)
  expect_equal(nrow(spacing_crop_stages("NoCrop")), 0L)
})

test_that("fertiliser applications accept types, dates, stages and depth", {
  a <- spacing_fertiliser_application(40, "uan_n", trigger = "Stage", stage = "FlagLeafAppearance", depth_mm = 50)
  expect_equal(a$fert_type, "UAN_N")
  l <- as.list(a)
  expect_equal(l$stage, "FlagLeafAppearance"); expect_equal(l$depth_mm, 50); expect_null(l$date)
  d <- as.list(spacing_fertiliser_application(60, "NH4NO3N", trigger = "Date", date = "01-MAR"))
  expect_equal(d$date, "1-mar"); expect_null(d$depth_mm)
  expect_error(spacing_fertiliser_application(10, "RockP"), "Unsupported")
  expect_error(spacing_fertiliser_application(10, trigger = "Date"), "date")
  expect_error(spacing_fertiliser_application(10, trigger = "Stage"), "stage")
  expect_error(spacing_fertiliser_application(10, depth_mm = -1))
  # defaults give the same request fields as rSpacing 0.10.07
  old <- as.list(spacing_fertilisation(50, list(spacing_fertiliser_application(100, trigger = "DaysAfterSowing", days_after_sowing = 35))))
  expect_setequal(names(old), c("nitrogen_mineral_kg_ha", "split_fertiliser"))
  expect_setequal(names(old$split_fertiliser[[1]]), c("trigger", "days_after_sowing", "amount_kg_n_ha", "fert_type"))
  basal <- as.list(spacing_fertilisation(50, fertiliser = "DAP", depth_mm = 50))
  expect_equal(basal$basal_fertiliser_type, "DAP"); expect_equal(basal$basal_fertiliser_depth_mm, 50)
})

test_that("stage names are checked against the crop", {
  f <- spacing_fertilisation(30, list(spacing_fertiliser_application(40, trigger = "Stage", stage = "flagleafappearance")))
  cr <- spacing_crop("Wheat", "Hartog", c("10-oct", "20-nov"), 180, 35, 180, fertilisation = f)
  expect_equal(cr$fertilisation$applications[[1]]$stage, "FlagLeafAppearance")
  bad <- spacing_fertilisation(30, list(spacing_fertiliser_application(40, trigger = "Stage", stage = "Tasseling")))
  expect_error(spacing_crop("Wheat", "Hartog", c("10-oct", "20-nov"), 180, 35, 180, fertilisation = bad),
               "not an APSIM stage")
})

test_that("organic applications, tillage operations and residue reach the request", {
  oa <- spacing_organic_application(40, "cm", cn_ratio = 10, trigger = "DaysAfterPreviousHarvest", days = 30)
  expect_equal(oa$days, 30L)
  expect_error(spacing_organic_application(40, trigger = "Date"), "date")
  expect_error(spacing_organic_application(40, "slurry"), "residue type")
  to <- list(spacing_tillage_operation("conventional", trigger = "Date", date = "15-nov"),
             spacing_tillage_operation("minimum", depth_mm = 80))
  maize <- spacing_crop("Maize", "B_110", c("1-apr", "10-may"), 7.5, 50, 750,
                        fertilisation = spacing_fertilisation(50, fertiliser = "DAP", depth_mm = 50),
                        organic = spacing_organic_n(25),
                        organic_applications = oa, tillage_operations = to)
  soy <- spacing_crop("Soybean", "Generic_MG1", c("1-may", "15-jun"), 35, 40, 450,
                      tillage_operations = list())
  sc <- new_spacing_scenario("ev", list(soy, maize), latitude = 45, longitude = 10,
                             start_year = 2020, end_year = 2022,
                             surface_residue = spacing_surface_residue("maize", 3000, cn_ratio = 60))
  out <- capture.output(print(sc)); expect_true(any(grepl("3000 kg/ha maize", out)))
  req <- jsonlite::read_json(write_spacing_scenario(sc, tempfile(fileext = ".json")))
  expect_equal(req$surface_residue$type, "maize"); expect_equal(req$surface_residue$name, "maize_residue")
  m <- req$crops[[2]]$management
  expect_equal(m$basal_fertiliser_type, "DAP")
  expect_equal(m$organic_applications[[1]]$trigger, "DaysAfterPreviousHarvest")
  expect_equal(m$organic_applications[[1]]$residue_type, "cm")
  expect_equal(m$tillage_operations[[1]]$date, "15-nov")
  expect_equal(m$tillage_operations[[2]]$depth_mm, 80)
  expect_null(m$tillage_operations[[2]]$date)
  expect_equal(length(req$crops[[1]]$management$tillage_operations), 0L)
  expect_true("tillage_operations" %in% names(req$crops[[1]]$management))
  expect_null(req$crops[[1]]$management$organic_applications)
  expect_error(new_spacing_scenario("ev", list(soy), latitude = 45, longitude = 10, start_year = 2020,
                                    end_year = 2022, surface_residue = list(type = "maize")), "surface_residue")
  expect_error(spacing_surface_residue("straw"), "residue type")
})
