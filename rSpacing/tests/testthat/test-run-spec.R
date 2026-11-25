make_soil_spec <- function() {
  new_spacing_soil_spec(
    profile_id = "soil-1",
    thickness = c(150, 150), bd = c(1.3, 1.4),
    air_dry = c(0.05, 0.06), ll15 = c(0.10, 0.12),
    dul = c(0.25, 0.27), sat = c(0.45, 0.43), ks = c(20, 10),
    carbon = c(1.2, 0.8), soil_cn_ratio = c(12, 11),
    ph = c(7, 7.2), no3 = c(10, 5), nh4 = c(2, 1),
    crop_parameters = list(Maize = list(
      LL = c(0.10, 0.12), KL = c(0.08, 0.05), XF = c(1, 1)
    ))
  )
}

make_run_spec <- function(weather_file = NULL) {
  catalogue <- spacing_parameter_catalogue(
    parameter_id = "min_leaf_no", crop = "Maize", scope = "cultivar",
    apsim_path = "[Phenology].MinimumLeafNumber.FixedValue",
    lower_bound = 4, upper_bound = 12
  )
  cultivar <- new_spacing_cultivar_spec(
    "Maize", "B_110_cal", list("[Phenology].MinimumLeafNumber.FixedValue" = 6.4)
  )
  rotation <- new_spacing_rotation_spec("Maize", list(list(
    Crop = "Maize", Cultivar = "B_110_cal",
    SowingWindow = list(Start = "01-Apr", End = "01-Jun"),
    Population = 10, Depth = 30, RowSpacing = 750
  )))
  management <- new_spacing_management_spec(
    fertilisation = list(list(Crop = "Maize", Trigger = "Sowing",
                              Type = "UreaN", Amount = 80, Depth = 50))
  )
  new_spacing_run_spec(
    "cal-1", 45.4, 9.2, 1990, 2023, weather_file = weather_file,
    soil = make_soil_spec(), rotation = rotation,
    cultivars = list(cultivar), management = management,
    catalogue = catalogue, parameter_values = list(min_leaf_no = 6.4)
  )
}

test_that("soil specifications enforce layer consistency", {
  expect_s3_class(make_soil_spec(), "spacing_soil_spec")
  expect_error(
    new_spacing_soil_spec("bad", c(100,100), c(1.3,1.4), c(.1,.1),
      c(.2,.2), c(.15,.3), c(.4,.4), c(10,10), c(1,1), c(10,10),
      c(7,7), c(1,1), c(1,1)),
    class = "spacing_validation_error"
  )
})

test_that("parameter values are checked against catalogue bounds", {
  cat <- spacing_parameter_catalogue(
    parameter_id = "p", crop = "Maize", scope = "cultivar",
    apsim_path = "[X].Y", lower_bound = 0, upper_bound = 1
  )
  expect_equal(validate_spacing_parameters(list(p = .5), cat)$p, .5)
  expect_error(validate_spacing_parameters(list(p = 2), cat),
               class = "spacing_validation_error")
})

test_that("run specifications round-trip through JSON", {
  x <- make_run_spec()
  path <- tempfile(fileext = ".json")
  write_spacing_run_spec(x, path)
  y <- read_spacing_run_spec(path)
  expect_s3_class(y, "spacing_run_spec")
  expect_equal(y$RunId, x$RunId)
  expect_equal(unlist(y$Soil$Thickness), x$Soil$Thickness)
  expect_equal(y$Rotation$Name, "Maize")
  expect_s3_class(y$ParameterCatalogue, "spacing_parameter_catalogue")
  expect_equal(names(y$ParameterCatalogue), names(x$ParameterCatalogue))
  expect_equal(y$ParameterCatalogue$parameter_id,
               x$ParameterCatalogue$parameter_id)
  expect_equal(y$ParameterCatalogue$apsim_path,
               x$ParameterCatalogue$apsim_path)
})

test_that("multi-row parameter catalogues round-trip through JSON", {
  catalogue <- spacing_parameter_catalogue(
    parameter_id = c("maize_rue", "maize_grain_fill"),
    crop = c("Maize", "Maize"),
    scope = c("cultivar", "cultivar"),
    apsim_path = c(
      "Leaf.Photosynthesis.RUE.FixedValue",
      "Phenology.GrainFilling.Target.FixedValue"
    ),
    type = c("double", "double"),
    unit = c("g/MJ", "degree day"),
    lower_bound = c(1.2, 400),
    upper_bound = c(3.2, 950),
    transform = c("identity", "identity"),
    application_stage = c("cultivar", "cultivar")
  )
  x <- make_run_spec()
  x$ParameterCatalogue <- catalogue
  x$ParameterValues <- list(maize_rue = 2.0, maize_grain_fill = 730)
  validate_spacing_run_spec(x)

  path <- tempfile(fileext = ".json")
  write_spacing_run_spec(x, path)
  y <- read_spacing_run_spec(path)

  expect_s3_class(y$ParameterCatalogue, "spacing_parameter_catalogue")
  expect_equal(nrow(y$ParameterCatalogue), 2L)
  expect_equal(y$ParameterCatalogue$parameter_id, catalogue$parameter_id)
  expect_equal(y$ParameterCatalogue$lower_bound, catalogue$lower_bound)
  expect_equal(y$ParameterCatalogue$upper_bound, catalogue$upper_bound)
  expect_equal(y$ParameterValues$maize_rue, 2.0)
  expect_equal(y$ParameterValues$maize_grain_fill, 730)
})

test_that("CLI includes a validated run-spec path", {
  td <- make_test_repository()
  cfg <- spacing_config(td)
  req <- new_spacing_request(45.4, 9.2, "Maize", mock = TRUE)
  path <- tempfile(fileext = ".json")
  write_spacing_run_spec(make_run_spec(), path)
  args <- rSpacing:::build_spacing_args(cfg, req, path)
  expect_true("--run-spec" %in% args)
  expect_equal(normalizePath(args[match("--run-spec", args) + 1L], winslash = "/"), normalizePath(path, winslash = "/"))
})

test_that("calibration parameters can be updated without changing code", {
  x <- make_run_spec()
  y <- update_spacing_parameters(x, list(min_leaf_no = 7.1))
  expect_equal(y$ParameterValues$min_leaf_no, 7.1)
  expect_equal(x$ParameterValues$min_leaf_no, 6.4)
})


test_that("single-row parameter catalogues use row-oriented JSON", {
  x <- make_run_spec()
  path <- tempfile(fileext = ".json")
  write_spacing_run_spec(x, path)
  raw <- jsonlite::read_json(path, simplifyVector = FALSE)
  expect_true(is.list(raw$ParameterCatalogue))
  expect_length(raw$ParameterCatalogue, 1L)
  expect_true(all(c("parameter_id", "lower_bound", "upper_bound") %in%
                  names(raw$ParameterCatalogue[[1L]])))
  y <- read_spacing_run_spec(path)
  expect_s3_class(y$ParameterCatalogue, "spacing_parameter_catalogue")
  expect_equal(nrow(y$ParameterCatalogue), 1L)
})

test_that("layered soil values are numeric vectors after a JSON round trip", {
  soil <- new_spacing_soil_spec("demo", thickness = c(150, 150), bd = c(1.3, 1.4),
    air_dry = c(.07, .08), ll15 = c(.14, .16), dul = c(.31, .33), sat = c(.45, .44),
    ks = c(100, 80), carbon = c(1.2, .8), soil_cn_ratio = c(12, 12), ph = c(7.2, 7.4),
    no3 = c(20, 8), nh4 = c(2, 1),
    crop_parameters = list(Maize = list(LL = c(.14, .16), KL = c(.08, .06), XF = c(1, 1))))
  spec <- new_spacing_run_spec("rt", 44.9, 10.6, 2020, 2021, soil = soil,
    rotation = new_spacing_rotation_spec("m", list(list(Crop = "Maize", Cultivar = "B_130"))))
  back <- read_spacing_run_spec(write_spacing_run_spec(spec, tempfile(fileext = ".json")))
  expect_identical(back$Soil$DUL, spec$Soil$DUL)
  expect_identical(back$Soil$Thickness, spec$Soil$Thickness)
  expect_identical(back$Soil$CropParameters$Maize$KL, c(.08, .06))
})
