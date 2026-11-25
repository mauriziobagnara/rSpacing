# Complete example: data-driven APSIM run specification for calibration.
library(rSpacing)

catalogue <- spacing_parameter_catalogue(
  parameter_id = c("min_leaf_no", "grain_fill_tt"),
  crop = c("Maize", "Maize"),
  scope = c("cultivar", "cultivar"),
  apsim_path = c(
    "[Phenology].MinimumLeafNumber.FixedValue",
    "[Phenology].GrainFilling.Target.FixedValue"
  ),
  unit = c("number", "degree day"),
  lower_bound = c(4, 300),
  upper_bound = c(12, 1200)
)

soil <- new_spacing_soil_spec(
  profile_id = "ORVIETO_001",
  thickness = c(150, 150, 300, 300, 300, 600),
  bd = c(1.28, 1.34, 1.39, 1.43, 1.45, 1.47),
  air_dry = c(.06, .07, .08, .09, .10, .11),
  ll15 = c(.12, .14, .16, .18, .19, .20),
  dul = c(.27, .29, .31, .32, .33, .34),
  sat = c(.45, .44, .43, .42, .41, .40),
  ks = c(80, 50, 30, 20, 15, 10),
  carbon = c(1.40, 1.05, .72, .45, .30, .20),
  soil_cn_ratio = c(12, 12, 11, 11, 10, 10),
  ph = c(7.4, 7.5, 7.6, 7.7, 7.8, 7.8),
  no3 = c(15, 10, 8, 5, 3, 2),
  nh4 = c(3, 2, 2, 1, 1, 1),
  crop_parameters = list(Maize = list(
    LL = c(.12, .14, .16, .18, .19, .20),
    KL = c(.08, .07, .06, .04, .03, .02),
    XF = rep(1, 6)
  )),
  metadata = list(Source = "Example only")
)

cultivar <- new_spacing_cultivar_spec(
  crop = "Maize",
  name = "B_130_calibrated",
  parameters = list(
    "[Phenology].MinimumLeafNumber.FixedValue" = 6.4,
    "[Phenology].GrainFilling.Target.FixedValue" = 640
  )
)

rotation <- new_spacing_rotation_spec(
  name = "Maize",
  phases = list(list(
    Crop = "Maize", Cultivar = "B_130_calibrated",
    SowingWindow = list(Start = "01-Apr", End = "01-Jun"),
    Population = 10, Depth = 30, RowSpacing = 750,
    ResidueFractionRetained = 1
  ))
)

management <- new_spacing_management_spec(
  fertilisation = list(
    list(Crop = "Maize", Trigger = "Sowing", Type = "UreaN", Amount = 80, Depth = 50),
    list(Crop = "Maize", Trigger = "V6", Type = "UreaN", Amount = 60, Depth = 50)
  ),
  irrigation = list(
    list(Crop = "Maize", Trigger = "SoilWaterFraction", Threshold = .45,
         Amount = 30, MinimumIntervalDays = 7, SeasonalMaximum = 240)
  )
)

spec <- new_spacing_run_spec(
  run_id = "orvieto_cal_000001",
  latitude = 42.71924, longitude = 12.11249,
  start_year = 1990, end_year = 2023,
  soil = soil, rotation = rotation, cultivars = list(cultivar),
  management = management, catalogue = catalogue,
  parameter_values = list(min_leaf_no = 6.4, grain_fill_tt = 640),
  metadata = list(Purpose = "Calibration example")
)

# Each algorithm iteration updates values without changing C# source code.
spec_iteration_2 <- update_spacing_parameters(
  spec, list(min_leaf_no = 6.8, grain_fill_tt = 660)
)

write_spacing_run_spec(spec_iteration_2, "run_spec.json", overwrite = TRUE)

# A compatible Tool2 build must implement --run-spec.
# cfg <- spacing_config(..., use_compiled = TRUE, executable = "Spacing.Tool2.exe")
# request <- new_spacing_request(42.71924, 12.11249, "Maize", mock = TRUE)
# run <- run_spacing(cfg, request, run_spec = spec_iteration_2)
# result <- read_spacing(run)
