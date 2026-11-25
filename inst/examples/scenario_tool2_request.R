# rSpacing >= 0.10.07 with Spacing.Tool2 >= 0.09.07:
# a complete scenario (rotation, management, soil) built and run from R.
library(rSpacing)

soil <- spacing_soil_profile(
  data.frame(DepthTop = c(0, 15, 30, 60), DepthBottom = c(15, 30, 60, 90),
             BD = c(1.28, 1.33, 1.39, 1.43), LL15 = c(.14, .15, .16, .16),
             DUL = c(.31, .32, .33, .32), SAT = c(.46, .45, .44, .43), SW = c(.28, .29, .30, .29),
             AirDry = c(.07, .075, .08, .08), SWCON = c(.3, .3, .3, .3), OC = c(1.45, 1.15, .75, .48),
             SoilCNRatio = c(12.5, 12.5, 12.5, 12.5), PH = c(7.2, 7.3, 7.4, 7.5),
             NO3 = c(18, 12, 8, 5), NH4 = c(2, 1.5, 1, .7)),       # kg N/ha per layer
  profile_id = "DEMO_LOAM")

crops <- list(
  spacing_crop("Soybean", "Generic_MG1", c("1-may", "15-jun"), density = 35, depth_mm = 35, row_spacing_mm = 450,
               tillage = spacing_tillage("no-till"),
               irrigation = spacing_irrigation(threshold = .45, target = .80, max_application_mm = 30,
                                               window_start = "15-jun", window_end = "31-aug")),
  spacing_crop("Wheat", "Claire", c("10-oct", "20-nov"), density = 180, depth_mm = 35, row_spacing_mm = 180,
               fertilisation = spacing_fertilisation(30, list(
                 spacing_fertiliser_application(70, trigger = "DaysAfterSowing", days_after_sowing = 100),
                 spacing_fertiliser_application(50, trigger = "DaysAfterSowing", days_after_sowing = 145))),
               tillage = spacing_tillage("minimum"), organic = spacing_organic_n(20)),
  spacing_crop("Maize", "B_130", c("1-apr", "10-may"), density = 7.5, depth_mm = 50, row_spacing_mm = 750,
               fertilisation = spacing_fertilisation(50, list(
                 spacing_fertiliser_application(100, trigger = "DaysAfterSowing", days_after_sowing = 35))),
               irrigation = spacing_irrigation(threshold = .50, target = .85, max_application_mm = 35,
                                               window_start = "1-jun", window_end = "31-aug"),
               tillage = spacing_tillage("conventional"), organic = spacing_organic_n(25)))

scenario <- new_spacing_scenario("demo_rotation", crops, soil = soil, latitude = 44.9, longitude = 10.63,
                                 start_year = 2020, end_year = 2024, cycles = 5, mock = TRUE)
print(scenario)

# Inspect or archive the exact request sent to Tool2
write_spacing_scenario(scenario, tempfile(fileext = ".json"))

# Run with the compiled DLL (needs only the .NET 8 runtime)
if (FALSE) {
cfg <- spacing_config(apsim_dir = "apsim", use_compiled = TRUE,
                      executable = "apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll")
run <- run_spacing(cfg, scenario)
run$result$yields
result <- read_spacing(run)
}
