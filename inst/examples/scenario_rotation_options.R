# rSpacing >= 0.10.09 with Spacing.Tool2 >= 0.09.10:
# repeated crops, sowing rules and fixed dates, scheduled irrigation, cover crop,
# intercrop, perennial crop, residue removal and extra report variables.
library(rSpacing)

wheat1 <- spacing_crop("Wheat", "Claire", c("10-oct", "20-nov"), 180, 35, 180,
                       fertilisation = spacing_fertilisation(30, list(
                         spacing_fertiliser_application(70, trigger = "DaysAfterSowing", days_after_sowing = 100))),
                       tillage = spacing_tillage("minimum"),
                       residue_removal = spacing_residue_removal(0.7))            # straw baled

oats_cover <- spacing_crop("Oats", "Drummond", c("1-aug", "30-sep"), 250, 30, 180,
                           tillage = spacing_tillage("no-till"),
                           termination = spacing_termination("Date", date = "15-mar"))

maize <- spacing_crop("Maize", "B_130", c("1-apr", "10-may"), 7.5, 50, 750,
                      fertilisation = spacing_fertilisation(50, list(
                        spacing_fertiliser_application(100, trigger = "DaysAfterSowing", days_after_sowing = 35))),
                      sowing_rule = spacing_sowing_rule(min_esw_mm = 40, min_rain_mm = 5, rain_days = 5,
                                                        min_soil_temp_c = 12, temp_days = 5,
                                                        sow_at_window_end = TRUE),
                      irrigation_schedule = list(
                        spacing_irrigation_event(30, "DaysAfterSowing", days = 40),
                        spacing_irrigation_event(40, "Stage", stage = "Flowering"),
                        spacing_irrigation_event(35, "Date", date = "15-jul")))

wheat2 <- spacing_crop("Wheat", "Claire", c("10-oct", "20-nov"), 220, 35, 180,   # same crop, own management
                       fertilisation = spacing_fertilisation(30))

soy_maize <- spacing_crop("Soybean", "Generic_MG1", sowing_window = NULL, sowing_date = "10-may",
                          density = 35, depth_mm = 35, row_spacing_mm = 450,
                          tillage = spacing_tillage("no-till"),
                          intercrop = spacing_intercrop("Maize", "B_130", density = 3.5, depth_mm = 50,
                                                        row_spacing_mm = 750))

scenario <- new_spacing_scenario("rotation_options", list(wheat1, oats_cover, maize, wheat2, soy_maize),
                                 latitude = 44.9, longitude = 10.63, start_year = 2020, end_year = 2025,
                                 report = spacing_report_spec(
                                   harvest_variables = "[Wheat].Grain.N * 10 as Wheat_GrainN_kgha",
                                   daily_variables = "[Weather].MeanT as Tmean"))
print(scenario)
write_spacing_scenario(scenario, tempfile(fileext = ".json"))

# A perennial: grapevine for 10 years, harvested every 15 September
vine <- spacing_crop("Grapevine", "SauvignonBlanc", c("1-jul", "31-jul"), 0.4, 300, 2500,
                     tillage = spacing_tillage("no-till"),
                     perennial = spacing_perennial(10, harvest_date = "15-sep"))

if (FALSE) {
cfg <- spacing_config(apsim_dir = "apsim", use_compiled = TRUE,
                      executable = "apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll")
run <- run_spacing(cfg, scenario)
report <- utils::read.csv(run$result$tradeoffs$control_csv_path)
report[, c("Clock.Today", "HarvestedCrop", "RotationSlot", "EndReason")]
}
