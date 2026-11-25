# rSpacing >= 0.10.08 with Spacing.Tool2 >= 0.09.09:
# management events not tied to sowing (dates, stages, autumn tillage, organic
# inputs after the previous harvest), any APSIM N fertiliser and an initial residue.
library(rSpacing)

spacing_fertiliser_types()          # N fertilisers and their N fraction
head(spacing_residue_types())       # residue types and their carbon fraction
spacing_crop_stages("Wheat")        # stage names usable with trigger = "Stage"

crops <- list(
  # soybean: farmyard manure on 15 March, no tillage at all
  spacing_crop("Soybean", "Generic_MG1", c("1-may", "15-jun"), density = 35, depth_mm = 35,
               row_spacing_mm = 450,
               organic_applications = list(
                 spacing_organic_application(30, "fym", trigger = "Date", date = "15-mar")),
               tillage_operations = list()),
  # wheat: 30 kg N at sowing, 60 kg N as ammonium nitrate on 1 March,
  # 40 kg N as UAN at flag leaf, placed at 50 mm
  spacing_crop("Wheat", "Claire", c("10-oct", "20-nov"), density = 180, depth_mm = 35,
               row_spacing_mm = 180,
               fertilisation = spacing_fertilisation(30, list(
                 spacing_fertiliser_application(60, "NH4NO3N", trigger = "Date", date = "1-mar"),
                 spacing_fertiliser_application(40, "UAN_N", trigger = "Stage",
                                                stage = "FlagLeafAppearance", depth_mm = 50))),
               tillage = spacing_tillage("minimum"), organic = spacing_organic_n(20)),
  # maize: chicken manure 30 days after the wheat harvest, ploughing on 15 Nov,
  # minimum tillage at sowing, basal 50 kg N as DAP at 50 mm
  spacing_crop("Maize", "B_130", c("1-apr", "10-may"), density = 7.5, depth_mm = 50,
               row_spacing_mm = 750,
               fertilisation = spacing_fertilisation(50, list(
                 spacing_fertiliser_application(100, trigger = "DaysAfterSowing", days_after_sowing = 35)),
                 fertiliser = "DAP", depth_mm = 50),
               organic = spacing_organic_n(25),
               organic_applications = list(
                 spacing_organic_application(40, "cm", cn_ratio = 10,
                                             trigger = "DaysAfterPreviousHarvest", days = 30)),
               tillage_operations = list(
                 spacing_tillage_operation("conventional", trigger = "Date", date = "15-nov"),
                 spacing_tillage_operation("minimum", depth_mm = 80))))

scenario <- new_spacing_scenario("events_rotation", crops, latitude = 44.9, longitude = 10.63,
                                 start_year = 2020, end_year = 2024, cycles = 2, mock = TRUE,
                                 surface_residue = spacing_surface_residue("maize", 3000, cn_ratio = 60,
                                                                           standing_fraction = .2))
print(scenario)
write_spacing_scenario(scenario, tempfile(fileext = ".json"))

if (FALSE) {
cfg <- spacing_config(apsim_dir = "apsim", use_compiled = TRUE,
                      executable = "apsim/src/Spacing.Tool2/bin/Debug/net8.0/spacing-tool2.dll")
run <- run_spacing(cfg, scenario)
daily <- utils::read.csv(run$result$tradeoffs$daily_csv_path)
# FertN_kgNha = product applied; FertMineralN_kgNha = mineral N applied
daily[daily$FertN_kgNha > 0, c("Clock.Today", "FertN_kgNha", "FertMineralN_kgNha")]
}
