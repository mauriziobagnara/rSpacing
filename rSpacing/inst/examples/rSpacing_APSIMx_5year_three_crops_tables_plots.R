# ==============================================================================
# rSpacing -> Spacing.Tool2 -> APSIMx
# Demo completa da RStudio, senza variazione dei parametri colturali
# Versione v1.03: tabelle e grafici restituiti in RStudio e salvati come file esterni
#
# Scenario:
#   - coordinate: Gualtieri (RE), 44.9000 N, 10.6300 E
#   - periodo: 2020-2024 (5 anni)
#   - suolo completo fornito da R
#   - tre specie/cultivar APSIM: Wheat/Hartog, Maize/B_110,
#     Soybean/Generic_MG1
#   - gestione: semina, densita, interfila, fertilizzazione frazionata,
#     lavorazione e irrigazione
#   - meteo: mock generato da Spacing.Tool2
#   - calibrazione e parameter_overrides: disattivati
#   - risultati restituiti come oggetto R e letti da JSON, CSV e SQLite
#
# USO IN RSTUDIO
#   1. Aprire questo file.
#   2. Modificare soltanto SPACING_ROOT se la soluzione e in un'altra cartella.
#   3. Eseguire Source.
#   4. I risultati saranno disponibili nell'oggetto globale rspacing_demo.
# ============================================================================== 

rm(list = ls())
graphics.off()

# ----------------------------------------------------------------------------  
# 1. Configurazione locale
# ----------------------------------------------------------------------------  

SPACING_ROOT <- "C:/Users/maurizio.bagnara/Documents/spacing-main/apsim"
BUILD_CONFIGURATION <- "Debug"
TARGET_FRAMEWORK <- "net8.0"
USE_MOCK_WEATHER <- TRUE

START_YEAR <- 2020L
END_YEAR <- 2024L
LATITUDE <- 44.9000
LONGITUDE <- 10.6300
CELL_ID <- "GUALTIERI_RE_DEMO"
RUN_ID <- paste0(
  "gualtieri_rotation_",
  START_YEAR, "_", END_YEAR, "_",
  format(Sys.time(), "%Y%m%d_%H%M%S")
)

# ----------------------------------------------------------------------------  
# 2. Dipendenze R
# ----------------------------------------------------------------------------  

ensure_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Impossibile caricare il pacchetto R: ", package, call. = FALSE)
  }
}

ensure_package("jsonlite")
ensure_package("ggplot2")

# RSQLite non e indispensabile per eseguire APSIMx. Se disponibile, lo script
# legge anche il DataStore SQLite. In caso contrario JSON e CSV sono comunque
# restituiti a RStudio.
has_rsqlite <- requireNamespace("RSQLite", quietly = TRUE)

# ----------------------------------------------------------------------------  
# 3. Individuazione e verifica di Spacing.Tool2
# ----------------------------------------------------------------------------  

normalize_win <- function(path, must_work = FALSE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

spacing_root <- normalize_win(SPACING_ROOT, must_work = TRUE)
project_path <- file.path(spacing_root, "src", "Spacing.Tool2", "Spacing.Tool2.csproj")
dll_path <- file.path(
  spacing_root,
  "src", "Spacing.Tool2", "bin", BUILD_CONFIGURATION,
  TARGET_FRAMEWORK, "spacing-tool2.dll"
)

if (!file.exists(project_path)) {
  stop("Progetto Spacing.Tool2 non trovato: ", project_path, call. = FALSE)
}

if (!file.exists(dll_path)) {
  stop(
    paste0(
      "DLL compilata non trovata:\n", dll_path, "\n\n",
      "Aprire spacing.sln in Visual Studio 2026, impostare ",
      BUILD_CONFIGURATION, " | Any CPU e selezionare ",
      "Compilazione > Ricompila soluzione."
    ),
    call. = FALSE
  )
}

if (!nzchar(Sys.which("dotnet"))) {
  stop(
    "dotnet non trovato nel PATH di RStudio. Riavviare RStudio dopo ",
    "l'installazione del .NET SDK usato da Visual Studio 2026.",
    call. = FALSE
  )
}

# ----------------------------------------------------------------------------  
# 4. Directory isolate per la run
# ----------------------------------------------------------------------------  

runs_root <- file.path(spacing_root, "rSpacing_runs")
run_dir <- file.path(runs_root, RUN_ID)
output_dir <- file.path(run_dir, "output")
weather_dir <- file.path(run_dir, "weather")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(weather_dir, recursive = TRUE, showWarnings = FALSE)

request_path <- file.path(run_dir, "request.json")
config_path <- file.path(run_dir, "spacing.config.json")
result_path <- file.path(output_dir, "result.json")
manifest_path <- file.path(output_dir, "manifest.json")
stdout_path <- file.path(output_dir, "spacing_tool2_stdout.log")
stderr_path <- file.path(output_dir, "spacing_tool2_stderr.log")

# ----------------------------------------------------------------------------  
# 5. Profilo del suolo fornito integralmente da R
#
# Valori dimostrativi plausibili per un suolo franco-limoso della pianura
# emiliana. Non sono dati pedologici osservati e vanno sostituiti prima di un
# uso scientifico o decisionale.
# ----------------------------------------------------------------------------  

soil_profile <- list(
  ProfileId = "GUALTIERI_LOAM_DEMO_001",
  CellId = CELL_ID,
  SoilType = "Franco limoso dimostrativo",
  DataSource = "Synthetic demo profile generated in R; not observed soil data",
  AreaFraction = 1.0,
  Layers = list(
    list(DepthTop =   0, DepthBottom =  15, Sand = 20, Silt = 58, Clay = 22,
         DUL = 0.31, LL15 = 0.14, SAT = 0.46, SW = 0.28,
         OC = 1.45, BD = 1.28, PH = 7.2, NO3 = 18.0, NH4 = 2.0),
    list(DepthTop =  15, DepthBottom =  30, Sand = 19, Silt = 59, Clay = 22,
         DUL = 0.32, LL15 = 0.15, SAT = 0.45, SW = 0.29,
         OC = 1.15, BD = 1.33, PH = 7.3, NO3 = 12.0, NH4 = 1.5),
    list(DepthTop =  30, DepthBottom =  60, Sand = 18, Silt = 57, Clay = 25,
         DUL = 0.33, LL15 = 0.16, SAT = 0.44, SW = 0.30,
         OC = 0.75, BD = 1.39, PH = 7.4, NO3 = 8.0, NH4 = 1.0),
    list(DepthTop =  60, DepthBottom =  90, Sand = 20, Silt = 54, Clay = 26,
         DUL = 0.32, LL15 = 0.16, SAT = 0.43, SW = 0.29,
         OC = 0.48, BD = 1.43, PH = 7.5, NO3 = 5.0, NH4 = 0.7),
    list(DepthTop =  90, DepthBottom = 120, Sand = 22, Silt = 52, Clay = 26,
         DUL = 0.31, LL15 = 0.16, SAT = 0.42, SW = 0.28,
         OC = 0.32, BD = 1.46, PH = 7.6, NO3 = 3.5, NH4 = 0.5),
    list(DepthTop = 120, DepthBottom = 150, Sand = 24, Silt = 50, Clay = 26,
         DUL = 0.30, LL15 = 0.15, SAT = 0.41, SW = 0.27,
         OC = 0.24, BD = 1.49, PH = 7.7, NO3 = 2.5, NH4 = 0.4)
  )
)

# ----------------------------------------------------------------------------  
# 6. Colture, cultivar e gestione
#
# Non viene inviato alcun campo parameter_overrides.
# Le cultivar sono nomi gia previsti dai default del RotationBuilder:
#   Wheat = Hartog; Maize = B_110; Soybean = Generic_MG1.
# ----------------------------------------------------------------------------  

crops <- list(
  # ---------------------------------------------------------------------------
  # WHEAT / HARTOG
  # N totale: 150 kg N/ha come UreaN.
  #   30 kg N/ha alla semina
  #   70 kg N/ha a 100 giorni dalla semina
  #   50 kg N/ha a 145 giorni dalla semina
  # Irrigazione di soccorso primaverile: soglia 35% dell'acqua disponibile,
  # ripristino al 70%, massimo 25 mm per intervento, marzo-maggio.
  # ---------------------------------------------------------------------------
  list(
    name = "Wheat",
    management = list(
      cultivar = "Hartog",
      tillage_type = "minimum",
      nitrogen_mineral_kg_ha = 30.0,
      nitrogen_organic_kg_ha = 0.0,
      sowing_window_start = "10-oct",
      sowing_window_end = "20-nov",
      sowing_density = 180.0,
      sowing_depth = 35.0,
      row_spacing = 180.0,
      auto_irrigate = TRUE,
      irrigation_threshold = 0.35,
      irrigation_target = 0.70,
      max_irrigation_mm = 25.0,
      irrigation_window_start = "1-mar",
      irrigation_window_end = "31-may",
      split_fertiliser = list(
        list(trigger = "DaysAfterSowing", days_after_sowing = 100L,
             amount_kg_n_ha = 70.0, fert_type = "UreaN"),
        list(trigger = "DaysAfterSowing", days_after_sowing = 145L,
             amount_kg_n_ha = 50.0, fert_type = "UreaN")
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # MAIZE / B_110
  # N totale: 200 kg N/ha come UreaN.
  #   50 kg N/ha alla semina
  #   100 kg N/ha a 35 giorni dalla semina
  #   50 kg N/ha a 60 giorni dalla semina
  # Irrigazione estiva: soglia 50%, ripristino all'85%, massimo 35 mm.
  # ---------------------------------------------------------------------------
  list(
    name = "Maize",
    management = list(
      cultivar = "B_110",
      tillage_type = "conventional",
      nitrogen_mineral_kg_ha = 50.0,
      nitrogen_organic_kg_ha = 0.0,
      sowing_window_start = "1-apr",
      sowing_window_end = "10-may",
      sowing_density = 7.5,
      sowing_depth = 50.0,
      row_spacing = 750.0,
      auto_irrigate = TRUE,
      irrigation_threshold = 0.50,
      irrigation_target = 0.85,
      max_irrigation_mm = 35.0,
      irrigation_window_start = "1-jun",
      irrigation_window_end = "31-aug",
      split_fertiliser = list(
        list(trigger = "DaysAfterSowing", days_after_sowing = 35L,
             amount_kg_n_ha = 100.0, fert_type = "UreaN"),
        list(trigger = "DaysAfterSowing", days_after_sowing = 60L,
             amount_kg_n_ha = 50.0, fert_type = "UreaN")
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # SOYBEAN / GENERIC_MG1
  # N minerale: 0 kg N/ha. La scelta evita di simulare una concimazione azotata
  # ordinaria sulla leguminosa. Il fertilizzante UreaN resta quindi non applicato.
  # Irrigazione: soglia 45%, ripristino all'80%, massimo 30 mm, giugno-agosto.
  # ---------------------------------------------------------------------------
  list(
    name = "Soybean",
    management = list(
      cultivar = "Generic_MG1",
      tillage_type = "no-till",
      nitrogen_mineral_kg_ha = 0.0,
      nitrogen_organic_kg_ha = 0.0,
      sowing_window_start = "1-may",
      sowing_window_end = "15-jun",
      sowing_density = 35.0,
      sowing_depth = 35.0,
      row_spacing = 450.0,
      auto_irrigate = TRUE,
      irrigation_threshold = 0.45,
      irrigation_target = 0.80,
      max_irrigation_mm = 30.0,
      irrigation_window_start = "15-jun",
      irrigation_window_end = "31-aug",
      split_fertiliser = list()
    )
  )
)

# ----------------------------------------------------------------------------  
# 7. Request e configurazione per Spacing.Tool2
# ----------------------------------------------------------------------------  

request <- list(
  id = "gualtieri_5y_three_species",
  name = "Scenario quinquennale frumento-mais-soia",
  run_id = RUN_ID,
  metadata = list(
    source = "RStudio",
    caller = "rSpacing demo integration",
    purpose = "End-to-end test with crop-specific fertilisation and irrigation; no parameter variation",
    location = "Gualtieri, Emilia-Romagna, Italy",
    rotation_label = "Wheat(Hartog)-Maize(B_110)-Soybean(Generic_MG1)",
    soil_warning = "Synthetic demonstrative soil; replace before scientific use"
  ),
  mock = isTRUE(USE_MOCK_WEATHER),
  calibrate = FALSE,
  lat = LATITUDE,
  lon = LONGITUDE,
  cell_id = CELL_ID,
  start_year = START_YEAR,
  end_year = END_YEAR,
  soil_profile = soil_profile,
  result_file = normalize_win(result_path, must_work = FALSE),
  manifest_file = normalize_win(manifest_path, must_work = FALSE),
  crops = crops,
  cycles = 5L
)

config <- list(
  DbApiBaseUrl = "http://localhost:8000/api/v1",
  DbApiKey = "",
  TempWeatherDir = normalize_win(weather_dir, must_work = FALSE),
  OutputDir = normalize_win(output_dir, must_work = FALSE),
  StartYear = START_YEAR,
  EndYear = END_YEAR,
  MaxParallelism = 1L
)

jsonlite::write_json(
  request,
  request_path,
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = NA,
  null = "null"
)
jsonlite::write_json(
  config,
  config_path,
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = NA,
  null = "null"
)

# Controllo esplicito: nessuna variazione di parametri deve essere presente.
request_text <- paste(readLines(request_path, warn = FALSE), collapse = "\n")
if (grepl("parameter_overrides", request_text, fixed = TRUE)) {
  stop("Il request contiene parameter_overrides, ma questa demo deve escluderli.",
       call. = FALSE)
}

# ----------------------------------------------------------------------------  
# 8. Esecuzione di Spacing.Tool2
# ----------------------------------------------------------------------------  

args <- c(
  shQuote(normalize_win(dll_path, must_work = TRUE)),
  "--config", shQuote(normalize_win(config_path, must_work = TRUE)),
  "--request", shQuote(normalize_win(request_path, must_work = TRUE)),
  "--result", shQuote(normalize_win(result_path, must_work = FALSE)),
  "--manifest", shQuote(normalize_win(manifest_path, must_work = FALSE))
)
if (isTRUE(USE_MOCK_WEATHER)) {
  args <- c(args, "--mock")
}

message("Esecuzione di Spacing.Tool2...")
message("Run directory: ", normalize_win(run_dir, must_work = TRUE))

exit_status <- system2(
  command = "dotnet",
  args = args,
  stdout = stdout_path,
  stderr = stderr_path,
  wait = TRUE
)

stdout_text <- if (file.exists(stdout_path)) {
  paste(readLines(stdout_path, warn = FALSE), collapse = "\n")
} else ""
stderr_text <- if (file.exists(stderr_path)) {
  paste(readLines(stderr_path, warn = FALSE), collapse = "\n")
} else ""

if (!identical(as.integer(exit_status), 0L)) {
  stop(
    paste0(
      "Spacing.Tool2 terminato con codice ", exit_status, ".\n\n",
      "STDERR:\n", stderr_text, "\n\n",
      "STDOUT:\n", stdout_text, "\n\n",
      "File diagnostici in: ", normalize_win(output_dir, must_work = TRUE)
    ),
    call. = FALSE
  )
}

if (!file.exists(result_path)) {
  stop(
    "Spacing.Tool2 ha restituito codice 0 ma non ha creato result.json. ",
    "Controllare stdout e stderr in: ", output_dir,
    call. = FALSE
  )
}

# ----------------------------------------------------------------------------  
# 9. Importazione dei risultati in RStudio
# ----------------------------------------------------------------------------  

result <- jsonlite::read_json(result_path, simplifyVector = TRUE)
manifest <- if (file.exists(manifest_path)) {
  jsonlite::read_json(manifest_path, simplifyVector = TRUE)
} else NULL

read_csv_if_present <- function(path) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !nzchar(path)) {
    return(NULL)
  }
  if (!file.exists(path)) {
    return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

tradeoffs <- result$tradeoffs
control <- read_csv_if_present(tradeoffs$control_csv_path)
daily <- read_csv_if_present(tradeoffs$daily_csv_path)
crop_summary <- read_csv_if_present(tradeoffs$crop_summary_path)
timeseries <- read_csv_if_present(tradeoffs$timeseries_csv)
harvest <- read_csv_if_present(tradeoffs$harvest_csv)

sqlite <- list(path = tradeoffs$datastore_path, tables = NULL, preview = list())
if (has_rsqlite && !is.null(sqlite$path) && file.exists(sqlite$path)) {
  connection <- DBI::dbConnect(RSQLite::SQLite(), sqlite$path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  sqlite$tables <- DBI::dbListTables(connection)
  for (table_name in sqlite$tables) {
    sqlite$preview[[table_name]] <- DBI::dbGetQuery(
      connection,
      paste0("SELECT * FROM [", table_name, "] LIMIT 25")
    )
  }
}

rotation_plan <- data.frame(
  Species = c("Wheat", "Maize", "Soybean"),
  Cultivar = c("Hartog", "B_110", "Generic_MG1"),
  Sowing_window = c("10-oct / 20-nov", "1-apr / 10-may", "1-may / 15-jun"),
  Tillage = c("minimum", "conventional", "no-till"),
  Mineral_N_total_kg_ha = c(150, 200, 0),
  Fertiliser_type = c("UreaN", "UreaN", "none"),
  Irrigated = c(TRUE, TRUE, TRUE),
  Irrigation_threshold = c(0.35, 0.50, 0.45),
  Irrigation_target = c(0.70, 0.85, 0.80),
  Max_irrigation_mm = c(25, 35, 30),
  stringsAsFactors = FALSE
)

fertilisation_plan <- data.frame(
  Species = c("Wheat", "Wheat", "Wheat", "Maize", "Maize", "Maize"),
  Cultivar = c("Hartog", "Hartog", "Hartog", "B_110", "B_110", "B_110"),
  Trigger = c("Sowing", "DaysAfterSowing", "DaysAfterSowing",
              "Sowing", "DaysAfterSowing", "DaysAfterSowing"),
  Days_after_sowing = c(0L, 100L, 145L, 0L, 35L, 60L),
  Amount_kg_N_ha = c(30, 70, 50, 50, 100, 50),
  Fertiliser_type = "UreaN",
  stringsAsFactors = FALSE
)

irrigation_plan <- data.frame(
  Species = c("Wheat", "Maize", "Soybean"),
  Cultivar = c("Hartog", "B_110", "Generic_MG1"),
  Window_start = c("1-mar", "1-jun", "15-jun"),
  Window_end = c("31-may", "31-aug", "31-aug"),
  Threshold_fraction = c(0.35, 0.50, 0.45),
  Target_fraction = c(0.70, 0.85, 0.80),
  Maximum_application_mm = c(25, 35, 30),
  stringsAsFactors = FALSE
)

rspacing_demo <- list(
  success = isTRUE(result$success),
  exit_status = as.integer(exit_status),
  run_id = RUN_ID,
  coordinates = data.frame(
    cell_id = CELL_ID,
    latitude = LATITUDE,
    longitude = LONGITUDE,
    stringsAsFactors = FALSE
  ),
  period = data.frame(start_year = START_YEAR, end_year = END_YEAR),
  rotation_plan = rotation_plan,
  fertilisation_plan = fertilisation_plan,
  irrigation_plan = irrigation_plan,
  soil_profile = soil_profile,
  request = request,
  result = result,
  manifest = manifest,
  tables = list(
    control = control,
    daily = daily,
    crop_summary = crop_summary,
    timeseries = timeseries,
    harvest = harvest
  ),
  sqlite = sqlite,
  files = list(
    run_directory = normalize_win(run_dir, must_work = TRUE),
    request = normalize_win(request_path, must_work = TRUE),
    config = normalize_win(config_path, must_work = TRUE),
    result = normalize_win(result_path, must_work = TRUE),
    manifest = if (file.exists(manifest_path)) normalize_win(manifest_path, TRUE) else NA_character_,
    stdout = normalize_win(stdout_path, must_work = TRUE),
    stderr = normalize_win(stderr_path, must_work = TRUE)
  ),
  diagnostics = list(stdout = stdout_text, stderr = stderr_text)
)

# ----------------------------------------------------------------------------  
# 10. Sommari tabellari e grafici salvati nella directory della run
# ----------------------------------------------------------------------------  

plots_dir <- file.path(output_dir, "plots")
summaries_dir <- file.path(output_dir, "summaries")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summaries_dir, recursive = TRUE, showWarnings = FALSE)

safe_numeric_summary <- function(data, table_name) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) return(NULL)
  numeric_names <- names(data)[vapply(data, is.numeric, logical(1))]
  if (length(numeric_names) == 0L) return(NULL)
  rows <- lapply(numeric_names, function(column_name) {
    values <- data[[column_name]]
    finite <- values[is.finite(values)]
    data.frame(
      table = table_name,
      variable = column_name,
      n = length(values),
      n_finite = length(finite),
      mean = if (length(finite)) mean(finite) else NA_real_,
      sd = if (length(finite) > 1L) stats::sd(finite) else NA_real_,
      min = if (length(finite)) min(finite) else NA_real_,
      median = if (length(finite)) stats::median(finite) else NA_real_,
      max = if (length(finite)) max(finite) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

run_summary <- data.frame(
  run_id = RUN_ID,
  success = isTRUE(result$success),
  latitude = LATITUDE,
  longitude = LONGITUDE,
  start_year = START_YEAR,
  end_year = END_YEAR,
  years = END_YEAR - START_YEAR + 1L,
  species_count = length(crops),
  soil_layers = length(soil_profile$Layers),
  parameter_overrides = FALSE,
  calibrated = FALSE,
  mock_weather = isTRUE(USE_MOCK_WEATHER),
  datastore_path = if (is.null(tradeoffs$datastore_path)) NA_character_ else tradeoffs$datastore_path,
  stringsAsFactors = FALSE
)
utils::write.csv(run_summary, file.path(summaries_dir, "run_summary.csv"), row.names = FALSE)
utils::write.csv(rotation_plan, file.path(summaries_dir, "rotation_plan.csv"), row.names = FALSE)
utils::write.csv(fertilisation_plan, file.path(summaries_dir, "fertilisation_plan.csv"), row.names = FALSE)
utils::write.csv(irrigation_plan, file.path(summaries_dir, "irrigation_plan.csv"), row.names = FALSE)

soil_summary <- do.call(rbind, lapply(soil_profile$Layers, as.data.frame))
utils::write.csv(soil_summary, file.path(summaries_dir, "soil_profile.csv"), row.names = FALSE)

yield_summary <- NULL
if (!is.null(result$yields) && length(result$yields) > 0L) {
  yield_values <- unlist(result$yields, use.names = TRUE)
  yield_summary <- data.frame(
    crop = names(yield_values),
    mean_yield_kg_ha = as.numeric(yield_values),
    stringsAsFactors = FALSE
  )
  utils::write.csv(yield_summary, file.path(summaries_dir, "yield_summary.csv"), row.names = FALSE)
}

numeric_summaries <- Filter(
  Negate(is.null),
  list(
    safe_numeric_summary(control, "control"),
    safe_numeric_summary(daily, "daily"),
    safe_numeric_summary(crop_summary, "crop_summary"),
    safe_numeric_summary(timeseries, "timeseries"),
    safe_numeric_summary(harvest, "harvest")
  )
)
if (length(numeric_summaries) > 0L) {
  numeric_summary <- do.call(rbind, numeric_summaries)
  utils::write.csv(
    numeric_summary,
    file.path(summaries_dir, "numeric_variables_summary.csv"),
    row.names = FALSE
  )
} else {
  numeric_summary <- NULL
}

output_inventory <- data.frame(
  object = c("request", "config", "result", "manifest", "stdout", "stderr",
             "control_csv", "daily_csv", "crop_summary_csv", "datastore"),
  path = c(
    request_path, config_path, result_path, manifest_path, stdout_path, stderr_path,
    tradeoffs$control_csv_path, tradeoffs$daily_csv_path,
    tradeoffs$crop_summary_path, tradeoffs$datastore_path
  ),
  exists = vapply(c(
    request_path, config_path, result_path, manifest_path, stdout_path, stderr_path,
    tradeoffs$control_csv_path, tradeoffs$daily_csv_path,
    tradeoffs$crop_summary_path, tradeoffs$datastore_path
  ), function(x) !is.null(x) && length(x) && !is.na(x) && file.exists(x), logical(1)),
  stringsAsFactors = FALSE
)
utils::write.csv(
  output_inventory,
  file.path(summaries_dir, "output_inventory.csv"),
  row.names = FALSE
)

plot_files <- character()

if (!is.null(yield_summary) && nrow(yield_summary) > 0L) {
  yield_plot <- file.path(plots_dir, "mean_yield_by_crop.png")
  grDevices::png(yield_plot, width = 1600, height = 1000, res = 160)
  graphics::barplot(
    yield_summary$mean_yield_kg_ha,
    names.arg = yield_summary$crop,
    col = c("#4472C4", "#ED7D31", "#70AD47")[seq_len(nrow(yield_summary))],
    ylab = "Resa media (kg/ha)",
    main = "APSIMx: resa media per coltura",
    las = 1
  )
  graphics::grid(nx = NA, ny = NULL)
  grDevices::dev.off()
  plot_files <- c(plot_files, yield_plot)
}

soil_plot <- file.path(plots_dir, "soil_profile_water_and_carbon.png")
grDevices::png(soil_plot, width = 1800, height = 1000, res = 160)
graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
mid_depth <- (soil_summary$DepthTop + soil_summary$DepthBottom) / 2
graphics::matplot(
  x = soil_summary[, c("LL15", "DUL", "SAT", "SW")],
  y = mid_depth,
  type = "o", pch = 16, lty = 1, lwd = 2,
  col = c("#C00000", "#4472C4", "#70AD47", "#7030A0"),
  ylim = rev(range(c(0, soil_summary$DepthBottom))),
  xlab = "Contenuto idrico volumetrico (m3/m3)",
  ylab = "Profondita (cm)",
  main = "Profilo idrico iniziale"
)
graphics::legend(
  "bottomright", legend = c("LL15", "DUL", "SAT", "SW"),
  col = c("#C00000", "#4472C4", "#70AD47", "#7030A0"),
  lty = 1, pch = 16, bty = "n"
)
graphics::plot(
  soil_summary$OC, mid_depth,
  type = "o", pch = 16, lwd = 2, col = "#8B4513",
  ylim = rev(range(c(0, soil_summary$DepthBottom))),
  xlab = "Carbonio organico (%)", ylab = "Profondita (cm)",
  main = "Carbonio organico del suolo"
)
grDevices::dev.off()
plot_files <- c(plot_files, soil_plot)

if (!is.null(daily) && is.data.frame(daily) && nrow(daily) > 0L) {
  date_candidates <- names(daily)[grepl("date|today", names(daily), ignore.case = TRUE)]
  numeric_candidates <- names(daily)[vapply(daily, is.numeric, logical(1))]
  preferred <- numeric_candidates[grepl(
    "yield|biomass|lai|soil.*carbon|water|sw|n2o|co2|leach",
    numeric_candidates, ignore.case = TRUE
  )]
  selected <- head(if (length(preferred)) preferred else numeric_candidates, 6L)
  if (length(selected) > 0L) {
    x <- seq_len(nrow(daily))
    xlab <- "Riga giornaliera"
    if (length(date_candidates) > 0L) {
      parsed <- suppressWarnings(as.Date(daily[[date_candidates[1L]]]))
      if (sum(!is.na(parsed)) > 0L) {
        x <- parsed
        xlab <- date_candidates[1L]
      }
    }
    daily_plot <- file.path(plots_dir, "daily_selected_variables.png")
    grDevices::png(daily_plot, width = 1800, height = 1200, res = 160)
    graphics::par(mfrow = c(ceiling(length(selected) / 2), 2), mar = c(4, 4, 3, 1))
    for (column_name in selected) {
      graphics::plot(
        x, daily[[column_name]], type = "l", lwd = 1.5,
        col = "#4472C4", xlab = xlab, ylab = column_name,
        main = column_name
      )
      graphics::grid()
    }
    grDevices::dev.off()
    plot_files <- c(plot_files, daily_plot)
  }
}

rspacing_demo$summaries <- list(
  run = run_summary,
  rotation = rotation_plan,
  fertilisation = fertilisation_plan,
  irrigation = irrigation_plan,
  yields = yield_summary,
  soil = soil_summary,
  numeric_variables = numeric_summary,
  output_inventory = output_inventory
)
# Tabelle pronte per il Data Viewer di RStudio.
rstudio_tables <- list(
  run_summary = run_summary,
  rotation_plan = rotation_plan,
  fertilisation_plan = fertilisation_plan,
  irrigation_plan = irrigation_plan,
  soil_profile = soil_summary,
  yield_summary = yield_summary,
  numeric_variables_summary = numeric_summary,
  output_inventory = output_inventory,
  daily = daily,
  control = control,
  crop_summary = crop_summary,
  timeseries = timeseries,
  harvest = harvest
)

# Grafici ggplot2 disponibili come oggetti R e come PNG/PDF esterni.
rstudio_plots <- list()
plot_file_table <- data.frame(
  plot = character(), png = character(), pdf = character(),
  stringsAsFactors = FALSE
)

save_external_plot <- function(plot_object, plot_name,
                               width = 10, height = 6, dpi = 180) {
  png_path <- file.path(plots_dir, paste0(plot_name, ".png"))
  pdf_path <- file.path(plots_dir, paste0(plot_name, ".pdf"))
  ggplot2::ggsave(png_path, plot = plot_object, width = width, height = height,
                  units = "in", dpi = dpi, bg = "white")
  ggplot2::ggsave(pdf_path, plot = plot_object, width = width, height = height,
                  units = "in", device = "pdf", bg = "white")
  plot_file_table <<- rbind(
    plot_file_table,
    data.frame(plot = plot_name, png = png_path, pdf = pdf_path,
               stringsAsFactors = FALSE)
  )
  invisible(plot_object)
}

if (!is.null(yield_summary) && nrow(yield_summary) > 0L) {
  rstudio_plots$yield_by_crop <- ggplot2::ggplot(
    yield_summary,
    ggplot2::aes(x = crop, y = mean_yield_kg_ha, fill = crop)
  ) +
    ggplot2::geom_col(width = 0.68, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = round(mean_yield_kg_ha, 0)),
                       vjust = -0.4, size = 4) +
    ggplot2::labs(title = "APSIMx: resa media per coltura",
                  x = "Coltura", y = "Resa media (kg/ha)") +
    ggplot2::theme_minimal(base_size = 13)
  save_external_plot(rstudio_plots$yield_by_crop, "yield_by_crop")
}

soil_long <- rbind(
  data.frame(mid_depth = mid_depth, variable = "LL15", value = soil_summary$LL15),
  data.frame(mid_depth = mid_depth, variable = "DUL", value = soil_summary$DUL),
  data.frame(mid_depth = mid_depth, variable = "SAT", value = soil_summary$SAT),
  data.frame(mid_depth = mid_depth, variable = "SW", value = soil_summary$SW)
)
rstudio_plots$soil_water_profile <- ggplot2::ggplot(
  soil_long, ggplot2::aes(x = value, y = mid_depth, colour = variable)
) +
  ggplot2::geom_path(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_y_reverse() +
  ggplot2::labs(title = "Profilo idrico iniziale del suolo",
                x = "Contenuto idrico volumetrico (m3/m3)",
                y = "Profondita (cm)", colour = "Variabile") +
  ggplot2::theme_minimal(base_size = 13)
save_external_plot(rstudio_plots$soil_water_profile,
                   "soil_water_profile", width = 9, height = 7)

rstudio_plots$soil_organic_carbon <- ggplot2::ggplot(
  soil_summary, ggplot2::aes(x = OC, y = mid_depth)
) +
  ggplot2::geom_path(linewidth = 1, colour = "#8B4513") +
  ggplot2::geom_point(size = 2.5, colour = "#8B4513") +
  ggplot2::scale_y_reverse() +
  ggplot2::labs(title = "Carbonio organico del suolo",
                x = "Carbonio organico (%)", y = "Profondita (cm)") +
  ggplot2::theme_minimal(base_size = 13)
save_external_plot(rstudio_plots$soil_organic_carbon,
                   "soil_organic_carbon", width = 8, height = 7)

rstudio_plots$nitrogen_plan <- ggplot2::ggplot(
  fertilisation_plan,
  ggplot2::aes(x = Days_after_sowing, y = Amount_kg_N_ha,
               colour = Species, shape = Trigger)
) +
  ggplot2::geom_segment(
    ggplot2::aes(xend = Days_after_sowing, y = 0, yend = Amount_kg_N_ha),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(size = 4) +
  ggplot2::facet_wrap(~ Species, scales = "free_x") +
  ggplot2::labs(title = "Piano di fertilizzazione azotata",
                subtitle = "UreaN; Soybean non riceve azoto minerale",
                x = "Giorni dopo la semina", y = "Dose (kg N/ha)",
                colour = "Specie", shape = "Trigger") +
  ggplot2::theme_minimal(base_size = 12)
save_external_plot(rstudio_plots$nitrogen_plan,
                   "fertilisation_plan", width = 11, height = 6)

irrigation_long <- rbind(
  data.frame(Species = irrigation_plan$Species,
             measure = "Threshold", fraction = irrigation_plan$Threshold_fraction),
  data.frame(Species = irrigation_plan$Species,
             measure = "Target", fraction = irrigation_plan$Target_fraction)
)
rstudio_plots$irrigation_settings <- ggplot2::ggplot(
  irrigation_long,
  ggplot2::aes(x = Species, y = fraction, fill = measure)
) +
  ggplot2::geom_col(position = "dodge", width = 0.7) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", fraction)),
    position = ggplot2::position_dodge(width = 0.7), vjust = -0.35
  ) +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::labs(title = "Soglie e target di irrigazione",
                x = "Specie", y = "Frazione dell'acqua disponibile",
                fill = "Parametro") +
  ggplot2::theme_minimal(base_size = 13)
save_external_plot(rstudio_plots$irrigation_settings,
                   "irrigation_settings", width = 9, height = 6)

if (!is.null(daily) && is.data.frame(daily) && nrow(daily) > 0L) {
  daily_numeric <- names(daily)[vapply(daily, is.numeric, logical(1))]
  daily_preferred <- daily_numeric[grepl(
    "yield|biomass|lai|soil.*carbon|water|sw|n2o|co2|leach",
    daily_numeric, ignore.case = TRUE
  )]
  daily_selected <- head(if (length(daily_preferred)) daily_preferred else daily_numeric, 6L)
  if (length(daily_selected) > 0L) {
    daily_plot_data <- do.call(rbind, lapply(daily_selected, function(column_name) {
      data.frame(index = seq_len(nrow(daily)), variable = column_name,
                 value = daily[[column_name]], stringsAsFactors = FALSE)
    }))
    rstudio_plots$daily_selected_variables <- ggplot2::ggplot(
      daily_plot_data, ggplot2::aes(x = index, y = value)
    ) +
      ggplot2::geom_line(linewidth = 0.6, colour = "#4472C4") +
      ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
      ggplot2::labs(title = "Variabili giornaliere APSIMx selezionate",
                    x = "Indice temporale", y = NULL) +
      ggplot2::theme_minimal(base_size = 11)
    save_external_plot(rstudio_plots$daily_selected_variables,
                       "daily_selected_variables", width = 12, height = 9)
  }
}

# File RDS completi per riaprire tabelle e grafici senza ripetere APSIMx.
tables_rds_path <- file.path(summaries_dir, "rspacing_tables_complete.rds")
plots_rds_path <- file.path(plots_dir, "rspacing_plots_complete.rds")
saveRDS(rstudio_tables, tables_rds_path)
saveRDS(rstudio_plots, plots_rds_path)
utils::write.csv(plot_file_table,
                 file.path(plots_dir, "plot_file_inventory.csv"),
                 row.names = FALSE)

rspacing_demo$tables_rstudio <- rstudio_tables
rspacing_demo$plots_rstudio <- rstudio_plots
rspacing_demo$plot_files <- plot_file_table
rspacing_demo$plots <- unique(c(plot_files, plot_file_table$png, plot_file_table$pdf))
rspacing_demo$files$tables_rds <- normalize_win(tables_rds_path, TRUE)
rspacing_demo$files$plots_rds <- normalize_win(plots_rds_path, TRUE)
rspacing_demo$files$summaries_directory <- normalize_win(summaries_dir, TRUE)
rspacing_demo$files$plots_directory <- normalize_win(plots_dir, TRUE)
assign("rspacing_demo", rspacing_demo, envir = .GlobalEnv)

# ----------------------------------------------------------------------------  
# 11. Riepilogo in console e grafico immediato
# ----------------------------------------------------------------------------  

cat("\n============================================================\n")
cat("rSpacing / APSIMx: RUN COMPLETATA\n")
cat("============================================================\n")
cat("Run ID:       ", RUN_ID, "\n", sep = "")
cat("Coordinate:   ", LATITUDE, ", ", LONGITUDE, "\n", sep = "")
cat("Periodo:      ", START_YEAR, "-", END_YEAR, "\n", sep = "")
cat("Specie:       Wheat, Maize, Soybean\n")
cat("Cultivar:     Hartog, B_110, Generic_MG1\n")
cat("Success:      ", isTRUE(result$success), "\n", sep = "")
cat("Run directory:", normalize_win(run_dir, TRUE), "\n")

if (!is.null(result$yields) && length(result$yields) > 0L) {
  yields <- unlist(result$yields, use.names = TRUE)
  cat("\nRese medie restituite da APSIMx (kg/ha):\n")
  print(yields)
  if (length(yields) > 0L && any(is.finite(yields))) {
    barplot(
      yields,
      las = 2,
      ylab = "Resa media (kg/ha)",
      main = "APSIMx: rese medie per coltura"
    )
  }
}

cat("\nOggetto disponibile in RStudio: rspacing_demo\n")
cat("Esempi:\n")
cat("  str(rspacing_demo, max.level = 2)\n")
cat("  rspacing_demo$result\n")
cat("  rspacing_demo$fertilisation_plan\n")
cat("  rspacing_demo$irrigation_plan\n")
cat("  rspacing_demo$tables_rstudio\n")
cat("  rspacing_demo$plots_rstudio\n")
cat("  rspacing_demo$plot_files\n")
cat("  rspacing_demo$tables$daily\n")
cat("  rspacing_demo$tables$crop_summary\n")
cat("  rspacing_demo$sqlite$tables\n")
cat("============================================================\n")

cat("\nTabella: piano colturale\n")
print(rotation_plan, row.names = FALSE)
cat("\nTabella: fertilizzazione\n")
print(fertilisation_plan, row.names = FALSE)
cat("\nTabella: irrigazione\n")
print(irrigation_plan, row.names = FALSE)
cat("\nInventario grafici esterni\n")
print(plot_file_table, row.names = FALSE)

# Gli oggetti ggplot vengono inviati al pannello Plots di RStudio.
if (length(rstudio_plots) > 0L) {
  invisible(lapply(rstudio_plots, print))
}

invisible(rspacing_demo)
