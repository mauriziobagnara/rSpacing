
make_test_repository <- function() {
  td <- tempfile("rSpacing-"); dir.create(td)
  dir.create(file.path(td,"apsim","src","Spacing.Tool2"), recursive=TRUE)
  cfg <- file.path(td,"apsim","spacing.config.json")
  jsonlite::write_json(list(OutputDir=file.path(td,"out"), TempWeatherDir=file.path(td,"weather")), cfg, auto_unbox=TRUE)
  td
}
