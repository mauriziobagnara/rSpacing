# rSpacing

An R interface for the custom `Spacing.Tool2` and APSIM Next Generation workflow.


## Install rSpacing with the vignette

The package vignette is built from `vignettes/rSpacing-workflow_EN.Rmd` and uses the bundled reference fixture in `inst/extdata/reference_maize/`. Building the vignette does **not** run APSIMx or `Spacing.Tool2`.

A standard `devtools::install()` call does not build vignettes unless this is requested explicitly. From the root directory of the `rSpacing` source package, install the package with:

```r
install.packages(c(
  "devtools",
  "knitr",
  "rmarkdown"
))

devtools::install(
  pkg = ".",
  build = TRUE,
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = FALSE
)
```

Restart R after installation, then verify the installed package and open the manual:

```r
library(rSpacing)

packageVersion("rSpacing")
vignette(package = "rSpacing")
vignette("rSpacing-workflow_EN", package = "rSpacing")
```

The expected package version is:

```text
0.10.05
```

The vignette is listed as:

```text
rSpacing user manual: workflow with real APSIMx results
```

To browse all installed vignettes interactively:

```r
browseVignettes(package = "rSpacing")
```

### Recommended clean source-package installation

For the closest test of what another user will receive, first build the source tarball with the vignette and then install that tarball:

```r
tarball <- devtools::build(
  pkg = ".",
  vignettes = TRUE,
  manual = FALSE
)

install.packages(
  tarball,
  repos = NULL,
  type = "source"
)
```

After restarting R:

```r
library(rSpacing)
packageVersion("rSpacing")
vignette(package = "rSpacing")
```

### Verify vignette creation before installation

If the vignette does not appear after installation, test its construction directly from the package root:

```r
tools::buildVignettes(dir = ".")
```

Then inspect the installed package:

```r
pkg <- find.package("rSpacing")

file.exists(file.path(pkg, "Meta", "vignette.rds"))
list.files(file.path(pkg, "doc"), full.names = TRUE)
```

A successful installation with the vignette should contain the vignette index in `Meta/vignette.rds` and the rendered manual under the installed package's `doc/` directory.

### Important source files

Keep these files in the source package:

```text
vignettes/rSpacing-workflow_EN.Rmd
inst/extdata/reference_maize/
```

The generated `vignettes/rSpacing-workflow_EN.html` file is not required in the source tree because the HTML manual is rebuilt from the Rmd during package build.

## Minimal intended use

```r
library(rSpacing)

cfg <- spacing_config(
  repository = "C:/Users/maurizio.bagnara/Documents/spacing",
  project = "apsim/src/Spacing.Tool2",
  config_file = "apsim/spacing.config.json"
)

request <- new_spacing_request(
  latitude = 45.4,
  longitude = 9.2,
  rotation = "Maize",
  mock = TRUE
)

run <- run_spacing(cfg, request)
result <- read_spacing(run)
```

Version 0.1 implements the orchestration core. Weather, soil, rotation and
report builders are planned as typed extensions after the mock execution path
has been tested against the actual repository.


## Development checks

The repository contains GitHub Actions workflows for:

- `R CMD check` on Linux, Windows, macOS and R-devel;
- deterministic regeneration of Roxygen documentation;
- unit-test coverage with a downloadable Cobertura report.

The CI unit tests do not require APSIMx or .NET. A real `--mock` Tool2 run is
an integration test and should be executed in an environment configured with
the custom C# repository.

Regenerate documentation and run local checks with:

```r
roxygen2::roxygenise()
testthat::test_local()
rcmdcheck::rcmdcheck(args = "--no-manual")
```

### Optional integration test

A runner with the custom C# repository can activate the real mock test with
`RSPACING_INTEGRATION=true` and `RSPACING_REPOSITORY=<path>`. It is skipped on
standard public CI workers.

## Preparing `spacing.config.json`

```r
values <- new_spacing_config_values(
  db_api_base_url = "http://localhost:8000/api/v1",
  db_api_key = Sys.getenv("SPACING_DB_API_KEY"),
  temp_weather_dir = "C:/spacing/work/weather",
  output_dir = "C:/spacing/results",
  start_year = 1990,
  end_year = 2023,
  max_parallelism = 4
)

write_spacing_config_file(
  values,
  "C:/Users/user/Documents/spacing/apsim/spacing.config.json",
  overwrite = FALSE
)

setup <- prepare_spacing_runtime(
  repository = "C:/Users/user/Documents/spacing",
  config = values,
  overwrite = TRUE
)

run <- run_spacing(
  setup$runtime,
  new_spacing_request(45.4, 9.2, "Maize", mock = TRUE)
)
```

Unknown JSON properties required by a custom C# build can be retained in
`additional` or preserved by `update_spacing_config_file()`.

## Creating and modifying APSIM `.met` files

```r
weather <- data.frame(
  date = seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day"),
  radn = 15,
  maxt = 25,
  mint = 12,
  rain = 0
)

met <- new_spacing_met(
  data = weather,
  latitude = 45.4,
  longitude = 9.2,
  site = "Example site"
)

met <- set_spacing_met_column(met, "wind", 2.5, "m/s")
met <- set_spacing_met_constant(met, "co2", 420, "ppm")
met <- subset_spacing_met(met, "2000-03-01", "2000-10-31")

met_file <- write_spacing_met(
  met,
  "C:/spacing/work/weather/example.met",
  overwrite = TRUE
)
```

Raw units can be converted explicitly:

```r
met <- prepare_spacing_met(
  raw_weather,
  latitude = 45.4,
  column_map = c(maxt = "tmax_k", mint = "tmin_k"),
  conversions = list(
    maxt = list(offset = -273.15),
    mint = list(offset = -273.15),
    radn = list(multiplier = 1e-6)
  )
)
```

## Compatibility with Tool2 weather files

Version 0.04 reads APSIM weather constants case-insensitively. Files containing
`Latitude`, `TAV`, or `AMP` are normalised internally to `latitude`, `tav`, and
`amp`, so they can be validated and plotted directly.

## Inspecting and plotting simulation results

Version 0.03 adds stable accessors for the standard CSV outputs. File-specific
cell and scenario suffixes do not need to be known in advance.

```r
daily <- spacing_daily(result)
annual <- spacing_report(result)
crop_summary <- spacing_crop_summary(result)

inventory_spacing_result(result)
classify_spacing_variables(names(daily))
```

A present but empty table, such as a crop summary with zero rows, remains a
valid data frame by default. Set `allow_empty = FALSE` to convert this condition
into an explicit error or warning, depending on `strict`.

All plotting functions return `ggplot` objects:

```r
plot_spacing_met(result)
plot_spacing_daily(result, "SoilSWtot_mm", x = "doy")
plot_spacing_daily(result, "Runoff_mm", x = "year", summary = "sum")
plot_spacing_report(result, "Maize_GrainYield_kgha")

plot(result, source = "weather")
plot(result, source = "daily", variable = "Maize_LAI", x = "doy")
```

Axis labels use units stored in the APSIM `_Units` table when available.
Variable classification uses exact names and anchored patterns, avoiding
accidental matches such as `Grain` being interpreted as rain.


## Multi-figure panels

Version 0.05 creates one coherent panel for each output variable:

```r
# Daily output: full series, annual summary, and DOY climatology
plot_spacing_daily_panel(result, "SoilSWtot_mm")

# Annual Report: time series, distribution, and anomalies
plot_spacing_report_panel(result, "Maize_GrainYield_kgha")

# Multiple variables returned as a named list of panels
panels <- plot_spacing_panels(
  result,
  source = "daily",
  variables = c("SoilSWtot_mm", "LeachNO3_kgNha", "Maize_LAI")
)

panels[["SoilSWtot_mm"]]
```

`plot_spacing_result()` and `plot(result, ...)` use panels by default. Set
`panel = FALSE` to obtain the historical single figure.



## Variable meaning and result summary (v0.06)

```r
summary(result)

spacing_variable_dictionary(result)

describe_spacing_variable(
  "N2O_total_kgNha",
  source = "daily",
  result = result
)
```

The integrated dictionary records the APSIMx expression, readable title, scientific description, unit, reporting event, stock/flow role, recommended annual summary, and soil-layer depth. Plot functions use these labels automatically.


### Mapping simulation locations (v0.07)

```r
# Standardise metadata from requests, runs, results, lists, or data frames
spacing_coordinates(result)

# Static map suitable for plots and reports
plot_spacing_location(result, type = "static")

# Interactive OpenStreetMap widget (optional leaflet dependency)
plot_spacing_location(result, type = "interactive")

# Compare several simulations
plot_spacing_map(
  list(Milan = result_milan, Orvieto = result_orvieto),
  type = "static",
  colour_by = "rotation"
)
```

Interactive tiles generally require Internet access. Set `basemap = FALSE` for
an interactive marker canvas without web tiles. The `leaflet` and `maps`
packages are optional and listed under Suggests.
