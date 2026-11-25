#' Find a column by candidate names or patterns
#' @param data Data frame.
#' @param candidates Exact names tried first.
#' @param patterns Case-insensitive regular expressions tried next.
#' @param numeric Logical. Accept numeric columns only.
#' @param required Logical. Error (rather than `NULL`) when nothing matches.
#' @param label Description for the error message.
#' @return A column name, or `NULL`.
#' @noRd
.spacing_find_column <- function(data, candidates = character(), patterns = character(),
                                 numeric = FALSE, required = TRUE, label = "column") {
  if (!is.data.frame(data)) .rspacing_abort("Expected a data frame.", "spacing_schema_error")
  hits <- candidates[candidates %in% names(data)]
  if (!length(hits) && length(patterns)) {
    hits <- unique(unlist(lapply(patterns, function(pattern) {
      grep(pattern, names(data), value = TRUE, ignore.case = TRUE)
    }), use.names = FALSE))
  }
  if (numeric && length(hits)) hits <- hits[vapply(data[hits], is.numeric, logical(1))]
  if (!length(hits)) {
    if (required) .rspacing_abort(sprintf("No %s was found.", label), "spacing_schema_error")
    return(NA_character_)
  }
  hits[[1L]]
}

#' Identifier of a result for labels
#' @param result A `spacing_result`.
#' @param fallback Label when no scenario identifier is recorded.
#' @return Character scalar.
#' @noRd
.spacing_result_id <- function(result, fallback = "run") {
  result$run$request$scenario_id %||% result$run$request$scenario %||% result$run$request$id %||%
    result$run$request$run_id %||% result$run$run_id %||% fallback
}

#' Crop name from a prefixed variable
#' @param variable Variable name such as `Maize_GrainYield_kgha`.
#' @return The prefix before the first underscore, or `NA`.
#' @noRd
.spacing_crop_from_variable <- function(variable) {
  sub("_.*$", "", variable)
}

#' Harvest year of each row of a crop summary or report table
#' @param data Data frame.
#' @return Integer years from `Year`, else from `HarvestDate` or `Clock.Today`;
#'   `NULL` when none of these exists.
#' @noRd
.spacing_row_year <- function(data) {
  col <- .spacing_find_column(data, c("Year", "year", "HarvestYear"), c("^year$"), FALSE, FALSE)
  if (!is.null(col) && !is.na(col)) return(as.integer(data[[col]]))
  for (date_col in c("HarvestDate", "Clock.Today")) if (date_col %in% names(data))
    return(as.integer(substr(as.character(data[[date_col]]), 1, 4)))
  NULL
}

#' Build a plot atlas for one result
#'
#' Builds plots for every numeric daily and report variable and for every
#' layered soil variable. A variable that cannot be plotted is recorded in the
#' `errors` attribute instead of stopping the atlas.
#'
#' @param result A `spacing_result`.
#' @param sources Character vector. Any of `"daily"`, `"report"` and `"soil"`.
#' @param variables Optional character vector of variable (or soil base)
#'   names; `NULL` selects all.
#' @param daily_x Character. Horizontal scale of daily plots, passed to
#'   [plot_spacing_daily()]: `"date"`, `"year"` or `"doy"`.
#' @param soil_types Character vector. Soil plot types: `"time_depth"` and/or
#'   `"profile"`.
#' @param max_plots Numeric. Maximum number of plots per source.
#' @return A named list of `ggplot` objects (names `source/variable`), with an
#'   `errors` attribute: a named list of error messages (empty when all
#'   plots were built).
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' atlas <- plot_spacing_atlas(result, sources = "report")
#' names(atlas)
#' attr(atlas, "errors")
plot_spacing_atlas <- function(result,
                               sources = c("daily", "report", "soil"),
                               variables = NULL,
                               daily_x = c("date", "year", "doy"),
                               soil_types = c("time_depth", "profile"),
                               max_plots = Inf) {
  .validate_spacing_result(result)
  sources <- match.arg(sources, c("daily", "report", "soil"), several.ok = TRUE)
  daily_x <- match.arg(daily_x)
  soil_types <- match.arg(soil_types, c("time_depth", "profile"), several.ok = TRUE)
  if (!is.numeric(max_plots) || length(max_plots) != 1L || is.na(max_plots) || max_plots < 1)
    .rspacing_abort("'max_plots' must be one positive number.", "spacing_validation_error")
  plots <- list(); errors <- list()
  add_plot <- function(key, expression) {
    value <- tryCatch(expression, error = identity)
    if (inherits(value, "error")) errors[[key]] <<- conditionMessage(value)
    else plots[[key]] <<- value
  }
  keep <- function(x) {
    if (!is.null(variables)) x <- intersect(x, variables)
    utils::head(x, max_plots)
  }
  if ("daily" %in% sources) {
    data <- spacing_daily(result, strict = TRUE, allow_empty = FALSE)
    ids <- tolower(names(data)) %in% c("year", "doy", "day", "month", "simulationid", "checkpointid")
    available <- names(data)[vapply(data, is.numeric, logical(1)) & !ids]
    for (variable in keep(available)) add_plot(
      paste0("daily/", variable),
      plot_spacing_daily(result, variable = variable, x = daily_x)
    )
  }
  if ("report" %in% sources) {
    data <- spacing_report(result, strict = FALSE, allow_empty = FALSE)
    if (!is.null(data)) {
      ids <- tolower(names(data)) %in% c("year", "doy", "day", "month", "simulationid", "checkpointid")
      available <- names(data)[vapply(data, is.numeric, logical(1)) & !ids]
      for (variable in keep(available)) add_plot(
        paste0("report/", variable),
        plot_spacing_report(result, variable = variable)
      )
    }
  }
  if ("soil" %in% sources) {
    data <- spacing_daily(result, strict = TRUE, allow_empty = FALSE)
    indexed <- grep("\\([0-9]+\\)$", names(data), value = TRUE)
    bases <- unique(sub("\\([0-9]+\\)$", "", indexed))
    bases <- bases[vapply(bases, function(base) {
      sum(sub("\\([0-9]+\\)$", "", indexed) == base) >= 2L
    }, logical(1))]
    for (variable in keep(bases)) for (type in soil_types) add_plot(
      paste0("soil/", variable, "/", type),
      plot_spacing_soil_profile(result, variable = variable, type = type)
    )
  }
  class(plots) <- c("spacing_plot_atlas", "list")
  attr(plots, "errors") <- errors
  plots
}

#' Plot simulated yield
#'
#' Finds the yield column (crop summary first, then the report table; names
#' such as `Yield_kgha` or `<Crop>_GrainYield_kgha`) and plots it by year,
#' coloured by crop or cultivar where those are known.
#'
#' @param result A `spacing_result`.
#' @param by Character vector. Grouping among `"year"`, `"crop"` and
#'   `"cultivar"`.
#' @param yield_variable Optional character scalar. Yield column to use.
#' @param title Optional plot title.
#' @return A `ggplot` object; attribute `data` holds the yield table used.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' p <- suppressWarnings(plot_spacing_yield(result))
#' head(attr(p, "data"))
plot_spacing_yield <- function(result, by = c("year", "crop", "cultivar"),
                               yield_variable = NULL, title = NULL) {
  .validate_spacing_result(result)
  by <- match.arg(by, c("year", "crop", "cultivar"), several.ok = TRUE)
  data <- spacing_crop_summary(result, strict = FALSE, allow_empty = FALSE)
  if (is.null(data)) data <- spacing_report(result, strict = TRUE, allow_empty = FALSE)
  if (is.null(yield_variable)) yield_variable <- .spacing_find_column(
    data,
    candidates = c("Yield_kgha", "Yield", "GrainYield_kgha", "Maize_GrainYield_kgha"),
    patterns = c("yield", "grain.*(wt|weight)"), numeric = TRUE, label = "numeric yield column"
  )
  if (!yield_variable %in% names(data) || !is.numeric(data[[yield_variable]]))
    .rspacing_abort("The selected yield variable is missing or not numeric.", "spacing_schema_error")
  years <- .spacing_row_year(data)
  crop_col <- .spacing_find_column(data, c("Crop", "crop", "CropName", "PlantName"), c("crop", "plant"), FALSE, FALSE)
  cultivar_col <- .spacing_find_column(data, c("Cultivar", "cultivar", "Variety"), c("cultivar", "variety"), FALSE, FALSE)
  normal <- data.frame(
    year = if (!is.null(years)) years else seq_len(nrow(data)),
    crop = if (!is.na(crop_col)) as.character(data[[crop_col]]) else .spacing_crop_from_variable(yield_variable),
    cultivar = if (!is.na(cultivar_col)) as.character(data[[cultivar_col]]) else NA_character_,
    yield_kg_ha = as.numeric(data[[yield_variable]]), stringsAsFactors = FALSE
  )
  normal <- normal[is.finite(normal$yield_kg_ha), , drop = FALSE]
  if (!nrow(normal)) .rspacing_abort("No finite yield values are available.", "spacing_empty_table_error")
  x_name <- by[[1L]]
  fill_name <- if (length(by) >= 2L) by[[2L]] else "crop"
  if (x_name == "cultivar" && all(is.na(normal$cultivar))) x_name <- "crop"
  if (fill_name == "cultivar" && all(is.na(normal$cultivar))) fill_name <- "crop"
  p <- ggplot2::ggplot(normal, ggplot2::aes(x = !!rlang::sym(x_name), y = yield_kg_ha, fill = !!rlang::sym(fill_name))) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = title %||% "Simulated yield", x = x_name,
                  y = "Yield (kg/ha)", fill = fill_name)
  if (length(by) >= 3L) {
    facet_name <- by[[3L]]
    if (!(facet_name == "cultivar" && all(is.na(normal$cultivar))))
      p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_name)), scales = "free_x")
  }
  attr(p, "data") <- normal
  p
}

#' Plot irrigation applied by APSIM
#'
#' Plots daily irrigation (column `Irrig_mm` or a column matching
#' irrigation), optionally with its cumulative total.
#'
#' @param result A `spacing_result`.
#' @param cumulative Logical. Add the cumulative amount.
#' @param facet_by_year Logical. One facet per calendar year.
#' @param variable Optional character scalar. Irrigation column (mm).
#' @return A `ggplot` object; attribute `data` holds a data frame with `date`,
#'   `year`, daily and cumulative irrigation (mm).
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' p <- plot_spacing_irrigation(result, facet_by_year = FALSE)
#' tail(attr(p, "data"), 3)
plot_spacing_irrigation <- function(result, cumulative = TRUE,
                                    facet_by_year = TRUE, variable = NULL) {
  data <- spacing_daily(result, strict = TRUE, allow_empty = FALSE)
  if (is.null(variable)) variable <- .spacing_find_column(
    data, c("Irrig_mm", "Irrigation_mm", "Irrigation"),
    c("irrig"), numeric = TRUE, label = "irrigation column"
  )
  dates <- .spacing_daily_dates(data)
  out <- data.frame(date = dates, irrigation_mm = as.numeric(data[[variable]]))
  out$irrigation_mm[is.na(out$irrigation_mm)] <- 0
  out$cumulative_mm <- cumsum(out$irrigation_mm)
  out$year <- as.integer(format(out$date, "%Y"))
  if (isTRUE(cumulative)) {
    long <- rbind(
      data.frame(date=out$date, year=out$year, metric="Daily", value=out$irrigation_mm),
      data.frame(date=out$date, year=out$year, metric="Cumulative", value=out$cumulative_mm)
    )
    p <- ggplot2::ggplot(long, ggplot2::aes(date, value, colour = metric)) +
      ggplot2::geom_line() + ggplot2::labs(x="Date", y="Irrigation (mm)", colour=NULL)
  } else {
    p <- ggplot2::ggplot(out, ggplot2::aes(date, irrigation_mm)) +
      ggplot2::geom_col(fill="steelblue", width=1) + ggplot2::labs(x="Date", y="Irrigation (mm)")
  }
  if (isTRUE(facet_by_year)) p <- p + ggplot2::facet_wrap(~year, scales="free_x", ncol=1)
  p <- p + ggplot2::labs(title="APSIMx irrigation", subtitle=paste("Total:", round(sum(out$irrigation_mm), 1), "mm"))
  attr(p, "data") <- out
  p
}

#' Plot nitrogen fertiliser applied by APSIM
#'
#' Plots daily applied fertiliser N, optionally with its cumulative total.
#' The column is found among `FertMineralN_kgNha`, `FertiliserN_kgNha`,
#' `FertilizerN_kgNha`, `NApplied_kgNha` and `FertN_kgNha`, or by pattern;
#' an error is raised when the daily table has none.
#'
#' @param result A `spacing_result`.
#' @param cumulative Logical. Add the cumulative amount.
#' @param facet_by_year Logical. One facet per calendar year.
#' @param variable Optional character scalar. Applied-N column (kg N/ha).
#' @return A `ggplot` object; attribute `data` holds a data frame with
#'   `date`, `nitrogen_kg_ha`, `cumulative_kg_ha` and `year`.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' # The reference run has no fertiliser column: add one to illustrate.
#' i <- grep("^daily", names(result$csv))
#' d <- result$csv[[i]]
#' d$FertN_kgNha <- ifelse(format(as.Date(d$Clock.Today), "%m-%d") == "04-20", 150, 0)
#' result$csv[[i]] <- d
#' p <- plot_spacing_fertilisation(result, facet_by_year = FALSE)
#' max(attr(p, "data")$cumulative_kg_ha)
plot_spacing_fertilisation <- function(result, cumulative = TRUE,
                                       facet_by_year = TRUE, variable = NULL) {
  data <- spacing_daily(result, strict = TRUE, allow_empty = FALSE)
  if (is.null(variable)) variable <- .spacing_find_column(
    data,
    candidates=c("FertMineralN_kgNha", "FertiliserN_kgNha", "FertilizerN_kgNha", "NApplied_kgNha", "FertN_kgNha"),
    patterns=c("fertili[sz].*n", "n.*applied", "fert.*kg"),
    numeric=TRUE, label="applied fertiliser-N column"
  )
  dates <- .spacing_daily_dates(data)
  out <- data.frame(date=dates, nitrogen_kg_ha=as.numeric(data[[variable]]))
  out$nitrogen_kg_ha[is.na(out$nitrogen_kg_ha)] <- 0
  out$cumulative_kg_ha <- cumsum(out$nitrogen_kg_ha)
  out$year <- as.integer(format(out$date, "%Y"))
  if (isTRUE(cumulative)) {
    long <- rbind(
      data.frame(date=out$date,year=out$year,metric="Daily",value=out$nitrogen_kg_ha),
      data.frame(date=out$date,year=out$year,metric="Cumulative",value=out$cumulative_kg_ha)
    )
    p <- ggplot2::ggplot(long, ggplot2::aes(date,value,colour=metric)) +
      ggplot2::geom_line() + ggplot2::labs(x="Date",y="Nitrogen (kg N/ha)",colour=NULL)
  } else {
    p <- ggplot2::ggplot(out,ggplot2::aes(date,nitrogen_kg_ha)) +
      ggplot2::geom_col(fill="darkgreen",width=1) + ggplot2::labs(x="Date",y="Nitrogen (kg N/ha)")
  }
  if (isTRUE(facet_by_year)) p <- p + ggplot2::facet_wrap(~year,scales="free_x",ncol=1)
  p <- p + ggplot2::labs(title="APSIMx fertilisation",subtitle=paste("Total:",round(sum(out$nitrogen_kg_ha),1),"kg N/ha"))
  attr(p,"data") <- out
  p
}

#' Plot soil carbon stock, change or profile
#'
#' @param result A `spacing_result`.
#' @param type Character. `"change"` (stock minus the initial stock),
#'   `"stock"`, `"percent_change"`, `"profile"` or `"time_depth"` (the last two
#'   use [plot_spacing_soil_profile()]).
#' @param total_variable Optional character scalar. Total soil-carbon column
#'   (default: found automatically, e.g. `SoilCtot_kgCha`).
#' @param profile_variable Optional character scalar. Layered soil-carbon base
#'   name (default e.g. `SoilC_kgCha`).
#' @param thickness Optional layer thicknesses (mm) for the profile types; see
#'   [spacing_soil_layers()].
#' @return A `ggplot` object; for the stock types, attribute `data` holds the
#'   daily carbon table used.
#' @family plots
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' plot_spacing_soil_carbon(result, type = "change")
#' plot_spacing_soil_carbon(result, type = "profile",
#'   thickness = c(150, 150, 300, 300, 300, 300))
plot_spacing_soil_carbon <- function(result,
                                     type = c("change", "stock", "percent_change", "profile", "time_depth"),
                                     total_variable = NULL,
                                     profile_variable = NULL,
                                     thickness = NULL) {
  type <- match.arg(type)
  data <- spacing_daily(result, strict=TRUE, allow_empty=FALSE)
  if (type %in% c("profile", "time_depth")) {
    indexed <- grep("\\([0-9]+\\)$", names(data), value=TRUE)
    bases <- unique(sub("\\([0-9]+\\)$", "", indexed))
    if (is.null(profile_variable)) {
      candidates <- bases[grepl("soilc|carbon", bases, ignore.case=TRUE) & !grepl("co2", bases, ignore.case=TRUE)]
      if (!length(candidates)) .rspacing_abort("No indexed soil-carbon variable was found.", "spacing_schema_error")
      profile_variable <- candidates[[1L]]
    }
    return(plot_spacing_soil_profile(result, variable=profile_variable,
                                     thickness=thickness, type=type))
  }
  if (is.null(total_variable)) total_variable <- .spacing_find_column(
    data, c("SoilCtot_kgCha", "SoilC_total_kgCha", "TotalSoilC_kgCha"),
    c("soilc.*tot", "total.*soil.*c"), numeric=TRUE,
    label="total soil-carbon column"
  )
  dates <- .spacing_daily_dates(data)
  stock <- as.numeric(data[[total_variable]])
  out <- data.frame(date=dates, stock_kgC_ha=stock)
  out$change_kgC_ha <- stock - stock[[1L]]
  out$percent_change <- 100 * (stock / stock[[1L]] - 1)
  y <- switch(type, stock=out$stock_kgC_ha, change=out$change_kgC_ha,
              percent_change=out$percent_change)
  ylab <- switch(type, stock="Soil C stock (kg C/ha)",
                 change="Change from baseline (kg C/ha)",
                 percent_change="Change from baseline (%)")
  plot_data <- data.frame(date=dates, value=y)
  p <- ggplot2::ggplot(plot_data,ggplot2::aes(date,value)) +
    ggplot2::geom_hline(yintercept=if(type=="stock") NA_real_ else 0,colour="grey60") +
    ggplot2::geom_line(colour="saddlebrown",linewidth=.8) +
    ggplot2::labs(title="Soil carbon",x="Date",y=ylab)
  attr(p,"data") <- out
  p
}

#' Compare one output variable across runs
#'
#' Aggregates one variable for each result (by the chosen fields) and draws
#' the values side by side, e.g. the yield of several scenarios.
#'
#' @param results A list of `spacing_result` objects, or one result. List
#'   names, when given, label the scenarios; otherwise the scenario identifier
#'   is used.
#' @param variable Character scalar. Variable name, or `"Yield"` to find the
#'   yield column automatically.
#' @param source Character. Table to read: `"auto"` (crop summary, then
#'   report, then daily), `"crop_summary"`, `"report"` or `"daily"`.
#' @param by Character vector. Grouping among `"scenario_id"`, `"year"`,
#'   `"crop"` and `"cultivar"`.
#' @param statistic Character. Aggregation: `"auto"`, `"sum"`, `"mean"`,
#'   `"last"` or `"max"`.
#' @return A `ggplot` object; attribute `data` holds the comparison table.
#' @family plots
#' @family scenarios
#' @export
#' @examples
#' result <- readRDS(system.file("extdata", "reference_maize", "result.rds",
#'   package = "rSpacing"))
#' p <- suppressWarnings(plot_spacing_comparison(list(a = result, b = result),
#'   by = c("scenario_id", "year")))
#' head(attr(p, "data"))
plot_spacing_comparison <- function(results, variable = "Yield",
                                    source = c("auto", "crop_summary", "report", "daily"),
                                    by = c("scenario_id", "crop", "cultivar"),
                                    statistic = c("auto", "sum", "mean", "last", "max")) {
  if (inherits(results,"spacing_result")) results <- list(results)
  if (!is.list(results) || !length(results) ||
      !all(vapply(results,inherits,logical(1),"spacing_result")))
    .rspacing_abort("'results' must contain spacing_result objects.", "spacing_validation_error")
  source <- match.arg(source); statistic <- match.arg(statistic)
  by <- match.arg(by,c("scenario_id","year","crop","cultivar"),several.ok=TRUE)
  rows <- list()
  for (i in seq_along(results)) {
    result <- results[[i]]
    candidates <- if (source=="auto") c("crop_summary","report","daily") else source
    data <- NULL; used <- NULL
    for (candidate in candidates) {
      value <- switch(candidate,
        crop_summary=spacing_crop_summary(result,strict=FALSE,allow_empty=FALSE),
        report=spacing_report(result,strict=FALSE,allow_empty=FALSE),
        daily=spacing_daily(result,strict=FALSE,allow_empty=FALSE))
      if (!is.null(value)) { data <- value; used <- candidate; break }
    }
    if (is.null(data)) next
    resolved <- if (variable %in% names(data)) variable else .spacing_find_column(
      data, patterns=if(tolower(variable)=="yield") c("yield","grain.*(wt|weight)") else variable,
      numeric=TRUE,required=FALSE,label="comparison variable")
    if (is.na(resolved)) next
    years <- .spacing_row_year(data)
    crop_col <- .spacing_find_column(data,c("Crop","crop","CropName"),c("crop"),FALSE,FALSE)
    cultivar_col <- .spacing_find_column(data,c("Cultivar","cultivar","Variety"),c("cultivar","variety"),FALSE,FALSE)
    rows[[length(rows)+1L]] <- data.frame(
      scenario_id=if (!is.null(names(results)) && nzchar(names(results)[i])) names(results)[i] else
        .spacing_result_id(result,paste0("run",i)),
      year=if(!is.null(years)) years else NA,
      crop=if(!is.na(crop_col)) as.character(data[[crop_col]]) else .spacing_crop_from_variable(resolved),
      cultivar=if(!is.na(cultivar_col)) as.character(data[[cultivar_col]]) else NA_character_,
      value=as.numeric(data[[resolved]]),source=used,variable=resolved,
      stringsAsFactors=FALSE)
  }
  if (!length(rows)) .rspacing_abort("The requested comparison variable was not found.", "spacing_schema_error")
  data <- do.call(rbind,rows); data <- data[is.finite(data$value),,drop=FALSE]
  grouping <- unique(c("scenario_id",by)); grouping <- grouping[!vapply(grouping,function(x) all(is.na(data[[x]])),logical(1))]
  if (statistic=="auto") statistic <- if(grepl("yield|rain|irrig|fert|n2o|co2",variable,ignore.case=TRUE)) "sum" else "mean"
  fun <- switch(statistic,sum=function(x)sum(x,na.rm=TRUE),mean=function(x)mean(x,na.rm=TRUE),last=function(x)utils::tail(x[!is.na(x)],1),max=function(x)max(x,na.rm=TRUE))
  key <- interaction(data[grouping],drop=TRUE,lex.order=TRUE)
  split_data <- split(data,key)
  summary <- do.call(rbind,lapply(split_data,function(x) {
    z <- x[1,grouping,drop=FALSE]; z$value <- fun(x$value); z
  }))
  rownames(summary) <- NULL
  x_name <- if(length(by) && by[[1L]] %in% names(summary) && !all(is.na(summary[[by[[1L]]]]))) by[[1L]] else "scenario_id"
  p <- ggplot2::ggplot(summary,ggplot2::aes(x=!!rlang::sym(x_name), y=value, fill=scenario_id)) +
    ggplot2::geom_col(position="dodge") + ggplot2::labs(title=paste("Comparison:",variable),x=x_name,y=paste(statistic,variable),fill="Scenario")
  if (length(by)>=2L && by[[2L]] %in% names(summary) && !all(is.na(summary[[by[[2L]]]])))
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~",by[[2L]])),scales="free_x")
  attr(p,"data") <- summary
  p
}
