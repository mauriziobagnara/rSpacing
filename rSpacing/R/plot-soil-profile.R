#' Resolve the soil-layer geometry used by all soil plots
#'
#' Reads layer thicknesses from the generated APSIMX Physical node and converts
#' them to cumulative depth intervals. The same geometry is used by individual
#' layer plots and cumulative profile plots.
#' @param result A spacing_result.
#' @param n_layers Expected number of layers, or NULL.
#' @param thickness Optional explicit layer thicknesses in mm.
#' @return A data frame with layer, thickness_mm, depth_top_mm,
#' depth_bottom_mm, depth_mid_mm, and depth_label.
#' @export
spacing_soil_layers <- function(result, n_layers = NULL, thickness = NULL) {
  .validate_spacing_result(result)
  if (is.null(thickness)) thickness <- .spacing_apsimx_thickness(result)
  if (is.null(n_layers)) {
    daily <- spacing_daily(result, strict = FALSE, allow_empty = TRUE)
    indexed <- if (is.data.frame(daily)) grep("\\([0-9]+\\)$", names(daily), value = TRUE) else character()
    idx <- suppressWarnings(as.integer(sub("^.*\\(([0-9]+)\\)$", "\\1", indexed)))
    idx <- idx[is.finite(idx)]
    if (length(idx)) n_layers <- max(idx)
  }
  if (is.null(thickness) && identical(as.integer(n_layers), 6L)) {
    thickness <- c(150, 150, 300, 300, 300, 300)
  }
  if (is.null(thickness)) {
    .rspacing_abort("Soil-layer thicknesses could not be resolved from the APSIMX file. Supply 'thickness' explicitly.", "spacing_schema_error")
  }
  thickness <- as.numeric(thickness)
  if (anyNA(thickness) || any(!is.finite(thickness)) || any(thickness <= 0)) {
    .rspacing_abort("Soil-layer thicknesses must be positive finite values.", "spacing_validation_error")
  }
  if (!is.null(n_layers) && length(thickness) != as.integer(n_layers)) {
    .rspacing_abort(sprintf("The APSIMX profile contains %d thickness values but the output contains %d layers.", length(thickness), as.integer(n_layers)), "spacing_schema_error")
  }
  bottom <- cumsum(thickness)
  top <- c(0, head(bottom, -1L))
  data.frame(
    layer = seq_along(thickness), thickness_mm = thickness,
    depth_top_mm = top, depth_bottom_mm = bottom,
    depth_mid_mm = (top + bottom) / 2,
    depth_label = paste0(top, "-", bottom, " mm"),
    stringsAsFactors = FALSE
  )
}

#' Plot one layer-specific soil variable against cumulative depth
#' @param result A spacing_result.
#' @param variable Base variable name, such as SWmm or NO3_kgha.
#' @param thickness Optional explicit layer thicknesses in mm.
#' @param type time_depth or profile.
#' @param start_date,end_date Optional date limits.
#' @param summary Summary for profile plots.
#' @param colours Low and high colours.
#' @param na_colour Missing-value colour.
#' @param base_size Base font size.
#' @return A ggplot object.
#' @export
plot_spacing_soil_profile <- function(result, variable, thickness = NULL,
  type = c("time_depth", "profile"), start_date = NULL, end_date = NULL,
  summary = c("mean", "median", "last", "min", "max"),
  colours = c("#F7FBFF", "#08306B"), na_colour = "grey90", base_size = 12) {
  .validate_spacing_result(result)
  type <- match.arg(type); summary <- match.arg(summary)
  daily <- spacing_daily(result, strict = TRUE, allow_empty = FALSE)
  base <- sub("\\([0-9]+\\)$", "", variable)
  indexed <- grep("\\([0-9]+\\)$", names(daily), value = TRUE)
  cols <- indexed[sub("\\([0-9]+\\)$", "", indexed) == base]
  if (!length(cols)) .rspacing_abort(sprintf("No indexed layers found for '%s'.", base), "spacing_schema_error")
  layer <- as.integer(sub("^.*\\(([0-9]+)\\)$", "\\1", cols))
  cols <- cols[order(layer)]; layer <- sort(layer)
  if (!identical(layer, seq_along(layer))) .rspacing_abort(sprintf("Layers for '%s' must be consecutive from 1.", base), "spacing_schema_error")
  geometry <- spacing_soil_layers(result, n_layers = length(layer), thickness = thickness)
  dates <- .spacing_daily_dates(daily)
  keep <- !is.na(dates)
  if (!is.null(start_date)) keep <- keep & dates >= as.Date(start_date)
  if (!is.null(end_date)) keep <- keep & dates <= as.Date(end_date)
  if (!any(keep)) .rspacing_abort("No observations remain after date filtering.", "spacing_schema_error")
  dates <- dates[keep]; daily <- daily[keep,,drop=FALSE]
  long <- do.call(rbind, lapply(seq_along(cols), function(i) data.frame(
    date=dates, geometry[i,], value=daily[[cols[i]]], stringsAsFactors=FALSE)))
  unit <- spacing_variable_unit(result, cols[1], table="DailyReport", default="")
  legend_title <- if (nzchar(unit)) paste0(base, " (", unit, ")") else base
  if (type == "time_depth") {
    p <- ggplot2::ggplot(long, ggplot2::aes(x=date, y=depth_mid_mm, height=thickness_mm, fill=value)) +
      ggplot2::geom_tile(width=1) +
      ggplot2::geom_hline(yintercept=unique(c(0, geometry$depth_bottom_mm)), colour="white", linewidth=.25) +
      ggplot2::scale_y_reverse(breaks=unique(c(0, geometry$depth_bottom_mm)), limits=c(max(geometry$depth_bottom_mm),0), expand=c(0,0)) +
      ggplot2::scale_fill_gradient(low=colours[1], high=colours[2], na.value=na_colour, name=legend_title) +
      ggplot2::labs(title=paste(base,"by soil depth"), subtitle=paste(geometry$depth_label, collapse="; "), x="Date", y="Cumulative depth (mm)")
  } else {
    values <- vapply(split(long$value,long$layer), function(x) {
      x <- x[is.finite(x)]; if (!length(x)) return(NA_real_)
      switch(summary, mean=mean(x), median=stats::median(x), last=tail(x,1), min=min(x), max=max(x))
    }, numeric(1))
    profile <- geometry; profile$value <- as.numeric(values)
    step <- data.frame(value=rep(profile$value,each=2), depth_mm=as.vector(rbind(profile$depth_top_mm,profile$depth_bottom_mm)))
    p <- ggplot2::ggplot(profile) +
      ggplot2::geom_rect(ggplot2::aes(xmin=0,xmax=value,ymin=depth_top_mm,ymax=depth_bottom_mm,fill=value), colour="white", linewidth=.35) +
      ggplot2::geom_step(data=step,ggplot2::aes(x=value,y=depth_mm),direction="vh",linewidth=.8,colour="#202020") +
      ggplot2::scale_y_reverse(breaks=unique(c(0,geometry$depth_bottom_mm)), limits=c(max(geometry$depth_bottom_mm),0), expand=c(0,0)) +
      ggplot2::scale_fill_gradient(low=colours[1],high=colours[2],na.value=na_colour,name=legend_title) +
      ggplot2::labs(title=paste(base,"soil profile"),subtitle=paste0(summary,"; ",paste(geometry$depth_label,collapse="; ")),x=legend_title,y="Cumulative depth (mm)")
  }
  p + ggplot2::theme_minimal(base_size=base_size) + ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),plot.title.position="plot")
}

#' Plot all indexed soil-variable families
#' @inheritParams plot_spacing_soil_profile
#' @param variables Optional base names.
#' @return A named list of ggplot objects.
#' @export
plot_spacing_soil_profiles <- function(result, variables=NULL, thickness=NULL,
 type=c("time_depth","profile"), start_date=NULL,end_date=NULL,
 summary=c("mean","median","last","min","max"), colours=c("#F7FBFF","#08306B"),
 na_colour="grey90",base_size=12) {
 type<-match.arg(type); summary<-match.arg(summary)
 daily<-spacing_daily(result,strict=TRUE,allow_empty=FALSE)
 indexed<-grep("\\([0-9]+\\)$",names(daily),value=TRUE)
 if (is.null(variables)) {
   bases<-unique(sub("\\([0-9]+\\)$","",indexed))
   variables<-bases[vapply(bases,function(b) sum(sub("\\([0-9]+\\)$","",indexed)==b)>=2,logical(1))]
 }
 if (!length(variables)) .rspacing_abort("No indexed soil variables are available.","spacing_schema_error")
 variables<-unique(sub("\\([0-9]+\\)$","",variables))
 stats::setNames(lapply(variables,function(v) plot_spacing_soil_profile(result,v,thickness,type,start_date,end_date,summary,colours,na_colour,base_size)),variables)
}

.spacing_daily_dates <- function(data) {
  date_col <- names(data)[tolower(names(data)) %in% c("clock.today","date","today")][1L]
  if (!is.na(date_col)) return(as.Date(sub(" .*","",data[[date_col]]),tryFormats=c("%Y-%m-%d","%m/%d/%Y","%d/%m/%Y")))
  if (all(c("Year","DOY") %in% names(data))) return(as.Date(paste0(as.integer(data$Year),"-01-01"))+as.integer(data$DOY)-1L)
  .rspacing_abort("The daily table requires a date column or Year and DOY.","spacing_schema_error")
}

.spacing_apsimx_thickness <- function(result) {
  paths<-character()
  if (!is.null(result$run$outputs$apsimx)) paths<-c(paths,result$run$outputs$apsimx)
  if (!is.null(result$outputs$apsimx)) paths<-c(paths,result$outputs$apsimx)
  paths<-unique(paths[file.exists(paths)])
  if (!length(paths)) return(NULL)
  model<-tryCatch(jsonlite::fromJSON(paths[1],simplifyVector=FALSE),error=function(e) NULL)
  if (is.null(model)) return(NULL)
  .spacing_find_physical_thickness(model)
}
.spacing_find_physical_thickness <- function(x) {
  if (!is.list(x)) return(NULL)
  type<-x[["$type"]] %||% ""; th<-x[["Thickness"]]
  if (is.character(type) && grepl("Physical",type,fixed=TRUE) && length(th)) {
    ans<-as.numeric(unlist(th,use.names=FALSE)); if (length(ans)&&all(is.finite(ans))&&all(ans>0)) return(ans)
  }
  for (item in x) { ans<-.spacing_find_physical_thickness(item); if (!is.null(ans)) return(ans) }
  NULL
}
