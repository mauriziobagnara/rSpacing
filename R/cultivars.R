#' APSIM cultivar catalogue
#'
#' Lists the cultivars defined in the crop resources of the APSIM `Models.dll`
#' shipped with Spacing.Tool2 0.09.10 (table `inst/extdata/apsim/cultivars.csv`),
#' and marks the cultivars of the rSpacing standard rotation used by the
#' five-year, Morris and uncertainty workflows (Wheat `Claire`, Maize `B_130`,
#' Soybean `Generic_MG1`).
#'
#' Spacing.Tool2 (>= 0.09.10) repeats the check against the APSIM version it
#' actually runs and stops with exit code 3 when a cultivar is unknown.
#'
#' @param crop Optional APSIM crop model name (case-insensitive), e.g.
#'   `"Wheat"`. `NULL` returns every crop.
#' @return A data frame with one row per cultivar and columns `crop`,
#'   `cultivar`, `source` (where the definition comes from) and `standard`
#'   (`TRUE` for the cultivars of the rSpacing standard rotation).
#' @seealso [spacing_validate_cultivar()], [spacing_crop()].
#' @export
#' @examples
#' head(spacing_cultivars("Maize"))
#' spacing_cultivars()[spacing_cultivars()$standard, ]
spacing_cultivars <- function(crop = NULL) {
  catalogue <- .spacing_apsim_table("cultivars")
  catalogue$source <- "APSIM Models resource (Spacing.Tool2 0.09.10)"
  catalogue$standard <- paste(catalogue$crop, catalogue$cultivar) %in%
    c("Wheat Claire", "Maize B_130", "Soybean Generic_MG1")
  if (is.null(crop)) return(catalogue)
  crop <- .spacing_optional_text(crop, "crop")
  out <- catalogue[tolower(catalogue$crop) == tolower(crop), , drop = FALSE]
  if (!nrow(out)) {
    stop("No APSIM cultivar catalogue is available for crop `", crop, "`. Crops: ",
         paste(unique(catalogue$crop), collapse = ", "), ".", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

#' Validate a crop/cultivar combination
#'
#' Checks that `cultivar` is defined for `crop` in the APSIM cultivar
#' catalogue ([spacing_cultivars()]) and returns its exact spelling. Matching
#' is case-insensitive. Used by [spacing_crop()] and [spacing_intercrop()]
#' when `verify_cultivar = TRUE`.
#'
#' @param crop APSIM crop model name, e.g. `"Soybean"`.
#' @param cultivar Cultivar name, e.g. `"generic_mg1"`.
#' @return The cultivar name as spelled in APSIM, invisibly. An error is
#'   raised when the crop has no catalogue or the cultivar is not defined.
#' @seealso [spacing_cultivars()].
#' @export
#' @examples
#' spacing_validate_cultivar("Soybean", "generic_mg1")
spacing_validate_cultivar <- function(crop, cultivar) {
  catalogue <- spacing_cultivars(crop)
  cultivar <- .spacing_optional_text(cultivar, "cultivar")
  hit <- which(tolower(catalogue$cultivar) == tolower(cultivar))
  if (length(hit) != 1L) {
    stop(
      "Cultivar `", cultivar, "` is not in the APSIM cultivar catalogue for ",
      crop, " (", nrow(catalogue), " cultivars; see spacing_cultivars(\"", crop, "\")).",
      call. = FALSE
    )
  }
  invisible(catalogue$cultivar[hit])
}
