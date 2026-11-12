#' Built-in APSIM cultivar catalogue used by rSpacing examples
#'
#' @param crop Optional crop name.
#' @return A data frame with verified crop/cultivar combinations.
#' @export
spacing_cultivars <- function(crop = NULL) {
  catalogue <- data.frame(
    crop = c("Wheat", "Maize", "Soybean"),
    cultivar = c("Hartog", "B_110", "Generic_MG1"),
    source = "Bundled APSIM Models resource",
    stringsAsFactors = FALSE
  )
  if (is.null(crop)) return(catalogue)
  crop <- .spacing_optional_text(crop, "crop")
  out <- catalogue[tolower(catalogue$crop) == tolower(crop), , drop = FALSE]
  if (!nrow(out)) {
    stop("No verified cultivar catalogue is available for crop `", crop, "`.",
         call. = FALSE)
  }
  out
}

#' Validate a crop/cultivar combination
#'
#' @param crop Crop name.
#' @param cultivar Cultivar name.
#' @return The normalized cultivar name, invisibly.
#' @export
spacing_validate_cultivar <- function(crop, cultivar) {
  catalogue <- spacing_cultivars(crop)
  cultivar <- .spacing_optional_text(cultivar, "cultivar")
  hit <- which(tolower(catalogue$cultivar) == tolower(cultivar))
  if (length(hit) != 1L) {
    stop(
      "Cultivar `", cultivar, "` is not in the verified rSpacing catalogue for ",
      crop, ". Available: ", paste(catalogue$cultivar, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(catalogue$cultivar[hit])
}
