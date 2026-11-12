\name{spacing_irrigation}
\alias{spacing_irrigation}
\alias{spacing_apply_irrigation}
\alias{as.list.spacing_irrigation}
\alias{print.spacing_irrigation}
\alias{summary.spacing_irrigation}
\title{Define and apply an APSIMx irrigation strategy}
\description{Creates a validated irrigation object and maps it to the current Spacing.Tool2 management fields.}
\usage{
spacing_irrigation(enabled = TRUE, threshold = 0.5, target = 0.85,
  max_application_mm = 35, window_start = NULL, window_end = NULL,
  label = NULL)
spacing_apply_irrigation(management, irrigation)
}
\value{A \code{spacing_irrigation} object or an updated management list.}
