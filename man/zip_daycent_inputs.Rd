% Generated manually for Packet 02; regenerate with roxygen2 when available.
\name{zip_daycent_inputs}
\alias{zip_daycent_inputs}
\title{ZIP DayCent site inputs for upload}
\usage{zip_daycent_inputs(project_path, site, out_zip = tempfile(fileext = ".zip"))}
\arguments{
  \item{project_path}{Project directory containing \code{sites/} and optionally \code{libs/}.}
  \item{site}{Site directory name under \code{sites/}.}
  \item{out_zip}{Destination archive path.}
}
\value{Invisibly returns the created ZIP path.}
\description{Creates a stable archive containing flat \code{sites/{site}/} inputs and optional top-level \code{libs/} files.}
