% Generated manually for Packet 02; regenerate with roxygen2 when available.
\name{zip_daycent_inputs}
\alias{zip_daycent_inputs}
\title{ZIP selected DayCent inputs for API upload}
\usage{zip_daycent_inputs(project_path, include = NULL, run_eq = FALSE, out_zip = tempfile(fileext = ".zip"))}
\arguments{
  \item{project_path}{Project directory containing \code{sites/} and optionally \code{libs/}.}
  \item{include}{API-shaped character vector of \code{site/scenario} selections.}
  \item{run_eq}{When \code{TRUE}, add each selected site's \code{eq} and \code{base} inputs.}
  \item{out_zip}{Destination archive path.}
}
\value{Invisibly returns the created ZIP path.}
\description{Creates a stable archive containing selected flat site inputs and optional top-level library files, excluding per-site outputs directories.}
