% Generated manually for Packet 04; regenerate with roxygen2 when available.
\name{unzip_daycent_results}
\alias{unzip_daycent_results}
\title{Safely extract DayCent API results}
\usage{unzip_daycent_results(zip_file, project_path, run_id = NULL,
  overwrite = FALSE)}
\arguments{
  \item{zip_file}{Existing result ZIP archive.}
  \item{project_path}{Project root containing the \code{sites/} directory.}
  \item{run_id}{Optional API run ID for the metadata archive name.}
  \item{overwrite}{Whether existing output files may be replaced.}
}
\value{A list containing extracted output paths and archived metadata paths.}
