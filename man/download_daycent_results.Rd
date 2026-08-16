% Generated manually for Packet 04; regenerate with roxygen2 when available.
\name{download_daycent_results}
\alias{download_daycent_results}
\title{Download and safely extract DayCent API results}
\usage{download_daycent_results(config, run_id, project_path, keep_zip = FALSE,
  output_zip = NULL, overwrite = FALSE)}
\arguments{
  \item{config}{API configuration.}
  \item{run_id}{Completed API run ID.}
  \item{project_path}{Project root containing the \code{sites/} directory.}
  \item{keep_zip}{Whether to retain the downloaded ZIP.}
  \item{output_zip}{Optional ZIP destination path.}
  \item{overwrite}{Whether existing output files may be replaced.}
}
\value{A list containing output paths, the metadata archive, and the retained ZIP path when requested.}
