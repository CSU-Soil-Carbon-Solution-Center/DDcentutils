% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_and_move_output_files.R
\name{rename_and_move_output_files}
\alias{rename_and_move_output_files}
\title{Rename and move DayCent output files}
\usage{
rename_and_move_output_files(
  run,
  output_dir = "./outputs/",
  extensions = "\\\\.(out|csv)$",
  ...
)
}
\arguments{
\item{run}{character. Scenario descriptor of DayCent block (equilibrium, base, experimental).}

\item{output_dir}{character. Local path indicating where output files will be saved.}

\item{extensions}{character. File extensions to be moved to the outputs folder. Use regex to determine the file extensions to be moved.}
}
\value{
Files created from a DayCent run are moved from their original location to the outputs folder location specified in the function.
}
\description{
This function finds all DayCent outputs and moves to an outputs folder within the site folder to avoid overwriting files.
Outputs are based on the outfiles.in.
}
\details{
This function is used in the \code{DayCentRunSite} function to reinforce and organize the DayCent file structure.
}
