#' @title Rename and move DayCent output files
#'
#' @description This function finds all DayCent outputs and moves to an outputs folder within the site folder to avoid overwriting files.
#' Outputs are based on the outfiles.in.
#'
#' @param run character. Scenario descriptor of DayCent block (equilibrium, base, experimental).
#' @param output_dir character. Local path indicating where output files will be saved.
#' @param extensions character. File extensions to be moved to the outputs folder. Use regex to determine the file extensions to be moved.
#'
#' @return Files created from a DayCent run are moved from their original location to the outputs folder location specified in the function.
#'
#' @details
#' This function is used in the `DayCentRunSite` function to reinforce and organize the DayCent file structure.
#'
#' @export
rename_and_move_output_files <- function(run, output_dir = "./outputs/",
                                         extensions = "\\.(out|csv)$", ...) {
  # Get a list of files with .out or .csv extensions
  output_files <- list.files(pattern = extensions)

  if(!dir.exists(output_dir)){
    dir.create(output_dir, recursive = T)
  }

  # Check if there are output files
  if (length(output_files) > 0) {
    # Rename the files by prepending the 'run' identifier
    file.rename(output_files, paste0(run, "_", output_files))

    # Update the list of files after renaming
    output_files <- list.files(pattern = extensions)

    # Copy the renamed files to the specified output directory
    file.copy(output_files, output_dir, overwrite = TRUE)

    # Remove the original files after copying
    file.remove(output_files)
  } else {
    message("No output files found to rename or move.")
  }
}
