#' @title Check for binary files
#'
#' @description This function checks if a binary file exists for the equilibrium or base DayCent block.
#'
#' @param run character. Name of the DayCent block run. Matches the block specified in the schedule file name (eq, base, ...).
#' @param runCheck logical. Checks if a DayCent block run was required.
#'
#' @returns The function returns a message based on the arguments. It will specify if a .bin file already exists or if it needs to be created.
#'
#' @details
#' This function is used in `DayCentRunSite` function.
#'
#' @export
noBinFlag <- function(run, runCheck) {
  # Check if the .bin file exists
  ext_site_file <- paste0("./", run, "_extend.100")

  if (!file.exists(ext_site_file)&!runCheck) {
    # Stop with an error if the .bin file is missing
    stop(paste0("No ", run, " extended site is found. Please set 'run_", run," = TRUE' and run the script again."))
  }else if(!runCheck) {
    # Continue if the file exists and notify the user
    message(paste0("Notice: The ", run, " was not run, but a ", run, " extended site file was found. Continuing with the process."))
  }else{
    paste0("Starting the ", run, " run.")
  }

}
