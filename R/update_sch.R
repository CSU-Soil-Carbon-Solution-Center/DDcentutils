#' @title Update Parameters in a DayCent .sch File
#'
#' @description This function reads a DayCent `.sch` schedule file and replaces one or more parameter
#' (e.g., "Site file name", "Starting year") with new values. This function will not write repeating parameters like schedule events.
#' It writes the modified file to a new output path.
#'
#' The function detects whether each replacement value is numeric or character, and formats it accordingly.
#'
#' @param in_sch Character. Path to the input `.sch` file to be read.
#' @param out_sch Character. Path to the output `.sch` file to be written.
#' @param param_names Character vector. The parameter names (e.g., "Site file name") to search for in the file.
#' @param param_values Character or numeric vector. The values to replace the matched parameters with. Must be the same length as `param_names`.
#'
#' @return Invisibly returns a character vector of parameters that were not found or had multiple matches.
#'
#' @examples
#' \dontrun{
#' update_sch(
#'   in_sch = "example.sch",
#'   out_sch = "example_updated.sch",
#'   param_names = c("Site file name", "Starting year"),
#'   param_values = c("site_new.100", 1985)
#' )
#' }
#'
#' @export
update_sch <- function(in_sch = "pendleton.sch",
                       out_sch = in_sch,
                       param_names = c("Site file name"),
                       param_values = c("pendleton_exp_all_active.100"),
                       overwrite= FALSE) {

  if (length(param_names) != length(param_values)) {
    stop("Parameter name and value lists are of unequal length.")
  }

  if ( in_sch == out_sch && overwrite == FALSE){
    stop("Please provide a new out_sch file name or make overwrite == TRUE")
  }

  lines <- readLines(in_sch)
  not_found <- character()

  for (i in seq_along(param_names)) {
    param <- param_names[i]
    value <- param_values[i]
    param_escaped <- gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", param))
    pattern <- sprintf("^\\s*\\S+\\s+%s\\b", param_escaped)
    match_index <- grep(pattern, lines)

    if (length(match_index) == 1) {
      old_line <- lines[match_index]

      # format line based on type of value
      if (suppressWarnings(!is.na(as.numeric(value)))) {
        new_line <- sprintf("%-20.8f %s", as.numeric(value), param)
      } else {
        new_line <- sprintf("%-20s %s", value, param)
      }

      lines[match_index] <- new_line
      message(sprintf("Updated %s: %s -> %s", param, old_line, new_line))
    } else {
      warning(sprintf("Parameter '%s' not found or found multiple times.", param))
      not_found <- c(not_found, param)
    }
  }

  writeLines(lines, out_sch)
  return(invisible(not_found))
}
