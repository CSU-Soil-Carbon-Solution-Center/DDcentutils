#' @title Update Sitepar File with New SRADADJ Values
#'
#' @description Replaces 12 `sradadj[i]` lines in a sitepar.in file with new values.
#'
#' @param file_path Path to the input sitepar file.
#' @param new_sradadj A numeric vector of 12 monthly transmission values.
#' @param output_path Optional path to save the updated file. If NULL, returns updated lines.
#'
#' @return Updated lines invisibly or writes to a file.
#'
#' @export
#'
update_sitepar_sradadj <- function(file_path, new_sradadj, output_path = NULL) {
  if (length(new_sradadj) != 12) stop("new_sradadj must be a vector of 12 monthly values.")

  lines <- readLines(file_path)
  pattern <- "^\\s*[-+]?[0-9]*\\.?[0-9]+\\s*/\\s*sradadj\\[[0-9]+\\]"
  srad_lines <- grep(pattern, lines)

  if (length(srad_lines) != 12) {
    stop("Expected 12 sradadj lines, found ", length(srad_lines))
  }

  for (i in 1:12) {
    comment <- sub(".*(/.*)", "\\1", lines[srad_lines[i]])
    lines[srad_lines[i]] <- sprintf("%.2f       %s", new_sradadj[i], comment)
  }

  if (!is.null(output_path)) {
    writeLines(lines, output_path)
  } else {
    return(lines)
  }
}
