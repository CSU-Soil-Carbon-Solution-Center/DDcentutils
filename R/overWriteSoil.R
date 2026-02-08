#' Read a DayCent soil.in file
#'
#' Reads a DayCent `soil.in` file and returns a data frame of soil layer
#' parameters formatted for insertion into a DayCent site (.100) file.
#' The first column (top depth) is removed.
#'
#' @param soil_in_path Character. Path to a DayCent `soil.in` file.
#'
#' @return A data frame containing soil layer parameters with columns:
#'   \code{SLDPMX}, \code{SLBLKD}, \code{SLFLDC}, \code{SLWLTP},
#'   \code{SLECOF}, \code{SLTCOF}, \code{SLSAND}, \code{SLCLAY},
#'   \code{SLORGF}, \code{SLCLIM}, \code{SLSATC}, \code{SLPH}.
#'
#' @examples
#' \dontrun{
#' soil_df <- read_soil_in("soil.in")
#' }
#'
#' @export
read_soil_in <- function(soil_in_path) {

  soil <- read.table(soil_in_path, header = FALSE, sep = "", strip.white = TRUE)

  colnames(soil) <- c(
    "top", "SLDPMX", "SLBLKD", "SLFLDC", "SLWLTP",
    "SLECOF", "SLTCOF", "SLSAND", "SLCLAY",
    "SLORGF", "SLCLIM", "SLSATC", "SLPH"
  )

  soil[, -1, drop = FALSE]
}


#' Format soil layer parameters for a DayCent site file
#'
#' Converts a soil parameter data frame into DayCent-formatted text
#' lines suitable for insertion into a site (.100) file.
#'
#' @param soil_df Data frame produced by \code{\link{read_soil_in}}.
#'
#' @return Character vector of formatted DayCent soil parameter lines.
#'
#' @examples
#' \dontrun{
#' soil_df <- read_soil_in("soil.in")
#' block <- format_soil_block(soil_df)
#' cat(block, sep = "\n")
#' }
#'
#' @export
format_soil_block <- function(soil_df) {

  params <- colnames(soil_df)
  n <- nrow(soil_df)

  out <- character(0)

  for (i in seq_len(n)) {
    for (p in params) {
      value <- as.numeric(soil_df[i, p])
      out <- c(out, sprintf(" %-12g %s(%d)", value, p, i))
    }
  }

  out
}


#' Overwrite soil-layer parameters in a DayCent site file
#'
#' Replaces the soil-layer parameter block (from \code{SLDPMX(1)} through
#' the final \code{SLPH(i)}) in a DayCent site (.100) file using values
#' from a `soil.in` file.
#'
#' All non-soil parameters in the site file remain unchanged.
#'
#' @param site_in_path Character. Path to the input DayCent site file.
#' @param soil_in_path Character. Path to the `soil.in` file.
#' @param site_out_path Character. Path to write the updated site file.
#'   If \code{NULL}, the input site file will be overwritten.
#'
#' @return Invisibly returns the path to the updated site file.
#'
#' @details
#' The function identifies the soil block by locating the first
#' \code{SLDPMX(1)} line and the last \code{SLPH(i)} line.
#' It replaces the entire block between these markers.
#'
#' A warning is issued if the number of soil layers differs
#' between the site file and the soil.in file.
#'
#' @examples
#' \dontrun{
#' overwrite_site_soil_layers(
#'   site_in_path = "wooster_base.100",
#'   soil_in_path = "soil.in",
#'   site_out_path = "wooster_base_UPDATED.100"
#' )
#' }
#'
#' @export
overwrite_site_soil_layers <- function(site_in_path,
                                       soil_in_path,
                                       site_out_path = NULL) {

  site_lines <- readLines(site_in_path, warn = FALSE)
  soil_df    <- read_soil_in(soil_in_path)
  new_block  <- format_soil_block(soil_df)

  start_idx <- grep("\\bSLDPMX\\(1\\)", site_lines)
  if (length(start_idx) == 0)
    stop("Could not find 'SLDPMX(1)' in site file.")
  start_idx <- start_idx[1]

  slph_idx <- grep("\\bSLPH\\([0-9]+\\)", site_lines)
  if (length(slph_idx) == 0)
    stop("Could not find any 'SLPH(i)' lines in site file.")
  end_idx <- max(slph_idx)

  if (end_idx < start_idx)
    stop("Found SLPH before SLDPMX(1); site file format unexpected.")

  site_layers <- suppressWarnings(
    as.integer(sub(".*\\((\\d+)\\).*", "\\1", site_lines[slph_idx]))
  )
  site_nlay <- max(site_layers, na.rm = TRUE)
  soil_nlay <- nrow(soil_df)

  if (is.finite(site_nlay) && site_nlay != soil_nlay) {
    warning(sprintf(
      "Layer count mismatch: site file has %d layers, soil.in has %d layers. Replacing anyway.",
      site_nlay, soil_nlay
    ))
  }

  new_site_lines <- c(
    site_lines[seq_len(start_idx - 1)],
    new_block,
    site_lines[(end_idx + 1):length(site_lines)]
  )

  if (is.null(site_out_path))
    site_out_path <- site_in_path

  writeLines(new_site_lines, site_out_path)

  invisible(site_out_path)
}
