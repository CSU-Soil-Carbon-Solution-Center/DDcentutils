#' @title Read a DayCent soil.in file
#'
#' @description
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
