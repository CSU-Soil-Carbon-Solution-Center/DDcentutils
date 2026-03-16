#' @title Format soil layer parameters for a DayCent site file
#'
#' @description Converts a soil parameter data frame into DayCent-formatted text
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
