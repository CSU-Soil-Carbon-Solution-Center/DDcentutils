#' @title Adjust Sitepar File with Cloudiness and Terrain
#'
#' @description Updates `sitepar.in` using NASA POWER cloudiness and elevation/terrain data.
#'
#' @param lat numeric. Latitude of site.
#' @param lon numeric. Longitude of site.
#' @param start_year numeric. Start year for cloudiness data.
#' @param end_year numeric. End year for cloudiness data.
#' @param sitepar_path string. Input path to `sitepar.in` file.
#' @param output_path string. Output path for updated file. If not given the provided sitepar_path file will be updated.
#' @param plot logical. If TRUE, plots cloudiness.
#'
#' @return Invisibly returns a data frame with monthly solar transmission values.
#'
#' @export
adjust_sitepar_srad_elev <- function(lat, lon, start_year = 1985, end_year = 2024,
                                     sitepar_path, output_path = NULL, plot = TRUE) {
  if(is.null(output_path)){
    output_path = sitepar_path
  }

  sradadj_vals <- get_monthly_cloudiness(lat, lon, start_year, end_year)
  update_sitepar_sradadj(sitepar_path, sradadj_vals$transmission, output_path)

  terrain_list <- get_terrain_attributes(lat, lon, z = 14)
  update_sitepar_terrain(output_path, terrain_list, output_path)

  if (!is.null(output_path)) {
    message(paste0("Updated ", output_path,
                   " with NASA POWER cloud percentages and terrain for lon = ", lon,
                   ", lat = ", lat))
  }

  return(invisible(sradadj_vals))
}
