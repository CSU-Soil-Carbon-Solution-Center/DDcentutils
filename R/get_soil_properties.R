#' @title Soil property data from SSURGO
#'
#' @description This functions queries SSURGO and gets soil properties for a specified location using the `soilDB` package.
#' The output is includes data for soil properties needed for a Daycent run.
#'
#' @param latitude  numeric. Latitude of the site in decimal degrees.
#' @param longitude numeric. Longitude of the site in decimal degrees.
#'
#' @return A data frame with data for soil properties and columns with record identifier information.
#'
#' @details
#' The function transforms the latitude and longitude into spatial objects to find the corresponding mapunit key in the SSURGO database
#' using the `soilDB` package. Data for specific soil properties are extracted using the correponding mapunit key for the soil depth interval
#' between 0 and 200 cm for the top and bottom depth, respectively. Each mapunit may present more than one component.
#' The function reports data only for the component with the highest percentage for the mapunit at the coordinates of interest.
#' Below is a short description of the columns returned in the data frame.
#' For more information, please visit the NRCS website: www.nrcs.usda.gov/resources/data-and-reports/soil-survey-geographic-database-ssurgo
#' \itemize{
#' \item mukey = numeric. Mapunit key.
#' \item cokey = numeric. Component key.
#' \item areasymbol = character. Area symbol. Identifies single occurrence of a particular type of area.
#' \item musym = character. Mapunit symbol. Uniquely identifies the soil mapunit in the soil survey.
#' \item muname = character. Mapunit name.
#' \item compname = character. Component name.
#' \item compkind = character. Component kind. Examples are series and miscellaneous areas.
#' \item comppct_r = numeric. Component percentage. Percentage of the component of the mapunit.
#' \item majcompflag = character. Major component. Indicates whether or not a component is a major component in the mapunit.
#' \item chkey = numeric. Chorizon key. Uniquely identifies a record in the Horizon table.
#' \item hzdept_r = numeric. Horizon top depth, in cm.
#' \item hzdepb_r = numeric. Horizon bottom depth, in cm.
#' \item awc_r = numeric. Available water capacity, as volume fraction.
#' \item dbthirdbar_r = numeric. Bulk density at a water tension of 1/3 bar, in g/cm3.
#' \item wfifteeenbar_r = numeric. Volumetric content of soil water, in percent.
#' \item ksat_r = numeric. Saturated hydraulic conductivity, in cm/sec.
#' \item claytotal_r = numeric. Clay total, in weight percentage of the less than 2 mm soil material.
#' \item sandtotal_r = numeric. Sand total, in weight percentage of the less than 2 mm soil material.
#' \item om_r = numeric. Organic matter, in weight percentage of the less than 2 mm soil material.
#' \item ph1to1h2o_r = numeric. pH using 1:1 soil-water ratio.
#' }
#'
#' @examples
#' lat <- 40.58
#' lon <- -105.08
#' soils <- get_soil_properties(lat, lon)
#'
#' @import sf
#' @import soilDB
#' @import dplyr
#'
#' @export
get_soil_properties <- function(latitude, longitude) {

  #define the point
  p <- sf::st_as_sf(data.frame(x = longitude,
                               y = latitude),
                    coords = c('x', 'y'),
                    crs = 4326)

  # query map unit records at this point
  res <- SDA_spatialQuery(p, what = 'mukey')

  # convert results into an SQL "IN" statement
  # useful when there are multiple intersecting records
  # mu.is <- format_SQL_in_statement(res$mukey)

  y <- get_SDA_property(property =
                          c("awc_r","dbthirdbar_r","wfifteenbar_r","wthirdbar_r","ksat_r",
                            "claytotal_r", "sandtotal_r","om_r", "ph1to1h2o_r"),
                        method = "None",
                        mukeys  = res$mukey,
                        top_depth = 0,
                        bottom_depth = 200)

  if(length(unique(y$comppct_r))>1){
    y <- y %>% group_by(mukey) %>%
      filter(comppct_r == max(comppct_r)) %>%
      ungroup()
  }

  return(y)
}
