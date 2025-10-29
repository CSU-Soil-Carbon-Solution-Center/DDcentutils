#' @title Get Elevation for a specific site
#'
#' @description This function retrieves the elevation of a specific site based on its latitude and longitude
#' using the `elevatr` package. The output includes the elevation, latitude, and longitude of the site.
#'
#' @param lat numeric. Latitude of the site in decimal degrees.
#' @param lon numeric. Longitude of the site in decimal degrees.
#' @param site character. The name or identifier for the site.
#'
#' @return A `sf` object (simple feature) with columns for the site name, latitude, longitude,
#'         and elevation in meters.
#'
#' @details
#' The function converts the latitude and longitude into a spatial object, queries an elevation
#' dataset using the `elevatr` package, and returns the elevation along with the latitude and
#' longitude. The coordinate reference system (CRS) used is WGS84 (EPSG:4326).
#'
#' @examples
#' # Example usage
#' lat <- 40.58
#' lon <- -105.08
#' site <- "Sample Site"
#' site_elevation <- getSiteElv(lat, lon, site)
#'
#' @importFrom sf st_as_sf st_coordinates
#' @importFrom elevatr get_elev_point
#' @import dplyr
#'
#' @export
  getSiteElv <- function(lat, lon, site) {
    # Create a data frame for the site's latitude, longitude, and name
    site_sf <- data.frame(x = lon, # longitude
                          y = lat, # latitude
                          names = site) # site name

    # Convert to a simple feature (sf) object with WGS84 CRS
    site_sf <- st_as_sf(x = site_sf, coords = c("x", "y"), crs = 4326)

    # Retrieve elevation data for the site
    site_sf = elevatr::get_elev_point(locations = site_sf, prj = 4326)

    # Add longitude and latitude columns back into the data
    site_sf = site_sf %>% mutate(lon = (site_sf %>% st_coordinates())[, 1],
                                 lat = (site_sf %>% st_coordinates())[, 2])

    return(site_sf)
  }
