#' @title Get Terrain Attributes for a Point
#'
#' @description Fetches elevation, slope, and aspect for a given lat/lon using Mapzen terrain tiles.
#'
#' @param lat Latitude of the point.
#' @param lon Longitude of the point.
#' @param z Zoom level for the elevation tile (default: 14).
#'
#' @return A list with elements `elevation`, `slope`, and `aspect`.
#'
#' @importFrom sf st_as_sf
#' @importFrom terra terrain extract rast vect
#' @importFrom elevatr get_elev_raster
#' @import dplyr
#'
#' @export
get_terrain_attributes <- function(lat, lon, z = 14) {

  pt_sf <- st_as_sf(data.frame(x = lon, y = lat), coords = c("x", "y"), crs = 4326)
  elev_raster <- get_elev_raster(locations = pt_sf, z = z, clip = "tile")

  slope_r <- terrain(elev_raster, opt = "slope", unit = "degrees")
  aspect_r <- terrain(elev_raster, opt = "aspect", unit = "degrees")

  elevation <- terra::extract(rast(elev_raster), vect(pt_sf))[, 2]
  slope     <- terra::extract(rast(slope_r), vect(pt_sf))[, 2]
  aspect    <- terra::extract(rast(aspect_r), vect(pt_sf))[, 2]

  return(list(elevation = elevation, slope = slope, aspect = aspect))
}
