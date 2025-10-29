#' @title Convert SoilGrids data to DayCent format
#'
#' @description This function converts SoilGrids data into a DayCent-ready file format for specific locations.
#' SoilsGrids data is interpolated to fit the soil depth intervals for DayCent when necessary.
#'
#' @param latitude numeric. Latitude of the site in decimal degrees.
#' @param longitude numeric. Longitude of the site in decimal degrees.
#' @param site character. Name of the site. Must correspond to an existing folder name.
#' @param templatesite character. Soils.in file template name.
#'                     Must correspond to an existing soils.in file to be used as a template.
#'
#' @return The function exports a soils.in file for the site of interest that is DayCent-ready.
#'
#' @details
#' This function leverages the `fetchSoilGrids` function from the `soilDB` package to obtain data that is converted into
#' a DayCent-ready file format for specific locations. This function interpolates the data for soil layers using a
#' simple weighted approach based on the overlap between the SoilGrids and DayCent soil layer thickness.
#' The ksat is calculated using the Saxton and Rawls (2006) equation after converting SOC to OM.
#' This function pulls the Q50 values (medians) from SoilGrids for the relevant columns.
#' The templatesite name should match the file name in the site folder where DayCent is being run.
#' The site name must correpond to the folder where the exported file will be saved.
#' The returned soils.in file follows the template format, and thus includes 13 variables:
#' \itemize{
#' \item upper_depth_cm, from soils.in template file.
#' \item lower_depth_cm, from soils.in template file.
#' \item bdodQ50, interpolated from SoilGrids data.
#' \item wv0033Q50, interpolated from SoilGrids data.
#' \item wv1500Q50, interpolated from SoilGrids data.
#' \item evap_coeff, from soils.in template file.
#' \item fract_roots, from soils.in template file.
#' \item sandQ50, interpolated from SoilGrids data.
#' \item clayQ50, interpolated from SoilGrids data.
#' \item om, interpolated from SoilGrids data.
#' \item min_fract, from soils.in template file.
#' \item ksat, interpolated from SoilGrids data.
#' \item phh2oQ50, interpolated from SoilGrids data.
#' }
#'
#' @examplesIf interactive()
#' latitude <- 45.7
#' longitude <- -118.6
#' site <- "test_site" ## this should be an existing folder within the sites folder for a DayCent run
#' templatesite <- "test_soils" ## this should be an existing soils.in to be used as a template
#' soils <- convert_soilgrids_to_daycent(latitude, longitude, site, templatesite) ## the file will be saved in the specified site folder.
#'
#' @import dplyr
#' @import readr
#' @import data.table
#' @import here
#' @import soilDB
#'
#' @export
convert_soilgrids_to_daycent <- function (latitude, longitude, site, templatesite)
{
  tempsoil_in <- fread(here("sites", templatesite, "soils.in")) %>%
    filter(!is.na(V1)) #read in template soil.in file
  names(tempsoil_in) <- c("upper_depth_cm", "lower_depth_cm",
                          "bd_gcm3", "fc_vol", "wp_vol", "evap_coeff", "fract_roots",
                          "sand_frac", "clay_fract", "OM_fract", "min_fract",
                          "ksat_cmsec", "pH") #add row headers to template soil file

  ## Pull soil data from soils grids
  # Create needed dataframe for API access
  locations <- data.frame(
    id = site,
    lat = latitude,
    lon = longitude)

  soil_data <- fetchSoilGrids(
    locations,
    loc.names = c("id", "lat", "lon"),
    variables = c("bdod", "wv0033", "wv1500","cec", "clay", "sand", "silt", "soc", "phh2o") # Select desired properties
  )

  grid_soils <- soil_data@horizons


  grid_soils$ksat <- exp(12.012 - 0.0755 * (grid_soils$sandQ50) - 0.217 * (grid_soils$clayQ50) - 0.506 * (grid_soils$socQ50/10*1.724))
  grid_soils$om <- grid_soils$socQ50*1.724 # use van Bemmeln factor to convert to om

  profiles <- tempsoil_in %>% select(upper_depth_cm, lower_depth_cm)
  soil_expanded <- grid_soils %>% select(hzdept, hzdepb) %>%
    rowwise() %>% mutate(
      profile_data = list(
        profiles %>% filter(hzdepb > upper_depth_cm & hzdept < lower_depth_cm) %>%
          mutate(Overlap = pmin(lower_depth_cm, hzdepb) - pmax(upper_depth_cm, hzdept), Weight = Overlap/(lower_depth_cm - upper_depth_cm)))) %>%
    unnest(cols = c(profile_data))

   relevant_columns <- c("hzdept", "hzdepb", "sandQ50", "bdodQ50",
                        "clayQ50", "wv0033Q50", "wv1500Q50", "om",
                         "phh2oQ50", "ksat")

   soil_expanded_full <- soil_expanded %>% left_join(grid_soils %>%
                                  select(all_of(relevant_columns)),
                                  by = c("hzdept","hzdepb"))

   interpolated_profiles <- soil_expanded_full %>%
     group_by(upper_depth_cm,lower_depth_cm) %>%
     summarise(
       sandQ50 = sum(sandQ50/100 *Weight, na.rm = TRUE) %>% round(3),
       clayQ50 = sum(clayQ50/100 * Weight, na.rm = TRUE) %>% round(3),
       bdodQ50 = sum(bdodQ50 * Weight, na.rm = TRUE) %>% round(3),
       om = sum(om/100 *Weight, na.rm = TRUE) %>% round(3),
       wv0033Q50 = sum(wv0033Q50/100 *Weight, na.rm = TRUE) %>% round(3),
       wv1500Q50 = sum(wv1500Q50/100 *Weight, na.rm = TRUE) %>% round(3),
       ksat = sum(ksat * 1e-04 * Weight, na.rm = TRUE) %>% round(3),
       phh2oQ50 = sum(phh2oQ50 * Weight, na.rm = TRUE) %>% round(3), .groups = "drop")

  outsoils_in <- interpolated_profiles %>%
    cbind(tempsoil_in %>%
            select(evap_coeff, fract_roots, min_fract)) %>%
    select(upper_depth_cm,lower_depth_cm, bdodQ50, wv0033Q50, wv1500Q50, evap_coeff, fract_roots, sandQ50, clayQ50, om, min_fract, ksat, phh2oQ50)
  write_delim(outsoils_in, file = here("sites", site, "soils.in"),
              col_names = FALSE)

}
