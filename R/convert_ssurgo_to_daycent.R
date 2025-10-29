#' @title Convert SSURGO data to DayCent format
#'
#' @description This function converts SSURGO data into a DayCent-ready file format for specific locations.
#' SSURGO data is interpolated to fit the soil depth intervals for DayCent when necessary.
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
#' This function leverages the get_soil_properties function to obtain SSURGO data that is then converted into
#' a DayCent-ready file format for specific locations. Downloaded SSURGO data might not have all the soil layers necessary
#' for a DayCent run; this function interpolates the data for soil layers using a simple weighted approach based on
#' the overlap between the SSURGO and DayCent soil layer thickness.
#' The templatesite name should match the file name in the site folder where DayCent is being run.
#' The site name must correpond to the folder where the exported file will be saved.
#' The returned soils.in file follows the template format, and thus includes 13 variables:
#' \itemize{
#' \item upper_depth_cm, from soils.in template file.
#' \item lower_depth_cm, from soils.in template file.
#' \item dthirdbar_r, interpolated from SSURGO data.
#' \item wthirdbar_r, interpolated from SSURGO data.
#' \item wfifteenbar_r, interpolated from SSURGO data.
#' \item evap_coeff, from soils.in template file.
#' \item fract_roots, from soils.in template file.
#' \item sandtotal_r, interpolated from SSURGO data.
#' \item claytotal_r, interpolated from SSURGO data.
#' \item om_r, interpolated from SSURGO data.
#' \item min_fract, from soils.in template file.
#' \item ksat_r, interpolated from SSURGO data.
#' \item ph1to1h2o_r, interpolated from SSURGO data.
#' }
#'
#' @examplesIf interactive()
#' latitude <- 45.7
#' longitude <- -118.6
#' site <- "test_site" ## this should be an existing folder within the sites folder for a DayCent run
#' templatesite <- "test_soils" ## this should be an existing soils.in to be used as a template
#' soils <- convert_ssurgo_to_daycent(latitude, longitude, site, templatesite) ## the file will be saved in the specified site folder.
#'
#' @import dplyr
#' @import readr
#' @import data.table
#' @import here
#'
#' @export
convert_ssurgo_to_daycent <- function(latitude, longitude, site, templatesite) {

  # Load template soil file
  tempsoil_in <- fread(here("sites", templatesite, "soils.in")) %>% filter(!is.na(V1))
  names(tempsoil_in) <- c("upper_depth_cm", "lower_depth_cm", "bd_gcm3", "fc_vol", "wp_vol",
                          "evap_coeff", "fract_roots", "sand_frac", "clay_fract", "OM_fract",
                          "min_fract", "ksat_cmsec", "pH")

  # Get SSURGO soils
  ssurgo_soils <- get_soil_properties(latitude, longitude)

  # Interpolate to DayCent layers
  profiles <- tempsoil_in %>% select(upper_depth_cm,lower_depth_cm)
  # Step 1: Calculate overlap and weights for each horizon-profile pair
  soil_expanded <- ssurgo_soils %>% select(hzdept_r,hzdepb_r) %>%
    rowwise() %>%
    mutate(
      profile_data = list(
        profiles %>%
          filter(hzdepb_r > upper_depth_cm & hzdept_r < lower_depth_cm) %>%
          mutate(
            Overlap = pmin(lower_depth_cm, hzdepb_r) - pmax(upper_depth_cm, hzdept_r),
            Weight = Overlap / (lower_depth_cm - upper_depth_cm)
          )
      )
    ) %>%
    unnest(cols = c(profile_data))

  relevant_columns <- c("hzdept_r", "hzdepb_r", "sandtotal_r","claytotal_r", "dbthirdbar_r",
                        "om_r", "wthirdbar_r", "wfifteenbar_r", "ksat_r", "ph1to1h2o_r")

  soil_expanded_full <- soil_expanded %>%
    left_join(ssurgo_soils %>% select(all_of(relevant_columns)), by = c("hzdept_r", "hzdepb_r"))

  # Step 2: Expand ssurgo_soils with profiles
  interpolated_profiles <- soil_expanded_full %>%
    group_by(upper_depth_cm, lower_depth_cm) %>%
    summarise(
      sandtotal_r = sum(sandtotal_r/100 * Weight, na.rm = TRUE)%>% round(3),
      claytotal_r = sum(claytotal_r/100 * Weight, na.rm = TRUE)%>% round(3),
      dbthirdbar_r = sum(dbthirdbar_r * Weight, na.rm = TRUE)%>% round(3),
      om_r = sum(om_r/100 * Weight, na.rm = TRUE)%>% round(3),
      wthirdbar_r = sum(wthirdbar_r/100 * Weight, na.rm = TRUE)%>% round(3),
      wfifteenbar_r = sum(wfifteenbar_r/100 * Weight, na.rm = TRUE)%>% round(3),
      ksat_r = sum(ksat_r*1e-4 * Weight, na.rm = TRUE)%>% round(3),
      ph1to1h2o_r = sum(ph1to1h2o_r* Weight, na.rm = TRUE) %>% round(3),
      .groups = "drop"
    )

  # Final interpolated file
  outsoils_in <- interpolated_profiles %>%
    cbind(tempsoil_in %>% select(evap_coeff, fract_roots, min_fract)) %>%
    select(upper_depth_cm,lower_depth_cm,dbthirdbar_r,wthirdbar_r, wfifteenbar_r,
           evap_coeff, fract_roots, sandtotal_r, claytotal_r,om_r, min_fract,ksat_r, ph1to1h2o_r)

  # Write output file
  write_delim(outsoils_in, file = here("sites", site, "soils.in"), col_names = FALSE)
}
