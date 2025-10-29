#' @title Download NASA Power weather data for DayCent run
#'
#' @description  This function retrieves the NASA Power weather data of a specific site based on its latitude and longitude
#' using the `nasapower` package. The output includes weather variables necessary for a DayCent run: day of the month,
#' month, maximum and minimum temperature (in degrees Celsius), daily precipitation (cm), and shortwave radiation (W/m2).
#'
#' @param raw_data_path character. Specifies the path where the Daymet files will be saved. Default saves files in temporary folder.
#' @param site character. The name or identifier for the site.
#' @param lat numeric. Latitude of the site in decimal degrees.
#' @param lon numeric. Longitude of the site in decimal degrees.
#' @param start numeric. Specifies the start year to download weather data.
#' @param end numeric. Specifies the end year to download weather data.
#'
#' @return A data frame with all dates from the start to the end year and data for weather variables necessary for a DayCent run.
#'
#' @details
#' The function downloads NASA Power weather data based on a user-specified time interval using data from the Agroclimatology Archive.
#' The function then saves the weather file to the working directory ./sites/{site}/{site}_daymet.wth in the 9 column format.
#' The returned data frame includes the following order necessary for a DayCent run:
#' \itemize{
#' \item day, corresponds to the day of the month.
#' \item month, corresponds to the month of the year.
#' \item year, correponds to the year.
#' \item yday, corresponds to the day of the year.
#' \item tmax_C, maximum temperature, in C.
#' \item tmin_C, minimum temperature, in C.
#' \item prcp_cm_day, daily precipitation, in cm.
#' }
#' These seven variables are required in all weather files regardless of the DayCent version.
#' Nonetheless, the returned data frame also includes:
#' \itemize{
#' \item srad_Wm2, shortwave radiation, in langley d−1.
#' \item RH2M, relative humidity, in %.
#' \item WS2M, wind speed at 2 meters, in mph.
#' }
#'
#' @examples
#' # Example usage
#' lat <- 40.58
#' lon <- -105.08
#' site <- "Sample Site"
#' weather <- getNASAPowerData(raw_data_path = NULL, site = site,
#'                             lat = lat, lon = lon, start = 2000, end = 2020)
#'
#' @import nasapower
#' @import dplyr
#'
#' @export
getNASAPowerData <- function(raw_data_path = NULL,
                             site = site, lat = lat, lon = lon,
                             start = start, end = end){

  weather_data <- get_power(
    community = "ag",
    pars = c("T2M_MIN", "T2M_MAX", "PRECTOTCORR", "RH2M", "ALLSKY_SFC_SW_DWN", "WS2M" ),
    # need all the parameters here: Tmin (C), Tmax (C), Prcip (mm/day), RHm (%), ALLSKY_SFC_SW_DWN (W/m²), wind (m/s),
    temporal_api = "daily",
    lonlat = c(lon,lat),
    dates = c(paste0(start, "-01-01"), paste0(end, "-12-31")))

  # Updating units and aligning with daycent variable order
  weather_data_DayCent <- weather_data %>% mutate(day = day(YYYYMMDD),
                                                  month = month(YYYYMMDD),
                                                  monthDay = mday(YYYYMMDD),
                                                  yday = DOY,
                                                  year = YEAR,
                                                  tmax_C = T2M_MAX,
                                                  tmin_C = T2M_MIN,
                                                  prcp_cm_day = PRECTOTCORR/10,
                                                  RH2M = RH2M,
                                                  srad_Ld = ALLSKY_SFC_SW_DWN*41.67*0.484583, #converting from kwh m2 to W m2 to 1 Langley/day = 0.484583 Watt/m2
                                                  c = WS2M*2.236) %>% # from ms to mph
    dplyr::select(day, monthDay, month, year, yday, tmax_C, tmin_C, prcp_cm_day, srad_Wm2, RH2M, WS2M)

  #double check that NASA power includes leap years.

  # We could let the use choose between the 7 and 10 column weather file.
  # We could also make this save out another function ind. of source to not have copy/paste code.

  weather_data_DayCent_out = weather_data_DayCent%>%
    select(monthDay, month, year,yday, tmax_C,tmin_C, prcp_cm_day,srad_Ld, RH2M, WS2M)
  temp_wth_file = here("sites", site, paste0(site, "_Power.wth"))
  write_delim(weather_data_DayCent_out, file = temp_wth_file,col_names = F)
  return(weather_data_DayCent)

}

