#' @title Download Daymet weather data for DayCent run
#'
#' @description  This function retrieves the Daymet weather data of a specific site in North America based on its latitude and longitude
#' using the `daymetr` package, including additional rows for day 366 in leap years.
#' The output includes weather variables necessary for a DayCent run: day of the month,
#' month, maximum and minimum temperature (in degrees Celsius), daily precipitation (cm), and shortwave radiation (W/m2).
#'
#' @param raw_data_path character. Specifies the path where the Daymet files will be saved. Default saves files in temporary folder.
#' @param site character. The name or identifier for the site.
#' @param lat numeric. Latitude of the site in decimal degrees.
#' @param lon numeric. Longitude of the site in decimal degrees.
#' @param start numeric. Specifies the start year to download weather data.
#' @param end numeric. Specifies the end year to download weather data.
#'
#' @return A data frame with all dates from the start to the end year, including an additional 366th row for leap years,
#'          and data for weather variables necessary for a DayCent run.
#'
#' @details
#' The function downloads Daymet weather data based on a user-specified time interval and adds an additional row for day 366 in leap years.
#' The function then saves the weather file to the working directory ./sites/{site}/{site}_daymet.wth in the 7 column format.
#' More information about Daymet data can be found on: daymet.ornl.gov/overview.
#' The returned data frame includes the following variables in the order necessary for a DayCent run:
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
#' \item srad_Wm2, shortwave radiation, in W/m2.
#' \item VPD_kpa_day, water vapor pressure, in kPa.
#' }
#'
#' @examples
#' # Example usage
#' lat <- 40.58
#' lon <- -105.08
#' site <- "Sample Site"
#' weather <- getDaymetData(raw_data_path = NULL, site = site,
#'                           lat = lat, lon = lon, start = 2000, end = 2010)
#'
#' @import daymetr
#' @import dplyr
#' @export
getDaymetData <- function(raw_data_path = NULL, site = site,
                           lat = lat, lon = lon,
                           start = start, end = end, ...){

  outDir <- ifelse(is.null(raw_data_path), tempdir(), raw_data_path)
  download_daymet(path = outDir,
                  site = site, lat = lat, lon = lon,
                  start = start, end = end, internal = FALSE)
  weather_data <- read_csv(file.path(outDir, paste0(site,"_", start, "_", end,".csv")), skip =6)

  #add leap years
  weather_data <- addLeapYear(weather_data) %>% arrange(date)

  #Columns and unit conversions
  weather_data_DayCent <- weather_data %>% mutate(
    day = day(date),
    monthDay = mday(date),
    month = month(date),
    tmax_C = `tmax (deg c)`,
    tmin_C = `tmin (deg c)`,
    prcp_cm_day =`prcp (mm/day)`/10,
    srad_Wm2 = `srad (W/m^2)`, # daily total radiation (MJ/m2/day) = (srad (W/m2) * dayl (s/day)) / l,000,000)
    VPD_kpa_day = `vp (Pa)`/1000 # calVPDfromVP(TAvg_c =(tmax_C+tmin_C)/2, VP_kpA = `vp (Pa)`/1000)
  ) %>%
    dplyr::select(day,monthDay, month, year, yday, tmax_C, tmin_C, prcp_cm_day, srad_Wm2, VPD_kpa_day)

  weather_data_DayCent_out = weather_data_DayCent %>%
    select(monthDay,month, year,yday, tmax_C,tmin_C, prcp_cm_day)
  temp_wth_file = here("sites", site, paste0(site, "_daymet.wth"))
  write_delim(weather_data_DayCent_out, file = temp_wth_file,col_names = F)
  return(weather_data_DayCent)
}
