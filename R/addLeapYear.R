#' @title Add day 366 to Leap years
#'
#' @description This function adds the 366th day for leap years to a dataset. It duplicates the 365th day
#' for leap years, changes its day of the year (DOY) to 366, and updates the date and month.
#'
#' @param data_in A data frame containing at least the columns `year` and `yday` (day of year).
#'                The `year` column should be numeric, and the `yday` column should have values
#'                representing the day of the year (1 to 365).
#'
#' @return A data frame with an additional row for the 366th day in leap years, and with updated
#'         `date` and `month` columns.
#'
#' @details
#' This function checks for leap years (where `year %% 4 == 0`) and duplicates the data for day
#' 365 as day 366. The `date` column is then recalculated using the `year` and `yday` (day of year),
#' and the `month` column is updated accordingly.
#'
#' @examples
#' # Example dataset
#' data_in <- data.frame(year = c(2020, 2020, 2021),
#'                       yday = c(365, 1, 365))
#'
#' # Add leap year day
#' new_data <- addLeapYear(data_in)
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
  addLeapYear <- function(data_in){

    # Filter for leap years and day 365
    doy365 = data_in %>% filter(year %% 4 == 0, yday == 365)
    doy365$yday = 366  # Change to day 366 for leap year

    # Append the new rows for the 366th day
    data_in = rbind(data_in, doy365)

    # Recalculate date and month
    data_in = data_in %>% mutate(
      date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"),
      month = month(date))

    return(data_in)
  }
