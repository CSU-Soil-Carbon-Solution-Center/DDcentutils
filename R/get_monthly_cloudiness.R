#' @title Get Monthly Cloudiness from NASA POWER
#'
#' @description Retrieves monthly average cloud cover and computes solar transmission estimates.
#'
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @param start_year First year of data.
#' @param end_year Last year of data.
#' @param plot Logical; whether to generate a cloudiness plot.
#' @param ... Additional arguments passed to `get_power`.
#'
#' @return A data frame of monthly cloud cover and estimated transmission.
#'
#' @importFrom nasapower get_power
#' @importFrom dplyr select %>% group_by summarise mutate arrange
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
#' @export
get_monthly_cloudiness <- function(lat, lon, start_year = 1985, end_year = 2024, plot = FALSE,...) {
  cloud_data <- get_power(
    community = "AG",
    lonlat = c(lon, lat),
    pars = "CLOUD_AMT",
    temporal_api = "monthly",
    dates = c(paste0(start_year, "-01-01"), paste0(end_year, "-12-31"))
  )

  cloud_df <- cloud_data %>%
    select(-YEAR) %>%
    group_by(LON, LAT, PARAMETER) %>%
    summarise(across(JAN:DEC, ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(cols = JAN:DEC, names_to = "month", values_to = "cloud_percent") %>%
    mutate(
      month_num = match(tolower(month), tolower(month.abb)),
      transmission = signif(1 - cloud_percent / 100, 2)
    ) %>%
    arrange(month_num)

  if (plot) {
    ggplot(cloud_df, aes(x = month_num, y = cloud_percent)) +
      geom_line(color = "blue") +
      geom_point() +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      labs(
        title = sprintf("Monthly Cloudiness at %.2f\u00b0N, %.2f\u00b0W", lat, lon),
        x = "Month", y = "Cloudiness (%)"
      ) +
      theme_minimal()
  }

  return(cloud_df)
}
