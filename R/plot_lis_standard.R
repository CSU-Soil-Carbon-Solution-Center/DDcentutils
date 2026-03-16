#' @title Standard plots for lis output: SOC stock, aboveground biomass, and harvested grain C.
#'
#' @param lis_df data.frame. From build_lis_from_bin()$all (or similar).
#' @param site character. Used for plot titles.
#' @param run character. Used for plot titles.
#' @param agb_col character. Column name for aboveground biomass. Default is "agcprd".
#'
#' @return list of ggplot objects: p_soc, p_agb, p_cgrain.
#'
#' @import ggplot2
#'
#' @export
plot_lis_standard <- function(lis_df, site = "", run = "", agb_col = 'agcprd') {

  # SOC plot expects somsc (g/m2). Convert to Mg/ha: g/m2 * 10 / 1000
  if (!("somsc" %in% names(lis_df))) stop("plot_lis_standard: 'somsc' not found in lis_df.")

  if (agb_col == 'agcprd' & !("agcprd" %in% names(lis_df))) stop("plot_lis_standard: 'agcprd' not found in lis_df.")

  if (agb_col != 'agcprd' & !(agb_col %in% names(lis_df))) stop(paste0("plot_lis_standard: ", agb_col, " not found in lis_df."))

  if (!("cgrain" %in% names(lis_df))) stop("plot_lis_standard: 'cgrain' not found in lis_df.")

  # # try to guess AGB column if not provided
  # if (is.null(agb_col)) {
  #   candidates <- c("aglivc", "aglive", "aglivb", "abovegndc", "shootc", "cgrain", "biomass")
  #   agb_col <- candidates[candidates %in% names(lis_df)][1]
  #   if (is.na(agb_col)) {
  #     stop("plot_lis_standard: couldn't find an aboveground biomass column. ",
  #          "Pass agb_col explicitly (e.g., agb_col='aglivc').")
  #   }
  # }
  # if (!(agb_col %in% names(lis_df))) stop("plot_lis_standard: agb_col not found: ", agb_col)

  p_soc <- ggplot2::ggplot(lis_df) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = somsc * 10 / 1000, color = run_period)) +
    ggplot2::geom_point(ggplot2::aes(x = time, y = somsc * 10 / 1000, color = run_period), size = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste(site, run, "SOC stock (0-30 cm) for eq, base, and experiment periods"),
      x = "Year",
      y = "SOC stock (Mg/ha)",
      color = "Period"
    )

  p_agb <- ggplot2::ggplot(lis_df %>%
                             mutate(year = floor(time)) %>%
                             group_by(year, run_period) %>%
                             summarise_at(vars(agb_col), max)) +
    ggplot2::geom_line(ggplot2::aes(x = year, y = .data[[agb_col]], color = run_period)) +
    ggplot2::geom_point(ggplot2::aes(x = year, y = .data[[agb_col]], color = run_period), size = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste(site, run, "Aboveground biomass:", agb_col),
      x = "Year",
      y = "Aboveground biomass C g/m2",
      color = "Period"
    )

  p_cgrain <- ggplot2::ggplot(lis_df %>%
                                mutate(year = floor(time)) %>%
                                group_by(year, run_period) %>%
                                summarise(cgrain = max(cgrain))) +
    # ggplot2::geom_line(ggplot2::aes(x = year, y = cgrain, color = run_period)) +
    ggplot2::geom_point(ggplot2::aes(x = year, y = cgrain, color = run_period), size = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste(site, run, "Harvested Grain C g/m2 cgrain"),
      x = "Year",
      y = "Harvested Grain C g/m2",
      color = "Period"
    )


  list(p_soc = p_soc, p_agb = p_agb, p_cgrain = p_cgrain)
}
