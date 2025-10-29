#' @title Soil sensitivity function
#'
#' @description This function runs a schedule file across a list of soil files. This is a flexible function that allows the user to input lists of variables,
#' and produce plots of the model output (and save to output folder).
#'
#' @param title character. Name for output folder and files exported in this function.
#' @param site character. Site name.
#' @param scen character. Describes the scenario relative to the schedule file.
#' @param exp_list character. Vector with one or more names for soil files that will replce soil.in for the soil sensitivity.
#' @param dc_exe_in Local path to the DayCent executable.
#' @param dc_path100_in character. Local path to the 100 files folder for DayCent.
#' @param run_eq logical. Default is FALSE. If TRUE, DayCent runs the equilibrium scenario.
#' @param run_base logical. Default is FALSE. If TRUE, DayCent runs the base scenario.
#' @param select_years numeric. Default is NULL. A vector that specifies the time interval (start and end year) for sensitivity analyses.
#' @param dc_var_list character. Default is NULL. A vector or list of one or more variables from the dc_sip.csv DayCent output.
#' @param dc_yr_cummsum_list character. Default is NULL. A vector or list of one or more variables from the dc_sip.csv DayCent output to analyze its cumulative response on a yearly timestep.
#' @param harvest_var_list character. Default is NULL. A vector or list of one or more variables from the harvest.csv DayCent output.
#'
#' @return The function exports the combined dc_sip results and the combined harvest results in two separate tabular-formatted files (.csv).
#' The function also exports plots of SOC and any other variables listed as single JPEG files.
#'
#' @details
#' This function requires the DayCentRunSite function and requires a path to DayCent and DDList executables. The function first runs DayCent for a specific
#' scenario from a schedule file for different soils.in files, and then collates all output files into combined dc_sip, summary, and harvest data frames. From the DayCent manual,
#' these outputs correspond to the following variables:
#'  - dc_sip.csv: Daily evaporation, transpiration, respiration, system C, and NPP.
#'  - harvest.csv: State of the system at time of harvest.
#'
#' These combined files are used to plot variables of interested listed in the function. Combined files for dc_sip and harvest are exported in tabular format (.csv).
#' The function also returns a list with the DayCent log for each scenario run.
#'
#' @examples
#' \dontrun{
#' exp <- c("./soils_n16_percent_clay.in", "./soils_n8_percent_clay.in", "./soils_0_percent_clay.in",
#'            "./soils_8_percent_clay.in", "./soils_16_percent_clay.in" ) # list of schedule files
#' sensitivity_soils(title = "Wooster clay vs sand grid search",
#'                    site = "wooster",
#'                    scen = "cc_nt",
#'                    exp_list = exp,
#'                    dc_exe_in = dc_exe, dc_path100_in = dc_path100,
#'                    select_years = c(2006,2012),
#'                    dc_var_list = list("aglivc", "NPP"),
#'                    dc_yr_cummsum_list = NULL,
#'                    harvest_var_list = NULL)
#' }
#'
#' @import dplyr
#' @import data.table
#'
#' @export
sensitivity_soils <- function(title, site, scen, exp_list,
                              dc_exe_in = dc_exe, dc_path100_in = dc_path100,
                              run_eq = FALSE, run_base = FALSE,
                              select_years = NULL,
                              dc_var_list = NULL,
                              dc_yr_cummsum_list = NULL,
                              harvest_var_list = NULL, ...) {

  file.copy("./soils.in", "./OGsoils.in")
  title_no_space = title %>% gsub(" ", "_",.)
  logs <- list()

  # Run the set of experiments and combine dc_sip data
  for (i in exp_list) {

    file.copy(i, "./soils.in", overwrite = T)
    # Run the DayCent simulation for the experiment
    print(paste0("Run DayCent with site = ", site, ", scenario = ", scen, ", soil.in = ", i))
    log <- DayCentRunSite(site = site, scen = scen, run_base = run_base, run_eq = run_eq)

    logs[[i]] <- log

    if(str_detect(log %>% tail(1), "Abnormal")){
      return(logs)
      stop()
    }

    # Read the dc_sip.csv file and add a date column
    temp_dc_sip <- read_csv(paste0("./outputs/", scen,"/",scen, "_dc_sip.csv")) %>%
      Add_dateCol() %>% mutate(soil = i)
    temp_harvest <- read_csv(paste0("./outputs/", scen,"/", scen, "_harvest.csv")) %>%
      Add_dateCol() %>% mutate(soil = i)
    # Combine the data into a single Dataframe
    if(which(i == exp_list) == 1){
      combined_dc_sip <- temp_dc_sip
      combined_harvest <- temp_harvest
    } else {
      combined_dc_sip  <- combined_dc_sip %>% add_row(temp_dc_sip)
      combined_harvest <- combined_harvest %>% add_row(temp_harvest)
    }
  }

  if (!is.null(select_years)){
    combined_dc_sip <- combined_dc_sip %>% filter(year(date) >= select_years[[1]],
                                                  year(date) <= select_years[[2]])
    combined_harvest <- combined_harvest %>% filter(year(date) >= select_years[[1]],
                                                    year(date) <= select_years[[2]])
  }

  # Create plots for the specified variables
  p_j <- list()
  p_j_names <- list()
  p_j[[1]] <- ggplot(combined_dc_sip) +
    # geom_point(aes(x = date, y = `som1c(2)`+`som2c(2)`+ som3c, col = scen)) + #.data[[j]]
    geom_line(aes(x = date, y = `som1c(2)`+`som2c(2)`+ som3c, col = soil)) + #.data[[j]]
    labs(title = paste(title, "SOC"), col = "Experiment", y = "SOC [g/m2 C]") +
    theme_classic()
  p_j_names[[1]] = paste0(title_no_space, "_SOC")
  k <- 2
  if(length(dc_var_list)>0){
    for (j in dc_var_list) {
      p_j[[k]] <- ggplot(combined_dc_sip) +
        # geom_point(aes(x = date, y = .data[[j]], col = scen)) + #.data[[j]]
        geom_line(aes(x = date, y = .data[[j]], col = soil)) + #.data[[j]]
        labs(title = paste(title, j), col = "Experiment") +
        theme_classic()
      p_j_names[[k]] = paste0(title_no_space, "_", j)
      k <- k + 1
    }
  }

  if(length(dc_yr_cummsum_list)>0){
    for (j in dc_yr_cummsum_list) {
      # Calculate cumulative sum grouped by year and scen
      combined_dc_sip_j <- combined_dc_sip %>%
        group_by(year(date), soil) %>%
        mutate(!!paste0(j, "_cum") := cumsum(.data[[j]])) # Dynamically create the cumulative sum column

      # Create the plot
      p_j[[k]] <- ggplot(combined_dc_sip_j) +
        # geom_point(aes(x = date, y = .data[[paste0(j, "_cum")]], col = scen)) + # Use cumulative column
        geom_line(aes(x = date, y = .data[[paste0(j, "_cum")]], col = soil)) +  # Use cumulative column
        labs(title = paste(title, j, "Cumulative Sum"), col = "Experiment") +
        theme_classic()
      p_j_names[[k]] = paste0(title_no_space, "_", j, "_cs")
      k <- k + 1
    }
  }

  if(length(harvest_var_list)>0){
    for (j in harvest_var_list) {
      p_j[[k]] <- ggplot(combined_harvest) +
        geom_point(aes(x = date, y = .data[[j]], col = soil, shape = crpval)) + #.data[[j]]
        geom_line(aes(x = date, y = .data[[j]], col = soil, group = interaction(scen, crpval))) + #.data[[j]]
        labs(title = paste(title, j), col = "Soil Variant", shape = "Crop") +
        theme_classic()
      p_j_names[[k]] = paste0(title_no_space, "_", j)
      k <- k + 1
    }
  }
  # Save the sensitivity report to a PDF
  save_figure_temp_path = paste0("./outputs/",title_no_space,"/")
  if(!dir.exists(save_figure_temp_path)){
    dir.create(save_figure_temp_path, recursive = T)
  }else{
    unlink(list.files(save_figure_temp_path, full.names = T))

  }

  write_csv(combined_dc_sip, file = paste0(save_figure_temp_path,
                                           title %>% gsub(" ", "_",.), "_dcsip.csv"))
  write_csv(combined_harvest, file = paste0(save_figure_temp_path,
                                            title %>% gsub(" ", "_",.), "_harvest.csv"))

  n = 1
  for (plot in p_j) {
    ggsave(filename = paste0(save_figure_temp_path,p_j_names[[n]],".jpeg"),plot = plot, width = 8, height = 6)
    n = n +1
  }

  message("Sensitivity figures saved to saved to: ", save_figure_temp_path)

  file.copy("./OGsoils.in", "./soils.in", overwrite = T)
  unlink("./OGsoils.in")
  return(logs)
}
