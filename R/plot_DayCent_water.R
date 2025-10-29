#' @title Visualizing Daily Water Balance Components
#'
#' @description This package provides functions for visualizing daily water balance data,
#' including inflows, outflows, and storage components, using stacked and faceted plots.
#'
#' @param scen character. Path to watrbal.out file.
#' @param soildata_file character. Path to soils.in file.
#'
#' @return This function returns plots for different water balance components.
#'
#' @details
#' This function was built to plot water balance components from DayCent outputs. This function requires the
#' following specific outfiles from DayCent:
#' - daily water balance (watrbal.out)
#' - daily volumetric soil water content by layer (vswc.out)
#' - daily water filled pore space by layer (wfps.out)
#'
#' This function also relies on the folder structure from the `DayCentRunSite` function.
#'
#' The output from this function is a list of five plots:
#' \itemize{
#' \item Plot 1: Stacked inflows
#' \item Plot 2: Stacked outflows
#' \item Plot 3: Soil and snow storage
#' \item Plot 4: Faceted inflow-storage-outflow
#' \item Plot 5: Combined inflow and outflow
#' }
#'
#' @examplesIf interactive()
#' # scen <- "cc_nt"
#'   soildata_file <- "./soils.in"
#'
#' test <- plot_DayCent_water(scen = scen,
#'                            soildata_file = soildata_file)
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import RColorBrewer
#'
#' @export
plot_DayCent_water <- function(scen = scen,
                               soildata_file){

  Watbaldata_file <- paste0("outputs/", scen, "/", scen, "_watrbal.out")
  vswcdata_file <- paste0("outputs/", scen, "/", scen, "_vswc.out")
  wfpsdata_file <- paste0("outputs/", scen, "/", scen, "_wfps.out")

  if(!file.exists(Watbaldata_file)){
    stop(paste0("Water balance file ", scen, "_watrbal.out not found in outputs scenario folder."))
  }
  if(!file.exists(vswcdata_file)){
    stop(paste0("Water balance file ", scen, "_vswc.out not found in outputs scenario folder."))
  }
  if(!file.exists(wfpsdata_file)){
    stop(paste0("Water balance file ", scen, "_wfps.out not found in outputs scenario folder."))
  }

  read_layer_data <- function(file_path){
    data = fread(file_path)
    names(data) = c("time", "dayofyr", paste0("Layer_",seq(1,(ncol(data)-2))))
    data = data %>% Add_dateCol()
  }

  # if(!is.null(years_in))
  Watbaldata <- fread(Watbaldata_file, skip = 1)
  soildata <- fread(soildata_file) %>%
    select("top_depth_cm" = V1,
           "bottem_depth_cm" = V2) %>%
    mutate(thickness_cm = bottem_depth_cm-top_depth_cm)
  soildata$layer <- paste0("Layer_", 1:nrow(soildata))
  vswcdata <- read_layer_data(vswcdata_file) #%>% select(-Layer_13)


  vswcdata_long <- vswcdata %>% pivot_longer(cols = contains("Layer"), names_to = "layer") %>%
    left_join(soildata)%>%
    mutate(
      soil_water_cm = value*thickness_cm,
      layer_num = as.numeric(gsub("Layer_", "", layer)),  # Extract numeric part
      layer = factor(layer, levels = unique(layer[rev(order(layer_num))]))  # Set order
    ) %>%
    select(-layer_num)  # Drop helper column if not needed

  # Generate a monochromatic gradient with n shades from the "Blues" palette
  layers_n <- vswcdata_long$layer %>% unique()
  Layer_blues <- rev(colorRampPalette(brewer.pal(9, "Blues"))(length(layers_n)))
  names(Layer_blues) <- layers_n


  wfpsdata <- read_layer_data(wfpsdata_file)#%>% select(-Layer_13)
  wfpsdata_long <- wfpsdata %>% pivot_longer(cols = contains("Layer"), names_to = "layer") %>%
    left_join(soildata)

  # Reshape data to long format and classify components
  Watbaldata_long <- Watbaldata  %>%
    mutate(baseflow = outflow - runoff) %>%
    pivot_longer(cols = ppt:baseflow, names_to = "component", values_to = "value") %>%
    mutate(category = case_when(
      component %in% c("ppt", "melt") ~ "Inflow", # "accum", "dsnlq",
      component %in% c("intrcpt", "evap", "transp", "sublim", "baseflow", "runoff") ~ "Outflow",
      component %in% c("snow","snlq") ~ "Storage", #"dswc",#"snow",
      TRUE ~ "Other"
    )) %>% Add_dateCol(time_col = "#time")

  # Define meaningful colors
  color_scheme <- c(
    "ppt" = "blue", "accum" = "lightblue", "dsnlq" = "cyan", "melt" = "steelblue",
    "intrcpt" = "tan", "evap" = "lightblue", "transp" = "darkgreen", "sublim" = "purple",
    "baseflow" = "blue4", "runoff" = "blue1", "dswc" = "green", "snow" = "lightgray", "snlq" = "gray"
  )

  # Accumulate daily within each year
  Watbaldata_long2 <- Watbaldata_long %>%
    mutate(year = year(date),
           water_year = ifelse(month(date) >= 10, year(date) + 1, year(date))) %>%
    group_by(category, component, year) %>%
    mutate(cum_value = cumsum(value)) %>% ungroup() %>%
    group_by(category, component, water_year) %>%
    mutate(wy_cum_value = cumsum(value)) %>% ungroup()

  # Plot 1: Stacked inflows
  plot_inflows <- ggplot(filter(Watbaldata_long2, category == "Inflow"),
                         aes(x = date, y = cum_value, fill = component)) +
    geom_area() +
    scale_fill_manual(values = color_scheme) + theme_minimal() +
    labs(title = "Cumulative Inflows", y = "Cumulative Water (cm)", x = "Day of Year")

  plot_inflows_wy <- ggplot(filter(Watbaldata_long2, category == "Inflow"),
                            aes(x = date, y = wy_cum_value, fill = component)) +
    geom_area() +
    scale_fill_manual(values = color_scheme) + theme_minimal() +
    labs(title = "Cumulative Inflows", y = "Cumulative Water (cm)", x = "Day of Year")

  # Plot 2: Stacked outflows
  plot_outflows <- ggplot(filter(Watbaldata_long2, category == "Outflow"),
                          aes(x = date, y = cum_value, fill = component)) +
    geom_area() +
    scale_fill_manual(values = color_scheme) + theme_minimal() +
    labs(title = "Cumulative Outflows", y = "Cumulative Water (cm)", x = "Day of Year")

  plot_outflows_wy <- ggplot(filter(Watbaldata_long2, category == "Outflow"),
                             aes(x = date, y = wy_cum_value, fill = component)) +
    geom_area() +
    scale_fill_manual(values = color_scheme) + theme_minimal() +
    labs(title = "Cumulative Outflows", y = "Cumulative Water (cm)", x = "Day of Year")


  # prep storage data

  Watbaldata_long3 <- Watbaldata_long2 %>%
    filter(category != "Other") %>%  # Filter first
    rename(time = `#time`)

  Watbaldata_long3 = Watbaldata_long3%>%  # Rename column
    rbind(vswcdata_long %>% mutate(category  = "Storage",
                                   component = layer,
                                   year = year(date),
                                   water_year = ifelse(month(date) >= 10, year(date) + 1, year(date)),                                     value = -soil_water_cm,
                                   cum_value = -soil_water_cm,
                                   wy_cum_value = -soil_water_cm) %>%
            select(all_of(names(Watbaldata_long3)))
    ) %>%
    mutate(
      cum_value = ifelse(str_detect(component, "snlq|snow"), value, cum_value),
      wy_cum_value = ifelse(str_detect(component, "snlq|snow"), value, wy_cum_value),
      category = factor(category, levels = c("Inflow", "Storage", "Outflow"))
    )

  # Define fixed order for non-layer components
  static_order <- c("ppt", "melt", "snlq", "snow",  "baseflow", "transp","evap","runoff",  "sublim","intrcpt")

  # Extract and sort layer components manually
  layer_order <- Watbaldata_long3$component %>%
    unique() %>%
    grep("^Layer_", ., value = TRUE) %>%  # Select only layers
    data.frame(layer = .) %>%  # Convert to dataframe
    mutate(layer_num = as.numeric(sub("Layer_", "", layer))) %>%  # Extract numeric part
    arrange(desc(layer_num)) %>%  # Sort descending (Layer_N to Layer_1)
    pull(layer)  # Extract as a character vector

  # Combine static and dynamic orders
  final_order <- c(static_order[1:3], layer_order, static_order[4:length(static_order)])

  # Apply ordered factor to component column (using base R method)
  Watbaldata_long3$component <- factor(Watbaldata_long3$component, levels = final_order)

  # Ensure color scheme matches the final order
  color_scheme_select <- c(color_scheme, Layer_blues)[final_order]


  # Plot 3: Soil and snow storage
  plot_storage <- ggplot(filter(Watbaldata_long3, category == "Storage"), aes(x = date, y = value, fill = component)) +
    geom_area(col = "grey75", linewidth = 0.01) + scale_fill_manual(values = color_scheme_select) + theme_minimal() +
    labs(title = "Daily Water Storage", y = "Water Storage (cm)", x = "Day of Year")

  plot_storage_wy <- ggplot(filter(Watbaldata_long3, category == "Storage"),
                            aes(x = date, y = wy_cum_value, fill = component)) +
    geom_area(col = "grey75", linewidth = 0.01) +
    scale_fill_manual(values = color_scheme_select) + theme_minimal() +
    labs(title = "Daily Water Storage", y = "Water Storage (cm)", x = "Day of Year")

  # Plot 4: Faceted inflow-storage-outflow
  plot_faceted <- ggplot(Watbaldata_long3,
                         aes(x = date, y = cum_value, fill = component)) +
    geom_area(color = "grey75", linewidth = .01) + scale_fill_manual(values = color_scheme_select) + theme_minimal() +
    facet_grid(rows = vars(category), scales = "free_y") +
    labs(title = "Water Balance Components", y = "Water (cm)", x = "Day of Year")

  plot_faceted_wy <- ggplot(Watbaldata_long3,
                            aes(x = date, y = wy_cum_value, fill = component)) +
    geom_area(color = "grey75", linewidth = .01) +
    scale_fill_manual(values = color_scheme_select) + theme_minimal() +
    facet_grid(rows = vars(category), scales = "free_y") +
    labs(title = "Water Balance Components", y = "Water (cm)", x = "Day of Year")

  # Plot 5: Combined inflow and outflow
  inflow_outflow <- filter(Watbaldata_long2, category %in% c("Inflow", "Outflow")) %>%
    mutate(value = ifelse(category == "Outflow", -value, value)) %>%
    group_by(date, category) %>% summarize (Value = sum(value),
                                            cum_value = sum(cum_value),
                                            wy_cum_value = sum(wy_cum_value))

  # Compute daily net balance
  daily_net <- inflow_outflow %>%
    group_by(date) %>%
    summarize(Value = sum(Value), cum_value = sum(cum_value)) %>%
    mutate(category = "Net Balance")

  plot_combined <- ggplot(inflow_outflow, aes(x = date, y = cum_value, fill = category)) +
    geom_area(position = "identity", alpha = 0.75) +
    geom_line(data= daily_net, aes(x = date, y = cum_value, col = category), linewidth = 1)+
    scale_fill_manual(values = c("Inflow" = "lightblue", "Outflow" = "gold", "Net Balance"= "darkgreen")) +
    scale_color_manual(values = c("Inflow" = "lightblue", "Outflow" = "gold", "Net Balance"= "darkgreen")) +
    theme_minimal() +
    labs(title = "Cumulative Inflow and Outflow", y = "Cumulative Water (cm)", x = "Day of Year")

  # Compute daily net balance
  wy_daily_net <- inflow_outflow %>%
    group_by(date) %>%
    summarize(Value = sum(Value), wy_cum_value = sum(wy_cum_value)) %>%
    mutate(category = "Net Balance")

  plot_combined_wy <- ggplot(inflow_outflow, aes(x = date, y = wy_cum_value, fill = category)) +
    geom_area(position = "identity", alpha = 0.75) +
    geom_line(data= wy_daily_net, aes(x = date, y = wy_cum_value, col = category), linewidth = 1)+
    scale_fill_manual(values = c("Inflow" = "lightblue", "Outflow" = "gold", "Net Balance"= "darkgreen")) +
    scale_color_manual(values = c("Inflow" = "lightblue", "Outflow" = "gold", "Net Balance"= "darkgreen")) +
    theme_minimal() +
    labs(title = "Cumulative Inflow and Outflow", y = "Cumulative Water (cm)", x = "Day of Year")


  plot_Wfps <- ggplot(wfpsdata_long, aes(x = date, y = (bottem_depth_cm + top_depth_cm) / 2, fill = value)) +
    geom_tile(aes(height = top_depth_cm - bottem_depth_cm), width = 1) +
    scale_y_reverse() +
    labs(x = "Date", y = "Soil Depth (cm)", fill = "Degree of /nSaturation",
         main = "Soil Water Saturation") +
    scale_fill_gradient(low = "lightyellow", high = "steelblue") +
    theme_minimal()


  plot_vswc <- ggplot(vswcdata_long, aes(x = date, y = soil_water_cm, fill = layer)) +
    geom_area() +
    scale_y_reverse() +
    labs(x = "Date", y = "Soil Water (cm)", fill = "Moisture",
         title = "Depth of soil water by layer") +
    scale_fill_manual(values = Layer_blues) +  # Apply the custom gradient
    theme_minimal()


  return(list(#"inflows" = plot_inflows, "outflows" = plot_outflows,
    #"all_flows" = plot_faceted, "inflow_outflow" = plot_combined,
    "wy_inflows" = plot_inflows_wy, "wy_outflows" = plot_outflows_wy,
    "Wy_storage" = plot_storage_wy, plot_faceted_wy, "wy_inflow_outflow" = plot_combined_wy,
    "moisture_content_prct" = plot_Wfps, "water_content_cm" = plot_vswc))
}

