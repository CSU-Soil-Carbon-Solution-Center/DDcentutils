#' @title Update DayCent Site File Parameters
#'
#' @description This function reads a DayCent site file, replaces specific parameter values,
#' and writes the updated file to a new location. It supports single-line parameters
#' that follow the format: `<value> <whitespace> <PARAM(NAME)>`.
#'
#' @param site_file_in Path to the input DayCent site file.
#' @param site_file_out Path to write the updated site file.
#' @param param_names A character vector of parameter names (e.g., `"SITLAT"`, `"SITLNG"`).
#' @param param_values A numeric vector of new values corresponding to `param_names`.
#' @param wth_file_stats Optional string to annotate climate source. If not NULL,
#' replaces "*** Climate parameters" with "*** Climate statistics <wth_file_stats>".
#' #'
#' @return Invisibly returns a character vector of parameter names that were not found in the file.
#'
#' @details
#' If `wth_file_stats` file is provided, the line beginning with `*** Climate parameters` will be
#' replaced with a `*** Climate statistics <wth_file_stats>` string to calculate
#' the climate statistics from the weather data.
#'
#' Then, this function performs string matching to find and replace numeric values associated with
#' specific parameter names. It only supports single-line parameters in the format:
#'
#' ```
#' <numeric_value> <whitespace> PARAM(NAME)
#' ```
#'
#' ### Supported Parameter Groups (from standard DayCent site files):
#' - **Climate Parameters**
#'   - `PRECIP(1)` to `PRECIP(12)`
#'   - `PRCSTD(1)` to `PRCSTD(12)`
#'   - `PRCSKW(1)` to `PRCSKW(12)`
#'   - `TMN2M(1)` to `TMN2M(12)`
#'   - `TMX2M(1)` to `TMX2M(12)`
#'
#' - **Site and Control**
#'   - `IVAUTO`, `NELEM`, `SITLAT`, `SITLNG`
#'   - `SAND`, `SILT`, `CLAY`, `'ROCK'`
#'   - `BULKD`, `NLAYER`, `NLAYPG`, `DRAIN`, `BASEF`, `STORMF`, `'PRECRO'`, `'FRACRO'`, `SWFLAG`
#'
#' - **Soil Water**
#'   - `AWILT(1)` to `AWILT(10)`
#'   - `AFIEL(1)` to `AFIEL(10)`
#'
#' - **Soil Chemistry**
#'   - `PH`, `PSLSRB`, `SORPMX`
#'
#' - **Nutrient Inputs**
#'   - `EPNFA(1)`, `EPNFA(2)`
#'   - `EPNFS(1)`, `EPNFS(2)`
#'   - `SATMOS(1)`, `SATMOS(2)`
#'   - `SIRRI`
#'
#' - **Organic Matter Pools**
#'   - `SOM1CI`, `SOM2CI`, `SOM3CI`
#'   - `RCES1`, `RCES2`, `RCES3`
#'   - `CLITTR`, `RCELIT`
#'   - `AGLCIS`, `AGLIVE`, `BGLCIS`, `BGLIVE`, `STDCIS`, `STDEDE`
#'
#' - **Forest Organic Matter**
#'   - `RLVCIS`, `RLEAVE`, `FBRCIS`, `FBRCHE`, `RLWCIS`, `RLWODE`
#'   - `FRTCIS`, `FROOTE`, `CRTCIS`, `CROOTE`
#'   - `WD1CIS`, `WD2CIS`, `WD3CIS`
#'
#' - **Mineral Initial Conditions**
#'   - `MINERL(1,1)` to `MINERL(10,3)`
#'   - `PARENT(1)` to `PARENT(3)`
#'   - `SECNDY(1)` to `SECNDY(3)`
#'   - `OCCLUD`
#'
#' - **Water Initial Conditions**
#'   - `RWCF(1)` to `RWCF(10)`
#'   - `SNLQ`, `SNOW`
#'
#' Parameters not found will raise a warning and will be returned invisibly. Multi-line
#' or block-array replacement is not yet supported.
#'
#' @note Both parameter names and values must be of equal length. Matching is case-sensitive and whitespace-sensitive.
#'
#' @examples
#' \dontrun{
#' update_daycent_site_file(
#'   site_file_in = "original.site",
#'   site_file_out = "updated.site",
#'   param_names = c("SITLAT", "SITLNG"),
#'   param_values = c(47.5, -118.5)
#' )
#' }
#'
#' @export
update_daycent_site_file <- function(site_file_in, site_file_out, param_names, param_values,
                                     wth_file_stats = NULL) {
  if (length(param_names) != length(param_values)) {
    stop("Parameter name and value lists are of unequal length.")
  }

  lines <- readLines(site_file_in)

  # Replace climate header if needed
  if (!is.null(wth_file_stats)) {
    header_line <- grep("^\\*\\*\\* Climate parameters", lines)
    if (length(header_line) == 1) {
      new_header <- paste("*** Climate statistics", wth_file_stats)
      lines[header_line] <- new_header
      message(sprintf("Updated climate header: %s -> %s", "*** Climate parameters", new_header))
    } else {
      warning("Could not find unique climate header line to replace.")
    }
  }

  not_found <- character()

  for (i in seq_along(param_names)) {
    param <- param_names[i]
    value <- param_values[i]
    param_escaped <- gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", param))
    pattern <- sprintf("^\\s*[-+]?[0-9.eE+-]+\\s+%s\\b", param_escaped)
    match_index <- grep(pattern, lines)

    if (length(match_index) == 1) {
      old_line <- lines[match_index]
      new_line <- sprintf("%-20.8f %s", value, param)
      lines[match_index] <- new_line
      message(sprintf("Updated %s: %s -> %s", param, old_line, new_line))
    } else {
      warning(sprintf("Parameter '%s' not found or found multiple times.", param))
      not_found <- c(not_found, param)
    }
  }

  writeLines(lines, site_file_out)
  message(sprintf("Updated site file saved to: %s", site_file_out))
  invisible(not_found)
}

