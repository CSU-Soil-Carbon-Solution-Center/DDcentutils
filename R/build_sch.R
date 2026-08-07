.sch_site_defaults <- list(
  labeling_type = 0,
  labeling_year = -1,
  microcosm = -1.00,
  co2_systems = -1,
  ph_effect = -1,
  soil_warming = -1,
  n_input_scalar_option = 0,
  omad_scalar_option = 0,
  climate_scalar_option = 0,
  initial_system = 1,
  initial_tree = ""
)

#' @title Build a schedule from tidy tables
#'
#' @description
#' The tidy-table entry point for constructing a DayCent schedule:
#' `site_table` (one row) plus `event_table` (one row per event), with an
#' optional `block_table`. When `block_table` is omitted, a single default
#' block spanning the full `start_year`-`end_year` range is derived. Missing
#' `site_table` columns that have a documented default (see Details) are
#' filled in; `start_year`, `end_year`, and `site_file` have no default and
#' must be supplied.
#'
#' @details
#' Default values used to fill missing `site_table` columns:
#' `labeling_type = 0`, `labeling_year = -1`, `microcosm = -1.00`,
#' `co2_systems = -1`, `ph_effect = -1`, `soil_warming = -1`,
#' `n_input_scalar_option = 0`, `omad_scalar_option = 0`,
#' `climate_scalar_option = 0`, `initial_system = 1`, `initial_tree = ""`.
#' `extra_header_lines` defaults to an empty list-column when absent.
#' `initial_crop` has no default and, like any other truly missing required
#' column, surfaces as an error from the underlying `new_sch()` call.
#'
#' `site_table` may optionally include a `weather_file` column; when
#' `block_table` is omitted, its (single) value determines the derived
#' block's `weather_choice` (`"F"` when non-`NA`, otherwise `"C"`) and
#' `weather_file`. This column is a build-time convenience only and is not
#' part of the final `sch` object's `site_table`.
#'
#' @param site_table A one-row data.frame. Must include `start_year`,
#'   `end_year`, `site_file`; other required columns are filled from
#'   documented defaults when absent (see Details).
#' @param event_table A data.frame with the columns from
#'   `sch_columns("event")`.
#' @param block_table Optional data.frame with the columns from
#'   `sch_columns("block")`. When `NULL` (the default), a single block is
#'   derived from `site_table`.
#'
#' @return An object of class `"sch"`.
#'
#' @export
build_sch <- function(site_table, event_table, block_table = NULL) {
  required_identity <- c("start_year", "end_year", "site_file")
  missing_identity <- setdiff(required_identity, names(site_table))
  if (length(missing_identity) > 0) {
    stop(
      sprintf(
        "build_sch(): site_table is missing required column(s): %s",
        paste(missing_identity, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  weather_file <- if ("weather_file" %in% names(site_table)) {
    site_table$weather_file[1]
  } else {
    NA_character_
  }
  site_table$weather_file <- NULL

  for (col in names(.sch_site_defaults)) {
    if (!col %in% names(site_table)) {
      site_table[[col]] <- .sch_site_defaults[[col]]
    }
  }
  if (!"extra_header_lines" %in% names(site_table)) {
    site_table$extra_header_lines <- list(character(0))
  }

  if (is.null(block_table)) {
    start_year <- site_table$start_year[1]
    end_year <- site_table$end_year[1]
    weather_choice <- if (!is.na(weather_file)) "F" else "C"

    block_table <- data.frame(
      block = 1,
      block_comment = "",
      last_year = end_year,
      repeats_years = end_year - start_year + 1,
      output_start_year = start_year,
      output_month = 12,
      output_interval = 1.0,
      weather_choice = weather_choice,
      weather_file = if (weather_choice == "F") weather_file else NA_character_,
      stringsAsFactors = FALSE
    )
  }

  new_sch(site_table, block_table, event_table)
}
