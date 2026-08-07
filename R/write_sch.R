.sch_header_render_specs <- list(
  list(col = "start_year", label = "Starting year", payload_key = NULL),
  list(col = "end_year", label = "Last year", payload_key = NULL),
  list(col = "site_file", label = "Site file name", payload_key = NULL),
  list(col = "labeling_type", label = "Labeling type", payload_key = NULL),
  list(col = "labeling_year", label = "Labeling year", payload_key = NULL),
  list(col = "microcosm", label = "Microcosm", payload_key = NULL),
  list(col = "co2_systems", label = "CO2 Systems", payload_key = "co2"),
  list(col = "ph_effect", label = "pH effect", payload_key = "ph"),
  list(col = "soil_warming", label = "Soil Warming", payload_key = "soil_warming"),
  list(col = "n_input_scalar_option", label = "N input scalar option", payload_key = "n_input"),
  list(col = "omad_scalar_option", label = "OMAD scalar option", payload_key = "omad"),
  list(col = "climate_scalar_option", label = "Climate scalar option", payload_key = "climate"),
  list(col = "initial_system", label = "Initial system", payload_key = NULL),
  list(col = "initial_crop", label = "Initial crop", payload_key = NULL),
  list(col = "initial_tree", label = "Initial tree", payload_key = NULL)
)

.format_sch_value <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  if (is.character(x)) return(x)
  if (is.numeric(x) && x == round(x)) return(as.character(as.integer(x)))
  format(x, trim = TRUE, scientific = FALSE)
}

# Unlabeled lines that follow certain header fields, carrying option start
# (and sometimes end) years. Inverse of read_sch.R's
# .augment_header_with_option_payloads(). Returns character(0) when the
# site_table has no data for this option.
.render_option_payload <- function(key, site) {
  if (is.null(key)) return(character(0))

  get_col <- function(name) {
    if (!name %in% names(site)) return(NA)
    site[[name]][1]
  }

  if (key == "co2") {
    start <- get_col("co2_systems_start_year")
    if (is.na(start)) return(character(0))
    end <- get_col("co2_systems_end_year")
    if (!is.na(end)) return(paste(.format_sch_value(start), .format_sch_value(end)))
    return(.format_sch_value(start))
  }
  if (key == "ph") {
    start <- get_col("ph_effect_start_year")
    if (is.na(start)) return(character(0))
    return(.format_sch_value(start))
  }
  if (key == "soil_warming") {
    start <- get_col("soil_warming_start_year")
    if (is.na(start)) return(character(0))
    delta <- get_col("soil_warming_delta")
    out <- .format_sch_value(start)
    if (!is.na(delta)) out <- c(out, .format_sch_value(delta))
    return(out)
  }
  if (key == "n_input") {
    start <- get_col("n_input_scalar_start_year")
    if (is.na(start)) return(character(0))
    return(.format_sch_value(start))
  }
  if (key == "omad") {
    start <- get_col("omad_scalar_start_year")
    if (is.na(start)) return(character(0))
    return(.format_sch_value(start))
  }
  if (key == "climate") {
    start <- get_col("climate_scalar_start_year")
    if (is.na(start)) return(character(0))
    return(.format_sch_value(start))
  }
  character(0)
}

.render_sch_header <- function(site, value_width) {
  pad <- function(value) sprintf(paste0("%-", value_width, "s"), .format_sch_value(value))

  lines <- character(0)
  for (spec in .sch_header_render_specs) {
    value <- site[[spec$col]][1]
    lines <- c(lines, paste0(pad(value), spec$label))
    lines <- c(lines, .render_option_payload(spec$payload_key, site))
  }

  extra <- site$extra_header_lines[[1]]
  if (is.null(extra)) extra <- character(0)

  c(lines, extra)
}

.render_sch_block <- function(block_row, block_events, value_width) {
  pad <- function(value) sprintf(paste0("%-", value_width, "s"), .format_sch_value(value))

  comment <- trimws(.format_sch_value(block_row$block_comment[1]))
  block_label <- if (nzchar(comment)) paste("Block", comment) else "Block"

  lines <- c(
    paste0(pad(block_row$block[1]), block_label),
    paste0(pad(block_row$last_year[1]), "Last year"),
    paste0(pad(block_row$repeats_years[1]), "Repeats # years"),
    paste0(pad(block_row$output_start_year[1]), "Output starting year"),
    paste0(pad(block_row$output_month[1]), "Output month"),
    paste0(pad(block_row$output_interval[1]), "Output interval"),
    paste0(pad(block_row$weather_choice[1]), "Weather choice")
  )

  if (identical(block_row$weather_choice[1], "F")) {
    lines <- c(lines, .format_sch_value(block_row$weather_file[1]))
  }

  if (nrow(block_events) > 0) {
    for (i in seq_len(nrow(block_events))) {
      ev <- block_events[i, ]
      ev_line <- sprintf("%3d %3d %s", ev$year, ev$doy, ev$event)
      if (nzchar(ev$arg)) ev_line <- paste0(ev_line, " ", ev$arg)
      lines <- c(lines, ev_line)
    }
  }

  c(lines, "-999 -999 X")
}

#' @title Render an `sch` object to DayCent schedule text
#'
#' @description
#' Renders an [new_sch()] object back into DayCent `.sch` schedule text: the
#' 15-field header (with any option-payload lines and extra header lines),
#' the `Year Month Option` marker, and one block per row of `block_table`
#' with its events and `-999 -999 X` terminator. This is the inverse of
#' [read_sch()].
#'
#' @param sch An `sch` object (or an object coercible via [as_sch()]).
#' @param path Optional. If given, the rendered text is written to this
#'   path (one schedule element per line, no trailing file-level
#'   terminator).
#' @param value_width Integer. Column width the header/block-header value
#'   is left-padded to before the label. Cosmetic only — DayCent reads the
#'   first whitespace-delimited token. Defaults to `14`.
#'
#' @return The rendered character vector (one schedule line per element),
#'   invisibly.
#'
#' @export
write_sch <- function(sch, path = NULL, value_width = 14) {
  sch <- as_sch(sch)
  site <- sch$site_table
  blocks <- sch$block_table
  events <- sch$event_table

  lines <- c(
    .render_sch_header(site, value_width),
    "",
    "Year Month Option"
  )

  if (nrow(blocks) > 0) {
    for (i in seq_len(nrow(blocks))) {
      block_row <- blocks[i, ]
      block_events <- events[events$block == block_row$block[1], , drop = FALSE]
      lines <- c(lines, .render_sch_block(block_row, block_events, value_width))
    }
  }

  if (!is.null(path)) {
    readr::write_lines(lines, path)
  }

  invisible(lines)
}
