# CSV cannot hold a list-column directly. extra_header_lines is stored as a
# single newline-joined string per row and split back apart on read. Extra
# header lines are never blank (read_sch.R only ever collects non-blank
# lines), so an empty joined string unambiguously round-trips to
# character(0).
.site_table_to_csv_df <- function(site_table) {
  df <- site_table
  df$extra_header_lines <- vapply(
    df$extra_header_lines, function(x) paste(x, collapse = "\n"), character(1)
  )
  df
}

.site_table_from_csv_df <- function(df) {
  if ("extra_header_lines" %in% names(df)) {
    raw <- df$extra_header_lines
    df$extra_header_lines <- lapply(raw, function(x) {
      if (is.na(x) || !nzchar(x)) character(0) else strsplit(x, "\n", fixed = TRUE)[[1]]
    })
  } else {
    df$extra_header_lines <- list(character(0))
  }
  df
}

# Explicit column types, rather than readr's guess-from-sample: a single-row
# table with a blank character value (e.g. initial_tree = "") gives the
# guesser no evidence, and it can guess logical instead of character --
# then fails to parse "" as a boolean and silently returns NA.
.sch_csv_col_types <- list(
  site = readr::cols(
    start_year = readr::col_double(), end_year = readr::col_double(),
    site_file = readr::col_character(), labeling_type = readr::col_double(),
    labeling_year = readr::col_double(), microcosm = readr::col_double(),
    co2_systems = readr::col_double(), ph_effect = readr::col_double(),
    soil_warming = readr::col_double(), n_input_scalar_option = readr::col_double(),
    omad_scalar_option = readr::col_double(), climate_scalar_option = readr::col_double(),
    initial_system = readr::col_double(), initial_crop = readr::col_character(),
    initial_tree = readr::col_character(), extra_header_lines = readr::col_character(),
    .default = readr::col_guess()
  ),
  block = readr::cols(
    block = readr::col_double(), block_comment = readr::col_character(),
    last_year = readr::col_double(), repeats_years = readr::col_double(),
    output_start_year = readr::col_double(), output_month = readr::col_double(),
    output_interval = readr::col_double(), weather_choice = readr::col_character(),
    weather_file = readr::col_character(), .default = readr::col_guess()
  ),
  event = readr::cols(
    block = readr::col_double(), year = readr::col_double(), doy = readr::col_double(),
    event = readr::col_character(), arg = readr::col_character(),
    .default = readr::col_guess()
  )
)

#' @title Write an `sch` object's tables to CSV
#'
#' @description
#' Writes `site_table`, `block_table`, and `event_table` to `site.csv`,
#' `blocks.csv`, and `events.csv` in `dir`. Thin by design: the engine
#' ([build_sch()], [write_sch()]) takes data.frames directly, so the file
#' format here is swappable without touching schedule-building logic.
#'
#' @param sch An `sch` object (or an object coercible via [as_sch()]).
#' @param dir Directory to write `site.csv` / `blocks.csv` / `events.csv`
#'   into. Created if it does not already exist.
#'
#' @return `dir`, invisibly.
#'
#' @export
write_sch_tables <- function(sch, dir) {
  sch <- as_sch(sch)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  readr::write_csv(.site_table_to_csv_df(sch$site_table), file.path(dir, "site.csv"))
  readr::write_csv(sch$block_table, file.path(dir, "blocks.csv"))
  readr::write_csv(sch$event_table, file.path(dir, "events.csv"))

  invisible(dir)
}

#' @title Read schedule tables from CSV
#'
#' @description
#' Reads `site.csv`, `blocks.csv`, and `events.csv` from `dir` (as written
#' by [write_sch_tables()]) back into plain data.frames. Returns the raw
#' tables rather than a constructed `sch` object -- pass them on to
#' [build_sch()] to validate and assemble one.
#'
#' @param dir Directory containing `site.csv`, `blocks.csv`, and
#'   `events.csv`.
#'
#' @return A named list with elements `site_table`, `block_table`, and
#'   `event_table`.
#'
#' @export
read_sch_tables <- function(dir) {
  # na = "NA" (not readr's default c("", "NA")): write_csv() writes NA as the
  # literal text "NA" and a real "" as a genuinely empty field, so reading
  # back with only "NA" treated as missing preserves that distinction --
  # otherwise blank strings like initial_tree = "" would round-trip as NA.
  site <- as.data.frame(readr::read_csv(
    file.path(dir, "site.csv"), na = "NA", col_types = .sch_csv_col_types$site,
    progress = FALSE
  ))
  blocks <- as.data.frame(readr::read_csv(
    file.path(dir, "blocks.csv"), na = "NA", col_types = .sch_csv_col_types$block,
    progress = FALSE
  ))
  events <- as.data.frame(readr::read_csv(
    file.path(dir, "events.csv"), na = "NA", col_types = .sch_csv_col_types$event,
    progress = FALSE
  ))

  site <- .site_table_from_csv_df(site)

  list(site_table = site, block_table = blocks, event_table = events)
}
