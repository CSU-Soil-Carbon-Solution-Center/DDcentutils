#' @title Required columns for a schedule table
#'
#' @description
#' Returns the canonical, required column names for one of the three tidy
#' tables that make up an `sch` object (`site_table`, `block_table`,
#' `event_table`). Every function that builds, reads, or writes a schedule
#' table should call this rather than hard-coding column names.
#'
#' @param which One of `"site"`, `"block"`, or `"event"`.
#'
#' @return A character vector of required column names, in canonical order.
#'
#' @examples
#' sch_columns("site")
#' sch_columns("block")
#' sch_columns("event")
#'
#' @export
sch_columns <- function(which = c("site", "block", "event")) {
  which <- match.arg(which)

  switch(which,
    site = c(
      "start_year", "end_year", "site_file", "labeling_type", "labeling_year",
      "microcosm", "co2_systems", "ph_effect", "soil_warming",
      "n_input_scalar_option", "omad_scalar_option", "climate_scalar_option",
      "initial_system", "initial_crop", "initial_tree", "extra_header_lines"
    ),
    block = c(
      "block", "block_comment", "last_year", "repeats_years",
      "output_start_year", "output_month", "output_interval",
      "weather_choice", "weather_file"
    ),
    event = c("block", "year", "doy", "event", "arg")
  )
}

.sch_column_types <- list(
  site = c(
    start_year = "numeric", end_year = "numeric", site_file = "character",
    labeling_type = "numeric", labeling_year = "numeric", microcosm = "numeric",
    co2_systems = "numeric", ph_effect = "numeric", soil_warming = "numeric",
    n_input_scalar_option = "numeric", omad_scalar_option = "numeric",
    climate_scalar_option = "numeric", initial_system = "numeric",
    initial_crop = "character", initial_tree = "character",
    extra_header_lines = "list"
  ),
  block = c(
    block = "numeric", block_comment = "character", last_year = "numeric",
    repeats_years = "numeric", output_start_year = "numeric",
    output_month = "numeric", output_interval = "numeric",
    weather_choice = "character", weather_file = "character"
  ),
  event = c(
    block = "numeric", year = "numeric", doy = "numeric",
    event = "character", arg = "character"
  )
)

.sch_table_label <- c(site = "site_table", block = "block_table", event = "event_table")

.coerce_sch_column <- function(x, target, table_name, col_name) {
  fail <- function() {
    stop(
      sprintf(
        "new_sch(): column '%s' in %s has type '%s', which cannot be used as %s",
        col_name, table_name, class(x)[1], target
      ),
      call. = FALSE
    )
  }

  if (target == "numeric") {
    if (is.numeric(x)) return(x)
    if (is.logical(x) && all(is.na(x))) return(as.numeric(x))
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      out <- suppressWarnings(as.numeric(x))
      if (any(is.na(out) & !is.na(x))) fail()
      return(out)
    }
    fail()
  }

  if (target == "character") {
    if (is.character(x)) return(x)
    if (is.factor(x)) return(as.character(x))
    if (is.numeric(x) || is.logical(x)) return(as.character(x))
    fail()
  }

  if (target == "list") {
    if (is.list(x) && !is.data.frame(x)) {
      return(lapply(x, function(el) {
        if (is.null(el)) character(0) else as.character(el)
      }))
    }
    if (is.character(x)) return(as.list(x))
    fail()
  }

  fail()
}

.check_and_coerce_sch_table <- function(df, which) {
  table_name <- .sch_table_label[[which]]

  if (!is.data.frame(df)) {
    stop(sprintf("new_sch(): %s must be a data.frame", table_name), call. = FALSE)
  }

  required <- sch_columns(which)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "new_sch(): %s is missing required column(s): %s",
        table_name, paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  types <- .sch_column_types[[which]]
  for (col in required) {
    df[[col]] <- .coerce_sch_column(df[[col]], types[[col]], table_name, col)
  }

  df
}

#' @title Construct an `sch` object
#'
#' @description
#' Builds the in-memory representation of a DayCent schedule from three
#' tidy tables. Required columns are defined by [sch_columns()]; missing
#' columns error, and columns are type-checked and coerced where a sensible
#' coercion exists (e.g. numeric-looking character columns).
#'
#' @param site_table A data.frame with the columns from `sch_columns("site")`.
#'   One row per schedule.
#' @param block_table A data.frame with the columns from `sch_columns("block")`.
#'   One row per block.
#' @param event_table A data.frame with the columns from `sch_columns("event")`.
#'   One row per event.
#' @param extra A named list for fields outside the three core tables (for
#'   example `parsed_ok` / `parse_error`, set when a schedule is parsed from
#'   file). Defaults to an empty list.
#'
#' @return An object of class `"sch"`.
#'
#' @examples
#' site <- data.frame(
#'   start_year = 1980, end_year = 1990, site_file = "site.100",
#'   labeling_type = 0, labeling_year = -1, microcosm = -1,
#'   co2_systems = -1, ph_effect = -1, soil_warming = -1,
#'   n_input_scalar_option = 0, omad_scalar_option = 0,
#'   climate_scalar_option = -1, initial_system = 1,
#'   initial_crop = "W1", initial_tree = ""
#' )
#' site$extra_header_lines <- list(character(0))
#' block <- data.frame(
#'   block = 1, block_comment = "", last_year = 1990, repeats_years = 11,
#'   output_start_year = 1980, output_month = 12, output_interval = 1,
#'   weather_choice = "C", weather_file = NA_character_
#' )
#' event <- data.frame(block = 1, year = 1, doy = 1, event = "CROP", arg = "W1")
#' s <- new_sch(site, block, event)
#' print(s)
#'
#' @export
new_sch <- function(site_table, block_table, event_table, extra = list()) {
  site_table  <- .check_and_coerce_sch_table(site_table, "site")
  block_table <- .check_and_coerce_sch_table(block_table, "block")
  event_table <- .check_and_coerce_sch_table(event_table, "event")

  structure(
    list(
      site_table = site_table,
      block_table = block_table,
      event_table = event_table,
      extra = extra
    ),
    class = "sch"
  )
}

#' @title Coerce an object to an `sch`
#'
#' @description
#' Generic entry point for converting other representations (e.g. a list of
#' tables read from CSV) into an `sch` object. The default method errors;
#' the `"sch"` method returns its argument unchanged.
#'
#' @param x Object to coerce.
#' @param ... Passed to methods.
#'
#' @return An object of class `"sch"`.
#'
#' @export
as_sch <- function(x, ...) {
  UseMethod("as_sch")
}

#' @rdname as_sch
#' @export
as_sch.sch <- function(x, ...) {
  x
}

#' @rdname as_sch
#' @export
as_sch.default <- function(x, ...) {
  stop(
    sprintf(
      "as_sch(): don't know how to coerce an object of class '%s' to 'sch'",
      paste(class(x), collapse = "/")
    ),
    call. = FALSE
  )
}

#' @title Print a summary of an `sch` object
#'
#' @description
#' Prints a one-screen summary: year span, site file, block count, events
#' per block, and weather reference per block.
#'
#' @param x An `sch` object.
#' @param ... Unused; present for S3 consistency.
#'
#' @return `x`, invisibly.
#'
#' @export
print.sch <- function(x, ...) {
  st <- x$site_table
  bt <- x$block_table
  et <- x$event_table

  start_year <- if (nrow(st) > 0) st$start_year[1] else NA
  end_year   <- if (nrow(st) > 0) st$end_year[1] else NA
  site_file  <- if (nrow(st) > 0) st$site_file[1] else NA_character_
  n_blocks   <- nrow(bt)

  cat("<sch>\n")
  cat(sprintf("  years:     %s-%s\n", start_year, end_year))
  cat(sprintf("  site file: %s\n", site_file))
  cat(sprintf("  blocks:    %d\n", n_blocks))

  if (n_blocks > 0) {
    event_counts <- table(factor(et$block, levels = bt$block))
    for (i in seq_len(n_blocks)) {
      weather_choice <- bt$weather_choice[i]
      weather_file   <- bt$weather_file[i]
      weather_desc <- if (identical(weather_choice, "F") && !is.na(weather_file)) {
        sprintf("F (%s)", weather_file)
      } else {
        as.character(weather_choice)
      }
      n_events <- event_counts[[as.character(bt$block[i])]]
      cat(sprintf(
        "    block %s: %d events, weather %s\n",
        bt$block[i], n_events, weather_desc
      ))
    }
  }

  invisible(x)
}
