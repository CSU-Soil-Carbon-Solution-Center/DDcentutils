.lib100_block_id_re <- "^([A-Za-z][A-Za-z0-9_]*)\\b"
.lib100_numeric_re <- "^[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?\\b"

#' @title Read a DayCent library (.100) file's block IDs
#'
#' @description
#' Reads a DayCent `.100` library file (`crop.100`, `fert.100`, `harv.100`,
#' etc.) well enough to resolve the block IDs schedule events reference --
#' it does not parse parameter values. A line is a block-ID line unless it
#' is blank, a `#` comment, or starts with a number; the ID is the leading
#' `[A-Za-z][A-Za-z0-9_]*` run. IDs are collected in file order, including
#' repeats; any ID seen more than once is additionally reported in
#' `duplicate_ids`.
#'
#' @param path Path to a `.100` library file.
#'
#' @return A list with elements `path`, `block_ids` (character vector, one
#'   entry per block-ID line, in file order -- duplicates included), and
#'   `duplicate_ids` (character vector of IDs that appeared more than once,
#'   each listed once).
#'
#' @export
read_library100 <- function(path) {
  lines <- tryCatch(
    readr::read_lines(path, locale = readr::locale(encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(lines)) {
    stop(sprintf("read_library100(): could not read file '%s'.", path), call. = FALSE)
  }

  ids <- character(0)
  duplicates <- character(0)
  seen <- character(0)

  for (raw in lines) {
    stripped <- trimws(raw)
    if (!nzchar(stripped) || startsWith(stripped, "#")) next
    if (grepl(.lib100_numeric_re, stripped, perl = TRUE)) next

    m <- regmatches(stripped, regexec(.lib100_block_id_re, stripped, perl = TRUE))[[1]]
    if (length(m) < 2) next
    block_id <- m[2]

    if (block_id %in% seen && !(block_id %in% duplicates)) {
      duplicates <- c(duplicates, block_id)
    }
    seen <- c(seen, block_id)
    ids <- c(ids, block_id)
  }

  list(path = path, block_ids = ids, duplicate_ids = duplicates)
}

.load_schedule_events_config <- function() {
  path <- system.file("config", "daycent", "schedule_events.v1.json", package = "DDcentutils")
  jsonlite::fromJSON(path)
}

#' @title Read all DayCent library (.100) files in a directory
#'
#' @description
#' Calls [read_library100()] over every file in `files` that exists in
#' `dir`. Defaults `files` to the library filenames named in the vendored
#' `schedule_events.v1.json`'s `event_to_library` map (`crop.100`,
#' `fert.100`, `harv.100`, ...). A file that does not exist is simply
#' absent from the result -- this function does not decide whether that
#' absence is a problem; the schedule validator does.
#'
#' @param dir Directory to look for `.100` files in.
#' @param files Optional character vector of filenames to look for.
#'   Defaults to the library files named in `event_to_library`.
#'
#' @return A named list, keyed by filename, of [read_library100()] results
#'   for each file that was found.
#'
#' @export
read_library_dir <- function(dir, files = NULL) {
  if (is.null(files)) {
    events_cfg <- .load_schedule_events_config()
    files <- unique(unlist(events_cfg$event_to_library, use.names = FALSE))
  }

  result <- list()
  for (f in files) {
    path <- file.path(dir, f)
    if (file.exists(path)) {
      result[[f]] <- read_library100(path)
    }
  }
  result
}
