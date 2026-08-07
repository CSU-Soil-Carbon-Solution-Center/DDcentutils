.sch_header_field_specs <- list(
  list(key = "start_year", labels = "Starting year", kind = "int"),
  list(key = "end_year", labels = "Last year", kind = "int"),
  list(key = "site_file", labels = "Site file name", kind = "str"),
  list(key = "labeling_type", labels = "Labeling type", kind = "int"),
  list(key = "labeling_year", labels = "Labeling year", kind = "int"),
  list(key = "microcosm", labels = "Microcosm", kind = "float"),
  list(key = "co2_systems", labels = "CO2 Systems", kind = "int"),
  list(key = "ph_effect", labels = c("pH effect", "pH Effect"), kind = "float"),
  list(key = "soil_warming", labels = c("Soil Warming", "Soil warming"), kind = "float"),
  list(key = "n_input_scalar_option", labels = "N input scalar option", kind = "int"),
  list(key = "omad_scalar_option", labels = "OMAD scalar option", kind = "int"),
  list(key = "climate_scalar_option", labels = "Climate scalar option", kind = "int"),
  list(key = "initial_system", labels = "Initial system", kind = "int"),
  list(key = "initial_crop", labels = "Initial crop", kind = "str"),
  list(key = "initial_tree", labels = "Initial tree", kind = "str")
)

.to_int <- function(s, default = NA_integer_) {
  val <- suppressWarnings(as.numeric(s))
  if (length(val) == 0 || is.na(val)) return(default)
  as.integer(val)
}

.to_float <- function(s, default = NA_real_) {
  val <- suppressWarnings(as.numeric(s))
  if (length(val) == 0 || is.na(val)) return(default)
  val
}

.to_str <- function(s) {
  if (is.null(s) || length(s) == 0 || is.na(s)) return("")
  trimws(s)
}

.first_token <- function(line) {
  toks <- strsplit(trimws(line), "\\s+")[[1]]
  if (length(toks) == 0) "" else toks[1]
}

.find_marker_line <- function(lines, marker) {
  target <- tolower(trimws(marker))
  for (i in seq_along(lines)) {
    if (tolower(trimws(lines[i])) == target) return(i)
  }
  NA_integer_
}

# Emulates Python's `stripped.split(maxsplit=1)` semantics: leading whitespace
# is ignored, at most one split occurs, and the remainder keeps its internal
# spacing intact.
.match_header_label <- function(line, labels) {
  stripped <- sub("[ \t\r\f\v]+$", "", line)
  lowered <- tolower(stripped)

  no_lead <- sub("^[ \t\r\f\v]+", "", stripped)
  if (no_lead == "") {
    comment <- lowered
  } else {
    m <- regmatches(no_lead, regexec("^(\\S+)[ \t\r\f\v]+(.*)$", no_lead))[[1]]
    if (length(m) == 3) {
      comment <- tolower(trimws(m[3]))
    } else {
      comment <- lowered
    }
  }

  for (label in labels) {
    label_lower <- tolower(label)
    if (endsWith(lowered, label_lower) || startsWith(comment, label_lower)) {
      return(label)
    }
  }
  NULL
}

.find_header_field <- function(lines, labels, start_idx) {
  if (start_idx > length(lines)) return(NULL)
  for (idx in start_idx:length(lines)) {
    matched <- .match_header_label(lines[idx], labels)
    if (!is.null(matched)) return(list(idx = idx, label = matched))
  }
  NULL
}

.extract_value_prefix <- function(line, label) {
  stripped <- sub("[ \t\r\f\v]+$", "", line)
  # base R's regexpr() ignores ignore.case when fixed = TRUE, so match on
  # lowercased copies instead (case-insensitive, literal substring search).
  pos <- regexpr(tolower(label), tolower(stripped), fixed = TRUE)
  if (pos > 0) {
    trimws(substr(stripped, 1, pos - 1))
  } else {
    trimws(stripped)
  }
}

.parse_header_value <- function(value_text, kind) {
  if (kind == "int") return(.to_int(value_text, default = NA_integer_))
  if (kind == "float") return(.to_float(value_text, default = NA_real_))
  .to_str(value_text)
}

.augment_header_with_option_payloads <- function(hdr, payload_lines_by_key) {
  if (length(payload_lines_by_key) == 0) return(hdr)

  co2_payload <- payload_lines_by_key[["co2_systems"]]
  if (!is.null(co2_payload)) {
    toks <- strsplit(trimws(co2_payload[1]), "\\s+")[[1]]
    if (length(toks) >= 1) hdr$co2_systems_start_year <- .to_int(toks[1], default = NA_integer_)
    if (length(toks) >= 2) hdr$co2_systems_end_year <- .to_int(toks[2], default = NA_integer_)
  }

  ph_payload <- payload_lines_by_key[["ph_effect"]]
  if (!is.null(ph_payload)) {
    hdr$ph_effect_start_year <- .to_int(.first_token(ph_payload[1]), default = NA_integer_)
  }

  soil_payload <- payload_lines_by_key[["soil_warming"]]
  if (!is.null(soil_payload)) {
    hdr$soil_warming_start_year <- .to_int(.first_token(soil_payload[1]), default = NA_integer_)
    if (length(soil_payload) >= 2) {
      hdr$soil_warming_delta <- .to_float(.first_token(soil_payload[2]), default = NA_real_)
    }
  }

  n_scalar_payload <- payload_lines_by_key[["n_input_scalar_option"]]
  if (!is.null(n_scalar_payload)) {
    hdr$n_input_scalar_start_year <- .to_int(.first_token(n_scalar_payload[1]), default = NA_integer_)
  }

  omad_payload <- payload_lines_by_key[["omad_scalar_option"]]
  if (!is.null(omad_payload)) {
    hdr$omad_scalar_start_year <- .to_int(.first_token(omad_payload[1]), default = NA_integer_)
  }

  climate_payload <- payload_lines_by_key[["climate_scalar_option"]]
  if (!is.null(climate_payload)) {
    hdr$climate_scalar_start_year <- .to_int(.first_token(climate_payload[1]), default = NA_integer_)
  }

  hdr
}

.parse_sch_header <- function(lines, marker_idx) {
  pre <- if (marker_idx > 1) lines[1:(marker_idx - 1)] else character(0)
  if (length(pre) == 0) {
    return(list(hdr = NULL, header_end_idx = NA_integer_,
                err = "Schedule header is empty before marker."))
  }

  specs <- .sch_header_field_specs
  matches <- vector("list", length(specs))
  search_idx <- 1

  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    found <- .find_header_field(pre, spec$labels, search_idx)
    if (is.null(found)) {
      labels_str <- paste(spec$labels, collapse = ", ")
      return(list(
        hdr = NULL, header_end_idx = NA_integer_,
        err = sprintf("Could not locate required schedule header field '%s' before marker.", labels_str)
      ))
    }
    matches[[i]] <- list(spec = spec, idx = found$idx, label = found$label)
    search_idx <- found$idx + 1
  }

  header_start_idx <- matches[[1]]$idx
  header_end_idx <- matches[[length(matches)]]$idx

  hdr <- list()
  payload_lines_by_key <- list()

  for (pos in seq_along(matches)) {
    mm <- matches[[pos]]
    field_line <- pre[mm$idx]
    value_text <- .extract_value_prefix(field_line, mm$label)
    hdr[[mm$spec$key]] <- .parse_header_value(value_text, mm$spec$kind)

    next_idx <- if (pos + 1 <= length(matches)) matches[[pos + 1]]$idx else header_end_idx + 1
    if (mm$idx + 1 <= next_idx - 1) {
      candidate <- pre[(mm$idx + 1):(next_idx - 1)]
      payload_lines <- candidate[nzchar(trimws(candidate))]
    } else {
      payload_lines <- character(0)
    }
    if (length(payload_lines) > 0) {
      payload_lines_by_key[[mm$spec$key]] <- payload_lines
    }
  }

  hdr <- .augment_header_with_option_payloads(hdr, payload_lines_by_key)

  if (is.na(hdr$start_year) || is.na(hdr$end_year)) {
    return(list(hdr = NULL, header_end_idx = header_end_idx,
                err = "Could not parse starting_year/last_year from header."))
  }
  if (is.null(hdr$site_file) || !nzchar(hdr$site_file)) {
    return(list(hdr = NULL, header_end_idx = header_end_idx,
                err = "Could not parse site file name from header."))
  }

  list(hdr = hdr, header_end_idx = header_end_idx, err = NULL)
}

.collect_extra_header_lines <- function(lines, header_end_idx, marker_idx) {
  extra <- list()
  if (header_end_idx + 1 > marker_idx - 1) return(extra)
  for (idx in (header_end_idx + 1):(marker_idx - 1)) {
    line <- lines[idx]
    if (nzchar(trimws(line))) {
      extra[[length(extra) + 1]] <- list(lineno = idx, line = line)
    }
  }
  extra
}

.parse_block_id_line <- function(line) {
  parts <- strsplit(trimws(line), "\\s+")[[1]]
  if (length(parts) < 2) {
    return(list(block_id = NA_integer_, block_comment = "", err = "Block header line has too few tokens."))
  }
  block_id <- suppressWarnings(as.integer(parts[1]))
  if (is.na(block_id)) {
    return(list(block_id = NA_integer_, block_comment = "", err = "Block ID is not an integer."))
  }
  block_pos <- NA_integer_
  for (idx in 2:length(parts)) {
    if (tolower(parts[idx]) == "block") {
      block_pos <- idx
      break
    }
  }
  if (is.na(block_pos)) {
    return(list(block_id = NA_integer_, block_comment = "", err = "Block header line does not contain token 'Block'."))
  }
  comment_tokens <- if (block_pos < length(parts)) parts[(block_pos + 1):length(parts)] else character(0)
  comment <- trimws(paste(comment_tokens, collapse = " "))
  list(block_id = block_id, block_comment = comment, err = NULL)
}

.parse_event_line <- function(line, lineno) {
  parts <- strsplit(trimws(line), "\\s+")[[1]]
  if (length(parts) < 3) {
    return(list(event = NULL, err = "Event line must have at least 3 tokens: year doy EVENT."))
  }
  yr <- suppressWarnings(as.integer(parts[1]))
  if (is.na(yr)) {
    return(list(event = NULL, err = "First token (year_in_pattern) is not an integer."))
  }
  doy <- suppressWarnings(as.integer(parts[2]))
  if (is.na(doy)) {
    return(list(event = NULL, err = "Second token (day_of_year) is not an integer."))
  }
  event <- parts[3]
  arg <- if (length(parts) > 3) trimws(paste(parts[4:length(parts)], collapse = " ")) else ""
  list(event = list(year = yr, doy = doy, event = event, arg = arg, lineno = lineno), err = NULL)
}

.is_block_terminator <- function(s) {
  parts <- strsplit(trimws(s), "\\s+")[[1]]
  if (length(parts) < 3) return(FALSE)
  parts[1] == "-999" && parts[2] == "-999" && toupper(parts[3]) == "X"
}

# Parses 0+ schedule blocks starting at start_idx (the line after the
# "Year Month Option" marker). Mirrors read_sch.py::_parse_blocks exactly,
# using R's native 1-based indices as line numbers directly.
.parse_blocks <- function(lines, start_idx) {
  blocks <- list()
  i <- start_idx
  n <- length(lines)

  while (i <= n && !nzchar(trimws(lines[i]))) i <- i + 1

  while (i <= n) {
    if (!nzchar(trimws(lines[i]))) {
      i <- i + 1
      next
    }

    if (i + 6 > n) {
      return(list(blocks = blocks, err = sprintf(
        "Unexpected end-of-file while reading block header near line %d.", i)))
    }

    block_lineno <- i
    parsed_id <- .parse_block_id_line(lines[i])
    if (!is.null(parsed_id$err)) {
      return(list(blocks = blocks, err = sprintf(
        "Failed to parse block header at line %d: %s", block_lineno, parsed_id$err)))
    }

    last_year <- .to_int(.first_token(lines[i + 1]), default = NA_integer_)
    if (is.na(last_year)) {
      return(list(blocks = blocks, err = sprintf(
        "Failed to parse block last_year at line %d: '%s'", i + 1, lines[i + 1])))
    }

    repeats <- .to_int(.first_token(lines[i + 2]), default = NA_integer_)
    if (is.na(repeats)) {
      return(list(blocks = blocks, err = sprintf(
        "Failed to parse block repeats_years at line %d: '%s'", i + 2, lines[i + 2])))
    }

    out_start <- .to_int(.first_token(lines[i + 3]), default = NA_integer_)
    if (is.na(out_start)) {
      return(list(blocks = blocks, err = sprintf(
        "Failed to parse output_start_year at line %d: '%s'", i + 3, lines[i + 3])))
    }

    out_month <- .to_int(.first_token(lines[i + 4]), default = NA_integer_)
    if (is.na(out_month)) {
      return(list(blocks = blocks, err = sprintf(
        "Failed to parse output_month at line %d: '%s'", i + 4, lines[i + 4])))
    }

    out_interval <- .to_float(.first_token(lines[i + 5]), default = NA_real_)
    if (is.na(out_interval)) {
      return(list(blocks = blocks, err = sprintf(
        "Failed to parse output_interval at line %d: '%s'", i + 5, lines[i + 5])))
    }

    wchoice <- toupper(trimws(.first_token(lines[i + 6])))
    if (!nzchar(wchoice)) {
      return(list(blocks = blocks, err = sprintf(
        "Failed to parse weather_choice at line %d: '%s'", i + 6, lines[i + 6])))
    }

    weather_file <- NA_character_
    lineno_weather_file <- NA_integer_
    j <- i + 7

    if (wchoice == "F") {
      if (j > n) {
        return(list(blocks = blocks, err = sprintf(
          "Expected weather file name after weather choice 'F' at line %d, but hit EOF.", i + 6)))
      }
      weather_file <- trimws(lines[j])
      lineno_weather_file <- j
      if (!nzchar(weather_file)) {
        return(list(blocks = blocks, err = sprintf(
          "Weather choice is 'F' but weather filename line is blank at line %d.", lineno_weather_file)))
      }
      j <- j + 1
    } else if (wchoice == "C") {
      # Constant weather; no filename line expected.
    } else {
      return(list(blocks = blocks, err = sprintf(
        "Unknown weather choice '%s' at line %d (expected 'F' or 'C').", wchoice, i + 6)))
    }

    bh <- list(
      block = parsed_id$block_id,
      block_comment = parsed_id$block_comment,
      last_year = last_year,
      repeats_years = repeats,
      output_start_year = out_start,
      output_month = out_month,
      output_interval = out_interval,
      weather_choice = wchoice,
      weather_file = weather_file,
      lineno_block = block_lineno,
      lineno_last_year = i + 1,
      lineno_repeats = i + 2,
      lineno_out_start = i + 3,
      lineno_out_month = i + 4,
      lineno_out_interval = i + 5,
      lineno_weather_choice = i + 6,
      lineno_weather_file = lineno_weather_file
    )

    events <- list()
    while (j <= n) {
      line <- lines[j]
      lineno <- j
      s <- trimws(line)
      if (!nzchar(s)) {
        j <- j + 1
        next
      }
      if (.is_block_terminator(s)) {
        j <- j + 1
        break
      }
      ev <- .parse_event_line(line, lineno)
      if (!is.null(ev$err)) {
        return(list(blocks = blocks, err = sprintf(
          "Failed to parse event line at %d: %s (line='%s')", lineno, ev$err, line)))
      }
      events[[length(events) + 1]] <- ev$event
      j <- j + 1
    }

    blocks[[length(blocks) + 1]] <- list(header = bh, events = events)

    i <- j
    while (i <= n && !nzchar(trimws(lines[i]))) i <- i + 1
  }

  list(blocks = blocks, err = NULL)
}

.sch_empty_site_table <- function() {
  df <- data.frame(
    start_year = NA_real_, end_year = NA_real_, site_file = NA_character_,
    labeling_type = NA_real_, labeling_year = NA_real_, microcosm = NA_real_,
    co2_systems = NA_real_, ph_effect = NA_real_, soil_warming = NA_real_,
    n_input_scalar_option = NA_real_, omad_scalar_option = NA_real_,
    climate_scalar_option = NA_real_, initial_system = NA_real_,
    initial_crop = NA_character_, initial_tree = NA_character_,
    stringsAsFactors = FALSE
  )
  df$extra_header_lines <- list(character(0))
  df
}

.sch_empty_block_table <- function() {
  data.frame(
    block = numeric(0), block_comment = character(0), last_year = numeric(0),
    repeats_years = numeric(0), output_start_year = numeric(0),
    output_month = numeric(0), output_interval = numeric(0),
    weather_choice = character(0), weather_file = character(0),
    stringsAsFactors = FALSE
  )
}

.sch_empty_event_table <- function() {
  data.frame(
    block = numeric(0), year = numeric(0), doy = numeric(0),
    event = character(0), arg = character(0),
    stringsAsFactors = FALSE
  )
}

.value_or <- function(x, default) if (is.null(x)) default else x

.header_to_site_table <- function(hdr, extra_lines = list()) {
  df <- data.frame(
    start_year = .value_or(hdr$start_year, NA_real_),
    end_year = .value_or(hdr$end_year, NA_real_),
    site_file = .value_or(hdr$site_file, NA_character_),
    labeling_type = .value_or(hdr$labeling_type, NA_real_),
    labeling_year = .value_or(hdr$labeling_year, NA_real_),
    microcosm = .value_or(hdr$microcosm, NA_real_),
    co2_systems = .value_or(hdr$co2_systems, NA_real_),
    ph_effect = .value_or(hdr$ph_effect, NA_real_),
    soil_warming = .value_or(hdr$soil_warming, NA_real_),
    n_input_scalar_option = .value_or(hdr$n_input_scalar_option, NA_real_),
    omad_scalar_option = .value_or(hdr$omad_scalar_option, NA_real_),
    climate_scalar_option = .value_or(hdr$climate_scalar_option, NA_real_),
    initial_system = .value_or(hdr$initial_system, NA_real_),
    initial_crop = .value_or(hdr$initial_crop, NA_character_),
    initial_tree = .value_or(hdr$initial_tree, NA_character_),
    stringsAsFactors = FALSE
  )

  df$extra_header_lines <- list(vapply(extra_lines, function(x) x$line, character(1)))

  payload_keys <- c(
    "co2_systems_start_year", "co2_systems_end_year", "ph_effect_start_year",
    "soil_warming_start_year", "soil_warming_delta", "n_input_scalar_start_year",
    "omad_scalar_start_year", "climate_scalar_start_year"
  )
  for (k in payload_keys) {
    if (!is.null(hdr[[k]])) df[[k]] <- hdr[[k]]
  }

  df
}

.blocks_to_tables <- function(blocks) {
  if (length(blocks) == 0) {
    return(list(block_table = .sch_empty_block_table(), event_table = .sch_empty_event_table()))
  }

  block_rows <- lapply(blocks, function(b) {
    h <- b$header
    data.frame(
      block = h$block, block_comment = h$block_comment, last_year = h$last_year,
      repeats_years = h$repeats_years, output_start_year = h$output_start_year,
      output_month = h$output_month, output_interval = h$output_interval,
      weather_choice = h$weather_choice, weather_file = h$weather_file,
      lineno_block = h$lineno_block, lineno_last_year = h$lineno_last_year,
      lineno_repeats = h$lineno_repeats, lineno_out_start = h$lineno_out_start,
      lineno_out_month = h$lineno_out_month, lineno_out_interval = h$lineno_out_interval,
      lineno_weather_choice = h$lineno_weather_choice, lineno_weather_file = h$lineno_weather_file,
      stringsAsFactors = FALSE
    )
  })
  block_table <- do.call(rbind, block_rows)
  rownames(block_table) <- NULL

  event_rows <- list()
  for (b in blocks) {
    for (ev in b$events) {
      event_rows[[length(event_rows) + 1]] <- data.frame(
        block = b$header$block, year = ev$year, doy = ev$doy,
        event = ev$event, arg = ev$arg, lineno = ev$lineno,
        stringsAsFactors = FALSE
      )
    }
  }
  event_table <- if (length(event_rows) > 0) do.call(rbind, event_rows) else .sch_empty_event_table()
  rownames(event_table) <- NULL

  list(block_table = block_table, event_table = event_table)
}

.sch_parse_failure <- function(path, parse_error, hdr = NULL, extra_lines = list(), marker_lineno = NA_integer_) {
  site_table <- if (is.null(hdr)) .sch_empty_site_table() else .header_to_site_table(hdr, extra_lines)

  new_sch(
    site_table, .sch_empty_block_table(), .sch_empty_event_table(),
    extra = list(
      path = path,
      parsed_ok = FALSE,
      parse_error = parse_error,
      marker_lineno = marker_lineno
    )
  )
}

.sch_from_parsed <- function(path, hdr, extra_lines, marker_idx, blocks) {
  site_table <- .header_to_site_table(hdr, extra_lines)
  tabs <- .blocks_to_tables(blocks)

  new_sch(
    site_table, tabs$block_table, tabs$event_table,
    extra = list(
      path = path,
      parsed_ok = TRUE,
      parse_error = NA_character_,
      marker_lineno = marker_idx,
      extra_header_linenos = if (length(extra_lines) > 0) {
        vapply(extra_lines, function(x) x$lineno, integer(1))
      } else {
        integer(0)
      }
    )
  )
}

#' @title Read a DayCent schedule (.sch) file
#'
#' @description
#' Parses a DayCent schedule file into an [new_sch()] object: the 15-field
#' labeled header (plus any option-payload lines and extra header lines),
#' and one or more schedule blocks with their events. Header fields are
#' located by label text, not fixed line offsets, so the parser tolerates
#' the optional payload lines DayCent inserts when CO2 / pH / soil-warming /
#' scalar options are enabled.
#'
#' This is a structural reader only. It does not check event vocabulary,
#' library references, or day-of-year plausibility — see the schedule
#' validator for that.
#'
#' @param path Path to a `.sch` file.
#' @param encoding Character. Text encoding to read the file with.
#'   Defaults to `"UTF-8"`.
#'
#' @return An `sch` object. On success, `sch$extra$parsed_ok` is `TRUE`. On
#'   any parse failure (missing marker, malformed header, malformed block),
#'   `sch$extra$parsed_ok` is `FALSE` and `sch$extra$parse_error` describes
#'   the failure; no error is thrown.
#'
#' @export
read_sch <- function(path, encoding = "UTF-8") {
  lines <- tryCatch(
    readr::read_lines(path, locale = readr::locale(encoding = encoding)),
    error = function(e) NULL
  )
  if (is.null(lines)) {
    return(.sch_parse_failure(path, sprintf("Could not read file '%s'.", path)))
  }

  marker_idx <- .find_marker_line(lines, "Year Month Option")
  if (is.na(marker_idx)) {
    return(.sch_parse_failure(path, "Missing required marker line 'Year Month Option'."))
  }

  parsed_header <- .parse_sch_header(lines, marker_idx)
  if (!is.null(parsed_header$err)) {
    return(.sch_parse_failure(path, parsed_header$err))
  }

  hdr <- parsed_header$hdr
  extra_lines <- .collect_extra_header_lines(lines, parsed_header$header_end_idx, marker_idx)

  parsed_blocks <- .parse_blocks(lines, marker_idx + 1)
  if (!is.null(parsed_blocks$err)) {
    return(.sch_parse_failure(
      path, parsed_blocks$err,
      hdr = hdr, extra_lines = extra_lines, marker_lineno = marker_idx
    ))
  }

  .sch_from_parsed(path, hdr, extra_lines, marker_idx, parsed_blocks$blocks)
}
