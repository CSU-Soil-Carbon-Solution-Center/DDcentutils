.load_daycent_rules_df <- function() {
  path <- system.file("config", "daycent", "rules.v1.json", package = "DDcentutils")
  jsonlite::fromJSON(path)$rules
}

.empty_findings <- function() {
  df <- data.frame(
    code = character(0), severity = character(0), file = character(0),
    line = integer(0), message = character(0), stringsAsFactors = FALSE
  )
  df$context <- list()
  df
}

.finalize_findings <- function(findings_list) {
  if (length(findings_list) == 0) return(.empty_findings())
  do.call(rbind, findings_list)
}

# Emits a finding whose severity is looked up from the vendored
# rules.v1.json -- mirrors the platform's qc_rules.emit(). Errors on an
# unrecognized code: every code this validator emits from the vendored set
# must actually be vendored.
.emit_finding <- function(rules_df, code, file, message = NULL, line = NA_integer_, context = list()) {
  row <- rules_df[rules_df$code == code, , drop = FALSE]
  if (nrow(row) == 0) {
    stop(
      sprintf("validate_sch(): unknown rule code '%s' (not in vendored rules.v1.json)", code),
      call. = FALSE
    )
  }
  out <- data.frame(
    code = code,
    severity = row$default_severity[1],
    file = if (is.null(file) || length(file) == 0 || is.na(file)) NA_character_ else as.character(file),
    line = if (is.null(line) || length(line) == 0 || is.na(line)) NA_integer_ else as.integer(line),
    message = if (is.null(message)) row$detail[1] else message,
    stringsAsFactors = FALSE
  )
  out$context <- list(context)
  out
}

# A DDcentutils-local informational finding, not part of the vendored
# preflight rule set (that set is fixed at "fix in preflight first, then
# re-vendor" -- see documentation/local/preflight_sync.md). Used only for
# the "library checks were skipped" notice.
.emit_local_info <- function(code, file, message, context = list()) {
  out <- data.frame(
    code = code, severity = "INFO",
    file = if (is.null(file) || is.na(file)) NA_character_ else as.character(file),
    line = NA_integer_, message = message, stringsAsFactors = FALSE
  )
  out$context <- list(context)
  out
}

.is_parenthesized <- function(value) {
  value <- trimws(value)
  startsWith(value, "(") && endsWith(value, ")")
}

# Port of fert_immediate_named_options() (schedule_qc.py:219). Direct keyed
# immediate values (e.g. "3.4N") are skipped; a token beginning with a
# letter is treated as a named fert.100 option needing a library lookup.
.fert_immediate_named_options <- function(argument) {
  payload <- trimws(argument)
  payload <- substr(payload, 2, nchar(payload) - 1)
  tokens <- strsplit(payload, "[\\s,]+", perl = TRUE)[[1]]
  tokens <- tokens[nzchar(tokens)]

  direct_value_re <- "^[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?[NPSTFI]$"

  out <- character(0)
  for (token in tokens) {
    if (grepl(direct_value_re, token, ignore.case = TRUE, perl = TRUE)) next
    if (grepl("^[A-Za-z]", token, perl = TRUE)) out <- c(out, token)
  }
  out
}

# Port of lookup_ids_for_event() (schedule_qc.py:203) exactly: empty arg ->
# no lookup; FERT with a parenthesized arg -> only its named options; IRRI
# / IRIG with a parenthesized arg -> no lookup at all; otherwise -> the
# first whitespace-delimited token. Matching is case-exact throughout.
.lookup_ids_for_event <- function(event, argument) {
  arg <- trimws(if (is.null(argument) || length(argument) == 0 || is.na(argument)) "" else argument)
  if (!nzchar(arg)) return(character(0))
  if (event == "FERT" && .is_parenthesized(arg)) {
    return(.fert_immediate_named_options(arg))
  }
  if (event %in% c("IRRI", "IRIG") && .is_parenthesized(arg)) {
    return(character(0))
  }
  strsplit(arg, "\\s+")[[1]][1]
}

# Port of check_pairings() (schedule_qc.py:232), order-aware, global across
# the whole schedule (not reset per block). Returns a list of error records
# (line/event/reason); the caller emits at most one SCH_EVENT_PAIRING_INVALID
# finding for the whole schedule.
.check_pairings <- function(pair_events) {
  open_crop <- NULL
  open_tree <- NULL
  errors <- list()

  for (pe in pair_events) {
    event <- pe$event
    lineno <- pe$lineno

    if (event %in% c("FRST", "PLTM")) {
      if (!is.null(open_crop)) {
        errors[[length(errors) + 1]] <- list(
          line = lineno, event = event,
          reason = sprintf("%s starts before prior %s is closed by LAST", event, open_crop$event)
        )
      }
      open_crop <- list(event = event, lineno = lineno)
    } else if (event == "LAST") {
      if (is.null(open_crop)) {
        errors[[length(errors) + 1]] <- list(line = lineno, event = event, reason = "LAST without open FRST or PLTM")
      } else {
        open_crop <- NULL
      }
    } else if (event == "TFST") {
      if (!is.null(open_tree)) {
        errors[[length(errors) + 1]] <- list(
          line = lineno, event = event,
          reason = "TFST starts before prior TFST is closed by TLST"
        )
      }
      open_tree <- list(event = event, lineno = lineno)
    } else if (event == "TLST") {
      if (is.null(open_tree)) {
        errors[[length(errors) + 1]] <- list(line = lineno, event = event, reason = "TLST without open TFST")
      } else {
        open_tree <- NULL
      }
    }
  }

  if (!is.null(open_crop)) {
    errors[[length(errors) + 1]] <- list(
      line = open_crop$lineno, event = open_crop$event,
      reason = sprintf("%s is not closed by LAST", open_crop$event)
    )
  }
  if (!is.null(open_tree)) {
    errors[[length(errors) + 1]] <- list(line = open_tree$lineno, event = "TFST", reason = "TFST is not closed by TLST")
  }

  errors
}

.check_schedule_events <- function(sch, rules_df, events_cfg, libraries, file_path) {
  findings <- list()

  event_to_library <- as.list(events_cfg$event_to_library)
  no_lookup <- events_cfg$no_lookup_events
  known_events <- union(names(event_to_library), no_lookup)
  doy_min <- events_cfg$event_day_of_year$minimum
  doy_stop <- events_cfg$event_day_of_year$stop_at_or_above

  et <- sch$event_table
  pair_events <- list()

  if (nrow(et) > 0) {
    for (i in seq_len(nrow(et))) {
      event_raw <- et$event[i]
      event_upper <- toupper(event_raw)
      arg <- et$arg[i]
      doy <- et$doy[i]
      lineno <- if ("lineno" %in% names(et)) et$lineno[i] else NA_integer_

      if (!(event_upper %in% known_events)) {
        findings[[length(findings) + 1]] <- .emit_finding(
          rules_df, "SCH_UNKNOWN_EVENT", file = file_path, line = lineno,
          message = sprintf("Unknown schedule event '%s'.", event_raw),
          context = list(event = event_raw)
        )
        next
      }

      if (doy < doy_min || doy >= doy_stop) {
        findings[[length(findings) + 1]] <- .emit_finding(
          rules_df, "SCH_EVENT_DOY_INVALID", file = file_path, line = lineno,
          message = sprintf("Invalid day-of-year %s for event %s.", doy, event_upper),
          context = list(event = event_upper, day_of_year = doy)
        )
      }

      if (event_upper %in% c("FRST", "PLTM", "TFST", "LAST", "TLST")) {
        pair_events[[length(pair_events) + 1]] <- list(event = event_upper, lineno = lineno)
      }

      lib_name <- event_to_library[[event_upper]]
      if (!is.null(lib_name) && !is.null(libraries)) {
        lookup_ids <- .lookup_ids_for_event(event_upper, arg)
        if (length(lookup_ids) > 0) {
          lib <- libraries[[lib_name]]
          if (is.null(lib)) {
            findings[[length(findings) + 1]] <- .emit_finding(
              rules_df, "LIB100_REQUIRED_FILE_MISSING", file = lib_name,
              message = sprintf(
                "Schedule event %s needs %s, but it is missing from the effective library set.",
                event_upper, lib_name
              ),
              context = list(event = event_upper, library = lib_name)
            )
          } else {
            for (lookup_id in lookup_ids) {
              if (!(lookup_id %in% lib$block_ids)) {
                findings[[length(findings) + 1]] <- .emit_finding(
                  rules_df, "SCH_EVENT_REF_NOT_IN_LIBRARY", file = file_path, line = lineno,
                  message = sprintf(
                    "Schedule event %s references '%s', which is not in %s.",
                    event_upper, lookup_id, lib_name
                  ),
                  context = list(event = event_upper, argument = lookup_id, library = lib_name)
                )
              }
            }
          }
        }
      }
    }
  }

  pairing_errors <- .check_pairings(pair_events)
  if (length(pairing_errors) > 0) {
    first_line <- pairing_errors[[1]]$line
    findings[[length(findings) + 1]] <- .emit_finding(
      rules_df, "SCH_EVENT_PAIRING_INVALID", file = file_path, line = first_line,
      message = "Crop/tree start and end events are not explicitly paired in order.",
      context = list(errors = pairing_errors[seq_len(min(10, length(pairing_errors)))])
    )
  }

  findings
}

.check_schedule_weather_refs <- function(sch, site_dir, rules_df, file_path) {
  findings <- list()
  bt <- sch$block_table
  if (nrow(bt) == 0) return(findings)

  site_dir_norm <- normalizePath(site_dir, winslash = "/", mustWork = FALSE)

  for (i in seq_len(nrow(bt))) {
    weather_file <- bt$weather_file[i]
    if (is.na(weather_file) || !nzchar(weather_file)) next

    lineno <- if ("lineno_weather_file" %in% names(bt)) bt$lineno_weather_file[i] else NA_integer_
    normalized <- gsub("\\\\", "/", weather_file)
    is_abs <- grepl("^(/|[A-Za-z]:)", normalized)
    has_dotdot <- ".." %in% strsplit(normalized, "/", fixed = TRUE)[[1]]

    if (is_abs || has_dotdot) {
      findings[[length(findings) + 1]] <- .emit_finding(
        rules_df, "SCH_WEATHER_REF_OUTSIDE_SITE_DIR", file = file_path, line = lineno,
        message = sprintf("Weather reference '%s' must stay inside the site directory.", weather_file),
        context = list(weather_file = weather_file)
      )
      next
    }

    resolved <- file.path(site_dir, normalized)
    resolved_norm <- normalizePath(resolved, winslash = "/", mustWork = FALSE)

    if (!startsWith(resolved_norm, site_dir_norm)) {
      findings[[length(findings) + 1]] <- .emit_finding(
        rules_df, "SCH_WEATHER_REF_OUTSIDE_SITE_DIR", file = file_path, line = lineno,
        message = sprintf("Weather reference '%s' resolves outside the site directory.", weather_file),
        context = list(weather_file = weather_file)
      )
      next
    }

    if (!file.exists(resolved)) {
      findings[[length(findings) + 1]] <- .emit_finding(
        rules_df, "SCH_WEATHER_REF_MISSING", file = resolved,
        message = sprintf("Referenced weather file '%s' does not exist in the site directory.", weather_file),
        context = list(schedule = file_path, weather_file = weather_file)
      )
    }
  }

  findings
}

#' @title Validate a DayCent schedule
#'
#' @description
#' Structural and semantic checks over an `sch` object, ported from the
#' EMDC preflight platform's `schfile_checks.py` and `schedule_qc.py`.
#' Accumulates every finding rather than stopping at the first -- that
#' matches the platform design and is the point of the exercise.
#'
#' Checks performed:
#' \itemize{
#'   \item structural: an explicit parse failure (`SCH_PARSE_ERROR`, which
#'     returns early -- nothing else is reliable), an empty/unparseable
#'     header (`SCH_HEADER_INVALID`), `end_year < start_year`
#'     (`SCH_RUN_LENGTH_DERIVATION_FAIL`)
#'   \item events: unknown event vocabulary (`SCH_UNKNOWN_EVENT`), invalid
#'     day-of-year (`SCH_EVENT_DOY_INVALID`; `366` is legal, `367+` and `<1`
#'     are not), unpaired crop/tree start-end events
#'     (`SCH_EVENT_PAIRING_INVALID`)
#'   \item libraries (only when `library_dir` is supplied): a resolved
#'     event token absent from its library (`SCH_EVENT_REF_NOT_IN_LIBRARY`),
#'     a library file an event needs that is not present
#'     (`LIB100_REQUIRED_FILE_MISSING`). Immediate-input arguments (e.g.
#'     `FERT (3.4N)`) are resolved per `lookup_ids_for_event()` before
#'     lookup; matching is exact, never case-folded.
#'   \item weather references (only when `site_dir` is supplied): a
#'     reference that is absolute, contains `..`, or resolves outside
#'     `site_dir` (`SCH_WEATHER_REF_OUTSIDE_SITE_DIR`), or does not exist
#'     (`SCH_WEATHER_REF_MISSING`)
#' }
#'
#' An `sch` with no `parsed_ok` metadata at all (e.g. one built with
#' [build_sch()] rather than read from a file) is treated as not-yet-parsed
#' rather than failed -- the parse-failure check only fires when
#' `parsed_ok` is explicitly `FALSE`.
#'
#' When `library_dir` is `NULL`, all library checks are skipped and one
#' `INFO` finding notes it, so the validator stays useful without a
#' licensed DayCent library set.
#'
#' @param sch An `sch` object (or an object coercible via [as_sch()]).
#' @param library_dir Optional path to a directory of `.100` library files
#'   (as read by [read_library_dir()]). Library checks are skipped when
#'   `NULL`.
#' @param site_dir Optional path to the site directory weather references
#'   are expected to resolve within. Weather-reference checks are skipped
#'   when `NULL`.
#' @param policy One of `"standard"`, `"strict_mrv"`, `"exploratory"` --
#'   accepted for forward compatibility with the platform's policy concept.
#'   None of the currently vendored rules define a policy override, so this
#'   has no effect yet.
#'
#' @return A data.frame with columns `code`, `severity`, `file`, `line`,
#'   `message`, `context` (a list-column), one row per finding.
#'
#' @export
validate_sch <- function(sch, library_dir = NULL, site_dir = NULL, policy = "standard") {
  policy <- match.arg(policy, c("standard", "strict_mrv", "exploratory"))

  sch <- as_sch(sch)
  rules_df <- .load_daycent_rules_df()
  findings <- list()

  file_path <- sch$extra$path
  if (is.null(file_path)) file_path <- NA_character_

  parsed_ok <- sch$extra$parsed_ok
  if (!is.null(parsed_ok) && !isTRUE(parsed_ok)) {
    findings[[length(findings) + 1]] <- .emit_finding(
      rules_df, "SCH_PARSE_ERROR", file = file_path,
      message = if (is.null(sch$extra$parse_error)) {
        "Schedule file could not be parsed."
      } else {
        sch$extra$parse_error
      }
    )
    return(.finalize_findings(findings))
  }

  st <- sch$site_table
  header_invalid <- nrow(st) == 0 ||
    is.na(st$start_year[1]) || is.na(st$end_year[1]) ||
    is.na(st$site_file[1]) || !nzchar(trimws(st$site_file[1]))
  if (header_invalid) {
    findings[[length(findings) + 1]] <- .emit_finding(
      rules_df, "SCH_HEADER_INVALID", file = file_path,
      message = "Schedule header missing or could not be parsed into key/value fields."
    )
  }

  if (nrow(st) > 0 && !is.na(st$start_year[1]) && !is.na(st$end_year[1]) &&
    st$end_year[1] < st$start_year[1]) {
    findings[[length(findings) + 1]] <- .emit_finding(
      rules_df, "SCH_RUN_LENGTH_DERIVATION_FAIL", file = file_path,
      message = sprintf(
        "Derived end_year < start_year (%s < %s).", st$end_year[1], st$start_year[1]
      ),
      context = list(start_year = st$start_year[1], end_year = st$end_year[1])
    )
  }

  events_cfg <- .load_schedule_events_config()
  libraries <- NULL
  if (is.null(library_dir)) {
    findings[[length(findings) + 1]] <- .emit_local_info(
      "SCH_LIBRARY_CHECKS_SKIPPED", file = file_path,
      message = "library_dir was not supplied; library-reference checks were skipped."
    )
  } else {
    libraries <- read_library_dir(library_dir)
  }

  findings <- c(findings, .check_schedule_events(sch, rules_df, events_cfg, libraries, file_path))

  if (!is.null(site_dir)) {
    findings <- c(findings, .check_schedule_weather_refs(sch, site_dir, rules_df, file_path))
  }

  .finalize_findings(findings)
}

#' @title Highest severity among a set of findings
#'
#' @description
#' Used by the CLI ([sch_cli()]) to decide the process exit code.
#'
#' @param findings A findings data.frame as returned by [validate_sch()].
#'
#' @return One of `"ERROR"`, `"WARN"`, `"INFO"`, or `"NONE"` (no findings).
#'
#' @export
severity_max <- function(findings) {
  if (is.null(findings) || nrow(findings) == 0) return("NONE")
  sev <- findings$severity
  if ("ERROR" %in% sev) return("ERROR")
  if ("WARN" %in% sev) return("WARN")
  if ("INFO" %in% sev) return("INFO")
  "NONE"
}

#' @title Format findings as greppable strings
#'
#' @description
#' Formats one or more findings using the platform's blocking-finding
#' string format (`modeling/preflight/input_qc/README.md`):
#' `"<file>[:<line>]: <CODE>: <message>"`, so DDcentutils output is
#' greppable alongside preflight output.
#'
#' @param finding A findings data.frame (as returned by [validate_sch()]),
#'   one or more rows.
#'
#' @return A character vector, one formatted string per row.
#'
#' @export
format_finding <- function(finding) {
  vapply(seq_len(nrow(finding)), function(i) {
    location <- finding$file[i]
    if (is.na(location) || !nzchar(location)) location <- "(no file)"
    line <- finding$line[i]
    if (!is.na(line)) location <- sprintf("%s:%d", location, as.integer(line))
    sprintf("%s: %s: %s", location, finding$code[i], finding$message[i])
  }, character(1))
}
