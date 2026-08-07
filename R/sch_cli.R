.sch_cli_usage <- function() {
  cat("
Usage:
  Rscript inst/scripts/ddcent-sch.R build \\
      --site-table <site.csv> --event-table <events.csv> \\
      [--block-table <blocks.csv>] --out <schedule.sch>

  Rscript inst/scripts/ddcent-sch.R validate <schedule.sch> \\
      [--library-dir <dir>] [--site-dir <dir>] [--json]

  Rscript inst/scripts/ddcent-sch.R inspect <schedule.sch> --out-dir <dir>

  -h, --help    Print this usage message and exit 0

Also callable as: Rscript -e 'DDcentutils::sch_cli()' --args <subcommand> ...

Exit codes: 0 clean (or WARN/INFO only), 1 any ERROR finding, 2 usage error.
")
}

# Splits argv into positional arguments and --flag/--flag value pairs.
# --json is the only boolean (valueless) flag; every other --flag consumes
# the following token as its value.
.parse_cli_flags <- function(args) {
  positional <- character(0)
  flags <- list()
  boolean_flags <- "json"

  i <- 1
  n <- length(args)
  while (i <= n) {
    a <- args[i]
    if (startsWith(a, "--")) {
      key <- substring(a, 3)
      if (key %in% boolean_flags) {
        flags[[key]] <- TRUE
        i <- i + 1
      } else {
        if (i == n) stop(sprintf("Flag '--%s' requires a value.", key), call. = FALSE)
        flags[[key]] <- args[i + 1]
        i <- i + 2
      }
    } else {
      positional <- c(positional, a)
      i <- i + 1
    }
  }

  list(positional = positional, flags = flags)
}

.read_sch_table_csv <- function(path, which) {
  as.data.frame(readr::read_csv(
    path, na = "NA", col_types = .sch_csv_col_types[[which]], progress = FALSE
  ))
}

.sch_cli_build <- function(opts) {
  site_table_path <- opts$flags[["site-table"]]
  event_table_path <- opts$flags[["event-table"]]
  block_table_path <- opts$flags[["block-table"]]
  out_path <- opts$flags[["out"]]

  if (is.null(site_table_path) || is.null(event_table_path) || is.null(out_path)) {
    cat("build requires --site-table, --event-table, and --out.\n\n")
    .sch_cli_usage()
    return(2L)
  }

  site_table <- .site_table_from_csv_df(.read_sch_table_csv(site_table_path, "site"))
  event_table <- .read_sch_table_csv(event_table_path, "event")
  block_table <- if (!is.null(block_table_path)) {
    .read_sch_table_csv(block_table_path, "block")
  } else {
    NULL
  }

  s <- build_sch(site_table, event_table, block_table)
  write_sch(s, path = out_path)
  cat(sprintf("Wrote %s\n", out_path))
  0L
}

.sch_cli_validate <- function(opts) {
  if (length(opts$positional) < 1) {
    cat("validate requires a <schedule.sch> path.\n\n")
    .sch_cli_usage()
    return(2L)
  }

  s <- read_sch(opts$positional[1])
  findings <- validate_sch(
    s,
    library_dir = opts$flags[["library-dir"]],
    site_dir = opts$flags[["site-dir"]]
  )

  if (isTRUE(opts$flags[["json"]])) {
    cat(jsonlite::toJSON(findings, auto_unbox = TRUE, na = "null"), "\n")
  } else if (nrow(findings) == 0) {
    cat("No findings.\n")
  } else {
    cat(format_finding(findings), sep = "\n")
  }

  if (identical(severity_max(findings), "ERROR")) return(1L)
  0L
}

.sch_cli_inspect <- function(opts) {
  if (length(opts$positional) < 1) {
    cat("inspect requires a <schedule.sch> path.\n\n")
    .sch_cli_usage()
    return(2L)
  }
  out_dir <- opts$flags[["out-dir"]]
  if (is.null(out_dir)) {
    cat("inspect requires --out-dir.\n\n")
    .sch_cli_usage()
    return(2L)
  }

  s <- read_sch(opts$positional[1])
  write_sch_tables(s, out_dir)
  cat(sprintf("Wrote site.csv / blocks.csv / events.csv to %s\n", out_dir))
  0L
}

#' @title Command-line interface for building, validating, and inspecting schedules
#'
#' @description
#' Shell access to the schedule engine: `build` (tables -> `.sch`),
#' `validate` (`.sch` -> findings), and `inspect` (`.sch` -> tables). See
#' `inst/scripts/ddcent-sch.R` for the shell entry point, or call this
#' directly as `Rscript -e 'DDcentutils::sch_cli()' --args <subcommand>
#' ...`.
#'
#' `inspect` closes the loop with `build`: parse an existing schedule to
#' tables, edit them, rebuild.
#'
#' Execution (running DayCent itself) is out of scope here -- that stays
#' with [DayCentRunSite_single_run()] / [runDayCent()].
#'
#' @param argv Character vector of command-line arguments, usually
#'   `commandArgs(trailingOnly = TRUE)`.
#'
#' @return Integer exit code: `0` clean (or only `WARN`/`INFO` findings),
#'   `1` if `validate` found any `ERROR`, `2` on a usage error. `--help`
#'   and no arguments print usage and return `0`.
#'
#' @export
sch_cli <- function(argv = commandArgs(trailingOnly = TRUE)) {
  if (length(argv) == 0 || argv[1] %in% c("-h", "--help")) {
    .sch_cli_usage()
    return(0L)
  }

  subcommand <- argv[1]
  rest <- argv[-1]

  if (!subcommand %in% c("build", "validate", "inspect")) {
    cat(sprintf("Unknown subcommand '%s'.\n\n", subcommand))
    .sch_cli_usage()
    return(2L)
  }

  opts <- tryCatch(.parse_cli_flags(rest), error = function(e) {
    cat(sprintf("%s\n\n", conditionMessage(e)))
    NULL
  })
  if (is.null(opts)) {
    .sch_cli_usage()
    return(2L)
  }

  switch(subcommand,
    build = .sch_cli_build(opts),
    validate = .sch_cli_validate(opts),
    inspect = .sch_cli_inspect(opts)
  )
}
