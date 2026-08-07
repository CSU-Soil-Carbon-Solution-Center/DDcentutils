sch_fixture <- function(name) {
  system.file("extdata", "sch", name, package = "DDcentutils")
}
lib_dir <- function() {
  dirname(system.file("extdata", "lib100", "crop.100", package = "DDcentutils"))
}

test_that("bad_unknown_event.sch produces exactly one SCH_UNKNOWN_EVENT with the correct line", {
  path <- sch_fixture("bad_unknown_event.sch")
  s <- read_sch(path)
  findings <- validate_sch(s, library_dir = lib_dir())

  hits <- findings[findings$code == "SCH_UNKNOWN_EVENT", ]
  expect_equal(nrow(hits), 1)

  expected_line <- grep("ZZZZ", readLines(path))
  expect_equal(hits$line, expected_line)
  expect_equal(hits$line, s$event_table$lineno[s$event_table$event == "ZZZZ"])
})

test_that("bad_doy.sch produces exactly two SCH_EVENT_DOY_INVALID; doy=366 produces none", {
  path <- sch_fixture("bad_doy.sch")
  s <- read_sch(path)
  findings <- validate_sch(s, library_dir = lib_dir())

  hits <- findings[findings$code == "SCH_EVENT_DOY_INVALID", ]
  expect_equal(nrow(hits), 2)

  line_366 <- s$event_table$lineno[s$event_table$doy == 366]
  expect_false(line_366 %in% hits$line)
})

test_that("bad_pairing.sch produces exactly one SCH_EVENT_PAIRING_INVALID", {
  s <- read_sch(sch_fixture("bad_pairing.sch"))
  findings <- validate_sch(s, library_dir = lib_dir())

  hits <- findings[findings$code == "SCH_EVENT_PAIRING_INVALID", ]
  expect_equal(nrow(hits), 1)
})

test_that("a CROP referencing an unknown ID against crop.100 produces SCH_EVENT_REF_NOT_IN_LIBRARY", {
  site <- data.frame(
    start_year = 1980, end_year = 1989, site_file = "site.100",
    initial_crop = "W1", stringsAsFactors = FALSE
  )
  events <- data.frame(
    block = 1, year = 1, doy = 1, event = "CROP", arg = "ZZZ",
    stringsAsFactors = FALSE
  )
  s <- build_sch(site, events)
  findings <- validate_sch(s, library_dir = lib_dir())

  hits <- findings[findings$code == "SCH_EVENT_REF_NOT_IN_LIBRARY", ]
  expect_equal(nrow(hits), 1)
  expect_equal(hits$context[[1]]$argument, "ZZZ")
})

test_that("immediate FERT/IRIG arguments resolve per lookup_ids_for_event() and pass", {
  s <- read_sch(sch_fixture("immediate_args.sch"))
  findings <- validate_sch(s, library_dir = lib_dir())

  # FERT (3.4N): no named option -> no lookup, so no finding for it.
  # FERT (3.7N,0.75F,N15C): resolves N15C against fert.100 -- present, passes.
  # IRIG (3A 0.99F -1L): parenthesized -> no lookup at all.
  # HARV G75S: resolves against harv.100 -- present, passes.
  expect_equal(nrow(findings[findings$code == "SCH_EVENT_REF_NOT_IN_LIBRARY", ]), 0)
  expect_equal(nrow(findings[findings$code == "LIB100_REQUIRED_FILE_MISSING", ]), 0)
})

test_that("a clean fixture with library_dir = NULL has zero ERROR findings and one INFO", {
  s <- read_sch(sch_fixture("single_block.sch"))
  findings <- validate_sch(s, library_dir = NULL)

  expect_equal(nrow(findings[findings$severity == "ERROR", ]), 0)
  expect_equal(nrow(findings[findings$severity == "INFO", ]), 1)
  expect_equal(findings$code[findings$severity == "INFO"], "SCH_LIBRARY_CHECKS_SKIPPED")
})

test_that("no finding anywhere has severity STOP", {
  all_fixtures <- c(
    "single_block.sch", "three_block.sch", "eq_aceq.sch", "option_payload.sch",
    "immediate_args.sch", "bad_unknown_event.sch", "bad_doy.sch", "bad_pairing.sch"
  )
  for (f in all_fixtures) {
    findings <- validate_sch(read_sch(sch_fixture(f)), library_dir = lib_dir())
    expect_false("STOP" %in% findings$severity, info = f)
  }
})

test_that("clean fixtures (excluding the bad_* ones) produce zero ERROR findings", {
  clean_fixtures <- c("single_block.sch", "three_block.sch", "eq_aceq.sch", "option_payload.sch")
  for (f in clean_fixtures) {
    findings <- validate_sch(read_sch(sch_fixture(f)), library_dir = lib_dir())
    expect_equal(nrow(findings[findings$severity == "ERROR", ]), 0, info = f)
  }
})

test_that("a parse failure produces exactly one SCH_PARSE_ERROR and returns early", {
  tmp <- tempfile(fileext = ".sch")
  writeLines(c("no marker here"), tmp)
  on.exit(unlink(tmp), add = TRUE)

  s <- read_sch(tmp)
  findings <- validate_sch(s, library_dir = lib_dir())

  expect_equal(nrow(findings), 1)
  expect_equal(findings$code[1], "SCH_PARSE_ERROR")
  expect_equal(findings$severity[1], "ERROR")
})

test_that("an sch with no parsed_ok metadata (built via build_sch()) is not treated as a parse failure", {
  site <- data.frame(
    start_year = 1980, end_year = 1989, site_file = "site.100",
    initial_crop = "W1", stringsAsFactors = FALSE
  )
  events <- data.frame(
    block = 1, year = 1, doy = 1, event = "FRST", arg = "",
    stringsAsFactors = FALSE
  )
  s <- build_sch(site, events)
  expect_null(s$extra$parsed_ok)

  findings <- validate_sch(s, library_dir = NULL)
  expect_equal(nrow(findings[findings$code == "SCH_PARSE_ERROR", ]), 0)
})

test_that("end_year < start_year produces SCH_RUN_LENGTH_DERIVATION_FAIL", {
  site <- data.frame(
    start_year = 1990, end_year = 1980, site_file = "site.100",
    initial_crop = "W1", stringsAsFactors = FALSE
  )
  events <- data.frame(
    block = 1, year = 1, doy = 1, event = "FRST", arg = "",
    stringsAsFactors = FALSE
  )
  s <- build_sch(site, events, block_table = data.frame(
    block = 1, block_comment = "", last_year = 1980, repeats_years = 1,
    output_start_year = 1990, output_month = 12, output_interval = 1,
    weather_choice = "C", weather_file = NA_character_, stringsAsFactors = FALSE
  ))
  findings <- validate_sch(s, library_dir = NULL)

  expect_equal(nrow(findings[findings$code == "SCH_RUN_LENGTH_DERIVATION_FAIL", ]), 1)
})

test_that("severity_max() reports the highest severity present", {
  s <- read_sch(sch_fixture("bad_unknown_event.sch"))
  findings <- validate_sch(s, library_dir = lib_dir())
  expect_equal(severity_max(findings), "ERROR")

  clean <- validate_sch(read_sch(sch_fixture("single_block.sch")), library_dir = NULL)
  expect_equal(severity_max(clean), "INFO")

  no_findings <- validate_sch(read_sch(sch_fixture("single_block.sch")), library_dir = lib_dir())[0, ]
  expect_equal(severity_max(no_findings), "NONE")
})

test_that("format_finding() matches the platform's '<file>[:<line>]: <CODE>: <message>' format", {
  s <- read_sch(sch_fixture("bad_unknown_event.sch"))
  findings <- validate_sch(s, library_dir = lib_dir())
  hit <- findings[findings$code == "SCH_UNKNOWN_EVENT", ][1, ]

  formatted <- format_finding(hit)
  expect_match(formatted, paste0("^", hit$file, ":", hit$line, ": SCH_UNKNOWN_EVENT: "))

  # A finding with no line number omits the ":<line>" segment.
  info_hit <- validate_sch(read_sch(sch_fixture("single_block.sch")), library_dir = NULL)
  info_hit <- info_hit[info_hit$code == "SCH_LIBRARY_CHECKS_SKIPPED", ][1, ]
  formatted_info <- format_finding(info_hit)
  expect_false(grepl(":\\d+:", formatted_info))
})
