fixture <- function(name) {
  system.file("extdata", "sch", name, package = "DDcentutils")
}

test_that("single_block.sch parses to 1 block with correct header fields", {
  s <- read_sch(fixture("single_block.sch"))
  expect_true(s$extra$parsed_ok)
  expect_equal(nrow(s$block_table), 1)
  expect_equal(s$site_table$start_year, 1980)
  expect_equal(s$site_table$end_year, 1989)
  expect_equal(s$site_table$site_file, "site.100")
})

test_that("three_block.sch parses to 3 blocks; block 2 is constant weather", {
  s <- read_sch(fixture("three_block.sch"))
  expect_true(s$extra$parsed_ok)
  expect_equal(nrow(s$block_table), 3)
  b2 <- s$block_table[s$block_table$block == 2, ]
  expect_equal(b2$weather_choice, "C")
  expect_true(is.na(b2$weather_file))
  expect_equal(b2$block_comment, "TEST_synthetic_block_comment")
})

test_that("eq_aceq.sch captures the extra header line", {
  s <- read_sch(fixture("eq_aceq.sch"))
  extra <- s$site_table$extra_header_lines[[1]]
  expect_true("+150 12/31 ACEQ A" %in% extra)
})

test_that("option_payload.sch parses the CO2 payload start year", {
  s <- read_sch(fixture("option_payload.sch"))
  expect_equal(s$site_table$co2_systems_start_year, 1985)
  expect_equal(s$site_table$co2_systems_end_year, 1995)
})

test_that("immediate_args.sch round-trips event args literally", {
  s <- read_sch(fixture("immediate_args.sch"))
  fert_args <- s$event_table$arg[s$event_table$event == "FERT"]
  expect_true("(3.4N)" %in% fert_args)
  expect_true("(3.7N,0.75F,N15C)" %in% fert_args)

  irig_args <- s$event_table$arg[s$event_table$event == "IRIG"]
  expect_equal(irig_args, "(3A 0.99F -1L)")

  harv_args <- s$event_table$arg[s$event_table$event == "HARV"]
  expect_equal(harv_args, "G75S")
})

test_that("a file with no marker fails to parse without throwing", {
  tmp <- tempfile(fileext = ".sch")
  writeLines(c("just some text", "with no marker"), tmp)
  s <- read_sch(tmp)
  expect_false(s$extra$parsed_ok)
  expect_true(nzchar(s$extra$parse_error))
  unlink(tmp)
})

test_that("bad_*.sch fixtures all parse successfully -- only the validator objects", {
  bad_files <- c("bad_unknown_event.sch", "bad_doy.sch", "bad_pairing.sch")
  for (f in bad_files) {
    s <- read_sch(fixture(f))
    expect_true(s$extra$parsed_ok, info = f)
  }
})

test_that("bad_unknown_event.sch keeps the unknown event token verbatim", {
  s <- read_sch(fixture("bad_unknown_event.sch"))
  expect_true("ZZZZ" %in% s$event_table$event)
})

test_that("bad_doy.sch keeps all three day-of-year values, including the legal 366", {
  s <- read_sch(fixture("bad_doy.sch"))
  expect_setequal(s$event_table$doy[s$event_table$event == "DRAN"], c(0, 400, 366))
})

test_that("bad_pairing.sch keeps the unpaired LAST event", {
  s <- read_sch(fixture("bad_pairing.sch"))
  expect_equal(s$event_table$event, "LAST")
})

test_that("event and block lines retain their source line numbers", {
  s <- read_sch(fixture("single_block.sch"))
  expect_true(all(!is.na(s$event_table$lineno)))
  expect_true(!is.na(s$block_table$lineno_block))
})

test_that("alternate header label spellings ('pH Effect', 'Soil warming') are recognized", {
  tmp <- tempfile(fileext = ".sch")
  writeLines(c(
    "1980          Starting year",
    "1989          Last year",
    "site.100      Site file name",
    "0             Labeling type",
    "-1            Labeling year",
    "-1.00         Microcosm",
    "-1            CO2 Systems",
    "-1            pH Effect",
    "-1            Soil warming",
    "0             N input scalar option",
    "0             OMAD scalar option",
    "-1            Climate scalar option",
    "1             Initial system",
    "W1            Initial crop",
    "              Initial tree",
    "",
    "Year Month Option",
    "1             Block",
    "1989          Last year",
    "10            Repeats # years",
    "1980          Output starting year",
    "12            Output month",
    "1.00          Output interval",
    "C             Weather choice",
    "  1   1 FRST",
    " 10 300 LAST",
    "-999 -999 X"
  ), tmp)
  s <- read_sch(tmp)
  expect_true(s$extra$parsed_ok)
  expect_equal(s$site_table$ph_effect, -1)
  expect_equal(s$site_table$soil_warming, -1)
  unlink(tmp)
})
