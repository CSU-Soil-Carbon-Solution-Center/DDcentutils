test_that("build_sch() renders a minimal schedule matching a hand-written expected string", {
  site <- data.frame(
    start_year = 1980, end_year = 1989, site_file = "site.100",
    initial_crop = "W1", stringsAsFactors = FALSE
  )
  events <- data.frame(
    block = 1, year = 1, doy = 1, event = "CROP", arg = "W1",
    stringsAsFactors = FALSE
  )

  s <- build_sch(site, events)
  rendered <- write_sch(s)

  pad14 <- function(v) sprintf("%-14s", v)

  expected <- c(
    paste0(pad14(1980), "Starting year"),
    paste0(pad14(1989), "Last year"),
    paste0(pad14("site.100"), "Site file name"),
    paste0(pad14(0), "Labeling type"),
    paste0(pad14(-1), "Labeling year"),
    paste0(pad14(-1), "Microcosm"),
    paste0(pad14(-1), "CO2 Systems"),
    paste0(pad14(-1), "pH effect"),
    paste0(pad14(-1), "Soil Warming"),
    paste0(pad14(0), "N input scalar option"),
    paste0(pad14(0), "OMAD scalar option"),
    paste0(pad14(0), "Climate scalar option"),
    paste0(pad14(1), "Initial system"),
    paste0(pad14("W1"), "Initial crop"),
    paste0(pad14(""), "Initial tree"),
    "",
    "Year Month Option",
    paste0(pad14(1), "Block"),
    paste0(pad14(1989), "Last year"),
    paste0(pad14(10), "Repeats # years"),
    paste0(pad14(1980), "Output starting year"),
    paste0(pad14(12), "Output month"),
    paste0(pad14(1), "Output interval"),
    paste0(pad14("C"), "Weather choice"),
    "  1   1 CROP W1",
    "-999 -999 X"
  )

  expect_equal(rendered, expected)
  expect_equal(sum(grepl("^-999 -999 X$", rendered)), 1)
})

test_that("build_sch() derives weather_choice = 'F' when site_table$weather_file is given", {
  site <- data.frame(
    start_year = 1980, end_year = 1989, site_file = "site.100",
    initial_crop = "W1", weather_file = "site.wth", stringsAsFactors = FALSE
  )
  events <- data.frame(
    block = 1, year = 1, doy = 1, event = "CROP", arg = "W1",
    stringsAsFactors = FALSE
  )

  s <- build_sch(site, events)
  expect_equal(s$block_table$weather_choice, "F")
  expect_equal(s$block_table$weather_file, "site.wth")
  expect_false("weather_file" %in% names(s$site_table))
})

test_that("build_sch() errors on a missing start_year, end_year, or site_file", {
  events <- data.frame(
    block = 1, year = 1, doy = 1, event = "CROP", arg = "W1",
    stringsAsFactors = FALSE
  )

  no_start <- data.frame(end_year = 1989, site_file = "site.100", initial_crop = "W1")
  expect_error(build_sch(no_start, events), "start_year")

  no_site_file <- data.frame(start_year = 1980, end_year = 1989, initial_crop = "W1")
  expect_error(build_sch(no_site_file, events), "site_file")
})

test_that("an explicit 3-row block_table with events spanning blocks 1-3 produces 3 ordered blocks", {
  site <- data.frame(
    start_year = 1980, end_year = 2009, site_file = "site.100",
    initial_crop = "W1", stringsAsFactors = FALSE
  )
  blocks <- data.frame(
    block = 1:3, block_comment = "", last_year = c(1989, 1999, 2009),
    repeats_years = c(10, 10, 10), output_start_year = c(1980, 1990, 2000),
    output_month = 12, output_interval = 1.0, weather_choice = "C",
    weather_file = NA_character_, stringsAsFactors = FALSE
  )
  events <- data.frame(
    block = c(1, 2, 3), year = c(1, 1, 1), doy = c(1, 1, 1),
    event = c("FRST", "PLTM", "FRST"), arg = "", stringsAsFactors = FALSE
  )

  s <- build_sch(site, events, blocks)
  expect_equal(nrow(s$block_table), 3)
  expect_equal(s$block_table$block, c(1, 2, 3))

  tmp <- tempfile(fileext = ".sch")
  on.exit(unlink(tmp), add = TRUE)
  write_sch(s, path = tmp)
  reread <- read_sch(tmp)

  expect_true(reread$extra$parsed_ok)
  expect_equal(sum(grepl("^-999 -999 X$", write_sch(s))), 3)
  expect_equal(reread$block_table$block, c(1, 2, 3))
  expect_equal(reread$event_table$event, c("FRST", "PLTM", "FRST"))
  expect_equal(reread$event_table$block, c(1, 2, 3))
})

test_that("write_sch_tables() -> read_sch_tables() -> build_sch() reproduces the original sch", {
  site <- data.frame(
    start_year = 1980, end_year = 1989, site_file = "site.100",
    initial_crop = "W1", initial_tree = "", stringsAsFactors = FALSE
  )
  events <- data.frame(
    block = 1, year = 1, doy = 1, event = "CROP", arg = "W1",
    stringsAsFactors = FALSE
  )
  original <- build_sch(site, events)

  dir <- tempfile()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  write_sch_tables(original, dir)
  tabs <- read_sch_tables(dir)

  expect_true(all(c("site.csv", "blocks.csv", "events.csv") %in% list.files(dir)))

  rebuilt <- build_sch(tabs$site_table, tabs$event_table, tabs$block_table)

  expect_equal(rebuilt$site_table, original$site_table)
  expect_equal(rebuilt$block_table, original$block_table)
  expect_equal(rebuilt$event_table, original$event_table)
})

test_that("write_sch_tables() -> read_sch_tables() round trip preserves extra_header_lines", {
  s <- read_sch(system.file("extdata", "sch", "eq_aceq.sch", package = "DDcentutils"))

  dir <- tempfile()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  write_sch_tables(s, dir)
  tabs <- read_sch_tables(dir)

  expect_equal(tabs$site_table$extra_header_lines[[1]], s$site_table$extra_header_lines[[1]])
})

test_that("the old schFileBuilder.r draft functions no longer exist in the package", {
  ns <- asNamespace("DDcentutils")
  for (fn in c("writeSchedule", "writeHeader", "writeBlockHeader", "writeBlock")) {
    expect_false(exists(fn, where = ns, inherits = FALSE), info = fn)
  }
})
