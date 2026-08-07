sch_fixture <- function(name) {
  system.file("extdata", "sch", name, package = "DDcentutils")
}

test_that("sch_cli(character()) prints usage and returns 0", {
  rc <- NA_integer_
  out <- capture.output(rc <- sch_cli(character()))
  expect_equal(rc, 0L)
  expect_true(any(grepl("Usage:", out)))
})

test_that("--help / -h print usage and return 0", {
  rc1 <- NA_integer_
  capture.output(rc1 <- sch_cli(c("--help")))
  expect_equal(rc1, 0L)

  rc2 <- NA_integer_
  capture.output(rc2 <- sch_cli(c("-h")))
  expect_equal(rc2, 0L)
})

test_that("an unknown subcommand returns 2", {
  rc <- NA_integer_
  capture.output(rc <- sch_cli(c("bogus")))
  expect_equal(rc, 2L)
})

test_that("validate on a clean fixture returns 0", {
  rc <- NA_integer_
  capture.output(rc <- sch_cli(c("validate", sch_fixture("single_block.sch"))))
  expect_equal(rc, 0L)
})

test_that("validate on bad_unknown_event.sch returns 1", {
  rc <- NA_integer_
  capture.output(rc <- sch_cli(c("validate", sch_fixture("bad_unknown_event.sch"))))
  expect_equal(rc, 1L)
})

test_that("validate --json output parses with jsonlite::fromJSON()", {
  out <- capture.output(rc <- sch_cli(c("validate", sch_fixture("bad_unknown_event.sch"), "--json")))
  expect_equal(rc, 1L)
  parsed <- jsonlite::fromJSON(paste(out, collapse = "\n"))
  expect_true("SCH_UNKNOWN_EVENT" %in% parsed$code)
})

test_that("build requires --site-table/--event-table/--out and returns 2 otherwise", {
  rc <- NA_integer_
  capture.output(rc <- sch_cli(c("build", "--site-table", "x.csv")))
  expect_equal(rc, 2L)
})

test_that("inspect requires a schedule path and --out-dir, returning 2 otherwise", {
  rc1 <- NA_integer_
  capture.output(rc1 <- sch_cli(c("inspect")))
  expect_equal(rc1, 2L)

  rc2 <- NA_integer_
  capture.output(rc2 <- sch_cli(c("inspect", sch_fixture("single_block.sch"))))
  expect_equal(rc2, 2L)
})

test_that("inspect then build reproduces an equivalent sch", {
  fixture_path <- sch_fixture("single_block.sch")
  outdir <- tempfile()
  outfile <- tempfile(fileext = ".sch")
  on.exit(unlink(c(outdir, outfile), recursive = TRUE), add = TRUE)

  rc1 <- NA_integer_
  capture.output(rc1 <- sch_cli(c("inspect", fixture_path, "--out-dir", outdir)))
  expect_equal(rc1, 0L)
  expect_true(all(c("site.csv", "blocks.csv", "events.csv") %in% list.files(outdir)))

  rc2 <- NA_integer_
  capture.output(rc2 <- sch_cli(c(
    "build",
    "--site-table", file.path(outdir, "site.csv"),
    "--event-table", file.path(outdir, "events.csv"),
    "--block-table", file.path(outdir, "blocks.csv"),
    "--out", outfile
  )))
  expect_equal(rc2, 0L)

  original <- read_sch(fixture_path)
  rebuilt <- read_sch(outfile)

  strip_lineno <- function(df) df[, !grepl("^lineno", names(df)), drop = FALSE]
  expect_equal(rebuilt$site_table, original$site_table)
  expect_equal(strip_lineno(rebuilt$block_table), strip_lineno(original$block_table))
  expect_equal(strip_lineno(rebuilt$event_table), strip_lineno(original$event_table))
})

test_that("inspect then build round-trips a multi-block fixture in order", {
  fixture_path <- sch_fixture("three_block.sch")
  outdir <- tempfile()
  outfile <- tempfile(fileext = ".sch")
  on.exit(unlink(c(outdir, outfile), recursive = TRUE), add = TRUE)

  capture.output(sch_cli(c("inspect", fixture_path, "--out-dir", outdir)))
  capture.output(sch_cli(c(
    "build",
    "--site-table", file.path(outdir, "site.csv"),
    "--event-table", file.path(outdir, "events.csv"),
    "--block-table", file.path(outdir, "blocks.csv"),
    "--out", outfile
  )))

  rebuilt <- read_sch(outfile)
  expect_equal(rebuilt$block_table$block, c(1, 2, 3))
  expect_equal(rebuilt$block_table$weather_choice, c("F", "C", "F"))
})
