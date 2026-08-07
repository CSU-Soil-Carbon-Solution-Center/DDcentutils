valid_site <- function() {
  df <- data.frame(
    start_year = 1980, end_year = 1990, site_file = "site.100",
    labeling_type = 0, labeling_year = -1, microcosm = -1,
    co2_systems = -1, ph_effect = -1, soil_warming = -1,
    n_input_scalar_option = 0, omad_scalar_option = 0,
    climate_scalar_option = -1, initial_system = 1,
    initial_crop = "W1", initial_tree = "",
    stringsAsFactors = FALSE
  )
  df$extra_header_lines <- list(character(0))
  df
}

valid_block <- function() {
  data.frame(
    block = 1, block_comment = "", last_year = 1990, repeats_years = 11,
    output_start_year = 1980, output_month = 12, output_interval = 1,
    weather_choice = "C", weather_file = NA_character_,
    stringsAsFactors = FALSE
  )
}

valid_event <- function() {
  data.frame(
    block = 1, year = 1, doy = 1, event = "CROP", arg = "W1",
    stringsAsFactors = FALSE
  )
}

test_that("new_sch constructs a valid sch object", {
  s <- new_sch(valid_site(), valid_block(), valid_event())
  expect_s3_class(s, "sch")
  expect_identical(names(s), c("site_table", "block_table", "event_table", "extra"))
  expect_equal(nrow(s$site_table), 1)
  expect_equal(nrow(s$block_table), 1)
  expect_equal(nrow(s$event_table), 1)
})

test_that("as_sch is the identity for an sch object and errors otherwise", {
  s <- new_sch(valid_site(), valid_block(), valid_event())
  expect_identical(as_sch(s), s)
  expect_error(as_sch(list()), "don't know how to coerce")
})

test_that("a missing required column errors and names the column", {
  site <- valid_site()
  site$site_file <- NULL
  expect_error(new_sch(site, valid_block(), valid_event()), "site_file")

  block <- valid_block()
  block$weather_choice <- NULL
  expect_error(new_sch(valid_site(), block, valid_event()), "weather_choice")

  event <- valid_event()
  event$doy <- NULL
  expect_error(new_sch(valid_site(), valid_block(), event), "doy")
})

test_that("a wrong-type, non-coercible column errors", {
  site <- valid_site()
  site$site_file <- list(1)
  expect_error(new_sch(site, valid_block(), valid_event()), "site_file")

  event <- valid_event()
  event$doy <- "not-a-number"
  expect_error(new_sch(valid_site(), valid_block(), event), "doy")
})

test_that("start_year/end_year/weather_file allow NA, and initial_tree allows blank", {
  site <- valid_site()
  site$start_year <- NA
  site$end_year <- NA
  block <- valid_block()
  block$weather_file <- NA_character_

  s <- new_sch(site, block, valid_event())
  expect_true(is.na(s$site_table$start_year))
  expect_true(is.na(s$site_table$end_year))
  expect_true(is.na(s$block_table$weather_file))
  expect_identical(s$site_table$initial_tree, "")
})

test_that("print.sch emits the block count", {
  s <- new_sch(valid_site(), valid_block(), valid_event())
  out <- capture.output(print(s))
  expect_true(any(grepl("blocks:\\s*1", out)))
})

test_that("sch_columns returns the documented columns for each table", {
  expect_true(all(c("start_year", "end_year", "site_file", "extra_header_lines") %in% sch_columns("site")))
  expect_true(all(c("block", "weather_choice", "weather_file") %in% sch_columns("block")))
  expect_identical(sch_columns("event"), c("block", "year", "doy", "event", "arg"))
})
