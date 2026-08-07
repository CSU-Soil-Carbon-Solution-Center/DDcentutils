fixture <- function(name) {
  system.file("extdata", "sch", name, package = "DDcentutils")
}

strip_lineno_cols <- function(df) {
  df[, !grepl("^lineno", names(df)), drop = FALSE]
}

expect_semantic_round_trip <- function(fname) {
  original <- read_sch(fixture(fname))
  rendered <- write_sch(original)

  tmp <- tempfile(fileext = ".sch")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(rendered, tmp)

  reread <- read_sch(tmp)

  expect_true(reread$extra$parsed_ok, info = fname)
  expect_equal(reread$site_table, original$site_table, info = fname)
  expect_equal(
    strip_lineno_cols(reread$block_table), strip_lineno_cols(original$block_table),
    info = fname
  )
  expect_equal(
    strip_lineno_cols(reread$event_table), strip_lineno_cols(original$event_table),
    info = fname
  )
}

all_fixtures <- c(
  "single_block.sch", "three_block.sch", "eq_aceq.sch", "option_payload.sch",
  "immediate_args.sch", "bad_unknown_event.sch", "bad_doy.sch", "bad_pairing.sch"
)

test_that("every P3 fixture survives a semantic round trip", {
  for (f in all_fixtures) {
    expect_semantic_round_trip(f)
  }
})

test_that("-999 -999 X appears exactly once per block and there is no file-level terminator", {
  s <- read_sch(fixture("three_block.sch"))
  lines <- write_sch(s)

  is_terminator <- grepl("^-999\\s+-999\\s+X\\s*$", lines)
  expect_equal(sum(is_terminator), 3)

  # No extra terminator-like line after the very last block's terminator.
  last_terminator_idx <- max(which(is_terminator))
  expect_equal(last_terminator_idx, length(lines))
})

test_that("a block with weather_choice == 'C' emits no filename line", {
  s <- read_sch(fixture("three_block.sch"))
  lines <- write_sch(s)

  block_start_idx <- which(grepl("^2\\s+Block", lines))
  expect_length(block_start_idx, 1)
  weather_choice_idx <- block_start_idx + 6
  expect_true(grepl("Weather choice$", lines[weather_choice_idx]))
  expect_true(grepl("^C\\s", lines[weather_choice_idx]))

  # The very next line must be an event line (two leading integers), not a
  # bare weather filename -- i.e. no filename line was inserted.
  next_line <- lines[weather_choice_idx + 1]
  expect_true(grepl("^\\s*\\d+\\s+\\d+\\s+\\S", next_line))
})

test_that("write_sch(path = ...) writes the file and returns the same text invisibly", {
  s <- read_sch(fixture("single_block.sch"))
  tmp <- tempfile(fileext = ".sch")
  on.exit(unlink(tmp), add = TRUE)

  returned <- write_sch(s, path = tmp)
  expect_true(file.exists(tmp))
  written <- readLines(tmp)
  expect_equal(returned, written)
})

test_that("no inline provenance comments are emitted on event lines", {
  s <- read_sch(fixture("immediate_args.sch"))
  lines <- write_sch(s)
  event_lines <- lines[grepl("^\\s*-?\\d+\\s+-?\\d+\\s+\\S", lines) & !grepl("^-999", lines)]
  expect_false(any(grepl("#", event_lines)))
})
