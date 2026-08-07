lib_fixture <- function(name) {
  system.file("extdata", "lib100", name, package = "DDcentutils")
}

test_that("crop.100 yields exactly the four stub block IDs", {
  r <- read_library100(lib_fixture("crop.100"))
  expect_equal(r$block_ids, c("W1", "W2", "G2", "C5"))
  expect_equal(r$duplicate_ids, character(0))
})

test_that("fert.100 and harv.100 stubs yield their documented IDs", {
  fert <- read_library100(lib_fixture("fert.100"))
  expect_equal(fert$block_ids, "N15C")

  harv <- read_library100(lib_fixture("harv.100"))
  expect_equal(harv$block_ids, c("G", "G75S"))
})

test_that("dup_ids.100 reports its repeated ID in duplicate_ids", {
  r <- read_library100(lib_fixture("dup_ids.100"))
  expect_equal(r$block_ids, c("DUPID", "DUPID"))
  expect_equal(r$duplicate_ids, "DUPID")
})

test_that("comment and blank lines are skipped, not treated as block IDs", {
  r <- read_library100(lib_fixture("crop.100"))
  expect_false(any(startsWith(r$block_ids, "#")))
})

test_that("read_library_dir() over a directory missing graz.100 omits it without erroring", {
  dir <- dirname(lib_fixture("crop.100"))
  result <- read_library_dir(dir)

  expect_false("graz.100" %in% names(result))
  expect_true("crop.100" %in% names(result))
  expect_equal(result[["crop.100"]]$block_ids, c("W1", "W2", "G2", "C5"))
})

test_that("read_library_dir() with explicit files only looks for those files", {
  dir <- dirname(lib_fixture("crop.100"))
  result <- read_library_dir(dir, files = c("crop.100", "dup_ids.100", "nope.100"))

  expect_setequal(names(result), c("crop.100", "dup_ids.100"))
})

test_that("read_library100() does not parse parameter values into block_ids", {
  r <- read_library100(lib_fixture("crop.100"))
  expect_false(any(grepl("^[0-9.]+$", r$block_ids)))
})
