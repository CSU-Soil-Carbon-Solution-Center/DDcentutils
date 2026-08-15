test_that("runDayCent builds local executable arguments for each run type", {
  expect_equal(DDcentutils:::.runDayCent_args("site", "eq", "C:/daycent/100.list"),
               '-s site_eq -N eq -W eq_extend.100 -l "C:/daycent/100.list"')
  expect_equal(DDcentutils:::.runDayCent_args("site", "base", "C:/daycent/100.list"),
               '-s site_base -N base -W base_extend.100 --site eq_extend.100 -l "C:/daycent/100.list"')
  expect_equal(DDcentutils:::.runDayCent_args("site", "scenario_01", "C:/daycent/100.list"),
               '-s site_scenario_01 -N scenario_01 --site base_extend.100 -l "C:/daycent/100.list"')
  expect_equal(DDcentutils:::.runDayCent_args("site", "base", "C:/daycent/100.list", TRUE),
               '-s site_base -N base -l "C:/daycent/100.list"')
})

test_that("runDayCent validates local input paths before execution", {
  temp_dir <- tempfile("run-daycent-")
  dir.create(temp_dir)
  outfiles <- file.path(temp_dir, "outfiles.in")
  executable <- file.path(temp_dir, "daycent.exe")
  list_file <- file.path(temp_dir, "100.list")
  file.create(outfiles, executable, list_file)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(runDayCent(file.path(temp_dir, "missing.in"), "site", "eq", executable, list_file),
               "Error:.*file does not exist")
  expect_error(runDayCent(outfiles, "site", "eq", file.path(temp_dir, "missing.exe"), list_file),
               "DayCent executable not found")
  expect_error(runDayCent(outfiles, "site", "eq", executable, file.path(temp_dir, "missing.list")),
               "Path to DC100 list file not found")
})

test_that("runDayCent keeps local working-directory and return behavior", {
  temp_dir <- tempfile("run-daycent-")
  dir.create(temp_dir)
  outfiles <- file.path(temp_dir, "no_outfiles.in")
  executable <- file.path(temp_dir, "daycent.exe")
  list_file <- file.path(temp_dir, "100.list")
  file.create(outfiles, executable, list_file)
  old_wd <- getwd()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  captured <- list()
  local_mocked_bindings(
    .runDayCent_process = function(command, args, wait = TRUE, stdout = TRUE, stderr = TRUE) {
      captured <<- list(command = command, args = args, wait = wait, stdout = stdout, stderr = stderr)
      c("mock stdout", "mock stderr")
    },
    .package = "DDcentutils"
  )

  log <- runDayCent("no_outfiles.in", "site", "scenario", executable, list_file)

  expect_equal(log, c("mock stdout", "mock stderr"))
  expect_equal(captured$command, executable)
  expect_equal(captured$args, paste0('-s site_scenario -N scenario --site base_extend.100 -l "', list_file, '"'))
  expect_identical(captured$wait, TRUE)
  expect_identical(captured$stdout, TRUE)
  expect_identical(captured$stderr, TRUE)
  expect_true(file.exists(file.path(temp_dir, "outfiles.in")))
  expect_equal(normalizePath(getwd(), winslash = "/"), normalizePath(temp_dir, winslash = "/"))
})
