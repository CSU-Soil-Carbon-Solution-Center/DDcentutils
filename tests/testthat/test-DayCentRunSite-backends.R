test_that("DayCentRunSite keeps the existing executable route by default", {
  site_dir <- tempfile("daycent-site-")
  dir.create(site_dir)
  on.exit(unlink(site_dir, recursive = TRUE), add = TRUE)
  old_wd <- setwd(site_dir)
  on.exit(setwd(old_wd), add = TRUE)
  file.create("siteA_scenario1.sch")
  calls <- list()

  testthat::local_mocked_bindings(
    noBinFlag = function(...) NULL,
    runDayCent = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      "Normal completion"
    },
    rename_and_move_output_files = function(...) NULL,
    .package = "DDcentutils"
  )

  result <- DayCentRunSite("siteA", "scenario1", dc_exe_in = "exe",
                           dc_path100_in = "list.100")
  expect_equal(result, "Normal completion")
  expect_length(calls, 1L)
  expect_equal(calls[[1L]]$site, "siteA")
  expect_equal(calls[[1L]]$run, "scenario1")
})

test_that("DayCentRunSite routes API options without local validation", {
  config <- list(backend = "api", api_key = "secret", product_id = "product")
  captured <- NULL
  testthat::local_mocked_bindings(
    runDayCent_api = function(...) {
      captured <<- list(...)
      list(run_id = "run-api")
    },
    runDayCent = function(...) stop("local runner should not be called"),
    noBinFlag = function(...) stop("local filesystem should not be checked"),
    .package = "DDcentutils"
  )

  result <- DayCentRunSite(
    "siteA", "scenario1", run_eq = TRUE,
    dc_exe_in = "missing-executable", dc_path100_in = "missing-list",
    config = config, project_path = "project", name = "api-name",
    wait = FALSE, keep_zip = TRUE, timeout_seconds = 9,
    output_zip = "result.zip", overwrite = TRUE
  )
  expect_equal(result$run_id, "run-api")
  expect_equal(captured[c("include", "run_eq", "run_base", "config", "project_path",
                          "name", "wait", "keep_zip", "timeout_seconds",
                          "output_zip", "overwrite")],
               list(include = "siteA/scenario1", run_eq = TRUE, run_base = TRUE,
                    config = config, project_path = "project", name = "api-name",
                    wait = FALSE, keep_zip = TRUE, timeout_seconds = 9,
                    output_zip = "result.zip", overwrite = TRUE))
})

test_that("DayCentRunSite rejects invalid backend combinations before routing", {
  config <- list(backend = "api", api_key = "secret", product_id = "product")
  called <- FALSE
  testthat::local_mocked_bindings(
    runDayCent_api = function(...) { called <<- TRUE },
    .package = "DDcentutils"
  )

  expect_error(DayCentRunSite("siteA", "scenario1", backend = "exe",
                              config = config), "agree")
  expect_error(DayCentRunSite("siteA", "scenario1", run_base = TRUE,
                              config = config), "Base-only")
  expect_false(called)
})
