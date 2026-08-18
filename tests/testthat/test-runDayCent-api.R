test_that("runDayCent_api composes multi-pair staging, submission, watching, and download", {
  project <- tempfile("daycent-project-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  config <- list(backend = "api", api_key = "secret", model_name = "DayCent",
                 model_version = "491", .__daycent_product_id = "product")
  calls <- list()

  testthat::local_mocked_bindings(
    zip_daycent_inputs = function(project_path, include, run_eq, out_zip) {
      calls$zip <<- list(project_path = project_path, include = include,
                        run_eq = run_eq, out_zip = out_zip)
      file.create(out_zip)
      invisible(out_zip)
    },
    submit_daycent_run = function(config, input_zip, include, run_eq, name, ..., wait) {
      calls$submit <<- list(input_zip = input_zip, include = include,
                           run_eq = run_eq, name = name, wait = wait)
      list(run_id = "run-12", status = "Queued")
    },
    watch_daycent_run = function(config, run_id, timeout_seconds) {
      calls$watch <<- list(run_id = run_id, timeout_seconds = timeout_seconds)
      list(run_id = run_id, status = "Completed")
    },
    download_daycent_results = function(config, run_id, project_path, keep_zip,
                                         output_zip, overwrite) {
      calls$download <<- list(run_id = run_id, project_path = project_path,
                              keep_zip = keep_zip, output_zip = output_zip,
                              overwrite = overwrite)
      list(output_paths = "result.out", metadata_dir = "apiDocs/run-12",
           zip_path = "result.zip")
    },
    .package = "DDcentutils"
  )

  result <- runDayCent_api(
    include = c("siteA/scenario1", "siteB/scenario2"),
    run_eq = TRUE, run_base = TRUE, config = config,
    project_path = project, name = "named-run", keep_zip = TRUE,
    timeout_seconds = 42, output_zip = "result.zip", overwrite = TRUE
  )
  expect_equal(calls$zip$include, c("siteA/scenario1", "siteB/scenario2"))
  expect_true(calls$zip$run_eq)
  expect_equal(calls$submit[c("include", "name", "wait")],
               list(include = c("siteA/scenario1", "siteB/scenario2"),
                    name = "named-run", wait = FALSE))
  expect_equal(calls$watch, list(run_id = "run-12", timeout_seconds = 42))
  expect_true(calls$download$keep_zip)
  expect_true(calls$download$overwrite)
  expect_equal(result$run_id, "run-12")
  expect_equal(result$status$status, "Completed")
  expect_equal(result$output_paths, "result.out")
})

test_that("runDayCent_api wait false returns resumable submission", {
  project <- tempfile("daycent-project-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  config <- list(backend = "api", api_key = "secret", model_name = "DayCent",
                 model_version = "491", .__daycent_product_id = "product")
  watched <- FALSE

  testthat::local_mocked_bindings(
    zip_daycent_inputs = function(project_path, include, run_eq, out_zip) {
      file.create(out_zip)
      invisible(out_zip)
    },
    submit_daycent_run = function(...) list(run_id = "run-resume", status = "Queued"),
    watch_daycent_run = function(...) { watched <<- TRUE },
    .package = "DDcentutils"
  )

  result <- runDayCent_api("siteA/scenario1", config = config,
                           project_path = project, wait = FALSE)
  expect_equal(result$run_id, "run-resume")
  expect_false(watched)
})

test_that("runDayCent_api validates before staging", {
  project <- tempfile("daycent-project-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  staged <- FALSE
  testthat::local_mocked_bindings(
    zip_daycent_inputs = function(...) { staged <<- TRUE },
    .package = "DDcentutils"
  )

  expect_error(runDayCent_api("siteA/scenario1",
                              config = list(backend = "api", api_key = "secret"),
                              project_path = project), "model_name")
  expect_error(runDayCent_api("siteA/eq",
                              config = list(backend = "api", api_key = "secret",
                                            model_name = "DayCent", model_version = "491"),
                              project_path = project), "run_eq")
  expect_error(runDayCent_api("siteA/scenario1", run_base = TRUE,
                              config = list(backend = "api", api_key = "secret",
                                            model_name = "DayCent", model_version = "491"),
                              project_path = project), "require.*match")
  expect_error(runDayCent_api("siteA/scenario1",
                              config = list(backend = "exe", api_key = "secret",
                                            model_name = "DayCent", model_version = "491"),
                              project_path = project), "API runner configuration")
  expect_false(staged)
})

test_that("runDayCent_api dry run stages and submits once without watching or downloading", {
  project <- tempfile("daycent-project-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  config <- list(backend = "api", api_key = "secret", model_name = "DayCent",
                 model_version = "491", .__daycent_product_id = "product")
  calls <- list(zip = 0L, submit = 0L, watch = 0L, download = 0L)
  testthat::local_mocked_bindings(
    zip_daycent_inputs = function(project_path, include, run_eq, out_zip) {
      calls$zip <<- calls$zip + 1L
      file.create(out_zip)
      invisible(out_zip)
    },
    submit_daycent_run = function(...) {
      calls$submit <<- calls$submit + 1L
      list(dry_run = TRUE, passed = TRUE)
    },
    watch_daycent_run = function(...) calls$watch <<- calls$watch + 1L,
    download_daycent_results = function(...) calls$download <<- calls$download + 1L,
    .package = "DDcentutils"
  )
  result <- runDayCent_api("siteA/scenario1", config = config,
                           project_path = project, dry_run = TRUE,
                           dry_run_mode = "Full")
  expect_true(result$dry_run)
  expect_equal(calls, list(zip = 1L, submit = 1L, watch = 0L, download = 0L))
})
