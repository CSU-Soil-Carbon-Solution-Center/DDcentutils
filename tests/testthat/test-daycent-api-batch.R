make_batch_project_api <- function() {
  project <- tempfile("daycent-batch-project-")
  for (site in c("siteA", "siteB")) {
    site_dir <- file.path(project, "sites", site)
    dir.create(site_dir, recursive = TRUE)
    writeLines("input", file.path(site_dir, paste0(site, ".100")))
    scenarios <- if (site == "siteA") c("sc1", "sc2") else "sc1"
    for (scenario in scenarios) {
      writeLines("schedule", file.path(site_dir, paste0(site, "_", scenario, ".sch")))
    }
  }
  project
}

test_that("API batch submits once and returns resumable parent metadata", {
  project <- make_batch_project_api()
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  config <- list(backend = "api", api_key = "secret", model_name = "DayCent", model_version = "491",
                 .__daycent_product_id = "product")
  captured <- list()
  testthat::local_mocked_bindings(
    runDayCent_api = function(include, run_eq, run_base, config, project_path,
                              name, wait, ...) {
      captured <<- list(include = include, run_eq = run_eq, run_base = run_base,
                        wait = wait, project_path = project_path)
      list(run_id = "parent-1", status = "Queued")
    },
    .package = "DDcentutils"
  )
  result <- runDayCent_batch(project, backend = "api",
                             include = c("siteB/sc1", "siteA/sc2"),
                             config = config, run_eq = TRUE, run_base = TRUE,
                             wait = FALSE)
  expect_equal(captured$include, c("siteB/sc1", "siteA/sc2"))
  expect_true(captured$run_eq)
  expect_true(captured$run_base)
  expect_false(captured$wait)
  expect_equal(result$parent_run_id, "parent-1")
  expect_equal(result$task_table$status, c("Submitted", "Submitted"))
  expect_equal(result$task_table$parent_run_id, rep("parent-1", 2))
})

test_that("API batch maps child records without inferring from parent status", {
  project <- make_batch_project_api()
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  config <- list(backend = "api", api_key = "secret", model_name = "DayCent", model_version = "491",
                 .__daycent_product_id = "product")
  testthat::local_mocked_bindings(
    runDayCent_api = function(...) list(
      run_id = "parent-2", status = list(status = "Completed"), results = list()
    ),
    .daycent_http_request = function(method, url, headers, verify_ssl) {
      list(status_code = 200L, text = "", body = list(
        list(id = "child-1", parentId = "parent-2", siteName = "siteA",
             scenarioName = "sc1", status = "Completed"),
        list(id = "child-2", parentId = "parent-2", siteName = "siteA",
             scenarioName = "sc2", status = "Completed"),
        list(id = "child-3", parentId = "parent-2", siteName = "siteB",
             scenarioName = "sc1", status = "Failed")
      ))
    },
    .package = "DDcentutils"
  )
  result <- runDayCent_batch(project, backend = "api", include = "*",
                             config = config, wait = TRUE)
  expect_equal(result$parent_run_id, "parent-2")
  expect_equal(result$task_table$status, c("Completed", "Completed", "Failed"))
  expect_equal(result$task_table$child_run_id, c("child-1", "child-2", "child-3"))
})

test_that("API batch requires model identity before orchestration", {
  project <- make_batch_project_api()
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  expect_error(runDayCent_batch(project, backend = "api",
                                config = list(api_key = "secret")), "model_name")
})
