make_batch_project <- function(sites = list(siteA = c("sc1", "sc2"),
                                             siteB = c("sc1"))) {
  project <- tempfile("daycent-batch-project-")
  for (site in names(sites)) {
    site_dir <- file.path(project, "sites", site)
    dir.create(site_dir, recursive = TRUE)
    writeLines("input", file.path(site_dir, paste0(site, ".100")))
    for (scenario in sites[[site]]) {
      writeLines("schedule", file.path(site_dir, paste0(site, "_", scenario, ".sch")))
    }
    writeLines("phase", file.path(site_dir, paste0(site, "_eq.sch")))
    writeLines("phase", file.path(site_dir, paste0(site, "_base.sch")))
  }
  project
}

test_that("batch selection supports all, exact union, and Cartesian pairs", {
  project <- make_batch_project()
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  all <- .daycent_batch_select(project, NULL, NULL, NULL)
  expect_equal(paste(all$site, all$scenario),
               c("siteA sc1", "siteA sc2", "siteB sc1"))
  exact <- .daycent_batch_select(project, c("siteB/sc1", "siteA/sc2"), NULL, NULL)
  expect_equal(paste(exact$site, exact$scenario), c("siteB sc1", "siteA sc2"))
  cartesian <- .daycent_batch_select(project, NULL, c("siteA", "siteB"), "sc1")
  expect_equal(paste(cartesian$site, cartesian$scenario), c("siteA sc1", "siteB sc1"))
})

test_that("batch selection rejects duplicates, wildcards, unmatched, and phases", {
  project <- make_batch_project()
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  expect_error(.daycent_batch_select(project, c("siteA/sc1", "siteA/sc1"), NULL, NULL),
               "duplicate")
  expect_error(.daycent_batch_select(project, c("siteA/*"), NULL, NULL), "exact")
  expect_error(.daycent_batch_select(project, c("siteA/sc9"), NULL, NULL), "not found")
  expect_error(.daycent_batch_select(project, c("siteA/base"), NULL, NULL), "reserved")
  expect_error(.daycent_batch_select(project, NULL, c("siteA"), c("base")), "reserved")
})

test_that("local batch execution preserves caller directory and supports stop/collect", {
  project <- make_batch_project()
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  old <- getwd()
  calls <- character()
  testthat::local_mocked_bindings(
    DayCentRunSite = function(site, scen, ...) {
      calls <<- c(calls, paste(site, scen, sep = "/"))
      if (identical(scen, "sc2")) stop("scenario failed")
      "ok"
    },
    .package = "DDcentutils"
  )
  collected <- runDayCent_batch(project, include = c("siteA/sc1", "siteA/sc2", "siteB/sc1"),
                                error_policy = "collect")
  expect_equal(calls, c("siteA/sc1", "siteA/sc2", "siteB/sc1"))
  expect_equal(collected$task_table$status, c("Completed", "Failed", "Completed"))
  expect_identical(getwd(), old)

  calls <- character()
  stopped <- runDayCent_batch(project, include = c("siteA/sc1", "siteA/sc2", "siteB/sc1"),
                              error_policy = "stop")
  expect_equal(calls, c("siteA/sc1", "siteA/sc2"))
  expect_equal(stopped$task_table$status, c("Completed", "Failed", "NotRun"))
})

test_that("injected parallel execution returns selector order", {
  project <- make_batch_project()
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    DayCentRunSite = function(...) "ok",
    .package = "DDcentutils"
  )
  executor <- function(tasks, worker, workers) {
    lapply(seq_along(tasks), function(index) worker(tasks[[index]], index))
  }
  result <- runDayCent_batch(project, include = "*", workers = 2,
                             executor = executor)
  expect_true(identical(paste(result$task_table$site, result$task_table$scenario),
                        c("siteA sc1", "siteA sc2", "siteB sc1")))
  expect_true(all(result$task_table$status == "Completed"))
})
