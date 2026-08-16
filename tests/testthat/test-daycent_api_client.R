test_that("submission maps the DayCent contract and repeated include fields", {
  input_zip <- tempfile(fileext = ".zip")
  file.create(input_zip)
  on.exit(unlink(input_zip), add = TRUE)
  config <- list(api_key = "secret-key", product_id = "product-02",
                 base_url = "https://api.example.test", verify_ssl = TRUE)
  captured <- NULL

  testthat::local_mocked_bindings(
    .daycent_http_request = function(method, url, headers, fields, verify_ssl) {
      captured <<- list(method = method, url = url, headers = headers,
                        fields = fields, verify_ssl = verify_ssl)
      list(status_code = 202,
           body = list(modelRun = list(id = "run-02", status = "Queued",
                                       type = "Batch", parentId = "parent-02",
                                       childStatusCounts = list(Queued = 2))),
           text = "")
    },
    .package = "DDcentutils"
  )

  result <- submit_daycent_run(
    config, input_zip, include = c("siteA/scenario1", "siteB/scenario2"),
    run_eq = TRUE, name = "test-run"
  )

  expect_equal(result$run_id, "run-02")
  expect_equal(result$run_type, "Batch")
  expect_equal(result$parent_id, "parent-02")
  expect_equal(result$child_status_counts$Queued, 2)
  expect_equal(captured$method, "POST")
  expect_equal(captured$url, "https://api.example.test/api/ModelRuns")
  expect_equal(captured$headers$`X-Api-Key`, "secret-key")
  expect_equal(captured$verify_ssl, TRUE)
  expect_equal(captured$fields$productId, "product-02")
  expect_equal(captured$fields$name, "test-run")
  expect_equal(captured$fields$runEquilibrium, "true")
  expect_equal(unname(captured$fields[which(names(captured$fields) == "include")]),
               list("siteA/scenario1", "siteB/scenario2"))
})

test_that("submission omits include for all discovered site/scenario pairs", {
  input_zip <- tempfile(fileext = ".zip")
  file.create(input_zip)
  on.exit(unlink(input_zip), add = TRUE)
  config <- list(api_key = "secret-key", product_id = "product-02")
  fields_seen <- list()
  testthat::local_mocked_bindings(
    .daycent_http_request = function(method, url, headers, fields, verify_ssl) {
      fields_seen <<- fields
      list(status_code = 202, body = list(id = "run-03", status = "Queued"), text = "")
    },
    .package = "DDcentutils"
  )

  submit_daycent_run(config, input_zip, include = "*", run_eq = FALSE)
  expect_false("include" %in% names(fields_seen))
})

test_that("submission validates product identity and include selectors", {
  input_zip <- tempfile(fileext = ".zip")
  file.create(input_zip)
  on.exit(unlink(input_zip), add = TRUE)

  expect_error(
    submit_daycent_run(list(api_key = "secret-key"), input_zip),
    "product_id"
  )
  expect_error(
    submit_daycent_run(list(api_key = "secret-key", product_id = "product-02"),
                       input_zip, include = "siteA/eq"),
    "run_eq"
  )
})

test_that("status requests parse the API response and redact API keys", {
  config <- list(api_key = "secret-key", product_id = "product-02",
                 base_url = "https://api.example.test")
  requests <- list()
  testthat::local_mocked_bindings(
    .daycent_http_request = function(method, url, headers, fields, verify_ssl) {
      requests[[length(requests) + 1L]] <<- list(method = method, url = url)
      list(status_code = 200,
           body = list(id = "run-04", status = "Running", runType = "Batch"),
           text = "")
    },
    .package = "DDcentutils"
  )

  result <- get_daycent_run(config, "run-04")
  expect_equal(result$status, "Running")
  expect_equal(result$run_type, "Batch")
  expect_equal(requests[[1]]$url, "https://api.example.test/api/ModelRuns/run-04")

  testthat::local_mocked_bindings(
    .daycent_http_request = function(...) {
      list(status_code = 403, body = NULL, text = "bad secret-key")
    },
    .package = "DDcentutils"
  )
  error_message <- tryCatch(get_daycent_run(config, "run-04"), error = function(e) e$message)
  expect_false(grepl("secret-key", error_message, fixed = TRUE))
  expect_true(grepl("redacted", error_message, fixed = TRUE))
})

test_that("watching polls until a successful terminal status", {
  config <- list(api_key = "secret-key", product_id = "product-02",
                 poll_seconds = 0.01)
  statuses <- c("Queued", "Running", "Completed")
  sleeps <- numeric()
  testthat::local_mocked_bindings(
    .daycent_http_request = function(...) {
      status <- statuses[[1L]]
      statuses <<- statuses[-1L]
      list(status_code = 200, body = list(id = "run-05", status = status), text = "")
    },
    .daycent_sleep = function(seconds) sleeps <<- c(sleeps, seconds),
    .daycent_now = function() as.POSIXct(0, origin = "1970-01-01", tz = "UTC"),
    .package = "DDcentutils"
  )

  result <- watch_daycent_run(config, "run-05", timeout_seconds = 10)
  expect_equal(result$status, "Completed")
  expect_equal(sleeps, c(0.01, 0.01))
})

test_that("watching retries transient HTTP failures but stops on authentication failures", {
  config <- list(api_key = "secret-key", product_id = "product-02",
                 poll_seconds = 0.01)
  responses <- list(
    list(status_code = 429, body = NULL, text = "busy"),
    list(status_code = 503, body = NULL, text = "unavailable"),
    list(status_code = 200, body = list(id = "run-06", status = "Completed"), text = "")
  )
  sleeps <- numeric()
  testthat::local_mocked_bindings(
    .daycent_http_request = function(...) {
      response <- responses[[1L]]
      responses <<- responses[-1L]
      response
    },
    .daycent_sleep = function(seconds) sleeps <<- c(sleeps, seconds),
    .daycent_now = function() as.POSIXct(0, origin = "1970-01-01", tz = "UTC"),
    .package = "DDcentutils"
  )
  expect_equal(watch_daycent_run(config, "run-06")$status, "Completed")
  expect_equal(length(sleeps), 2L)

  testthat::local_mocked_bindings(
    .daycent_http_request = function(...) {
      list(status_code = 401, body = NULL, text = "unauthorized")
    },
    .package = "DDcentutils"
  )
  expect_error(watch_daycent_run(config, "run-07"), "HTTP 401")
})

test_that("watching reports terminal failures, unknown statuses, and timeouts", {
  config <- list(api_key = "secret-key", product_id = "product-02",
                 poll_seconds = 0.01)
  testthat::local_mocked_bindings(
    .daycent_http_request = function(...) {
      list(status_code = 200, body = list(id = "run-08", status = "Failed"), text = "")
    },
    .package = "DDcentutils"
  )
  expect_error(watch_daycent_run(config, "run-08"), "run-08.*Failed")

  testthat::local_mocked_bindings(
    .daycent_http_request = function(...) {
      list(status_code = 200, body = list(id = "run-09", status = "Mystery"), text = "")
    },
    .package = "DDcentutils"
  )
  expect_error(watch_daycent_run(config, "run-09"), "unknown status")

  times <- c(0, 11)
  testthat::local_mocked_bindings(
    .daycent_http_request = function(...) {
      list(status_code = 200, body = list(id = "run-10", status = "Running"), text = "")
    },
    .daycent_now = function() {
      value <- times[[1L]]
      times <<- if (length(times) > 1L) times[-1L] else times
      as.POSIXct(value, origin = "1970-01-01", tz = "UTC")
    },
    .daycent_sleep = function(...) NULL,
    .package = "DDcentutils"
  )
  expect_error(watch_daycent_run(config, "run-10", timeout_seconds = 10), "Timed out")
})
