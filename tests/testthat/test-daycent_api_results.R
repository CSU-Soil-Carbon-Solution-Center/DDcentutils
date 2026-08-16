make_result_zip <- function(entries, contents = character()) {
  root <- tempfile("daycent-result-zip-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  if (!length(contents)) contents <- rep("result", length(entries))
  for (index in seq_along(entries)) {
    path <- file.path(root, entries[[index]])
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(contents[[index]], path)
  }
  archive <- tempfile("daycent-result-", fileext = ".zip")
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(archive, files = entries, flags = "-q")
  archive
}

result_config <- function() {
  list(api_key = "secret-key", product_id = "product-02",
       base_url = "https://api.example.test", verify_ssl = TRUE)
}

test_that("safe extraction maps outputs and archives API metadata", {
  zip_file <- make_result_zip(c(
    "status.json", "run_config.json", "input_qc.json", "input_qc.md",
    "sites/siteA/scenario1/siteA_summary.out",
    "sites/siteA/scenario1/siteA_table.csv",
    "sites/siteA/scenario1/status.json",
    "sites/siteA/scenario1/siteA_site.100",
    "sites/siteA/scenario1/notes.txt"
  ))
  project <- tempfile("daycent-project-")
  dir.create(file.path(project, "sites", "siteA"), recursive = TRUE)
  input <- file.path(project, "sites", "siteA", "siteA_site.100")
  writeLines("original input", input)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  result <- unzip_daycent_results(zip_file, project, run_id = "run-123")

  expect_true(all(file.exists(result$output_paths)))
  expect_true(file.exists(file.path(project, "sites", "siteA", "outputs",
                                    "scenario1", "siteA_summary.out")))
  expect_equal(readLines(input), "original input")
  expect_true(dir.exists(result$metadata_dir))
  expect_true(all(file.exists(result$metadata_paths)))
  expect_true(file.exists(file.path(result$metadata_dir, "sites/siteA/scenario1/status.json")))
  expect_false(file.exists(file.path(project, "sites", "siteA", "outputs",
                                      "scenario1", "notes.txt")))
})

test_that("unsafe ZIP entries are rejected before extraction", {
  root <- tempfile("daycent-unsafe-")
  dir.create(file.path(root, "sub"), recursive = TRUE)
  writeLines("escape", file.path(root, "escape.out"))
  old_wd <- setwd(file.path(root, "sub"))
  on.exit(setwd(old_wd), add = TRUE)
  archive <- tempfile("daycent-unsafe-", fileext = ".zip")
  utils::zip(archive, "../escape.out", flags = "-q")
  project <- tempfile("daycent-project-")
  dir.create(project)
  on.exit(unlink(c(root, project), recursive = TRUE), add = TRUE)

  expect_error(unzip_daycent_results(archive, project), "Unsafe ZIP entry")
  expect_false(file.exists(file.path(project, "escape.out")))
})

test_that("output overwrite is explicit and corrupt or empty archives fail", {
  project <- tempfile("daycent-project-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  zip_file <- make_result_zip("sites/siteA/scenario1/result.out", "first")

  unzip_daycent_results(zip_file, project)
  expect_error(unzip_daycent_results(zip_file, project), "overwrite")
  writeLines("second", zip_file)
  expect_error(unzip_daycent_results(zip_file, project), "Could not read result ZIP")

  empty_zip <- make_result_zip("status.json")
  expect_error(unzip_daycent_results(empty_zip, project), "no recognized DayCent outputs")
})

test_that("download extracts results, cleans temporary ZIPs, and supports retention", {
  source_zip <- make_result_zip(c("status.json", "sites/siteA/scenario1/result.out"))
  project <- tempfile("daycent-project-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  captured <- list()
  testthat::local_mocked_bindings(
    .daycent_result_download_request = function(url, headers, output_zip, verify_ssl) {
      captured <<- list(url = url, headers = headers, output_zip = output_zip,
                        verify_ssl = verify_ssl)
      file.copy(source_zip, output_zip, overwrite = TRUE)
      list(status_code = 200L, text = "")
    },
    .package = "DDcentutils"
  )

  result <- download_daycent_results(result_config(), "run-456", project)
  expect_equal(result$run_id, "run-456")
  expect_null(result$zip_path)
  expect_false(file.exists(captured$output_zip))
  expect_equal(captured$url, "https://api.example.test/api/ModelRuns/run-456/download")
  expect_equal(captured$headers$`X-Api-Key`, "secret-key")

  retained <- file.path(project, "retained.zip")
  result <- download_daycent_results(result_config(), "run-456", project,
                                     keep_zip = TRUE, output_zip = retained,
                                     overwrite = TRUE)
  expect_equal(normalizePath(retained, winslash = "/"), result$zip_path)
  expect_true(file.exists(retained))
})

test_that("download failures clean temporary files and redact API keys", {
  project <- tempfile("daycent-project-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  captured_path <- NULL
  testthat::local_mocked_bindings(
    .daycent_result_download_request = function(url, headers, output_zip, verify_ssl) {
      captured_path <<- output_zip
      list(status_code = 500L, text = "secret-key backend failure")
    },
    .package = "DDcentutils"
  )
  error_message <- tryCatch(
    download_daycent_results(result_config(), "run-500", project),
    error = function(error) error$message
  )
  expect_false(grepl("secret-key", error_message, fixed = TRUE))
  expect_true(grepl("redacted", error_message, fixed = TRUE))
  expect_false(file.exists(captured_path))
})
