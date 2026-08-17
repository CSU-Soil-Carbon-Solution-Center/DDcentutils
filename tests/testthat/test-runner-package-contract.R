test_that("runner exports and generated documentation agree", {
  package_root <- system.file(package = "DDcentutils")
  if (!nzchar(package_root)) {
    package_root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                                  mustWork = TRUE)
  }
  namespace <- readLines(file.path(package_root, "NAMESPACE"), warn = FALSE)
  description <- readLines(file.path(package_root, "DESCRIPTION"), warn = FALSE)
  expect_true(any(grepl("export\\(DayCentRunSite\\)", namespace, fixed = FALSE)))
  expect_true(any(grepl("export\\(runDayCent_api\\)", namespace, fixed = FALSE)))
  expect_true(any(grepl("Imports:.*httr|^ httr", description)))
  expect_true(file.exists(file.path(package_root, "man", "DayCentRunSite.Rd")))
  expect_true(file.exists(file.path(package_root, "man", "runDayCent_api.Rd")))
  expect_true(any(grepl("DayCentRunSite <- function",
                        readLines(file.path(package_root, "R", "DayCentRunSite.R")),
                        fixed = TRUE)))
  expect_true(any(grepl("runDayCent_api <- function",
                        readLines(file.path(package_root, "R", "runDayCent_api.R")),
                        fixed = TRUE)))
})

test_that("documented API contract stays offline and credential-free", {
  package_root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                                mustWork = TRUE)
  readme_path <- file.path(package_root, "README.Rmd")
  testthat::skip_if_not(file.exists(readme_path),
                        "README.Rmd is source-only and is absent from installed packages")
  readme <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  expect_match(readme, "EMDC_BACKEND_SERVER")
  expect_match(readme, "EMDC_API_KEY")
  expect_match(readme, "EMDC_DAYCENT_PRODUCT_ID")
  expect_match(readme, "product_id")
  expect_match(readme, "wait = FALSE")
  expect_match(readme, "Packet 07")
  expect_match(readme, "d4d00d61fd70f3a32ef2b11504d60e9aa7a8158f")
  expect_false(any(grepl("X-Api-Key: [A-Za-z0-9]", readme)))
  expect_false(any(grepl("dryRun", readme, fixed = TRUE) &
                    grepl("curl|POST", readme, ignore.case = TRUE)))
})

test_that("local API artifacts are ignored", {
  package_root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                                mustWork = TRUE)
  ignore_path <- file.path(package_root, ".gitignore")
  testthat::skip_if_not(file.exists(ignore_path),
                        ".gitignore is repository-only and is absent from installed packages")
  ignore <- readLines(ignore_path, warn = FALSE)
  expect_true(any(ignore == ".Renviron"))
  expect_true(any(ignore == "apiDocs/"))
  expect_true(any(ignore == "run_*.zip"))
})
