test_that("zip_daycent_inputs keeps flat DayCent inputs and optional libs", {
  project <- tempfile("daycent-zip-")
  dir.create(file.path(project, "sites", "wooster"), recursive = TRUE)
  dir.create(file.path(project, "sites", "wooster", "nested"))
  dir.create(file.path(project, "libs"))
  file.create(
    file.path(project, "sites", "wooster", "wooster_eq.sch"),
    file.path(project, "sites", "wooster", "wooster_site.100"),
    file.path(project, "sites", "wooster", "weather.wth"),
    file.path(project, "sites", "wooster", "nested", "extra.in"),
    file.path(project, "sites", "wooster", "out_daily.csv"),
    file.path(project, "sites", "wooster", "status.json"),
    file.path(project, "sites", "wooster", "run.log"),
    file.path(project, "sites", "wooster", "old.zip"),
    file.path(project, "libs", "crop.100")
  )
  archive <- file.path(project, "upload.zip")
  old_wd <- getwd()
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  result <- zip_daycent_inputs(project, "wooster", archive)

  expect_true(file.exists(result))
  expect_equal(basename(result), "upload.zip")
  expect_equal(normalizePath(getwd(), winslash = "/"), normalizePath(old_wd, winslash = "/"))
  entries <- gsub("\\\\", "/", utils::unzip(archive, list = TRUE)$Name)
  expect_true(all(grepl("^(sites/wooster/|libs/)", entries)))
  expect_true(all(c("sites/wooster/wooster_eq.sch", "sites/wooster/wooster_site.100",
                    "sites/wooster/weather.wth", "sites/wooster/nested/extra.in",
                    "libs/crop.100") %in% entries))
  expect_false(any(grepl("out_daily|status|run\\.log|old\\.zip", entries)))
})

test_that("zip_daycent_inputs rejects missing and empty input sets", {
  project <- tempfile("daycent-zip-")
  dir.create(file.path(project, "sites", "empty"), recursive = TRUE)
  archive <- file.path(project, "empty.zip")
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  expect_error(zip_daycent_inputs(project, "missing", archive), "Site directory not found")
  expect_error(zip_daycent_inputs(project, "empty", archive), "No DayCent input files")
  expect_false(file.exists(archive))
})
