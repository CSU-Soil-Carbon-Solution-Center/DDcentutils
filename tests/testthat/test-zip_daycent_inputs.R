test_that("zip_daycent_inputs supports batch include and run_eq selection", {
  project <- tempfile("daycent-zip-")
  for (site in c("wooster", "fortcollins")) {
    dir.create(file.path(project, "sites", site, "outputs"), recursive = TRUE)
    file.create(
      file.path(project, "sites", site, paste0(site, "_site.100")),
      file.path(project, "sites", site, "weather.wth"),
      file.path(project, "sites", site, paste0(site, "_eq.sch")),
      file.path(project, "sites", site, paste0(site, "_base.sch")),
      file.path(project, "sites", site, paste0(site, "_cb_ct.sch")),
      file.path(project, "sites", site, paste0(site, "_corn.sch")),
      file.path(project, "sites", site, "status.json"),
      file.path(project, "sites", site, "outputs", "summary.out")
    )
  }
  dir.create(file.path(project, "libs"))
  file.create(file.path(project, "libs", "crop.100"))
  archive <- file.path(project, "upload.zip")
  old_wd <- getwd()
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  result <- zip_daycent_inputs(
    project,
    include = c("wooster/cb_ct", "fortcollins/corn"),
    run_eq = TRUE,
    out_zip = archive
  )

  expect_true(file.exists(result))
  expect_equal(basename(result), "upload.zip")
  expect_equal(normalizePath(getwd(), winslash = "/"), normalizePath(old_wd, winslash = "/"))
  entries <- gsub("\\\\", "/", utils::unzip(archive, list = TRUE)$Name)
  expect_true(all(grepl("^(sites/(wooster|fortcollins)/|libs/)", entries)))
  expect_true(all(c(
    "sites/wooster/wooster_site.100", "sites/wooster/weather.wth",
    "sites/wooster/wooster_eq.sch", "sites/wooster/wooster_base.sch",
    "sites/wooster/wooster_cb_ct.sch", "sites/fortcollins/fortcollins_corn.sch",
    "sites/fortcollins/fortcollins_eq.sch", "sites/fortcollins/fortcollins_base.sch",
    "libs/crop.100"
  ) %in% entries))
  expect_false(any(grepl("_corn\\.sch$|_cb_ct\\.sch$", entries) &
                   !grepl("wooster_cb_ct|fortcollins_corn", entries)))
  expect_true("sites/wooster/status.json" %in% entries)
  expect_false(any(grepl("/outputs/", entries, fixed = TRUE)))
})

test_that("zip_daycent_inputs supports all-site selection and rejects missing inputs", {
  project <- tempfile("daycent-zip-")
  dir.create(file.path(project, "sites", "empty"), recursive = TRUE)
  archive <- file.path(project, "empty.zip")
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  expect_error(zip_daycent_inputs(project, include = "missing/scenario", out_zip = archive),
               "Site input directory not found")
  expect_error(zip_daycent_inputs(project, include = "empty/scenario", out_zip = archive),
               "No DayCent input files")
  expect_false(file.exists(archive))
})
