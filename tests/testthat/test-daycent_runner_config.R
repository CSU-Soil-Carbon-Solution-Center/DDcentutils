test_that("exe configuration does not require API credentials", {
  cfg <- daycent_runner_config(backend = "exe")

  expect_equal(cfg$backend, "exe")
  expect_equal(cfg$base_url, "https://api.emdc.eco")
  expect_true(cfg$verify_ssl)
  expect_equal(cfg$poll_seconds, 60)
  expect_equal(cfg$model_name, "DayCent")
  expect_equal(cfg$model_version, "491")
  expect_null(cfg$api_key)
  expect_null(cfg$product_id)
})

test_that("exe configuration validates requested paths", {
  temp_dir <- tempfile("daycent-config-")
  dir.create(temp_dir)
  exe <- file.path(temp_dir, "daycent.exe")
  list_file <- file.path(temp_dir, "100.list")
  file.create(exe, list_file)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  cfg <- daycent_runner_config(backend = "exe", exe_path = exe,
                               dc_path100 = list_file, validate_paths = TRUE)
  expect_equal(cfg$exe_path, exe)
  expect_equal(cfg$dc_path100, list_file)
  expect_error(daycent_runner_config(backend = "exe", validate_paths = TRUE),
               "requires exe_path")
})

test_that("api configuration requires a key but uses model identity defaults", {
  expect_error(daycent_runner_config(backend = "api", api_key = ""),
               "EMDC_API_KEY")

  cfg <- daycent_runner_config(backend = "api", api_key = "secret-value")
  expect_equal(cfg$api_key, "secret-value")
  expect_null(cfg$product_id)
  expect_equal(cfg$model_name, "DayCent")
  expect_equal(cfg$model_version, "491")

  error_message <- tryCatch(
    daycent_runner_config(backend = "api", api_key = "secret-value", model_name = ""),
    error = function(error) error$message
  )
  expect_false(grepl("secret-value", error_message, fixed = TRUE))
})
