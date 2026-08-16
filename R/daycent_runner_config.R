#' Configure a local or API-backed DayCent runner
#'
#' @param backend Character. Either `"exe"` for the local executable or
#'   `"api"` for the EMDC API backend.
#' @param exe_path Character path to the DayCent executable. Required when
#'   `validate_paths = TRUE` for the executable backend.
#' @param dc_path100 Character path to the DayCent `.100` list file. Required
#'   when `validate_paths = TRUE` for the executable backend.
#' @param validate_paths Logical. Validate `exe_path` and `dc_path100` when
#'   using the executable backend.
#' @param base_url Character API base URL. Defaults to the production API.
#' @param verify_ssl Logical. Whether API TLS certificates should be verified.
#' @param poll_seconds Positive numeric polling interval for later API work.
#' @param api_key Character API key. Defaults to `EMDC_API_KEY`.
#' @param product_id Optional DayCent product ID. Defaults to
#'   `EMDC_DAYCENT_PRODUCT_ID` when set; model name and version are the primary
#'   API identity.
#' @param model_name Character API model name. Defaults to `DayCent`.
#' @param model_version Character API model version. Defaults to `491`.
#'
#' @return A named configuration list. The API key is returned for later
#'   authenticated work but is never printed or included in error messages.
#' @export
daycent_runner_config <- function(
    backend = c("exe", "api"),
    exe_path = NULL,
    dc_path100 = NULL,
    validate_paths = FALSE,
    base_url = "https://api.emdc.eco",
    verify_ssl = TRUE,
    poll_seconds = 60,
    api_key = Sys.getenv("EMDC_API_KEY", ""),
    product_id = Sys.getenv("EMDC_DAYCENT_PRODUCT_ID", ""),
    model_name = "DayCent",
    model_version = "491") {
  backend <- match.arg(backend)

  if (length(validate_paths) != 1L || !is.logical(validate_paths) || is.na(validate_paths)) {
    stop("validate_paths must be a single TRUE or FALSE value.", call. = FALSE)
  }
  if (length(verify_ssl) != 1L || !is.logical(verify_ssl) || is.na(verify_ssl)) {
    stop("verify_ssl must be a single TRUE or FALSE value.", call. = FALSE)
  }
  if (length(poll_seconds) != 1L || !is.numeric(poll_seconds) ||
      is.na(poll_seconds) || poll_seconds <= 0) {
    stop("poll_seconds must be a positive number.", call. = FALSE)
  }
  if (length(base_url) != 1L || !is.character(base_url) || is.na(base_url) || !nzchar(base_url)) {
    stop("base_url must be a non-empty character value.", call. = FALSE)
  }
  if (length(model_name) != 1L || !is.character(model_name) || is.na(model_name) || !nzchar(model_name)) {
    stop("model_name must be a non-empty character value.", call. = FALSE)
  }
  if (length(model_version) != 1L || !is.character(model_version) || is.na(model_version) || !nzchar(model_version)) {
    stop("model_version must be a non-empty character value.", call. = FALSE)
  }

  if (backend == "api" &&
      (!is.character(api_key) || length(api_key) != 1L || is.na(api_key) || !nzchar(api_key))) {
    stop("API backend requires EMDC_API_KEY or an explicit api_key.", call. = FALSE)
  }
  if (!is.character(product_id) || length(product_id) != 1L || is.na(product_id)) {
    stop("product_id must be a single character value when supplied.", call. = FALSE)
  }

  if (backend == "exe" && isTRUE(validate_paths)) {
    if (!is.character(exe_path) || length(exe_path) != 1L || !nzchar(exe_path)) {
      stop("validate_paths = TRUE requires exe_path for the executable backend.", call. = FALSE)
    }
    if (!file.exists(exe_path) || isTRUE(file.info(exe_path)$isdir)) {
      stop("DayCent executable path does not exist or is not a file.", call. = FALSE)
    }
    if (!is.character(dc_path100) || length(dc_path100) != 1L || !nzchar(dc_path100)) {
      stop("validate_paths = TRUE requires dc_path100 for the executable backend.", call. = FALSE)
    }
    if (!file.exists(dc_path100) || isTRUE(file.info(dc_path100)$isdir)) {
      stop("DayCent .100 list path does not exist or is not a file.", call. = FALSE)
    }
  }

  list(
    backend = backend,
    exe_path = exe_path,
    dc_path100 = dc_path100,
    base_url = base_url,
    verify_ssl = verify_ssl,
    poll_seconds = poll_seconds,
    api_key = if (backend == "api") api_key else NULL,
    product_id = if (nzchar(product_id)) product_id else NULL,
    model_name = model_name,
    model_version = model_version
  )
}
