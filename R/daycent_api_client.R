.daycent_now <- function() Sys.time()
.daycent_sleep <- function(seconds) Sys.sleep(seconds)

.daycent_http_request <- function(method, url, headers = list(), fields = list(),
                                   verify_ssl = TRUE) {
  ssl <- if (isTRUE(verify_ssl)) httr::config() else
    httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)
  auth <- httr::add_headers(.headers = headers)
  response <- if (identical(method, "POST")) {
    httr::POST(url, auth, ssl, body = fields, encode = "multipart")
  } else {
    httr::GET(url, auth, ssl)
  }
  text <- httr::content(response, "text", encoding = "UTF-8")
  body <- tryCatch(httr::content(response, "parsed", encoding = "UTF-8"),
                   error = function(e) NULL)
  list(status_code = httr::status_code(response), body = body, text = text)
}

.daycent_config_value <- function(config, name, default = NULL) {
  if (is.null(config) || is.null(config[[name]])) default else config[[name]]
}

.daycent_validate_api_config <- function(config) {
  api_key <- .daycent_config_value(config, "api_key", "")
  product_id <- .daycent_config_value(config, "product_id", "")
  if (!is.character(api_key) || length(api_key) != 1L || is.na(api_key) || !nzchar(api_key)) {
    stop("API submission requires config$api_key.", call. = FALSE)
  }
  if (!is.character(product_id) || length(product_id) != 1L || is.na(product_id) || !nzchar(product_id)) {
    stop("API submission requires config$product_id for the ModelRuns endpoint.", call. = FALSE)
  }
  invisible(TRUE)
}

.daycent_url <- function(config, suffix) {
  base <- sub("/+$", "", .daycent_config_value(config, "base_url", "https://api.emdc.eco"))
  paste0(base, suffix)
}

.daycent_sanitize <- function(text, api_key) {
  if (!is.character(text) || !length(text) || !nzchar(api_key)) return(text)
  gsub(api_key, "<redacted>", text, fixed = TRUE)
}

.daycent_response_error <- function(response, api_key = "") {
  status <- response$status_code %||% NA_integer_
  detail <- response$text %||% "<no response body>"
  sprintf("HTTP %s: %s", status, .daycent_sanitize(detail, api_key))
}

.daycent_first <- function(...) {
  values <- list(...)
  for (value in values) if (!is.null(value) && length(value)) return(value)
  NULL
}

.daycent_parse_run <- function(body, raw_text = "") {
  if (!is.list(body)) stop("Malformed ModelRuns response: expected an object.", call. = FALSE)
  run <- body[["modelRun"]] %||% body[["modelrun"]] %||% body[["run"]] %||% body
  if (!is.list(run)) stop("Malformed ModelRuns response: missing run object.", call. = FALSE)
  run_id <- .daycent_first(run[["id"]], run[["runId"]], run[["modelRunId"]],
                           body[["runId"]], body[["modelRunId"]])
  if (is.null(run_id) || !nzchar(as.character(run_id))) {
    stop(sprintf("ModelRuns response did not contain a run ID: %s", raw_text), call. = FALSE)
  }
  list(
    run_id = as.character(run_id[[1L]]),
    status = as.character(.daycent_first(run[["status"]], body[["status"]]) %||% ""),
    run_type = .daycent_first(run[["type"]], run[["runType"]], run[["modelRunType"]], body[["type"]]),
    parent_id = .daycent_first(run[["parentId"]], run[["parentRunId"]], body[["parentId"]]),
    child_status_counts = .daycent_first(
      run[["childStatusCounts"]], run[["batchChildStatusCounts"]], run[["childrenByStatus"]],
      body[["childStatusCounts"]], body[["batchChildStatusCounts"]]
    ),
    raw = body
  )
}

.daycent_include <- function(include) {
  if (is.null(include) || !length(include) || identical(include, "*")) return(NULL)
  if (!is.character(include) || any(!nzchar(include))) {
    stop("include must be NULL, '*', or non-empty site/scenario strings.", call. = FALSE)
  }
  if (any(!grepl("^[A-Za-z0-9_-]+/[A-Za-z0-9_-]+$", include))) {
    stop("include values must have the form site/scenario.", call. = FALSE)
  }
  if (any(grepl("/(eq|base)$", include, ignore.case = TRUE))) {
    stop("include must not contain eq or base; use run_eq for equilibrium stages.", call. = FALSE)
  }
  include
}

.daycent_fields <- function(input_zip, config, include, run_eq, name,
                            site_name, scenario_name, organization_membership_id,
                            dry_run, dry_run_mode) {
  fields <- list(
    inputZip = httr::upload_file(input_zip, type = "application/zip"),
    productId = config$product_id,
    name = name,
    runEquilibrium = tolower(as.character(run_eq))
  )
  if (length(include)) fields <- c(fields, stats::setNames(as.list(include), rep("include", length(include))))
  if (!is.null(site_name)) fields$siteName <- site_name
  if (!is.null(scenario_name)) fields$scenarioName <- scenario_name
  if (!is.null(organization_membership_id)) fields$organizationMembershipId <- organization_membership_id
  if (!is.null(dry_run)) fields$dryRun <- tolower(as.character(dry_run))
  if (!is.null(dry_run_mode)) fields$dryRunMode <- dry_run_mode
  fields
}

#' Submit a DayCent ModelRuns request
#'
#' @param config A configuration list from [daycent_runner_config()]. Its API
#'   key and product ID are required for submission.
#' @param input_zip Path to an existing ZIP archive.
#' @param include Character vector of site/scenario selectors. `NULL`, empty,
#'   or `"*"` lets the API select all discovered pairs.
#' @param run_eq Logical value sent as the API's `runEquilibrium` field.
#' @param name Descriptive run name.
#' @param site_name Optional API site name.
#' @param scenario_name Optional API scenario name.
#' @param organization_membership_id Optional organization membership ID.
#' @param dry_run Optional API dry-run flag.
#' @param dry_run_mode Optional API dry-run mode.
#' @param wait Logical. If `FALSE`, return after submission without polling.
#'
#' @return A parsed submission result including `run_id`, initial `status`,
#'   run type, parent ID, and child status counts when supplied by the server.
#' @export
submit_daycent_run <- function(config, input_zip, include = NULL, run_eq = FALSE,
                               name = "daycent-r-api-run", site_name = NULL,
                               scenario_name = NULL, organization_membership_id = NULL,
                               dry_run = NULL, dry_run_mode = NULL, wait = FALSE) {
  .daycent_validate_api_config(config)
  if (!is.character(input_zip) || length(input_zip) != 1L || !file.exists(input_zip)) {
    stop("input_zip must name an existing ZIP file.", call. = FALSE)
  }
  if (length(run_eq) != 1L || !is.logical(run_eq) || is.na(run_eq)) {
    stop("run_eq must be a single TRUE or FALSE value.", call. = FALSE)
  }
  include <- .daycent_include(include)
  fields <- .daycent_fields(input_zip, config, include, run_eq, name,
                            site_name, scenario_name, organization_membership_id,
                            dry_run, dry_run_mode)
  response <- .daycent_http_request(
    "POST", .daycent_url(config, "/api/ModelRuns"),
    headers = list(`X-Api-Key` = config$api_key), fields = fields,
    verify_ssl = .daycent_config_value(config, "verify_ssl", TRUE)
  )
  if (response$status_code < 200L || response$status_code >= 300L) {
    stop(paste("DayCent submission failed:", .daycent_response_error(response, config$api_key)), call. = FALSE)
  }
  result <- .daycent_parse_run(response$body, response$text)
  if (isTRUE(wait)) {
    return(watch_daycent_run(config, result$run_id))
  }
  result
}

#' Get a DayCent ModelRuns status
#'
#' @param config API configuration from [daycent_runner_config()].
#' @param run_id Model run ID.
#' @return A parsed status result preserving server fields.
#' @export
get_daycent_run <- function(config, run_id) {
  .daycent_validate_api_config(config)
  if (!is.character(run_id) || length(run_id) != 1L || !nzchar(run_id)) {
    stop("run_id must be a non-empty character value.", call. = FALSE)
  }
  response <- .daycent_http_request(
    "GET", .daycent_url(config, paste0("/api/ModelRuns/", run_id)),
    headers = list(`X-Api-Key` = config$api_key),
    verify_ssl = .daycent_config_value(config, "verify_ssl", TRUE)
  )
  if (response$status_code < 200L || response$status_code >= 300L) {
    stop(paste("DayCent status request failed:", .daycent_response_error(response, config$api_key)), call. = FALSE)
  }
  .daycent_parse_run(response$body, response$text)
}

#' Watch a DayCent ModelRuns status until completion
#'
#' @param config API configuration from [daycent_runner_config()].
#' @param run_id Model run ID.
#' @param timeout_seconds Maximum watch duration.
#' @param max_consecutive_failures Maximum retryable HTTP failures in a row.
#' @return The terminal parsed status result.
#' @export
watch_daycent_run <- function(config, run_id, timeout_seconds = 3600,
                              max_consecutive_failures = 3L) {
  .daycent_validate_api_config(config)
  if (!is.numeric(timeout_seconds) || length(timeout_seconds) != 1L ||
      is.na(timeout_seconds) || timeout_seconds <= 0) {
    stop("timeout_seconds must be positive.", call. = FALSE)
  }
  if (!is.numeric(max_consecutive_failures) || length(max_consecutive_failures) != 1L ||
      is.na(max_consecutive_failures) || max_consecutive_failures < 1) {
    stop("max_consecutive_failures must be positive.", call. = FALSE)
  }
  start <- .daycent_now()
  failures <- 0L
  success <- c("Completed", "CompletedWithErrors", "PartiallyExecuted")
  terminal <- c(success, "Failed", "Canceled", "Cancelled", "ValidationFailed",
                "InsufficientCredits", "NotRun")

  repeat {
    result <- tryCatch(get_daycent_run(config, run_id), error = function(error) error)
    if (inherits(result, "error")) {
      message_text <- conditionMessage(result)
      retryable <- grepl("HTTP (429|5[0-9][0-9])", message_text)
      if (!retryable) stop(message_text, call. = FALSE)
      failures <- failures + 1L
      if (failures >= max_consecutive_failures) {
        stop(sprintf("Run %s status retries exhausted after %d failures: %s",
                     run_id, failures, message_text), call. = FALSE)
      }
    } else {
      failures <- 0L
      if (!nzchar(result$status)) stop("Malformed status response: missing status.", call. = FALSE)
      if (result$status %in% terminal) {
        if (!(result$status %in% success)) {
          stop(sprintf("Run %s ended with status '%s'.", run_id, result$status), call. = FALSE)
        }
        return(result)
      }
      if (!(result$status %in% c("Queued", "Staging", "Starting", "Running"))) {
        stop(sprintf("Run %s returned unknown status '%s'.", run_id, result$status), call. = FALSE)
      }
    }
    if (as.numeric(difftime(.daycent_now(), start, units = "secs")) >= timeout_seconds) {
      stop(sprintf("Timed out watching run %s after %s seconds.", run_id, timeout_seconds), call. = FALSE)
    }
    .daycent_sleep(.daycent_config_value(config, "poll_seconds", 60))
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
