#' Run selected DayCent site/scenario pairs through the EMDC API
#'
#' Stages one or more exact `site/scenario` selections, submits one ModelRuns
#' request, and optionally watches and downloads the result. The API represents
#' equilibrium and base as one paired choice; an independent base-only run is
#' not exposed.
#'
#' @param include Character vector of exact `site/scenario` selections. `NULL`,
#'   empty, or `"*"` selects all pairs discovered by the API.
#' @param run_eq Logical. Request equilibrium and base processing before each
#'   selected scenario.
#' @param run_base Logical. Must equal `run_eq` for API runs because the API
#'   exposes one `runEquilibrium` field rather than an independent base flag.
#' @param config API configuration from [daycent_runner_config()].
#' @param project_path Project root containing `sites/` and optionally `libs/`.
#' @param name Descriptive API run name.
#' @param wait Logical. Wait for completion and download results when `TRUE`.
#' @param keep_zip Logical. Retain the downloaded result ZIP.
#' @param timeout_seconds Maximum status-watch duration in seconds.
#' @param output_zip Optional result ZIP destination.
#' @param overwrite Logical. Allow existing result ZIPs and outputs to be replaced.
#'
#' @return With `wait = FALSE`, submission information containing `run_id`.
#'   With `wait = TRUE`, a list containing submission, terminal status, and
#'   extracted result paths.
#' @export
runDayCent_api <- function(include = NULL, run_eq = FALSE, run_base = run_eq, config,
                           project_path = ".", name = "daycent-r-api-run",
                           wait = TRUE, keep_zip = FALSE,
                           timeout_seconds = 3600, output_zip = NULL,
                           overwrite = FALSE) {
  .daycent_validate_api_config(config)
  if (!is.null(config$backend) && !identical(config$backend, "api")) {
    stop("runDayCent_api requires an API runner configuration.", call. = FALSE)
  }
  if (length(run_eq) != 1L || !is.logical(run_eq) || is.na(run_eq) ||
      length(run_base) != 1L || !is.logical(run_base) || is.na(run_base) ||
      length(wait) != 1L || !is.logical(wait) || is.na(wait) ||
      length(keep_zip) != 1L || !is.logical(keep_zip) || is.na(keep_zip) ||
      length(overwrite) != 1L || !is.logical(overwrite) || is.na(overwrite)) {
    stop("run_eq, run_base, wait, keep_zip, and overwrite must be single TRUE or FALSE values.",
         call. = FALSE)
  }
  if (!identical(run_eq, run_base)) {
    stop("API runs require run_eq and run_base to match; base-only execution is local-only.",
         call. = FALSE)
  }
  include <- .daycent_include(include)
  if (!is.numeric(timeout_seconds) || length(timeout_seconds) != 1L ||
      is.na(timeout_seconds) || timeout_seconds <= 0) {
    stop("timeout_seconds must be positive.", call. = FALSE)
  }
  if (!is.character(project_path) || length(project_path) != 1L ||
      !dir.exists(project_path)) {
    stop("project_path must name an existing project directory.", call. = FALSE)
  }

  input_zip <- tempfile("daycent-inputs-", fileext = ".zip")
  on.exit(unlink(input_zip, force = TRUE), add = TRUE)
  zip_daycent_inputs(project_path, include = include, run_eq = run_eq,
                     out_zip = input_zip)
  submission <- submit_daycent_run(
    config = config, input_zip = input_zip, include = include,
    run_eq = run_eq, name = name, wait = FALSE
  )
  if (!isTRUE(wait)) return(submission)

  status <- watch_daycent_run(config, submission$run_id,
                              timeout_seconds = timeout_seconds)
  results <- download_daycent_results(
    config, submission$run_id, project_path, keep_zip = keep_zip,
    output_zip = output_zip, overwrite = overwrite
  )
  list(run_id = submission$run_id, submission = submission,
       status = status, results = results,
       output_paths = results$output_paths,
       metadata_dir = results$metadata_dir,
       zip_path = results$zip_path)
}
