#' Run one DayCent site/scenario through the EMDC API
#'
#' Stages one exact `site/scenario` selection, submits it to the ModelRuns API,
#' and optionally watches and downloads the result. The API supports
#' equilibrium through `run_eq`; an independent base-only run is not exposed.
#'
#' @param site Character site name.
#' @param scen Character scenario name.
#' @param run_eq Logical. Request equilibrium/base processing before the scenario.
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
runDayCent_api <- function(site, scen, run_eq = FALSE, config,
                           project_path = ".", name = "daycent-r-api-run",
                           wait = TRUE, keep_zip = FALSE,
                           timeout_seconds = 3600, output_zip = NULL,
                           overwrite = FALSE) {
  .daycent_validate_api_config(config)
  if (!is.null(config$backend) && !identical(config$backend, "api")) {
    stop("runDayCent_api requires an API runner configuration.", call. = FALSE)
  }
  if (!is.character(site) || length(site) != 1L || is.na(site) || !nzchar(site) ||
      !grepl("^[A-Za-z0-9_-]+$", site)) {
    stop("site must be one non-empty API-safe name.", call. = FALSE)
  }
  if (!is.character(scen) || length(scen) != 1L || is.na(scen) || !nzchar(scen) ||
      !grepl("^[A-Za-z0-9_-]+$", scen)) {
    stop("scen must be one non-empty API-safe name.", call. = FALSE)
  }
  if (tolower(scen) %in% c("eq", "base")) {
    stop("scen must not be eq or base; use run_eq for equilibrium stages.", call. = FALSE)
  }
  if (length(run_eq) != 1L || !is.logical(run_eq) || is.na(run_eq) ||
      length(wait) != 1L || !is.logical(wait) || is.na(wait) ||
      length(keep_zip) != 1L || !is.logical(keep_zip) || is.na(keep_zip) ||
      length(overwrite) != 1L || !is.logical(overwrite) || is.na(overwrite)) {
    stop("run_eq, wait, keep_zip, and overwrite must be single TRUE or FALSE values.",
         call. = FALSE)
  }
  if (!is.numeric(timeout_seconds) || length(timeout_seconds) != 1L ||
      is.na(timeout_seconds) || timeout_seconds <= 0) {
    stop("timeout_seconds must be positive.", call. = FALSE)
  }
  if (!is.character(project_path) || length(project_path) != 1L ||
      !dir.exists(project_path)) {
    stop("project_path must name an existing project directory.", call. = FALSE)
  }

  include <- paste(site, scen, sep = "/")
  input_zip <- tempfile("daycent-inputs-", fileext = ".zip")
  on.exit(unlink(input_zip, force = TRUE), add = TRUE)
  zip_daycent_inputs(project_path, include = include, run_eq = run_eq,
                     out_zip = input_zip)
  submission <- submit_daycent_run(
    config = config, input_zip = input_zip, include = include,
    run_eq = run_eq, name = name, site_name = site,
    scenario_name = scen, wait = FALSE
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
