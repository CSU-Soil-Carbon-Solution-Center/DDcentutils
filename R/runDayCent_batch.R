#' Run selected DayCent site/scenario pairs through a local or API backend
#'
#' @param project_path Project root containing `sites/`.
#' @param include Exact `site/scenario` selectors. `NULL`, empty, or `"*"`
#'   selects all discovered pairs.
#' @param sites Optional site names for an explicit Cartesian selection.
#' @param scenarios Optional scenario names for an explicit Cartesian selection.
#' @param backend Either `"exe"` or `"api"`. When omitted, uses
#'   `config$backend` when available and otherwise defaults to `"exe"`.
#' @param config Optional runner configuration.
#' @param run_eq Logical. Run equilibrium and base phases for every task.
#' @param run_base Logical. Local-only base phase flag; API runs must match
#'   `run_eq`.
#' @param name API run name.
#' @param wait Logical. Wait for API completion; local runs always execute.
#' @param keep_zip Logical. Retain an API result ZIP.
#' @param timeout_seconds API watch timeout.
#' @param output_zip Optional API result ZIP path.
#' @param overwrite Logical. Allow API outputs and ZIPs to be replaced.
#' @param workers Local worker count. Defaults to sequential execution.
#' @param error_policy Either `"stop"` or `"collect"` for local failures.
#' @param executor Optional injected parallel executor with signature
#'   `function(tasks, worker, workers)`.
#' @param parent_run_id Optional API parent run ID to resume.
#' @param ... Arguments forwarded to the atomic local runner.
#'
#' @return A list with `task_table`, `backend`, and parent API metadata when
#'   applicable. Task rows preserve selector order and contain site, scenario,
#'   status, parent/child IDs, output path, and error.
#' @export
runDayCent_batch <- function(
    project_path = ".", include = NULL, sites = NULL, scenarios = NULL,
    backend = NULL, config = NULL, run_eq = FALSE,
    run_base = run_eq, name = "daycent-r-api-batch", wait = TRUE,
    keep_zip = FALSE, timeout_seconds = 3600, output_zip = NULL,
    overwrite = FALSE, workers = 1L, error_policy = c("stop", "collect"),
    executor = NULL, parent_run_id = NULL, ...) {
  if (is.null(backend)) backend <- if (is.null(config)) "exe" else config$backend %||% "exe"
  backend <- match.arg(backend, c("exe", "api"))
  if (!is.null(config$backend) && !identical(backend, config$backend)) {
    stop("backend must agree with config$backend.", call. = FALSE)
  }
  error_policy <- match.arg(error_policy)
  project_path <- .daycent_batch_project(project_path)
  pairs <- .daycent_batch_select(project_path, include, sites, scenarios)
  .daycent_batch_flags(run_eq, run_base, wait, keep_zip, overwrite)
  if (!is.numeric(workers) || length(workers) != 1L || is.na(workers) ||
      workers < 1 || workers != as.integer(workers)) {
    stop("workers must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(timeout_seconds) || length(timeout_seconds) != 1L ||
      is.na(timeout_seconds) || timeout_seconds <= 0) {
    stop("timeout_seconds must be positive.", call. = FALSE)
  }
  if (!is.null(parent_run_id) &&
      (!is.character(parent_run_id) || length(parent_run_id) != 1L ||
       !nzchar(parent_run_id))) {
    stop("parent_run_id must be NULL or a non-empty character value.", call. = FALSE)
  }

  if (identical(backend, "api")) {
    return(.daycent_batch_api(
      project_path, pairs, include, config, run_eq, run_base, name, wait,
      keep_zip, timeout_seconds, output_zip, overwrite, parent_run_id
    ))
  }
  if (!is.null(parent_run_id)) stop("parent_run_id is only valid for backend = 'api'.", call. = FALSE)
  task_table <- .daycent_batch_local(project_path, pairs, config, run_eq, run_base,
                                     workers, error_policy, executor, overwrite, ...)
  list(backend = "exe", task_table = task_table)
}

.daycent_batch_project <- function(project_path) {
  if (!is.character(project_path) || length(project_path) != 1L ||
      !dir.exists(project_path)) {
    stop("project_path must name an existing project directory.", call. = FALSE)
  }
  normalizePath(project_path, winslash = "/", mustWork = TRUE)
}

.daycent_batch_flags <- function(run_eq, run_base, wait, keep_zip, overwrite) {
  values <- list(run_eq = run_eq, run_base = run_base, wait = wait,
                 keep_zip = keep_zip, overwrite = overwrite)
  if (any(!vapply(values, function(x) length(x) == 1L && is.logical(x) && !is.na(x), logical(1)))) {
    stop("run_eq, run_base, wait, keep_zip, and overwrite must be single TRUE or FALSE values.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.daycent_batch_discover <- function(project_path) {
  root <- file.path(project_path, "sites")
  if (!dir.exists(root)) stop("Project sites directory not found: sites", call. = FALSE)
  site_names <- sort(list.files(root, full.names = FALSE, recursive = FALSE))
  site_names <- site_names[file.info(file.path(root, site_names))$isdir]
  pairs <- lapply(site_names, function(site) {
    files <- list.files(file.path(root, site), full.names = FALSE, recursive = FALSE)
    prefix <- paste0("^", site, "_([A-Za-z0-9_-]+)\\.sch$")
    scenarios <- sub(prefix, "\\1", files[grepl(prefix, files)])
    scenarios <- sort(unique(scenarios[!tolower(scenarios) %in% c("eq", "base")]))
    if (!length(scenarios)) return(NULL)
    data.frame(site = site, scenario = scenarios, stringsAsFactors = FALSE)
  })
  pairs <- do.call(rbind, pairs)
  if (is.null(pairs) || !nrow(pairs)) stop("No DayCent site/scenario pairs were discovered.", call. = FALSE)
  rownames(pairs) <- NULL
  pairs
}

.daycent_batch_select <- function(project_path, include, sites, scenarios) {
  discovered <- .daycent_batch_discover(project_path)
  cartesian <- !is.null(sites) || !is.null(scenarios)
  if (cartesian && (!is.character(sites) || !length(sites) ||
                    !is.character(scenarios) || !length(scenarios))) {
    stop("sites and scenarios must both be non-empty character vectors for Cartesian selection.", call. = FALSE)
  }
  if (cartesian && !is.null(include) && length(include)) {
    stop("Use include or sites/scenarios, not both.", call. = FALSE)
  }
  if (cartesian) {
    if (any(!grepl("^[A-Za-z0-9_-]+$", sites)) ||
        any(!grepl("^[A-Za-z0-9_-]+$", scenarios)) ||
        any(tolower(scenarios) %in% c("eq", "base"))) {
      stop("Cartesian sites/scenarios contain an invalid or reserved name.", call. = FALSE)
    }
    requested <- expand.grid(site = sites, scenario = scenarios,
                             stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    return(.daycent_batch_match(discovered, requested))
  }
  if (is.null(include) || !length(include) || identical(include, "*")) return(discovered)
  if (!is.character(include) || any(!nzchar(include)) || any(include == "*")) {
    stop("include must be NULL, '*', or exact site/scenario values; '*' cannot be combined with other selectors.", call. = FALSE)
  }
  if (any(!grepl("^[A-Za-z0-9_-]+/[A-Za-z0-9_-]+$", include))) {
    stop("include values must be exact site/scenario pairs.", call. = FALSE)
  }
  requested <- do.call(rbind, lapply(include, function(value) {
    parts <- strsplit(value, "/", fixed = TRUE)[[1L]]
    if (tolower(parts[[2L]]) %in% c("eq", "base")) {
      stop("eq and base are reserved phase names and cannot be selected.", call. = FALSE)
    }
    data.frame(site = parts[[1L]], scenario = parts[[2L]], stringsAsFactors = FALSE)
  }))
  if (anyDuplicated(paste(requested$site, requested$scenario, sep = "/"))) {
    stop("include contains duplicate site/scenario pairs.", call. = FALSE)
  }
  .daycent_batch_match(discovered, requested)
}

.daycent_batch_match <- function(discovered, requested) {
  keys <- paste(discovered$site, discovered$scenario, sep = "/")
  requested_keys <- paste(requested$site, requested$scenario, sep = "/")
  missing <- requested_keys[!requested_keys %in% keys]
  if (length(missing)) stop(sprintf("Requested site/scenario pair(s) not found: %s",
                                    paste(missing, collapse = ", ")), call. = FALSE)
  discovered[match(requested_keys, keys), , drop = FALSE]
}

.daycent_batch_row <- function(task, backend, status, parent = NA_character_,
                               child = NA_character_, output = NA_character_,
                               error = NA_character_) {
  data.frame(site = task$site, scenario = task$scenario, backend = backend,
             status = status, parent_run_id = parent, child_run_id = child,
             output_path = output, error = error, stringsAsFactors = FALSE)
}

.daycent_batch_local <- function(project_path, pairs, config, run_eq, run_base,
                                 workers, error_policy, executor, overwrite, ...) {
  tasks <- split(pairs, seq_len(nrow(pairs)))
  execute <- function(task, root = project_path) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(file.path(root, "sites", task$site))
    result <- tryCatch({
      do.call(DayCentRunSite, c(list(site = task$site, scen = task$scenario,
                                     run_eq = run_eq, run_base = run_base,
                                     backend = "exe", config = config), list(...)))
      if (!identical(root, project_path)) {
        source_out <- file.path(root, "sites", task$site, "outputs", task$scenario)
        target_out <- file.path(project_path, "sites", task$site, "outputs", task$scenario)
        if (dir.exists(source_out)) {
          files <- list.files(source_out, full.names = TRUE, recursive = TRUE,
                              all.files = TRUE, no.. = TRUE)
          for (source in files[file.info(files)$isdir %in% FALSE]) {
            relative <- substring(source, nchar(source_out) + 2L)
            destination <- file.path(target_out, relative)
            dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
            if (!file.copy(source, destination, overwrite = isTRUE(overwrite))) {
              stop(sprintf("Could not collect isolated output: %s", destination), call. = FALSE)
            }
          }
        }
      }
      .daycent_batch_row(task, "exe", "Completed", output = file.path(
        project_path, "sites", task$site, "outputs", task$scenario))
    }, error = function(error) {
      .daycent_batch_row(task, "exe", "Failed", error = conditionMessage(error))
    })
    result
  }
  if (workers > 1L) {
    if (!is.function(executor)) stop("workers > 1 requires an injected executor.", call. = FALSE)
    roots <- lapply(seq_along(tasks), function(i) tempfile(paste0("daycent-batch-", i, "-")))
    for (root in roots) {
      dir.create(root, recursive = TRUE)
      files <- list.files(project_path, all.files = TRUE, full.names = TRUE,
                          no.. = TRUE)
      file.copy(files, root, recursive = TRUE)
    }
    on.exit(unlink(unlist(roots), recursive = TRUE, force = TRUE), add = TRUE)
    rows <- executor(tasks, function(task, index) execute(task, roots[[index]]), workers)
    return(do.call(rbind, rows))
  }
  rows <- list()
  for (i in seq_along(tasks)) {
    row <- execute(tasks[[i]])
    rows[[length(rows) + 1L]] <- row
    if (identical(error_policy, "stop") && identical(row$status, "Failed")) {
      if (i < length(tasks)) rows <- c(rows, lapply(tasks[(i + 1L):length(tasks)],
        function(task) .daycent_batch_row(task, "exe", "NotRun",
                                           error = "Not run after an earlier task failed.")))
      break
    }
  }
  do.call(rbind, rows)
}

.daycent_batch_api <- function(project_path, pairs, include, config, run_eq,
                               run_base, name, wait, keep_zip,
                               timeout_seconds, output_zip, overwrite,
                               parent_run_id) {
  .daycent_validate_api_config(config)
  selectors <- paste(pairs$site, pairs$scenario, sep = "/")
  if (!is.null(parent_run_id)) {
    parent <- if (isTRUE(wait)) watch_daycent_run(config, parent_run_id,
                                                  timeout_seconds = timeout_seconds)
              else get_daycent_run(config, parent_run_id)
    results <- if (isTRUE(wait)) download_daycent_results(
      config, parent_run_id, project_path, keep_zip, output_zip, overwrite) else NULL
  } else {
    parent <- runDayCent_api(include = selectors, run_eq = run_eq,
                             run_base = run_base, config = config,
                             project_path = project_path, name = name,
                             wait = wait, keep_zip = keep_zip,
                             timeout_seconds = timeout_seconds,
                             output_zip = output_zip, overwrite = overwrite)
    if (!isTRUE(wait)) {
      return(list(backend = "api", parent_run_id = parent$run_id,
                  parent = parent,
                  task_table = do.call(rbind, lapply(split(pairs, seq_len(nrow(pairs))),
                    function(task) .daycent_batch_row(task, "api", "Submitted",
                                                       parent = parent$run_id)))))
    }
    results <- parent$results
  }
  children <- .daycent_batch_child_records(config, parent_run_id %||% parent$run_id,
                                            pairs, project_path)
  if (!is.null(results)) children$output_path <- file.path(
    project_path, "sites", children$site, "outputs", children$scenario)
  list(backend = "api", parent_run_id = parent_run_id %||% parent$run_id,
       parent = parent, results = results, task_table = children)
}

.daycent_batch_child_records <- function(config, parent_run_id, pairs, project_path) {
  response <- .daycent_http_request(
    "GET", .daycent_url(config, "/api/ModelRuns?types=BatchRun"),
    headers = list(`X-Api-Key` = config$api_key),
    verify_ssl = .daycent_config_value(config, "verify_ssl", TRUE))
  if (response$status_code < 200L || response$status_code >= 300L) {
    stop(paste("Batch child status request failed:",
               .daycent_response_error(response, config$api_key)), call. = FALSE)
  }
  raw <- response$body
  records <- if (is.list(raw) && !is.null(raw$items)) raw$items else
    if (is.list(raw) && !is.null(raw$modelRuns)) raw$modelRuns else raw
  if (is.list(records) && !is.null(records$id)) records <- list(records)
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    task <- pairs[i, , drop = FALSE]
    match_record <- Filter(function(record) {
      if (!is.list(record)) return(FALSE)
      parent <- record$parentId %||% record$parentRunId
      identical(as.character(parent), as.character(parent_run_id)) &&
        identical(as.character(record$siteName %||% record$site), task$site) &&
        identical(as.character(record$scenarioName %||% record$scenario), task$scenario)
    }, records)
    if (!length(match_record)) return(.daycent_batch_row(
      task, "api", "Unknown", parent = parent_run_id,
      error = "No child record matched this site/scenario."))
    record <- match_record[[1L]]
    .daycent_batch_row(task, "api", as.character(record$status %||% "Unknown"),
                       parent = parent_run_id,
                       child = as.character(record$id %||% record$runId),
                       output = file.path(project_path, "sites", task$site,
                                          "outputs", task$scenario))
  })
  do.call(rbind, rows)
}
