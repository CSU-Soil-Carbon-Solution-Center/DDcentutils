.daycent_result_download_request <- function(url, headers, output_zip, verify_ssl = TRUE) {
  ssl <- if (isTRUE(verify_ssl)) httr::config() else
    httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)
  response <- httr::GET(url, httr::add_headers(.headers = headers), ssl,
                        httr::write_disk(output_zip, overwrite = TRUE))
  list(status_code = httr::status_code(response),
       text = httr::content(response, "text", encoding = "UTF-8"))
}

.daycent_result_entries <- function(zip_file) {
  entries <- tryCatch(utils::unzip(zip_file, list = TRUE)$Name,
                       error = function(error) {
                         stop("Could not read result ZIP: ", conditionMessage(error), call. = FALSE)
                       })
  entries <- gsub("\\\\", "/", entries)
  for (entry in entries) {
    if (grepl("^/|^[A-Za-z]:", entry) ||
        any(strsplit(entry, "/", fixed = TRUE)[[1L]] == "..")) {
      stop(sprintf("Unsafe ZIP entry rejected: %s", entry), call. = FALSE)
    }
  }
  entries[!grepl("/$", entries) & nzchar(entries)]
}

.daycent_result_output <- function(entry) {
  parts <- strsplit(entry, "/", fixed = TRUE)[[1L]]
  if (length(parts) < 4L) return(NULL)
  if (identical(parts[[1L]], "sites")) {
    site <- parts[[2L]]
    scenario <- parts[[3L]]
    relative <- parts[-c(1:3)]
  } else if (identical(parts[[1L]], "outputs")) {
    site <- parts[[2L]]
    scenario <- parts[[3L]]
    relative <- parts[-c(1:3)]
  } else {
    return(NULL)
  }
  if (identical(relative[[1L]], "outputs")) relative <- relative[-1L]
  if (!length(relative) || any(!nzchar(c(site, scenario, relative)))) return(NULL)
  if (!grepl("\\.(out|csv|lis|bin)$", relative[[length(relative)]], ignore.case = TRUE)) {
    return(NULL)
  }
  list(site = site, scenario = scenario, relative = paste(relative, collapse = "/"))
}

.daycent_result_metadata <- function(entry) {
  basename <- basename(entry)
  basename %in% c("status.json", "run_config.json", "input_qc.json", "input_qc.md")
}

.daycent_metadata_dir <- function(project_path, run_id) {
  root <- file.path(project_path, "apiDocs")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  prefix <- paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_", substr(run_id %||% "run", 1L, 8L))
  archive <- file.path(root, prefix)
  suffix <- 1L
  while (file.exists(archive)) {
    archive <- file.path(root, paste0(prefix, "_", suffix))
    suffix <- suffix + 1L
  }
  dir.create(archive, recursive = TRUE, showWarnings = FALSE)
  archive
}

#' Safely extract DayCent API results
#'
#' @param zip_file Existing result ZIP archive.
#' @param project_path Project root containing the `sites/` directory.
#' @param run_id Optional API run ID used to name the metadata archive.
#' @param overwrite Logical. Allow existing output files to be replaced.
#' @return A list containing output paths and the retained metadata archive path.
#' @export
unzip_daycent_results <- function(zip_file, project_path, run_id = NULL,
                                  overwrite = FALSE) {
  if (!is.character(zip_file) || length(zip_file) != 1L || !file.exists(zip_file)) {
    stop("zip_file must name an existing ZIP archive.", call. = FALSE)
  }
  if (!is.character(project_path) || length(project_path) != 1L ||
      !dir.exists(project_path)) {
    stop("project_path must name an existing project directory.", call. = FALSE)
  }
  if (length(overwrite) != 1L || !is.logical(overwrite) || is.na(overwrite)) {
    stop("overwrite must be a single TRUE or FALSE value.", call. = FALSE)
  }
  if (!is.null(run_id) &&
      (!is.character(run_id) || length(run_id) != 1L || !nzchar(run_id))) {
    stop("run_id must be NULL or a non-empty character value.", call. = FALSE)
  }

  zip_file <- normalizePath(zip_file, winslash = "/", mustWork = TRUE)
  project_path <- normalizePath(project_path, winslash = "/", mustWork = TRUE)
  entries <- .daycent_result_entries(zip_file)
  output_candidates <- lapply(entries, .daycent_result_output)
  keep_outputs <- !vapply(output_candidates, is.null, logical(1L))
  output_entries <- entries[keep_outputs]
  outputs <- output_candidates[keep_outputs]
  if (!length(outputs)) {
    stop("Result ZIP contained no recognized DayCent outputs.", call. = FALSE)
  }

  temp_dir <- tempfile("daycent-results-")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  tryCatch(utils::unzip(zip_file, exdir = temp_dir),
           error = function(error) stop("Could not extract result ZIP: ",
                                       conditionMessage(error), call. = FALSE))

  output_paths <- vapply(outputs, function(output) {
    file.path(project_path, "sites", output$site, "outputs", output$scenario,
              output$relative)
  }, character(1L))
  if (anyDuplicated(output_paths)) {
    stop("Result ZIP contains duplicate output destinations.", call. = FALSE)
  }
  if (!isTRUE(overwrite) && any(file.exists(output_paths))) {
    stop("Output exists; set overwrite = TRUE to replace it.", call. = FALSE)
  }

  for (index in seq_along(outputs)) {
    # Use the output's archive entry directly; input-like entries are never copied.
    source <- file.path(temp_dir, output_entries[[index]])
    destination <- output_paths[[index]]
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, destination, overwrite = isTRUE(overwrite))) {
      stop(sprintf("Could not copy result output: %s", destination), call. = FALSE)
    }
  }

  metadata_entries <- entries[vapply(entries, .daycent_result_metadata, logical(1L))]
  metadata_dir <- NULL
  metadata_paths <- character()
  if (length(metadata_entries)) {
    metadata_dir <- .daycent_metadata_dir(project_path, run_id %||% "run")
    for (entry in metadata_entries) {
      destination <- file.path(metadata_dir, entry)
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(file.path(temp_dir, entry),
                     destination, overwrite = FALSE)) {
        stop(sprintf("Could not archive API metadata: %s", entry), call. = FALSE)
      }
      metadata_paths <- c(metadata_paths, destination)
    }
  }
  list(output_paths = unname(output_paths), metadata_dir = metadata_dir,
       metadata_paths = metadata_paths)
}

#' Download and safely extract DayCent API results
#'
#' @param config API configuration from [daycent_runner_config()].
#' @param run_id Completed API run ID.
#' @param project_path Project root containing the `sites/` directory.
#' @param keep_zip Logical. Retain the downloaded ZIP only when `TRUE`.
#' @param output_zip Optional path for a retained or temporary ZIP.
#' @param overwrite Logical. Allow existing output files to be replaced.
#' @return A list containing the run ID, output paths, metadata archive, and
#'   `zip_path` when the ZIP was retained.
#' @export
download_daycent_results <- function(config, run_id, project_path,
                                     keep_zip = FALSE, output_zip = NULL,
                                     overwrite = FALSE) {
  .daycent_validate_api_config(config)
  if (!is.character(run_id) || length(run_id) != 1L || !nzchar(run_id)) {
    stop("run_id must be a non-empty character value.", call. = FALSE)
  }
  if (!is.character(project_path) || length(project_path) != 1L ||
      !dir.exists(project_path)) {
    stop("project_path must name an existing project directory.", call. = FALSE)
  }
  if (length(keep_zip) != 1L || !is.logical(keep_zip) || is.na(keep_zip) ||
      length(overwrite) != 1L || !is.logical(overwrite) || is.na(overwrite)) {
    stop("keep_zip and overwrite must be single TRUE or FALSE values.", call. = FALSE)
  }
  if (isTRUE(keep_zip) && is.null(output_zip)) {
    output_zip <- file.path(project_path, paste0("run_", run_id, ".zip"))
  } else if (is.null(output_zip)) {
    output_zip <- tempfile("daycent-results-", fileext = ".zip")
  }
  if (!is.character(output_zip) || length(output_zip) != 1L || !nzchar(output_zip)) {
    stop("output_zip must be NULL or a non-empty file path.", call. = FALSE)
  }
  if (isTRUE(keep_zip) && file.exists(output_zip) && !isTRUE(overwrite)) {
    stop("Retained ZIP exists; set overwrite = TRUE to replace it.", call. = FALSE)
  }
  parent <- dirname(normalizePath(output_zip, winslash = "/", mustWork = FALSE))
  if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(parent)) stop("Could not create the ZIP destination directory.", call. = FALSE)

  cleanup <- !isTRUE(keep_zip)
  if (cleanup) on.exit(unlink(output_zip, force = TRUE), add = TRUE)
  response <- tryCatch(.daycent_result_download_request(
    .daycent_url(config, paste0("/api/ModelRuns/", run_id, "/download")),
    headers = list(`X-Api-Key` = config$api_key), output_zip = output_zip,
    verify_ssl = .daycent_config_value(config, "verify_ssl", TRUE)),
    error = function(error) stop("Result download failed: ", conditionMessage(error), call. = FALSE))
  if (response$status_code < 200L || response$status_code >= 300L) {
    stop(paste("Result download failed:", .daycent_response_error(
      response, config$api_key)), call. = FALSE)
  }
  if (!file.exists(output_zip) || is.na(file.size(output_zip)) || file.size(output_zip) == 0) {
    stop("Result download returned an empty ZIP.", call. = FALSE)
  }
  extracted <- unzip_daycent_results(output_zip, project_path, run_id, overwrite)
  extracted$run_id <- run_id
  extracted$zip_path <- if (isTRUE(keep_zip)) normalizePath(output_zip, winslash = "/") else NULL
  extracted
}

`%||%` <- function(x, y) if (is.null(x)) y else x
