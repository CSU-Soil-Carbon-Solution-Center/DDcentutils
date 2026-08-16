#' ZIP selected DayCent inputs for API upload
#'
#' Creates an archive containing selected flat `sites/{site}/` inputs and,
#' when present, the project's top-level `libs/` directory. Entries in
#' `sites/{site}/outputs/` are excluded; input QC remains responsible for
#' reporting other unexpected files.
#'
#' @param project_path Character project directory containing `sites/` and
#'   optionally `libs/`.
#' @param include Character vector of API-shaped `site/scenario` selections.
#'   `NULL` or `"*"` includes all sites and scenarios.
#' @param run_eq Logical. When `TRUE`, adds each selected site's `eq` and
#'   `base` entries to the effective include set.
#' @param out_zip Character destination path for the archive.
#'
#' @return Invisibly returns the created ZIP path.
#' @export
zip_daycent_inputs <- function(project_path, include = NULL, run_eq = FALSE,
                                out_zip = tempfile(fileext = ".zip")) {
  if (!is.character(project_path) || length(project_path) != 1L ||
      !dir.exists(project_path)) {
    stop("project_path must name an existing directory.", call. = FALSE)
  }
  if (length(run_eq) != 1L || !is.logical(run_eq) || is.na(run_eq)) {
    stop("run_eq must be a single TRUE or FALSE value.", call. = FALSE)
  }
  if (!is.null(include) &&
      (!is.character(include) || any(!nzchar(include)))) {
    stop("include must be NULL or character values of the form site/scenario.", call. = FALSE)
  }
  if (!is.character(out_zip) || length(out_zip) != 1L || !nzchar(out_zip)) {
    stop("out_zip must be a non-empty file path.", call. = FALSE)
  }

  project_path <- normalizePath(project_path, winslash = "/", mustWork = TRUE)
  out_zip <- normalizePath(out_zip, winslash = "/", mustWork = FALSE)
  sites_root <- file.path(project_path, "sites")
  if (!dir.exists(sites_root)) {
    stop("Project sites directory not found: sites", call. = FALSE)
  }

  site_names <- list.files(sites_root, full.names = FALSE, recursive = FALSE)
  site_names <- site_names[file.info(file.path(sites_root, site_names))$isdir]
  if (!length(site_names)) {
    stop("No site input directories found under sites.", call. = FALSE)
  }

  all_sites <- is.null(include) || identical(include, "*") || "*" %in% include
  if (all_sites) {
    selected <- stats::setNames(lapply(site_names, function(x) "*"), site_names)
  } else {
    valid_include <- grepl("^[A-Za-z0-9_-]+/[A-Za-z0-9_-]+$", include)
    if (any(!valid_include)) {
      stop("include values must have the form site/scenario or be '*'.", call. = FALSE)
    }
    requested_sites <- unique(sub("/.*$", "", include))
    missing_sites <- setdiff(requested_sites, site_names)
    if (length(missing_sites)) {
      stop(sprintf("Site input directory not found: %s", paste(missing_sites, collapse = ", ")), call. = FALSE)
    }
    selected <- stats::setNames(
      lapply(site_names, function(site) sub("^[^/]*/", "", include[startsWith(include, paste0(site, "/"))])),
      site_names
    )
    selected <- selected[vapply(selected, length, integer(1L)) > 0L]
  }

  if (isTRUE(run_eq)) {
    selected <- lapply(selected, function(scenarios) {
      if (identical(scenarios, "*")) scenarios else unique(c(scenarios, "eq", "base"))
    })
  }

  relative_files <- character()
  for (site in names(selected)) {
    site_dir <- file.path(sites_root, site)
    site_files <- list.files(site_dir, recursive = TRUE, full.names = FALSE,
                             include.dirs = FALSE, all.files = FALSE)
    site_files <- gsub("\\\\", "/", site_files)
    site_files <- site_files[!grepl("^outputs/", site_files, ignore.case = TRUE)]
    scenarios <- selected[[site]]
    if (!identical(scenarios, "*")) {
      scenario_pattern <- paste0("^", site, "_(", paste(scenarios, collapse = "|"), ")\\.")
      basenames <- basename(site_files)
      scenario_specific <- grepl(paste0("^", site, "_[^/]+\\.[^/]+$"), basenames) &
        !grepl(paste0("^", site, "_site\\."), basenames)
      keep_shared <- !scenario_specific | grepl(scenario_pattern, basenames)
      site_files <- site_files[keep_shared]
    }
    relative_files <- c(relative_files, file.path("sites", site, site_files))
  }

  libs_dir <- file.path(project_path, "libs")
  if (dir.exists(libs_dir)) {
    libs_files <- list.files(libs_dir, recursive = TRUE, full.names = FALSE,
                             include.dirs = FALSE, all.files = FALSE)
    relative_files <- c(relative_files, file.path("libs", libs_files))
  }

  relative_files <- sort(unique(gsub("\\\\", "/", relative_files)))
  if (!length(relative_files)) {
    stop("No DayCent input files remain after exclusions; nothing to ZIP.", call. = FALSE)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_path)
  status <- utils::zip(zipfile = out_zip, files = relative_files, flags = "-r9Xq")
  if (!identical(status, 0L) && !identical(status, 0)) {
    stop("ZIP creation failed.", call. = FALSE)
  }
  if (!file.exists(out_zip) || is.na(file.size(out_zip)) || file.size(out_zip) == 0) {
    stop("ZIP creation failed; output archive is missing or empty.", call. = FALSE)
  }

  invisible(out_zip)
}
