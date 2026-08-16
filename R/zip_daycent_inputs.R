#' ZIP DayCent site inputs for upload
#'
#' Creates an archive containing the flat `sites/{site}/` input directory and,
#' when present, the project's top-level `libs/` directory. Generated outputs,
#' logs, status snapshots, nested archives, and other generated files are not
#' included.
#'
#' @param project_path Character project directory containing `sites/` and
#'   optionally `libs/`.
#' @param site Character site directory name under `sites/`.
#' @param out_zip Character destination path for the archive.
#'
#' @return Invisibly returns the created ZIP path.
#' @export
zip_daycent_inputs <- function(project_path, site,
                                out_zip = tempfile(fileext = ".zip")) {
  if (!is.character(project_path) || length(project_path) != 1L ||
      !dir.exists(project_path)) {
    stop("project_path must name an existing directory.", call. = FALSE)
  }
  if (!is.character(site) || length(site) != 1L ||
      !grepl("^[A-Za-z0-9_-]+$", site)) {
    stop("site must contain only letters, numbers, underscores, and hyphens.", call. = FALSE)
  }
  if (!is.character(out_zip) || length(out_zip) != 1L || !nzchar(out_zip)) {
    stop("out_zip must be a non-empty file path.", call. = FALSE)
  }

  project_path <- normalizePath(project_path, winslash = "/", mustWork = TRUE)
  out_zip <- normalizePath(out_zip, winslash = "/", mustWork = FALSE)
  site_dir <- file.path(project_path, "sites", site)
  if (!dir.exists(site_dir)) {
    stop(sprintf("Site directory not found: sites/%s", site), call. = FALSE)
  }

  site_files <- list.files(site_dir, recursive = TRUE, full.names = FALSE,
                           include.dirs = FALSE, all.files = FALSE)
  relative_files <- file.path("sites", site, site_files)

  libs_dir <- file.path(project_path, "libs")
  if (dir.exists(libs_dir)) {
    libs_files <- list.files(libs_dir, recursive = TRUE, full.names = FALSE,
                             include.dirs = FALSE, all.files = FALSE)
    relative_files <- c(relative_files, file.path("libs", libs_files))
  }

  relative_files <- gsub("\\\\", "/", relative_files)
  keep <- !grepl(paste(c(
    "(^|/)out_[^/]+\\.(csv|txt)$",
    "(^|/)(log|run|status|status_snapshot)\\.(txt|log|json)$",
    "(^|/)(outputs?|logs?|statuses?|downloads?)(/|$)",
    "\\.(zip|lis|bin|out|csv)$",
    "(^|/)(\\.DS_Store|Thumbs\\.db)$"
  ), collapse = "|"), relative_files, ignore.case = TRUE, perl = TRUE)
  relative_files <- sort(unique(relative_files[keep]))

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
