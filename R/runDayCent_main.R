#!/usr/bin/env Rscript

## ===== Strict fail-fast mode =====
options(
  warn = 2,  # treat warnings as errors (remove if too strict)
  error = function() {
    traceback(2)
    quit(save = "no", status = 1, runLast = FALSE)
  }
)

## ===== Argument guidance =====
print_help <- function(){
  cat("
Usage:
  Rscript run_daycent.R <site> <run_dir> <config_file> [run_eq]

Arguments:
  site        Name of the site folder under 'sites/' (e.g. 'canola_CO')
  run_dir     Directory with run scripts and helper functions
  config_file Path to configuration file (YAML/JSON/RDS with dc_exe, dc_path100)
  run_eq      (optional) Whether to run DayCent equilibrium spin-up
              Accepts: TRUE/FALSE, 1/0, yes/no
              Default: TRUE

Examples:
  Rscript run_daycent.R site01 /data/daycent/runs config.yaml
  Rscript run_daycent.R canola_CO ./runs site_config.yml FALSE
\n")
}

parse_bool <- function(x){
  if (missing(x) || is.null(x)) return(TRUE) # default TRUE
  x <- tolower(as.character(x))
  if (x %in% c("1","true","t","yes","y"))  return(TRUE)
  if (x %in% c("0","false","f","no","n"))  return(FALSE)
  stop(sprintf("Invalid boolean for run_eq: %s (use TRUE/FALSE, 1/0, yes/no)", x))
}

## ===== Parse arguments =====
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0 || args[1] %in% c("-h","--help")){
  print_help()
  quit(save = "no", status = 0)
}

if (length(args) < 3){
  cat("❌ Not enough arguments provided.\n\n")
  print_help()
  quit(save = "no", status = 1)
}

site    <- args[1]
run_dir <- args[2]
config  <- args[3]
run_eq  <- if (length(args) >= 4) parse_bool(args[4]) else TRUE

cat("▶ site:", site, "\n")
cat("▶ run_dir:", run_dir, "\n")
cat("▶ config:", config, "\n")
cat("▶ run_eq:", run_eq, "(default TRUE if not given)\n")

## ===== Dependency loader =====
require_pkg <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

for (p in c("data.table","tidyverse","here","DDcentutils","logger")) require_pkg(p)

## ===== Logging setup =====
log_file <- file.path(run_dir, "daycent_run.log")
logger::log_appender(logger::appender_tee(log_file)) # console + file
logger::log_layout(logger::layout_glue_colors)
logger::log_info("Initialized run: site={site} run_dir={run_dir} config={config} run_eq={run_eq}")

## ===== Path checks =====
stopf <- function(...){ stop(sprintf(...), call. = FALSE) }
must_exist_dir  <- function(p) if (!dir.exists(p))  stopf("Missing directory: %s", p)
must_exist_file <- function(p) if (!file.exists(p)) stopf("Missing file: %s", p)

run_dir <- normalizePath(run_dir, mustWork = FALSE)
must_exist_dir(run_dir)

site_dir <- here::here("sites", site)
site_dir <- normalizePath(site_dir, mustWork = FALSE)
must_exist_dir(site_dir)

config <- normalizePath(config, mustWork = FALSE)
must_exist_file(config)

## ===== Environment prep =====
setwd(run_dir)
logger::log_info("Working directory set to {getwd()}")

helper_script <- file.path(run_dir, "helper_functions.R")
if (file.exists(helper_script)) {
  source(helper_script, local = TRUE)
  logger::log_info("Loaded helper script: {helper_script}")
} else {
  logger::log_warn("No helper script found at {helper_script}")
}

## ===== Validate DayCent executables =====
if (!exists("dc_exe", inherits = TRUE))      stopf("dc_exe not defined (from config or env).")
if (!exists("dc_path100", inherits = TRUE))  stopf("dc_path100 not defined (from config or env).")
must_exist_file(dc_exe)
must_exist_dir(dc_path100)

## ===== Switch to site directory =====
setwd(site_dir)
logger::log_info("Now in site_dir: {getwd()}")

## ===== Run DayCent =====
run_safe <- function(expr, msg){
  tryCatch(expr, error = function(e){
    logger::log_error("{msg}: {e$message}")
    stopf("%s: %s", msg, e$message)
  })
}

if (isTRUE(run_eq)) {
  logger::log_info("Running equilibrium + base")
  daycent_log <- run_safe(
    DayCentRunSite(
      site = site, scen = "A",
      run_base = TRUE, run_eq = TRUE,
      dc_exe_in = dc_exe,
      dc_path100_in = dc_path100
    ),
    "DayCentRunSite failed"
  )
} else {
  logger::log_info("Running single pass (no equilibrium)")
  daycent_log <- run_safe(
    DayCentRunSite_single_run(
      site = site, scen = "A",
      dc_exe_in = dc_exe,
      dc_path100_in = dc_path100
    ),
    "DayCentRunSite_single_run failed"
  )
}

## ===== Post-run checks =====
if (is.null(daycent_log) || NROW(daycent_log) == 0) {
  stopf("Run produced no log output.")
}

logger::log_info("Run completed successfully.")
cat("✅ Done.\n")
