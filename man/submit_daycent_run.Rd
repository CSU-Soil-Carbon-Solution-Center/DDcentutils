% Generated manually for Packet 03; regenerate with roxygen2 when available.
\name{submit_daycent_run}
\alias{submit_daycent_run}
\title{Submit a DayCent ModelRuns request}
\usage{submit_daycent_run(config, input_zip, include = NULL, run_eq = FALSE,
  name = "daycent-r-api-run", site_name = NULL, scenario_name = NULL,
  organization_membership_id = NULL, dry_run = NULL, dry_run_mode = NULL,
  wait = FALSE)}
\arguments{\item{config}{API configuration.}\item{input_zip}{Existing ZIP archive.}\item{include}{Site/scenario selectors.}\item{run_eq}{API runEquilibrium value.}\item{name}{Run name.}\item{site_name}{Optional site name.}\item{scenario_name}{Optional scenario name.}\item{organization_membership_id}{Optional organization membership ID.}\item{dry_run}{Optional dry-run flag.}\item{dry_run_mode}{Optional dry-run mode.}\item{wait}{Whether to watch before returning.}}
\value{Parsed submission result.}
