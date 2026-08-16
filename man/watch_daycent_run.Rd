% Generated manually for Packet 03; regenerate with roxygen2 when available.
\name{watch_daycent_run}
\alias{watch_daycent_run}
\title{Watch a DayCent ModelRuns status until completion}
\usage{watch_daycent_run(config, run_id, timeout_seconds = 3600, max_consecutive_failures = 3L)}
\arguments{\item{config}{API configuration.}\item{run_id}{Model run ID.}\item{timeout_seconds}{Maximum watch duration.}\item{max_consecutive_failures}{Maximum retryable failures in a row.}}
\value{Terminal parsed status result.}
