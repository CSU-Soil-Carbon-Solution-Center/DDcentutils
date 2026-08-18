% Generated manually for Packet 02; regenerate with roxygen2 when available.
\name{daycent_runner_config}
\alias{daycent_runner_config}
\title{Configure a local or API-backed DayCent runner}
\usage{
daycent_runner_config(backend = c("exe", "api"), exe_path = NULL,
  dc_path100 = NULL, validate_paths = FALSE,
  base_url = "https://api.emdc.eco", verify_ssl = TRUE,
  poll_seconds = 60, api_key = Sys.getenv("EMDC_API_KEY", ""),
  model_name = "DayCent", model_version = "491")
}
\arguments{
  \item{backend}{Either \code{"exe"} or \code{"api"}.}
  \item{exe_path}{Path to the local DayCent executable.}
  \item{dc_path100}{Path to the DayCent .100 list file.}
  \item{validate_paths}{Whether to validate local executable paths.}
  \item{base_url}{API base URL.}
  \item{verify_ssl}{Whether to verify API TLS certificates.}
  \item{poll_seconds}{Positive polling interval in seconds.}
  \item{api_key}{API key, defaulting to \code{EMDC_API_KEY}.}
  \item{model_name}{API model name, defaulting to \code{DayCent}.}
  \item{model_version}{API model version, defaulting to \code{491}.}
}
\value{A named runner configuration list.}
\description{Creates validated configuration for local executable or API-backed DayCent runs. API product identity is resolved from model name and version at submission time.}
