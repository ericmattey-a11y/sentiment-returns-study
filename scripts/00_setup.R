# =============================================================
# 00_setup.R
# -------------------------------------------------------------
# Installs and loads all packages required for the pipeline.
# Run this once per fresh clone, then re-run if dependencies
# change. Subsequent scripts call library() directly assuming
# packages are already installed.
# =============================================================

# Required packages, grouped by purpose
required_pkgs <- c(
  # Core tidyverse + IO
  "tidyverse",     # dplyr, ggplot2, readr, tidyr, etc.
  "lubridate",     # date handling
  "arrow",         # parquet IO

  # Finance / market data
  "tidyquant",     # Yahoo Finance via tq_get()

  # Web acquisition
  "httr2",         # modern HTTP client
  "rvest",         # HTML scraping
  "jsonlite",      # JSON parsing

  # Text / sentiment
  "tidytext",      # tokenization (used for descriptive stats only)

  # Utility
  "glue",          # string interpolation
  "fs",            # filesystem
  "janitor",       # clean_names() etc.
  "here"           # project-relative paths
)

# Install whatever is missing
to_install <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
}

# Load
invisible(lapply(required_pkgs, library, character.only = TRUE))

# Sanity-check that the .Renviron is in place
if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
  warning(
    "ANTHROPIC_API_KEY is empty. Copy .Renviron.example to .Renviron, ",
    "fill in your key, and restart R before running 03_news_llm_extraction.R"
  )
}

message("Setup complete. ", length(required_pkgs), " packages loaded.")
