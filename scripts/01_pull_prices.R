# =============================================================
# 01_pull_prices.R
# -------------------------------------------------------------
# Pulls trailing 12 months of daily OHLCV for the 30-ticker
# universe + SPY (used as market benchmark for abnormal returns)
# from Yahoo Finance via tidyquant. Saves a tidy long-format
# parquet to data/raw/prices_raw.parquet.
#
# Runtime: ~1-3 minutes depending on connection.
# =============================================================

library(tidyverse)
library(tidyquant)
library(lubridate)
library(arrow)
library(here)
library(janitor)

# ---- 1. Load ticker universe ----
tickers_df <- read_csv(here("tickers.csv"), show_col_types = FALSE) |>
  clean_names()

study_tickers <- tickers_df$ticker
all_tickers   <- c(study_tickers, "SPY")  # SPY = market benchmark

message("Pulling ", length(all_tickers), " tickers (30 study + SPY benchmark)")

# ---- 2. Define window ----
end_date   <- Sys.Date()
start_date <- end_date %m-% months(12)

message("Window: ", start_date, " to ", end_date)

# ---- 3. Pull from Yahoo Finance ----
# tq_get is rate-friendly and handles batches well.
prices_raw <- tq_get(
  all_tickers,
  get  = "stock.prices",
  from = start_date,
  to   = end_date
)

# ---- 4. Quick sanity prints ----
cat("\n--- Shape ---\n")
print(dim(prices_raw))

cat("\n--- Tickers returned ---\n")
returned <- prices_raw |> distinct(symbol) |> pull(symbol)
print(returned)

missing <- setdiff(all_tickers, returned)
if (length(missing) > 0) {
  warning("Missing tickers (no data returned): ", paste(missing, collapse = ", "))
}

cat("\n--- Date coverage per ticker ---\n")
prices_raw |>
  group_by(symbol) |>
  summarise(
    first_date = min(date),
    last_date  = max(date),
    n_days     = n()
  ) |>
  print(n = Inf)

# ---- 5. Add bucket labels for study tickers; tag SPY as benchmark ----
prices_tagged <- prices_raw |>
  left_join(
    tickers_df |> select(ticker, bucket, sector, personal_holding),
    by = c("symbol" = "ticker")
  ) |>
  mutate(
    bucket = if_else(symbol == "SPY", "benchmark", bucket),
    sector = if_else(symbol == "SPY", "benchmark", sector)
  )

# ---- 6. Save ----
out_path <- here("data", "raw", "prices_raw.parquet")
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write_parquet(prices_tagged, out_path)

message("Saved ", nrow(prices_tagged), " rows to ", out_path)

# ---- 7. Bonus: a quick sanity plot of normalized prices by bucket ----
# (Saved to output/figures so you can eyeball the data immediately)
if (!dir.exists(here("output", "figures"))) {
  dir.create(here("output", "figures"), recursive = TRUE)
}

p <- prices_tagged |>
  filter(symbol != "SPY") |>
  group_by(symbol) |>
  arrange(date) |>
  mutate(norm = adjusted / first(adjusted)) |>
  ungroup() |>
  ggplot(aes(date, norm, group = symbol, color = bucket)) +
  geom_line(alpha = 0.6) +
  facet_wrap(~ bucket, ncol = 1, scales = "free_y") +
  labs(
    title = "Normalized adjusted close by bucket (12-month window)",
    subtitle = "Each line = one ticker, normalized to 1.0 at window start",
    y = "Normalized price",
    x = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  here("output", "figures", "01_prices_sanity_check.png"),
  p, width = 10, height = 8, dpi = 120
)

message("Sanity-check plot saved to output/figures/01_prices_sanity_check.png")
message("\nDone. Next: scripts/02_scrape_social.R")
