# =============================================================
# 04_clean_merge.R
# -------------------------------------------------------------
# Merges all three sources into a single tidy panel:
#   ticker × date × features
#
# Sources:
#   data/raw/prices_raw.parquet          (script 01)
#   data/processed/daily_sentiment.parquet (script 03)
#   data/processed/stocktwits_summary.parquet (script 03)
#
# Output:
#   data/processed/panel_final.parquet
#
# Panel structure (one row per ticker-trading-day):
#   - Price/return features
#   - Abnormal returns vs SPY
#   - Lagged sentiment features (t-1, t-3, t-5)
#   - StockTwits bull ratio (cross-sectional, recent window)
#   - Bucket labels
# =============================================================

library(tidyverse)
library(lubridate)
library(arrow)
library(here)
library(janitor)

# ---- 1. Load all sources ----
message("Loading data sources...")

prices_raw   <- read_parquet(here("data", "raw", "prices_raw.parquet"))
daily_sent   <- read_parquet(here("data", "processed", "daily_sentiment.parquet"))
st_summary   <- read_parquet(here("data", "processed", "stocktwits_summary.parquet"))
tickers_df   <- read_csv(here("tickers.csv"), show_col_types = FALSE) |> clean_names()

message("Prices: ",       nrow(prices_raw), " rows")
message("Sentiment: ",    nrow(daily_sent),  " rows")
message("StockTwits: ",   nrow(st_summary),  " tickers")

# ---- 2. Build returns from price data ----
message("\nComputing returns...")

# Separate SPY for benchmark
spy_prices <- prices_raw |>
  filter(symbol == "SPY") |>
  select(date, spy_close = adjusted) |>
  arrange(date) |>
  mutate(spy_return = (spy_close - lag(spy_close)) / lag(spy_close))

# Study tickers
stock_prices <- prices_raw |>
  filter(symbol != "SPY") |>
  select(symbol, date, open, high, low, close, volume, adjusted,
         bucket, sector, personal_holding) |>
  arrange(symbol, date) |>
  group_by(symbol) |>
  mutate(
    # Simple daily return
    daily_return = (adjusted - lag(adjusted)) / lag(adjusted),
    # Forward returns (what we're trying to predict)
    fwd_return_1 = lead(daily_return, 1),
    fwd_return_3 = (lead(adjusted, 3) - adjusted) / adjusted,
    fwd_return_5 = (lead(adjusted, 5) - adjusted) / adjusted,
    # Log return for normality
    log_return   = log(adjusted / lag(adjusted))
  ) |>
  ungroup()

# ---- 3. Compute abnormal returns (vs SPY benchmark) ----
message("Computing abnormal returns vs SPY...")

stock_prices <- stock_prices |>
  left_join(spy_prices |> select(date, spy_return), by = "date") |>
  mutate(
    abnormal_return   = daily_return - spy_return,
    fwd_abnormal_1    = fwd_return_1 - lead(spy_return, 1),
    fwd_abnormal_3    = fwd_return_3 - (lead(spy_return, 1) +
                          lead(spy_return, 2) + lead(spy_return, 3)),
    fwd_abnormal_5    = fwd_return_5 - (lead(spy_return, 1) +
                          lead(spy_return, 2) + lead(spy_return, 3) +
                          lead(spy_return, 4) + lead(spy_return, 5))
  )

# ---- 4. Build lagged sentiment features ----
message("Building lagged sentiment features...")

sent_lagged <- daily_sent |>
  arrange(ticker, date) |>
  group_by(ticker) |>
  mutate(
    # Lagged sentiment (past signal predicting future return)
    sent_lag1  = lag(mean_sentiment, 1),
    sent_lag3  = lag(mean_sentiment, 3),
    sent_lag5  = lag(mean_sentiment, 5),
    # Lagged sentiment ratio
    ratio_lag1 = lag(sentiment_ratio, 1),
    ratio_lag3 = lag(sentiment_ratio, 3),
    ratio_lag5 = lag(sentiment_ratio, 5),
    # Lagged headline count (attention signal)
    news_lag1  = lag(n_headlines, 1),
    news_lag3  = lag(n_headlines, 3),
    # Rolling 5-day average sentiment
    sent_roll5 = zoo::rollmean(mean_sentiment, k = 5,
                               fill = NA, align = "right")
  ) |>
  ungroup()

# ---- 5. Merge everything ----
message("Merging panel...")

panel <- stock_prices |>
  rename(ticker = symbol) |>
  # Join current-day sentiment
  left_join(
    daily_sent |> select(ticker, date, n_headlines, mean_sentiment,
                         sentiment_ratio, n_positive_headlines,
                         n_negative_headlines),
    by = c("ticker", "date")
  ) |>
  # Join lagged sentiment
  left_join(
    sent_lagged |> select(ticker, date, sent_lag1, sent_lag3, sent_lag5,
                          ratio_lag1, ratio_lag3, ratio_lag5,
                          news_lag1, news_lag3, sent_roll5),
    by = c("ticker", "date")
  ) |>
  # Join StockTwits (cross-sectional — same value for all dates per ticker)
  left_join(
    st_summary |> select(ticker, n_bullish, n_bearish, bull_ratio),
    by = "ticker"
  ) |>
  # Fill missing sentiment with 0 (no news = neutral)
  mutate(
    across(c(n_headlines, mean_sentiment, sentiment_ratio,
             n_positive_headlines, n_negative_headlines),
           ~ replace_na(.x, 0)),
    across(c(sent_lag1, sent_lag3, sent_lag5,
             ratio_lag1, ratio_lag3, ratio_lag5),
           ~ replace_na(.x, 0))
  )

message("Panel rows: ", nrow(panel))
message("Panel columns: ", ncol(panel))

# ---- 6. Clean up ----
panel_final <- panel |>
  # Drop first/last few rows per ticker where lags/leads are NA
  filter(!is.na(daily_return), !is.na(fwd_return_1)) |>
  arrange(ticker, date)

message("Final panel rows (after removing NA returns): ", nrow(panel_final))
message("Tickers: ", n_distinct(panel_final$ticker))
message("Date range: ", min(panel_final$date), " to ", max(panel_final$date))

# ---- 7. Quick descriptive summary ----
message("\n--- Return summary by bucket ---")
panel_final |>
  group_by(bucket) |>
  summarise(
    n_obs          = n(),
    mean_return    = mean(daily_return, na.rm = TRUE),
    sd_return      = sd(daily_return, na.rm = TRUE),
    mean_abnormal  = mean(abnormal_return, na.rm = TRUE),
    mean_sentiment = mean(mean_sentiment, na.rm = TRUE),
    pct_news_days  = mean(n_headlines > 0)
  ) |>
  print()

message("\n--- Correlation: lagged sentiment vs forward returns ---")
panel_final |>
  select(fwd_return_1, fwd_return_3, fwd_return_5,
         fwd_abnormal_1, sent_lag1, sent_lag3, ratio_lag1) |>
  cor(use = "complete.obs") |>
  round(3) |>
  print()

# ---- 8. Save ----
dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)
write_parquet(panel_final, here("data", "processed", "panel_final.parquet"))

message("\nSaved data/processed/panel_final.parquet")
message("Columns: ", paste(names(panel_final), collapse = ", "))
message("\nDone. Next: scripts/05_validate.R")
