# =============================================================
# 06_features_analysis.R
# -------------------------------------------------------------
# Produces the core analytical findings:
#   1. Sentiment-return correlations by bucket
#   2. Simple OLS regressions (sentiment -> forward returns)
#   3. High vs low sentiment day return comparisons
#   4. News volume attention signal analysis
#   5. StockTwits bull ratio vs return analysis
#   6. Export-ready summary tables for Tableau
#
# Outputs:
#   output/tables/correlations_by_bucket.csv
#   output/tables/regression_summary.csv
#   output/tables/sentiment_quintile_returns.csv
#   output/tables/tableau_export.csv   <- main Tableau input
#   output/figures/                    <- exploratory plots
# =============================================================

library(tidyverse)
library(lubridate)
library(arrow)
library(here)
library(janitor)
library(glue)

panel <- read_parquet(here("data", "processed", "panel_final.parquet"))

dir.create(here("output", "tables"),  showWarnings = FALSE, recursive = TRUE)
dir.create(here("output", "figures"), showWarnings = FALSE, recursive = TRUE)

message("Panel loaded: ", nrow(panel), " rows")

# =============================================================
# 1. CORRELATIONS BY BUCKET
# =============================================================

message("\n=== 1. Sentiment-Return Correlations by Bucket ===")

corr_by_bucket <- panel |>
  group_by(bucket) |>
  summarise(
    n_obs             = n(),
    # Sentiment -> next-day return
    cor_sent_fwd1     = cor(sent_lag1, fwd_return_1,   use = "complete.obs"),
    cor_sent_fwd3     = cor(sent_lag1, fwd_return_3,   use = "complete.obs"),
    cor_sent_fwd5     = cor(sent_lag1, fwd_return_5,   use = "complete.obs"),
    # Sentiment -> abnormal return
    cor_sent_abnorm1  = cor(sent_lag1, fwd_abnormal_1, use = "complete.obs"),
    # Ratio -> next-day return
    cor_ratio_fwd1    = cor(ratio_lag1, fwd_return_1,  use = "complete.obs"),
    # News volume -> next-day return
    cor_news_fwd1     = cor(news_lag1,  fwd_return_1,  use = "complete.obs"),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

message("Correlations by bucket:")
print(corr_by_bucket)

write_csv(corr_by_bucket, here("output", "tables", "correlations_by_bucket.csv"))

# ---- Also by individual ticker ----
corr_by_ticker <- panel |>
  group_by(ticker, bucket) |>
  summarise(
    n_obs            = n(),
    cor_sent_fwd1    = cor(sent_lag1,  fwd_return_1,   use = "complete.obs"),
    cor_sent_fwd3    = cor(sent_lag1,  fwd_return_3,   use = "complete.obs"),
    cor_ratio_fwd1   = cor(ratio_lag1, fwd_return_1,   use = "complete.obs"),
    cor_news_fwd1    = cor(news_lag1,  fwd_return_1,   use = "complete.obs"),
    mean_daily_ret   = mean(daily_return, na.rm = TRUE),
    sd_daily_ret     = sd(daily_return,   na.rm = TRUE),
    mean_sentiment   = mean(mean_sentiment, na.rm = TRUE),
    bull_ratio       = first(bull_ratio),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

message("\nCorrelations by ticker (top/bottom 5 by sent->fwd1):")
corr_by_ticker |>
  arrange(desc(abs(cor_sent_fwd1))) |>
  select(ticker, bucket, cor_sent_fwd1, cor_sent_fwd3, cor_ratio_fwd1) |>
  slice(1:10) |>
  print()

write_csv(corr_by_ticker, here("output", "tables", "correlations_by_ticker.csv"))

# =============================================================
# 2. OLS REGRESSIONS: SENTIMENT -> FORWARD RETURNS
# =============================================================

message("\n=== 2. OLS Regressions by Bucket ===")

run_regression <- function(data, bucket_name, outcome_var) {
  formula <- as.formula(paste(outcome_var, "~ sent_lag1 + ratio_lag1 + news_lag1"))
  model   <- lm(formula, data = data)
  s       <- summary(model)

  tibble(
    bucket         = bucket_name,
    outcome        = outcome_var,
    n_obs          = nobs(model),
    r_squared      = round(s$r.squared, 4),
    adj_r_squared  = round(s$adj.r.squared, 4),
    coef_sent_lag1 = round(coef(model)["sent_lag1"], 6),
    pval_sent_lag1 = round(summary(model)$coefficients["sent_lag1", "Pr(>|t|)"], 4),
    coef_ratio_lag1 = round(coef(model)["ratio_lag1"], 6),
    pval_ratio_lag1 = round(summary(model)$coefficients["ratio_lag1", "Pr(>|t|)"], 4),
    significant    = pval_sent_lag1 < 0.05
  )
}

buckets   <- c("retail_favorite", "large_cap_institutional", "mid_attention")
outcomes  <- c("fwd_return_1", "fwd_return_3", "fwd_return_5", "fwd_abnormal_1")

reg_results <- map_dfr(buckets, function(b) {
  bucket_data <- panel |> filter(bucket == b)
  map_dfr(outcomes, function(o) {
    tryCatch(
      run_regression(bucket_data, b, o),
      error = function(e) {
        message("  Regression failed: ", b, " / ", o, " -- ", conditionMessage(e))
        NULL
      }
    )
  })
})

message("\nRegression results (sentiment -> forward returns):")
print(reg_results, n = Inf)

write_csv(reg_results, here("output", "tables", "regression_summary.csv"))

# =============================================================
# 3. SENTIMENT QUINTILE ANALYSIS
# =============================================================

message("\n=== 3. Sentiment Quintile Returns ===")

# Assign sentiment quintiles within each bucket
quintile_returns <- panel |>
  filter(!is.na(sent_lag1)) |>
  group_by(bucket) |>
  mutate(
    sent_quintile = ntile(sent_lag1, 5),
    sent_tertile  = ntile(sent_lag1, 3)
  ) |>
  group_by(bucket, sent_quintile) |>
  summarise(
    n_obs          = n(),
    mean_sent      = mean(sent_lag1),
    mean_fwd1      = mean(fwd_return_1,   na.rm = TRUE),
    mean_fwd3      = mean(fwd_return_3,   na.rm = TRUE),
    mean_fwd5      = mean(fwd_return_5,   na.rm = TRUE),
    mean_abnorm1   = mean(fwd_abnormal_1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 5)))

message("Average returns by sentiment quintile and bucket:")
print(quintile_returns, n = Inf)

write_csv(quintile_returns,
          here("output", "tables", "sentiment_quintile_returns.csv"))

# =============================================================
# 4. HIGH SENTIMENT DAY ANALYSIS
# =============================================================

message("\n=== 4. High vs Low Sentiment Days ===")

sentiment_split <- panel |>
  filter(!is.na(sent_lag1)) |>
  mutate(
    sentiment_group = case_when(
      sent_lag1 > 0  ~ "positive_news",
      sent_lag1 < 0  ~ "negative_news",
      TRUE           ~ "neutral_news"
    )
  ) |>
  group_by(bucket, sentiment_group) |>
  summarise(
    n_obs        = n(),
    mean_fwd1    = mean(fwd_return_1,   na.rm = TRUE),
    mean_fwd3    = mean(fwd_return_3,   na.rm = TRUE),
    mean_abnorm1 = mean(fwd_abnormal_1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 5)))

print(sentiment_split, n = Inf)
write_csv(sentiment_split, here("output", "tables", "sentiment_split_returns.csv"))

# =============================================================
# 5. MAIN TABLEAU EXPORT — all features in one flat file
# =============================================================

message("\n=== 5. Building Tableau Export ===")

tableau_export <- panel |>
  mutate(
    # Readable bucket labels for Tableau
    bucket_label = case_when(
      bucket == "retail_favorite"        ~ "Retail Favorite",
      bucket == "large_cap_institutional" ~ "Large Cap",
      bucket == "mid_attention"           ~ "Mid Attention"
    ),
    # Sentiment category
    news_sentiment_cat = case_when(
      mean_sentiment > 0.02  ~ "Positive",
      mean_sentiment < -0.02 ~ "Negative",
      TRUE                   ~ "Neutral"
    ),
    # Return magnitude category
    return_cat = case_when(
      fwd_return_1 >  0.03 ~ "Strong Up (>3%)",
      fwd_return_1 >  0.01 ~ "Up (1-3%)",
      fwd_return_1 > -0.01 ~ "Flat",
      fwd_return_1 > -0.03 ~ "Down (1-3%)",
      TRUE                 ~ "Strong Down (>3%)"
    ),
    # Month label
    month = floor_date(date, "month"),
    year_month = format(date, "%Y-%m")
  ) |>
  select(
    # Identifiers
    ticker, date, month, year_month, bucket, bucket_label, sector,
    personal_holding,
    # Price
    close, adjusted, volume, daily_return, abnormal_return,
    # Forward returns (prediction targets)
    fwd_return_1, fwd_return_3, fwd_return_5,
    fwd_abnormal_1,
    # Current sentiment
    n_headlines, mean_sentiment, sentiment_ratio,
    news_sentiment_cat, return_cat,
    # Lagged sentiment (predictors)
    sent_lag1, sent_lag3, sent_lag5,
    ratio_lag1, ratio_lag3,
    news_lag1, sent_roll5,
    # StockTwits
    n_bullish, n_bearish, bull_ratio
  )

write_csv(tableau_export, here("output", "tables", "tableau_export.csv"))
message("Tableau export: ", nrow(tableau_export), " rows, ",
        ncol(tableau_export), " columns")
message("Saved to output/tables/tableau_export.csv")

# =============================================================
# 6. EXPLORATORY PLOTS
# =============================================================

message("\n=== 6. Saving exploratory plots ===")

# Plot 1: Correlation heatmap by bucket
p1 <- corr_by_ticker |>
  select(ticker, bucket, cor_sent_fwd1, cor_sent_fwd3, cor_ratio_fwd1) |>
  pivot_longer(cols = c(cor_sent_fwd1, cor_sent_fwd3, cor_ratio_fwd1),
               names_to = "metric", values_to = "correlation") |>
  ggplot(aes(metric, reorder(ticker, correlation), fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  facet_wrap(~ bucket, scales = "free_y") +
  labs(title = "Sentiment-Return Correlations by Ticker",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("output", "figures", "06_correlation_heatmap.png"),
       p1, width = 14, height = 10, dpi = 120)

# Plot 2: Quintile returns by bucket
p2 <- quintile_returns |>
  ggplot(aes(sent_quintile, mean_fwd1, fill = bucket)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ bucket) +
  labs(title = "Average Next-Day Return by Sentiment Quintile",
       subtitle = "Q1 = most negative sentiment, Q5 = most positive",
       x = "Sentiment Quintile", y = "Mean Forward 1-Day Return") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here("output", "figures", "06_quintile_returns.png"),
       p2, width = 12, height = 5, dpi = 120)

# Plot 3: Return volatility by bucket
p3 <- panel |>
  ggplot(aes(daily_return, fill = bucket)) +
  geom_histogram(bins = 80, alpha = 0.7) +
  facet_wrap(~ bucket, ncol = 1, scales = "free_y") +
  coord_cartesian(xlim = c(-0.15, 0.15)) +
  labs(title = "Daily Return Distribution by Bucket",
       x = "Daily Return", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here("output", "figures", "06_return_distributions.png"),
       p3, width = 10, height = 8, dpi = 120)

message("Plots saved to output/figures/")
message("\n=== Pipeline complete. Load tableau_export.csv into Tableau. ===")
