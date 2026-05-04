# =============================================================
# 05_validate.R
# -------------------------------------------------------------
# Produces the required data validation table for the merged
# panel. Checks expected rules, flags violations, and writes
# a clean validation_table.csv to data/validation/.
#
# Validation checks:
#   1. Date completeness per ticker
#   2. Price sanity (no negatives, no extreme moves)
#   3. Sentiment score range
#   4. Join integrity (no silent NAs from merge)
#   5. Volume consistency
#   6. Forward return plausibility
#   7. Duplicate ticker-date pairs
#   8. Bucket coverage (all 3 buckets present)
# =============================================================

library(tidyverse)
library(arrow)
library(here)
library(janitor)

panel <- read_parquet(here("data", "processed", "panel_final.parquet"))

message("Loaded panel: ", nrow(panel), " rows, ", ncol(panel), " columns")
message("Running validation checks...\n")

dir.create(here("data", "validation"), showWarnings = FALSE, recursive = TRUE)

# Helper: log a check result
results <- list()

log_check <- function(check_name, n_violations, n_total, details = "") {
  status <- if (n_violations == 0) "PASS" else "FAIL"
  pct    <- round(n_violations / n_total * 100, 2)
  message(glue::glue("[{status}] {check_name}: {n_violations}/{n_total} violations ({pct}%)"),
          if (details != "") glue::glue(" -- {details}"))
  list(
    check        = check_name,
    status       = status,
    n_violations = n_violations,
    n_total      = n_total,
    pct_flagged  = pct,
    details      = details
  )
}

# ---- Check 1: Duplicate ticker-date pairs ----
dupes <- panel |>
  count(ticker, date) |>
  filter(n > 1)

results[[1]] <- log_check(
  "No duplicate ticker-date pairs",
  nrow(dupes), nrow(panel),
  if (nrow(dupes) > 0) paste(dupes$ticker[1:min(3, nrow(dupes))], collapse = ", ") else ""
)

# ---- Check 2: No negative prices ----
neg_prices <- panel |>
  filter(close <= 0 | adjusted <= 0)

results[[2]] <- log_check(
  "No negative or zero prices",
  nrow(neg_prices), nrow(panel)
)

# ---- Check 3: Extreme single-day price moves (>50%) ----
extreme_moves <- panel |>
  filter(abs(daily_return) > 0.50, !is.na(daily_return))

results[[3]] <- log_check(
  "No extreme daily moves (>50%)",
  nrow(extreme_moves), nrow(panel),
  if (nrow(extreme_moves) > 0)
    paste(extreme_moves |> arrange(desc(abs(daily_return))) |>
            slice(1:min(3, nrow(extreme_moves))) |>
            mutate(s = paste0(ticker, " ", date, " ", round(daily_return*100,1), "%")) |>
            pull(s), collapse = "; ")
  else ""
)

# ---- Check 4: Sentiment score range [-1, 1] ----
sent_out_of_range <- panel |>
  filter(mean_sentiment < -1 | mean_sentiment > 1)

results[[4]] <- log_check(
  "Sentiment scores in [-1, 1]",
  nrow(sent_out_of_range), nrow(panel)
)

# ---- Check 5: Volume > 0 on trading days ----
zero_volume <- panel |>
  filter(volume <= 0)

results[[5]] <- log_check(
  "Volume > 0 on trading days",
  nrow(zero_volume), nrow(panel)
)

# ---- Check 6: All 30 tickers present ----
expected_tickers <- 30
actual_tickers   <- n_distinct(panel$ticker)
missing_tickers  <- setdiff(
  c("NVDA","TSLA","GME","AMC","PLTR","AMD","COIN","HOOD","SOFI","RIVN",
    "AAPL","MSFT","LLY","JPM","JNJ","PG","KO","WMT","UNH","V",
    "VKTX","DKNG","RBLX","SNAP","AFRM","CHWY","BYND","F","DAL","U"),
  unique(panel$ticker)
)

results[[6]] <- log_check(
  "All 30 tickers present",
  length(missing_tickers), expected_tickers,
  if (length(missing_tickers) > 0) paste(missing_tickers, collapse = ", ") else ""
)

# ---- Check 7: All 3 buckets present ----
expected_buckets <- c("retail_favorite", "large_cap_institutional", "mid_attention")
missing_buckets  <- setdiff(expected_buckets, unique(panel$bucket))

results[[7]] <- log_check(
  "All 3 buckets present",
  length(missing_buckets), 3,
  if (length(missing_buckets) > 0) paste(missing_buckets, collapse = ", ") else ""
)

# ---- Check 8: Date range within expected window ----
expected_start <- Sys.Date() %m-% months(12) - 5  # 5-day buffer
expected_end   <- Sys.Date()

early_dates <- panel |> filter(date < expected_start)
late_dates  <- panel |> filter(date > expected_end)

results[[8]] <- log_check(
  "All dates within 12-month window",
  nrow(early_dates) + nrow(late_dates), nrow(panel)
)

# ---- Check 9: Forward returns plausible (< 200%) ----
implausible_fwd <- panel |>
  filter(abs(fwd_return_5) > 2.0, !is.na(fwd_return_5))

results[[9]] <- log_check(
  "Forward 5-day returns plausible (<200%)",
  nrow(implausible_fwd), nrow(panel),
  if (nrow(implausible_fwd) > 0)
    paste(implausible_fwd$ticker[1:min(3,nrow(implausible_fwd))], collapse = ", ")
  else ""
)

# ---- Check 10: Consistent trading days across tickers ----
days_per_ticker <- panel |>
  count(ticker, name = "n_days")

expected_days <- median(days_per_ticker$n_days)
outlier_tickers <- days_per_ticker |>
  filter(abs(n_days - expected_days) > 10)

results[[10]] <- log_check(
  "Consistent trading day count across tickers (±10 days)",
  nrow(outlier_tickers), nrow(days_per_ticker),
  if (nrow(outlier_tickers) > 0)
    paste(outlier_tickers$ticker, "(", outlier_tickers$n_days, "days)", collapse = "; ")
  else glue::glue("median = {expected_days} days")
)

# ---- Compile validation table ----
validation_table <- bind_rows(results) |>
  select(check, status, n_violations, n_total, pct_flagged, details)

message("\n=== Validation Summary ===")
print(validation_table, n = Inf)

n_pass <- sum(validation_table$status == "PASS")
n_fail <- sum(validation_table$status == "FAIL")
message(glue::glue("\n{n_pass}/{nrow(validation_table)} checks passed, {n_fail} failed"))

# ---- Save ----
write_csv(validation_table, here("data", "validation", "validation_table.csv"))
message("Saved data/validation/validation_table.csv")

# ---- Descriptive summary for README / presentation ----
message("\n=== Descriptive Panel Summary ===")
message("Observations: ", nrow(panel))
message("Tickers: ", n_distinct(panel$ticker))
message("Date range: ", min(panel$date), " to ", max(panel$date))
message("Trading days: ", n_distinct(panel$date))

message("\nMissing value rates (key columns):")
panel |>
  select(daily_return, fwd_return_1, mean_sentiment,
         sent_lag1, n_headlines, bull_ratio) |>
  summarise(across(everything(),
                   ~ round(mean(is.na(.x)) * 100, 1))) |>
  pivot_longer(everything(), names_to = "column", values_to = "pct_missing") |>
  print()

message("\nDone. Next: scripts/06_features_analysis.R")
