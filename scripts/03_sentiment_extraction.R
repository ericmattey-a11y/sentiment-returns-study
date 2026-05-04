# =============================================================
# 03_sentiment_extraction.R
# -------------------------------------------------------------
# Scores the 2,977 Finviz headlines using the Loughran-McDonald
# (LM) financial sentiment dictionary via the tidytext package.
#
# LM is purpose-built for financial text and is the standard
# in academic financial NLP research. Words like "liability",
# "risk", and "loss" are correctly scored as negative here —
# whereas general dictionaries like AFINN treat them as neutral.
#
# Method:
#   1. Tokenize each headline into words
#   2. Join against LM dictionary (Positive / Negative categories)
#   3. Compute net sentiment score per headline
#   4. Aggregate to daily ticker-level score
#
# Output:
#   data/raw/sentiment_scores.parquet   -- headline-level scores
#   data/processed/daily_sentiment.parquet -- daily ticker scores
# =============================================================

library(tidyverse)
library(tidytext)
library(lubridate)
library(arrow)
library(here)
library(janitor)

# ---- 1. Load headlines ----
news_raw <- read_parquet(here("data", "raw", "news_raw.parquet"))

message("Loaded ", nrow(news_raw), " headlines across ",
        n_distinct(news_raw$ticker), " tickers")

# Drop rows with no date (can't place them in the time series)
news_clean <- news_raw |>
  filter(!is.na(date), !is.na(headline)) |>
  mutate(headline_id = row_number())

message("Headlines with valid date: ", nrow(news_clean))

# ---- 2. Load Loughran-McDonald dictionary ----
# tidytext ships this — no download needed
lm_dict <- get_sentiments("loughran")

message("\nLM dictionary categories available:")
lm_dict |> count(sentiment) |> print()

# We focus on Positive and Negative for a clean [-1, +1] score
lm_pn <- lm_dict |>
  filter(sentiment %in% c("positive", "negative")) |>
  mutate(score = if_else(sentiment == "positive", 1L, -1L))

# ---- 3. Tokenize headlines ----
tokens <- news_clean |>
  select(headline_id, ticker, date, headline) |>
  unnest_tokens(word, headline) |>
  # Remove stop words and numbers
  anti_join(stop_words, by = "word") |>
  filter(!str_detect(word, "^\\d+$"))

message("\nTotal tokens after cleaning: ", nrow(tokens))

# ---- 4. Join against LM dictionary ----
scored_tokens <- tokens |>
  inner_join(lm_pn, by = "word")

message("Tokens matched in LM dictionary: ", nrow(scored_tokens))
message("Match rate: ",
        round(nrow(scored_tokens) / nrow(tokens) * 100, 1), "%")

# ---- 5. Score per headline ----
headline_scores <- news_clean |>
  select(headline_id, ticker, date, headline) |>
  left_join(
    scored_tokens |>
      group_by(headline_id) |>
      summarise(
        n_positive    = sum(score == 1),
        n_negative    = sum(score == -1),
        n_scored_words = n(),
        raw_score     = sum(score),
        .groups = "drop"
      ),
    by = "headline_id"
  ) |>
  mutate(
    across(c(n_positive, n_negative, n_scored_words, raw_score),
           ~ replace_na(.x, 0L)),
    # Normalized sentiment: raw_score / total words in headline
    # Bounded to [-1, +1] by construction when all words are scored
    n_words = str_count(headline, "\\S+"),
    sentiment_score = if_else(
      n_words > 0,
      raw_score / n_words,
      0
    ),
    # Binary label for quick reference
    sentiment_label = case_when(
      sentiment_score > 0  ~ "positive",
      sentiment_score < 0  ~ "negative",
      TRUE                 ~ "neutral"
    )
  )

message("\n--- Headline-level sentiment distribution ---")
headline_scores |> count(sentiment_label) |> print()

message("\nSentiment score summary:")
summary(headline_scores$sentiment_score) |> print()

# ---- 6. Aggregate to daily ticker level ----
daily_sentiment <- headline_scores |>
  group_by(ticker, date) |>
  summarise(
    n_headlines        = n(),
    mean_sentiment     = mean(sentiment_score),
    median_sentiment   = median(sentiment_score),
    n_positive_headlines = sum(sentiment_label == "positive"),
    n_negative_headlines = sum(sentiment_label == "negative"),
    n_neutral_headlines  = sum(sentiment_label == "neutral"),
    # Sentiment ratio: (pos - neg) / total, ranges [-1, +1]
    sentiment_ratio    = (n_positive_headlines - n_negative_headlines) / n_headlines,
    .groups = "drop"
  )

message("\n--- Daily sentiment panel ---")
message("Rows (ticker-date pairs): ", nrow(daily_sentiment))
message("Date range: ", min(daily_sentiment$date), " to ", max(daily_sentiment$date))

message("\nAverage headlines per ticker-day:")
daily_sentiment |>
  group_by(ticker) |>
  summarise(
    n_days        = n(),
    avg_headlines = mean(n_headlines)
  ) |>
  arrange(avg_headlines) |>
  print(n = Inf)

# ---- 7. Also aggregate StockTwits bull/bear by ticker ----
message("\n--- StockTwits sentiment summary ---")
st_raw <- read_parquet(here("data", "raw", "stocktwits_raw.parquet"))

st_summary <- st_raw |>
  filter(!is.na(sentiment)) |>
  group_by(ticker) |>
  summarise(
    n_bullish   = sum(sentiment == "Bullish"),
    n_bearish   = sum(sentiment == "Bearish"),
    n_labeled   = n(),
    bull_ratio  = n_bullish / n_labeled,
    .groups = "drop"
  )

message("StockTwits bull ratio by ticker:")
st_summary |> arrange(bull_ratio) |> print(n = Inf)

# ---- 8. Save outputs ----
dir.create(here("data", "raw"),       showWarnings = FALSE, recursive = TRUE)
dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)

write_parquet(headline_scores,  here("data", "raw", "sentiment_scores.parquet"))
write_parquet(daily_sentiment,  here("data", "processed", "daily_sentiment.parquet"))
write_parquet(st_summary,       here("data", "processed", "stocktwits_summary.parquet"))

message("\nSaved:")
message("  data/raw/sentiment_scores.parquet")
message("  data/processed/daily_sentiment.parquet")
message("  data/processed/stocktwits_summary.parquet")
message("\nDone. Next: scripts/04_clean_merge.R")

