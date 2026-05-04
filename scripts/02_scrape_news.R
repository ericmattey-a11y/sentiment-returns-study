# =============================================================
# 02_scrape_news.R (v2 - Alpaca News API)
# -------------------------------------------------------------
# Pulls dated financial news headlines for all 30 tickers
# using the Alpaca News API (httr2). Alpaca provides ticker-
# tagged headlines with full timestamps going back 2+ years.
#
# Also scrapes StockTwits public pages for mention volume
# and bull/bear sentiment (web scraping via rvest).
#
# Outputs:
#   data/raw/news_raw.parquet        -- dated headlines per ticker
#   data/raw/stocktwits_raw.parquet  -- ST mention + sentiment
#
# Runtime: ~10-15 minutes (API rate limits + ST scrape delays)
# =============================================================

library(tidyverse)
library(httr2)
library(rvest)
library(jsonlite)
library(lubridate)
library(arrow)
library(glue)
library(here)
library(janitor)

# ---- Credentials ----
alpaca_key    <- Sys.getenv("ALPACA_KEY_ID")
alpaca_secret <- Sys.getenv("ALPACA_SECRET_KEY")

if (alpaca_key == "" || alpaca_secret == "") {
  readRenviron(paste0(here::here(), "/.Renviron"))
  alpaca_key    <- Sys.getenv("ALPACA_KEY_ID")
  alpaca_secret <- Sys.getenv("ALPACA_SECRET_KEY")
}

# ---- Load tickers ----
tickers_df <- read_csv(here("tickers.csv"), show_col_types = FALSE) |>
  clean_names()

study_tickers <- tickers_df$ticker

dir.create(here("data", "raw"), showWarnings = FALSE, recursive = TRUE)

# Null coalescing helper
`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================
# PART A: ALPACA NEWS API
# =============================================================

message("\n=== PART A: Alpaca News API ===")

ALPACA_NEWS_URL <- "https://data.alpaca.markets/v1beta1/news"

end_date   <- Sys.Date()
start_date <- end_date %m-% months(12)

message("Window: ", start_date, " to ", end_date)
message("Pulling news for ", length(study_tickers), " tickers...\n")

pull_alpaca_news <- function(ticker, start, end) {
  all_articles <- list()
  page_token   <- NULL
  page_num     <- 1

  repeat {
    Sys.sleep(0.3)

    query_params <- list(
      symbols = ticker,
      start   = format(as.POSIXct(start), "%Y-%m-%dT00:00:00Z"),
      end     = format(as.POSIXct(end),   "%Y-%m-%dT23:59:59Z"),
      limit   = 50,
      sort    = "desc"
    )

    if (!is.null(page_token)) {
      query_params$page_token <- page_token
    }

    req <- request(ALPACA_NEWS_URL) |>
      req_headers(
        "APCA-API-KEY-ID"     = alpaca_key,
        "APCA-API-SECRET-KEY" = alpaca_secret
      ) |>
      req_url_query(!!!query_params) |>
      req_timeout(15)

    resp <- tryCatch(
      req_perform(req),
      error = function(e) {
        message("  Request error for ", ticker, ": ", conditionMessage(e))
        return(NULL)
      }
    )

    if (is.null(resp)) break
    if (resp_status(resp) != 200) {
      message("  HTTP ", resp_status(resp), " for ", ticker)
      break
    }

    body     <- resp |> resp_body_json()
    articles <- body$news

    if (length(articles) == 0) break

    all_articles <- c(all_articles, articles)

    page_token <- body$next_page_token
    if (is.null(page_token) || page_token == "") break

    page_num <- page_num + 1
    if (page_num > 20) break
  }

  if (length(all_articles) == 0) return(NULL)

  map_dfr(all_articles, function(a) {
    tibble(
      ticker     = ticker,
      article_id = as.character(a$id %||% NA),
      headline   = a$headline %||% NA_character_,
      source     = a$source %||% NA_character_,
      summary    = a$summary %||% NA_character_,
      url        = a$url %||% NA_character_,
      created_at = a$created_at %||% NA_character_
    )
  })
}

news_list <- vector("list", length(study_tickers))

for (i in seq_along(study_tickers)) {
  ticker <- study_tickers[i]
  message(glue("[{i}/{length(study_tickers)}] {ticker}..."), appendLF = FALSE)

  news_list[[i]] <- tryCatch(
    pull_alpaca_news(ticker, start_date, end_date),
    error = function(e) {
      message(" FAILED: ", conditionMessage(e))
      NULL
    }
  )

  n <- nrow(news_list[[i]])
  message(glue(" {if (!is.null(n)) n else 0} articles"))
}

news_raw <- bind_rows(news_list) |>
  mutate(
    created_at = ymd_hms(created_at, quiet = TRUE),
    date       = as.Date(created_at)
  ) |>
  filter(!is.na(headline)) |>
  distinct(ticker, article_id, .keep_all = TRUE)

message("\n--- Alpaca news results ---")
message("Total headlines: ", nrow(news_raw))
message("Date range: ", min(news_raw$date, na.rm = TRUE),
        " to ", max(news_raw$date, na.rm = TRUE))

message("\nHeadlines per ticker:")
news_raw |>
  count(ticker, name = "n_headlines") |>
  arrange(n_headlines) |>
  print(n = Inf)

write_parquet(news_raw, here("data", "raw", "news_raw.parquet"))
message("Saved to data/raw/news_raw.parquet")

# =============================================================
# PART B: STOCKTWITS API (web scrape fallback)
# =============================================================

message("\n=== PART B: StockTwits ===")

scrape_stocktwits <- function(ticker) {
  Sys.sleep(1.5)

  url  <- glue("https://api.stocktwits.com/api/2/streams/symbol/{ticker}.json")
  resp <- tryCatch(
    request(url) |> req_timeout(10) |> req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp) || resp_status(resp) != 200) return(NULL)

  data     <- resp |> resp_body_json()
  messages <- data$messages
  if (length(messages) == 0) return(NULL)

  map_dfr(messages, function(msg) {
    sentiment <- msg$entities$sentiment$basic
    tibble(
      ticker     = ticker,
      st_id      = as.character(msg$id),
      created_at = msg$created_at,
      body       = msg$body,
      sentiment  = if (!is.null(sentiment)) sentiment else NA_character_,
      like_count = msg$likes$total %||% 0L
    )
  })
}

st_list <- vector("list", length(study_tickers))

for (i in seq_along(study_tickers)) {
  ticker <- study_tickers[i]
  message(glue("[{i}/{length(study_tickers)}] StockTwits: {ticker}..."))
  st_list[[i]] <- tryCatch(scrape_stocktwits(ticker), error = function(e) NULL)
}

st_raw <- bind_rows(st_list) |>
  mutate(
    created_at = ymd_hms(created_at, quiet = TRUE),
    date       = as.Date(created_at)
  ) |>
  filter(!is.na(body))

message("\n--- StockTwits results ---")
message("Total messages: ", nrow(st_raw))
st_raw |> count(sentiment) |> print()

write_parquet(st_raw, here("data", "raw", "stocktwits_raw.parquet"))
message("Saved to data/raw/stocktwits_raw.parquet")
message("\n=== Done. Next: scripts/03_sentiment_extraction.R ===")
