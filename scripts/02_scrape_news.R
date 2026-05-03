# =============================================================
# 02_scrape_news.R
# -------------------------------------------------------------
# Two sources in one script:
#   A) Finviz news headlines per ticker (web scraping via rvest)
#   B) StockTwits recent mention volume + sentiment per ticker
#      (public unauthenticated API, recent window only)
#
# Outputs:
#   data/raw/news_raw.parquet       -- headlines with timestamps
#   data/raw/stocktwits_raw.parquet -- mention counts + bull/bear
#
# Runtime: ~20-30 min (intentional sleep delays to be polite)
# Finviz depth: varies by ticker (~30-200 headlines per name)
# StockTwits: most recent ~30 posts per ticker (recent window)
# =============================================================

library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(lubridate)
library(arrow)
library(glue)
library(here)
library(janitor)

# ---- Setup ----
tickers_df <- read_csv(here("tickers.csv"), show_col_types = FALSE) |>
  clean_names()

study_tickers <- tickers_df$ticker

dir.create(here("data", "raw"), showWarnings = FALSE, recursive = TRUE)

# =============================================================
# PART A: FINVIZ NEWS SCRAPER
# =============================================================

message("\n=== PART A: Finviz News Headlines ===")
message("Scraping ", length(study_tickers), " tickers with 2s delay between requests")
message("Estimated time: ", round(length(study_tickers) * 2.5 / 60, 1), " minutes\n")

# Helper: parse Finviz timestamp strings to Date
# Finviz uses mixed formats: "May-01-25 06:30PM", "Today 08:00AM", "2 hours ago"
parse_finviz_time <- function(raw_time) {
  raw_time <- str_trim(raw_time)
  today <- Sys.Date()

  # Format: "May-01-25 06:30PM"
  if (str_detect(raw_time, "^[A-Za-z]{3}-\\d{2}-\\d{2}")) {
    date_part <- str_extract(raw_time, "^[A-Za-z]{3}-\\d{2}-\\d{2}")
    parsed <- tryCatch(
      as.Date(date_part, format = "%b-%d-%y"),
      error = function(e) NA_real_
    )
    return(parsed)
  }

  # Format: "Today HH:MMAM/PM"
  if (str_detect(raw_time, "^Today")) return(today)

  # Format: "X hours ago" / "X minutes ago"
  if (str_detect(raw_time, "hours? ago|minutes? ago")) return(today)

  # Format: "Yesterday"
  if (str_detect(raw_time, "^Yesterday")) return(today - 1)

  return(NA_Date_)
}

# Scraper function for a single ticker
scrape_finviz_news <- function(ticker) {
  Sys.sleep(2)  # polite delay

  url <- glue("https://finviz.com/quote.ashx?t={ticker}&ty=c&ta=1&p=d")

  html <- tryCatch(
    read_html(
      url,
      # Set a realistic user agent to reduce blocking
      config = httr::user_agent(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120.0.0.0 Safari/537.36"
      )
    ),
    error = function(e) {
      message("  ERROR reading ", ticker, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(html)) return(NULL)

  # Find the news table — Finviz uses class "news-link-container" for headlines
  # Try multiple selectors for robustness (site structure can shift slightly)
  news_rows <- html |>
    html_elements("table.fullview-news-outer tr") |>
    suppressWarnings()

  if (length(news_rows) == 0) {
    # Fallback: try the news table by id
    news_rows <- html |>
      html_elements("#news tr") |>
      suppressWarnings()
  }

  if (length(news_rows) == 0) {
    message("  WARNING: No news rows found for ", ticker)
    return(tibble(
      ticker    = ticker,
      headline  = NA_character_,
      source    = NA_character_,
      raw_time  = NA_character_,
      date      = NA_Date_,
      url       = NA_character_
    ))
  }

  # Parse each row
  results <- map_dfr(news_rows, function(row) {
    # Time is in the first <td>, headline + link in the second
    cells <- row |> html_elements("td")
    if (length(cells) < 2) return(NULL)

    raw_time <- cells[[1]] |> html_text(trim = TRUE)
    link_el  <- cells[[2]] |> html_element("a")

    if (is.na(link_el)) return(NULL)

    headline <- link_el |> html_text(trim = TRUE)
    href     <- link_el |> html_attr("href")

    # Source is in a <span> inside the second cell
    source <- cells[[2]] |>
      html_element("span") |>
      html_text(trim = TRUE)
    if (is.na(source)) source <- "Unknown"

    tibble(
      ticker   = ticker,
      headline = headline,
      source   = source,
      raw_time = raw_time,
      url      = href
    )
  })

  if (nrow(results) == 0) return(NULL)

  results |>
    mutate(date = map_vec(raw_time, parse_finviz_time))
}

# Run for all tickers
news_list <- vector("list", length(study_tickers))

for (i in seq_along(study_tickers)) {
  ticker <- study_tickers[i]
  message(glue("[{i}/{length(study_tickers)}] Scraping news for {ticker}..."))

  news_list[[i]] <- tryCatch(
    scrape_finviz_news(ticker),
    error = function(e) {
      message("  FAILED: ", conditionMessage(e))
      NULL
    }
  )
}

news_raw <- bind_rows(news_list) |>
  filter(!is.na(headline)) |>
  distinct(ticker, headline, date, .keep_all = TRUE)

message("\n--- Finviz news results ---")
message("Total headlines scraped: ", nrow(news_raw))
message("Headlines per ticker:")
news_raw |>
  count(ticker, name = "n_headlines") |>
  arrange(n_headlines) |>
  print(n = Inf)

write_parquet(news_raw, here("data", "raw", "news_raw.parquet"))
message("Saved to data/raw/news_raw.parquet")

# =============================================================
# PART B: STOCKTWITS PUBLIC API
# =============================================================

message("\n=== PART B: StockTwits Mention Volume ===")
message("Note: Public endpoint returns most recent ~30 posts only")
message("This gives us current-window volume + bull/bear label counts")
message("Used as a supplementary signal, not the primary 12-month series\n")

scrape_stocktwits <- function(ticker) {
  Sys.sleep(1.5)

  url <- glue("https://api.stocktwits.com/api/2/streams/symbol/{ticker}.json")

  resp <- tryCatch(
    request(url) |>
      req_headers(
        "User-Agent" = "Mozilla/5.0 (educational research project)"
      ) |>
      req_timeout(10) |>
      req_perform(),
    error = function(e) {
      message("  ERROR for ", ticker, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(resp)) return(NULL)
  if (resp_status(resp) != 200) {
    message("  HTTP ", resp_status(resp), " for ", ticker)
    return(NULL)
  }

  data <- resp |> resp_body_json()

  messages <- data$messages
  if (length(messages) == 0) return(NULL)

  map_dfr(messages, function(msg) {
    sentiment <- msg$entities$sentiment$basic
    tibble(
      ticker      = ticker,
      st_id       = msg$id,
      created_at  = msg$created_at,
      body        = msg$body,
      sentiment   = if (!is.null(sentiment)) sentiment else NA_character_,
      like_count  = msg$likes$total %||% 0L
    )
  })
}

# Null coalescing helper
`%||%` <- function(a, b) if (!is.null(a)) a else b

st_list <- vector("list", length(study_tickers))

for (i in seq_along(study_tickers)) {
  ticker <- study_tickers[i]
  message(glue("[{i}/{length(study_tickers)}] StockTwits: {ticker}..."))

  st_list[[i]] <- tryCatch(
    scrape_stocktwits(ticker),
    error = function(e) {
      message("  FAILED: ", conditionMessage(e))
      NULL
    }
  )
}

st_raw <- bind_rows(st_list) |>
  mutate(
    created_at = ymd_hms(created_at, quiet = TRUE),
    date       = as.Date(created_at)
  )

message("\n--- StockTwits results ---")
message("Total messages retrieved: ", nrow(st_raw))
message("Sentiment breakdown:")
st_raw |> count(sentiment) |> print()

message("\nMessages per ticker:")
st_raw |>
  count(ticker, name = "n_messages") |>
  arrange(n_messages) |>
  print(n = Inf)

write_parquet(st_raw, here("data", "raw", "stocktwits_raw.parquet"))
message("Saved to data/raw/stocktwits_raw.parquet")

message("\n=== Done. Next: scripts/03_news_llm_extraction.R ===")
