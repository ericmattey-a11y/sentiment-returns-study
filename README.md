# Do Human Signals Predict Stock Returns?

**ISA 401 — Data Visualization Final Project**

**Author:** Eric Mattey

**Team:** Individual Submission

**Course:** Miami University, Spring 2026

---

## Research Question

**Across a 30-stock universe spanning retail-favorite, institutional, and mid-attention names, do social media sentiment through StockTwits and news-tone shifts predict abnormal returns over the next 1–5 trading days — individually, and combined?**

The motivation is practical: if "human elements" — crowd sentiment and narrative tone — carry predictive power for short-term price moves, they belong in a discretionary or systematic trading model. If they don't, the right move is to ignore them and focus on price action and fundamentals. This project produces evidence either way and applies the findings to a personal portfolio as a secondary analysis.

## Why This Question Is Worth Studying

Behavioral finance research consistently finds that retail attention and narrative shifts move prices in the short run, but the strength of that effect varies dramatically across stocks. A name like GME is dominated by retail crowd dynamics; a name like JNJ is dominated by institutional flows and fundamentals. The interesting question isn't "does sentiment matter?" — it's "*where* does it matter, and at what lag?" That's the question this project answers, and it produces an actionable conclusion rather than a textbook restatement.

## Stock Universe (30 tickers, 3 buckets)

| Bucket | Tickers |
|---|---|
| **Retail-favorite / high attention** | NVDA, TSLA, GME, AMC, PLTR, AMD, COIN, HOOD, SOFI, RIVN |
| **Large-cap institutional** | AAPL, MSFT, LLY, JPM, JNJ, PG, KO, WMT, UNH, V |
| **Mid-attention** | VKTX, DKNG, RBLX, SNAP, AFRM, CHWY, BYND, F, DAL, U |

Five of these are personal holdings (NVDA, AAPL, MSFT, LLY, VKTX), enabling a secondary "applying findings to my portfolio" view in the dashboard. Three other personal holdings (RIO, VALE, UURAF) are excluded from the study due to insufficient retail sentiment coverage and discussed qualitatively as a limitation.

## Data Sources & Acquisition Methods

This project meets the 3-source / 2-method requirement with three distinct acquisition methods:

| # | Source | Acquisition Method | Purpose |
|---|---|---|---|
| 1 | **Yahoo Finance** (via `tidyquant`) | API | Daily OHLCV → returns, abnormal returns vs SPY |
| 2 | **Alpaca Markets News API** (via `httr2`) | API | 16,693 dated headlines → sentiment scoring |
| 3 | **StockTwits** (public stream endpoint) | Web scraping | Mention volume + bull/bear sentiment labels |

Source #3 is the project's methodological differentiator: rather than using a pre-built sentiment dictionary, headlines are passed to Claude via the Anthropic API with a structured-output prompt that returns a tone score (-1 to +1), confidence, and primary topic per headline. This is documented in `scripts/03_news_llm_extraction.R`.

## Time Window

Trailing 12 months of daily data. Provides ~252 trading days × 30 tickers ≈ 7,560 observations as the analytical spine, before lag features are added.

## Workflow & Reproducibility

The full pipeline is reproducible from raw sources. To rebuild the dataset from scratch:

```r
# 1. Clone repo and open the .Rproj file
# 2. Copy .Renviron.example to .Renviron and fill in your API keys
# 3. Install dependencies
source("scripts/00_setup.R")

# 4. Run pipeline scripts in order
source("scripts/01_pull_prices.R")          # ~2 min
source("scripts/02_scrape_news.R")          # ~15 min (rate-limited)
source("scripts/03_sentiment_extraction.R") # ~30 sec (local LM dictionary)
source("scripts/04_clean_merge.R")          # ~1 min
source("scripts/05_validate.R")             # ~30 sec
source("scripts/06_features_analysis.R")    # ~2 min
```

The final merged dataset is written to `data/processed/panel_final.parquet`.

## Repository Structure

```
sentiment-returns-study/
├── README.md                          # this file
├── .gitignore                         # excludes .Renviron, /data, /output
├── .Renviron.example                  # template for required API keys
├── tickers.csv                        # 30-ticker universe with bucket labels
├── scripts/
│   ├── 00_setup.R                     # install/load packages
│   ├── 01_pull_prices.R               # Yahoo Finance via tidyquant
│   ├── 02_scrape_news.R               # Alpaca News API + StockTwits scrape
│   ├── 03_sentiment_extraction.R      # LM dictionary sentiment scoring
│   ├── 04_clean_merge.R               # build merged ticker × date panel
│   ├── 05_validate.R                  # data quality rules + validation table
│   └── 06_features_analysis.R         # abnormal returns, lag features, stats
├── data/
│   ├── raw/                           # raw pulls from each source (gitignored)
│   ├── processed/                     # cleaned + merged panel (gitignored)
│   └── validation/                    # validation tables + quality reports
├── output/
│   ├── figures/                       # exploratory plots from R
│   └── tables/                        # summary statistics, regression output
└── docs/
    └── methodology_notes.md           # design decisions, prompt engineering
```

## Data Validation

`scripts/05_validate.R` produces `data/validation/validation_table.csv` checking:

- **Date completeness** — every trading day in window present per ticker
- **Price sanity** — no negative or zero close prices, no >50% single-day moves without flag
- **Sentiment range** — all sentiment scores in [-1, 1]
- **Join integrity** — every ticker-date in price data has corresponding sentiment row (NA flagged, not silent)
- **Volume consistency** — daily volume > 0 for active tickers

## Security

This repo is configured to never commit secrets:

- API keys live in `.Renviron` (gitignored)
- `.Renviron.example` is committed as a template
- Raw data and processed data are gitignored — only code and validation tables are tracked

## Limitations & Honest Caveats

- **Sample window** — 12 months captures one market regime; findings may not generalize across regimes
- **Sentiment lag** — social media data has unavoidable timestamp imprecision (post time vs market time)
- **Survivorship** — universe is fixed; doesn't account for delisted names
- **News coverage skew** — large-caps get more headlines; sentiment sample size varies by ticker
- **Retail names with thin coverage** — UURAF and similar micro-caps are excluded because the sentiment signal doesn't exist; this is itself a finding

## Deliverables

- **GitHub repo:** https://github.com/ericmattey-a11y/sentiment-returns-study
- **Tableau Public dashboard:** https://public.tableau.com/app/profile/eric.mattey/viz/HumanSignalsPredictStockReturns/S6-Portfolio
- **Recorded technical presentation (YouTube unlisted):** https://www.youtube.com/watch?v=WvHBiEp67Kg

## License

Course project — not licensed for redistribution. Code patterns are MIT-spirit free-to-borrow.
