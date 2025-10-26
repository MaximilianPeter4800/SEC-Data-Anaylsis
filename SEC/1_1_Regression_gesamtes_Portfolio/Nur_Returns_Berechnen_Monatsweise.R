#####============================================================
#####  Small Cap Value Portfolio + Monthly Performance Export
#####============================================================

# ── Libraries ─────────────────────────────────────────────────
library(dplyr)
library(lubridate)
library(purrr)
library(tidyquant)
library(progressr)
library(readr)

# ── Progress bar aktivieren ───────────────────────────────────
handlers(global = TRUE)

# ── 1️⃣ Arbeitsverzeichnis festlegen ───────────────────────────
setwd("C:/_Maximilian_Peter_Masterarbeit_2025_/code/1_1_Regression_gesamtes_Portfolio")

# ── 2️⃣ Deine Basisdaten laden ────────────────────────────────
geo_data_combined <- readRDS("geo_data_combined.rds")

# ── 3️⃣ Portfolio-Konstitution (Small-Cap-Value) ──────────────
years <- 2013:2022

portfolios <- map(years, function(y) {
  geo_data_combined %>%
    filter(Year == y, !is.na(ticker),
           Value >= 3e8, BookToMarket > 0, BookToMarket < 10) %>%
    distinct(CIK, .keep_all = TRUE) %>%
    {
      filter(.,
             between(BookToMarket,
                     quantile(BookToMarket, .2, na.rm = TRUE),
                     quantile(BookToMarket, .8, na.rm = TRUE)),
             between(Value,
                     quantile(Value, .2, na.rm = TRUE),
                     quantile(Value, .8, na.rm = TRUE)))
    } %>%
    {
      size_med <- median(.$Value, na.rm = TRUE)
      btm_70   <- quantile(.$BookToMarket, .7, na.rm = TRUE)
      filter(., Value < size_med, BookToMarket >= btm_70) %>%
        mutate(Year = y)
    }
})

portfolio_combined <- bind_rows(portfolios)
tickers_set <- unique(portfolio_combined$ticker)

# ── 4️⃣ Helper: Tages-Returns inkl. Delisting Return ──────────
get_daily_series <- function(ticker, start_date, end_date) {
  raw <- tryCatch(
    tq_get(ticker, from = start_date, to = end_date, get = "stock.prices"),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) < 2 || !"adjusted" %in% names(raw)) return(NULL)
  
  daily <- raw %>%
    arrange(date) %>%
    mutate(Return = adjusted / lag(adjusted) - 1) %>%
    filter(!is.na(Return))
  
  # Hier kein Delisting Return nötig — optional:
  # daily <- bind_rows(daily, tibble(date = max(daily$date) + 1, Return = -1))
  
  daily %>%
    mutate(ticker = ticker) %>%
    select(ticker, date, Return)
}

# ── 5️⃣ Alle Tages-Returns abrufen ─────────────────────────────
daily_returns_raw <- with_progress({
  p <- progressor(steps = length(tickers_set))
  map_dfr(tickers_set, \(tkr) {
    p(); get_daily_series(tkr, as.Date("2013-01-01"), as.Date("2023-01-01"))
  })
})

# ── 6️⃣ Optional: ersten und letzten Tag streichen ─────────────
daily_returns_trim <- daily_returns_raw %>%
  group_by(ticker) %>%
  filter(row_number() != 1) %>%  # nur ersten Tag streichen
  ungroup()

# ── 7️⃣ Gleichgewichtete Portfolio-Tagesrendite ────────────────
portfolio_final_daily <- daily_returns_trim %>%
  group_by(date) %>%
  summarise(PortfolioReturn = mean(Return, na.rm = TRUE), .groups = "drop")

# ── 8️⃣ In CSV speichern ──────────────────────────────────────
portfolio_export_daily <- portfolio_final_daily %>%
  mutate(Date = as.Date(date)) %>%
  select(Date, PortfolioReturn) %>%
  arrange(Date)

write_csv(portfolio_export_daily, "Portfolio_Daily_Returns.csv")