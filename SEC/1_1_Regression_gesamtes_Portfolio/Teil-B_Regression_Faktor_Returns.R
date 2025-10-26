#####============================================================
#####  PART B – Monthly Portfolio Returns + FF-3-Factor-Regression
#####============================================================

# ── Libraries ─────────────────────────────────────────────────
library(dplyr)
library(lubridate)
library(purrr)
library(tidyquant)
library(progressr)
library(readr)
library(broom)

handlers(global = TRUE)
setwd("C:/_Maximilian_Peter_Masterarbeit_2025_/code/1_1_Regression_gesamtes_Portfolio")

# ── 1. Basisdaten laden ───────────────────────────────────────
geo_data_combined <- readRDS("geo_data_combined.rds")

# ── 2. Portfolio-Konstitution (Small-Value) pro Jahr ──────────
years <- 2013:2022

portfolios <- map(years, function(y) {
  geo_data_combined %>%
    filter(Year == y, !is.na(ticker),
           Value >= 3e8, BookToMarket > 0, BookToMarket < 10) %>%
    distinct(CIK, .keep_all = TRUE) %>%
    {             # 20-80-% Trimmen je Dimension
      filter(.,
             between(BookToMarket,
                     quantile(BookToMarket, .2, na.rm = TRUE),
                     quantile(BookToMarket, .8, na.rm = TRUE)),
             between(Value,
                     quantile(Value, .2, na.rm = TRUE),
                     quantile(Value, .8, na.rm = TRUE)))
    } %>%
    {             # Small & High-B/M-Tilt
      size_med <- median(.$Value, na.rm = TRUE)
      btm_70   <- quantile(.$BookToMarket, .7, na.rm = TRUE)
      filter(., Value < size_med, BookToMarket >= btm_70) %>%
        mutate(Year = y)
    }
})
portfolio_combined <- bind_rows(portfolios)
tickers_set <- unique(portfolio_combined$ticker)

# ── 3. Helper: Monatsserie mit genau *einem* Delisting-Return −100 % ───
get_monthly_series <- function(ticker, start_date, end_date) {
  raw <- tryCatch(
    tq_get(ticker, from = start_date, to = end_date,
           get = "stock.prices"),
    error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0 || !"adjusted" %in% names(raw)) return(NULL)
  
  monthly_last <- raw %>%
    group_by(month = floor_date(date, "month")) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(Return = adjusted / lag(adjusted) - 1)
  
  first_month <- min(monthly_last$month)
  cal <- tibble(month = seq.Date(first_month,
                                 floor_date(end_date, "month"),
                                 by = "month"))
  
  full <- cal %>%
    left_join(monthly_last, by = "month") %>%
    arrange(month)
  
  last_obs <- max(which(!is.na(full$Return)))
  if (last_obs < nrow(full)) {
    full$Return[last_obs + 1] <- -1
    full <- full[1:(last_obs + 1), ]
  }
  
  full %>%
    mutate(ticker = ticker) %>%
    dplyr::select(ticker, month, Return)
}

# ── 4. Gleichgewichtete Portfolio-Rendite (je Ticker) ─────────────
monthly_returns_raw <- with_progress({
  p <- progressor(steps = length(tickers_set))
  map_dfr(tickers_set, \(tkr) {
    p(); get_monthly_series(tkr, as.Date("2013-01-01"), as.Date("2023-01-01"))
  })
})

# ── 5. Ersten & letzten Monats-Return je Ticker streichen ────────
monthly_returns_trim <- monthly_returns_raw %>%
  group_by(ticker) %>%
  filter(row_number() != 1, row_number() != n()) %>%
  ungroup()

# ── 6. Gleichgewichtetes Portfolio bilden ────────────────────────
portfolio_final <- monthly_returns_trim %>%
  group_by(month) %>%
  summarise(PortfolioReturn = mean(Return, na.rm = TRUE),
            .groups = "drop")

# ── 7. Fama-French-Faktoren einlesen ─────────────────────────────
factors_monthly <- read_delim("F-F_Research_Data_Factors_m.csv",
                              delim = ";", col_types = cols()) %>%
  rename(Month = 1) %>%
  mutate(month = ymd(paste0(str_trim(Month), "01"))) %>%
  select(month, `Mkt-RF`, SMB, HML, RF) %>%
  mutate(across(`Mkt-RF`:RF, ~ as.numeric(str_trim(.x)))) %>%
  drop_na()

# ── 8. Merge & Winsorising ───────────────────────────────────────
ff_data <- portfolio_final %>%
  inner_join(factors_monthly, by = "month") %>%
  mutate(
    PortfolioReturn = PortfolioReturn * 100,
    ExcessReturn    = PortfolioReturn - RF
  ) %>%
  {
    lo <- quantile(.$ExcessReturn, .02, na.rm = TRUE)
    hi <- quantile(.$ExcessReturn, .98, na.rm = TRUE)
    mutate(., ExcessReturn = pmax(pmin(ExcessReturn, hi), lo))
  }

# ── 9. Regression durchführen & Ergebnisse anzeigen ──────────────
model <- lm(ExcessReturn ~ `Mkt-RF` + SMB + HML, data = ff_data)
print(tidy(model, conf.int = TRUE, digits = 3))

