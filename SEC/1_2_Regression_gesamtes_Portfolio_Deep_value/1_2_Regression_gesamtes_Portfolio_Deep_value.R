# ── Libraries ────────────────────────────────────────────────
library(dplyr)
library(lubridate)
library(purrr)
library(tidyquant)
library(progressr)
library(readr)
library(broom)

handlers(global = TRUE)
setwd("C:/_Maximilian_Peter_Masterarbeit_2025_/code/1_2_Regression_gesamtes_Portfolio_Deep_value")

# ── 1. Basisdaten laden ─────────────────────────────────────
geo_data_combined <- readRDS("geo_data_combined.rds")

# ── 2. Portfolio-Konstitution (Small Deep-Value) pro Jahr ───
years <- 2013:2022

portfolios_new <- map(years, function(y) {
  geo_data_combined %>%
    filter(
      Year == y,
      !is.na(ticker),
      Value > 0,
      BookToMarket > 0, BookToMarket < 10
    ) %>%
    distinct(CIK, .keep_all = TRUE) %>%
    {
      size_80 <- quantile(.$Value, 0.8, na.rm = TRUE)
      btm_99  <- quantile(.$BookToMarket, 0.99, na.rm = TRUE)
      filter(., Value < size_80, BookToMarket >= btm_99) %>%
        mutate(PortfolioYear = y)
    }
})

portfolio_combined_new <- bind_rows(portfolios_new)
tickers_set_new <- unique(portfolio_combined_new$ticker)

# ── 3. Helper: Monatsserie mit *einem* Delisting-Return −100 % ───
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

# ── 4. Monatsrenditen für ALLE Ticker laden ────────────────
monthly_returns_all <- with_progress({
  p <- progressor(steps = length(tickers_set_new))
  map_dfr(tickers_set_new, \(tkr) {
    p(); get_monthly_series(tkr, as.Date("2013-01-01"), as.Date("2023-01-01"))
  })
})

# ── 5. Pro Monat nur Ticker des jeweiligen Jahrgangs verwenden ───
portfolio_def <- portfolio_combined_new %>%
  select(ticker, PortfolioYear)

monthly_returns_valid <- monthly_returns_all %>%
  mutate(Year = year(month)) %>%
  left_join(portfolio_def, by = "ticker") %>%
  filter(Year == PortfolioYear)

# ── 6. Ersten Monats-Return je Ticker streichen ──
monthly_returns_trim <- monthly_returns_valid %>%
  group_by(ticker) %>%
  filter(row_number() != 1) %>%  # only remove the first month, keep last!
  ungroup()

# ── 7. Gleichgewichtetes Portfolio bilden ──────────────────
portfolio_final <- monthly_returns_trim %>%
  group_by(month) %>%
  summarise(PortfolioReturn = mean(Return, na.rm = TRUE),
            .groups = "drop")

# ── 8. Fama-French-Faktoren einlesen ───────────────────────
factors_monthly <- read_delim("F-F_Research_Data_Factors_m.csv",
                              delim = ";", col_types = cols()) %>%
  rename(Month = 1) %>%
  mutate(month = ymd(paste0(str_trim(Month), "01"))) %>%
  select(month, `Mkt-RF`, SMB, HML, RF) %>%
  mutate(across(`Mkt-RF`:RF, ~ as.numeric(str_trim(.x)))) %>%
  drop_na()

# ── 9. Merge & Baseline ─────────────────────────────────────
ff_data <- portfolio_final %>%
  inner_join(factors_monthly, by = "month") %>%
  mutate(
    PortfolioReturn = PortfolioReturn * 100,
    ExcessReturn = PortfolioReturn - RF
  )

# ── 10. Optional: Winsorising separat ──────────────────────
ff_data_winsor <- ff_data %>%
  {
    lo <- quantile(.$ExcessReturn, 0.02, na.rm = TRUE)
    hi <- quantile(.$ExcessReturn, 0.98, na.rm = TRUE)
    mutate(., ExcessReturn = pmax(pmin(ExcessReturn, hi), lo))
  }

# ── 11. Regression (z.B. mit Winsorized) ───────────────────
model_winsor <- lm(ExcessReturn ~ `Mkt-RF` + SMB + HML, data = ff_data_winsor)
print(tidy(model_winsor, conf.int = TRUE, digits = 3))

# ── 12. Regression (z.B. mit unbeschnittenen Daten) ─────────
model_raw <- lm(ExcessReturn ~ `Mkt-RF` + SMB + HML, data = ff_data)
print(tidy(model_raw, conf.int = TRUE, digits = 3))
