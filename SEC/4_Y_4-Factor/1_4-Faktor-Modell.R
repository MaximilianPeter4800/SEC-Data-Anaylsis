# ── Libraries ─────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyquant)
library(readr)
library(broom)
library(ggplot2)

# ── Set Working Directory ─────────────────────────────────────
setwd("C:/_Maximilian_Peter_Masterarbeit_2025_/code/4_Y_4-Factor/")

# ── Daten laden ─────────────────────────────────────────────
geo_data_combined <- readRDS("geo_data_combined.rds")
monthly_prices <- read_csv("monthly_prices_small_value.csv")

# ── Neue Momentumperiode: Jan–Dez 2013 ─────────────────────
momentum_scores <- monthly_prices %>%
  filter(month >= as.Date("2013-01-01") & month <= as.Date("2013-12-01")) %>%
  group_by(ticker) %>%
  summarise(momentum_return = prod(1 + Return, na.rm = TRUE) - 1, .groups = "drop")

# ── Small-Value-Bucket für 2014 ─────────────────────────────
sv_2014 <- geo_data_combined %>%
  filter(Year == 2014,
         Value >= 3e8,
         BookToMarket > 0, BookToMarket < 10) %>%
  distinct(CIK, .keep_all = TRUE) %>%
  filter(
    between(BookToMarket, quantile(BookToMarket, 0.2, na.rm = TRUE), quantile(BookToMarket, 0.8, na.rm = TRUE)),
    between(Value,        quantile(Value, 0.2, na.rm = TRUE), quantile(Value, 0.8, na.rm = TRUE))
  ) %>%
  mutate(size_median = median(Value, na.rm = TRUE),
         btm_70 = quantile(BookToMarket, 0.7, na.rm = TRUE)) %>%
  filter(Value < size_median, BookToMarket >= btm_70)

# ── Momentum integrieren ────────────────────────────────────
sv_momentum <- sv_2014 %>%
  left_join(momentum_scores, by = "ticker") %>%
  filter(!is.na(momentum_return))

momentum_cutoff <- quantile(sv_momentum$momentum_return, 0.7, na.rm = TRUE)
portfolio_mom_2014 <- sv_momentum %>%
  filter(momentum_return >= momentum_cutoff)

# ── Monatsrenditen 2014–2018 ───────────────────────────────
tickers_mom <- unique(portfolio_mom_2014$ticker)

portfolio_returns <- monthly_prices %>%
  filter(ticker %in% tickers_mom,
         month >= as.Date("2014-01-01"),
         month <= as.Date("2018-12-31")) %>%
  group_by(month) %>%
  summarise(PortfolioReturn = mean(Return, na.rm = TRUE), .groups = "drop") %>%
  mutate(GrowthIndex = cumprod(1 + PortfolioReturn))

# ── Momentum-Faktor selbst berechnen (2013 als Basis) ──────
momentum_scores_full <- momentum_scores  # identisch zu oben

winners <- momentum_scores_full %>%
  filter(momentum_return >= quantile(momentum_return, 0.7, na.rm = TRUE)) %>%
  pull(ticker)

losers <- momentum_scores_full %>%
  filter(momentum_return <= quantile(momentum_return, 0.3, na.rm = TRUE)) %>%
  pull(ticker)

momentum_panel <- monthly_prices %>%
  filter(month >= as.Date("2014-01-01"), month <= as.Date("2018-12-31")) %>%
  filter(ticker %in% c(winners, losers)) %>%
  mutate(group = case_when(
    ticker %in% winners ~ "Winner",
    ticker %in% losers  ~ "Loser",
    TRUE                ~ NA_character_
  )) %>%
  group_by(month, group) %>%
  summarise(group_return = mean(Return, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = group_return) %>%
  mutate(MOM = Winner - Loser) %>%
  select(month, MOM)

# 🔧 SKALIERUNG: MOM auf dieselbe Skala wie andere Faktoren bringen
momentum_panel <- momentum_panel %>%
  mutate(MOM = MOM * 100)

# ── FF3-Daten laden und mit MOM kombinieren ────────────────
factors_base <- read_delim("F-F_Research_Data_Factors_m.csv", delim = ";", col_types = cols()) %>%
  rename(Month = 1) %>%
  mutate(month = ymd(paste0(Month, "01"))) %>%
  select(month, `Mkt-RF`, SMB, HML, RF) %>%
  mutate(across(`Mkt-RF`:RF, as.numeric))

factors4 <- left_join(factors_base, momentum_panel, by = "month")

# ── Regressionsfunktion (4-Faktor) ──────────────────────────
regression_ff4 <- function(data) {
  merged <- data %>%
    inner_join(factors4, by = "month") %>%
    mutate(ExcessReturn = PortfolioReturn * 100 - RF)
  
  lm(ExcessReturn ~ `Mkt-RF` + SMB + HML + MOM, data = merged) %>%
    tidy()
}

# ── Analyse: 2014–2016 vs. 2017–2018 ────────────────────────
reg_2014_2016 <- regression_ff4(portfolio_returns %>% filter(month <= as.Date("2016-12-31")))
reg_2017_2018 <- regression_ff4(portfolio_returns %>% filter(month >= as.Date("2017-01-01")))

reg_combined <- bind_rows(
  mutate(reg_2014_2016, Period = "2014–2016"),
  mutate(reg_2017_2018, Period = "2017–2018")
) %>%
  select(Period, term, estimate)

print(reg_combined)

# ── Visualisierung: Style Drift ─────────────────────────────
ggplot(reg_combined %>% filter(term != "(Intercept)"),
       aes(x = term, y = estimate, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Style Drift: Factor Exposures Over Time",
       x = "Factor", y = "Regression Coefficient (Beta)") +
  theme_minimal()

# ── Performance-Kennzahlen berechnen ─────────────────────────────
n_years <- 5  # Zeitraum: 2014–2018

# Kumulierte Gesamtrendite
total_return <- prod(1 + portfolio_returns$PortfolioReturn, na.rm = TRUE) - 1

# Geometrische Durchschnittsrendite (CAGR)
cagr <- (1 + total_return)^(1 / n_years) - 1

# Arithmetischer Mittelwert
mean_return <- mean(portfolio_returns$PortfolioReturn, na.rm = TRUE)

# Standardabweichung (Volatilität)
volatility <- sd(portfolio_returns$PortfolioReturn, na.rm = TRUE)

# Risikofreier Zins (durchschnittlich)
avg_rf <- mean(factors4$RF[factors4$month %in% portfolio_returns$month], na.rm = TRUE) / 100

# Sharpe Ratio (ex post)
sharpe_ratio <- (mean_return - avg_rf) / volatility

# Ausgabe
cat("Kumulative Rendite: ", round(total_return * 100, 2), "%\n")
cat("Geometrischer Durchschnitt (CAGR): ", round(cagr * 100, 2), "%\n")
cat("Arithm. Durchschnitt: ", round(mean_return * 100, 2), "%\n")
cat("Volatilität: ", round(volatility * 100, 2), "%\n")
cat("Sharpe Ratio: ", round(sharpe_ratio, 2), "\n")


# ── 1. Zeitreihe der Wertentwicklung ─────────────────────────────
ggplot(portfolio_returns, aes(x = month, y = GrowthIndex)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Portfolio Growth (2014–2018)",
       x = "Month", y = "Growth Index") +
  theme_minimal()

# ── 2. Log-Wertentwicklung ───────────────────────────────────────
ggplot(portfolio_returns, aes(x = month, y = log(GrowthIndex))) +
  geom_line(color = "darkred", linewidth = 1) +
  labs(title = "Logarithmic Portfolio Growth",
       x = "Month", y = "log(Value)") +
  theme_minimal()

# ── 3. Histogramm der Monatsrenditen ─────────────────────────────
ggplot(portfolio_returns, aes(x = PortfolioReturn)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Monthly Returns",
       x = "Monthly Return", y = "Number of Months") +
  theme_minimal()


##Vergleich mit SP500

# S&P 500 von Yahoo: letzter Tag jedes Monats von Jan 2014 bis Dez 2018
sp500_raw <- tq_get("^GSPC", from = "2013-12-31", to = "2018-12-31", get = "stock.prices")

# Monatsrenditen aus letztem Börsentag im Monat
sp500_monthly <- sp500_raw %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(month) %>%
  mutate(SP500_Return = adjusted / lag(adjusted) - 1) %>%
  select(month, SP500_Return) %>%
  drop_na()

#Join mit deinem Portfolio (portfolio_returns)
portfolio_with_sp500 <- portfolio_returns %>%
  left_join(sp500_monthly, by = "month") %>%
  mutate(
    SP500_Return   = replace_na(SP500_Return, 0),
    SP500_Growth   = cumprod(1 + SP500_Return),
    Portfolio_Growth = GrowthIndex  # aus deinem Code bereits berechnet
  )

# Umbenennen für Plot-Legende
portfolio_long <- portfolio_with_sp500 %>%
  select(month, Portfolio_Growth, SP500_Growth) %>%
  pivot_longer(cols = -month, names_to = "Strategie", values_to = "Wert") %>%
  mutate(Strategie = case_when(
    Strategie == "Portfolio_Growth" ~ "Small-Value-Momentum",
    Strategie == "SP500_Growth"     ~ "S&P 500"
  ))

ggplot(portfolio_long, aes(x = month, y = Wert, color = Strategie)) +
  geom_line(linewidth = 1.2) +
  labs(
    title    = "Portfolio vs. S&P 500 (2014–2018)",
    subtitle = "Start value = 1 (indexed)",
    x        = "Month",
    y        = "Capital Development (Index)",
    color    = "Strategy"
  ) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c(
    "Small-Value-Momentum" = "steelblue",
    "S&P 500"               = "forestgreen"
  )) 


####-----------Berechnung Sharpe ratios

# Risikofreier Zins (Monatsdurchschnitt aus FF-Daten)
avg_rf_monthly <- mean(factors4$RF[factors4$month %in% portfolio_with_sp500$month], na.rm = TRUE) / 100

# Sharpe für Small-Value-Momentum
mean_sv <- mean(portfolio_with_sp500$PortfolioReturn, na.rm = TRUE)
vol_sv  <- sd(portfolio_with_sp500$PortfolioReturn, na.rm = TRUE)
sharpe_sv_monthly <- (mean_sv - avg_rf_monthly) / vol_sv
sharpe_sv_annual  <- sharpe_sv_monthly * sqrt(12)

# Sharpe für S&P 500
mean_sp500 <- mean(portfolio_with_sp500$SP500_Return, na.rm = TRUE)
vol_sp500  <- sd(portfolio_with_sp500$SP500_Return, na.rm = TRUE)
sharpe_sp500_monthly <- (mean_sp500 - avg_rf_monthly) / vol_sp500
sharpe_sp500_annual  <- sharpe_sp500_monthly * sqrt(12)

# Ausgabe
cat("== Sharpe Ratios (2014–2018) ==\n")
cat(sprintf("Small-Value-Momentum:\n  Monthly: %.3f\n  Annual:  %.3f\n\n", sharpe_sv_monthly, sharpe_sv_annual))
cat(sprintf("S&P 500:\n  Monthly: %.3f\n  Annual:  %.3f\n", sharpe_sp500_monthly, sharpe_sp500_annual))
