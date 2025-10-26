# ── Libraries ─────────────────────────────────────────────────
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(tidyquant)
library(furrr)
library(readr)
library(broom)
library(MASS)  # für mvrnorm()
library(quantmod)
library(scales)

# Setze Working Directory
setwd("C:/_Maximilian_Peter_Masterarbeit_2025_/code/2_Monte_Carlo_Simulation/")

# ======================================================================
#  ▸ PART C   Monte-Carlo (5 Stocks, empirische Loadings)
# ======================================================================

sim_results <- read_csv("montecarlo_factor_loadings_5stocks.csv",
                        col_types = cols()) %>%
  rename(Intercept = `(Intercept)`,
         Mkt_RF    = "`Mkt-RF`") %>%
  filter(between(Mkt_RF, -1.5, 1.5),
         between(SMB,    -1.5, 1.5),
         between(HML,    -1.5, 1.5))

n_years  <- 10
n_months <- 12 * n_years
n_sims   <- 1000

factor_means <- c(Mkt_RF = 0.686, SMB = 0.177, HML = 0.338)
factor_cov <- matrix(c(28.38,5.35,4.31,
                       5.35,10.08,1.37,
                       4.31,1.37,12.70),
                     ncol = 3, byrow = TRUE,
                     dimnames = list(c("Mkt_RF","SMB","HML"),
                                     c("Mkt_RF","SMB","HML")))

rf_monthly_mean <- 0.27
rf_monthly_sd   <- 0.25

simulate_factors <- function() {
  as_tibble(mvrnorm(n_months, factor_means, factor_cov)) %>%
    setNames(c("Mkt_RF","SMB","HML")) %>%
    mutate(RF = rnorm(n_months, rf_monthly_mean, rf_monthly_sd))
}

sample_factor_loading <- function() {
  sim_results[sample(nrow(sim_results), 1), ]
}

calc_portfolio_return <- function(df) {
  fl <- sample_factor_loading()
  excess  <- fl$Intercept +
    fl$Mkt_RF * df$Mkt_RF +
    fl$SMB    * df$SMB +
    fl$HML    * df$HML
  monthly <- (excess + df$RF) / 100
  prod(1 + monthly) - 1
}

set.seed(42)
simulated_total_returns <- replicate(n_sims, {
  calc_portfolio_return(simulate_factors())
}, simplify = TRUE)

# ======================================================================
#  ▸ PART D   Monte-Carlo reines Markt-Exposure (β = 1)
# ======================================================================

factor_loadings <- c(Intercept = 0, Mkt_RF = 1, SMB = 0, HML = 0)
factor_stats <- list(
  mean = c(Mkt_RF = 0.686455161, SMB = 0.176903553, HML = 0.338494078),
  sd   = c(Mkt_RF = 5.325004885, SMB = 3.173827810, HML = 3.562016212))
rf_monthly_mean <- 0.26928934
rf_monthly_sd   <- 0.24986088

simulate_factors_D <- function() {
  tibble(
    Mkt_RF = rnorm(n_months, factor_stats$mean["Mkt_RF"],
                   factor_stats$sd["Mkt_RF"]),
    SMB    = rnorm(n_months, factor_stats$mean["SMB"],
                   factor_stats$sd["SMB"]),
    HML    = rnorm(n_months, factor_stats$mean["HML"],
                   factor_stats$sd["HML"]),
    RF     = rnorm(n_months, rf_monthly_mean, rf_monthly_sd))
}

calc_portfolio_return_D <- function(df) {
  excess  <- factor_loadings["Mkt_RF"] * df$Mkt_RF
  monthly <- (excess + df$RF) / 100
  prod(1 + monthly) - 1
}

set.seed(42)
simulated_total_returns_D <- replicate(n_sims, {
  calc_portfolio_return_D(simulate_factors_D())
}, simplify = TRUE)


# ======================================================================
#  ▸ CPI-Inflation 2013–2022 aus FRED (Monthly US CPI, not seasonally adjusted)
# ======================================================================
# CPI-Daten von FRED laden
getSymbols("CPIAUCNS", src = "FRED")

# Daten extrahieren (inkl. Dezember 2012 für lag)
cpi <- CPIAUCNS["2012-12/2022-12"]

# Inflationsraten berechnen (Dezember-zu-Dezember)
cpi_df <- data.frame(date = index(cpi), value = as.numeric(coredata(cpi))) %>%
  filter(month(date) == 12) %>%
  mutate(year = year(date)) %>%
  arrange(year) %>%
  mutate(
    inflation_rate = (value / lag(value)) - 1
  ) %>%
  filter(!is.na(inflation_rate)) %>%
  dplyr::select(year, cpi_dec = value, inflation_rate)

# Durchschnitt berechnen
avg_inflation <- mean(cpi_df$inflation_rate)

# Ausgabe
print(cpi_df)
print(paste("Durchschnittliche Inflation 2013–2022:", round(avg_inflation * 100, 2), "%"))


# ======================================================================
#  ▸ PART E   After-Costs / Tax-Adjustment
# ======================================================================

adjust_for_trading_costs_tax_inflation <- function(total_returns,
                                                   portfolio_size     = 10000,
                                                   allocation_share   = 1.0,
                                                   n_stocks           = 5,
                                                   rebalance_frequency = 1,
                                                   years              = 10,
                                                   capital_gains_tax  = 0.275,
                                                   inflation_rate     = 0.0289) {
  n_rebalances <- years * rebalance_frequency
  invested_per_stock <- (portfolio_size * allocation_share) / n_stocks
  
  order_cost <- function(amount) {
    if (amount <=  3500) 5.90
    else if (amount <= 12500) 9.90
    else 19.90
  }
  cost_per_trade     <- order_cost(invested_per_stock)
  cost_per_rebalance <- cost_per_trade * n_stocks * 2
  yearly_cost_frac   <- cost_per_rebalance / portfolio_size
  
  annual_ret      <- (1 + total_returns)^(1/years) - 1
  annual_after_c  <- annual_ret - yearly_cost_frac
  annual_after_ct <- annual_after_c * (1 - capital_gains_tax)
  
  # Realer Ertrag nach Inflation (Fisher-Approximation)
  annual_real     <- (1 + annual_after_ct) / (1 + inflation_rate) - 1
  (1 + annual_real)^years - 1
}

# ── Anwendung ────────────────────────────────────────────────
adjusted_real_returns <- adjust_for_trading_costs_tax_inflation(simulated_total_returns)

# ── Ausgabe ─────────────────────────────────────────────────
quantile(adjusted_real_returns, c(0.01, 0.05, 0.5, 0.95, 0.99))

# Create a data frame for plotting
df_returns <- data.frame(real_return = adjusted_real_returns)

# Compute quantiles
qtiles <- quantile(df_returns$real_return, c(0.01, 0.05, 0.5, 0.95, 0.99))

# Convert to label-ready format
q_df <- data.frame(
  quantile = names(qtiles),
  value = as.numeric(qtiles)
)

# Plot
ggplot(df_returns, aes(x = real_return)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(data = q_df, aes(xintercept = value), linetype = "dashed", color = "red") +
  geom_text(data = q_df, aes(x = value, y = Inf, label = paste0(quantile, ": ", percent(value, accuracy = 0.1))),
            angle = 90, vjust = -0.5, hjust = 1.1, color = "red", size = 3.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Distribution of Real After-Tax, After-Cost Returns",
    subtitle = "5-stock portfolio, 10-year horizon, incl. trading costs, taxes, inflation",
    x = "Real Total Return over 10 Years",
    y = "Frequency"
  ) +
  theme_minimal()