
# Rolling 5-Year Portfolio Simulation

library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(readr)     # für read_csv()
library(ggplot2)   # für alle Plots
library(quadprog)  # für QP
library(BLCOP)     # für Black-Litterman
library(broom)     # für tidy()

# ── Set Working Directory ─────────────────────────────────────
setwd("C:/_Maximilian_Peter_Masterarbeit_2025_/code/4_Y_4-Factor/")

# ── Daten laden ─────────────────────────────────────────────
geo_data_combined <- readRDS("geo_data_combined.rds")
monthly_prices <- read_csv("monthly_prices_small_value.csv")

# Parameters
formation_years <- 2014:2018
holding_period_years <- 5

# Function to build and evaluate portfolio
simulate_portfolio <- function(formation_year, geo_data_combined, monthly_prices) {
  # 1. Momentum Calculation (previous year)
  momentum_start <- as.Date(paste0(formation_year - 1, "-01-01"))
  momentum_end   <- as.Date(paste0(formation_year - 1, "-12-01"))
  
  momentum_scores <- monthly_prices %>%
    filter(month >= momentum_start & month <= momentum_end) %>%
    group_by(ticker) %>%
    summarise(momentum_return = prod(1 + Return, na.rm = TRUE) - 1, .groups = "drop")
  
  # 2. Small-Value Selection
  sv_selection <- geo_data_combined %>%
    filter(Year == formation_year,
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
  
  # 3. Merge with Momentum
  sv_momentum <- sv_selection %>%
    left_join(momentum_scores, by = "ticker") %>%
    filter(!is.na(momentum_return))
  
  momentum_cutoff <- quantile(sv_momentum$momentum_return, 0.7, na.rm = TRUE)
  portfolio <- sv_momentum %>%
    filter(momentum_return >= momentum_cutoff)
  
  # 4. Holding Period Monthly Returns
  tickers <- unique(portfolio$ticker)
  start_date <- as.Date(paste0(formation_year, "-01-01"))
  end_date   <- as.Date(paste0(formation_year + holding_period_years - 1, "-12-31"))
  
  portfolio_returns <- monthly_prices %>%
    filter(ticker %in% tickers,
           month >= start_date,
           month <= end_date) %>%
    group_by(month) %>%
    summarise(PortfolioReturn = mean(Return, na.rm = TRUE), .groups = "drop") %>%
    mutate(GrowthIndex = cumprod(1 + PortfolioReturn),
           FormationYear = formation_year)
  
  return(portfolio_returns)
}

# Run Simulation
results_list <- map(formation_years, ~simulate_portfolio(.x, geo_data_combined, monthly_prices))
all_results <- bind_rows(results_list)

# Ready for visualization or regression
head(all_results)

# Visualize growth paths

ggplot(all_results, aes(x = month, y = GrowthIndex, color = as.factor(FormationYear))) +
  geom_line(size = 1.2) +
  labs(title = "5-Year Portfolio Growth by Formation Year",
       x = "Month", y = "Growth Index",
       color = "Formation Year") +
  theme_minimal()

#Compare performance metrics across formation years

all_results %>%
  group_by(FormationYear) %>%
  summarise(
    TotalReturn = last(GrowthIndex) - 1,
    CAGR = (last(GrowthIndex))^(1/5) - 1,
    Volatility = sd(PortfolioReturn),
    Sharpe_monthly = (mean(PortfolioReturn) - 0.002) / sd(PortfolioReturn),
    Sharpe_annual = Sharpe_monthly * sqrt(12)
  )


####Predicting style drift
# Reuse your regression function
regression_ff4 <- function(data) {
  merged <- data %>%
    inner_join(factors4, by = "month") %>%
    mutate(ExcessReturn = PortfolioReturn * 100 - RF)
  
  lm(ExcessReturn ~ `Mkt-RF` + SMB + HML + MOM, data = merged) %>%
    tidy()
}

# Run regressions for each formation year portfolio
style_drift_results <- all_results %>%
  group_by(FormationYear) %>%
  group_modify(~ regression_ff4(.x)) %>%
  ungroup() %>%
  filter(term != "(Intercept)")  # optional: exclude intercept


ggplot(style_drift_results, aes(x = term, y = estimate, fill = factor(FormationYear))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Style Drift Across Portfolios Formed in 2014–2018",
    x = "Factor",
    y = "Regression Coefficient (Beta)",
    fill = "Formation Year"
  ) +
  theme_minimal(base_size = 13)

###--------
###---------
###--------


# ── 1. Regression Function (handles missing data gracefully) ──────
regression_ff4 <- function(data) {
  merged <- data %>%
    inner_join(factors4, by = "month") %>%
    mutate(ExcessReturn = PortfolioReturn * 100 - RF) %>%
    drop_na(ExcessReturn, `Mkt-RF`, SMB, HML, MOM)
  
  if (nrow(merged) < 6) {
    return(tibble(term = c("Mkt-RF", "SMB", "HML", "MOM"),
                  estimate = NA_real_))
  }
  
  lm(ExcessReturn ~ `Mkt-RF` + SMB + HML + MOM, data = merged) %>%
    broom::tidy() %>%
    filter(term %in% c("Mkt-RF", "SMB", "HML", "MOM")) %>%
    complete(term = c("Mkt-RF", "SMB", "HML", "MOM"), fill = list(estimate = NA_real_))
}

# ── 2. Function: Split a 5-Year Portfolio into Two Subperiods ─────
regression_subperiods <- function(portfolio_df, formation_year) {
  df <- portfolio_df %>%
    filter(FormationYear == formation_year) %>%
    mutate(Period = if_else(year(month) <= formation_year + 2,
                            "First Half", "Second Half"))
  
  # Define all required combinations
  periods <- c("First Half", "Second Half")
  factors <- c("Mkt-RF", "SMB", "HML", "MOM")
  
  df %>%
    group_by(Period) %>%
    group_modify(~ regression_ff4(.x)) %>%
    ungroup() %>%
    complete(Period = periods, term = factors, fill = list(estimate = NA_real_)) %>%
    mutate(FormationYear = formation_year)
}

# ── 3. Run for All 5-Year Portfolios ──────────────────────────────
style_drift_subperiods <- purrr::map_dfr(2014:2018, ~ regression_subperiods(all_results, .x))

# ── 4. Plot: Style Drift Before vs After (Y0–Y2 vs Y3–Y5) ─────────
library(ggplot2)

ggplot(style_drift_subperiods, aes(x = term, y = estimate, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  facet_wrap(~ FormationYear) +
  labs(
    title = "Style Drift Within Each 5-Year Portfolio",
    subtitle = "Comparison of First Half (Y0–Y2) vs Second Half (Y3–Y5)",
    x = "Factor",
    y = "Factor Loading (Beta)",
    fill = "Subperiod"
  ) +
  theme_minimal(base_size = 13)


#Portfolio Optimazation
#Option 1: Mean-Variance Optimization (MVO)
# Libraries
library(quadprog)
library(dplyr)

# Assume: monthly returns for your small-cap value portfolio and SP500
# Combine the two into one data frame
returns_df <- all_results %>%
  select(month, PortfolioReturn) %>%
  left_join(sp500_monthly, by = "month") %>%
  rename(SmallValue = PortfolioReturn, SP500 = SP500_Return) %>%
  drop_na()

# Calculate expected returns and covariance
mu <- colMeans(returns_df[, c("SmallValue", "SP500")])  # mean monthly returns
cov_mat <- cov(returns_df[, c("SmallValue", "SP500")])

# Target return (e.g., somewhere between both assets)
target_return <- mean(mu)

# Set up for quadratic programming
Dmat <- 2 * cov_mat
dvec <- rep(0, 2)
Amat <- cbind(1, mu, diag(2))
bvec <- c(1, target_return, 0, 0)
meq  <- 2

# Solve
opt <- solve.QP(Dmat, dvec, Amat, bvec, meq)
weights <- opt$solution
names(weights) <- c("SmallValue", "SP500")
weights



#Option 2: Black-Litterman Model

# Install and load
# install.packages("BLCOP")
library(BLCOP)

# Erwartete Renditen (prior) und Kovarianz
mu_market <- colMeans(returns_df[, c("SmallValue", "SP500")])
cov_mat   <- cov(returns_df[, c("SmallValue", "SP500")])
tau       <- 0.05

# Relative Sicht: SmallValue outperformt SP500 um 2 %
P <- matrix(c(1, -1), nrow = 1)    # Sicht: SmallValue - SP500
Q <- matrix(0.02, nrow = 1)        # Erwartete Outperformance

# View erstellen
views <- BLViews(P = P, q = Q, confidences = NULL, assetNames = colnames(mu_market))

# Posterior berechnen
bl_posterior <- posteriorEst(
  views,
  mu = mu_market,
  tau = tau,
  sigma = cov_mat
)

# Zugriff auf Ergebnis
bl_mu    <- bl_posterior@posteriorMean
bl_sigma <- bl_posterior@posteriorCovar

# Portfolio optimieren (Mean-Variance auf Posterior)
library(quadprog)
Dmat <- 2 * bl_sigma
dvec <- rep(0, 2)
Amat <- cbind(1, bl_mu, diag(2))
bvec <- c(1, mean(bl_mu), 0, 0)
meq  <- 2

opt_bl <- solve.QP(Dmat, dvec, Amat, bvec, meq)
weights_bl <- opt_bl$solution
names(weights_bl) <- colnames(mu_market)
weights_bl