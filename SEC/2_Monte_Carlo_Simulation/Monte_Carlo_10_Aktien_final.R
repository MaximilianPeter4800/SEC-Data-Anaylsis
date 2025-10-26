library(MASS)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

# 1️⃣ Parameter
n_years  <- 20
n_months <- 12 * n_years
n_sims   <- 1000

factor_means <- c(Mkt_RF = 0.686, SMB = 0.177, HML = 0.338)
factor_cov <- matrix(c(28.38,5.35,4.31,
                       5.35,10.08,1.37,
                       4.31,1.37,12.70),
                     ncol = 3, byrow = TRUE)
rf_monthly_mean <- 0.27
rf_monthly_sd   <- 0.25

# 2️⃣ Factor Loadings laden
sim_results <- read_csv("montecarlo_factor_loadings_20stocks.csv",
                        col_types = cols()) %>%
  rename(Intercept = `(Intercept)`,
         Mkt_RF = "`Mkt-RF`") %>%
  filter(between(Mkt_RF, -1.5, 1.5),
         between(SMB, -1.5, 1.5),
         between(HML, -1.5, 1.5))

# 3️⃣ Simuliere viele Szenarien (robust)
set.seed(42)
cagr_values <- numeric(n_sims)

for (i in 1:n_sims) {
  factor_path <- tryCatch({
    as_tibble(mvrnorm(n_months, factor_means, factor_cov)) %>%
      setNames(c("Mkt_RF", "SMB", "HML")) %>%
      mutate(RF = rnorm(n_months, rf_monthly_mean, rf_monthly_sd))
  }, error = function(e) NULL)
  
  if (is.null(factor_path)) {
    cagr_values[i] <- NA
    next
  }
  
  loadings_for_path <- sim_results[i:(i + n_years - 1), ] %>%
    slice(rep(1:n(), each = 12))
  
  if (nrow(loadings_for_path) != n_months) {
    cagr_values[i] <- NA
    next
  }
  
  excess_return <- loadings_for_path$Intercept +
    loadings_for_path$Mkt_RF * factor_path$Mkt_RF +
    loadings_for_path$SMB * factor_path$SMB +
    loadings_for_path$HML * factor_path$HML
  
  monthly_return <- (excess_return + factor_path$RF) / 100
  
  total_return <- tryCatch({
    prod(1 + monthly_return) - 1
  }, warning = function(w) NA, error = function(e) NA)
  
  if (is.na(total_return)) {
    cagr_values[i] <- NA
    next
  }
  
  cagr_values[i] <- (1 + total_return)^(1/20) - 1
}

# 4️⃣ Ergebnisse als Dataframe (nur gültige!)
results_df <- tibble(CAGR = cagr_values) %>% filter(!is.na(CAGR))

# 5️⃣ Statistiken berechnen
mean_cagr <- mean(results_df$CAGR) * 100
median_cagr <- median(results_df$CAGR) * 100
sd_cagr <- sd(results_df$CAGR) * 100

# 6️⃣ Ausgeben
cat("Mean CAGR (%):", round(mean_cagr, 3), "\n")
cat("Median CAGR (%):", round(median_cagr, 3), "\n")
cat("SD of CAGR (%):", round(sd_cagr, 3), "\n")

# 7️⃣ Plot
ggplot(results_df, aes(x = CAGR * 100)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  geom_vline(xintercept = median_cagr, color = "black", linetype = "dashed") +
  labs(
    title = paste0(
      "Distribution of Simulated 10-Year CAGR (%)\n",
      "Mean = ", round(mean_cagr, 2), "%, ",
      "Median = ", round(median_cagr, 2), "%, ",
      "SD = ", round(sd_cagr, 2), "%"
    ),
    x = "CAGR (%)",
    y = "Density"
  ) +
  theme_minimal()


#  Gewünschte Quantile
quantiles_cagr <- quantile(results_df$CAGR, probs = c(0.01, 0.05, 0.5, 0.95, 0.99)) * 100

#  Ausgabe
cat("Mean CAGR (%):", round(mean_cagr, 3), "\n")
cat("Median CAGR (%):", round(median_cagr, 3), "\n")
cat("SD of CAGR (%):", round(sd_cagr, 3), "\n")
cat("Quantiles (1% / 5% / 50% / 95% / 99%) (%):\n")
print(round(quantiles_cagr, 3))

#  Plot: Verteilung mit Median & Quantiles
ggplot(results_df, aes(x = CAGR * 100)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  geom_vline(xintercept = quantiles_cagr, color = "black", linetype = "dashed") +
  labs(
    title = paste0(
      "Distribution of Simulated 10-Year CAGR (%)\n",
      "Mean = ", round(mean_cagr, 2), "% | ",
      "Median = ", round(median_cagr, 2), "% | ",
      "SD = ", round(sd_cagr, 2), "%"
    ),
    x = "CAGR (%)",
    y = "Density"
  ) +
  theme_minimal()


# Was wäre das annualisierte Marktwachstum nur mit Mkt_RF + RF:
mean_monthly_mkt <- factor_means["Mkt_RF"] + rf_monthly_mean
annual_return <- (1 + mean_monthly_mkt/100) ^ 12 - 1
print(annual_return)  # => ca. 9-10% realistisch!




####--------------------------------------Check return Markt

library(MASS)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

# 1️⃣ Parameter
n_years  <- 20
n_months <- 12 * n_years
n_sims   <- 1000

factor_means <- c(Mkt_RF = 0.686, SMB = 0.177, HML = 0.338)
factor_cov <- matrix(c(28.38,5.35,4.31,
                       5.35,10.08,1.37,
                       4.31,1.37,12.70),
                     ncol = 3, byrow = TRUE)
rf_monthly_mean <- 0.27
rf_monthly_sd   <- 0.25

# 2️⃣ Factor Loadings: NICHT mehr gebraucht, da nur Mkt_RF = 1
# (sim_results bleibt ungenutzt!)

# 3️⃣ Monte Carlo nur mit Mkt_RF * 1 + RF
set.seed(42)
cagr_values <- numeric(n_sims)

for (i in 1:n_sims) {
  factor_path <- tryCatch({
    as_tibble(mvrnorm(n_months, factor_means, factor_cov)) %>%
      setNames(c("Mkt_RF", "SMB", "HML")) %>%
      mutate(RF = rnorm(n_months, rf_monthly_mean, rf_monthly_sd))
  }, error = function(e) NULL)
  
  if (is.null(factor_path)) {
    cagr_values[i] <- NA
    next
  }
  
  # Reiner Market Factor mit β = 1, ohne Intercept & ohne SMB/HML
  excess_return <- factor_path$Mkt_RF
  monthly_return <- (excess_return + factor_path$RF) / 100
  
  total_return <- tryCatch({
    prod(1 + monthly_return) - 1
  }, warning = function(w) NA, error = function(e) NA)
  
  if (is.na(total_return)) {
    cagr_values[i] <- NA
    next
  }
  
  cagr_values[i] <- (1 + total_return)^(1 / n_years) - 1  # korrekt annualisieren!
}

# 4️⃣ Ergebnisse als Dataframe (nur gültige!)
results_df <- tibble(CAGR = cagr_values) %>% filter(!is.na(CAGR))

# 5️⃣ Statistiken berechnen
mean_cagr <- mean(results_df$CAGR) * 100
median_cagr <- median(results_df$CAGR) * 100
sd_cagr <- sd(results_df$CAGR) * 100

# 6️⃣ Ausgeben
cat("Mean CAGR (%):", round(mean_cagr, 3), "\n")
cat("Median CAGR (%):", round(median_cagr, 3), "\n")
cat("SD of CAGR (%):", round(sd_cagr, 3), "\n")

# 7️⃣ Quantile
quantiles_cagr <- quantile(results_df$CAGR, probs = c(0.01, 0.05, 0.5, 0.95, 0.99)) * 100
cat("Quantiles (1% / 5% / 50% / 95% / 99%) (%):\n")
print(round(quantiles_cagr, 3))

# 8️⃣ Plot: Verteilung mit Median & Quantiles
ggplot(results_df, aes(x = CAGR * 100)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  geom_vline(xintercept = quantiles_cagr, color = "black", linetype = "dashed") +
  labs(
    title = paste0(
      "Distribution of Simulated 10-Year CAGR (Market Only)\n",
      "Mean = ", round(mean_cagr, 2), "% | ",
      "Median = ", round(median_cagr, 2), "% | ",
      "SD = ", round(sd_cagr, 2), "%"
    ),
    x = "CAGR (%)",
    y = "Density"
  ) +
  theme_minimal()

# 🔑 Kontrollcheck: theoretisch erwarteter Markt-Return p.a.
mean_monthly_mkt <- factor_means["Mkt_RF"] + rf_monthly_mean
annual_return <- (1 + mean_monthly_mkt / 100) ^ 12 - 1
cat("Expected Market Annual Return (%):", round(annual_return * 100, 2), "\n")
