# Hintergrundidee: Wie stark streuen die Faktor-Loadings in Small-Value-Portfolios mit nur 5 Aktien?
# Er dokumentiert die Streuung der Fama-French-Faktorbelastungen (Loadings) für ein realistisches Portfolio 
# mit nur 5 Aktien – eine zentrale Grundlage für deine Performance-Simulationen.

# Pakete
library(dplyr)
library(tidyr)
library(tidyquant)
library(lubridate)
library(readr)
library(ggplot2)
library(purrr)
library(broom)
library(stringr)

# Setze Working Directory
setwd("C:/_Maximilian_Peter_Masterarbeit_2025_/code/1_3_Subportfolio/")


# 1. Daten vorbereiten
geo_data_combined <- readRDS("geo_data_combined.rds")
performance_only <- readRDS("performance_only.rds")

# 2. Small-Value-Portfolio identifizieren (wie zuvor)
years <- 2013:2022
portfolios <- list()

for (y in years) {
  portfolio <- geo_data_combined %>%
    filter(
      Year == y,
      !is.na(ticker),
      Value >= 3e8,
      BookToMarket > 0,
      BookToMarket < 10
    ) %>%
    select(CIK, ticker, Value, BookToMarket) %>%
    distinct(CIK, .keep_all = TRUE)
  
  # zentrale 60 % behalten
  lower_bm <- quantile(portfolio$BookToMarket, 0.2, na.rm = TRUE)
  upper_bm <- quantile(portfolio$BookToMarket, 0.8, na.rm = TRUE)
  lower_val <- quantile(portfolio$Value, 0.2, na.rm = TRUE)
  upper_val <- quantile(portfolio$Value, 0.8, na.rm = TRUE)
  
  filtered <- portfolio %>%
    filter(
      BookToMarket >= lower_bm, BookToMarket <= upper_bm,
      Value >= lower_val, Value <= upper_val
    )
  
  size_median <- median(filtered$Value, na.rm = TRUE)
  btm_70pct <- quantile(filtered$BookToMarket, 0.7, na.rm = TRUE)
  
  value_tilted <- filtered %>%
    filter(Value < size_median, BookToMarket >= btm_70pct) %>%
    mutate(Year = y)
  
  portfolios[[as.character(y)]] <- value_tilted
}

portfolio_combined <- bind_rows(portfolios)

# 🔁 3. Monte Carlo Sampling von 5 Aktien-Portfolios

set.seed(123)  # für Reproduzierbarkeit
n_sim <- 1000  # Anzahl Simulationen
tickers <- unique(portfolio_combined$ticker)

# Hilfsfunktion: Monatsdaten je Ticker holen (wie vorher)
get_prices_safe <- function(ticker) {
  tryCatch({
    tq_get(ticker, from = "2013-01-01", to = "2023-01-01", get = "stock.prices") %>%
      mutate(ticker = ticker)
  }, error = function(e) {
    message(paste("❌ Fehler bei Ticker:", ticker))
    return(NULL)
  })
}

# 🔁 Alle Kursdaten nur 1x laden
all_prices <- map_dfr(tickers, get_prices_safe)

# Monatsrenditen vorbereiten
monthly_prices <- all_prices %>%
  group_by(ticker, month = floor_date(date, "month")) %>%
  filter(date == max(date)) %>%
  arrange(ticker, month) %>%
  group_by(ticker) %>%
  mutate(Return = adjusted / lag(adjusted) - 1) %>%
  ungroup()

# 💾 Speichern als RDS-Datei
saveRDS(monthly_prices, file = "monthly_prices_small_value.rds")

# Optional: auch als CSV, falls du die Daten z. B. in Excel nutzen möchtest
write.csv(monthly_prices, "monthly_prices_small_value.csv", row.names = FALSE)

message("✅ Monatliche Preise wurden gespeichert.")

# 📥 Fama-French Daten laden
factors_monthly <- read_delim("F-F_Research_Data_Factors_m.csv", delim = ";", col_types = cols()) %>%
  rename(Month = Date) %>%
  mutate(month = ymd(paste0(Month, "01"))) %>%
  select(month, `Mkt-RF`, SMB, HML, RF) %>%
  mutate(across(`Mkt-RF`:RF, ~ as.numeric(str_trim(.))))


# Funktion zum Winsorisieren von Excess Returns
winsorize_returns <- function(x, lower = 0.02, upper = 0.98) {
  qnt <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  pmax(pmin(x, qnt[2]), qnt[1])
}

# 💡 Monte-Carlo-Simulation für Regressionskoeffizienten ----------------------

simulate_one <- function(sample_tickers) {
  sample_data <- monthly_prices %>%
    filter(ticker %in% sample_tickers) %>%
    group_by(month) %>%
    summarise(Return = mean(Return, na.rm = TRUE), .groups = "drop") %>%
    inner_join(factors_monthly, by = "month") %>%
    mutate(
      PortfolioReturn = Return * 100,
      ExcessReturn_raw = PortfolioReturn - RF,  # Original
      ExcessReturn = winsorize_returns(PortfolioReturn - RF)  # Winsorized
    )
  
  model <- lm(ExcessReturn ~ `Mkt-RF` + SMB + HML, data = sample_data)
  tidy(model)[, c("term", "estimate")] %>% pivot_wider(names_from = term, values_from = estimate)
}

# 1000 Simulationen mit je 5 zufällig gezogenen Aktien
set.seed(123)
sim_results <- map_dfr(1:n_sim, function(i) {
  sampled_tickers <- sample(tickers, 5)
  simulate_one(sampled_tickers)
})


# Ergebnisse speichern
write_csv(sim_results, "montecarlo_factor_loadings_5stocks.csv")
message("✅ Monte-Carlo-Simulation abgeschlossen.")

# 5%- und 95%-Quantile je Faktor berechnen
quantiles <- data.frame(
  Variable = c("Intercept", "Mkt-RF", "SMB", "HML"),
  P05 = c(
    quantile(sim_results[["(Intercept)"]], 0.05, na.rm = TRUE),
    quantile(sim_results[["`Mkt-RF`"]], 0.05, na.rm = TRUE),
    quantile(sim_results$SMB, 0.05, na.rm = TRUE),
    quantile(sim_results$HML, 0.05, na.rm = TRUE)
  ),
  P95 = c(
    quantile(sim_results[["(Intercept)"]], 0.95, na.rm = TRUE),
    quantile(sim_results[["`Mkt-RF`"]], 0.95, na.rm = TRUE),
    quantile(sim_results$SMB, 0.95, na.rm = TRUE),
    quantile(sim_results$HML, 0.95, na.rm = TRUE)
  )
)

print(quantiles)


ggplot(quantiles, aes(x = Variable)) +
  geom_linerange(aes(ymin = P05, ymax = P95), size = 2, color = "steelblue") +
  geom_point(aes(y = P05 + (P95 - P05)/2), size = 3, color = "darkblue") +
  scale_y_continuous(
    limits = c(min(quantiles$P05) - 0.2, max(quantiles$P95) + 0.2)
  ) +
  labs(
    title = "90% Interval of Fama-French Factor Loadings (5-stock Portfolios)",
    y = "Factor Loading", x = NULL
  ) +
  theme_minimal()

Ausgabe ----------------------------------------------------------
summary(model)