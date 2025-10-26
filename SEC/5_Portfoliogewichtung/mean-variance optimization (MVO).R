# ── Pakete ─────────────────────────────
library(quadprog)
library(dplyr)
library(quantmod)
library(lubridate)
library(zoo)  # für na.locf
library(tidyr)     # ← für pivot_longer()
library(ggplot2)   # ← für ggplot()

# ── 1. FRED-Daten laden: 3-Monats-TBill (DTB3) ─────────────
getSymbols("DTB3", src = "FRED", from = "2013-01-01")
rf_monthly_raw <- to.monthly(DTB3, indexAt = "lastof", OHLC = FALSE)
rf_monthly_df <- data.frame(
  month = floor_date(as.Date(index(rf_monthly_raw)), unit = "month"),
  RiskFree = as.numeric(rf_monthly_raw) / 100
)

# ── 2. Daten vorbereiten ──────────────────────────────────
returns_df <- all_results %>%
  filter(FormationYear == 2014) %>%
  left_join(sp500_monthly, by = "month") %>%
  left_join(rf_monthly_df, by = "month") %>%
  rename(SmallValue = PortfolioReturn, SP500 = SP500_Return) %>%
  arrange(month) %>%
  mutate(RiskFree = na.locf(RiskFree, na.rm = FALSE)) %>%
  drop_na(SmallValue, SP500, RiskFree)

# ── 3. Zielrendite setzen (z. B. 6 % p.a.) ─────────────────
target_annual_return <- 0.07
target_return <- (1 + target_annual_return)^(1/12) - 1  # ≈ 0.00487

# ── 4. Erwartete Monatsrenditen und Kovarianzmatrix ───────
mu <- colMeans(returns_df[, c("SmallValue", "SP500")], na.rm = TRUE)
cov_mat <- cov(returns_df[, c("SmallValue", "SP500")])

# ── 5. Zielrendite prüfen und ggf. anpassen ───────────────
if (any(is.na(mu))) stop("❌ Erwartete Renditen enthalten NA – überprüfe die Inputdaten.")

range_ret <- range(mu, na.rm = TRUE)
if (target_return < range_ret[1] || target_return > range_ret[2]) {
  message(paste("⚠️ Zielrendite nicht erreichbar: ", round(target_return, 5),
                "nicht in [", round(range_ret[1], 5), ",", round(range_ret[2], 5), "] – Ziel = max(mu)"))
  target_return <- max(mu, na.rm = TRUE)
}

# ── 6. Quadratische Optimierung ─────────────────────────────
Dmat <- 2 * cov_mat
dvec <- rep(0, 2)
Amat <- cbind(1, mu, diag(2))  # Summe = 1, Rendite = Ziel, keine Shorts
bvec <- c(1, target_return, 0, 0)
meq  <- 2

opt <- solve.QP(Dmat, dvec, Amat, bvec, meq)

# ── 7. Gewichtung berechnen inkl. risikofreiem Anteil ──────
w_risky <- opt$solution
w_riskfree <- 1 - sum(w_risky)
weights <- c(w_risky, w_riskfree)
names(weights) <- c("SmallValue", "SP500", "RiskFree")

# ── 8. Ergebnisse anzeigen ─────────────────────────────────
print(round(weights, 4))



### Print Debug Info 
print(paste("=== Formation Year:", fy, "==="))
print("Expected returns:")
print(round(mu, 5))
print("Covariance matrix:")
print(round(cov_mat, 5))
print("Optimal weights:")
print(round(weights, 5))


# ── Pakete ─────────────────────────────
library(quadprog)
library(dplyr)
library(quantmod)
library(lubridate)
library(zoo)  # für na.locf

# ── 1. FRED-Daten laden: 3-Monats-TBill (DTB3) ─────────────
getSymbols("DTB3", src = "FRED", from = "2013-01-01")
rf_monthly_raw <- to.monthly(DTB3, indexAt = "lastof", OHLC = FALSE)
rf_monthly_df <- data.frame(
  month = floor_date(as.Date(index(rf_monthly_raw)), unit = "month"),
  RiskFree = as.numeric(rf_monthly_raw) / 100
)

# ── 2. Daten vorbereiten ──────────────────────────────────
returns_df <- all_results %>%
  filter(FormationYear == 2015) %>%
  left_join(sp500_monthly, by = "month") %>%
  left_join(rf_monthly_df, by = "month") %>%
  rename(SmallValue = PortfolioReturn, SP500 = SP500_Return) %>%
  arrange(month) %>%
  mutate(RiskFree = na.locf(RiskFree, na.rm = FALSE)) %>%
  drop_na(SmallValue, SP500, RiskFree)

# ── 3. Zielrendite setzen (z. B. 6 % p.a.) ─────────────────
target_annual_return <- 0.07
target_return <- (1 + target_annual_return)^(1/12) - 1  # ≈ 0.00487

# ── 4. Erwartete Monatsrenditen und Kovarianzmatrix ───────
mu <- colMeans(returns_df[, c("SmallValue", "SP500")], na.rm = TRUE)
cov_mat <- cov(returns_df[, c("SmallValue", "SP500")])

# ── 5. Zielrendite prüfen und ggf. anpassen ───────────────
if (any(is.na(mu))) stop("❌ Erwartete Renditen enthalten NA – überprüfe die Inputdaten.")

range_ret <- range(mu, na.rm = TRUE)
if (target_return < range_ret[1] || target_return > range_ret[2]) {
  message(paste("⚠️ Zielrendite nicht erreichbar: ", round(target_return, 5),
                "nicht in [", round(range_ret[1], 5), ",", round(range_ret[2], 5), "] – Ziel = max(mu)"))
  target_return <- max(mu, na.rm = TRUE)
}

# ── 6. Quadratische Optimierung ─────────────────────────────
Dmat <- 2 * cov_mat
dvec <- rep(0, 2)
Amat <- cbind(1, mu, diag(2))  # Summe = 1, Rendite = Ziel, keine Shorts
bvec <- c(1, target_return, 0, 0)
meq  <- 2

opt <- solve.QP(Dmat, dvec, Amat, bvec, meq)

# ── 7. Gewichtung berechnen inkl. risikofreiem Anteil ──────
w_risky <- opt$solution
w_riskfree <- 1 - sum(w_risky)
weights <- c(w_risky, w_riskfree)
names(weights) <- c("SmallValue", "SP500", "RiskFree")

# ── 8. Ergebnisse anzeigen ─────────────────────────────────
print(round(weights, 4))




#Jahr 2016


# Zielrendite: 6 % p.a. in monatlicher Rate
target_annual_return <- 0.06
target_return <- (1 + target_annual_return)^(1/12) - 1  # ≈ 0.00487

# Filter: Nur 5-Jahres-Zeitraum ab Formation Year 2014
returns_df <- all_results %>%
  filter(FormationYear == 2016) %>%
  left_join(sp500_monthly, by = "month") %>%
  rename(SmallValue = PortfolioReturn, SP500 = SP500_Return) %>%
  drop_na()

# Sicherheitscheck
if (nrow(returns_df) < 12) {
  stop("❌ Nicht genug Beobachtungen für 2014 vorhanden.")
}

# Erwartete Monatsrenditen und Kovarianzmatrix
mu <- colMeans(returns_df[, c("SmallValue", "SP500")])
cov_mat <- cov(returns_df[, c("SmallValue", "SP500")])

# Zielrendite prüfen
range_ret <- range(mu)
if (target_return < range_ret[1] || target_return > range_ret[2]) {
  message(paste("⚠️ Zielrendite nicht erreichbar: ", round(target_return, 5),
                "nicht in [", round(range_ret[1], 5), ",", round(range_ret[2], 5), "] – Ziel = max(mu)"))
  target_return <- max(mu)
}

# QP vorbereiten (Long-only)
Dmat <- 2 * cov_mat
dvec <- rep(0, 2)
Amat <- cbind(1, mu, diag(2))  # sum=1, target return, no shorting
bvec <- c(1, target_return, 0, 0)
meq  <- 2

# Optimieren
opt <- solve.QP(Dmat, dvec, Amat, bvec, meq)
weights <- opt$solution
names(weights) <- c("SmallValue", "SP500")

# Ergebnis anzeigen
print(round(weights, 4))

##Performance vergleich

returns_df <- returns_df %>%
  mutate(OptimalPortfolio = weights["SmallValue"] * SmallValue +
           weights["SP500"] * SP500,
         OptimalGrowth = cumprod(1 + OptimalPortfolio),
         SV_Growth = cumprod(1 + SmallValue),
         SP500_Growth = cumprod(1 + SP500))

library(ggplot2)

returns_df %>%
  select(month, OptimalGrowth, SV_Growth, SP500_Growth) %>%
  pivot_longer(-month, names_to = "Strategy", values_to = "Growth") %>%
  ggplot(aes(x = month, y = Growth, color = Strategy)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Portfolio Growth – Formation Year 2014", y = "Growth Index", x = "Month")



#
#--------------------------optimierung des 5-Aktien Portfolios-----------------------------
#
# S&P 500 Preisindex laden (z. B. Symbol "^GSPC")
getSymbols("^GSPC", from = "1925-01-01")

# Monatliche Preise
sp500_monthly <- to.monthly(GSPC, indexAt = "lastof", OHLC = TRUE)

# Monatsrenditen berechnen
sp500_returns <- monthlyReturn(Cl(sp500_monthly)) * 100  # in %

# Erwarteter Monatsreturn & SD berechnen
mean_return <- mean(sp500_returns, na.rm = TRUE)
sd_return   <- sd(sp500_returns, na.rm = TRUE)

cat("Mean monthly return (%):", round(mean_return, 4), "\n")
cat("Monthly SD (%):", round(sd_return, 4), "\n")



#Berechnung der richtigen Portffolio Weights


# ── 1) Get S&P 500 daily data from Yahoo ─────────────
getSymbols("^GSPC", from = "1925-01-01")

# ── 2) Convert to monthly close prices ───────────────
sp500_monthly <- to.monthly(GSPC, indexAt = "lastof", OHLC = TRUE)

# ── 3) Compute monthly returns (%) ───────────────────
sp500_returns_xts <- monthlyReturn(Cl(sp500_monthly)) * 100  # convert to %

# ── 4) Convert to data.frame with clear columns ──────
sp500_returns_df <- data.frame(
  Date = index(sp500_returns_xts),
  SP500_Return = coredata(sp500_returns_xts)
)

# ── 5) Inspect first rows ────────────────────────────
head(sp500_returns_df)

# ── 6) Also compute mean and SD for info ─────────────
mean_return <- mean(sp500_returns_df$SP500_Return, na.rm = TRUE)
sd_return <- sd(sp500_returns_df$SP500_Return, na.rm = TRUE)

cat("Mean monthly return (%):", round(mean_return, 4), "\n")
cat("Monthly SD (%):", round(sd_return, 4), "\n")

# Optional: save to CSV
write.csv(sp500_returns_df, "sp500_monthly_returns.csv", row.names = FALSE)