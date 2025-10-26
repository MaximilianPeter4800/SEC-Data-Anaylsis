library(MASS)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

# 1️⃣ Parameter
n_years  <- 20
n_months <- 12 * n_years

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

# 3️⃣ EINEN Faktorpfad simulieren
set.seed(42)
factor_path <- as_tibble(mvrnorm(n_months, factor_means, factor_cov)) %>%
  setNames(c("Mkt_RF", "SMB", "HML")) %>%
  mutate(RF = rnorm(n_months, rf_monthly_mean, rf_monthly_sd))

# 4️⃣ Richtige Loadings: pro Jahr 1 Zeile, 12 Monate lang wiederholen
loadings_for_path <- sim_results[1:n_years, ] %>%
  slice(rep(1:n(), each = 12))

stopifnot(nrow(loadings_for_path) == n_months)

# 5️⃣ Monatliche Returns:
# Portfolio-Return (mit empirischen Loadings)
excess_portfolio <- loadings_for_path$Intercept +
  loadings_for_path$Mkt_RF * factor_path$Mkt_RF +
  loadings_for_path$SMB * factor_path$SMB +
  loadings_for_path$HML * factor_path$HML
port_monthly <- (excess_portfolio + factor_path$RF) / 100

# Market-Return (nur Mkt_RF = 1)
excess_market <- factor_path$Mkt_RF
mkt_monthly <- (excess_market + factor_path$RF) / 100

# 6️⃣ Auf Jahresbasis umwandeln:
monthly_df <- tibble(
  month = 1:n_months,
  port = port_monthly,
  mkt = mkt_monthly
) %>%
  mutate(year = ceiling(month / 12)) %>%
  group_by(year) %>%
  summarise(
    port_annual = prod(1 + port) - 1,
    mkt_annual  = prod(1 + mkt) - 1
  )

print(monthly_df)

# 7️⃣ Korrelation berechnen (annualisiert!)
correlation <- cor(monthly_df$port_annual, monthly_df$mkt_annual)
cat("Correlation (Annual Portfolio vs. Market):", round(correlation, 4), "\n")

# 8️⃣ Mittelwerte & SD
port_mean <- mean(monthly_df$port_annual)
port_sd   <- sd(monthly_df$port_annual)
mkt_mean  <- mean(monthly_df$mkt_annual)
mkt_sd    <- sd(monthly_df$mkt_annual)

# 9️⃣ Kovarianzmatrix ready für QP
cov_pm <- correlation * port_sd * mkt_sd
cov_mat <- matrix(
  c(port_sd^2, cov_pm,
    cov_pm, mkt_sd^2),
  nrow = 2, byrow = TRUE
)
print(cov_mat)

# 10️⃣ Target-Returns
print(c(port_mean, mkt_mean))



#####-------------------------------

# ───────────────────────────────
# 1️⃣ Packages
library(quadprog)
library(dplyr)

# ───────────────────────────────
# 2️⃣ Inputs: aus deinen Simulationsergebnissen!

# --- Werte aus deiner Simulation ---
# Beispielwerte: bitte mit deinen echten Simulationsergebnissen ersetzen!
# Small Cap Value:
mu_scv <- 13.31 / 100  # Mean CAGR 20 Jahre (5 Stocks)
sd_scv <- 5.78 / 100   # SD

# Market:
mu_mkt <- 10.03 / 100  # Mean CAGR 20 Jahre (Market)
sd_mkt <- 4.49 / 100   # SD

# Annahme: Korrelation aus Faktoren simuliert:
corr <- 0.9334  # Beispiel, bitte anpassen oder schätzen!

# Kovarianzmatrix:
cov_mat <- matrix(
  c(sd_scv^2, corr * sd_scv * sd_mkt,
    corr * sd_scv * sd_mkt, sd_mkt^2),
  nrow = 2
)

# Erwartungsvektor:
mu <- c(mu_scv, mu_mkt)

# ───────────────────────────────
# 3️⃣ Minimum Variance Portfolio mit Constraints: 
# w1 >= 0, w2 >= 0, w1 + w2 = 1

Dmat <- 2 * cov_mat
dvec <- rep(0, 2)
Amat <- cbind(
  c(1, 1),   # Sum of weights = 1
  diag(2)    # No shorting: w1 >= 0, w2 >= 0
)
bvec <- c(1, 0, 0)
meq  <- 1

result <- solve.QP(Dmat, dvec, Amat, bvec, meq)

weights <- result$solution
names(weights) <- c("SmallCapValue", "Market")

# ───────────────────────────────
# 4️⃣ Ergebnis: optimale Gewichtung
print(round(weights, 4))

# Erwarteter Portfolio-Return:
expected_return <- sum(weights * mu)
expected_sd <- sqrt( t(weights) %*% cov_mat %*% weights )
expected_sharpe <- (expected_return - 0.0027) / expected_sd  # monthly RF ~ 0.27% => annual ~ 3%

cat("Expected Portfolio Return (%):", round(expected_return * 100, 2), "\n")
cat("Expected Portfolio SD (%):", round(expected_sd * 100, 2), "\n")
cat("Expected Sharpe (annualized):", round(expected_sharpe * sqrt(12), 3), "\n")


###---------------------------------


library(quadprog)
library(ggplot2)
library(dplyr)

# === 1) Deine Inputs ===
mu_scv <- 13.31 / 100  # 20Y Mean CAGR Small-Cap-Value
sd_scv <- 5.78 / 100

mu_mkt <- 10.03 / 100  # 20Y Mean CAGR Market
sd_mkt <- 4.49 / 100

corr <- 0.9334

cov_mat <- matrix(
  c(sd_scv^2, corr * sd_scv * sd_mkt,
    corr * sd_scv * sd_mkt, sd_mkt^2),
  nrow = 2
)

mu <- c(mu_scv, mu_mkt)

# === 2) Frontier: mehrere Zielrenditen ===
target_returns <- seq(min(mu), max(mu), length.out = 50)

frontier <- data.frame(
  target_return = numeric(),
  sd = numeric()
)

for (target in target_returns) {
  Dmat <- 2 * cov_mat
  dvec <- rep(0, 2)
  Amat <- cbind(
    1,       # Sum weights = 1
    mu,      # Achieve target return
    diag(2)  # No shorting
  )
  bvec <- c(1, target, 0, 0)
  meq  <- 2
  
  result <- tryCatch(
    solve.QP(Dmat, dvec, Amat, bvec, meq),
    error = function(e) NULL
  )
  
  if (!is.null(result)) {
    w <- result$solution
    port_return <- sum(w * mu)
    port_sd <- sqrt(t(w) %*% cov_mat %*% w)
    frontier <- rbind(frontier, data.frame(
      target_return = port_return * 100,
      sd = port_sd * 100
    ))
  }
}

# === 3) Plot ===
ggplot(frontier, aes(x = sd, y = target_return)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(aes(x = sd_scv * 100, y = mu_scv * 100), color = "red", size = 3) +
  geom_point(aes(x = sd_mkt * 100, y = mu_mkt * 100), color = "darkgreen", size = 3) +
  annotate("text", x = sd_scv * 100, y = mu_scv * 100 + 0.2, label = "SCV", color = "red") +
  annotate("text", x = sd_mkt * 100, y = mu_mkt * 100 + 0.2, label = "Market", color = "darkgreen") +
  labs(
    title = "Efficient Frontier (SCV + Market, No Shorting)",
    x = "Standard Deviation (%)",
    y = "Expected Return (%)"
  ) +
  theme_minimal()

