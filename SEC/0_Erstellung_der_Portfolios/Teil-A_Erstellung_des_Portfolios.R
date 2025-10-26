#####============================================================
#####  PART A – SEC-Daten, Buckets, Annual Returns
#####============================================================

# ── Libraries ─────────────────────────────────────────────────
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(tidyquant)
library(furrr)
library(progressr)     # Fortschritts-Balken

handlers(global = TRUE)  # Standard-Handler aktivieren

# ── Helper ▸ Survivorship-Bias-Fix ────────────────────────────
get_eoy_price <- function(ticker, year) {
  tgt <- as.Date(paste0(year, "-12-31"))
  px  <- tryCatch(
    tq_get(ticker,
           from = tgt - days(5), to = tgt,
           get  = "stock.prices") |>
      filter(!is.na(adjusted)) |>
      arrange(desc(date)) |>
      slice_head(n = 1) |>
      pull(adjusted),
    error = \(e) NA_real_)
  if (length(px) == 0) px <- NA_real_
  px
}

one_year_return <- function(ticker, year0) {
  p0 <- get_eoy_price(ticker, year0)
  p1 <- get_eoy_price(ticker, year0 + 1)
  perf <- if (is.na(p0) | is.na(p1)) -1 else (p1 - p0) / p0
  tibble(ticker, Year = year0 + 1, Performance = perf)
}

compute_panel_no_bias <- function(ticker_vec,
                                  first_year = 2012,
                                  last_year  = 2022,
                                  n_workers  = 4) {
  plan(multisession, workers = n_workers)
  with_progress({
    p <- progressor(
      steps = (last_year - first_year) * length(ticker_vec))
    map_dfr(first_year:(last_year - 1), \(y) {
      future_map_dfr(
        ticker_vec,
        \(tkr) { p(); one_year_return(tkr, y) },
        .options = furrr_options(seed = TRUE)
      )
    })
  })
}

# ── Pfade ─────────────────────────────────────────────────────
setwd("C:/_Maximilian_Peter_Masterarbeit_2025_/code")
folder_path <- "C:/_Maximilian_Peter_Masterarbeit_2025_/Data/Bulk_Download_22_11/"
file_list   <- list.files(folder_path, pattern = "\\.json$", full.names = TRUE)

# ── A-1  Public Float ─────────────────────────────────────────
public_float_data <- with_progress({
  p <- progressor(steps = length(file_list))
  map_dfr(file_list, \(f) {
    p()
    d <- fromJSON(f, flatten = TRUE)
    if (!is.null(d$facts$dei$EntityPublicFloat$units$USD$val) &&
        !is.null(d$facts$dei$EntityPublicFloat$units$USD$end)) {
      tibble(FileName = basename(f),
             Date  = as.Date(d$facts$dei$EntityPublicFloat$units$USD$end),
             Value = d$facts$dei$EntityPublicFloat$units$USD$val)
    }
  })
}) |>
  distinct(FileName, Date, .keep_all = TRUE) |>
  mutate(Year = year(Date),
         CIK  = str_extract(FileName, "\\d+")) |>
  group_by(FileName, Year) |>
  slice_min(Date) |>
  ungroup() |>
  filter(Year >= 2013 & Year <= 2022)

# ── A-2  Stockholders’ Equity ─────────────────────────────────
stockholders_equity <- with_progress({
  p <- progressor(steps = length(file_list))
  map_dfr(file_list, \(f) {
    p()
    d <- fromJSON(f, flatten = TRUE)
    if (!is.null(d$facts$`us-gaap`$StockholdersEquity$units$USD$val) &&
        !is.null(d$facts$`us-gaap`$StockholdersEquity$units$USD$end)) {
      tibble(FileName = basename(f),
             Date   = as.Date(d$facts$`us-gaap`$StockholdersEquity$units$USD$end),
             Equity = d$facts$`us-gaap`$StockholdersEquity$units$USD$val)
    }
  })
}) |>
  distinct(FileName, Date, .keep_all = TRUE) |>
  mutate(Year = year(Date),
         CIK  = str_extract(FileName, "\\d+")) |>
  group_by(FileName, Year) |>
  slice_min(Date) |>
  ungroup() |>
  filter(between(Year, 2013, 2022)) |>
  dplyr::select(CIK, Year, Equity)    # <— hier explizit dplyr::select

# ── A-3  Ticker-Mapping ───────────────────────────────────────
tickers <- fromJSON("C:/_Maximilian_Peter_Masterarbeit_2025_/Data/company_tickers.json") |>
  bind_rows() |>
  mutate(cik_str = str_pad(as.character(cik_str), 10, pad = "0"))

# ── A-4  Combine + Book-to-Market ─────────────────────────────
geo_data_combined <- public_float_data |>
  left_join(tickers, by = c("CIK" = "cik_str")) |>
  left_join(stockholders_equity, by = c("CIK", "Year")) |>
  mutate(BookToMarket = Equity / Value) |>
  filter(Value >= 4e7)

# ── A-5  Annual Returns (Survivorship-free) ───────────────────
tickers_list <- geo_data_combined |>
  filter(!is.na(ticker) & ticker != "") |>
  pull(ticker) |>
  unique()

all_yearly_performance <- compute_panel_no_bias(
  ticker_vec = tickers_list,
  first_year = 2012, last_year = 2022, n_workers = 4)

saveRDS(all_yearly_performance, "performance_only.rds")

# ── A-6  Size- und Book-to-Market-Buckets + Performance ─────────────────

# 1) Median-Schwelle für Small/Big (Public Float 2013)
median_2013 <- geo_data_combined |>
  dplyr::filter(Year == 2013) |>
  dplyr::summarise(Med = median(Value, na.rm = TRUE)) |>
  dplyr::pull(Med)

# 2) Size-Bucket anlegen
bucket_size <- geo_data_combined |>
  dplyr::mutate(SizeBucket = if_else(Value < median_2013, "Small", "Big")) |>
  dplyr::select(CIK, ticker, Year, SizeBucket)

# 3) Book-to-Market-Bucket (High / Neutral / Low)
bucket_btm <- geo_data_combined |>
  dplyr::group_by(Year) |>
  dplyr::mutate(
    pctl      = percent_rank(BookToMarket),
    BM_Bucket = case_when(
      pctl >= 0.7              ~ "High",
      pctl >= 0.3 & pctl < 0.7 ~ "Neutral",
      TRUE                     ~ "Low")
  ) |>
  dplyr::ungroup() |>
  dplyr::select(CIK, ticker, Year, BM_Bucket)

# winsorised performance per bucket
final_size <- all_yearly_performance |>
  left_join(bucket_size, by = c("ticker", "Year")) |>
  filter(!is.na(SizeBucket)) |>
  mutate(Performance = pmin(pmax(Performance, -1), 2))

perf_size_clean <- final_size |>
  group_by(Year, SizeBucket) |>
  summarise(AvgPerf = mean(Performance, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = SizeBucket, values_from = AvgPerf) |>
  mutate(Diff_Small_Minus_Big = Small - Big)

final_btm <- all_yearly_performance |>
  left_join(bucket_btm, by = c("ticker", "Year")) |>
  filter(!is.na(BM_Bucket)) |>
  mutate(Performance = pmin(pmax(Performance, -1), 2))

perf_btm_clean <- final_btm |>
  group_by(Year, BM_Bucket) |>
  summarise(AvgPerf = mean(Performance, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = BM_Bucket, values_from = AvgPerf) |>
  mutate(Diff_Value_Minus_Growth = High - Low)

saveRDS(geo_data_combined, "geo_data_combined.rds")

