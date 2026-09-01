## ====================================================================
## California cumulative spent-battery diagnostic
##
## Computes per-year cumulative retirement (LDV LIB_recycling + LDV BESS
## retire + HDV LIB_recycling + HDV BESS retire) for California and the
## U.S. as a whole, under both ACCII and Repeal, so we can update text
## like "California alone contributes about XX million batteries..."
## ====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

OUT_DIR <- "Outputs/Visualization"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

compute_annual <- function(scen, state_filter = NULL) {
  ldv <- read_csv(paste0("Outputs/EVLIB_Flows_detail_", scen, ".csv"),
                  show_col_types = FALSE)
  if (!is.null(state_filter)) ldv <- ldv %>% filter(State == state_filter)
  ldv_yr <- ldv %>% group_by(Year) %>%
    summarise(LDV = sum(LIB_recycling, na.rm = TRUE), .groups = "drop")

  bess_p <- paste0("Outputs/BESS_Retire_Vector_byStateSegProp_", scen, ".csv")
  bess_yr <- if (file.exists(bess_p)) {
    b <- read_csv(bess_p, show_col_types = FALSE)
    if (!is.null(state_filter)) b <- b %>% filter(State == state_filter)
    b %>% group_by(Year) %>%
      summarise(LDV_BESS = sum(BESS_retire_total, na.rm = TRUE), .groups = "drop")
  } else tibble(Year = integer(), LDV_BESS = numeric())

  ## HDV file holds all 3 countries; restrict to United States
  hdv_p <- paste0("Outputs/HDV/HDV_EV_Turnover_", scen, ".csv")
  hdv_yr <- if (file.exists(hdv_p)) {
    h <- read_csv(hdv_p, show_col_types = FALSE) %>%
      filter(Country == "United States")
    if (!is.null(state_filter)) h <- h %>% filter(State == state_filter)
    h %>% group_by(Year) %>%
      summarise(HDV = sum(LIB_recycling, na.rm = TRUE), .groups = "drop")
  } else tibble(Year = integer(), HDV = numeric())

  hb_p <- paste0("Outputs/HDV/HDV_BESS_Retire_", scen, ".csv")
  hb_yr <- if (file.exists(hb_p)) {
    h <- read_csv(hb_p, show_col_types = FALSE) %>%
      filter(Country == "United States")
    if (!is.null(state_filter)) h <- h %>% filter(State == state_filter)
    h %>% group_by(Year) %>%
      summarise(HDV_BESS = sum(BESS_retire_total, na.rm = TRUE), .groups = "drop")
  } else tibble(Year = integer(), HDV_BESS = numeric())

  ldv_yr %>%
    full_join(bess_yr, by = "Year") %>%
    full_join(hdv_yr,  by = "Year") %>%
    full_join(hb_yr,   by = "Year") %>%
    arrange(Year) %>%
    mutate(across(-Year, ~ replace_na(.x, 0)),
           Total = LDV + LDV_BESS + HDV + HDV_BESS) %>%
    filter(Year >= 2020, Year <= 2050)
}

ca_ac <- compute_annual("ACCII",  "California") %>% mutate(Cum = cumsum(Total))
ca_re <- compute_annual("Repeal", "California") %>% mutate(Cum = cumsum(Total))
us_ac <- compute_annual("ACCII",  NULL)         %>% mutate(Cum = cumsum(Total))
us_re <- compute_annual("Repeal", NULL)         %>% mutate(Cum = cumsum(Total))

cat("\n=== California cumulative (LDV + LDV-BESS + HDV + HDV-BESS, M) ===\n")
ca_cum <- tibble(
  Year       = ca_ac$Year,
  ACCII_M    = ca_ac$Cum / 1e6,
  Repeal_M   = ca_re$Cum / 1e6,
  diff_M     = (ca_ac$Cum - ca_re$Cum) / 1e6,
  diff_pct   = (ca_ac$Cum - ca_re$Cum) / pmax(ca_re$Cum, 1) * 100
)
print(ca_cum %>%
        filter(Year %in% c(2025, 2030, 2032, 2035, 2040, 2042, 2045, 2048, 2050)),
      n = Inf)

cat("\n=== U.S. total cumulative (M) and CA share of U.S. total ===\n")
share <- tibble(
  Year       = ca_ac$Year,
  CA_M       = ca_ac$Cum / 1e6,
  US_M       = us_ac$Cum / 1e6,
  CA_share   = ca_ac$Cum / pmax(us_ac$Cum, 1) * 100
)
print(share %>%
        filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050)),
      n = Inf)

cat("\n=== Annual ACCII-Repeal gap for California (thousand batteries) ===\n")
gap <- tibble(
  Year       = ca_ac$Year,
  gap_annual_k    = (ca_ac$Total - ca_re$Total) / 1e3,
  gap_cumul_M     = (ca_ac$Cum   - ca_re$Cum)   / 1e6
)
print(gap, n = Inf)

## ---- Find divergence year (when gap first exceeds 5% of cumulative) ----
ca_diverge <- tibble(
  Year       = ca_ac$Year,
  gap_pct    = (ca_ac$Cum - ca_re$Cum) / pmax(ca_re$Cum, 1) * 100
)
first_5pct  <- ca_diverge %>% filter(gap_pct >= 5,  Year >= 2025) %>% slice(1)
first_10pct <- ca_diverge %>% filter(gap_pct >= 10, Year >= 2025) %>% slice(1)
first_20pct <- ca_diverge %>% filter(gap_pct >= 20, Year >= 2025) %>% slice(1)
cat("\n=== Divergence checkpoints (CA cumulative gap pct vs Repeal) ===\n")
cat("  Gap >=  5% first reached: ", ifelse(nrow(first_5pct)>0,  first_5pct$Year,  "n/a"), "\n")
cat("  Gap >= 10% first reached: ", ifelse(nrow(first_10pct)>0, first_10pct$Year, "n/a"), "\n")
cat("  Gap >= 20% first reached: ", ifelse(nrow(first_20pct)>0, first_20pct$Year, "n/a"), "\n")

## ---- Save summary ----
write_csv(ca_cum,  file.path(OUT_DIR, "CA_Cumulative_2020_2050.csv"))
write_csv(share,   file.path(OUT_DIR, "CA_share_of_US_cumulative.csv"))
cat("\n[Saved] Outputs/Visualization/CA_Cumulative_2020_2050.csv\n")
cat("[Saved] Outputs/Visualization/CA_share_of_US_cumulative.csv\n")
