## ====================================================================
## Diagnostic: California per-year EoL recycling vector verification
##
## Reconstructs exactly what the main5 pipeline ingests for California
## from the four upstream files:
##   Outputs/EVLIB_Flows_detail_ACCII.csv               (LDV)
##   Outputs/BESS_Retire_Vector_byStateSegProp_ACCII.csv (LDV second-life BESS)
##   Outputs/HDV/HDV_EV_Turnover_ACCII.csv               (HDV truck)
##   Outputs/HDV/HDV_BESS_Retire_ACCII.csv               (HDV second-life BESS)
##
## Outputs:
##   Outputs/Recycling_Plots_main/CA_Recycle_Vector_Diagnostic.csv
##   Outputs/Recycling_Plots_main/NA_EoL_Counts_Diagnostic.csv
##   Outputs/Recycling_Plots_main/CA_LDV_BEV_Car_VectorMatrix.csv
## ====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr); library(stringr)
})

OUT      <- file.path(getwd(), "Outputs")
PLOT_DIR <- file.path(OUT, "Recycling_Plots_main")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

## ---- helper: same logic as main5 name_vector_with_years() -----------
expand_vec <- function(vec_string, year) {
  vec <- as.numeric(strsplit(as.character(vec_string), "\\|")[[1]])
  if (!length(vec)) return(tibble(Sale_Year = integer(), n = numeric()))
  tibble(Sale_Year = year - (seq_along(vec) - 1L), n = vec)
}

## ---- 1. Read California pieces --------------------------------------
ldv <- read_csv(file.path(OUT, "EVLIB_Flows_detail_ACCII.csv"),
                show_col_types = FALSE) %>%
  filter(State == "California") %>%
  transmute(State, Segment, Propulsion, Year,
            LIB_recycling_annual = LIB_recycling,
            recycle_df = map2(LIB_recycling_vector, Year, expand_vec)) %>%
  unnest(recycle_df)

ldv_bess_path <- file.path(OUT, "BESS_Retire_Vector_byStateSegProp_ACCII.csv")
ldv_bess <- if (file.exists(ldv_bess_path)) {
  read_csv(ldv_bess_path, show_col_types = FALSE) %>%
    filter(State == "California") %>%
    transmute(State, Segment, Propulsion, Year,
              retire_df = map2(BESS_retire_vector, Year, expand_vec)) %>%
    unnest(retire_df)
} else NULL

hdv_path <- file.path(OUT, "HDV", "HDV_EV_Turnover_ACCII.csv")
hdv <- if (file.exists(hdv_path)) {
  read_csv(hdv_path, show_col_types = FALSE) %>%
    filter(State == "California") %>%
    transmute(State, Vehicle, Year,
              recycle_df = map2(LIB_recycling_vector, Year, expand_vec)) %>%
    unnest(recycle_df)
} else NULL

hdv_bess_path <- file.path(OUT, "HDV", "HDV_BESS_Retire_ACCII.csv")
hdv_bess <- if (file.exists(hdv_bess_path)) {
  read_csv(hdv_bess_path, show_col_types = FALSE) %>%
    filter(State == "California") %>%
    transmute(State, Vehicle, Year,
              retire_df = map2(BESS_retire_vector, Year, expand_vec)) %>%
    unnest(retire_df)
} else NULL

## ---- 2. Per-Year California totals ----------------------------------
ldv_yr  <- ldv     %>% group_by(Year) %>% summarise(LDV      = sum(n), .groups = "drop")
bess_yr <- ldv_bess %>% group_by(Year) %>% summarise(LDV_BESS = sum(n), .groups = "drop")
hdv_yr  <- hdv     %>% group_by(Year) %>% summarise(HDV      = sum(n), .groups = "drop")
hbess_yr <- hdv_bess %>% group_by(Year) %>% summarise(HDV_BESS = sum(n), .groups = "drop")

ca_combo <- ldv_yr %>%
  full_join(bess_yr,  by = "Year") %>%
  full_join(hdv_yr,   by = "Year") %>%
  full_join(hbess_yr, by = "Year") %>%
  arrange(Year) %>%
  mutate(across(-Year, ~ replace_na(.x, 0)),
         Total = LDV + LDV_BESS + HDV + HDV_BESS) %>%
  filter(Year >= 2020 & Year <= 2050)

write_csv(ca_combo, file.path(PLOT_DIR, "CA_Recycle_Vector_Diagnostic.csv"))

cat("\n========== California EoL battery COUNT per Year (recycled) ==========\n")
print(ca_combo, n = Inf)

## ---- 3. Vector reconstruction sanity check -------------------------
##
## For each LDV row, the LIB_recycling (annual scalar) MUST equal the
## sum of the LIB_recycling_vector values. Verify:
sanity <- ldv %>%
  group_by(Year, Segment, Propulsion, LIB_recycling_annual) %>%
  summarise(vector_sum = sum(n), .groups = "drop") %>%
  mutate(diff = vector_sum - LIB_recycling_annual)
cat("\n========== Vector reconstruction check (LDV CA, |diff| > 0.5) =========\n")
bad <- sanity %>% filter(abs(diff) > 0.5)
if (nrow(bad) == 0) {
  cat("  PASS — every California LDV row has vector sum == annual LIB_recycling\n")
} else {
  cat("  FAIL — ", nrow(bad), " rows mismatch:\n", sep = "")
  print(bad, n = Inf)
}

## ---- 4. NA (continent) totals - to compare with plot 06 ------------
##
## Re-run the LDV / LDV-BESS / HDV / HDV-BESS expansion across ALL states
## and sum to a single NA series. This is what the main5 plot uses
## (before Battery_Cap × Cathode_Mix transforms).
all_states_count <- function() {
  ldv <- read_csv(file.path(OUT, "EVLIB_Flows_detail_ACCII.csv"),
                  show_col_types = FALSE) %>%
    transmute(Year, recycle_df = map2(LIB_recycling_vector, Year, expand_vec)) %>%
    unnest(recycle_df) %>%
    group_by(Year) %>% summarise(LDV = sum(n), .groups = "drop")
  bess <- if (file.exists(ldv_bess_path)) {
    read_csv(ldv_bess_path, show_col_types = FALSE) %>%
      transmute(Year, retire_df = map2(BESS_retire_vector, Year, expand_vec)) %>%
      unnest(retire_df) %>%
      group_by(Year) %>% summarise(LDV_BESS = sum(n), .groups = "drop")
  } else tibble(Year = integer(), LDV_BESS = numeric())
  h <- if (file.exists(hdv_path)) {
    read_csv(hdv_path, show_col_types = FALSE) %>%
      transmute(Year, recycle_df = map2(LIB_recycling_vector, Year, expand_vec)) %>%
      unnest(recycle_df) %>%
      group_by(Year) %>% summarise(HDV = sum(n), .groups = "drop")
  } else tibble(Year = integer(), HDV = numeric())
  hb <- if (file.exists(hdv_bess_path)) {
    read_csv(hdv_bess_path, show_col_types = FALSE) %>%
      transmute(Year, retire_df = map2(BESS_retire_vector, Year, expand_vec)) %>%
      unnest(retire_df) %>%
      group_by(Year) %>% summarise(HDV_BESS = sum(n), .groups = "drop")
  } else tibble(Year = integer(), HDV_BESS = numeric())
  ldv %>% full_join(bess, by = "Year") %>% full_join(h, by = "Year") %>% full_join(hb, by = "Year") %>%
    arrange(Year) %>%
    mutate(across(-Year, ~ replace_na(.x, 0)),
           Total = LDV + LDV_BESS + HDV + HDV_BESS) %>%
    filter(Year >= 2020 & Year <= 2050)
}
na_combo <- all_states_count()
write_csv(na_combo, file.path(PLOT_DIR, "NA_EoL_Counts_Diagnostic.csv"))

cat("\n========== NA (US+Canada+Mexico) EoL battery COUNT per Year ==========\n")
print(na_combo, n = Inf)

## ---- 5. Quick MT estimate using NA averages -----------------------
##
## Approx avg battery capacity (kWh / batt):
##   LDV BEV  ~ 75 kWh,  LDV PHEV ~ 14 kWh  -> blend ~ 60 kWh
##   LDV BESS retire = ex-LDV cells repurposed first -> ~60 kWh equivalent
##   HDV (medium + heavy) ~ 350 kWh blend
##   Pack mass density ~ 6 kg / kWh (i.e. 6 t / GWh / 1000? -> 6 kg/kWh)
kwh_LDV  <- 60
kwh_BESS <- 60
kwh_HDV  <- 350
kwh_HBESS<- 350
kg_per_kwh <- 6

na_mt <- na_combo %>%
  mutate(
    GWh = (LDV * kwh_LDV + LDV_BESS * kwh_BESS + HDV * kwh_HDV + HDV_BESS * kwh_HBESS) / 1e6,
    Batt_Mass_MT_millions = GWh * 1e6 * kg_per_kwh / 1000 / 1e6
  )
cat("\n========== NA EoL converted to GWh and Mass (rough MT estimate) ==========\n")
print(na_mt %>% select(Year, LDV, LDV_BESS, HDV, HDV_BESS, GWh, Batt_Mass_MT_millions),
      n = Inf)

cat("\n  -> NA 2050 EoL Mass (millions MT, this estimate)  : ",
    sprintf("%.2f", na_mt$Batt_Mass_MT_millions[na_mt$Year == 2050]), "\n")
cat("  -> Plot_06 ('Increasing Batt Cap')  shows  ~10 M MT\n")
cat("  -> Colleague's reference plot      shows  ~25 M MT\n")

## ---- 6. Detailed vector matrix for CA Car BEV ----------------------
##
## Save as CSV so it's easy to compare row-by-row with hand-crunch.
ca_carbev_mat <- ldv %>%
  filter(Segment == "Car", Propulsion == "BEV", Year >= 2025, Year <= 2050) %>%
  select(Year, Sale_Year, n) %>%
  pivot_wider(names_from = Sale_Year, values_from = n, values_fill = 0) %>%
  arrange(Year)
write_csv(ca_carbev_mat, file.path(PLOT_DIR, "CA_LDV_BEV_Car_VectorMatrix.csv"))

cat("\n  -> Saved 3 diagnostic CSVs to: Outputs/Recycling_Plots_main/\n")
cat("       CA_Recycle_Vector_Diagnostic.csv     (CA per-year totals)\n")
cat("       NA_EoL_Counts_Diagnostic.csv         (NA per-year totals)\n")
cat("       CA_LDV_BEV_Car_VectorMatrix.csv      (Year x Sale_Year matrix)\n")
