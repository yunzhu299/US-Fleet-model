## ====================================================================
## 00-Build_Master_Tables.R
##
## Consolidates US / Canada / Mexico fleet model outputs into ONE Excel
## workbook (Fleet_Turnover_Master.xlsx). Each original table is kept
## intact as a separate sheet — column layouts unchanged, only stacked
## across countries/scenarios with two extra columns (Country, Scenario).
##
## Sheets written:
##   LDV_EVLIB             <- EVLIB_Flows_detail               (US/CA/MX)
##   LDV_ClosedLoop        <- ClosedLoop_StateTotals           (US/CA/MX)
##   LDV_AddRetire         <- ClosedLoop_AddRetire_byStateSeg  (US/CA)
##   HDV                   <- HDV_EV_Turnover                  (single file)
##   BESS_LDV_Retire       <- BESS_Retire_Vector_byStateSegProp(US/CA/MX)
##   BESS_LDV_Stock        <- BESS_Stock_Vector_byStateSegProp (US/CA/MX)
##   BESS_HDV_Retire       <- HDV_BESS_Retire                  (single file)
##   BESS_HDV_Stock        <- HDV_BESS_Stock                   (single file)
##   US_LDV_AddRet_byPowertrain  <- aggregated US national LDV flows by
##                                  Year x Scenario x Powertrain (long fmt)
##
## Run from project root (Fleet model/)
## ====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(writexl)
})

BASE_DIR   <- getwd()
OUTPUT_DIR <- file.path(BASE_DIR, "Outputs")

safe_read <- function(path, ...) {
  if (!file.exists(path)) {
    message("  [skip] not found: ", basename(path))
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE, ...)
}

read_tag <- function(path, country, scenario) {
  df <- safe_read(path)
  if (is.null(df)) return(NULL)
  df %>% mutate(Country = country, Scenario = scenario)
}

SCENARIOS <- c("ACCII", "Repeal")

## =====================================================================
## 1.  LDV
## =====================================================================
cat("=== Reading LDV tables ===\n")

ldv_evlib <- map_dfr(SCENARIOS, function(scen) {
  bind_rows(
    read_tag(file.path(OUTPUT_DIR,           paste0("EVLIB_Flows_detail_", scen, ".csv")), "United States", scen),
    read_tag(file.path(OUTPUT_DIR, "Canada", paste0("EVLIB_Flows_detail_", scen, ".csv")), "Canada",        scen),
    read_tag(file.path(OUTPUT_DIR, "Mexico", paste0("EVLIB_Flows_detail_", scen, ".csv")), "Mexico",        scen)
  )
})
cat("  LDV_EVLIB:        ", nrow(ldv_evlib), "rows\n")

# Optional: ICE stock vector by State x Year x Segment x Age (US only)
ice_stock_us <- map_dfr(SCENARIOS, function(scen) {
  df <- safe_read(file.path(OUTPUT_DIR, paste0("ICE_Stock_Vector_byStateSegAge_", scen, ".csv")))
  if (is.null(df)) return(NULL)
  df %>% mutate(Country = "United States", Scenario = scen)
})
if (!is.null(ice_stock_us) && nrow(ice_stock_us) > 0 && !is.null(ldv_evlib) && nrow(ldv_evlib) > 0) {
  ldv_evlib <- ldv_evlib %>%
    left_join(
      ice_stock_us %>%
        select(Country, Scenario, State, Segment, Year, Age, ICE_stock_vector),
      by = c("Country", "Scenario", "State", "Segment", "Year")
    )
  cat("  LDV_EVLIB(+ICE):  ", nrow(ldv_evlib), "rows\n")
} else {
  cat("  LDV_EVLIB(+ICE):  skipped (ICE stock source not found)\n")
}

ldv_cl <- map_dfr(SCENARIOS, function(scen) {
  bind_rows(
    read_tag(file.path(OUTPUT_DIR,           paste0("ClosedLoop_StateTotals_", scen, ".csv")), "United States", scen),
    read_tag(file.path(OUTPUT_DIR, "Canada", paste0("ClosedLoop_StateTotals_", scen, ".csv")), "Canada",        scen),
    read_tag(file.path(OUTPUT_DIR, "Mexico", paste0("ClosedLoop_StateTotals_", scen, ".csv")), "Mexico",        scen)
  )
})
cat("  LDV_ClosedLoop:   ", nrow(ldv_cl), "rows\n")

ldv_seg <- map_dfr(SCENARIOS, function(scen) {
  bind_rows(
    read_tag(file.path(OUTPUT_DIR,           paste0("ClosedLoop_AddRetire_byStateSegment_", scen, ".csv")), "United States", scen),
    read_tag(file.path(OUTPUT_DIR, "Canada", paste0("ClosedLoop_AddRetire_byStateSegment_", scen, ".csv")), "Canada",        scen)
  )
})
cat("  LDV_AddRetire:    ", nrow(ldv_seg), "rows\n")


## =====================================================================
## 2.  HDV
## =====================================================================
cat("=== Reading HDV table ===\n")

hdv_master <- map_dfr(SCENARIOS, function(scen) {
  f <- file.path(OUTPUT_DIR, "HDV", paste0("HDV_EV_Turnover_", scen, ".csv"))
  df <- safe_read(f)
  if (is.null(df)) return(NULL)
  if (!"Scenario" %in% names(df)) df$Scenario <- scen
  df
})
cat("  HDV:              ", nrow(hdv_master), "rows\n")


## =====================================================================
## 3.  BESS — separate tables for LDV / HDV × Retire / Stock
## =====================================================================
cat("=== Reading BESS tables ===\n")

bess_ldv_ret <- map_dfr(SCENARIOS, function(scen) {
  bind_rows(
    read_tag(file.path(OUTPUT_DIR,           paste0("BESS_Retire_Vector_byStateSegProp_", scen, ".csv")), "United States", scen),
    read_tag(file.path(OUTPUT_DIR, "Canada", paste0("BESS_Retire_Vector_byStateSegProp_", scen, ".csv")), "Canada",        scen),
    read_tag(file.path(OUTPUT_DIR, "Mexico", paste0("BESS_Retire_Vector_byStateSegProp_", scen, ".csv")), "Mexico",        scen)
  )
})
cat("  BESS_LDV_Retire:  ", nrow(bess_ldv_ret), "rows\n")

bess_ldv_stk <- map_dfr(SCENARIOS, function(scen) {
  bind_rows(
    read_tag(file.path(OUTPUT_DIR,           paste0("BESS_Stock_Vector_byStateSegProp_", scen, ".csv")), "United States", scen),
    read_tag(file.path(OUTPUT_DIR, "Canada", paste0("BESS_Stock_Vector_byStateSegProp_", scen, ".csv")), "Canada",        scen),
    read_tag(file.path(OUTPUT_DIR, "Mexico", paste0("BESS_Stock_Vector_byStateSegProp_", scen, ".csv")), "Mexico",        scen)
  )
})
cat("  BESS_LDV_Stock:   ", nrow(bess_ldv_stk), "rows\n")

bess_hdv_ret <- map_dfr(SCENARIOS, function(scen) {
  f <- file.path(OUTPUT_DIR, "HDV", paste0("HDV_BESS_Retire_", scen, ".csv"))
  df <- safe_read(f)
  if (is.null(df)) return(NULL)
  df %>% mutate(Scenario = scen)
})
cat("  BESS_HDV_Retire:  ", nrow(bess_hdv_ret), "rows\n")

bess_hdv_stk <- map_dfr(SCENARIOS, function(scen) {
  f <- file.path(OUTPUT_DIR, "HDV", paste0("HDV_BESS_Stock_", scen, ".csv"))
  df <- safe_read(f)
  if (is.null(df)) return(NULL)
  df %>% mutate(Scenario = scen)
})
cat("  BESS_HDV_Stock:   ", nrow(bess_hdv_stk), "rows\n")


## =====================================================================
## 4.  US NATIONAL — LDV annual additions / retirements by powertrain
## =====================================================================
## Aggregates ldv_cl (ClosedLoop_StateTotals) across the 50 US states (+DC)
## into a single national time series, then pivots to long format with
## one row per (Scenario, Year, Powertrain). Powertrain values: BEV, PHEV, ICE.
## Columns:
##   Adds        - new additions to the fleet that year
##   Retires     - vehicles leaving the road (end-of-life retirements)
##   Exports     - vehicles exported (subset of total outflow)
##   Net_Change  - Adds - Retires - Exports (approx. fleet stock change)
## =====================================================================
cat("=== Building US-national LDV add/retire by powertrain ===\n")

us_ldv_pt <- if (!is.null(ldv_cl) && nrow(ldv_cl) > 0) {
  ldv_cl %>%
    filter(Country == "United States") %>%
    group_by(Scenario, Year) %>%
    summarise(
      add_BEV  = sum(add_BEV,  na.rm = TRUE),
      add_PHEV = sum(add_PHEV, na.rm = TRUE),
      add_ICE  = sum(add_ICE,  na.rm = TRUE),
      ret_BEV  = sum(ret_BEV,  na.rm = TRUE),
      ret_PHEV = sum(ret_PHEV, na.rm = TRUE),
      ret_ICE  = sum(ret_ICE,  na.rm = TRUE),
      exp_BEV  = sum(exp_BEV,  na.rm = TRUE),
      exp_PHEV = sum(exp_PHEV, na.rm = TRUE),
      exp_ICE  = sum(exp_ICE,  na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    pivot_longer(
      cols          = -c(Scenario, Year),
      names_to      = c(".value", "Powertrain"),
      names_pattern = "^(add|ret|exp)_(.+)$"
    ) %>%
    rename(Adds = add, Retires = ret, Exports = exp) %>%
    mutate(
      Net_Change = Adds - Retires - Exports,
      Powertrain = factor(Powertrain, levels = c("BEV", "PHEV", "ICE"))
    ) %>%
    arrange(Scenario, Year, Powertrain) %>%
    mutate(Powertrain = as.character(Powertrain)) %>%
    select(Scenario, Year, Powertrain, Adds, Retires, Exports, Net_Change)
} else NULL

if (!is.null(us_ldv_pt)) {
  cat("  US_LDV_AddRet_byPowertrain:", nrow(us_ldv_pt), "rows  (",
      length(unique(us_ldv_pt$Year)), "years x",
      length(unique(us_ldv_pt$Scenario)), "scenarios x 3 powertrains )\n")
}


## =====================================================================
## 5.  Write a single Excel workbook (one sheet per master table)
## =====================================================================
cat("=== Writing single Excel workbook ===\n")

drop_if_empty <- function(lst) lst[!vapply(lst, function(x) is.null(x) || nrow(x) == 0, logical(1))]

sheets <- drop_if_empty(list(
  LDV_EVLIB                  = ldv_evlib,
  LDV_ClosedLoop             = ldv_cl,
  LDV_AddRetire              = ldv_seg,
  HDV                        = hdv_master,
  BESS_LDV_Retire            = bess_ldv_ret,
  BESS_LDV_Stock             = bess_ldv_stk,
  BESS_HDV_Retire            = bess_hdv_ret,
  BESS_HDV_Stock             = bess_hdv_stk,
  US_LDV_AddRet_byPowertrain = us_ldv_pt
))

out_path <- file.path(OUTPUT_DIR, "Fleet_Turnover_Master.xlsx")
write_xlsx(sheets, path = out_path)

out_path_ice <- file.path(OUTPUT_DIR, "Fleet_Turnover_Master_ICE.xlsx")
write_xlsx(sheets, path = out_path_ice)

cat("\n=== Done ===\n")
cat("Wrote", length(sheets), "sheets to:", out_path, "\n")
cat("Wrote", length(sheets), "sheets to:", out_path_ice, "\n")
for (nm in names(sheets)) {
  cat(sprintf("  %-18s %8d rows  x  %3d cols\n",
              nm, nrow(sheets[[nm]]), ncol(sheets[[nm]])))
}
