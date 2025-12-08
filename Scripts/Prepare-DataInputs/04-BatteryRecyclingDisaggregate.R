# Battery Recycling Disaggregation Model
# EV Fleet Model - Historical Breakdown
# Yunzhu - July 2025

# Load libraries -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load data -----
EVSurvivalResults <- read_csv("Parameters/EVSurvivalResults.csv")
EVSales <- read_csv("Parameters/EVSales.csv")
PowertrainShares <- read_csv("Parameters/PowertrainShares.csv")
EVRegistrationsExtended <- read_csv("Parameters/EVRegistrationsExtended.csv")
VehicleCategoryShare <- read_csv("Parameters/VehicleCategoryShare.csv")
AvgBatteryCapacity <- read_csv("Parameters/AvgBatteryCapacity.csv")
ChemistryShares <- read_csv("Parameters/ChemistryShares.csv")

# Step 1: Aggregate total recycling from survival results -----
results_hist_df <- EVSurvivalResults %>%
  rename(`Sale Year` = sale_year) %>%
  mutate(Total_Recycle = map2(recycle_from_ev_fail, recycle_from_battery_only,
                              ~ .x + .y) %>%
           map2(recycle_from_both_fail, ~ .x + .y))

# Step 2: Apply powertrain shares -----
results_pt <- left_join(EVSales, results_hist_df, by = "Sale Year")

for (pt in c("BEV", "PHEV", "FCEV")) {
  results_pt[[paste0("Recycle_", pt)]] <- mapply(function(share, recycle) {
    as.numeric(share) * as.numeric(recycle)
  }, results_pt[[paste0("Share ", pt)]], results_pt$Total_Recycle, SIMPLIFY = FALSE)
}

# Retain relevant columns
results_pt <- results_pt %>% 
  select(`Sale Year`, years, starts_with("Recycle_"))

# Step 3: Apply state-level registration shares -----
regs_df <- EVRegistrationsExtended %>% rename(`Sale Year` = `Reg Year`)
results_state <- left_join(regs_df, results_pt, by = "Sale Year")

for (pt in c("BEV", "PHEV", "FCEV")) {
  results_state[[paste0("Recycle_", pt)]] <- mapply(function(frac, val) {
    as.numeric(frac) * as.numeric(val)
  }, results_state[[paste0("Fraction_", pt)]], results_state[[paste0("Recycle_", pt)]], SIMPLIFY = FALSE)
}

results_state <- results_state %>% 
  select(State, `Sale Year`, years, starts_with("Recycle_"))

# Step 4: Apply vehicle category shares -----
results_type <- left_join(VehicleCategoryShare, results_state, by = "Sale Year")

for (pt in c("BEV", "PHEV", "FCEV")) {
  results_type[[paste0("Recycle_", pt)]] <- mapply(function(frac, val) {
    as.numeric(frac) * as.numeric(val)
  }, results_type[[pt]], results_type[[paste0("Recycle_", pt)]], SIMPLIFY = FALSE)
}

results_type <- results_type %>% 
  select(`Sale Year`, State, `Global Segment`, years, starts_with("Recycle_"))

# Step 5: Apply average battery capacity (kWh) -----
results_cap <- left_join(AvgBatteryCapacity, results_type, by = c("Sale Year", "Global Segment"))

for (pt in c("BEV", "PHEV", "FCEV")) {
  results_cap[[paste0("Recycle_", pt, "_kwh")]] <- mapply(function(cap, qty) {
    as.numeric(cap) * as.numeric(qty)
  }, results_cap[[pt]], results_cap[[paste0("Recycle_", pt)]], SIMPLIFY = FALSE)
}

results_cap <- results_cap %>% 
  select(`Sale Year`, State, `Global Segment`, years, ends_with("_kwh"))

# Step 6: Apply chemistry share -----
results_chem <- left_join(ChemistryShares, results_cap, by = c("Sale Year", "Global Segment"))

for (pt in c("BEV", "PHEV")) {
  results_chem[[paste0("Recycle_", pt, "_kwh")]] <- mapply(function(frac, qty) {
    as.numeric(frac) * as.numeric(qty)
  }, results_chem[[pt]], results_chem[[paste0("Recycle_", pt, "_kwh")]], SIMPLIFY = FALSE)
}

# Clean up and replace unspecified chemistries -----
chem_replacements <- c("NCA (unspecified)" = "NCA",
                       "LFP (unspecified)" = "LFP",
                       "LMO (unspecified)" = "LMO",
                       "LTO (unspecified)" = "LMO-LTO",
                       "NMC 111 + NCA" = "NMCA 89:4:4:3",
                       "NMC 811 + 111" = "NMC 811",
                       "70 % NMC 111 + 30 % NMC 622" = "NMC 111",
                       "NMC 422" = "NMC 532",
                       "NMC 111 + LMO" = "NMC 111",
                       "LMO+NMC+NCA" = "NMCA 89:4:4:3")

results_chem <- results_chem %>% 
  mutate(`Cathode Mix` = recode(`Cathode Mix`, !!!chem_replacements)) %>% 
  select(-FCEV, -`Recycle_FCEV_kwh`)

# Export final processed dataset -----
write_csv(results_chem, "Parameters/Recycled_LIB_By_Chemistry.csv")