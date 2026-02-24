## 023 — Export Projection based on Export Factor
## Calculate projected exports for 2020-2050 based on retirement × export_factor
## YZC Feb 2026

library(dplyr)
library(readr)
library(tidyr)

# -----------------------------
# 1) Load retirement data from simulation outputs
# -----------------------------
cat("=== Loading Retirement Data ===\n")

# Load ACCII scenario (can also do Repeal)
us_accii <- read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv", show_col_types = FALSE)

# Aggregate to national level
us_retire <- us_accii %>%
  group_by(Year) %>%
  summarise(
    Ret_ICE = sum(ret_ICE, na.rm = TRUE),
    Ret_BEV = sum(ret_BEV, na.rm = TRUE),
    Ret_PHEV = sum(ret_PHEV, na.rm = TRUE),
    Ret_EV = Ret_BEV + Ret_PHEV,
    Ret_Total = Ret_ICE + Ret_BEV + Ret_PHEV,
    .groups = "drop"
  )

cat("Retirement data loaded for years:", min(us_retire$Year), "-", max(us_retire$Year), "\n")

# -----------------------------
# 2) Get export factors
# -----------------------------
cat("\n=== Export Factors ===\n")

# Check if ratio_tbl exists (from 003-PopulationDemand.R)
if (exists("ratio_tbl")) {
  export_factors_historical <- ratio_tbl %>%
    select(Year, export_factor = ratio)
  cat("Using historical export factors from ratio_tbl\n")
} else {
  # Recreate from data
  cat("ratio_tbl not found, using default values\n")
  # These are approximate values - run 003 first for accurate values
  export_factors_historical <- tibble(
    Year = 2020:2024,
    export_factor = c(0.08, 0.09, 0.10, 0.09, 0.08)  # Approximate
  )
}

# Get average factor for 2025+
if (exists("shrink_ratio_avg")) {
  avg_export_factor <- shrink_ratio_avg
} else {
  avg_export_factor <- mean(export_factors_historical$export_factor, na.rm = TRUE)
}

cat("Average export factor for 2025+:", round(avg_export_factor, 4), "\n")

# Create export factor table for all years
export_factors <- tibble(Year = 2020:2050) %>%
  left_join(export_factors_historical, by = "Year") %>%
  mutate(
    export_factor = ifelse(is.na(export_factor), avg_export_factor, export_factor)
  )

# -----------------------------
# 3) Calculate projected exports
# -----------------------------
cat("\n=== Calculating Export Projections ===\n")

export_projection <- us_retire %>%
  left_join(export_factors, by = "Year") %>%
  mutate(
    # Export volumes = Retirement × Export Factor
    Export_ICE = round(Ret_ICE * export_factor),
    Export_BEV = round(Ret_BEV * export_factor),
    Export_PHEV = round(Ret_PHEV * export_factor),
    Export_EV = Export_BEV + Export_PHEV,
    Export_Total = Export_ICE + Export_EV,
    
    # Domestic volumes = Retirement × (1 - Export Factor)
    domestic_factor = 1 - export_factor,
    Domestic_ICE = round(Ret_ICE * domestic_factor),
    Domestic_BEV = round(Ret_BEV * domestic_factor),
    Domestic_PHEV = round(Ret_PHEV * domestic_factor),
    Domestic_EV = Domestic_BEV + Domestic_PHEV,
    Domestic_Total = Domestic_ICE + Domestic_EV
  ) %>%
  select(
    Year,
    export_factor,
    domestic_factor,
    # Retirements
    Ret_ICE, Ret_BEV, Ret_PHEV, Ret_EV, Ret_Total,
    # Exports
    Export_ICE, Export_BEV, Export_PHEV, Export_EV, Export_Total,
    # Domestic
    Domestic_ICE, Domestic_BEV, Domestic_PHEV, Domestic_EV, Domestic_Total
  )

# -----------------------------
# 4) Save output
# -----------------------------
write_csv(export_projection, "Outputs/US_Export_Projection_2020_2050.csv")
cat("\nSaved: Outputs/US_Export_Projection_2020_2050.csv\n")

# -----------------------------
# 5) Summary
# -----------------------------
cat("\n=== Export Projection Summary ===\n")

# Key years summary
summary_years <- export_projection %>%
  filter(Year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050))

print(summary_years %>% 
        select(Year, export_factor, Ret_Total, Export_Total, Export_ICE, Export_EV))

# Total exports over period
cat("\n=== Cumulative Exports (2020-2050) ===\n")
cumulative <- export_projection %>%
  summarise(
    Total_Export_ICE = sum(Export_ICE),
    Total_Export_BEV = sum(Export_BEV),
    Total_Export_PHEV = sum(Export_PHEV),
    Total_Export_EV = sum(Export_EV),
    Total_Export_All = sum(Export_Total)
  )
print(cumulative)

cat("\n=== Export Projection Complete! ===\n")

