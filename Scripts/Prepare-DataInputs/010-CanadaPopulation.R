## 010 — Canada Population Projection
## Process CanadaPop.xlsx and create population projection CSV
## YZC Dec 2025

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# -----------------------------
# 1) Read CanadaPop.xlsx
# -----------------------------
# Row 9 = region names, Row 15-39 = years 2024-2048 (or similar)
raw <- read_excel("Inputs/CanadaPop.xlsx", col_names = FALSE)

# Get region names from row 9 (index 9 in R)
regions <- as.character(raw[9, ])
regions <- regions[!is.na(regions)]

# Find column indices for each region (skip "Canada" and "Geography2")
region_cols <- which(!is.na(raw[9, ]) & raw[9, ] != "Canada" & raw[9, ] != "Geography2")
region_names <- as.character(raw[9, region_cols])

cat("Regions found:", paste(region_names, collapse = ", "), "\n")

# Get year data from rows 15-39
year_rows <- 15:39
years_raw <- as.numeric(raw[[1]][year_rows])  # First column has years

cat("Years found:", paste(years_raw, collapse = ", "), "\n")

# Extract population data for each region
pop_data <- data.frame(Year = years_raw)

for (i in seq_along(region_cols)) {
  col_idx <- region_cols[i]
  region_name <- region_names[i]
  # Get population values, multiply by 1000
  pop_values <- as.numeric(raw[[col_idx]][year_rows]) * 1000
  pop_data[[region_name]] <- pop_values
}

# -----------------------------
# 2) Linear extrapolation for 2050
# -----------------------------
# Get last two years to calculate slope
last_year <- max(pop_data$Year, na.rm = TRUE)
second_last_year <- last_year - 1

# Create 2050 row
pop_2050 <- data.frame(Year = 2050)

for (region in region_names) {
  y1 <- pop_data[[region]][pop_data$Year == second_last_year]
  y2 <- pop_data[[region]][pop_data$Year == last_year]
  slope <- y2 - y1  # annual change
  years_to_2050 <- 2050 - last_year
  pop_2050[[region]] <- y2 + slope * years_to_2050
}

# Append 2050 row
pop_data <- bind_rows(pop_data, pop_2050)

# -----------------------------
# 3) Convert to long format
# -----------------------------
canada_pop_long <- pop_data %>%
  pivot_longer(
    cols = -Year,
    names_to = "Province",
    values_to = "Population"
  ) %>%
  arrange(Province, Year)

cat("\nSummary of Canada Population Data:\n")
print(head(canada_pop_long, 20))
cat("\nTotal rows:", nrow(canada_pop_long), "\n")
cat("Provinces:", length(unique(canada_pop_long$Province)), "\n")
cat("Years:", min(canada_pop_long$Year), "-", max(canada_pop_long$Year), "\n")

# -----------------------------
# 4) Save to CSV
# -----------------------------
write.csv(canada_pop_long, "Parameters/CanadaPopulation.csv", row.names = FALSE)
cat("\nSaved to Parameters/CanadaPopulation.csv\n")

# Also save wide format for reference
write.csv(pop_data, "Parameters/CanadaPopulation_wide.csv", row.names = FALSE)
cat("Saved wide format to Parameters/CanadaPopulation_wide.csv\n")

# ============================================================
# PART B: Canada Vehicle Registrations
# ============================================================

# -----------------------------
# 5) Read CanadaRegs.xlsx - Sheet "Reg"
# -----------------------------
regs_raw <- read_excel("Inputs/CanadaRegs.xlsx", sheet = "Reg")

# Check column names
cat("\n=== Processing Canada Registrations ===\n")
cat("Columns:", paste(names(regs_raw), collapse = ", "), "\n")

# Rename columns for easier handling
# Expected: Geography, Fuel Type, Year, TotalLDV, CAR, MPV, PUT, Vans, Other
names(regs_raw) <- c("Province", "FuelType", "Year", "TotalLDV", "CAR", "MPV", "PUT", "Vans", "Other")

# Clean province names (remove trailing spaces, etc.)
regs_raw <- regs_raw %>%
  mutate(Province = trimws(Province),
         FuelType = trimws(FuelType))

# Remove header rows that appear in the middle of data
regs_raw <- regs_raw %>%
  filter(Province != "Geography" | is.na(Province))

# Fill down Province AND FuelType (they only appear on first row of each group)
regs_raw <- regs_raw %>%
  mutate(Province = ifelse(Province == "" | is.na(Province), NA, Province),
         FuelType = ifelse(FuelType == "" | is.na(FuelType), NA, FuelType)) %>%
  fill(Province, .direction = "down") %>%
  fill(FuelType, .direction = "down")

cat("Provinces found:", paste(unique(regs_raw$Province), collapse = ", "), "\n")
cat("Fuel types found:", paste(unique(regs_raw$FuelType), collapse = ", "), "\n")
cat("Years found:", paste(sort(unique(regs_raw$Year)), collapse = ", "), "\n")

# -----------------------------
# 6) Calculate Car and Truck (MPV + PUT + Vans + Other)
# -----------------------------
regs_clean <- regs_raw %>%
  mutate(
    Car = as.numeric(CAR),
    Truck = as.numeric(MPV) + as.numeric(PUT) + as.numeric(Vans) + as.numeric(Other),
    TotalLDV = as.numeric(TotalLDV),
    # Clean province names (remove trailing numbers like "Ontario 14")
    Province = str_trim(str_replace(Province, "\\s+\\d+.*$", ""))
  ) %>%
  select(Province, FuelType, Year, TotalLDV, Car, Truck)

# -----------------------------
# 7) 2020 Total Registrations by Province (all fuel types)
# -----------------------------
regs_2020_total <- regs_clean %>%
  filter(Year == 2020) %>%
  group_by(Province) %>%
  summarise(
    Total_LDV = sum(TotalLDV, na.rm = TRUE),
    Total_Car = sum(Car, na.rm = TRUE),
    Total_Truck = sum(Truck, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== 2020 Total Registrations by Province ===\n")
print(regs_2020_total)

# -----------------------------
# 8) 2020 Registrations by Province and Powertrain
# -----------------------------
regs_2020_by_pt <- regs_clean %>%
  filter(Year == 2020) %>%
  mutate(
    Powertrain = case_when(
      FuelType %in% c("Gasoline", "Hybrid electric") ~ "ICE",
      FuelType == "Battery electric" ~ "BEV",
      FuelType == "Plug-in hybrid electric" ~ "PHEV",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(Province, Powertrain) %>%
  summarise(
    Total = sum(TotalLDV, na.rm = TRUE),
    Car = sum(Car, na.rm = TRUE),
    Truck = sum(Truck, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== 2020 Registrations by Province and Powertrain (Ontario example) ===\n")
print(regs_2020_by_pt %>% filter(Province == "Ontario"))

# -----------------------------
# 9) Type Share (Car vs Truck) by Province - 2020
# -----------------------------
canada_type_share_2020 <- regs_2020_total %>%
  mutate(
    type_share_Car = Total_Car / Total_LDV,
    type_share_Truck = Total_Truck / Total_LDV
  ) %>%
  select(Province, Total_Car, Total_Truck, type_share_Car, type_share_Truck)

cat("\n=== Type Share (Car/Truck) by Province - 2020 ===\n")
print(canada_type_share_2020)

# -----------------------------
# 10) ICE Pool (Gasoline + Hybrid) for 2020 - needed for initialization
# -----------------------------
canada_ice_pool_2020 <- regs_2020_by_pt %>%
  filter(Powertrain == "ICE") %>%
  select(Province, ICE_Total = Total, ICE_Car = Car, ICE_Truck = Truck)

cat("\n=== ICE Pool by Province - 2020 ===\n")
print(canada_ice_pool_2020)

# -----------------------------
# 11) Save Registration outputs
# -----------------------------
# Total registrations 2020
write.csv(regs_2020_total, "Parameters/Canada_Registrations_2020.csv", row.names = FALSE)
cat("\nSaved: Parameters/Canada_Registrations_2020.csv\n")

# Registrations by powertrain 2020
regs_2020_by_pt_wide <- regs_2020_by_pt %>%
  pivot_wider(
    names_from = Powertrain,
    values_from = c(Total, Car, Truck),
    values_fill = 0
  )
write.csv(regs_2020_by_pt_wide, "Parameters/Canada_Registrations_2020_byPT.csv", row.names = FALSE)
cat("Saved: Parameters/Canada_Registrations_2020_byPT.csv\n")

# Type share 2020
write.csv(canada_type_share_2020, "Parameters/Canada_TypeShare_2020.csv", row.names = FALSE)
cat("Saved: Parameters/Canada_TypeShare_2020.csv\n")

# ICE pool 2020
write.csv(canada_ice_pool_2020, "Parameters/Canada_ICE_Pool_2020.csv", row.names = FALSE)
cat("Saved: Parameters/Canada_ICE_Pool_2020.csv\n")

# Full cleaned data (all years, all fuel types)
write.csv(regs_clean, "Parameters/Canada_Registrations_AllYears.csv", row.names = FALSE)
cat("Saved: Parameters/Canada_Registrations_AllYears.csv\n")

cat("\n=== Part B Done! ===\n")

# ============================================================
# PART C: Canada EV Historical Sales (allocated from national)
# ============================================================

cat("\n=== Processing Canada EV Historical Sales ===\n")

# -----------------------------
# 12) Read BatteryInstallation data for Canada
# -----------------------------
battery_raw <- read_csv("Inputs/BatteryInstallation-Tracker-January_2025_Data.csv", 
                        show_col_types = FALSE)

# Filter for Canada only
canada_ev_raw <- battery_raw %>%
  filter(`Sales Country` == "Canada")

cat("Canada EV records found:", nrow(canada_ev_raw), "\n")

# -----------------------------
# 13) Aggregate monthly registrations to annual
# -----------------------------
reg_cols <- grep("^Reg_\\d{4}", names(canada_ev_raw), value = TRUE)

# Extract year from column names and sum by year
reg_year_groups <- split(reg_cols, str_extract(reg_cols, "\\d{4}"))

for (yr in names(reg_year_groups)) {
  cols_this_year <- reg_year_groups[[yr]]
  canada_ev_raw[[paste0("Total_Reg_", yr)]] <- rowSums(
    canada_ev_raw[, cols_this_year, drop = FALSE] %>% 
      mutate(across(everything(), ~as.numeric(.))), 
    na.rm = TRUE
  )
}

# Similarly for MWh columns
mwh_cols <- grep("^Mwh_\\d{4}", names(canada_ev_raw), value = TRUE)
mwh_year_groups <- split(mwh_cols, str_extract(mwh_cols, "\\d{4}"))

for (yr in names(mwh_year_groups)) {
  cols_this_year <- mwh_year_groups[[yr]]
  canada_ev_raw[[paste0("Total_Mwh_", yr)]] <- rowSums(
    canada_ev_raw[, cols_this_year, drop = FALSE] %>% 
      mutate(across(everything(), ~as.numeric(.))), 
    na.rm = TRUE
  )
}

# -----------------------------
# 14) Reshape to long format
# -----------------------------
canada_ev_raw$id <- 1:nrow(canada_ev_raw)
canada_ev_long <- canada_ev_raw %>%
  select(id, `Global Segment`, Propulsion, `Battery kWh`, 
         starts_with("Total_Reg_"), starts_with("Total_Mwh_")) %>%
  pivot_longer(
    cols = starts_with("Total_"),
    names_to = c(".value", "Sale_Year"),
    names_pattern = "Total_(.*)_(\\d{4})"
  ) %>%
  rename(Sales = Reg, Mwh = Mwh) %>%
  mutate(Sale_Year = as.integer(Sale_Year))

# -----------------------------
# 15) Normalize segment to Car/SUV
# -----------------------------
canada_ev_long <- canada_ev_long %>%
  mutate(Segment = case_when(
    str_starts(`Global Segment`, "Car") ~ "Car",
    str_starts(`Global Segment`, "SUV") ~ "SUV",
    str_starts(`Global Segment`, "MPV") ~ "SUV",
    str_starts(`Global Segment`, "SS")  ~ "SUV",
    str_starts(`Global Segment`, "LCV") ~ "SUV",
    str_starts(`Global Segment`, "PUP") ~ "SUV",
    TRUE ~ "SUV"  # default to SUV for other types
  ))

# -----------------------------
# 16) National Canada EV sales by Year × Segment × Propulsion
# -----------------------------
canada_nat_ev_sales <- canada_ev_long %>%
  filter(Propulsion %in% c("BEV", "PHEV"), Sale_Year %in% 2014:2024) %>%
  group_by(Sale_Year, Segment, Propulsion) %>%
  summarise(
    National_Sales = sum(Sales, na.rm = TRUE),
    National_Mwh = sum(Mwh, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nNational Canada EV Sales summary:\n")
print(canada_nat_ev_sales %>% filter(Sale_Year >= 2020))

# -----------------------------
# 17) Calculate province EV share from CanadaRegs
# -----------------------------
# Get EV registrations by province and year
province_ev_regs <- regs_clean %>%
  filter(FuelType %in% c("Battery electric", "Plug-in hybrid electric")) %>%
  mutate(Propulsion = case_when(
    FuelType == "Battery electric" ~ "BEV",
    FuelType == "Plug-in hybrid electric" ~ "PHEV"
  )) %>%
  group_by(Province, Year, Propulsion) %>%
  summarise(
    EV_Regs = sum(TotalLDV, na.rm = TRUE),
    Car_Regs = sum(Car, na.rm = TRUE),
    Truck_Regs = sum(Truck, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate province share of national EV registrations by year and propulsion
province_share <- province_ev_regs %>%
  group_by(Year, Propulsion) %>%
  mutate(
    National_EV = sum(EV_Regs, na.rm = TRUE),
    share = if_else(National_EV > 0, EV_Regs / National_EV, 0)
  ) %>%
  ungroup() %>%
  select(Province, Year, Propulsion, share)

cat("\nProvince EV shares (2020 BEV example):\n")
print(province_share %>% filter(Year == 2020, Propulsion == "BEV"))

# -----------------------------
# 18) Extend shares to years outside registration data
# -----------------------------
# Ensure Year is numeric
province_share <- province_share %>%
  mutate(Year = as.integer(Year))

# Use earliest available year for earlier years, latest for later years
min_reg_year <- min(province_share$Year, na.rm = TRUE)
max_reg_year <- max(province_share$Year, na.rm = TRUE)

cat("Registration year range:", min_reg_year, "-", max_reg_year, "\n")

# For years before min_reg_year, use min_reg_year shares
share_early <- province_share %>%
  filter(Year == min_reg_year) %>%
  select(Province, Propulsion, share) %>%
  crossing(Year = 2014:(min_reg_year - 1))

# For years after max_reg_year, use max_reg_year shares  
share_late <- province_share %>%
  filter(Year == max_reg_year) %>%
  select(Province, Propulsion, share) %>%
  crossing(Year = (max_reg_year + 1):2024)

province_share_extended <- bind_rows(
  province_share,
  share_early,
  share_late
) %>%
  filter(Year %in% 2014:2024) %>%
  distinct(Province, Year, Propulsion, .keep_all = TRUE)

# -----------------------------
# 19) Allocate national sales to provinces
# -----------------------------
# Also need segment-level allocation - use Car/Truck ratio from regs
segment_ratio <- regs_clean %>%
  mutate(Year = as.integer(Year)) %>%
  filter(FuelType %in% c("Battery electric", "Plug-in hybrid electric")) %>%
  mutate(Propulsion = case_when(
    FuelType == "Battery electric" ~ "BEV",
    FuelType == "Plug-in hybrid electric" ~ "PHEV"
  )) %>%
  group_by(Province, Year, Propulsion) %>%
  summarise(
    Car = sum(Car, na.rm = TRUE),
    Truck = sum(Truck, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Total = Car + Truck,
    Car_ratio = if_else(Total > 0, Car / Total, 0.5),
    Truck_ratio = if_else(Total > 0, Truck / Total, 0.5)
  ) %>%
  select(Province, Year, Propulsion, Car_ratio, Truck_ratio)

# Extend segment ratios
seg_ratio_early <- segment_ratio %>%
  filter(Year == min_reg_year) %>%
  select(Province, Propulsion, Car_ratio, Truck_ratio) %>%
  crossing(Year = 2014:(min_reg_year - 1))

seg_ratio_late <- segment_ratio %>%
  filter(Year == max_reg_year) %>%
  select(Province, Propulsion, Car_ratio, Truck_ratio) %>%
  crossing(Year = (max_reg_year + 1):2024)

segment_ratio_extended <- bind_rows(
  segment_ratio,
  seg_ratio_early,
  seg_ratio_late
) %>%
  filter(Year %in% 2014:2024) %>%
  distinct(Province, Year, Propulsion, .keep_all = TRUE)

# Allocate national sales to provinces
canada_ev_historical <- province_share_extended %>%
  inner_join(
    canada_nat_ev_sales %>% 
      group_by(Sale_Year, Propulsion) %>%
      summarise(National_Sales = sum(National_Sales), .groups = "drop"),
    by = c("Year" = "Sale_Year", "Propulsion")
  ) %>%
  mutate(Province_Sales = share * National_Sales) %>%
  inner_join(segment_ratio_extended, by = c("Province", "Year", "Propulsion")) %>%
  mutate(
    Car_Sales = Province_Sales * Car_ratio,
    SUV_Sales = Province_Sales * Truck_ratio
  ) %>%
  select(Province, Year, Propulsion, Province_Sales, Car_Sales, SUV_Sales)

# Reshape to match US EV_historical format: State, Sale Year, Vehicle, Propulsion, Sales
canada_ev_historical_long <- canada_ev_historical %>%
  pivot_longer(
    cols = c(Car_Sales, SUV_Sales),
    names_to = "Vehicle",
    values_to = "Sales"
  ) %>%
  mutate(Vehicle = str_replace(Vehicle, "_Sales", "")) %>%
  rename(`Sale Year` = Year, State = Province) %>%
  select(State, `Sale Year`, Vehicle, Propulsion, Sales) %>%
  arrange(State, `Sale Year`, Vehicle, Propulsion)

cat("\nCanada EV Historical Sales (sample):\n")
print(canada_ev_historical_long %>% filter(State == "Ontario", `Sale Year` >= 2020))

# -----------------------------
# 20) Save Canada EV Historical Sales
# -----------------------------
write.csv(canada_ev_historical_long, "Parameters/Canada_EV_Historical.csv", row.names = FALSE)
cat("\nSaved: Parameters/Canada_EV_Historical.csv\n")

# Also save summary by province and year
canada_ev_summary <- canada_ev_historical %>%
  group_by(Province, Year) %>%
  summarise(
    BEV = sum(Province_Sales[Propulsion == "BEV"], na.rm = TRUE),
    PHEV = sum(Province_Sales[Propulsion == "PHEV"], na.rm = TRUE),
    Total_EV = sum(Province_Sales, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(canada_ev_summary, "Parameters/Canada_EV_Historical_Summary.csv", row.names = FALSE)
cat("Saved: Parameters/Canada_EV_Historical_Summary.csv\n")

cat("\n=== Part C Done! ===\n")

# ============================================================
# PART D: Canada Penetration Rates (use California's)
# ============================================================

cat("\n=== Processing Canada Penetration Rates ===\n")

# -----------------------------
# 21) Read California penetration rates from PR_ACCII
# -----------------------------
PR_ACCII <- read_csv("~/Downloads/PR_ACCII.csv", show_col_types = FALSE)
PR_Repeal <- read_csv("~/Downloads/PR_Repeal.csv", show_col_types = FALSE)

# Get California's rates
ca_pr_accii <- PR_ACCII %>%
  filter(State == "California") %>%
  select(Year, Propulsion, Fraction)

ca_pr_repeal <- PR_Repeal %>%
  mutate(State = trimws(State)) %>%
  filter(State == "California") %>%
  select(Year, Propulsion, Fraction)

cat("California PR years (ACCII):", min(ca_pr_accii$Year), "-", max(ca_pr_accii$Year), "\n")

# -----------------------------
# 22) Apply California's rates to all Canadian provinces
# -----------------------------
canada_provinces <- unique(canada_pop_long$Province)

# ACCII scenario
canada_pr_accii <- ca_pr_accii %>%
  crossing(State = canada_provinces) %>%
  select(State, Year, Propulsion, Fraction) %>%
  arrange(State, Year, Propulsion)

# Repeal scenario
canada_pr_repeal <- ca_pr_repeal %>%
  crossing(State = canada_provinces) %>%
  select(State, Year, Propulsion, Fraction) %>%
  arrange(State, Year, Propulsion)

cat("\nCanada PR ACCII (sample - Ontario):\n")
print(canada_pr_accii %>% filter(State == "Ontario", Year >= 2025) %>% head(10))

# -----------------------------
# 23) Save Canada penetration rates
# -----------------------------
write.csv(canada_pr_accii, "Parameters/Canada_PR_ACCII.csv", row.names = FALSE)
cat("\nSaved: Parameters/Canada_PR_ACCII.csv\n")

write.csv(canada_pr_repeal, "Parameters/Canada_PR_Repeal.csv", row.names = FALSE)
cat("Saved: Parameters/Canada_PR_Repeal.csv\n")

cat("\n=== Part D Done! ===\n")

# ============================================================
# PART E: Canada Age Distribution (use US average)
# ============================================================

cat("\n=== Processing Canada Age Distribution ===\n")

# -----------------------------
# 24) Get US average age distribution from 001-ICEAgeDistribution
# -----------------------------
# This should already be in memory as us_avg_by_year_age_by_type
# If not, we'll create it from state_age_long_filled_by_type

if (!exists("us_avg_by_year_age_by_type")) {
  cat("Note: us_avg_by_year_age_by_type not found. Please run 001-ICEAgeDistribution.R first.\n")
  cat("Skipping age distribution for now.\n")
} else {
  # Apply US average to all Canadian provinces
  canada_age_dist <- us_avg_by_year_age_by_type %>%
    filter(yearID == 2020) %>%  # Use 2020 distribution
    crossing(Province = canada_provinces) %>%
    mutate(
      Segment = case_when(
        Type == "Car" ~ "Car",
        Type == "Truck" ~ "SUV",
        TRUE ~ Type
      )
    ) %>%
    select(Province, Segment, ageID, ageFraction = ageFraction_state_type) %>%
    arrange(Province, Segment, ageID)
  
  cat("\nCanada Age Distribution (sample - Ontario Car):\n")
  print(canada_age_dist %>% filter(Province == "Ontario", Segment == "Car") %>% head(10))
  
  # Save
  write.csv(canada_age_dist, "Parameters/Canada_AgeDistribution.csv", row.names = FALSE)
  cat("\nSaved: Parameters/Canada_AgeDistribution.csv\n")
}

cat("\n=== Part E Done! ===\n")

# ============================================================
# PART F: Canada Total Vehicle Sales (from CanadaSale.xlsx)
# ============================================================

cat("\n=== Processing Canada Total Vehicle Sales ===\n")

# -----------------------------
# 25) Read CanadaSale.xlsx
# -----------------------------
# Structure: Row 1 = years, Row 2 = quarters (1,2,3,4), Data starts row 3
# Columns: Vehicle Type | Fuel Type | quarterly sales...

if (file.exists("Inputs/CanadaSale.xlsx")) {
  
  # Read raw without headers
  sale_raw <- read_excel("Inputs/CanadaSale.xlsx", col_names = FALSE)
  
  # Row 1 has years (2017, 2018, etc.) - need to extract and repeat for quarters
  year_row <- as.character(sale_raw[1, ])
  quarter_row <- as.character(sale_raw[2, ])
  
  # Build year-quarter mapping for data columns (starting from column 3)
  # Fill forward the year values
  years <- c()
  current_year <- "UNKNOWN"
  for (i in 3:length(year_row)) {
    val <- year_row[i]
    if (!is.na(val) && val != "" && val != "NA") {
      current_year <- val
    }
    years <- c(years, current_year)
  }
  
  # Get quarters (1, 2, 3, 4)
  quarters <- quarter_row[3:length(quarter_row)]
  
  # Build column names
  col_names <- paste0("Y", years, "_Q", quarters)
  
  cat("Sample column names:", head(col_names, 8), "\n")
  
  # Data starts from row 3
  sale_data <- sale_raw[3:nrow(sale_raw), ]
  names(sale_data) <- c("VehicleType", "FuelType", col_names)
  
  # Fill down VehicleType
  sale_data <- sale_data %>%
    mutate(VehicleType = ifelse(VehicleType == "" | is.na(VehicleType), NA, VehicleType)) %>%
    fill(VehicleType, .direction = "down")
  
  # Remove header rows that might appear in data
  sale_data <- sale_data %>%
    filter(!is.na(FuelType), FuelType != "")
  
  # Convert all Y* columns to numeric first
  sale_data <- sale_data %>%
    mutate(across(starts_with("Y"), ~as.numeric(as.character(.))))
  
  # Convert to long format
  sale_long <- sale_data %>%
    pivot_longer(
      cols = starts_with("Y"),
      names_to = "Year_Quarter",
      values_to = "Sales"
    ) %>%
    mutate(
      Year = as.integer(str_extract(Year_Quarter, "\\d{4}")),
      Quarter = str_extract(Year_Quarter, "Q\\d"),
      Sales = as.numeric(Sales)
    ) %>%
    filter(!is.na(Year), !is.na(Sales))
  
  # Aggregate quarters to annual
  canada_total_sales <- sale_long %>%
    group_by(VehicleType, FuelType, Year) %>%
    summarise(Annual_Sales = sum(Sales, na.rm = TRUE), .groups = "drop")
  
  cat("\nCanada Total Sales by Vehicle Type and Fuel Type:\n")
  print(canada_total_sales %>% filter(Year == 2020) %>% head(20))
  
  # -----------------------------
  # 26) Summarize to get total LDV sales by year
  # -----------------------------
  # Map vehicle types to Car/SUV
  # Passenger car = Car
  # Multi-purpose, Pickup trucks, Vans = SUV (Truck)
  
  canada_sales_by_segment <- canada_total_sales %>%
    mutate(
      Segment = case_when(
        str_detect(VehicleType, regex("passenger car", ignore_case = TRUE)) ~ "Car",
        str_detect(VehicleType, regex("multi-purpose|pickup|vans", ignore_case = TRUE)) ~ "SUV",
        str_detect(VehicleType, regex("total", ignore_case = TRUE)) ~ "Total",
        TRUE ~ "Other"
      )
    ) %>%
    filter(Segment %in% c("Car", "SUV"))
  
  # Total sales by year and segment (all fuel types)
  canada_total_by_year_seg <- canada_sales_by_segment %>%
    group_by(Year, Segment) %>%
    summarise(Total_Sales = sum(Annual_Sales, na.rm = TRUE), .groups = "drop")
  
  cat("\nCanada Total Sales by Year and Segment:\n")
  print(canada_total_by_year_seg)
  
  # Save
  write.csv(canada_total_sales, "Parameters/Canada_TotalSales_Detail.csv", row.names = FALSE)
  cat("\nSaved: Parameters/Canada_TotalSales_Detail.csv\n")
  
  write.csv(canada_total_by_year_seg, "Parameters/Canada_TotalSales_bySegment.csv", row.names = FALSE)
  cat("Saved: Parameters/Canada_TotalSales_bySegment.csv\n")
  
} else {
  cat("Note: CanadaSale.xlsx not found. Skipping total sales processing.\n")
}

cat("\n=== Part F Done! ===\n")

cat("\n========================================\n")
cat("=== All Canada Data Processing Done! ===\n")
cat("========================================\n")
cat("\nOutput files in Parameters/ folder:\n")
cat("  - CanadaPopulation.csv\n")
cat("  - CanadaPopulation_wide.csv\n")
cat("  - Canada_Registrations_2020.csv\n")
cat("  - Canada_Registrations_2020_byPT.csv\n")
cat("  - Canada_TypeShare_2020.csv\n")
cat("  - Canada_ICE_Pool_2020.csv\n")
cat("  - Canada_Registrations_AllYears.csv\n")
cat("  - Canada_EV_Historical.csv\n")
cat("  - Canada_EV_Historical_Summary.csv\n")
cat("  - Canada_PR_ACCII.csv\n")
cat("  - Canada_PR_Repeal.csv\n")
cat("  - Canada_AgeDistribution.csv (if 001 was run)\n")
