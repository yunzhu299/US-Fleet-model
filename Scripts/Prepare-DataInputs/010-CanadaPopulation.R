## 010 — Canada Population Projection
## Process CanadaPop.xlsx and create population projection CSV
## YZC Dec 2025

library(readxl)
library(dplyr)
library(tidyr)

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

