## 024 — Mexico Fleet Turnover Model
## Similar to US/Canada with EV battery tracking
## YZC Feb 2026

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)

# -----------------------------
# 0) Global parameters (same as US)
# -----------------------------
GLOBAL_ev_age_newLib         <- 8
GLOBAL_max_reuse_lib_share   <- 0.5
GLOBAL_max_ev_age            <- 12
GLOBAL_max_lib_age_ev        <- 12
GLOBAL_max_lib_age_repurpose <- 12

GLOBAL_reuse_share_evfail      <- 0.50
GLOBAL_repurpose_share_evfail  <- 0.25
GLOBAL_recycle_share_evfail    <- 0.25
GLOBAL_repurpose_share_libfail <- 0.50
GLOBAL_recycle_share_libfail   <- 0.50

# Lifetime params (same as US, SUV battery mean = 15)
life_param <- tibble(
  Vehicle  = c("Car", "SUV"),
  mean_ev  = c(17, 17),
  sd_ev    = c(4, 4),
  mean_lib = c(15, 15),  # Both Car and SUV use mean_lib = 15
  sd_lib   = c(4, 4)
)

# -----------------------------
# 1) Read Mexico EV Sales (2016-2025)
# -----------------------------
cat("=== Reading Mexico Sales Data ===\n")

# Check for file
mx_sales_path <- "Inputs/MX_Sales.xlsx"
if (!file.exists(mx_sales_path)) {
  mx_sales_path <- list.files("Inputs", pattern = "MX_Sales", full.names = TRUE)[1]
}

mx_sales_raw <- read_excel(mx_sales_path, sheet = "Annual_National_Total")
cat("MX Sales columns:", paste(names(mx_sales_raw), collapse = ", "), "\n")

# Extract sales by powertrain
mx_sales <- mx_sales_raw %>%
  transmute(
    Year = as.integer(Year),
    BEV = as.numeric(BEV_units),
    PHEV = as.numeric(PHEV_units),
    HEV = as.numeric(HEV_units),
    ICE = as.numeric(ICE_units)
  ) %>%
  filter(!is.na(Year))

cat("Mexico sales data loaded for years:", min(mx_sales$Year), "-", max(mx_sales$Year), "\n")
print(mx_sales)

# -----------------------------
# 2) Read Mexico ICE Age Distribution (2022)
# -----------------------------
cat("\n=== Reading Mexico ICE Age Distribution ===\n")

mx_data_path <- "Inputs/Mexico data.xlsx"
mx_fleet_age <- read_excel(mx_data_path, sheet = "Fleet Age", col_names = FALSE)

# Data structure:
# Row 1: Title
# Row 2: Headers (AGE, New ICE, New EV)
# Row 3-33: Data for ages 0-30
# Column 2 = New ICE (million vehicles)

# Read ICE stock by age (column 2, rows 3-33 = ages 0-30)
ice_age_values <- mx_fleet_age[3:33, 2] %>%
  pull(1) %>%
  as.numeric() * 1e6  # million -> vehicles

# Read EV stock by age (column 3, rows 3-33 = ages 0-30)
ev_age_values <- mx_fleet_age[3:33, 3] %>%
  pull(1) %>%
  as.numeric() * 1e6  # million -> vehicles

# Replace NA with 0
ice_age_values[is.na(ice_age_values)] <- 0
ev_age_values[is.na(ev_age_values)] <- 0

ice_age_dist_raw <- ice_age_values
ev_age_dist_raw <- ev_age_values

names(ice_age_dist_raw) <- as.character(0:30)
names(ev_age_dist_raw) <- as.character(0:30)

cat("ICE age distribution (2022):\n")
cat("  Total ICE vehicles:", format(sum(ice_age_dist_raw, na.rm = TRUE), big.mark = ","), "\n")
cat("  Ages 0-5:", format(sum(ice_age_dist_raw[1:6], na.rm = TRUE), big.mark = ","), "\n")
cat("  Ages 6-10:", format(sum(ice_age_dist_raw[7:11], na.rm = TRUE), big.mark = ","), "\n")
cat("  First 5 values (ages 0-4):", paste(format(round(ice_age_dist_raw[1:5]), big.mark = ","), collapse = ", "), "\n")

cat("\nEV age distribution (2022):\n")
cat("  Total EV vehicles:", format(sum(ev_age_dist_raw, na.rm = TRUE), big.mark = ","), "\n")
cat("  First 5 values (ages 0-4):", paste(format(round(ev_age_dist_raw[1:5]), big.mark = ","), collapse = ", "), "\n")

# -----------------------------
# 3) Read US Export Projection (Mexico imports from US)
# -----------------------------
cat("\n=== Reading US Export Projection ===\n")

# Use the export projection file from 023-ExportProjection.R
us_export_file <- "Outputs/US_Export_Projection_2020_2050.csv"

if (file.exists(us_export_file)) {
  us_export_proj <- read_csv(us_export_file, show_col_types = FALSE)
  cat("Using US_Export_Projection_2020_2050.csv\n")
  
  # Mexico receives ~70% of US used vehicle exports (largest destination)
  # Adjust this ratio based on actual data if available
  mexico_share_of_exports <- 0.70
  
  us_to_mex_extended <- us_export_proj %>%
    transmute(
      Year = Year,
      Import_from_US_Total = round(Export_Total * mexico_share_of_exports),
      Import_ICE = round(Export_ICE * mexico_share_of_exports),
      Import_BEV = round(Export_BEV * mexico_share_of_exports),
      Import_PHEV = round(Export_PHEV * mexico_share_of_exports),
      Import_EV = Import_BEV + Import_PHEV
    )
  
  cat("Mexico share of US exports:", mexico_share_of_exports * 100, "%\n")
  cat("Sample import projection:\n")
  print(us_to_mex_extended %>% filter(Year %in% c(2025, 2030, 2040, 2050)))
  
} else {
  cat("US_Export_Projection not found. Using historical data...\n")
  
  # Fallback: read from original export file
  export_df <- read_excel("Inputs/Used PV Countries Export Volume.xlsx")
  us_to_mex <- export_df %>%
    filter(Partner == "Mexico") %>%
    pivot_longer(cols = -Partner, names_to = "Year", values_to = "Import_from_US") %>%
    mutate(Year = as.integer(Year)) %>%
    select(Year, Import_from_US)
  
  avg_import <- mean(us_to_mex$Import_from_US, na.rm = TRUE)
  
  us_to_mex_extended <- tibble(Year = 2016:2050) %>%
    left_join(us_to_mex, by = "Year") %>%
    mutate(
      Import_from_US_Total = coalesce(Import_from_US, avg_import),
      Import_ICE = Import_from_US_Total * 0.95,
      Import_EV = Import_from_US_Total * 0.05,
      Import_BEV = Import_EV * 0.75,
      Import_PHEV = Import_EV * 0.25
    )
}

avg_import <- mean(us_to_mex_extended$Import_from_US_Total, na.rm = TRUE)
cat("Average annual import from US:", round(avg_import), "vehicles\n")

# -----------------------------
# 4) Population and Growth
# -----------------------------
cat("\n=== Population and Growth ===\n")

pop_2025 <- 131025338.4
pop_2050 <- 149333386.4
regs_per_cap_2025 <- 0.304597672
regs_per_cap_2050 <- 0.440283372

# Linear interpolation for population
years_sim <- 2020:2050
pop_by_year <- tibble(Year = years_sim) %>%
  mutate(
    Population = pop_2025 + (pop_2050 - pop_2025) * (Year - 2025) / (2050 - 2025),
    Regs_per_cap = regs_per_cap_2025 + (regs_per_cap_2050 - regs_per_cap_2025) * (Year - 2025) / (2050 - 2025),
    Target_Fleet = Population * Regs_per_cap
  )

# Target fleet in 2025
target_2025 <- pop_2025 * regs_per_cap_2025
cat("Target fleet 2025:", round(target_2025), "\n")
cat("Target fleet 2050:", round(pop_2050 * regs_per_cap_2050), "\n")

# -----------------------------
# 5) Penetration Rate Scenario
# -----------------------------
cat("\n=== Penetration Rate Scenario ===\n")

pr_milestones <- tibble(
  Year = c(2025, 2030, 2035, 2040, 2045, 2050),
  EV_PR = c(0.10, 0.30, 0.65, 1.00, 1.00, 1.00)
)

# Interpolate for all years
pr_all <- tibble(Year = 2020:2050) %>%
  left_join(pr_milestones, by = "Year") %>%
  arrange(Year) %>%
  mutate(EV_PR = approx(Year[!is.na(EV_PR)], EV_PR[!is.na(EV_PR)], Year, rule = 2)$y) %>%
  mutate(
    # Split EV into BEV/PHEV: 75% BEV, 25% PHEV
    BEV_PR = EV_PR * 0.75,
    PHEV_PR = EV_PR * 0.25,
    ICE_PR = 1 - EV_PR
  )

print(pr_all %>% filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050)))

# -----------------------------
# 6) Car/SUV Split (use US ratio)
# -----------------------------
cat("\n=== Car/SUV Split ===\n")

# Use US 2020 ratio as proxy for Mexico
# Typical US split: ~45% Car, ~55% SUV
car_share <- 0.45
suv_share <- 0.55
cat("Car share:", car_share, ", SUV share:", suv_share, "\n")

# -----------------------------
# 7) Survival Functions (same as US)
# -----------------------------
make_surv_tbl <- function(mu, b) {
  ages <- 0:49
  S <- 1 / (1 + exp((ages - mu) / b))
  y <- c(S[-1] / S[-length(S)], 0)
  tibble(ageID = ages, S = S, y = y)
}

surv_tbl_ice <- list(
  Car = make_surv_tbl(mu = 16, b = 4),
  SUV = make_surv_tbl(mu = 19, b = 4.5)
)

# -----------------------------
# 8) Outflow probability helper
# -----------------------------
f.getOutflows <- function(n_veh = 1, EV_age, LIB_age,
                          maxEV_age = 30, maxLIB_age = 30,
                          mean_ev = 17, sd_ev = 4,
                          mean_lib = 15, sd_lib = 4) {
  sdev <- sd_ev * sqrt(3) / pi
  sdli <- sd_lib * sqrt(3) / pi
  y1 <- (1 - plogis(EV_age + 1, mean_ev, sdev)) / (1 - plogis(EV_age, mean_ev, sdev))
  y2 <- (1 - plogis(LIB_age + 1, mean_lib, sdli)) / (1 - plogis(LIB_age, mean_lib, sdli))
  
  if (EV_age >= maxEV_age) y1 <- 0
  if (LIB_age >= maxLIB_age) y2 <- 0
  
  tibble(
    both_fail = (1 - y1) * (1 - y2) * n_veh,
    ev_fail   = (1 - y1) * y2 * n_veh,
    lib_fail  = y1 * (1 - y2) * n_veh,
    none      = y1 * y2 * n_veh
  )
}

# -----------------------------
# 9) EV Engine Functions
# -----------------------------
EV_engine_init_mx <- function(segment = "Car") {
  mat <- matrix(0, nrow = 31, ncol = 31,
                dimnames = list(paste0("EV_", 0:30), paste0("LIB_", 0:30)))
  lp <- life_param %>% filter(Vehicle == segment)
  list(
    matrix = mat,
    mean_ev = lp$mean_ev,
    sd_ev = lp$sd_ev,
    mean_lib = lp$mean_lib,
    sd_lib = lp$sd_lib
  )
}

EV_engine_step_mx <- function(engine, sales_y = 0, import_y = 0, import_age = 7) {
  mat <- engine$matrix
  mean_ev <- engine$mean_ev
  sd_ev <- engine$sd_ev
  mean_lib <- engine$mean_lib
  sd_lib <- engine$sd_lib
  
  new_matrix <- matrix(0, nrow = 31, ncol = 31,
                       dimnames = list(paste0("EV_", 0:30), paste0("LIB_", 0:30)))
  matrix_ev <- new_matrix
  matrix_lib <- new_matrix
  matrix_both <- new_matrix
  
  ages_ev <- 0:30
  ages_lib <- 0:30
  
  # Process existing stock
  for (ev_a in ages_ev) {
    for (lib_a in ages_lib) {
      N <- mat[ev_a + 1, lib_a + 1]
      if (N < 0.5) next
      
      # Skip age 0 - no retirement in first year
      if (ev_a == 0 && lib_a == 0) {
        new_matrix[2, 2] <- new_matrix[2, 2] + N
        next
      }
      
      out <- f.getOutflows(N, ev_a, lib_a,
                           mean_ev = mean_ev, sd_ev = sd_ev,
                           mean_lib = mean_lib, sd_lib = sd_lib)
      
      new_ev <- min(ev_a + 1, 30)
      new_lib <- min(lib_a + 1, 30)
      
      new_matrix[new_ev + 1, new_lib + 1] <- new_matrix[new_ev + 1, new_lib + 1] + out$none
      matrix_ev[new_ev + 1, lib_a + 1] <- matrix_ev[new_ev + 1, lib_a + 1] + out$ev_fail
      matrix_lib[ev_a + 1, new_lib + 1] <- matrix_lib[ev_a + 1, new_lib + 1] + out$lib_fail
      matrix_both[ev_a + 1, lib_a + 1] <- matrix_both[ev_a + 1, lib_a + 1] + out$both_fail
    }
  }
  
  # Add new sales (age 0, 0)
  new_matrix[1, 1] <- new_matrix[1, 1] + sales_y
  
  # Add imports (age import_age, assume battery same age)
  if (import_y > 0 && import_age <= 30) {
    new_matrix[import_age + 1, import_age + 1] <- 
      new_matrix[import_age + 1, import_age + 1] + import_y
  }
  
  # Compute LIB flows
  lib_evfail_total_vec <- as.integer(round(colSums(matrix_ev)))
  lib_failed_only_vec <- as.integer(round(rowSums(matrix_lib)))
  lib_bothfail_vec <- as.integer(round(colSums(matrix_both)))
  
  evfail_total <- sum(matrix_ev) + sum(matrix_both)
  
  # LIB flow allocation
  lib_evfail_recycle_vec <- as.integer(round(lib_evfail_total_vec * GLOBAL_recycle_share_evfail))
  lib_evfail_repurp_vec <- as.integer(round(lib_evfail_total_vec * GLOBAL_repurpose_share_evfail))
  lib_evfail_reuse_vec <- lib_evfail_total_vec - lib_evfail_recycle_vec - lib_evfail_repurp_vec
  
  lib_fail_recycle_vec <- as.integer(round(lib_failed_only_vec * GLOBAL_recycle_share_libfail))
  lib_fail_repurp_vec <- lib_failed_only_vec - lib_fail_recycle_vec
  
  lib_recycling_vec <- as.integer(lib_evfail_recycle_vec + lib_fail_recycle_vec + lib_bothfail_vec)
  lib_repurpose_vec <- as.integer(lib_evfail_repurp_vec + lib_fail_repurp_vec)
  lib_available_vec <- lib_evfail_total_vec
  
  # -----------------------------
  # Reuse matching logic (simplified version of US model)
  # EVs needing batteries: lib_failed_only_vec (battery failed, EV survived)
  # Reuse pool: lib_evfail_reuse_vec (batteries from retired EVs)
  # -----------------------------
  ev_need_vec <- lib_failed_only_vec  # EVs that need new batteries
  lib_reuse_pool <- lib_evfail_reuse_vec  # batteries available for reuse
  
  # Simple matching: match by age (same age first)
  lib_reuse_vec <- integer(31)
  ev_need_remaining <- ev_need_vec
  lib_pool_remaining <- lib_reuse_pool
  
  # Greedy matching: for each age, match EV needs with available reuse batteries
  for (age in 1:31) {
    matched <- min(ev_need_remaining[age], lib_pool_remaining[age])
    lib_reuse_vec[age] <- matched
    ev_need_remaining[age] <- ev_need_remaining[age] - matched
    lib_pool_remaining[age] <- lib_pool_remaining[age] - matched
  }
  
  # Cross-age matching: remaining needs can use any remaining pool
  total_remaining_need <- sum(ev_need_remaining)
  total_remaining_pool <- sum(lib_pool_remaining)
  cross_age_match <- min(total_remaining_need, total_remaining_pool)
  
  # Distribute cross-age match proportionally to remaining needs
  if (cross_age_match > 0 && total_remaining_need > 0) {
    for (age in 1:31) {
      additional <- as.integer(round(cross_age_match * ev_need_remaining[age] / total_remaining_need))
      lib_reuse_vec[age] <- lib_reuse_vec[age] + additional
      ev_need_remaining[age] <- ev_need_remaining[age] - additional
    }
  }
  
  # LIB_newadd_vector: remaining EV needs after reuse (NOT including new sales/imports)
  # New sales and imports get new batteries but that's handled separately in matrix[1,1]
  lib_newadd_vec <- as.integer(ev_need_remaining)
  
  # Update engine
  engine$matrix <- new_matrix
  
  list(
    engine = engine,
    EV_retired = evfail_total,
    EV_stock = sum(new_matrix),
    EV_stock_vector = as.integer(round(rowSums(new_matrix))),
    LIB_recycling_vector = lib_recycling_vec,
    LIB_repurpose_vector = lib_repurpose_vec,
    LIB_reuse_vector = lib_reuse_vec,
    LIB_available_vector = lib_available_vec,
    LIB_newadd_vector = lib_newadd_vec,
    LIB_evfail_total_vector = lib_evfail_total_vec,
    LIB_bothfail_vector = lib_bothfail_vec
  )
}

# -----------------------------
# 10) Run Mexico Simulation
# -----------------------------
cat("\n=== Running Mexico Turnover Simulation ===\n")

# Initialize ICE stock (2022 age distribution, split by Car/SUV)
ice_stock_car <- ice_age_dist_raw * car_share
ice_stock_suv <- ice_age_dist_raw * suv_share

# Pad to 50 ages
ice_stock_car <- c(ice_stock_car, rep(0, 50 - length(ice_stock_car)))
ice_stock_suv <- c(ice_stock_suv, rep(0, 50 - length(ice_stock_suv)))

# Initialize EV engines with 2022 age distribution
# Assume: 75% BEV, 25% PHEV; battery age = EV age
cat("Initializing EV engines with 2022 age distribution...\n")

bev_share_of_ev <- 0.75
phev_share_of_ev <- 0.25

# Create initial matrices from 2022 EV age distribution
init_ev_matrix <- function(segment, propulsion_share) {
  eng <- EV_engine_init_mx(segment)
  seg_share <- ifelse(segment == "Car", car_share, suv_share)
  
  # Fill diagonal with EV stock by age (battery age = EV age)
  for (age in 0:30) {
    stock_at_age <- ev_age_dist_raw[age + 1] * seg_share * propulsion_share
    if (!is.na(stock_at_age) && stock_at_age > 0) {
      eng$matrix[age + 1, age + 1] <- stock_at_age
    }
  }
  eng
}

ev_engines <- list(
  BEV_Car = init_ev_matrix("Car", bev_share_of_ev),
  BEV_SUV = init_ev_matrix("SUV", bev_share_of_ev),
  PHEV_Car = init_ev_matrix("Car", phev_share_of_ev),
  PHEV_SUV = init_ev_matrix("SUV", phev_share_of_ev)
)

cat("  BEV_Car stock:", sum(ev_engines$BEV_Car$matrix), "\n")
cat("  BEV_SUV stock:", sum(ev_engines$BEV_SUV$matrix), "\n")
cat("  PHEV_Car stock:", sum(ev_engines$PHEV_Car$matrix), "\n")
cat("  PHEV_SUV stock:", sum(ev_engines$PHEV_SUV$matrix), "\n")
cat("  Total EV stock:", sum(ev_engines$BEV_Car$matrix) + sum(ev_engines$BEV_SUV$matrix) + 
      sum(ev_engines$PHEV_Car$matrix) + sum(ev_engines$PHEV_SUV$matrix), "\n")
cat("EV initialization complete.\n")

# Results storage
results_list <- list()
evlib_list <- list()
evlib_detail_list <- list()

# Helper: vec to string with "|" separator
vec_to_string <- function(v) paste(as.integer(v), collapse = "|")

# Track previous year's total stock for demand calculation
prev_total_stock <- sum(ice_stock_car) + sum(ice_stock_suv) + 
  sum(ev_engines$BEV_Car$matrix) + sum(ev_engines$BEV_SUV$matrix) +
  sum(ev_engines$PHEV_Car$matrix) + sum(ev_engines$PHEV_SUV$matrix)

# Main simulation loop (2022-2050)
for (yr in 2022:2050) {
  
  # Get imports from US first (needed for demand calculation)
  import_data <- us_to_mex_extended %>% filter(Year == yr)
  if (nrow(import_data) == 0) {
    import_yr <- avg_import
    import_ice <- import_yr * 0.95
    import_ev_bev <- import_yr * 0.05 * 0.75
    import_ev_phev <- import_yr * 0.05 * 0.25
  } else {
    import_yr <- coalesce(import_data$Import_from_US_Total[1], avg_import)
    import_ice <- coalesce(import_data$Import_ICE[1], import_yr * 0.95)
    import_ev_bev <- coalesce(import_data$Import_BEV[1], import_yr * 0.05 * 0.75)
    import_ev_phev <- coalesce(import_data$Import_PHEV[1], import_yr * 0.05 * 0.25)
  }
  import_ev <- import_ev_bev + import_ev_phev
  
  # Get sales data
  if (yr <= 2025) {
    # Use actual sales (imports are additional)
    sales_yr <- mx_sales %>% filter(Year == yr)
    bev_sales <- coalesce(sales_yr$BEV[1], 0)
    phev_sales <- coalesce(sales_yr$PHEV[1], 0)
    ice_sales <- coalesce(sales_yr$ICE[1], 0)
  } else {
    # Simulate based on PR and demand
    pr_yr <- pr_all %>% filter(Year == yr)
    
    # Calculate target fleet for this year (VPP linear growth)
    target_fleet_yr <- pop_by_year %>% filter(Year == yr) %>% pull(Target_Fleet)
    target_fleet_prev <- pop_by_year %>% filter(Year == yr - 1) %>% pull(Target_Fleet)
    
    # Growth demand = target fleet growth
    growth_demand <- max(0, target_fleet_yr - target_fleet_prev)
    
    # Retirement will be calculated below, use estimate for now
    # Total demand = growth + estimated retirement - imports (imports fulfill part of demand)
    # Estimated retirement based on previous year's stock and average retirement rate (~6%)
    est_retirement <- prev_total_stock * 0.06
    
    # Total demand = retirement + growth
    total_raw_demand <- est_retirement + growth_demand
    
    # Imports fulfill part of demand (reduce new domestic sales needed)
    total_domestic_demand <- max(0, total_raw_demand - import_yr)
    
    # Split by powertrain based on PR
    bev_sales <- total_domestic_demand * pr_yr$BEV_PR
    phev_sales <- total_domestic_demand * pr_yr$PHEV_PR
    ice_sales <- total_domestic_demand * pr_yr$ICE_PR
  }
  
  # ICE retirement and aging
  surv_car <- surv_tbl_ice$Car
  surv_suv <- surv_tbl_ice$SUV
  
  y_car <- surv_car$y
  y_suv <- surv_suv$y
  
  ret_ice_car <- sum(ice_stock_car * (1 - y_car[1:50]), na.rm = TRUE)
  ret_ice_suv <- sum(ice_stock_suv * (1 - y_suv[1:50]), na.rm = TRUE)
  ret_ice <- ret_ice_car + ret_ice_suv
  
  # Age ICE stock
  ice_stock_car_new <- numeric(50)
  ice_stock_suv_new <- numeric(50)
  ice_stock_car_new[2:50] <- ice_stock_car[1:49] * y_car[1:49]
  ice_stock_suv_new[2:50] <- ice_stock_suv[1:49] * y_suv[1:49]
  
  # Add new ICE (domestic sales + imports)
  ice_stock_car_new[1] <- ice_sales * car_share
  ice_stock_suv_new[1] <- ice_sales * suv_share
  
  # Add ICE imports at age 7
  ice_stock_car_new[8] <- ice_stock_car_new[8] + import_ice * car_share
  ice_stock_suv_new[8] <- ice_stock_suv_new[8] + import_ice * suv_share
  
  ice_stock_car <- ice_stock_car_new
  ice_stock_suv <- ice_stock_suv_new
  
  # EV simulation (import_ev_bev and import_ev_phev already from export projection)
  step_bev_car <- EV_engine_step_mx(ev_engines$BEV_Car, 
                                     sales_y = bev_sales * car_share,
                                     import_y = import_ev_bev * car_share,
                                     import_age = 7)
  ev_engines$BEV_Car <- step_bev_car$engine
  
  step_bev_suv <- EV_engine_step_mx(ev_engines$BEV_SUV, 
                                     sales_y = bev_sales * suv_share,
                                     import_y = import_ev_bev * suv_share,
                                     import_age = 7)
  ev_engines$BEV_SUV <- step_bev_suv$engine
  
  step_phev_car <- EV_engine_step_mx(ev_engines$PHEV_Car, 
                                      sales_y = phev_sales * car_share,
                                      import_y = import_ev_phev * car_share,
                                      import_age = 7)
  ev_engines$PHEV_Car <- step_phev_car$engine
  
  step_phev_suv <- EV_engine_step_mx(ev_engines$PHEV_SUV, 
                                      sales_y = phev_sales * suv_share,
                                      import_y = import_ev_phev * suv_share,
                                      import_age = 7)
  ev_engines$PHEV_SUV <- step_phev_suv$engine
  
  # Aggregate results
  ret_bev <- step_bev_car$EV_retired + step_bev_suv$EV_retired
  ret_phev <- step_phev_car$EV_retired + step_phev_suv$EV_retired
  
  stock_bev <- step_bev_car$EV_stock + step_bev_suv$EV_stock
  stock_phev <- step_phev_car$EV_stock + step_phev_suv$EV_stock
  stock_ice <- sum(ice_stock_car) + sum(ice_stock_suv)
  
  # Store main results
  results_list[[as.character(yr)]] <- tibble(
    Year = yr,
    add_BEV = bev_sales,
    add_PHEV = phev_sales,
    add_ICE = ice_sales,
    add_Total = bev_sales + phev_sales + ice_sales,
    ret_BEV = ret_bev,
    ret_PHEV = ret_phev,
    ret_ICE = ret_ice,
    ret_Total = ret_bev + ret_phev + ret_ice,
    stock_BEV = stock_bev,
    stock_PHEV = stock_phev,
    stock_ICE = stock_ice,
    stock_Total = stock_bev + stock_phev + stock_ice,
    import_from_US = import_yr,
    import_ICE = import_ice,
    import_EV = import_ev
  )
  
  # Store EVLIB results by Segment × Propulsion (like US)
  evlib_detail_list[[paste(yr, "Car", "BEV")]] <- tibble(
    State = "Mexico", Segment = "Car", Propulsion = "BEV", Year = yr,
    LIB_recycling = sum(as.integer(step_bev_car$LIB_recycling_vector)),
    LIB_available = sum(as.integer(step_bev_car$LIB_available_vector)),
    LIB_reuse_EV = sum(as.integer(step_bev_car$LIB_reuse_vector)),
    LIB_new_add = sum(as.integer(step_bev_car$LIB_newadd_vector)),
    LIB_repurpose = sum(as.integer(step_bev_car$LIB_repurpose_vector)),
    EV_stock = step_bev_car$EV_stock,
    LIB_recycling_vector = vec_to_string(step_bev_car$LIB_recycling_vector),
    LIB_available_vector = vec_to_string(step_bev_car$LIB_available_vector),
    LIB_reuse_vector = vec_to_string(step_bev_car$LIB_reuse_vector),
    LIB_newadd_vector = vec_to_string(step_bev_car$LIB_newadd_vector),
    LIB_repurpose_vector = vec_to_string(step_bev_car$LIB_repurpose_vector),
    EV_stock_vector = vec_to_string(step_bev_car$EV_stock_vector)
  )
  
  evlib_detail_list[[paste(yr, "SUV", "BEV")]] <- tibble(
    State = "Mexico", Segment = "SUV", Propulsion = "BEV", Year = yr,
    LIB_recycling = sum(as.integer(step_bev_suv$LIB_recycling_vector)),
    LIB_available = sum(as.integer(step_bev_suv$LIB_available_vector)),
    LIB_reuse_EV = sum(as.integer(step_bev_suv$LIB_reuse_vector)),
    LIB_new_add = sum(as.integer(step_bev_suv$LIB_newadd_vector)),
    LIB_repurpose = sum(as.integer(step_bev_suv$LIB_repurpose_vector)),
    EV_stock = step_bev_suv$EV_stock,
    LIB_recycling_vector = vec_to_string(step_bev_suv$LIB_recycling_vector),
    LIB_available_vector = vec_to_string(step_bev_suv$LIB_available_vector),
    LIB_reuse_vector = vec_to_string(step_bev_suv$LIB_reuse_vector),
    LIB_newadd_vector = vec_to_string(step_bev_suv$LIB_newadd_vector),
    LIB_repurpose_vector = vec_to_string(step_bev_suv$LIB_repurpose_vector),
    EV_stock_vector = vec_to_string(step_bev_suv$EV_stock_vector)
  )
  
  evlib_detail_list[[paste(yr, "Car", "PHEV")]] <- tibble(
    State = "Mexico", Segment = "Car", Propulsion = "PHEV", Year = yr,
    LIB_recycling = sum(as.integer(step_phev_car$LIB_recycling_vector)),
    LIB_available = sum(as.integer(step_phev_car$LIB_available_vector)),
    LIB_reuse_EV = sum(as.integer(step_phev_car$LIB_reuse_vector)),
    LIB_new_add = sum(as.integer(step_phev_car$LIB_newadd_vector)),
    LIB_repurpose = sum(as.integer(step_phev_car$LIB_repurpose_vector)),
    EV_stock = step_phev_car$EV_stock,
    LIB_recycling_vector = vec_to_string(step_phev_car$LIB_recycling_vector),
    LIB_available_vector = vec_to_string(step_phev_car$LIB_available_vector),
    LIB_reuse_vector = vec_to_string(step_phev_car$LIB_reuse_vector),
    LIB_newadd_vector = vec_to_string(step_phev_car$LIB_newadd_vector),
    LIB_repurpose_vector = vec_to_string(step_phev_car$LIB_repurpose_vector),
    EV_stock_vector = vec_to_string(step_phev_car$EV_stock_vector)
  )
  
  evlib_detail_list[[paste(yr, "SUV", "PHEV")]] <- tibble(
    State = "Mexico", Segment = "SUV", Propulsion = "PHEV", Year = yr,
    LIB_recycling = sum(as.integer(step_phev_suv$LIB_recycling_vector)),
    LIB_available = sum(as.integer(step_phev_suv$LIB_available_vector)),
    LIB_reuse_EV = sum(as.integer(step_phev_suv$LIB_reuse_vector)),
    LIB_new_add = sum(as.integer(step_phev_suv$LIB_newadd_vector)),
    LIB_repurpose = sum(as.integer(step_phev_suv$LIB_repurpose_vector)),
    EV_stock = step_phev_suv$EV_stock,
    LIB_recycling_vector = vec_to_string(step_phev_suv$LIB_recycling_vector),
    LIB_available_vector = vec_to_string(step_phev_suv$LIB_available_vector),
    LIB_reuse_vector = vec_to_string(step_phev_suv$LIB_reuse_vector),
    LIB_newadd_vector = vec_to_string(step_phev_suv$LIB_newadd_vector),
    LIB_repurpose_vector = vec_to_string(step_phev_suv$LIB_repurpose_vector),
    EV_stock_vector = vec_to_string(step_phev_suv$EV_stock_vector)
  )
  
  # Also keep aggregated version for backward compatibility
  lib_recycling <- as.integer(step_bev_car$LIB_recycling_vector) +
    as.integer(step_bev_suv$LIB_recycling_vector) +
    as.integer(step_phev_car$LIB_recycling_vector) +
    as.integer(step_phev_suv$LIB_recycling_vector)
  
  lib_available <- as.integer(step_bev_car$LIB_available_vector) +
    as.integer(step_bev_suv$LIB_available_vector) +
    as.integer(step_phev_car$LIB_available_vector) +
    as.integer(step_phev_suv$LIB_available_vector)
  
  # Aggregate all segments for evlib_list
  lib_recycling_total <- as.integer(step_bev_car$LIB_recycling_vector) +
    as.integer(step_bev_suv$LIB_recycling_vector) +
    as.integer(step_phev_car$LIB_recycling_vector) +
    as.integer(step_phev_suv$LIB_recycling_vector)
  
  lib_repurpose_total <- as.integer(step_bev_car$LIB_repurpose_vector) +
    as.integer(step_bev_suv$LIB_repurpose_vector) +
    as.integer(step_phev_car$LIB_repurpose_vector) +
    as.integer(step_phev_suv$LIB_repurpose_vector)
  
  lib_reuse_total <- as.integer(step_bev_car$LIB_reuse_vector) +
    as.integer(step_bev_suv$LIB_reuse_vector) +
    as.integer(step_phev_car$LIB_reuse_vector) +
    as.integer(step_phev_suv$LIB_reuse_vector)
  
  lib_available_total <- as.integer(step_bev_car$LIB_available_vector) +
    as.integer(step_bev_suv$LIB_available_vector) +
    as.integer(step_phev_car$LIB_available_vector) +
    as.integer(step_phev_suv$LIB_available_vector)
  
  evlib_list[[as.character(yr)]] <- tibble(
    Year = yr,
    LIB_recycling = sum(lib_recycling_total),
    LIB_repurpose = sum(lib_repurpose_total),
    LIB_reuse_EV = sum(lib_reuse_total),
    LIB_available = sum(lib_available_total),
    EV_retired = ret_bev + ret_phev,
    EV_stock = stock_bev + stock_phev
  )
  
  # Update prev_total_stock for next iteration
  prev_total_stock <- stock_bev + stock_phev + stock_ice
  
  if (yr %% 5 == 0) cat("Year", yr, "- Stock:", format(round(prev_total_stock), big.mark = ","), 
                        "- Imports:", format(round(import_yr), big.mark = ","), "\n")
}

# -----------------------------
# 11) Combine and Save Results
# -----------------------------
cat("\n=== Saving Results ===\n")

results_df <- bind_rows(results_list)
evlib_df <- bind_rows(evlib_list)
evlib_detail_df <- bind_rows(evlib_detail_list)

dir.create("Outputs/Mexico", showWarnings = FALSE, recursive = TRUE)

# Main fleet turnover
write_csv(results_df, "Outputs/Mexico/Mexico_FleetTurnover_2022_2050.csv")
cat("Saved: Outputs/Mexico/Mexico_FleetTurnover_2022_2050.csv\n")

# Aggregated EVLIB
write_csv(evlib_df, "Outputs/Mexico/Mexico_EVLIB_Flows_2022_2050.csv")
cat("Saved: Outputs/Mexico/Mexico_EVLIB_Flows_2022_2050.csv\n")

# Detailed EVLIB by Segment × Propulsion (like US)
write_csv(evlib_detail_df, "Outputs/Mexico/EVLIB_Flows_detail_ACCII.csv")
cat("Saved: Outputs/Mexico/EVLIB_Flows_detail_ACCII.csv\n")

# State totals (aggregated by year, like US ClosedLoop_StateTotals)
state_totals <- results_df %>%
  mutate(State = "Mexico") %>%
  select(State, Year, add_BEV, add_PHEV, add_ICE, ret_BEV, ret_PHEV, ret_ICE,
         stock_BEV, stock_PHEV, stock_ICE, stock_Total, import_from_US) %>%
  mutate(Scenario = "ACCII")

write_csv(state_totals, "Outputs/Mexico/ClosedLoop_StateTotals_ACCII.csv")
cat("Saved: Outputs/Mexico/ClosedLoop_StateTotals_ACCII.csv\n")

# -----------------------------
# 12) Summary
# -----------------------------
cat("\n=== Mexico Fleet Turnover Summary ===\n")

summary_years <- results_df %>%
  filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
  mutate(EV_Share = (add_BEV + add_PHEV) / add_Total * 100)

print(summary_years %>% select(Year, add_BEV, add_PHEV, add_ICE, EV_Share, stock_Total))

cat("\n=== EVLIB Summary ===\n")
evlib_summary <- evlib_df %>%
  filter(Year %in% c(2030, 2040, 2050)) %>%
  select(Year, LIB_recycling, LIB_repurpose, LIB_reuse_EV, EV_retired, EV_stock)
print(evlib_summary)

cat("\n=== Mexico Turnover Model Complete! ===\n")

