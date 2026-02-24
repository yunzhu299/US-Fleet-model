## 022 — HDV (Heavy Duty Vehicle) EV Fleet Turnover using ICCT data
## Medium trucks + Heavy trucks for Mexico, Canada, United States
## YZC Feb 2026

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# -----------------------------
# 0) Global parameters for HDV
# -----------------------------
# User specified: mean_ev = 16, mean_lib = 8, sd = 4, max = 30
GLOBAL_hdv_mean_ev  <- 16   # EV survival mean
GLOBAL_hdv_mean_lib <- 8    # LIB survival mean
GLOBAL_hdv_sd       <- 4    # sd for both
GLOBAL_hdv_max_age  <- 30   # max age

# LIB reuse parameters (same as LDV)
GLOBAL_ev_age_newLib         <- 8    # new LIB needed if no reuse after this EV age
GLOBAL_max_reuse_lib_share   <- 0.5  # share of only-EV-fail LIB that can be reused in EV (cap)
GLOBAL_max_ev_age            <- 12   # EVs older than this do not receive new LIBs
GLOBAL_max_lib_age_ev        <- 12   # max LIB age that can be reused in EV
GLOBAL_max_lib_age_repurpose <- 12   # max LIB age that can be repurposed to BESS

# Split rules
GLOBAL_reuse_share_evfail      <- 0.50
GLOBAL_repurpose_share_evfail  <- 0.25
GLOBAL_recycle_share_evfail    <- 0.25
GLOBAL_repurpose_share_libfail <- 0.50
GLOBAL_recycle_share_libfail   <- 0.50

# -----------------------------
# 1) Read ICCT data
# -----------------------------
cat("=== Reading ICCT data ===\n")
icct_path <- "Inputs/ICCT.xlsx"

# Read first sheet (adjust if needed)
icct_raw <- read_excel(icct_path, sheet = 1)

cat("Column names:", paste(names(icct_raw), collapse = ", "), "\n")
cat("Unique countries:", paste(unique(icct_raw$Country), collapse = ", "), "\n")
cat("Unique vehicles:", paste(unique(icct_raw$Vehicle), collapse = ", "), "\n")
cat("Unique scenarios:", paste(unique(icct_raw$Scenario), collapse = ", "), "\n")

# Filter for Mexico, Canada, United States
# Filter for Medium truck, Heavy trucks
# Filter for BEV powertrain only
# Map scenarios: Baseline 2024 -> Repeal, Momentum -> ACCII
icct_hdv <- icct_raw %>%
  filter(Country %in% c("Mexico", "Canada", "United States")) %>%
  filter(Vehicle %in% c("Medium trucks", "Heavy trucks")) %>%
  filter(Powertrain == "BEV") %>%  # Only BEV
  mutate(
    Scenario_mapped = case_when(
      Scenario == "Baseline 2024" ~ "Repeal",
      Scenario == "Momentum" ~ "ACCII",
      TRUE ~ Scenario
    ),
    Year = as.integer(CY),
    Sales = as.numeric(Sales)
  ) %>%
  select(Country, Year, Vehicle, Scenario = Scenario_mapped, Sales) %>%
  filter(!is.na(Sales))

cat("\nFiltered HDV data:\n")
print(head(icct_hdv, 20))

# Keep Medium and Heavy trucks separate (no aggregation)
icct_hdv_agg <- icct_hdv %>%
  group_by(Country, Year, Scenario, Vehicle) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nHDV sales by Country × Year × Scenario × Vehicle:\n")
print(icct_hdv_agg)

# -----------------------------
# 2) Outflow probability helper (same as 004)
# -----------------------------
f.getOutflows_HDV <- function(n_veh = 1, EV_age, LIB_age,
                               maxEV_age = GLOBAL_hdv_max_age, 
                               maxLIB_age = GLOBAL_hdv_max_age,
                               mean_ev   = GLOBAL_hdv_mean_ev, 
                               sd_ev     = GLOBAL_hdv_sd,
                               mean_lib  = GLOBAL_hdv_mean_lib, 
                               sd_lib    = GLOBAL_hdv_sd) {
  
  # Logistic survival
  sdev <- sd_ev  * sqrt(3)/pi
  sdli <- sd_lib * sqrt(3)/pi
  y1 <- (1 - plogis(EV_age+1, mean_ev,  sdev)) /
    (1 - plogis(EV_age,   mean_ev,  sdev))
  y2 <- (1 - plogis(LIB_age+1, mean_lib, sdli)) /
    (1 - plogis(LIB_age,   mean_lib, sdli))
  
  # Forced retirement at max age
  if (EV_age  >= maxEV_age)  y1 <- 0
  if (LIB_age >= maxLIB_age) y2 <- 0
  
  tibble(
    both_fail = (1-y1) * (1-y2) * n_veh,
    ev_fail   = (1-y1) *  y2   * n_veh,
    lib_fail  =  y1   * (1-y2) * n_veh,
    none      =  y1   *  y2   * n_veh
  )
}

# -----------------------------
# 3) HDV Engine step function
# -----------------------------
HDV_engine_step <- function(engine, sales_y = 0, reuse_shrink = 1.0) {
  
  mat <- engine$matrix
  max_age <- GLOBAL_hdv_max_age
  
  # Empty matrices for next-year stock
  new_matrix <- matrix(0, nrow = max_age + 1, ncol = max_age + 1,
                       dimnames = list(paste0("EV_", 0:max_age),
                                       paste0("LIB_", 0:max_age)))
  matrix_ev   <- new_matrix
  matrix_lib  <- new_matrix
  matrix_both <- new_matrix
  
  ages_ev      <- 0:max_age
  ages_lib_0_30 <- 0:max_age
  
  # Loop over current EV age × LIB age
  # Skip age 0 - no retirement in first year (consistent with LDV)
  for (ev_a in ages_ev) {
    for (lib_a in ages_lib_0_30) {
      N <- mat[ev_a + 1, lib_a + 1]
      if (N < 0.5) next
      
      # Skip age 0 vehicles - they don't retire in their first year
      if (ev_a == 0 && lib_a == 0) {
        # Just age them to (1,1) without any failure
        new_matrix[2, 2] <- new_matrix[2, 2] + N
        next
      }
      
      out <- f.getOutflows_HDV(N, ev_a, lib_a)
      
      new_ev  <- min(ev_a + 1, max_age)
      new_lib <- min(lib_a + 1, max_age)
      
      new_matrix[new_ev + 1, new_lib + 1] <- 
        new_matrix[new_ev + 1, new_lib + 1] + out$none
      matrix_ev[new_ev + 1, lib_a + 1] <- 
        matrix_ev[new_ev + 1, lib_a + 1] + out$ev_fail
      matrix_lib[ev_a + 1, new_lib + 1] <- 
        matrix_lib[ev_a + 1, new_lib + 1] + out$lib_fail
      matrix_both[ev_a + 1, lib_a + 1] <- 
        matrix_both[ev_a + 1, lib_a + 1] + out$both_fail
    }
  }
  
  # Compute flows
  lib_evfail_total_vec <- as.integer(round(colSums(matrix_ev)))
  lib_failed_only_vec  <- as.integer(round(rowSums(matrix_lib)))
  lib_bothfail_vec     <- as.integer(round(colSums(matrix_both)))
  
  # Total EV retirements
  evfail_total <- sum(matrix_ev) + sum(matrix_both)
  
  # LIB flows
  lib_evfail_recycle_vec <- as.integer(round(lib_evfail_total_vec * GLOBAL_recycle_share_evfail))
  lib_evfail_repurp_vec  <- as.integer(round(lib_evfail_total_vec * GLOBAL_repurpose_share_evfail))
  lib_evfail_reuse_vec   <- lib_evfail_total_vec - lib_evfail_recycle_vec - lib_evfail_repurp_vec
  
  lib_fail_recycle_vec <- as.integer(round(lib_failed_only_vec * GLOBAL_recycle_share_libfail))
  lib_fail_repurp_vec  <- lib_failed_only_vec - lib_fail_recycle_vec
  
  lib_bothfail_recycle_vec <- lib_bothfail_vec
  
  # Total recycling = evfail recycle + libfail recycle + bothfail
  lib_recycling_vec <- as.integer(lib_evfail_recycle_vec + lib_fail_recycle_vec + lib_bothfail_recycle_vec)
  
  # Total repurpose
  lib_repurpose_vec <- as.integer(lib_evfail_repurp_vec + lib_fail_repurp_vec)
  
  # Reuse (LIB available for EV reuse)
  lib_reuse_ev_vec <- as.integer(round(lib_evfail_reuse_vec * reuse_shrink))
  
  # New batteries needed
  lib_available_vec <- lib_evfail_total_vec
  
  # New additions
  new_matrix[1, 1] <- new_matrix[1, 1] + sales_y
  
  # Return updated engine
  list(
    matrix    = new_matrix,
    evfail    = evfail_total,
    lib_flows = list(
      lib_recycling_vec = lib_recycling_vec,
      lib_repurpose_vec = lib_repurpose_vec,
      lib_reuse_ev_vec  = lib_reuse_ev_vec,
      lib_available_vec = lib_available_vec,
      lib_evfail_total_vec = lib_evfail_total_vec,
      lib_bothfail_vec = lib_bothfail_vec
    ),
    total_stock = sum(new_matrix)
  )
}

# -----------------------------
# 4) Initialize HDV engine
# -----------------------------
HDV_engine_init <- function(max_age = GLOBAL_hdv_max_age) {
  mat <- matrix(0, nrow = max_age + 1, ncol = max_age + 1,
                dimnames = list(paste0("EV_", 0:max_age),
                                paste0("LIB_", 0:max_age)))
  list(matrix = mat)
}

# -----------------------------
# 5) Run simulation for each country × scenario × vehicle
# -----------------------------
run_hdv_simulation <- function(sales_data, country_name, scenario_name, vehicle_type) {
  
  cat("\n=== Running HDV simulation:", country_name, "-", scenario_name, "-", vehicle_type, "===\n")
  
  # Filter data for this country, scenario, and vehicle type
  country_sales <- sales_data %>%
    filter(Country == country_name, Scenario == scenario_name, Vehicle == vehicle_type) %>%
    arrange(Year)
  
  if (nrow(country_sales) == 0) {
    cat("No data found for", country_name, "-", scenario_name, "\n")
    return(NULL)
  }
  
  years <- sort(unique(country_sales$Year))
  min_year <- min(years)
  max_year <- max(years)
  
  # Extend to 2050 if needed
  if (max_year < 2050) {
    # Use last available year's sales for projection
    last_sales <- country_sales %>% filter(Year == max_year)
    for (y in (max_year + 1):2050) {
      country_sales <- bind_rows(country_sales, 
                                  last_sales %>% mutate(Year = y))
    }
  }
  
  all_years <- sort(unique(country_sales$Year))
  
  # Initialize engine
  engine <- HDV_engine_init()
  
  # Results storage
  results <- list()
  
  for (yr in all_years) {
    sales_yr <- country_sales %>% 
      filter(Year == yr) %>% 
      pull(Total_Sales) %>% 
      sum(na.rm = TRUE)
    
    # Run one step
    engine <- HDV_engine_step(engine, sales_y = sales_yr, reuse_shrink = 1.0)
    
    # Store results (use "|" as vector separator)
    results[[as.character(yr)]] <- tibble(
      Country = country_name,
      Scenario = scenario_name,
      Vehicle = vehicle_type,
      Year = yr,
      New_Sales = sales_yr,
      Total_Stock = engine$total_stock,
      EV_Retirement = engine$evfail,
      LIB_recycling_vector = paste(engine$lib_flows$lib_recycling_vec, collapse = "|"),
      LIB_repurpose_vector = paste(engine$lib_flows$lib_repurpose_vec, collapse = "|"),
      LIB_reuse_ev_vector = paste(engine$lib_flows$lib_reuse_ev_vec, collapse = "|"),
      LIB_available_vector = paste(engine$lib_flows$lib_available_vec, collapse = "|"),
      LIB_evfail_total_vector = paste(engine$lib_flows$lib_evfail_total_vec, collapse = "|"),
      LIB_bothfail_vector = paste(engine$lib_flows$lib_bothfail_vec, collapse = "|"),
      # Scalar totals
      LIB_recycling = sum(engine$lib_flows$lib_recycling_vec),
      LIB_repurpose = sum(engine$lib_flows$lib_repurpose_vec),
      LIB_reuse_EV = sum(engine$lib_flows$lib_reuse_ev_vec)
    )
    
    if (yr %% 5 == 0) cat("  Year", yr, "- Stock:", round(engine$total_stock), 
                          "- Retire:", round(engine$evfail), "\n")
  }
  
  bind_rows(results)
}

# -----------------------------
# 6) Run all simulations
# -----------------------------
countries <- c("Mexico", "Canada", "United States")
scenarios <- c("ACCII", "Repeal")
vehicles <- c("Medium trucks", "Heavy trucks")

all_results <- list()

for (country in countries) {
  for (scenario in scenarios) {
    for (vehicle in vehicles) {
      res <- run_hdv_simulation(icct_hdv_agg, country, scenario, vehicle)
      if (!is.null(res)) {
        all_results[[paste(country, scenario, vehicle, sep = "_")]] <- res
      }
    }
  }
}

# Combine all results
hdv_results <- bind_rows(all_results)

# -----------------------------
# 7) Save outputs - only two tables by scenario
# -----------------------------
dir.create("Outputs/HDV", showWarnings = FALSE, recursive = TRUE)

# Save by scenario (two tables only)
for (scenario in scenarios) {
  scenario_data <- hdv_results %>% filter(Scenario == scenario)
  write_csv(scenario_data, paste0("Outputs/HDV/HDV_EV_Turnover_", scenario, ".csv"))
  cat("Saved: Outputs/HDV/HDV_EV_Turnover_", scenario, ".csv\n")
}

# Summary table
cat("\n=== HDV EV Turnover Summary (2030, 2040, 2050) ===\n")
summary_tbl <- hdv_results %>%
  filter(Year %in% c(2030, 2040, 2050)) %>%
  select(Country, Scenario, Vehicle, Year, New_Sales, Total_Stock, EV_Retirement, 
         LIB_recycling, LIB_repurpose, LIB_reuse_EV) %>%
  arrange(Country, Vehicle, Scenario, Year)
print(summary_tbl)

cat("\n=== HDV Turnover Complete! ===\n")
cat("Parameters used:\n")
cat("  EV mean lifetime:", GLOBAL_hdv_mean_ev, "years\n")
cat("  LIB mean lifetime:", GLOBAL_hdv_mean_lib, "years\n")
cat("  Standard deviation:", GLOBAL_hdv_sd, "years\n")
cat("  Max age:", GLOBAL_hdv_max_age, "years\n")

