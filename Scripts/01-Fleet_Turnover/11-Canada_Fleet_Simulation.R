## 011 — Canada Fleet Simulation (2020-2050) with full EV battery tracking
## Uses empirical ICE survival curves and EV engine from 004
## YZC Feb 2026

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(readr)

# -----------------------------
# 0) Global settings
# -----------------------------
years <- 2020:2050
MAX_AGE <- 30

as_scalar_num <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- suppressWarnings(as.numeric(x))
  if (length(x) > 1) x <- sum(x, na.rm = TRUE)
  if (!is.finite(x)) x <- default
  x
}

dir.create("Outputs/Canada", showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 1) Load Canada data files
# -----------------------------
cat("Loading Canada data files...\n")

canada_pop <- read_csv("Inputs/Parameters/CanadaPopulation.csv", show_col_types = FALSE)
canada_type_share <- read_csv("Inputs/Parameters/Canada_TypeShare_2020.csv", show_col_types = FALSE)
canada_ice_pool <- read_csv("Inputs/Parameters/Canada_ICE_Pool_2020.csv", show_col_types = FALSE)
canada_ev_historical <- read_csv("Inputs/Parameters/Canada_EV_Historical.csv", show_col_types = FALSE)
canada_pr_accii <- read_csv("Inputs/Parameters/Canada_PR_ACCII.csv", show_col_types = FALSE)
canada_pr_repeal <- read_csv("Inputs/Parameters/Canada_PR_Repeal.csv", show_col_types = FALSE)
canada_age_dist <- read_csv("Inputs/Parameters/Canada_AgeDistribution.csv", show_col_types = FALSE)
canada_total_sales <- read_csv("Inputs/Parameters/Canada_TotalSales_bySegment.csv", show_col_types = FALSE)

cat("Data files loaded.\n")

# -----------------------------
# 2) ICE Survival Curves (Logistic, same as 002)
# -----------------------------
make_surv_tbl <- function(mu, b) {
  ages <- 0:49
  S <- 1 / (1 + exp((ages - mu) / b))
  y <- c(S[-1] / S[-length(S)], 0)
  tibble(ageID = ages, S = S, y = pmin(pmax(y, 1e-8), 0.999999))
}

surv_tbl_ice <- list(
  Car = make_surv_tbl(mu = 16, b = 4),
  SUV = make_surv_tbl(mu = 19, b = 4.5)
)

cat("ICE Survival curves loaded (logistic)\n")
cat("Car: mu=16, b=4 (median ~16 years)\n")
cat("SUV/Truck: mu=19, b=4.5 (median ~19 years)\n")

# -----------------------------
# 3) EV Engine & Lifetime Params
# -----------------------------
# Always re-source 004 to guarantee f.getOutflows has the correct signature
# (dist.Age parameter). Without this, a prior run of 24-Mexico_Turnover.R could
# leave an incompatible f.getOutflows in the environment.
local_004 <- file.path("Scripts", "01-Fleet_Turnover", "04-EV_Turnover.R")
if (!file.exists(local_004)) local_004 <- "04-EV_Turnover.R"
if (file.exists(local_004)) {
  source(local_004, local = FALSE)
  cat("04-EV_Turnover.R re-sourced to ensure correct f.getOutflows definition.\n")
} else {
  stop("Cannot find 04-EV_Turnover.R. Please run it first.")
}
if (!exists("life_param")) {
  life_param <- tibble(
    Vehicle       = c("Car", "SUV"),
    mean_ev       = c(17, 17),
    sd_ev         = c(4, 4),
    mean_lib      = c(15, 15),
    sd_lib        = c(4, 4),
    scen_lifetime = "Baseline"
  )
}
# 确保 scen_lifetime 列存在（旧版 life_param 可能缺少此列）
if (!"scen_lifetime" %in% names(life_param)) {
  life_param <- life_param %>% mutate(scen_lifetime = "Baseline")
}

# -----------------------------
# 4) Export factor for Canada
# -----------------------------
canada_export_factor <- 0.00
domestic_factor <- 1 - canada_export_factor
cat("Canada export factor:", canada_export_factor, "(disabled)\n")

# -----------------------------
# 5) Prepare data structures
# -----------------------------
provinces <- unique(canada_type_share$Province)
cat("Provinces:", length(provinces), "\n")

# Segment share 2020
seg_share_2020 <- canada_type_share %>%
  transmute(
    State = Province,
    Car = type_share_Car,
    SUV = type_share_Truck
  )

# Population growth
pop_growth <- canada_pop %>%
  arrange(Province, Year) %>%
  group_by(Province) %>%
  mutate(
    Pop_lag = lag(Population),
    Pop_growth = Population - Pop_lag
  ) %>%
  ungroup() %>%
  filter(!is.na(Pop_growth))

# VPP from 2020
first_pop_year <- min(canada_pop$Year)
canada_pop_first <- canada_pop %>% filter(Year == first_pop_year)

vpp <- canada_ice_pool %>%
  left_join(canada_pop_first, by = "Province") %>%
  mutate(
    Total_Vehicles = ICE_Total,
    VPP = Total_Vehicles / Population
  ) %>%
  select(Province, VPP)

growth_from_pop <- pop_growth %>%
  left_join(vpp, by = "Province") %>%
  mutate(
    Growth_from_pop = pmax(0, Pop_growth * VPP)
  ) %>%
  select(State = Province, Year, Growth_from_pop)

# Split growth by segment
growth_seg_base <- growth_from_pop %>%
  left_join(seg_share_2020, by = "State") %>%
  pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "seg_share") %>%
  mutate(Growth_seg = Growth_from_pop * coalesce(seg_share, 0.5)) %>%
  select(State, Segment, Year, Growth_seg)

# -----------------------------
# 5b) Prepare ACTUAL 2020-2024 sales (like US 005)
#     National total sales - national EV = national ICE
#     ICE allocated to provinces by ICE registration weights
# -----------------------------
cat("\nPreparing 2020-2024 actual sales data...\n")

# Actual EV sales by province (already province-level from 010)
real_ev_ca <- canada_ev_historical %>%
  mutate(Segment = case_when(
    Vehicle %in% c("Car", "Passenger Car") ~ "Car",
    Vehicle %in% c("SUV", "Truck", "Light Truck") ~ "SUV",
    TRUE ~ "Car"
  )) %>%
  rename(Year = `Sale Year`) %>%
  filter(Year >= 2020, Year <= 2024, Propulsion %in% c("BEV", "PHEV")) %>%
  select(State, Segment, Propulsion, Year, Sales)

# National EV totals by year × segment
ev_national_ca <- real_ev_ca %>%
  group_by(Year, Segment) %>%
  summarise(EV_total = sum(Sales, na.rm = TRUE), .groups = "drop")

# National ICE = total sales - EV (by year × segment)
ice_national_ca <- canada_total_sales %>%
  filter(Year >= 2020, Year <= 2024) %>%
  left_join(ev_national_ca, by = c("Year", "Segment")) %>%
  mutate(EV_total = coalesce(EV_total, 0),
         ICE_national = pmax(0, Total_Sales - EV_total)) %>%
  select(Year, Segment, ICE_national)

# Province ICE weights from 2020 registrations (ICE_Car, ICE_Truck)
ice_weights_ca <- canada_ice_pool %>%
  select(Province, ICE_Car, ICE_Truck) %>%
  pivot_longer(c(ICE_Car, ICE_Truck), names_to = "seg_raw", values_to = "ICE_reg") %>%
  mutate(Segment = if_else(seg_raw == "ICE_Car", "Car", "SUV")) %>%
  select(State = Province, Segment, ICE_reg) %>%
  group_by(Segment) %>%
  mutate(w_share = ICE_reg / sum(ICE_reg, na.rm = TRUE)) %>%
  ungroup() %>%
  select(State, Segment, w_share)

# Allocate national ICE to provinces
ice_province_2020_24 <- ice_national_ca %>%
  left_join(ice_weights_ca, by = "Segment") %>%
  mutate(ICE_sales = round(ICE_national * w_share)) %>%
  select(State, Segment, Year, ICE_sales)

cat("  Actual EV rows (2020-2024):", nrow(real_ev_ca), "\n")
cat("  ICE province allocations:", nrow(ice_province_2020_24), "\n")

# -----------------------------
# 6) PR tables
# -----------------------------
make_PR_wide <- function(PR_table) {
  PR_table %>%
    mutate(State = str_trim(State), Year = as.integer(Year)) %>%
    filter(Propulsion %in% c("BEV", "PHEV")) %>%
    select(State, Year, Propulsion, Fraction) %>%
    pivot_wider(names_from = Propulsion, values_from = Fraction) %>%
    mutate(BEV = coalesce(BEV, 0), PHEV = coalesce(PHEV, 0)) %>%
    {
      min_year <- min(.$Year, na.rm = TRUE)
      have_min <- dplyr::filter(., Year == min_year)
      if (min_year > 2020) {
        add_years <- have_min %>%
          select(State, BEV, PHEV) %>%
          crossing(Year = 2020:(min_year - 1))
        bind_rows(., add_years)
      } else {
        .
      }
    } %>%
    arrange(State, Year) %>%
    mutate(ICE = pmax(0, 1 - BEV - PHEV))
}

# -----------------------------
# 7) Initialize ICE stock (age distribution)
# -----------------------------
cat("\nInitializing ICE stock...\n")

if ("ICE_Car" %in% names(canada_ice_pool) && "ICE_Truck" %in% names(canada_ice_pool)) {
  ice_by_seg <- canada_ice_pool %>%
    select(Province, Car = ICE_Car, SUV = ICE_Truck) %>%
    pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "ICE_Total")
} else {
  ice_by_seg <- canada_ice_pool %>%
    mutate(Car = ICE_Total * 0.5, SUV = ICE_Total * 0.5) %>%
    select(Province, Car, SUV) %>%
    pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "ICE_Total")
}

# Age fractions
if (exists("canada_age_dist") && nrow(canada_age_dist) > 0) {
  age_fracs <- canada_age_dist %>%
    filter(Segment %in% c("Car", "SUV")) %>%
    group_by(Province, Segment) %>%
    mutate(ageFraction = ageFraction / sum(ageFraction, na.rm = TRUE)) %>%
    ungroup()
} else {
  age_fracs <- expand.grid(
    Province = unique(canada_ice_pool$Province),
    Segment = c("Car", "SUV"),
    ageID = 0:49
  ) %>%
    as_tibble() %>%
    mutate(ageFraction = 1/50)
}

ice_init <- ice_by_seg %>%
  left_join(age_fracs, by = c("Province", "Segment")) %>%
  mutate(
    ageID = coalesce(ageID, 0L),
    N = ICE_Total * coalesce(ageFraction, 1/50)
  ) %>%
  filter(!is.na(ageID)) %>%
  select(State = Province, Segment, ageID, N)

cat("ICE stock initialized for", length(unique(ice_init$State)), "provinces\n")

# -----------------------------
# 8) Simulation function
# -----------------------------
run_canada_simulation <- function(PR_table, scenario_tag) {
  
  cat("\n========================================\n")
  cat("Running Canada simulation:", scenario_tag, "\n")
  cat("========================================\n")
  
  PR_wide <- make_PR_wide(PR_table)
  
  # Initialize ICE environment
  ice_env <- new.env(parent = emptyenv())
  ice_keys <- ice_init %>% 
    distinct(State, Segment) %>% 
    arrange(State, Segment)
  
  for (i in seq_len(nrow(ice_keys))) {
    k <- ice_keys[i, ]
    vec <- ice_init %>% 
      filter(State == k$State, Segment == k$Segment) %>%
      arrange(ageID) %>% 
      pull(N)
    if (length(vec) < 50) vec <- c(vec, rep(0, 50 - length(vec)))
    ice_env[[paste(k$State, k$Segment, sep = " | ")]] <- vec
  }
  
  # Prepare historical EV data for warm-up
  # canada_ev_historical has: State, Sale Year, Vehicle, Propulsion, Sales
  ev_hist_formatted <- canada_ev_historical %>%
    mutate(
      Segment = case_when(
        Vehicle %in% c("Car", "Passenger Car") ~ "Car",
        Vehicle %in% c("SUV", "Truck", "Light Truck") ~ "SUV",
        TRUE ~ "Car"
      )
    )
  
  # Initialize EV engines with warm-up
  ev_engines <- list()
  for (prov in provinces) {
    for (seg in c("Car", "SUV")) {
      for (pp in c("BEV", "PHEV")) {
        key <- paste(prov, seg, pp, sep = " | ")
        
        # Get historical sales for this province/segment/propulsion
        slice <- ev_hist_formatted %>%
          filter(State == prov, Segment == seg, Propulsion == pp) %>%
          select(`Sale Year`, Sales)
        
        # If no data, create empty slice
        if (nrow(slice) == 0) {
          slice <- tibble(`Sale Year` = 2014:2019, Sales = 0)
        }
        
        ev_engines[[key]] <- EV_engine_init(
          slice,
          segment = seg,
          propulsion = pp,
          lifetime_scen = "Baseline",
          start_year = 2014,
          warmup_last_year = 2019
        )
      }
    }
  }
  
  results_rows <- list()
  evlib_rows <- list()
  
  # Yearly simulation
  for (yr in years) {
    
    # ICE retirement and aging
    ice_retire_df <- map_dfr(seq_len(nrow(ice_keys)), function(i) {
      k <- ice_keys[i, ]
      key <- paste(k$State, k$Segment, sep = " | ")
      N <- ice_env[[key]]
      surv <- surv_tbl_ice[[k$Segment]]
      y <- surv$y
      
      ret <- sum(N * (1 - y), na.rm = TRUE)
      N_new <- c(0, N[-length(N)] * y[-length(y)])
      ice_env[[key]] <- N_new
      
      tibble(State = k$State, Segment = k$Segment, Year = yr, ret_ICE = ret)
    })
    
    # Get growth demand
    grow_now <- growth_seg_base %>% filter(Year == yr)
    
    # PR for this year
    pr_yr <- PR_wide %>% filter(Year == yr)
    
    # Process each state×segment
    for (i in seq_len(nrow(ice_keys))) {
      k <- ice_keys[i, ]
      st <- k$State
      seg <- k$Segment
      
      ret_ice_now <- ice_retire_df %>% 
        filter(State == st, Segment == seg) %>% 
        pull(ret_ICE) %>% 
        as_scalar_num()
      
      growth_now <- grow_now %>% 
        filter(State == st, Segment == seg) %>% 
        pull(Growth_seg) %>% 
        as_scalar_num()
      
      pr_now <- pr_yr %>% filter(State == st)
      bev_pr <- as_scalar_num(pr_now$BEV)
      phev_pr <- as_scalar_num(pr_now$PHEV)
      
      # EV retirement from previous year's stock
      key_bev <- paste(st, seg, "BEV", sep = " | ")
      key_phev <- paste(st, seg, "PHEV", sep = " | ")
      
      # Read-only retirement estimate: use copies to avoid double-aging
      tmp_bev  <- ev_engines[[key_bev]]
      tmp_phev <- ev_engines[[key_phev]]
      step_bev  <- EV_engine_step(tmp_bev,  0)
      step_phev <- EV_engine_step(tmp_phev, 0)
      
      ret_bev  <- step_bev$EV_retired
      ret_phev <- step_phev$EV_retired
      # Note: ev_engines NOT updated here; step2 below does the actual aging
      
      # Total demand
      total_ret <- ret_ice_now + ret_bev + ret_phev
      total_demand <- total_ret + growth_now
      
      if (yr <= 2024) {
        # 2020-2024: use actual sales
        add_bev_actual <- real_ev_ca %>%
          filter(State == st, Segment == seg, Propulsion == "BEV", Year == yr) %>%
          pull(Sales) %>% {if(length(.) == 0) 0 else sum(., na.rm = TRUE)}
        add_phev_actual <- real_ev_ca %>%
          filter(State == st, Segment == seg, Propulsion == "PHEV", Year == yr) %>%
          pull(Sales) %>% {if(length(.) == 0) 0 else sum(., na.rm = TRUE)}
        add_ice_actual <- ice_province_2020_24 %>%
          filter(State == st, Segment == seg, Year == yr) %>%
          pull(ICE_sales) %>% {if(length(.) == 0) 0 else sum(., na.rm = TRUE)}
        
        add_bev  <- add_bev_actual
        add_phev <- add_phev_actual
        add_ice  <- add_ice_actual
        total_demand <- add_bev + add_phev + add_ice
      } else {
        # 2025+: simulation driven by demand × PR
        add_bev <- total_demand * bev_pr
        add_phev <- total_demand * phev_pr
        add_ice <- total_demand * (1 - bev_pr - phev_pr)
      }
      
      # Add new ICE
      ice_key <- paste(st, seg, sep = " | ")
      ice_vec <- ice_env[[ice_key]]
      ice_vec[1] <- add_ice
      ice_env[[ice_key]] <- ice_vec
      
      # Add new EVs
      step2_bev <- EV_engine_step(ev_engines[[key_bev]], as.integer(add_bev))
      step2_phev <- EV_engine_step(ev_engines[[key_phev]], as.integer(add_phev))
      
      ev_engines[[key_bev]] <- step2_bev$engine
      ev_engines[[key_phev]] <- step2_phev$engine
      
      # Store results
      results_rows[[length(results_rows) + 1]] <- tibble(
        State = st, Segment = seg, Year = yr,
        ret_ICE = ret_ice_now,
        ret_BEV = ret_bev,
        ret_PHEV = ret_phev,
        ret_EV = ret_bev + ret_phev,
        Growth_seg = growth_now,
        add_BEV = add_bev,
        add_PHEV = add_phev,
        add_ICE = add_ice,
        Demand_total = total_demand
      )
      
      # EVLIB flows (apply domestic factor)
      for (pp in c("BEV", "PHEV")) {
        key <- paste(st, seg, pp, sep = " | ")
        step <- if (pp == "BEV") step2_bev else step2_phev
        
        lib_recyc <- as.integer(round(step$LIB_recycling_vector * domestic_factor))
        lib_avail <- as.integer(round(step$LIB_available_vector * domestic_factor))
        lib_reuse <- as.integer(round(step$LIB_reuse_vector * domestic_factor))
        lib_repurp <- as.integer(round(step$LIB_repurpose_vector * domestic_factor))
        
        evlib_rows[[length(evlib_rows) + 1]] <- tibble(
          State = st, Segment = seg, Propulsion = pp, Year = yr,
          LIB_recycling = sum(lib_recyc),
          LIB_available = sum(lib_avail),
          LIB_reuse_EV = sum(lib_reuse),
          LIB_repurpose = sum(lib_repurp),
          LIB_new_add = sum(step$LIB_newadd_vector),
          EV_stock = step$EV_stock,
          LIB_recycling_vector = paste(lib_recyc, collapse = "|"),
          LIB_available_vector = paste(lib_avail, collapse = "|"),
          LIB_reuse_vector = paste(lib_reuse, collapse = "|"),
          LIB_repurpose_vector = paste(lib_repurp, collapse = "|"),
          LIB_newadd_vector = paste(as.integer(step$LIB_newadd_vector), collapse = "|"),
          EV_stock_vector = paste(as.integer(step$EV_stock_vector), collapse = "|")
        )
      }
    }
    
    if (yr %% 5 == 0) cat("  Year", yr, "done\n")
  }
  
  # Combine results
  result_df <- bind_rows(results_rows)
  evlib_df <- bind_rows(evlib_rows)
  
  # Save
  write_csv(result_df, 
            paste0("Outputs/Canada/ClosedLoop_AddRetire_byStateSegment_", scenario_tag, ".csv"))
  write_csv(evlib_df, 
            paste0("Outputs/Canada/EVLIB_Flows_detail_", scenario_tag, ".csv"))
  
  # State totals
  result_state <- result_df %>%
    group_by(State, Year) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  write_csv(result_state, 
            paste0("Outputs/Canada/ClosedLoop_StateTotals_", scenario_tag, ".csv"))
  
  # EVLIB totals
  evlib_totals <- evlib_df %>%
    group_by(Year) %>%
    summarise(
      LIB_recycling = sum(LIB_recycling),
      LIB_available = sum(LIB_available),
      LIB_reuse_EV = sum(LIB_reuse_EV),
      LIB_repurpose = sum(LIB_repurpose),
      LIB_new_add = sum(LIB_new_add),
      EV_stock = sum(EV_stock),
      .groups = "drop"
    )
  write_csv(evlib_totals, 
            paste0("Outputs/Canada/EVLIB_Flows_totals_", scenario_tag, ".csv"))
  
  cat("Saved outputs for", scenario_tag, "\n")
  return(result_df)
}

# -----------------------------
# 9) Run both scenarios
# -----------------------------
result_accii <- run_canada_simulation(canada_pr_accii, "ACCII")
result_repeal <- run_canada_simulation(canada_pr_repeal, "Repeal")

# -----------------------------
# 10) Summary
# -----------------------------
cat("\n========================================\n")
cat("=== Canada Simulation Complete! ===\n")
cat("========================================\n")

cat("\nACCII Scenario - 2050 Summary:\n")
print(result_accii %>%
        filter(Year == 2050) %>%
        summarise(
          Total_BEV = sum(add_BEV),
          Total_PHEV = sum(add_PHEV),
          Total_ICE = sum(add_ICE),
          Total = Total_BEV + Total_PHEV + Total_ICE,
          EV_Share = (Total_BEV + Total_PHEV) / Total
        ))

cat("\nRepeal Scenario - 2050 Summary:\n")
print(result_repeal %>%
        filter(Year == 2050) %>%
        summarise(
          Total_BEV = sum(add_BEV),
          Total_PHEV = sum(add_PHEV),
          Total_ICE = sum(add_ICE),
          Total = Total_BEV + Total_PHEV + Total_ICE,
          EV_Share = (Total_BEV + Total_PHEV) / Total
        ))

cat("\nOutputs saved to Outputs/Canada/\n")
