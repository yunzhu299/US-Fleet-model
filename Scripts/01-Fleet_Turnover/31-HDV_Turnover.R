## 031 — HDV (Heavy Duty Vehicle) EV Fleet Turnover
## Medium trucks + Heavy trucks for US (by state), Canada (by province), Mexico
## Sales distributed to states/provinces by population share
## BESS processing is handled centrally by 41-BESS_Second_Life.R
## YZC Feb 2026

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)

# =====================================================================
# 0) Global parameters
# =====================================================================
GLOBAL_hdv_mean_ev  <- 16
GLOBAL_hdv_mean_lib <- 8
GLOBAL_hdv_sd       <- 4
GLOBAL_hdv_max_age  <- 30

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

SIM_START <- 2022
SIM_END   <- 2050

# =====================================================================
# 1) Read ICCT data
# =====================================================================
cat("=== Reading ICCT data ===\n")
icct_raw <- read_excel("Inputs/ICCT.xlsx", sheet = 1)

icct_hdv <- icct_raw %>%
  filter(Country %in% c("Mexico", "Canada", "United States")) %>%
  filter(Vehicle %in% c("Medium trucks", "Heavy trucks")) %>%
  filter(Powertrain == "BEV") %>%
  mutate(
    Scenario = case_when(
      Scenario == "Baseline 2024" ~ "Repeal",
      Scenario == "Momentum"      ~ "ACCII",
      TRUE ~ Scenario
    ),
    Year  = as.integer(CY),
    Sales = as.numeric(Sales)
  ) %>%
  filter(Scenario %in% c("ACCII", "Repeal"), !is.na(Sales)) %>%
  group_by(Country, Year, Scenario, Vehicle) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE), .groups = "drop")

# =====================================================================
# 2) Load population data and compute state shares
# =====================================================================
cat("=== Loading population data ===\n")

# --- US population ---
us_pop_raw <- read_excel(
  "~/Library/CloudStorage/GoogleDrive-yuzchen@ucdavis.edu/My Drive/US Fleet modeling/NationalProjections_ProjectedTotalPopulation_2030-2050.xlsx",
  range = "A3:F56"
) %>%
  rename(State = `Geography Name`) %>%
  filter(!is.na(State), State != "United States") %>%
  pivot_longer(cols = matches("^20\\d{2}$"), names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year)) %>%
  group_by(State) %>%
  complete(Year = full_seq(c(2020, 2050), 1)) %>%
  arrange(State, Year) %>%
  mutate(Population = approx(
    x = Year[!is.na(Population)], y = Population[!is.na(Population)],
    xout = Year, method = "linear", rule = 2
  )$y) %>%
  ungroup() %>%
  filter(Year >= SIM_START, Year <= SIM_END)

us_pop_total <- us_pop_raw %>%
  group_by(Year) %>%
  summarise(Pop_total = sum(Population, na.rm = TRUE), .groups = "drop")

us_pop_share <- us_pop_raw %>%
  left_join(us_pop_total, by = "Year") %>%
  mutate(pop_share = Population / Pop_total) %>%
  select(State, Year, pop_share)

cat("  US states:", length(unique(us_pop_share$State)), "\n")

# --- Canada population ---
ca_pop_raw <- read_csv("Inputs/Parameters/CanadaPopulation.csv", show_col_types = FALSE) %>%
  rename(State = Province) %>%
  filter(!State %in% c("Geography 2")) %>%
  filter(Year >= SIM_START, Year <= SIM_END)

# For years before 2025 (pop data starts 2025), use 2025 values
ca_min_pop_year <- min(ca_pop_raw$Year)
if (ca_min_pop_year > SIM_START) {
  ca_first_year <- ca_pop_raw %>% filter(Year == ca_min_pop_year)
  for (y in SIM_START:(ca_min_pop_year - 1)) {
    ca_pop_raw <- bind_rows(ca_pop_raw, ca_first_year %>% mutate(Year = y))
  }
}

ca_pop_total <- ca_pop_raw %>%
  group_by(Year) %>%
  summarise(Pop_total = sum(Population, na.rm = TRUE), .groups = "drop")

ca_pop_share <- ca_pop_raw %>%
  left_join(ca_pop_total, by = "Year") %>%
  mutate(pop_share = Population / Pop_total) %>%
  select(State, Year, pop_share)

cat("  Canada provinces:", length(unique(ca_pop_share$State)), "\n")

# =====================================================================
# 3) Distribute country-level sales to states/provinces
# =====================================================================
cat("=== Distributing sales by population share ===\n")

distribute_sales <- function(country_name, pop_share_df, icct_data) {
  country_sales <- icct_data %>% filter(Country == country_name)
  if (nrow(country_sales) == 0) return(tibble())

  all_years <- SIM_START:SIM_END
  max_icct_year <- max(country_sales$Year)

  # Extend to SIM_END using last year's sales
  if (max_icct_year < SIM_END) {
    last_yr_data <- country_sales %>% filter(Year == max_icct_year)
    for (y in (max_icct_year + 1):SIM_END) {
      country_sales <- bind_rows(country_sales, last_yr_data %>% mutate(Year = y))
    }
  }

  country_sales %>%
    # Intentional expansion: each country-year/scenario/vehicle sales row is
    # distributed across every state/province row for that year.
    inner_join(pop_share_df, by = "Year", relationship = "many-to-many") %>%
    mutate(State_Sales = Total_Sales * pop_share) %>%
    select(State, Year, Scenario, Vehicle, State_Sales)
}

us_state_sales <- distribute_sales("United States", us_pop_share, icct_hdv)
ca_state_sales <- distribute_sales("Canada", ca_pop_share, icct_hdv)

mx_sales <- icct_hdv %>%
  filter(Country == "Mexico") %>%
  mutate(State = "Mexico") %>%
  select(State, Year, Scenario, Vehicle, State_Sales = Total_Sales)

max_mx_year <- max(mx_sales$Year)
if (max_mx_year < SIM_END) {
  last_mx <- mx_sales %>% filter(Year == max_mx_year)
  for (y in (max_mx_year + 1):SIM_END) {
    mx_sales <- bind_rows(mx_sales, last_mx %>% mutate(Year = y))
  }
}

all_state_sales <- bind_rows(
  us_state_sales %>% mutate(Country = "United States"),
  ca_state_sales %>% mutate(Country = "Canada"),
  mx_sales       %>% mutate(Country = "Mexico")
)

cat("  Total rows:", nrow(all_state_sales), "\n")
cat("  US states with sales:", length(unique(us_state_sales$State)), "\n")
cat("  CA provinces with sales:", length(unique(ca_state_sales$State)), "\n")

# =====================================================================
# 4) EV turnover engine (same as before)
# =====================================================================
f.getOutflows_HDV <- function(n_veh = 1, EV_age, LIB_age,
                               maxEV_age  = GLOBAL_hdv_max_age,
                               maxLIB_age = GLOBAL_hdv_max_age,
                               mean_ev    = GLOBAL_hdv_mean_ev,
                               sd_ev      = GLOBAL_hdv_sd,
                               mean_lib   = GLOBAL_hdv_mean_lib,
                               sd_lib     = GLOBAL_hdv_sd) {
  sdev <- sd_ev  * sqrt(3) / pi
  sdli <- sd_lib * sqrt(3) / pi
  y1 <- (1 - plogis(EV_age + 1, mean_ev, sdev)) / (1 - plogis(EV_age, mean_ev, sdev))
  y2 <- (1 - plogis(LIB_age + 1, mean_lib, sdli)) / (1 - plogis(LIB_age, mean_lib, sdli))
  if (EV_age  >= maxEV_age)  y1 <- 0
  if (LIB_age >= maxLIB_age) y2 <- 0
  tibble(both_fail = (1 - y1) * (1 - y2) * n_veh,
         ev_fail   = (1 - y1) *  y2       * n_veh,
         lib_fail  =  y1      * (1 - y2)  * n_veh,
         none      =  y1      *  y2       * n_veh)
}

HDV_engine_init <- function(max_age = GLOBAL_hdv_max_age) {
  mat <- matrix(0, nrow = max_age + 1, ncol = max_age + 1,
                dimnames = list(paste0("EV_", 0:max_age), paste0("LIB_", 0:max_age)))
  list(matrix = mat)
}

HDV_engine_step <- function(engine, sales_y = 0) {
  mat     <- engine$matrix
  max_age <- GLOBAL_hdv_max_age
  new_matrix  <- matrix(0, nrow = max_age + 1, ncol = max_age + 1)
  matrix_ev   <- new_matrix
  matrix_lib  <- new_matrix
  matrix_both <- new_matrix

  for (ev_a in 0:max_age) {
    for (lib_a in 0:max_age) {
      N <- mat[ev_a + 1, lib_a + 1]
      if (N < 0.5) next
      if (ev_a == 0 && lib_a == 0) {
        new_matrix[2, 2] <- new_matrix[2, 2] + N
        next
      }
      out <- f.getOutflows_HDV(N, ev_a, lib_a)
      new_ev  <- min(ev_a + 1, max_age)
      new_lib <- min(lib_a + 1, max_age)
      new_matrix[new_ev + 1, new_lib + 1]  <- new_matrix[new_ev + 1, new_lib + 1]  + out$none
      matrix_ev[new_ev + 1, lib_a + 1]     <- matrix_ev[new_ev + 1, lib_a + 1]     + out$ev_fail
      matrix_lib[ev_a + 1, new_lib + 1]    <- matrix_lib[ev_a + 1, new_lib + 1]    + out$lib_fail
      matrix_both[ev_a + 1, lib_a + 1]     <- matrix_both[ev_a + 1, lib_a + 1]     + out$both_fail
    }
  }

  lib_evfail_total_vec <- as.integer(round(colSums(matrix_ev)))
  lib_failed_only_vec  <- as.integer(round(rowSums(matrix_lib)))
  lib_bothfail_vec     <- as.integer(round(colSums(matrix_both)))
  evfail_total         <- sum(matrix_ev) + sum(matrix_both)

  lib_evfail_recycle_vec <- as.integer(round(lib_evfail_total_vec * GLOBAL_recycle_share_evfail))
  lib_evfail_repurp_vec  <- as.integer(round(lib_evfail_total_vec * GLOBAL_repurpose_share_evfail))
  lib_evfail_reuse_vec   <- lib_evfail_total_vec - lib_evfail_recycle_vec - lib_evfail_repurp_vec

  lib_fail_recycle_vec <- as.integer(round(lib_failed_only_vec * GLOBAL_recycle_share_libfail))
  lib_fail_repurp_vec  <- lib_failed_only_vec - lib_fail_recycle_vec

  lib_recycling_vec <- as.integer(lib_evfail_recycle_vec + lib_fail_recycle_vec + lib_bothfail_vec)
  lib_repurpose_vec <- as.integer(lib_evfail_repurp_vec + lib_fail_repurp_vec)
  lib_reuse_ev_vec  <- as.integer(round(lib_evfail_reuse_vec * 1.0))

  new_matrix[1, 1] <- new_matrix[1, 1] + sales_y

  list(
    matrix    = new_matrix,
    evfail    = evfail_total,
    lib_flows = list(
      lib_recycling_vec    = lib_recycling_vec,
      lib_repurpose_vec    = lib_repurpose_vec,
      lib_reuse_ev_vec     = lib_reuse_ev_vec,
      lib_available_vec    = lib_evfail_total_vec,
      lib_evfail_total_vec = lib_evfail_total_vec,
      lib_bothfail_vec     = lib_bothfail_vec
    ),
    total_stock = sum(new_matrix)
  )
}

# =====================================================================
# 5) Run simulation for each State × Scenario × Vehicle
# =====================================================================
run_one_group <- function(sales_df) {
  engine <- HDV_engine_init()
  years  <- sort(unique(sales_df$Year))
  results <- vector("list", length(years))

  for (i in seq_along(years)) {
    yr <- years[i]
    s  <- sales_df %>% filter(Year == yr) %>% pull(State_Sales) %>% sum(na.rm = TRUE)
    engine <- HDV_engine_step(engine, sales_y = s)
    results[[i]] <- tibble(
      Year             = yr,
      New_Sales        = s,
      Total_Stock      = engine$total_stock,
      EV_Retirement    = engine$evfail,
      LIB_recycling_vector    = paste(engine$lib_flows$lib_recycling_vec, collapse = "|"),
      LIB_repurpose_vector    = paste(engine$lib_flows$lib_repurpose_vec, collapse = "|"),
      LIB_reuse_ev_vector     = paste(engine$lib_flows$lib_reuse_ev_vec, collapse = "|"),
      LIB_available_vector    = paste(engine$lib_flows$lib_available_vec, collapse = "|"),
      LIB_evfail_total_vector = paste(engine$lib_flows$lib_evfail_total_vec, collapse = "|"),
      LIB_bothfail_vector     = paste(engine$lib_flows$lib_bothfail_vec, collapse = "|"),
      LIB_recycling  = sum(engine$lib_flows$lib_recycling_vec),
      LIB_repurpose  = sum(engine$lib_flows$lib_repurpose_vec),
      LIB_reuse_EV   = sum(engine$lib_flows$lib_reuse_ev_vec)
    )
  }
  bind_rows(results)
}

cat("\n=== Running state-level HDV simulations ===\n")

groups <- all_state_sales %>%
  group_by(Country, State, Scenario, Vehicle) %>%
  group_split()

n_groups <- length(groups)
cat("  Total groups:", n_groups, "\n")

all_results <- vector("list", n_groups)
for (i in seq_along(groups)) {
  g <- groups[[i]]
  keys <- g %>% distinct(Country, State, Scenario, Vehicle) %>% slice(1)
  sim  <- run_one_group(g)
  all_results[[i]] <- bind_cols(keys[rep(1, nrow(sim)), , drop = FALSE], sim)
  if (i %% 100 == 0) cat("  Processed", i, "/", n_groups, "groups\n")
}

hdv_results <- bind_rows(all_results)
cat("  Done. Total rows:", nrow(hdv_results), "\n")

# =====================================================================
# 6) Save turnover outputs
# =====================================================================
dir.create("Outputs/HDV", showWarnings = FALSE, recursive = TRUE)

scenarios <- c("ACCII", "Repeal")
for (sc in scenarios) {
  sc_data <- hdv_results %>% filter(Scenario == sc)
  write_csv(sc_data, paste0("Outputs/HDV/HDV_EV_Turnover_", sc, ".csv"))
  cat("Saved: Outputs/HDV/HDV_EV_Turnover_", sc, ".csv\n")
}

cat("\n=== HDV EV Turnover Summary (2030, 2040, 2050) ===\n")
hdv_results %>%
  filter(Year %in% c(2030, 2040, 2050)) %>%
  group_by(Country, Scenario, Vehicle, Year) %>%
  summarise(
    New_Sales     = sum(New_Sales, na.rm = TRUE),
    Total_Stock   = sum(Total_Stock, na.rm = TRUE),
    LIB_recycling = sum(LIB_recycling, na.rm = TRUE),
    LIB_repurpose = sum(LIB_repurpose, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Country, Vehicle, Scenario, Year) %>%
  print(n = 40)


cat("\n=== HDV turnover complete; run 41-BESS_Second_Life.R next. ===\n")
