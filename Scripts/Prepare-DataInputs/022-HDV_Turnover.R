## 022 — HDV (Heavy Duty Vehicle) EV Fleet Turnover
## Medium trucks + Heavy trucks for US (by state), Canada (by province), Mexico
## Sales distributed to states/provinces by population share
## Includes BESS simulation for repurposed batteries
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

# BESS parameters (same as LDV 031BESS.R)
BESS_LIB_MEAN    <- 15
BESS_LIB_SD      <- 4
BESS_MAX_LIB_AGE <- 30
BESS_AGE_BINS    <- 0:BESS_MAX_LIB_AGE

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
ca_pop_raw <- read_csv("Parameters/CanadaPopulation.csv", show_col_types = FALSE) %>%
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
    inner_join(pop_share_df, by = "Year") %>%
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

# =====================================================================
# 7) BESS Simulation for HDV repurposed batteries
# =====================================================================
cat("\n=== Running HDV BESS Simulation ===\n")

BESS_SIM_YEARS <- SIM_START:SIM_END

bess_survival <- function(age, mean_val, sd_val) {
  scale_val <- sd_val * sqrt(3) / pi
  1 - plogis(pmax(age, 0), location = mean_val, scale = scale_val)
}

bess_parse_pipe <- function(s) {
  if (is.null(s) || length(s) == 0 || is.na(s)) return(numeric(0))
  s <- as.character(s)
  if (nchar(s) == 0) return(numeric(0))
  parts <- trimws(strsplit(s, "\\|")[[1]])
  parts[parts == "" | is.na(parts) | toupper(parts) == "NA"] <- "0"
  v <- suppressWarnings(as.numeric(parts))
  v[!is.finite(v)] <- 0
  v
}

bess_sum_vec <- function(a, b) {
  a <- as.numeric(a); b <- as.numeric(b)
  a[!is.finite(a)] <- 0; b[!is.finite(b)] <- 0
  len <- max(length(a), length(b))
  if (len == 0) return(numeric(0))
  c(a, rep(0, len - length(a))) + c(b, rep(0, len - length(b)))
}

bess_simulate_one_group <- function(df_g) {
  df_year <- df_g %>%
    mutate(vec = map(LIB_repurpose_vector, bess_parse_pipe)) %>%
    group_by(Year) %>%
    summarise(vec_sum = list(reduce(vec, bess_sum_vec)), .groups = "drop") %>%
    arrange(Year)

  # Build inflow table (Year, Vintage, Inflow)
  df_inflow <- pmap_dfr(list(df_year$Year, df_year$vec_sum), function(y, v) {
    if (length(v) == 0) return(tibble(Year = integer(), Vintage = integer(), Inflow = double()))
    age <- 0:(length(v) - 1)
    keep <- is.finite(v) & v > 0
    if (!any(keep)) return(tibble(Year = integer(), Vintage = integer(), Inflow = double()))
    tibble(Year = as.integer(y), Vintage = as.integer(y - age[keep]), Inflow = v[keep])
  })
  if (nrow(df_inflow) == 0) return(list(stock = tibble(), retire = tibble()))

  all_vintages <- min(df_inflow$Vintage, na.rm = TRUE):SIM_END
  n_v <- length(all_vintages)
  n_y <- length(BESS_SIM_YEARS)

  stock_mat  <- matrix(0, nrow = n_v, ncol = n_y, dimnames = list(all_vintages, BESS_SIM_YEARS))
  retire_mat <- matrix(0, nrow = n_v, ncol = n_y, dimnames = list(all_vintages, BESS_SIM_YEARS))

  for (j in seq_len(n_y)) {
    y <- BESS_SIM_YEARS[j]
    ages_end   <- y - all_vintages
    ages_start <- ages_end - 1

    inflow_vec <- numeric(n_v)
    yr_inf <- df_inflow %>% filter(Year == y)
    if (nrow(yr_inf) > 0) {
      idx <- match(yr_inf$Vintage, all_vintages)
      ok  <- !is.na(idx)
      inflow_vec[idx[ok]] <- yr_inf$Inflow[ok]
    }

    stock_start <- if (j == 1) inflow_vec else (stock_mat[, j - 1] + inflow_vec)
    prob_end   <- bess_survival(ages_end,   BESS_LIB_MEAN, BESS_LIB_SD)
    prob_start <- bess_survival(ages_start, BESS_LIB_MEAN, BESS_LIB_SD)
    decay <- prob_end / prob_start
    decay[is.na(decay) | is.infinite(decay) | prob_start <= 0] <- 0
    decay[ages_start >= BESS_MAX_LIB_AGE] <- 0

    stock_end <- stock_start * decay
    stock_mat[, j]  <- stock_end
    retire_mat[, j] <- pmax(stock_start - stock_end, 0)
  }

  # Collapse to age-bin vectors
  make_vecs <- function(mat_data, val_col) {
    lapply(seq_len(n_y), function(j) {
      y <- BESS_SIM_YEARS[j]
      v_age <- pmin(y - all_vintages, BESS_MAX_LIB_AGE)
      v_val <- as.numeric(mat_data[, j])
      tmp <- tibble(Age = v_age, Val = v_val) %>%
        filter(Age >= 0, Age <= BESS_MAX_LIB_AGE, Val > 0) %>%
        group_by(Age) %>% summarise(Val = sum(Val), .groups = "drop")
      age_vec <- numeric(length(BESS_AGE_BINS))
      if (nrow(tmp) > 0) age_vec[tmp$Age + 1] <- tmp$Val
      as.integer(round(pmax(age_vec, 0)))
    })
  }

  stock_vecs  <- make_vecs(stock_mat, "stock")
  retire_vecs <- make_vecs(retire_mat, "retire")

  list(
    stock = tibble(
      Year = BESS_SIM_YEARS,
      BESS_stock_vector = sapply(stock_vecs, \(v) paste(v, collapse = "|")),
      BESS_stock_total  = sapply(stock_vecs, sum)
    ),
    retire = tibble(
      Year = BESS_SIM_YEARS,
      BESS_retire_vector = sapply(retire_vecs, \(v) paste(v, collapse = "|")),
      BESS_retire_total  = sapply(retire_vecs, sum)
    )
  )
}

zero_stock_vec  <- paste(rep(0, length(BESS_AGE_BINS)), collapse = "|")
zero_retire_vec <- paste(rep(0, length(BESS_AGE_BINS)), collapse = "|")

for (sc in scenarios) {
  cat("\n--- HDV BESS:", sc, "---\n")

  sc_data <- hdv_results %>%
    filter(Scenario == sc) %>%
    select(Country, State, Vehicle, Year, LIB_repurpose_vector)

  bess_groups <- sc_data %>%
    group_by(Country, State, Vehicle) %>%
    group_split()

  all_stock <- vector("list", length(bess_groups))
  all_ret   <- vector("list", length(bess_groups))

  for (i in seq_along(bess_groups)) {
    g <- bess_groups[[i]]
    keys <- g %>% distinct(Country, State, Vehicle) %>% slice(1)
    sim  <- bess_simulate_one_group(g)

    out_s <- sim$stock
    out_r <- sim$retire
    if (nrow(out_s) == 0) out_s <- tibble(Year = BESS_SIM_YEARS, BESS_stock_vector = zero_stock_vec, BESS_stock_total = 0L)
    if (nrow(out_r) == 0) out_r <- tibble(Year = BESS_SIM_YEARS, BESS_retire_vector = zero_retire_vec, BESS_retire_total = 0L)

    all_stock[[i]] <- bind_cols(keys[rep(1, nrow(out_s)), , drop = FALSE], out_s)
    all_ret[[i]]   <- bind_cols(keys[rep(1, nrow(out_r)), , drop = FALSE], out_r)

    if (i %% 100 == 0) cat("  BESS processed", i, "/", length(bess_groups), "\n")
  }

  df_stock_all <- bind_rows(all_stock)
  df_ret_all   <- bind_rows(all_ret)

  write_csv(df_stock_all, paste0("Outputs/HDV/HDV_BESS_Stock_", sc, ".csv"))
  write_csv(df_ret_all,   paste0("Outputs/HDV/HDV_BESS_Retire_", sc, ".csv"))
  cat("  Saved: Outputs/HDV/HDV_BESS_Stock_", sc, ".csv\n")
  cat("  Saved: Outputs/HDV/HDV_BESS_Retire_", sc, ".csv\n")
}

cat("\n=== HDV BESS Summary (2030, 2040, 2050) ===\n")
for (sc in scenarios) {
  d <- read_csv(paste0("Outputs/HDV/HDV_BESS_Retire_", sc, ".csv"), show_col_types = FALSE) %>%
    filter(Year %in% c(2030, 2040, 2050)) %>%
    group_by(Country, Year) %>%
    summarise(BESS_retire = sum(BESS_retire_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = sc)
  print(d)
}

cat("\n=== All HDV processing complete! ===\n")
cat("EV params: mean_ev=", GLOBAL_hdv_mean_ev, " mean_lib=", GLOBAL_hdv_mean_lib,
    " sd=", GLOBAL_hdv_sd, " max_age=", GLOBAL_hdv_max_age, "\n")
cat("BESS params: mean=", BESS_LIB_MEAN, " sd=", BESS_LIB_SD, " max_age=", BESS_MAX_LIB_AGE, "\n")
