## 011 — Canada Fleet Simulation (2020–2050) with Full EV Battery Tracking
## Based on US 005-Fleetsimulation.R, same EV engine logic
## Uses Canada data prepared in 010-CanadaPopulation.R
## YZC Dec 2025

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(readr)
library(readxl)

# -----------------------------
# 0) Global settings (same as US)
# -----------------------------
years <- 2020:2050

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

life_param <- tibble(
  Vehicle  = c("Car","SUV"),
  mean_ev  = c(17,17),
  sd_ev    = c(4,4),
  mean_lib = c(15,10),
  sd_lib   = c(4,4),
  scen_lifetime = "Baseline"
)

as_scalar_num <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- suppressWarnings(as.numeric(x))
  if (length(x) > 1) x <- sum(x, na.rm = TRUE)
  if (!is.finite(x)) x <- default
  x
}

dir.create("Outputs/Canada", showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 1) EV Engine Functions (same as 004)
# -----------------------------
f.getOutflows <- function(n_veh = 1, EV_age, LIB_age,
                          maxEV_age = 30, maxLIB_age = 30,
                          dist.Age  = "Logistic",
                          mean_ev   = 17, sd_ev   = 4,
                          mean_lib  = 15, sd_lib  = 4) {
  if (dist.Age == "Normal") {
    y1 <- (1 - pnorm(EV_age+1, mean_ev,  sd_ev)) / (1 - pnorm(EV_age, mean_ev, sd_ev))
    y2 <- (1 - pnorm(LIB_age+1, mean_lib, sd_lib)) / (1 - pnorm(LIB_age, mean_lib, sd_lib))
  } else {
    sdev <- sd_ev  * sqrt(3)/pi
    sdli <- sd_lib * sqrt(3)/pi
    y1 <- (1 - plogis(EV_age+1, mean_ev, sdev)) / (1 - plogis(EV_age, mean_ev, sdev))
    y2 <- (1 - plogis(LIB_age+1, mean_lib, sdli)) / (1 - plogis(LIB_age, mean_lib, sdli))
  }
  if (EV_age  >= maxEV_age)  y1 <- 0
  if (LIB_age >= maxLIB_age) y2 <- 0
  
  tibble(
    both_fail = (1-y1) * (1-y2) * n_veh,
    ev_fail   = (1-y1) *  y2   * n_veh,
    lib_fail  =  y1   * (1-y2) * n_veh,
    none      =  y1   *  y2   * n_veh
  )
}

EV_engine_step <- function(engine, sales_y = 0, reuse_shrink = 1.0) {
  mat <- engine$matrix
  new_matrix <- matrix(0, nrow = 31, ncol = 31,
                       dimnames = list(paste0("EV_",0:30), paste0("LIB_",0:30)))
  matrix_ev   <- new_matrix
  matrix_lib  <- new_matrix
  matrix_both <- new_matrix
  
  for (i in 1:31) {
    for (j in 1:31) {
      if (mat[i,j] != 0) {
        res <- f.getOutflows(
          n_veh = mat[i,j], EV_age = i-1, LIB_age = j-1,
          maxEV_age = 30, maxLIB_age = 30, dist.Age = "Logistic",
          mean_ev = engine$mean_ev, sd_ev = engine$sd_ev,
          mean_lib = engine$mean_lib, sd_lib = engine$sd_lib
        )
        if (i != 31 & j != 31) {
          new_matrix[i+1, j+1] <- new_matrix[i+1, j+1] + res$none
          matrix_ev[i+1, j+1]  <- matrix_ev[i+1, j+1] + res$lib_fail
          matrix_lib[i+1, j+1] <- matrix_lib[i+1, j+1] + res$ev_fail
          matrix_both[i+1, j+1] <- matrix_both[i+1, j+1] + res$both_fail
        } else if (j == 31 & i != 31) {
          matrix_ev[i+1, j] <- matrix_ev[i+1, j] + res$lib_fail
        } else if (i == 31 & j != 31) {
          matrix_lib[i, j+1] <- matrix_lib[i, j+1] + res$ev_fail
        }
      }
    }
  }
  
  ev_need_vec_raw <- as.integer(round(rowSums(matrix_ev)))
  ev_retired_vec  <- as.integer(round(rowSums(matrix_lib) + rowSums(matrix_both)))
  
  if (GLOBAL_max_ev_age < 30) ev_need_vec_raw[(GLOBAL_max_ev_age+1):31] <- 0L
  
  lib_failed_only_vec <- as.integer(round(colSums(matrix_ev)[-1]))
  lib_bothfail_vec    <- as.integer(round(colSums(matrix_both)[-1]))
  lib_evfail_total_vec <- as.integer(round(colSums(matrix_lib)))
  
  ages_lib_1_30 <- 1:30
  ages_lib_0_30 <- 0:30
  
  lib_fail_repurpose_vec <- as.integer(round(lib_failed_only_vec * GLOBAL_repurpose_share_libfail))
  lib_fail_recycle_vec   <- lib_failed_only_vec - lib_fail_repurpose_vec
  if (GLOBAL_max_lib_age_repurpose < 30) {
    too_old_idx <- which(ages_lib_1_30 > GLOBAL_max_lib_age_repurpose)
    if (length(too_old_idx) > 0) {
      lib_fail_recycle_vec[too_old_idx] <- lib_fail_recycle_vec[too_old_idx] + lib_fail_repurpose_vec[too_old_idx]
      lib_fail_repurpose_vec[too_old_idx] <- 0L
    }
  }
  
  lib_to_EV_vec <- as.integer(round(lib_evfail_total_vec * GLOBAL_reuse_share_evfail))
  eligible_reuse <- ages_lib_0_30 <= GLOBAL_max_lib_age_ev
  if (GLOBAL_max_lib_age_ev < 30) lib_to_EV_vec[!eligible_reuse] <- 0L
  lib_to_EV_init_vec <- lib_to_EV_vec
  
  ev_need_ext   <- c(ev_need_vec_raw, rep(0L, GLOBAL_ev_age_newLib))
  lib_to_EV_ext <- c(rep(0L, GLOBAL_ev_age_newLib), lib_to_EV_vec)
  allocation    <- pmin(ev_need_ext, lib_to_EV_ext)
  
  reuse_offset_vec <- allocation[-(1:GLOBAL_ev_age_newLib)]
  if (length(reuse_offset_vec) < 30) reuse_offset_vec <- c(reuse_offset_vec, rep(0L, 30 - length(reuse_offset_vec)))
  
  ev_need_ext   <- ev_need_ext - allocation
  lib_to_EV_ext <- lib_to_EV_ext - allocation
  ev_need_vec   <- ev_need_ext[1:31]
  lib_to_EV_vec <- lib_to_EV_ext[-(1:GLOBAL_ev_age_newLib)]
  
  lib_to_EV_before <- lib_to_EV_vec
  start_bat <- 1
  for (i in 31:1) {
    if (i > GLOBAL_ev_age_newLib) {
      for (j in start_bat:31) {
        allocated <- min(ev_need_vec[i], lib_to_EV_vec[j])
        if (allocated > 0L) {
          ev_need_vec[i]   <- ev_need_vec[i] - allocated
          lib_to_EV_vec[j] <- lib_to_EV_vec[j] - allocated
          new_matrix[i,j]  <- new_matrix[i,j] + allocated
          start_bat <- j
        }
        if (ev_need_vec[i] == 0L) break
      }
    }
  }
  lib_to_EV_after <- lib_to_EV_vec
  
  reuse_loop_vec <- (lib_to_EV_before - lib_to_EV_after)[-1]
  if (length(reuse_loop_vec) < 30) reuse_loop_vec <- c(reuse_loop_vec, rep(0L, 30 - length(reuse_loop_vec)))
  
  LIB_reuse_vector <- as.integer(reuse_offset_vec + reuse_loop_vec)
  LIB_reuse_EV <- sum(LIB_reuse_vector)
  lib_reuse_byLIB_vec <- lib_to_EV_init_vec - lib_to_EV_after
  
  LIB_newadd_vector <- as.integer(ev_need_vec)
  LIB_new_add <- sum(LIB_newadd_vector)
  
  reuse_shrink <- max(0, min(1, reuse_shrink))
  if (reuse_shrink < 1) {
    lost_reuse_vec <- as.integer(round((1 - reuse_shrink) * LIB_reuse_vector))
    if (sum(lost_reuse_vec) > 0) {
      LIB_reuse_vector  <- LIB_reuse_vector - lost_reuse_vec
      LIB_reuse_EV      <- sum(LIB_reuse_vector)
      LIB_newadd_vector <- LIB_newadd_vector + lost_reuse_vec
      LIB_new_add       <- sum(LIB_newadd_vector)
      new_matrix[,1]    <- new_matrix[,1] + lost_reuse_vec
    }
  }
  
  remaining_after_reuse_evfail <- lib_evfail_total_vec - lib_reuse_byLIB_vec
  remaining_after_reuse_evfail[remaining_after_reuse_evfail < 0] <- 0L
  
  repurpose_target_evfail_vec <- as.integer(round(lib_evfail_total_vec * GLOBAL_repurpose_share_evfail))
  eligible_rep_ev <- ages_lib_0_30 <= GLOBAL_max_lib_age_repurpose
  if (GLOBAL_max_lib_age_repurpose < 30) repurpose_target_evfail_vec[!eligible_rep_ev] <- 0L
  
  lib_evfail_repurpose_vec <- pmin(remaining_after_reuse_evfail, repurpose_target_evfail_vec)
  lib_evfail_recycle_vec   <- remaining_after_reuse_evfail - lib_evfail_repurpose_vec
  
  pad31 <- function(v30) {
    v31 <- integer(31)
    v31[2:31] <- as.integer(v30)
    v31
  }
  
  lib_fail_repurpose_vec31 <- pad31(lib_fail_repurpose_vec)
  LIB_repurpose_vector <- as.integer(lib_evfail_repurpose_vec + lib_fail_repurpose_vec31)
  LIB_repurpose <- sum(LIB_repurpose_vector)
  
  lib_fail_recycle_vec31 <- pad31(lib_fail_recycle_vec)
  lib_bothfail_vec31     <- pad31(lib_bothfail_vec)
  lib_recycling_vec <- as.integer(lib_evfail_recycle_vec + lib_fail_recycle_vec31 + lib_bothfail_vec31)
  LIB_recycling <- sum(lib_recycling_vec)
  
  LIB_available_vector <- LIB_repurpose_vector
  LIB_available <- LIB_repurpose
  
  new_matrix[,1] <- new_matrix[,1] + as.integer(LIB_newadd_vector)
  new_matrix[1,1] <- new_matrix[1,1] + as.integer(round(sales_y))
  
  EV_retired <- sum(ev_retired_vec)
  engine$matrix <- new_matrix
  
  list(
    engine = engine,
    EV_retired = EV_retired,
    LIB_recycling_vector = lib_recycling_vec,
    LIB_recycling = LIB_recycling,
    LIB_available_vector = LIB_available_vector,
    LIB_available = LIB_available,
    LIB_reuse_vector = LIB_reuse_vector,
    LIB_reuse_EV = LIB_reuse_EV,
    LIB_repurpose_vector = LIB_repurpose_vector,
    LIB_repurpose = LIB_repurpose,
    LIB_new_add = LIB_new_add,
    LIB_newadd_vector = LIB_newadd_vector,
    EV_stock_vector = as.integer(round(rowSums(new_matrix))),
    EV_stock = as.integer(round(sum(new_matrix)))
  )
}

EV_engine_init <- function(ev_hist_slice, segment, propulsion,
                           lifetime_scen = "Baseline",
                           start_year = 2014,
                           warmup_last_year = 2019) {
  param <- life_param %>% filter(scen_lifetime == lifetime_scen, Vehicle == segment)
  mean_ev  <- param$mean_ev
  sd_ev    <- param$sd_ev
  mean_lib <- param$mean_lib
  sd_lib   <- param$sd_lib
  
  mat <- matrix(0, nrow = 31, ncol = 31,
                dimnames = list(paste0("EV_",0:30), paste0("LIB_",0:30)))
  
  engine <- list(
    matrix = mat,
    mean_ev = mean_ev, sd_ev = sd_ev,
    mean_lib = mean_lib, sd_lib = sd_lib,
    scen = lifetime_scen,
    segment = segment,
    propulsion = propulsion
  )
  
  for (y in start_year:warmup_last_year) {
    sales_y <- ev_hist_slice %>% filter(Year == y) %>% pull(Sales)
    if (length(sales_y) == 0) sales_y <- 0
    step <- EV_engine_step(engine, sales_y = sales_y, reuse_shrink = 1.0)
    engine <- step$engine
  }
  
  engine
}

# -----------------------------
# 2) Load Canada data files
# -----------------------------
cat("Loading Canada data files...\n")

canada_pop <- read_csv("Parameters/CanadaPopulation.csv", show_col_types = FALSE)
canada_type_share <- read_csv("Parameters/Canada_TypeShare_2020.csv", show_col_types = FALSE)
canada_ice_pool <- read_csv("Parameters/Canada_ICE_Pool_2020.csv", show_col_types = FALSE)
canada_ev_historical <- read_csv("Parameters/Canada_EV_Historical.csv", show_col_types = FALSE)
canada_pr_accii <- read_csv("Parameters/Canada_PR_ACCII.csv", show_col_types = FALSE)
canada_pr_repeal <- read_csv("Parameters/Canada_PR_Repeal.csv", show_col_types = FALSE)
canada_age_dist <- read_csv("Parameters/Canada_AgeDistribution.csv", show_col_types = FALSE)
canada_total_sales <- read_csv("Parameters/Canada_TotalSales_bySegment.csv", show_col_types = FALSE)

cat("Data files loaded.\n")

# -----------------------------
# 2b) Prepare REAL sales data for 2020-2024
# -----------------------------
# National total sales by segment (Car/SUV)
canada_national_sales <- canada_total_sales %>%
  filter(Year >= 2020, Year <= 2024) %>%
  rename(Segment = Segment, National_Sales = Total_Sales)

# EV sales by province/segment/propulsion (already allocated to provinces)
canada_ev_real <- canada_ev_historical %>%
  filter(`Sale Year` >= 2020, `Sale Year` <= 2024) %>%
  rename(Year = `Sale Year`, Segment = Vehicle) %>%
  select(State, Year, Segment, Propulsion, Sales)

# National EV totals by year/segment
canada_ev_national <- canada_ev_real %>%
  group_by(Year, Segment) %>%
  summarise(EV_Sales = sum(Sales, na.rm = TRUE), .groups = "drop")

# National ICE = Total - EV
canada_ice_national <- canada_national_sales %>%
  left_join(canada_ev_national, by = c("Year", "Segment")) %>%
  mutate(
    EV_Sales = coalesce(EV_Sales, 0),
    ICE_Sales = pmax(0, National_Sales - EV_Sales)
  )

cat("\n=== Canada Real Sales (National) ===\n")
print(canada_ice_national)

# Allocate ICE sales to provinces by their 2020 ICE stock share
ice_stock_share <- canada_ice_pool %>%
  mutate(
    Total_ICE = sum(ICE_Total, na.rm = TRUE),
    Province_Share = ICE_Total / Total_ICE
  ) %>%
  select(Province, Province_Share)

# Also need Car/Truck share within each province
canada_ice_real <- canada_ice_national %>%
  crossing(ice_stock_share) %>%
  mutate(ICE_Province = ICE_Sales * Province_Share) %>%
  select(State = Province, Year, Segment, ICE_Sales = ICE_Province)

cat("\n=== Canada ICE Real Sales (by Province) - Sample ===\n")
print(canada_ice_real %>% filter(Year == 2024) %>% head(10))

# -----------------------------
# 3) Export factor (from US trade data)
# -----------------------------
if (exists("export_long") && exists("import_long")) {
  canada_export_from_us <- export_long %>%
    filter(Partner == "Canada") %>%
    group_by(Year) %>%
    summarise(US_Export_to_Canada = sum(as.numeric(Export), na.rm = TRUE), .groups = "drop")
  
  canada_import_to_us <- import_long %>%
    filter(Partner == "Canada") %>%
    group_by(Year) %>%
    summarise(US_Import_from_Canada = sum(as.numeric(Import), na.rm = TRUE), .groups = "drop")
  
  canada_trade <- full_join(canada_export_from_us, canada_import_to_us, by = "Year") %>%
    mutate(
      US_Export_to_Canada = coalesce(US_Export_to_Canada, 0),
      US_Import_from_Canada = coalesce(US_Import_from_Canada, 0),
      Canada_Net_Import = US_Export_to_Canada - US_Import_from_Canada
    ) %>%
    filter(Year >= 2020, Year <= 2024)
  
  avg_canada_net_import <- mean(canada_trade$Canada_Net_Import, na.rm = TRUE)
  canada_export_factor <- if (avg_canada_net_import >= 0) 0 else 0.05
} else {
  canada_export_factor <- 0
}
cat("Canada export factor:", canada_export_factor, "\n")

# -----------------------------
# 4) Prepare simulation data
# -----------------------------
provinces <- unique(canada_type_share$Province)
cat("Provinces:", length(provinces), "\n")

seg_share_2020 <- canada_type_share %>%
  transmute(State = Province, Car = type_share_Car, SUV = type_share_Truck)

# Population growth
pop_growth <- canada_pop %>%
  arrange(Province, Year) %>%
  group_by(Province) %>%
  mutate(Pop_lag = lag(Population), Pop_growth = Population - Pop_lag) %>%
  ungroup() %>%
  filter(!is.na(Pop_growth))

first_pop_year <- min(canada_pop$Year)
canada_pop_first <- canada_pop %>% filter(Year == first_pop_year)

vpp <- canada_ice_pool %>%
  left_join(canada_pop_first, by = "Province") %>%
  mutate(VPP = ICE_Total / Population) %>%
  select(Province, VPP)

growth_from_pop <- pop_growth %>%
  left_join(vpp, by = "Province") %>%
  mutate(Growth_from_pop = pmax(0, Pop_growth * VPP)) %>%
  select(State = Province, Year, Growth_from_pop)

growth_seg_base <- growth_from_pop %>%
  left_join(seg_share_2020, by = "State") %>%
  pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "seg_share") %>%
  mutate(Growth_seg = Growth_from_pop * coalesce(seg_share, 0.5)) %>%
  select(State, Segment, Year, Growth_seg)

# PR tables
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
        add_years <- have_min %>% select(State, BEV, PHEV) %>% crossing(Year = 2020:(min_year - 1))
        bind_rows(., add_years)
      } else .
    } %>%
    arrange(State, Year) %>%
    mutate(ICE = pmax(0, 1 - BEV - PHEV))
}

# ICE initialization
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

if ("ageID" %in% names(canada_age_dist)) {
  age_fracs <- canada_age_dist %>%
    filter(Segment %in% c("Car", "SUV")) %>%
    group_by(Province, Segment) %>%
    mutate(ageFraction = ageFraction / sum(ageFraction, na.rm = TRUE)) %>%
    ungroup()
} else {
  age_fracs <- expand.grid(Province = provinces, Segment = c("Car","SUV"), ageID = 0:49) %>%
    as_tibble() %>%
    mutate(ageFraction = 1/50)
}

if ("ICE_Car" %in% names(canada_ice_pool)) {
  ice_by_seg <- canada_ice_pool %>%
    select(Province, Car = ICE_Car, SUV = ICE_Truck) %>%
    pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "ICE_Total")
} else if ("Car" %in% names(canada_ice_pool)) {
  ice_by_seg <- canada_ice_pool %>%
    select(Province, Car, SUV = Truck) %>%
    pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "ICE_Total")
} else {
  ice_by_seg <- canada_ice_pool %>%
    mutate(Car = ICE_Total * 0.5, SUV = ICE_Total * 0.5) %>%
    select(Province, Car, SUV) %>%
    pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "ICE_Total")
}

ice_init <- ice_by_seg %>%
  left_join(age_fracs, by = c("Province", "Segment")) %>%
  mutate(ageID = coalesce(ageID, 0L), N = ICE_Total * coalesce(ageFraction, 1/50)) %>%
  filter(!is.na(ageID)) %>%
  select(State = Province, Segment, ageID, N)

cat("ICE stock initialized for", length(unique(ice_init$State)), "provinces\n")

# -----------------------------
# 5) Main simulation function
# -----------------------------
scale_int <- function(v, factor) {
  v <- as.integer(round(as.numeric(v) * factor))
  v[is.na(v)] <- 0L
  v
}

add_proportional <- function(dst_vec, src_vec, add_total) {
  w <- as.numeric(src_vec); w[is.na(w)] <- 0
  if (sum(w) <= 0 || add_total <= 0) return(dst_vec)
  share <- w / sum(w)
  addv <- as.integer(round(add_total * share))
  residue <- add_total - sum(addv)
  if (residue > 0) {
    ord <- order(share - addv / pmax(1, add_total), decreasing = TRUE)
    addv[head(ord, residue)] <- addv[head(ord, residue)] + 1L
  }
  dst_vec + addv
}

run_canada_simulation <- function(PR_table, scenario_tag) {
  cat("\n========================================\n")
  cat("Running Canada simulation:", scenario_tag, "\n")
  cat("========================================\n")
  
  PR_wide <- make_PR_wide(PR_table)
  
  # Initialize ICE environment
  ice_env <- new.env(parent = emptyenv())
  ice_keys <- ice_init %>% distinct(State, Segment) %>% arrange(State, Segment)
  
  for (i in seq_len(nrow(ice_keys))) {
    k <- ice_keys[i, ]
    vec <- ice_init %>% filter(State == k$State, Segment == k$Segment) %>% arrange(ageID) %>% pull(N)
    if (length(vec) < 50) vec <- c(vec, rep(0, 50 - length(vec)))
    ice_env[[paste(k$State, k$Segment, sep = " | ")]] <- vec
  }
  
  # Initialize EV engines (warm-up 2014-2019)
  ev_engines <- new.env(parent = emptyenv())
  
  get_or_create_engine <- function(state, segment, propulsion) {
    key <- paste(state, segment, propulsion, sep = " | ")
    if (exists(key, envir = ev_engines, inherits = FALSE))
      return(get(key, envir = ev_engines, inherits = FALSE))
    
    # Note: canada_ev_historical uses 'State', 'Sale Year', 'Vehicle' columns
    slice <- canada_ev_historical %>%
      filter(State == state, Vehicle == segment, Propulsion == propulsion, `Sale Year` >= 2014, `Sale Year` <= 2019) %>%
      rename(Year = `Sale Year`) %>%
      arrange(Year)
    if (nrow(slice) == 0) slice <- tibble(Year = 2014:2019, Sales = 0)
    
    eng <- EV_engine_init(slice, segment = segment, propulsion = propulsion,
                          lifetime_scen = "Baseline", start_year = 2014, warmup_last_year = 2019)
    assign(key, eng, envir = ev_engines)
    eng
  }
  
  # Initialize all engines
  for (st in provinces) for (seg in c("Car","SUV")) for (pp in c("BEV","PHEV")) get_or_create_engine(st, seg, pp)
  
  # Results storage
  results_rows <- list()
  evlib_rows <- list()
  
  domestic_factor <- 1 - canada_export_factor
  
  for (yr in years) {
    
    # --- ICE retirement and aging ---
    ice_retire_df <- purrr::map_dfr(seq_len(nrow(ice_keys)), function(i) {
      k <- ice_keys[i, ]
      key <- paste(k$State, k$Segment, sep = " | ")
      N <- ice_env[[key]]
      surv <- surv_tbl_ice[[k$Segment]]
      y <- surv$y
      names(y) <- as.character(surv$ageID)
      q <- 1 - y
      retire_by_age <- N * q[as.character(0:49)]
      survivors <- N * y[as.character(0:49)]
      N_next <- numeric(50)
      N_next[2:50] <- survivors[1:49]
      ice_env[[key]] <- N_next
      
      tibble(State = k$State, Segment = k$Segment, Year = yr, ret_ICE = sum(retire_by_age, na.rm = TRUE))
    })
    
    # --- EV retirement (estimate from engines) ---
    ev_retire_rows <- list()
    for (nm in ls(envir = ev_engines)) {
      eng <- get(nm, envir = ev_engines, inherits = FALSE)
      step1 <- EV_engine_step(eng, sales_y = 0)
      parts <- str_split(nm, " \\| ", simplify = TRUE)
      ev_retire_rows[[nm]] <- tibble(
        State = parts[1], Segment = parts[2], Propulsion = parts[3],
        Year = yr, ret_EV_pt = as_scalar_num(step1$EV_retired)
      )
    }
    ev_retire_df <- if (length(ev_retire_rows)) bind_rows(ev_retire_rows) else
      tibble(State=character(), Segment=character(), Propulsion=character(), Year=integer(), ret_EV_pt=double())
    
    ev_retire_seg <- ev_retire_df %>%
      group_by(State, Segment, Year) %>%
      summarise(ret_BEV = sum(ret_EV_pt[Propulsion == "BEV"], na.rm = TRUE),
                ret_PHEV = sum(ret_EV_pt[Propulsion == "PHEV"], na.rm = TRUE), .groups = "drop")
    
    # --- Demand calculation ---
    grow_now <- growth_seg_base %>% filter(Year == yr)
    
    if (yr <= 2024) {
      # Use REAL sales data for 2020-2024
      ev_real_yr <- canada_ev_real %>% filter(Year == yr)
      ice_real_yr <- canada_ice_real %>% filter(Year == yr)
      
      add_need <- ice_retire_df %>%
        left_join(ev_retire_seg, by = c("State","Segment","Year")) %>%
        left_join(grow_now, by = c("State","Segment")) %>%
        mutate(
          ret_BEV = coalesce(ret_BEV, 0),
          ret_PHEV = coalesce(ret_PHEV, 0),
          Growth_seg = coalesce(Growth_seg, 0),
          Year = yr  # Ensure Year column exists
        ) %>%
        # Get real ICE sales
        left_join(ice_real_yr %>% select(State, Segment, add_ICE = ICE_Sales), by = c("State", "Segment")) %>%
        # Get real EV sales
        left_join(ev_real_yr %>% filter(Propulsion == "BEV") %>% 
                    group_by(State, Segment) %>% summarise(add_BEV = sum(Sales), .groups = "drop"),
                  by = c("State", "Segment")) %>%
        left_join(ev_real_yr %>% filter(Propulsion == "PHEV") %>% 
                    group_by(State, Segment) %>% summarise(add_PHEV = sum(Sales), .groups = "drop"),
                  by = c("State", "Segment")) %>%
        mutate(
          add_ICE = coalesce(add_ICE, 0),
          add_BEV = coalesce(add_BEV, 0),
          add_PHEV = coalesce(add_PHEV, 0),
          Demand_total = add_BEV + add_PHEV + add_ICE
        )
    } else {
      # 2025+: Simulate from retirement + growth + penetration rates
      add_need <- ice_retire_df %>%
        left_join(ev_retire_seg, by = c("State","Segment","Year")) %>%
        left_join(grow_now, by = c("State","Segment")) %>%
        mutate(
          ret_BEV = coalesce(ret_BEV, 0),
          ret_PHEV = coalesce(ret_PHEV, 0),
          Growth_seg = coalesce(Growth_seg, 0),
          Demand_total = ret_ICE + ret_BEV + ret_PHEV + Growth_seg
        ) %>%
        left_join(PR_wide %>% filter(Year == yr), by = "State") %>%
        mutate(
          add_BEV = Demand_total * coalesce(BEV, 0),
          add_PHEV = Demand_total * coalesce(PHEV, 0),
          add_ICE = Demand_total * coalesce(ICE, 1)
        )
    }
    
    # --- Feed ICE ---
    for (i in seq_len(nrow(ice_keys))) {
      k <- ice_keys[i, ]
      key <- paste(k$State, k$Segment, sep = " | ")
      N <- ice_env[[key]]
      add0 <- add_need %>% filter(State == k$State, Segment == k$Segment) %>%
        summarise(val = sum(add_ICE, na.rm = TRUE)) %>% pull(val)
      N[1] <- N[1] + as_scalar_num(add0, 0)
      ice_env[[key]] <- N
    }
    
    # --- Feed EV engines and collect LIB flows ---
    for (st in provinces) {
      for (seg in c("Car","SUV")) {
        row_seg <- add_need %>% filter(State == st, Segment == seg)
        if (nrow(row_seg) == 0) next
        
        add_bev_seg <- as_scalar_num(sum(row_seg$add_BEV, na.rm = TRUE), 0)
        add_phev_seg <- as_scalar_num(sum(row_seg$add_PHEV, na.rm = TRUE), 0)
        
        for (pp in c("BEV","PHEV")) {
          sales_y <- ifelse(pp == "BEV", add_bev_seg, add_phev_seg)
          eng <- get_or_create_engine(st, seg, pp)
          step2 <- EV_engine_step(eng, sales_y = sales_y, reuse_shrink = domestic_factor)
          assign(paste(st, seg, pp, sep = " | "), step2$engine, envir = ev_engines)
          
          LIB_recycling_vec_adj <- scale_int(step2$LIB_recycling_vector, domestic_factor)
          LIB_available_vec_adj <- scale_int(step2$LIB_available_vector, domestic_factor)
          LIB_reuse_vec_adj <- scale_int(step2$LIB_reuse_vector, domestic_factor)
          LIB_repurpose_vec_adj <- scale_int(step2$LIB_repurpose_vector, domestic_factor)
          
          lost_reuse <- sum(as.integer(step2$LIB_reuse_vector)) - sum(LIB_reuse_vec_adj)
          LIB_newadd_vec_adj <- as.integer(step2$LIB_newadd_vector)
          LIB_newadd_vec_adj <- add_proportional(LIB_newadd_vec_adj, step2$LIB_reuse_vector, as.integer(lost_reuse))
          
          evlib_rows[[length(evlib_rows) + 1]] <- tibble(
            State = st, Segment = seg, Propulsion = pp, Year = yr,
            LIB_recycling = as.integer(sum(LIB_recycling_vec_adj)),
            LIB_available = as.integer(sum(LIB_available_vec_adj)),
            LIB_reuse_EV = as.integer(sum(LIB_reuse_vec_adj)),
            LIB_new_add = as.integer(sum(LIB_newadd_vec_adj)),
            LIB_repurpose = as.integer(sum(LIB_repurpose_vec_adj)),
            EV_stock = as.integer(step2$EV_stock),
            LIB_recycling_vector = list(as.integer(LIB_recycling_vec_adj)),
            LIB_available_vector = list(as.integer(LIB_available_vec_adj)),
            LIB_reuse_vector = list(as.integer(LIB_reuse_vec_adj)),
            LIB_newadd_vector = list(as.integer(LIB_newadd_vec_adj)),
            LIB_repurpose_vector = list(as.integer(LIB_repurpose_vec_adj)),
            EV_stock_vector = list(as.integer(step2$EV_stock_vector)),
            domestic_factor = domestic_factor
          )
        }
      }
    }
    
    # --- Record results ---
    res_tmp <- add_need %>%
      mutate(ret_EV = ret_BEV + ret_PHEV) %>%
      select(State, Segment, Year, ret_ICE, ret_BEV, ret_PHEV, ret_EV, Growth_seg, add_BEV, add_PHEV, add_ICE, Demand_total)
    
    results_rows[[as.character(yr)]] <- res_tmp
    if (yr %% 5 == 0) cat("  Year", yr, "done\n")
  }
  
  # --- Outputs ---
  result_df <- bind_rows(results_rows) %>% arrange(State, Segment, Year)
  
  result_state <- result_df %>%
    group_by(State, Year) %>%
    summarise(
      add_BEV = sum(add_BEV, na.rm = TRUE),
      add_PHEV = sum(add_PHEV, na.rm = TRUE),
      add_ICE = sum(add_ICE, na.rm = TRUE),
      ret_ICE = sum(ret_ICE, na.rm = TRUE),
      ret_BEV = sum(ret_BEV, na.rm = TRUE),
      ret_PHEV = sum(ret_PHEV, na.rm = TRUE),
      Growth_from_pop = sum(Growth_seg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(ret_EV = ret_BEV + ret_PHEV, Demand_total = add_BEV + add_PHEV + add_ICE)
  
  write.csv(result_df, paste0("Outputs/Canada/ClosedLoop_AddRetire_byStateSegment_", scenario_tag, ".csv"), row.names = FALSE)
  write.csv(result_state, paste0("Outputs/Canada/ClosedLoop_StateTotals_", scenario_tag, ".csv"), row.names = FALSE)
  
  # --- EVLIB outputs ---
  evlib_detail <- if (length(evlib_rows)) bind_rows(evlib_rows) else
    tibble(State=character(), Segment=character(), Propulsion=character(), Year=integer(),
           LIB_recycling=double(), LIB_available=double(), LIB_reuse_EV=double(), LIB_new_add=double(),
           LIB_repurpose=double(), EV_stock=double())
  
  sum_vec <- function(a, b) {
    a <- as.integer(a); b <- as.integer(b)
    len <- max(length(a), length(b))
    if (len == 0) return(integer())
    a <- c(a, rep(0L, len - length(a)))
    b <- c(b, rep(0L, len - length(b)))
    a + b
  }
  
  evlib_totals <- evlib_detail %>%
    group_by(State, Segment, Year) %>%
    summarise(
      LIB_recycling = sum(as.integer(LIB_recycling), na.rm = TRUE),
      LIB_available = sum(as.integer(LIB_available), na.rm = TRUE),
      LIB_reuse_EV = sum(as.integer(LIB_reuse_EV), na.rm = TRUE),
      LIB_new_add = sum(as.integer(LIB_new_add), na.rm = TRUE),
      LIB_repurpose = sum(as.integer(LIB_repurpose), na.rm = TRUE),
      EV_stock = sum(as.integer(EV_stock), na.rm = TRUE),
      LIB_recycling_vector = list(reduce(LIB_recycling_vector, sum_vec)),
      LIB_available_vector = list(reduce(LIB_available_vector, sum_vec)),
      LIB_reuse_vector = list(reduce(LIB_reuse_vector, sum_vec)),
      LIB_newadd_vector = list(reduce(LIB_newadd_vector, sum_vec)),
      LIB_repurpose_vector = list(reduce(LIB_repurpose_vector, sum_vec)),
      EV_stock_vector = list(reduce(EV_stock_vector, sum_vec)),
      .groups = "drop"
    )
  
  flatify <- function(df) {
    df %>%
      mutate(
        LIB_recycling_vector = sapply(LIB_recycling_vector, function(v) paste(v, collapse = "|")),
        LIB_available_vector = sapply(LIB_available_vector, function(v) paste(v, collapse = "|")),
        LIB_reuse_vector = sapply(LIB_reuse_vector, function(v) paste(v, collapse = "|")),
        LIB_newadd_vector = sapply(LIB_newadd_vector, function(v) paste(v, collapse = "|")),
        LIB_repurpose_vector = sapply(LIB_repurpose_vector, function(v) paste(v, collapse = "|")),
        EV_stock_vector = sapply(EV_stock_vector, function(v) paste(v, collapse = "|"))
      )
  }
  
  write.csv(flatify(evlib_detail), paste0("Outputs/Canada/EVLIB_Flows_detail_", scenario_tag, ".csv"), row.names = FALSE)
  write.csv(flatify(evlib_totals), paste0("Outputs/Canada/EVLIB_Flows_totals_", scenario_tag, ".csv"), row.names = FALSE)
  
  cat("Saved outputs for", scenario_tag, "\n")
  
  list(addret = result_df, totals = result_state, evlib_detail = evlib_detail, evlib_totals = evlib_totals)
}

# -----------------------------
# 6) Run both scenarios
# -----------------------------
result_accii <- run_canada_simulation(canada_pr_accii, "ACCII")
result_repeal <- run_canada_simulation(canada_pr_repeal, "Repeal")

# -----------------------------
# 7) Summary
# -----------------------------
cat("\n========================================\n")
cat("=== Canada Simulation Complete! ===\n")
cat("========================================\n")

cat("\nACCII 2050:\n")
accii_2050 <- result_accii$totals %>%
  filter(Year == 2050) %>%
  summarise(Total_BEV = sum(add_BEV), Total_PHEV = sum(add_PHEV), Total_ICE = sum(add_ICE))
print(accii_2050)

cat("\nRepeal 2050:\n")
repeal_2050 <- result_repeal$totals %>%
  filter(Year == 2050) %>%
  summarise(Total_BEV = sum(add_BEV), Total_PHEV = sum(add_PHEV), Total_ICE = sum(add_ICE))
print(repeal_2050)

cat("\nOutput files in Outputs/Canada/:\n")
list.files("Outputs/Canada/")
