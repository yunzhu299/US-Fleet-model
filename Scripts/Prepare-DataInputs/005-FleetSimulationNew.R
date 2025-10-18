## 05 — Closed-loop Fleet Simulation (2020–2050) with dynamic penetration rates
## Uses ICE survival from 02 and EV engine from 04 (Logistic + LIB reuse/recycling)
## Warm-up: 2014–2019 EV_historical only
## 2020–2024: REAL EV sales; ICE = (TotalSales - EV) allocated to states by regs-based ICE shares
##            (2020–2023 use each year's share; 2024 uses 2023's share)
## 2025+: simulate via PR
## Exports: shrink domestic LIB flows from EV retirements by an export factor
##          (does NOT change vehicle stock; ONLY adjusts LIB reuse/recycle/new-add flows)
## YZC Oct 2025

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(readr)
library(ggplot2)
library(scales)
library(readxl)

# -----------------------------
# 0) Global settings
# -----------------------------
years <- 2020:2030  # change to 2030 for quick tests if needed

as_scalar_num <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- suppressWarnings(as.numeric(x))
  if (length(x) > 1) x <- sum(x, na.rm = TRUE)
  if (!is.finite(x)) x <- default
  x
}

dir.create("Outputs", showWarnings = FALSE)

# -----------------------------
# Penetration rates (two scenarios)
# -----------------------------
P_R_ACCII  <- read_csv("~/Downloads/PR_ACCII.csv", show_col_types = FALSE)
P_R_Repeal <- read_csv("~/Downloads/PR_Repeal.csv", show_col_types = FALSE) %>%
  mutate(State = str_trim(State),
         State = dplyr::recode(State, "Massachusettes" = "Massachusetts"))

# -----------------------------
# 1) Split population growth to Car/SUV by 2020 shares
# -----------------------------
seg_share_2020 <- state_type_share %>%
  filter(yearID == 2020) %>%
  transmute(
    State = state,
    Car = type_share_Car,
    SUV = type_share_Truck
  )

growth_seg_base <- growth_from_pop %>%
  transmute(State, Year, Growth_from_pop = coalesce(Growth_from_pop, 0)) %>%
  inner_join(seg_share_2020, by = "State") %>%
  pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "seg_share") %>%
  mutate(Growth_seg = Growth_from_pop * seg_share) %>%
  select(State, Segment, Year, Growth_seg)

# -----------------------------
# 2) Required inputs from 04
# -----------------------------
if (!exists("EV_historical")) {
  stop("EV_historical not found. In 04, run: EV_historical <- read_csv('~/Downloads/historical_state_pt_veh_df.csv')")
}
if (!exists("EV_engine_init") || !exists("EV_engine_step")) {
  stop("EV engine functions not found. Source 04 before running 05.")
}

# -----------------------------
# 3) PR_wide with 2020 backfilled from 2021
# -----------------------------
make_PR_wide <- function(PR_table) {
  PR_table %>%
    mutate(State = str_trim(State), Year = as.integer(Year)) %>%
    filter(Propulsion %in% c("BEV", "PHEV")) %>%
    select(State, Year, Propulsion, Fraction) %>%
    pivot_wider(names_from = Propulsion, values_from = Fraction) %>%
    mutate(BEV = coalesce(BEV, 0), PHEV = coalesce(PHEV, 0)) %>%
    { have2021 <- dplyr::filter(., Year == 2021)
    add2020  <- dplyr::mutate(have2021, Year = 2020)
    bind_rows(., add2020) } %>%
    arrange(State, Year) %>%
    mutate(ICE = pmax(0, 1 - BEV - PHEV))
}

# -----------------------------
# 3b) REAL EV sales (2020–2024) + NATIONAL ICE totals by year
#      ICE allocation uses regs-based ICE stock ratios
# -----------------------------
# EV real by state×segment×propulsion×year
real_ev_2020_24 <- EV_historical %>%
  filter(`Sale Year` >= 2020, `Sale Year` <= 2024) %>%
  transmute(State,
            Segment = `Global Segment`,
            Propulsion,
            Year = `Sale Year`,
            Sales = as.numeric(Sales))

# National total vehicle sales (Car, SUV, Total columns; years as headers)
X2020_2025sales <- read_excel("~/Downloads/2020-2025sales.xlsx", sheet = "Sheet1") %>%
  rename(Category = 1) %>%
  pivot_longer(cols = -Category, names_to = "Year", values_to = "TotalSales") %>%
  mutate(Year = as.integer(Year)) %>%
  filter(Category == "Total") %>%
  select(Year, TotalSales)

# National EV totals (BEV+PHEV)
ev_totals_by_year <- real_ev_2020_24 %>%
  filter(Propulsion %in% c("BEV", "PHEV")) %>%
  group_by(Year) %>%
  summarise(EV_sales_total = sum(Sales, na.rm = TRUE), .groups = "drop")

# National ICE totals = total - EV
ice_total_by_year <- X2020_2025sales %>%
  left_join(ev_totals_by_year, by = "Year") %>%
  mutate(EV_sales_total = coalesce(EV_sales_total, 0),
         ICE_total = pmax(0, TotalSales - EV_sales_total)) %>%
  filter(Year >= 2020, Year <= 2024) %>%
  select(Year, ICE_total)

# -----------------------------
# Build state ICE weights from regs:
#  - For 2020–2023: use that year's ICE stock share
#  - For 2024: reuse the 2023 share
# -----------------------------
regs_ice <- regs %>%
  transmute(
    State = State,
    Year  = as.integer(Year),
    ICE_reg = coalesce(Gasoline, 0) + coalesce(`Hybrid Electric (HEV)`, 0)
  ) %>%
  filter(!is.na(State), State != "United States")

w_2020_2023 <- regs_ice %>%
  filter(Year %in% 2020:2023) %>%
  group_by(Year) %>%
  mutate(w_share = ICE_reg / sum(ICE_reg, na.rm = TRUE)) %>%
  ungroup() %>%
  select(State, Year, w_share)

w_2024 <- w_2020_2023 %>%
  filter(Year == 2023) %>%
  mutate(Year = 2024)

weights_20_24 <- bind_rows(w_2020_2023, w_2024) %>%
  arrange(State, Year)

# Allocate national ICE totals to states by these weights
ice_state_alloc_raw <- ice_total_by_year %>%
  filter(Year %in% 2020:2024) %>%
  left_join(weights_20_24, by = "Year") %>%
  mutate(
    w_share = coalesce(w_share, 0),
    ICE_state_float = ICE_total * w_share
  ) %>%
  select(State, Year, ICE_state_float)

# Round per year and adjust to match the national total exactly
ice_state_totals <- ice_state_alloc_raw %>%
  group_by(Year) %>%
  mutate(ICE_state = floor(ICE_state_float)) %>%
  mutate(
    remainder = ice_total_by_year$ICE_total[match(Year, ice_total_by_year$Year)] -
      sum(ICE_state)
  ) %>%
  arrange(Year, desc(ICE_state_float - ICE_state)) %>%
  mutate(add = as.integer(row_number() <= pmax(0, remainder))) %>%
  mutate(ICE_state = ICE_state + add) %>%
  ungroup() %>%
  select(State, Year, ICE_state) %>%
  arrange(State, Year)

# Split ICE to Car/SUV by each state's 2020 shares
ice_real_2020_24 <- ice_state_totals %>%
  left_join(seg_share_2020, by = "State") %>%
  pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "seg_share") %>%
  mutate(seg_share = coalesce(seg_share, 0)) %>%
  transmute(State, Segment, Year,
            ICE_sales_seg = as.integer(round(ICE_state * seg_share))) %>%
  arrange(State, Segment, Year)

# -----------------------------
# 3c) Export factor helpers
# -----------------------------
# Priority:
#   - If Year in 2020–2024 and 'ratio_tbl' exists with per-year ratios: use that year’s ratio.
#   - Else if Year >= 2025 and 'shrink_ratio_avg' exists: use the single average ratio.
#   - Else: 0.
get_export_factor <- function(yr) {
  ef <- 0
  if (yr <= 2024 && exists("ratio_tbl")) {
    r <- tryCatch({
      ratio_tbl %>% filter(Year == yr) %>% pull(ratio)
    }, error = function(e) numeric(0))
    if (length(r) == 1 && is.finite(r)) ef <- as.numeric(r)
  } else if (yr >= 2025 && exists("shrink_ratio_avg")) {
    r <- tryCatch(as.numeric(shrink_ratio_avg), error = function(e) NA_real_)
    if (is.finite(r)) ef <- r
  }
  ef <- max(0, min(1, ef))
  ef
}

# Integer scaling + proportional add helper
scale_int <- function(v, factor) {
  v <- as.integer(round(as.numeric(v) * factor))
  v[is.na(v)] <- 0L
  v
}
add_proportional <- function(dst_vec, src_vec, add_total) {
  w <- as.numeric(src_vec); w[is.na(w)] <- 0
  if (sum(w) <= 0 || add_total <= 0) return(dst_vec)
  share <- w / sum(w)
  addv  <- as.integer(round(add_total * share))
  residue <- add_total - sum(addv)
  if (residue > 0) {
    ord <- order(share - addv / pmax(1, add_total), decreasing = TRUE)
    addv[head(ord, residue)] <- addv[head(ord, residue)] + 1L
  }
  dst_vec + addv
}

# -----------------------------
# 4) Main scenario runner
# -----------------------------
run_one_scenario <- function(PR_table, scenario_tag = "ACCII") {
  
  PR_wide <- make_PR_wide(PR_table)
  
  # ---- Initialize ICE stock at start of 2020 (Car/SUV split, age 0..49)
  regs_2020_pt <- regs %>%
    filter(Year == 2020) %>%
    transmute(State,
              ICE = coalesce(Gasoline, 0) + coalesce(`Hybrid Electric (HEV)`, 0))
  
  regs_2020_ICE_seg <- regs_2020_pt %>%
    inner_join(seg_share_2020, by = "State") %>%
    pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "seg_share") %>%
    mutate(TotalPT_Seg = ICE * seg_share) %>%
    select(State, Segment, TotalPT_Seg)
  
  frac_age_2020 <- state_age_long_filled_by_type %>%
    filter(yearID == 2020) %>%
    transmute(
      State   = state,
      Segment = if_else(Type == "Truck", "SUV", Type),
      ageID,
      age_frac = ageFraction_state_type
    ) %>%
    filter(Segment %in% c("Car", "SUV"))
  
  ice_init_0_49 <- regs_2020_ICE_seg %>%
    inner_join(frac_age_2020, by = c("State", "Segment")) %>%
    mutate(N = pmax(TotalPT_Seg * age_frac, 0)) %>%
    filter(ageID <= 49) %>%
    group_by(State, Segment, ageID) %>%
    summarise(N = sum(N, na.rm = TRUE), .groups = "drop")
  
  surv_tbl_ice <- list(
    Car = surv_tbl_by_type$Car,
    SUV = { tmp <- surv_tbl_by_type$Truck; tmp$ageID <- 0:49; tmp }
  )
  
  ice_env <- new.env(parent = emptyenv())
  ice_keys <- ice_init_0_49 %>% distinct(State, Segment) %>% arrange(State, Segment)
  for (i in seq_len(nrow(ice_keys))) {
    k <- ice_keys[i, ]
    vec <- ice_init_0_49 %>% filter(State == k$State, Segment == k$Segment) %>%
      arrange(ageID) %>% pull(N)
    if (length(vec) < 50) vec <- c(vec, rep(0, 50 - length(vec)))
    ice_env[[paste(k$State, k$Segment, sep = " | ")]] <- vec
  }
  
  # ---- EV engines (warm-up 2014–2019 ONLY)
  ev_engines <- new.env(parent = emptyenv())
  
  get_or_create_engine <- function(state, segment, propulsion) {
    key <- paste(state, segment, propulsion, sep = " | ")
    if (exists(key, envir = ev_engines, inherits = FALSE))
      return(get(key, envir = ev_engines, inherits = FALSE))
    
    slice <- EV_historical %>%
      filter(State == state,
             `Global Segment` == segment,
             Propulsion == propulsion,
             `Sale Year` >= 2014, `Sale Year` <= 2019) %>%
      arrange(`Sale Year`)
    if (nrow(slice) == 0)
      slice <- tibble(`Sale Year` = 2014:2019, Sales = 0)
    
    eng <- EV_engine_init(
      slice, segment = segment, propulsion = propulsion,
      lifetime_scen = "Baseline",
      start_year = 2014, warmup_last_year = 2019
    )
    assign(key, eng, envir = ev_engines)
    eng
  }
  
  states_ev   <- unique(EV_historical$State)
  segments_ev <- c("Car", "SUV")
  props_ev    <- c("BEV", "PHEV")
  for (st in states_ev) for (seg in segments_ev) for (pp in props_ev) get_or_create_engine(st, seg, pp)
  
  # -----------------------------
  # 5) Yearly loop
  # -----------------------------
  results_rows <- list()
  evlib_rows   <- list()
  exports_rows <- list()   # national exports by PT
  
  for (yr in years) {
    
    # --- retire ICE & age
    ice_retire_df <- purrr::map_dfr(seq_len(nrow(ice_keys)), function(i) {
      k <- ice_keys[i, ]; key <- paste(k$State, k$Segment, sep = " | ")
      N <- ice_env[[key]]
      surv <- surv_tbl_ice[[k$Segment]]
      y <- surv$y; names(y) <- as.character(surv$ageID)
      q <- 1 - y
      retire_by_age <- N * q[as.character(0:49)]
      survivors     <- N * y[as.character(0:49)]
      N_next <- numeric(50)
      N_next[2:50] <- survivors[1:49]
      ice_env[[key]] <- N_next
      
      tibble(State = k$State, Segment = k$Segment, Year = yr,
             ret_ICE = sum(retire_by_age, na.rm = TRUE))
    })
    
    # --- estimate EV retirements (read-only)
    ev_retire_rows <- list()
    for (nm in ls(envir = ev_engines)) {
      eng <- get(nm, envir = ev_engines, inherits = FALSE)
      eng_tmp <- eng
      step1 <- EV_engine_step(eng_tmp, sales_y = 0)
      parts <- str_split(nm, " \\| ", simplify = TRUE)
      ev_retire_rows[[nm]] <- tibble(
        State = parts[1], Segment = parts[2], Propulsion = parts[3],
        Year = yr, ret_EV_pt = as_scalar_num(step1$EV_retired)
      )
    }
    ev_retire_df <- if (length(ev_retire_rows)) bind_rows(ev_retire_rows) else
      tibble(State=character(), Segment=character(), Propulsion=character(),
             Year=integer(), ret_EV_pt=double())
    
    ev_retire_seg <- ev_retire_df %>%
      group_by(State, Segment, Year) %>%
      summarise(
        ret_BEV  = sum(ret_EV_pt[Propulsion == "BEV"],  na.rm = TRUE),
        ret_PHEV = sum(ret_EV_pt[Propulsion == "PHEV"], na.rm = TRUE),
        .groups = "drop"
      )
    
    # --- compute export factor for this year (EV battery flows only)
    export_factor_y <- get_export_factor(yr)
    domestic_factor <- 1 - export_factor_y
    
    # -----------------
    # Branch by year:
    # 2020–2024 → REAL adds (EV real; ICE from regs allocation)
    # 2025+     → simulate from demand & PR
    # -----------------
    if (yr <= 2024) {
      add_ice_real <- ice_real_2020_24 %>% filter(Year == yr)
      ev_real      <- real_ev_2020_24 %>% filter(Year == yr)
      
      # feed ICE
      for (i in seq_len(nrow(ice_keys))) {
        k <- ice_keys[i, ]; key <- paste(k$State, k$Segment, sep = " | ")
        N   <- ice_env[[key]]
        add0 <- add_ice_real %>% filter(State==k$State, Segment==k$Segment) %>%
          summarise(val = sum(ICE_sales_seg, na.rm = TRUE)) %>% pull(val)
        N[1] <- N[1] + as_scalar_num(add0, 0)
        ice_env[[key]] <- N
      }
      
      # feed EV engines with REAL EV sales, then adjust LIB flows by export
      for (st in unique(ev_real$State)) {
        for (seg in c("Car","SUV")) {
          for (pp in c("BEV","PHEV")) {
            sales_y <- ev_real %>% filter(State==st, Segment==seg, Propulsion==pp) %>%
              summarise(val = sum(Sales, na.rm = TRUE)) %>% pull(val)
            if (length(sales_y)==0) sales_y <- 0
            eng <- get_or_create_engine(st, seg, pp)
            step2 <- EV_engine_step(eng, sales_y = sales_y)
            assign(paste(st, seg, pp, sep = " | "), step2$engine, envir = ev_engines)
            
            # Adjust LIB vectors/scalars for exports (domestic only)
            LIB_recycling_vec_adj <- scale_int(step2$LIB_recycling_vector, domestic_factor)
            LIB_available_vec_adj <- scale_int(step2$LIB_available_vector,  domestic_factor)
            LIB_reuse_vec_adj     <- scale_int(step2$LIB_reuse_vector,      domestic_factor)
            
            lost_reuse <- sum(as.integer(step2$LIB_reuse_vector)) - sum(LIB_reuse_vec_adj)
            LIB_newadd_vec_adj <- as.integer(step2$LIB_newadd_vector)
            LIB_newadd_vec_adj <- add_proportional(LIB_newadd_vec_adj,
                                                   src_vec = step2$LIB_reuse_vector,
                                                   add_total = as.integer(lost_reuse))
            
            evlib_rows[[length(evlib_rows) + 1]] <- tibble(
              State = st, Segment = seg, Propulsion = pp, Year = yr,
              LIB_recycling = as.integer(sum(LIB_recycling_vec_adj)),
              LIB_available = as.integer(sum(LIB_available_vec_adj)),
              LIB_reuse_EV  = as.integer(sum(LIB_reuse_vec_adj)),
              LIB_new_add   = as.integer(sum(LIB_newadd_vec_adj)),
              EV_stock      = as.integer(step2$EV_stock),
              LIB_recycling_vector = list(as.integer(LIB_recycling_vec_adj)),
              LIB_available_vector = list(as.integer(LIB_available_vec_adj)),
              LIB_reuse_vector     = list(as.integer(LIB_reuse_vec_adj)),
              LIB_newadd_vector    = list(as.integer(LIB_newadd_vec_adj)),
              EV_stock_vector      = list(as.integer(step2$EV_stock_vector)),
              domestic_factor = domestic_factor,
              export_factor   = export_factor_y
            )
          }
        }
      }
      
      # record adds/retire for 2020–2024 (+ export tallies by PT)
      res_tmp <- ice_retire_df %>%
        left_join(ev_retire_seg, by = c("State","Segment","Year")) %>%
        mutate(ret_BEV = coalesce(ret_BEV, 0), ret_PHEV = coalesce(ret_PHEV, 0)) %>%
        left_join(add_ice_real, by = c("State","Segment","Year")) %>%
        rename(add_ICE = ICE_sales_seg) %>%
        mutate(add_ICE = coalesce(add_ICE, 0)) %>%
        left_join(
          ev_real %>% filter(Propulsion=="BEV") %>%
            group_by(State, Segment, Year) %>% summarise(add_BEV=sum(Sales), .groups="drop"),
          by=c("State","Segment","Year")
        ) %>%
        left_join(
          ev_real %>% filter(Propulsion=="PHEV") %>%
            group_by(State, Segment, Year) %>% summarise(add_PHEV=sum(Sales), .groups="drop"),
          by=c("State","Segment","Year")
        ) %>%
        mutate(
          add_BEV = coalesce(add_BEV,0), add_PHEV=coalesce(add_PHEV,0),
          Growth_seg = 0,
          ret_EV = ret_BEV + ret_PHEV,
          Demand_total = add_BEV + add_PHEV + add_ICE,
          exp_ICE  = export_factor_y * ret_ICE,
          exp_BEV  = export_factor_y * ret_BEV,
          exp_PHEV = export_factor_y * ret_PHEV
        )
      
      exports_rows[[as.character(yr)]] <- res_tmp %>%
        group_by(Year) %>%
        summarise(
          Export_ICE  = sum(exp_ICE,  na.rm = TRUE),
          Export_BEV  = sum(exp_BEV,  na.rm = TRUE),
          Export_PHEV = sum(exp_PHEV, na.rm = TRUE),
          Export_All  = Export_ICE + Export_BEV + Export_PHEV,
          .groups = "drop"
        )
      
      results_rows[[as.character(yr)]] <- res_tmp %>%
        select(State, Segment, Year, ret_ICE, ret_BEV, ret_PHEV, ret_EV,
               Growth_seg, add_BEV, add_PHEV, add_ICE, Demand_total,
               exp_ICE, exp_BEV, exp_PHEV)
      
      next
    }
    
    # ---- (2025+) simulate demand and split by PR
    grow_now <- growth_seg_base %>% filter(Year == yr)
    
    add_need <- ice_retire_df %>%
      left_join(ev_retire_seg, by = c("State","Segment","Year")) %>%
      left_join(grow_now,     by = c("State","Segment","Year")) %>%
      mutate(
        ret_BEV    = coalesce(ret_BEV, 0),
        ret_PHEV   = coalesce(ret_PHEV, 0),
        Growth_seg = coalesce(Growth_seg, 0),
        Demand_total = ret_ICE + ret_BEV + ret_PHEV + Growth_seg
      ) %>%
      left_join(PR_wide, by = c("State","Year")) %>%
      mutate(
        add_BEV  = Demand_total * coalesce(BEV,  0),
        add_PHEV = Demand_total * coalesce(PHEV, 0),
        add_ICE  = Demand_total * coalesce(ICE,  1)
      )
    
    # feed ICE
    for (i in seq_len(nrow(ice_keys))) {
      k <- ice_keys[i, ]; key <- paste(k$State, k$Segment, sep = " | ")
      N <- ice_env[[key]]
      add0 <- add_need %>%
        filter(State == k$State, Segment == k$Segment, Year == yr) %>%
        summarise(val = sum(add_ICE, na.rm = TRUE)) %>% pull(val)
      N[1] <- N[1] + as_scalar_num(add0, 0)
      ice_env[[key]] <- N
    }
    
    # feed EV engines with simulated sales, then adjust LIB flows by export
    for (st in unique(add_need$State)) {
      for (seg in c("Car","SUV")) {
        row_seg <- add_need %>% filter(State==st, Segment==seg, Year==yr)
        if (nrow(row_seg) == 0) next
        
        add_bev_seg  <- as_scalar_num(sum(row_seg$add_BEV,  na.rm = TRUE), 0)
        add_phev_seg <- as_scalar_num(sum(row_seg$add_PHEV, na.rm = TRUE), 0)
        
        for (pp in c("BEV","PHEV")) {
          sales_y <- ifelse(pp=="BEV", add_bev_seg, add_phev_seg)
          eng <- get_or_create_engine(st, seg, pp)
          step2 <- EV_engine_step(eng, sales_y = sales_y)
          assign(paste(st, seg, pp, sep = " | "), step2$engine, envir = ev_engines)
          
          LIB_recycling_vec_adj <- scale_int(step2$LIB_recycling_vector, domestic_factor)
          LIB_available_vec_adj <- scale_int(step2$LIB_available_vector,  domestic_factor)
          LIB_reuse_vec_adj     <- scale_int(step2$LIB_reuse_vector,      domestic_factor)
          
          lost_reuse <- sum(as.integer(step2$LIB_reuse_vector)) - sum(LIB_reuse_vec_adj)
          LIB_newadd_vec_adj <- as.integer(step2$LIB_newadd_vector)
          LIB_newadd_vec_adj <- add_proportional(LIB_newadd_vec_adj,
                                                 src_vec = step2$LIB_reuse_vector,
                                                 add_total = as.integer(lost_reuse))
          
          evlib_rows[[length(evlib_rows) + 1]] <- tibble(
            State = st, Segment = seg, Propulsion = pp, Year = yr,
            LIB_recycling = as.integer(sum(LIB_recycling_vec_adj)),
            LIB_available = as.integer(sum(LIB_available_vec_adj)),
            LIB_reuse_EV  = as.integer(sum(LIB_reuse_vec_adj)),
            LIB_new_add   = as.integer(sum(LIB_newadd_vec_adj)),
            EV_stock      = as.integer(step2$EV_stock),
            LIB_recycling_vector = list(as.integer(LIB_recycling_vec_adj)),
            LIB_available_vector = list(as.integer(LIB_available_vec_adj)),
            LIB_reuse_vector     = list(as.integer(LIB_reuse_vec_adj)),
            LIB_newadd_vector    = list(as.integer(LIB_newadd_vec_adj)),
            EV_stock_vector      = list(as.integer(step2$EV_stock_vector)),
            domestic_factor = domestic_factor,
            export_factor   = export_factor_y
          )
        }
      }
    }
    
    res_tmp <- add_need %>%
      mutate(
        ret_EV = ret_BEV + ret_PHEV,
        Demand_total_check = add_BEV + add_PHEV + add_ICE,
        exp_ICE  = export_factor_y * ret_ICE,
        exp_BEV  = export_factor_y * ret_BEV,
        exp_PHEV = export_factor_y * ret_PHEV
      ) %>%
      select(State, Segment, Year,
             ret_ICE, ret_BEV, ret_PHEV, ret_EV,
             Growth_seg,
             add_BEV, add_PHEV, add_ICE,
             Demand_total = Demand_total_check,
             exp_ICE, exp_BEV, exp_PHEV)
    
    exports_rows[[as.character(yr)]] <- res_tmp %>%
      group_by(Year) %>%
      summarise(
        Export_ICE  = sum(exp_ICE,  na.rm = TRUE),
        Export_BEV  = sum(exp_BEV,  na.rm = TRUE),
        Export_PHEV = sum(exp_PHEV, na.rm = TRUE),
        Export_All  = Export_ICE + Export_BEV + Export_PHEV,
        .groups = "drop"
      )
    
    results_rows[[as.character(yr)]] <- res_tmp
  }
  
  # -----------------------------
  # 6) Outputs
  # -----------------------------
  result_add_and_retire <- bind_rows(results_rows) %>%
    arrange(State, Segment, Year)
  
  result_state_total <- result_add_and_retire %>%
    group_by(State, Year) %>%
    summarise(
      add_BEV  = sum(add_BEV,  na.rm = TRUE),
      add_PHEV = sum(add_PHEV, na.rm = TRUE),
      add_ICE  = sum(add_ICE,  na.rm = TRUE),
      ret_ICE  = sum(ret_ICE,  na.rm = TRUE),
      ret_BEV  = sum(ret_BEV,  na.rm = TRUE),
      ret_PHEV = sum(ret_PHEV, na.rm = TRUE),
      Growth_from_pop = sum(Growth_seg, na.rm = TRUE),
      exp_ICE  = sum(exp_ICE,  na.rm = TRUE),
      exp_BEV  = sum(exp_BEV,  na.rm = TRUE),
      exp_PHEV = sum(exp_PHEV, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ret_EV = ret_BEV + ret_PHEV,
      Demand_total = add_BEV + add_PHEV + add_ICE,
      Export_All = exp_ICE + exp_BEV + exp_PHEV
    )
  
  write.csv(result_add_and_retire,
            paste0("Outputs/ClosedLoop_AddRetire_byStateSegment_", scenario_tag, ".csv"),
            row.names = FALSE)
  write.csv(result_state_total,
            paste0("Outputs/ClosedLoop_StateTotals_", scenario_tag, ".csv"),
            row.names = FALSE)
  
  # ---- EVLIB flows (detail + totals) — adjusted by export 'domestic_factor'
  evlib_detail <- if (length(evlib_rows)) bind_rows(evlib_rows) else
    tibble(
      State=character(), Segment=character(), Propulsion=character(),
      Year=integer(), LIB_recycling=double(), LIB_available=double(),
      LIB_reuse_EV=double(), LIB_new_add=double(), EV_stock=double(),
      LIB_recycling_vector=list(), LIB_available_vector=list(),
      LIB_reuse_vector=list(), LIB_newadd_vector=list(), EV_stock_vector=list(),
      domestic_factor=double(), export_factor=double()
    )
  
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
      LIB_reuse_EV  = sum(as.integer(LIB_reuse_EV),  na.rm = TRUE),
      LIB_new_add   = sum(as.integer(LIB_new_add),   na.rm = TRUE),
      EV_stock      = sum(as.integer(EV_stock),      na.rm = TRUE),
      LIB_recycling_vector = list(reduce(LIB_recycling_vector, sum_vec)),
      LIB_available_vector = list(reduce(LIB_available_vector, sum_vec)),
      LIB_reuse_vector     = list(reduce(LIB_reuse_vector,     sum_vec)),
      LIB_newadd_vector    = list(reduce(LIB_newadd_vector,    sum_vec)),
      EV_stock_vector      = list(reduce(EV_stock_vector,      sum_vec)),
      .groups = "drop"
    )
  
  flatify <- function(df) {
    df %>%
      mutate(
        LIB_recycling_vector = sapply(LIB_recycling_vector, function(v) paste(v, collapse = "|")),
        LIB_available_vector = sapply(LIB_available_vector, function(v) paste(v, collapse = "|")),
        LIB_reuse_vector     = sapply(LIB_reuse_vector,     function(v) paste(v, collapse = "|")),
        LIB_newadd_vector    = sapply(LIB_newadd_vector,    function(v) paste(v, collapse = "|")),
        EV_stock_vector      = sapply(EV_stock_vector,      function(v) paste(v, collapse = "|"))
      )
  }
  
  write.csv(flatify(evlib_detail),
            paste0("Outputs/EVLIB_Flows_detail_", scenario_tag, ".csv"),
            row.names = FALSE)
  write.csv(flatify(evlib_totals),
            paste0("Outputs/EVLIB_Flows_totals_", scenario_tag, ".csv"),
            row.names = FALSE)
  
  # ---- Export summary by year (national)
  exports_yearly <- if (length(exports_rows)) bind_rows(exports_rows) else
    tibble(Year=integer(), Export_ICE=double(), Export_BEV=double(),
           Export_PHEV=double(), Export_All=double())
  write.csv(exports_yearly,
            paste0("Outputs/Exports_byYear_", scenario_tag, ".csv"),
            row.names = FALSE)
  
  invisible(list(
    addret = result_add_and_retire,
    totals = result_state_total,
    evlib_detail = evlib_detail,
    evlib_totals = evlib_totals,
    exports_by_year = exports_yearly
  ))
}

# -----------------------------
# Run both scenarios
# -----------------------------
run_one_scenario(P_R_ACCII,  "ACCII")
run_one_scenario(P_R_Repeal, "Repeal")