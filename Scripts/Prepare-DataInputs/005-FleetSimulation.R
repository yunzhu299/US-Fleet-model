## 05 — Closed-loop Fleet Simulation (2020–2050) with dynamic penetration rates
## Uses ICE survival from 02 and EV engine from 04 (Logistic + LIB reuse/recycling)
## YZC Sep 2025

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

# Safe scalar helper (avoid vector/tibble being assigned to [1,1])
as_scalar_num <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- suppressWarnings(as.numeric(x))
  if (length(x) > 1) x <- sum(x, na.rm = TRUE)
  if (!is.finite(x)) x <- default
  return(x)
}

# -----------------------------
# Penetration rates (choose scenario)
# -----------------------------
P_R_ACCII  <- read_csv("~/Downloads/PR_ACCII.csv")
P_R_Repeal <- read_csv("~/Downloads/PR_Repeal.csv")

# select one scenario to run
PR_table <- P_R_Repeal   # or change to P_R_Repeal

# reshape to wide for easier join
PR_wide <- PR_table %>%
  filter(Propulsion %in% c("BEV","PHEV")) %>%
  select(State, Year, Propulsion, Fraction) %>%
  pivot_wider(names_from = Propulsion, values_from = Fraction) %>%
  mutate(BEV  = coalesce(BEV, 0),
         PHEV = coalesce(PHEV, 0),
         ICE  = pmax(0, 1 - BEV - PHEV))

pr2020 <- PR_wide %>%
  filter(Year == 2021) %>%
  mutate(Year = 2020)


PR_wide <- bind_rows(PR_wide, pr2020) %>%
  arrange(State, Year)

# -----------------------------
# 1) Split population growth to Car/SUV by 2020 shares
# -----------------------------
seg_share_2020 <- state_type_share %>%
  filter(yearID == 2020) %>%
  transmute(State = state,
            Car = coalesce(type_share_Car,   0.5),
            SUV = coalesce(type_share_Truck, 0.5))

growth_seg <- growth_from_pop %>%
  transmute(State, Year, Growth_from_pop = coalesce(Growth_from_pop, 0)) %>%
  inner_join(seg_share_2020, by = "State") %>%
  pivot_longer(c(Car, SUV), names_to = "Segment", values_to = "seg_share") %>%
  mutate(Growth_seg = Growth_from_pop * seg_share) %>%
  select(State, Segment, Year, Growth_seg)

# -----------------------------
# 2) Initialize ICE stock at 2020 (Car/SUV split, age 0..49)
# -----------------------------
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
  transmute(State = state,
            Segment = if_else(Type == "Truck", "SUV", Type),
            ageID,
            age_frac = ageFraction_state_type) %>%
  filter(Segment %in% c("Car", "SUV"))

ice_init_0_49 <- regs_2020_ICE_seg %>%
  inner_join(frac_age_2020, by = c("State","Segment")) %>%
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
  k <- ice_keys[i,]
  vec <- ice_init_0_49 %>% filter(State==k$State, Segment==k$Segment) %>% arrange(ageID) %>% pull(N)
  if (length(vec) < 50) vec <- c(vec, rep(0, 50 - length(vec)))
  ice_env[[paste(k$State, k$Segment, sep = " | ")]] <- vec
}

# -----------------------------
# 3) Initialize EV engines with 2014–2019 history
# -----------------------------
ev_engines <- new.env(parent = emptyenv())

get_or_create_engine <- function(state, segment, propulsion) {
  key <- paste(state, segment, propulsion, sep = " | ")
  if (exists(key, envir = ev_engines, inherits = FALSE)) {
    return(get(key, envir = ev_engines, inherits = FALSE))
  }
  slice <- EV_historical %>%
    filter(State == state, `Global Segment` == segment, Propulsion == propulsion) %>%
    arrange(`Sale Year`)
  if (nrow(slice) == 0) slice <- tibble(`Sale Year` = 2014:2019, Sales = 0)
  eng <- EV_engine_init(slice, segment = segment, propulsion = propulsion,
                        lifetime_scen = "Baseline",
                        start_year = 2014, warmup_last_year = 2019)
  assign(key, eng, envir = ev_engines)
  return(eng)
}

states_ev   <- unique(EV_historical$State)
segments_ev <- c("Car", "SUV")
props_ev    <- c("BEV", "PHEV")
for (st in states_ev) {
  for (seg in segments_ev) {
    for (pp in props_ev) {
      slice <- EV_historical %>% filter(State==st, `Global Segment`==seg, Propulsion==pp)
      if (nrow(slice) == 0) next
      eng <- EV_engine_init(slice, segment = seg, propulsion = pp,
                            lifetime_scen = "Baseline",
                            start_year = 2014, warmup_last_year = 2019)
      assign(paste(st, seg, pp, sep = " | "), eng, envir = ev_engines)
    }
  }
}

# -----------------------------
# 4) Yearly loop
# -----------------------------
results_rows <- list()

for (yr in years) {
  
  # --- 4.1 ICE retirements and advance (without additions yet) ---
  ice_retire_df <- map_dfr(seq_len(nrow(ice_keys)), function(i) {
    k <- ice_keys[i,]; key <- paste(k$State, k$Segment, sep = " | ")
    N <- ice_env[[key]]
    surv <- surv_tbl_ice[[k$Segment]]
    y <- surv$y; names(y) <- as.character(surv$ageID)
    q <- 1 - y
    retire_by_age <- N * q[as.character(0:49)]
    survivors <- N * y[as.character(0:49)]
    N_next <- numeric(50); N_next[2:50] <- survivors[1:49]
    ice_env[[key]] <- N_next
    tibble(State = k$State, Segment = k$Segment, Year = yr,
           ret_ICE = sum(retire_by_age, na.rm = TRUE))
  })
  
  # --- 4.2 EV retirements only (first step with sales_y = 0) ---
  ev_retire_rows <- list()
  for (nm in ls(envir = ev_engines)) {
    eng <- get(nm, envir = ev_engines, inherits = FALSE)
    step1 <- EV_engine_step(eng, sales_y = 0)
    parts <- str_split(nm, " \\| ", simplify = TRUE)
    ev_retire_rows[[nm]] <- tibble(State = parts[1], Segment = parts[2], Propulsion = parts[3],
                                   Year = yr, ret_EV_pt = as_scalar_num(step1$EV_retired))
    assign(nm, step1$engine, envir = ev_engines)
  }
  ev_retire_df <- if (length(ev_retire_rows)) bind_rows(ev_retire_rows) else
    tibble(State=character(), Segment=character(), Propulsion=character(), Year=integer(), ret_EV_pt=double())
  
  ev_retire_seg <- ev_retire_df %>%
    group_by(State, Segment, Year) %>%
    summarise(ret_BEV  = sum(ret_EV_pt[Propulsion == "BEV"],  na.rm = TRUE),
              ret_PHEV = sum(ret_EV_pt[Propulsion == "PHEV"], na.rm = TRUE),
              .groups = "drop")
  
  # --- 4.3 Population growth for this year (Car/SUV) ---
  grow_now <- growth_seg %>% filter(Year == yr)
  
  # --- 4.4 Total demand per State×Segment and split to PT ---
  add_need <- ice_retire_df %>%
    left_join(ev_retire_seg, by = c("State","Segment","Year")) %>%
    left_join(grow_now,     by = c("State","Segment","Year")) %>%
    mutate(ret_BEV   = coalesce(ret_BEV, 0),
           ret_PHEV  = coalesce(ret_PHEV, 0),
           Growth_seg = coalesce(Growth_seg, 0),
           Demand_total = ret_ICE + ret_BEV + ret_PHEV + Growth_seg) %>%
    left_join(PR_wide, by = c("State","Year")) %>%
    mutate(add_BEV  = Demand_total * BEV,
           add_PHEV = Demand_total * PHEV,
           add_ICE  = Demand_total * ICE)
  
  # --- 4.5 Feed additions back to ICE (age 0) ---
  for (i in seq_len(nrow(ice_keys))) {
    k <- ice_keys[i,]; key <- paste(k$State, k$Segment, sep = " | ")
    N   <- ice_env[[key]]
    add0 <- add_need %>%
      filter(State == k$State, Segment == k$Segment, Year == yr) %>%
      summarise(val = sum(add_ICE, na.rm = TRUE)) %>% pull(val)
    add0 <- as_scalar_num(add0, 0)
    N[1] <- N[1] + add0
    ice_env[[key]] <- N
  }
  
  # --- 4.6 Feed additions back to EV engines (second step with real sales) ---
  for (st in unique(add_need$State)) {
    for (seg in c("Car","SUV")) {
      row_seg <- add_need %>% filter(State==st, Segment==seg, Year==yr)
      if (nrow(row_seg) == 0) next
      add_bev_seg  <- as_scalar_num(sum(row_seg$add_BEV,  na.rm = TRUE), 0)
      add_phev_seg <- as_scalar_num(sum(row_seg$add_PHEV, na.rm = TRUE), 0)
      
      eng_bev <- get_or_create_engine(st, seg, "BEV")
      step2b  <- EV_engine_step(eng_bev, sales_y = add_bev_seg)
      assign(paste(st, seg, "BEV", sep = " | "), step2b$engine, envir = ev_engines)
      
      eng_ph  <- get_or_create_engine(st, seg, "PHEV")
      step2p  <- EV_engine_step(eng_ph, sales_y = add_phev_seg)
      assign(paste(st, seg, "PHEV", sep = " | "), step2p$engine, envir = ev_engines)
    }
  }
  
  # --- 4.7 Save row-level results for this year ---
  results_rows[[as.character(yr)]] <- add_need %>%
    mutate(ret_EV = ret_BEV + ret_PHEV,
           Demand_total_check = add_BEV + add_PHEV + add_ICE) %>%
    select(State, Segment, Year,
           ret_ICE, ret_BEV, ret_PHEV, ret_EV,
           Growth_seg,
           add_BEV, add_PHEV, add_ICE,
           Demand_total = Demand_total_check)
}

# -----------------------------
# 5) Outputs
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
    .groups = "drop"
  ) %>%
  mutate(ret_EV = ret_BEV + ret_PHEV,
         Demand_total = add_BEV + add_PHEV + add_ICE)

dir.create("Outputs", showWarnings = FALSE)
write.csv(result_add_and_retire, "Outputs/ClosedLoop_AddRetire_byStateSegment2.csv", row.names = FALSE)
write.csv(result_state_total,   "Outputs/ClosedLoop_StateTotals2.csv", row.names = FALSE)
library(tidyverse)

library(tidyverse)



#graph
df <- read_csv("Outputs/ClosedLoop_StateTotals2.csv")


df <- df %>% filter(Year >= 2021, Year <= 2050)

df_long <- df %>%
  select(State, Year, add_BEV, add_PHEV, add_ICE,
         ret_BEV, ret_PHEV, ret_ICE) %>%
  pivot_longer(cols = starts_with(c("add","ret")),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Type = case_when(
    str_detect(Variable, "add") ~ "Additions",
    str_detect(Variable, "ret") ~ "Retirements"
  ),
  Powertrain = case_when(
    str_detect(Variable, "BEV")  ~ "BEV",
    str_detect(Variable, "PHEV") ~ "PHEV",
    str_detect(Variable, "ICE")  ~ "ICE"
  ))


ggplot(df_long, aes(x = Year, y = Value,
                    color = Powertrain, linetype = Type)) +
  geom_line(size = 0.9, alpha = 0.9) +
  facet_wrap(~ State, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("ICE"  = "#d95f02",   
                                "PHEV" = "#1b9e77",   
                                "BEV"  = "#317CB7")) + 
  labs(title = "Vehicle Additions and Retirements by State (2021–2050, Repealed)",
       y = "Number of Vehicles",
       x = "Year",
       color = "Powertrain",
       linetype = "Flow Type") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        strip.text = element_text(size = 8),
        plot.title = element_text(size = 14, face = "bold"))