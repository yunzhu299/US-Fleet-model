# ICE & HEV Survival and new demand
## YZC Aug 2025
source("Scripts/00-Libraries.R", encoding = "UTF-8")
# ---------------------------
# 0) Survival (logistic)
#    S(a) = 1/(1 + exp((a - mu)/b)); annual y(a) = S(a+1)/S(a), ages 0..49
# ---------------------------
S_log <- function(age, mu, b) 1 / (1 + exp((age - mu) / b))

make_surv_tbl_0_49 <- function(mu, b) {
  S <- S_log(0:50, mu, b)
  y <- S[-1] / S[-length(S)]
  tibble(
    ageID = 0:49,
    y     = pmin(pmax(y, 1e-8), 0.999999),
    q     = 1 - y
  )
}

# Car and Truck survival tables (different parameters)
surv_tbl_by_type <- list(
  Car   = make_surv_tbl_0_49(mu = 16, b = 4),
  Truck = make_surv_tbl_0_49(mu = 19, b = 4.5)
)

# ---------------------------
# 1) Build 2020 state totals and split by Car/Truck share
#    - If HEV is present, include it in ICE pool (ICE+HEV)
# ---------------------------
regs_2020 <- regs %>%
  filter(Year == 2020) %>%
  mutate(HEV_col = dplyr::coalesce(`Hybrid Electric (HEV)`, 0),
         ICE_pool = Gasoline + HEV_col) %>%
  select(State, ICE_pool)

share_2020 <- state_type_share %>%
  filter(yearID == 2020) %>%
  select(State = state,
         share_Car   = type_share_Car,
         share_Truck = type_share_Truck)

totals_by_type <- regs_2020 %>%
  left_join(share_2020, by = "State") %>%
  # safety: if any share is NA, fall back to 50/50 (you can change this)
  mutate(share_Car   = coalesce(share_Car,   0.5),
         share_Truck = coalesce(share_Truck, 0.5)) %>%
  transmute(State,
            Car   = ICE_pool * share_Car,
            Truck = ICE_pool * share_Truck) %>%
  pivot_longer(c(Car, Truck), names_to = "Type", values_to = "TotalType")

# ---------------------------
# 2) Apply state×Type×age fractions to split totals into age bins
#    Input fractions: state_age_long_filled_by_type (yearID=2020)
#    Result: 0..30 (where 30 ≡ "30+")
# ---------------------------
frac_2020_by_type <- state_age_long_filled_by_type %>%
  filter(yearID == 2020) %>%
  select(State = state, Type, ageID, ageFraction_state_type)

stock_by_type_0_30 <- totals_by_type %>%
  inner_join(frac_2020_by_type, by = c("State", "Type")) %>%
  mutate(N = TotalType * ageFraction_state_type) %>%
  select(State, Type, ageID, N) %>%
  arrange(State, Type, ageID)

# ---------------------------
# 3) Tail weights 30+ → 30..50 per Type (use matching logistic params)
#    w(a) ∝ S(a) - S(a+1), a=30..49; w(50) ∝ S(50); normalize to 1
# ---------------------------
make_tail_weights <- function(mu, b, Type) {
  mass_30_49 <- S_log(30:49, mu, b) - S_log(31:50, mu, b)
  mass_50    <- S_log(50,    mu, b)
  w_raw      <- c(mass_30_49, mass_50)
  w          <- if (sum(w_raw) > 0) w_raw / sum(w_raw) else rep(1/21, 21)
  tibble(Type = Type, ageID = 30:50, w = as.numeric(w))
}

tail_w_tbl <- bind_rows(
  make_tail_weights(mu = 16, b = 4, Type = "Car"),
  make_tail_weights(mu = 20, b = 5, Type = "Truck")
)

# Expand 30+ per Type and build turnover/vintage sets
plus30 <- stock_by_type_0_30 %>%
  filter(ageID == 30) %>%
  select(State, Type, N30 = N)

spread_30_50 <- plus30 %>%
  inner_join(tail_w_tbl, by = "Type") %>%
  mutate(N = N30 * w) %>%
  select(State, Type, ageID, N)

df50 <- stock_by_type_0_30 %>%
  filter(ageID <= 29) %>%
  bind_rows(spread_30_50) %>%
  arrange(State, Type, ageID)

stock_turnover_0_49_Type <- df50 %>%
  filter(ageID <= 49) %>%
  select(State, Type, ageID, N)

vintage_init_by_state_Type <- df50 %>%
  filter(ageID >= 50) %>%
  group_by(State, Type) %>%
  summarise(vintage_2020 = sum(N, na.rm = TRUE), .groups = "drop")

# ---------------------------
# 4) Simulator by Type (Type-specific survival tables)
#    - Ages 0..49 participate; 49→vintage (>=50)
#    - No new sales here (retirement-only)
# ---------------------------
simulate_retire_turnover_by_type <- function(stock_0_49_Type,
                                             surv_tbl_by_type,
                                             years = 2020:2035,
                                             vintage_init_Type = vintage_init_by_state_Type) {
  keys <- stock_0_49_Type %>% distinct(State, Type) %>% arrange(State, Type)
  
  map_dfr(seq_len(nrow(keys)), function(i) {
    st <- keys$State[i]; tp <- keys$Type[i]
    
    base <- stock_0_49_Type %>% filter(State == st, Type == tp) %>% arrange(ageID)
    full <- tibble(ageID = 0:49) %>%
      left_join(base, by = "ageID") %>%
      mutate(State = st, Type = tp, N = coalesce(N, 0))
    N <- full$N
    
    surv_tbl <- surv_tbl_by_type[[tp]]
    y_vec <- surv_tbl$y; names(y_vec) <- as.character(surv_tbl$ageID)
    q_vec <- 1 - y_vec
    
    vint0 <- vintage_init_Type %>% filter(State == st, Type == tp) %>% pull(vintage_2020)
    if (!length(vint0)) vint0 <- 0
    vintage <- vint0
    
    out <- vector("list", length(years)); names(out) <- years
    for (k in seq_along(years)) {
      yr <- years[k]
      
      retire_by_age <- N * q_vec[as.character(0:49)]
      retire_total  <- sum(retire_by_age, na.rm = TRUE)
      
      survivors <- N * y_vec[as.character(0:49)]
      N_next <- numeric(length(N))
      N_next[2:50] <- survivors[1:49]    # ages 0..48 → 1..49
      vintage <- vintage + survivors[50] # age 49 → vintage
      
      out[[k]] <- tibble(State = st, Type = tp, Year = yr,
                         retire_total = retire_total,
                         vintage_stock = vintage)
      N <- N_next
    }
    bind_rows(out)
  })
}

# ---------------------------
# 5) Run and aggregate
# ---------------------------
retire_state_year_Type <- simulate_retire_turnover_by_type(
  stock_turnover_0_49_Type,
  surv_tbl_by_type,
  years = 2020:2050,
  vintage_init_Type = vintage_init_by_state_Type
)

# (a) Keep split by Type (Car vs Truck)
# head(retire_state_year_Type)

# (b) Total across Types
retire_state_year_total <- retire_state_year_Type %>%
  group_by(State, Year) %>%
  summarise(retire_total = sum(retire_total, na.rm = TRUE),
            vintage_stock = sum(vintage_stock, na.rm = TRUE),
            .groups = "drop")

# (c) National totals by Type and overall
retire_US_by_Type <- retire_state_year_Type %>%
  group_by(Year, Type) %>%
  summarise(retire_total_US = sum(retire_total, na.rm = TRUE),
            vintage_US      = sum(vintage_stock, na.rm = TRUE),
            .groups = "drop")

retire_US_total <- retire_state_year_total %>%
  group_by(Year) %>%
  summarise(retire_total_US = sum(retire_total, na.rm = TRUE),
            vintage_US      = sum(vintage_stock, na.rm = TRUE),
            .groups = "drop")

# Quick looks
# head(stock_turnover_0_49_Type)
# head(vintage_init_by_state_Type)
# head(retire_state_year_Type)
# retire_US_by_Type
# retire_US_total
