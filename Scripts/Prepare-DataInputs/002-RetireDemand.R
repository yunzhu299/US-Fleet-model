# ICE Survival and new demand
## YZC Aug 2025
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# ---------------------------
# 1) Build 2020 State × Age stock from totals × fractions
#    Output: stock_2020_state_age (State, ageID=0..30, N; 30 == "30+")
# ---------------------------
build_stock_2020_state_age <- function(regs, state_age_long_filled) {
  regs_2020 <- regs %>%
    filter(Year == 2020) %>%
    select(State, Gasoline)
  
  frac_2020 <- state_age_long_filled %>%
    filter(yearID == 2020) %>%
    select(State = state, ageID, ageFraction_state)
  
  regs_2020 %>%
    inner_join(frac_2020, by = "State") %>%
    mutate(N = Gasoline * ageFraction_state) %>%
    select(State, ageID, N) %>%
    arrange(State, ageID)
}

stock_2020_state_age <- build_stock_2020_state_age(regs, state_age_long_filled)

# ---------------------------
# 2) Logistic survival (μ=16, b=4)
#    S(a) = 1 / (1 + exp((a - mu)/b))
#    Annual survival y(a) = S(a+1)/S(a), we need ages 0..49
# ---------------------------
mu_comp <- 16
b_comp  <- 4

S_log <- function(age, mu = mu_comp, b = b_comp) {
  1 / (1 + exp((age - mu) / b))
}

make_surv_tbl_0_49 <- function(mu = mu_comp, b = b_comp) {
  S <- S_log(0:50, mu, b)
  y <- S[-1] / S[-length(S)]
  tibble(ageID = 0:49,
         y     = pmin(pmax(y, 1e-8), 0.999999),  # numeric safety
         q     = 1 - y)
}

surv_tbl <- make_surv_tbl_0_49()

# ---------------------------
# 3) Split "30+" into 30..50 using logistic tail weights
#    weight for a=30..49: S(a)-S(a+1); for 50+: S(50); normalized to sum=1
#    Build turnover stock (0..49) and initial vintage (>=50)
# ---------------------------
make_tail_weights_30_50 <- function(mu = mu_comp, b = b_comp) {
  mass_30_49 <- S_log(30:49, mu, b) - S_log(31:50, mu, b)
  mass_50    <- S_log(50, mu, b)
  w_raw <- c(mass_30_49, mass_50)
  w     <- if (sum(w_raw) > 0) w_raw / sum(w_raw) else rep(1/21, 21)
  tibble(ageID = 30:50, w = as.numeric(w))
}

tail_w <- make_tail_weights_30_50()

build_stock_0_49_and_vintage <- function(stock_2020_state_age, tail_w) {
  base   <- stock_2020_state_age
  plus30 <- base %>% filter(ageID == 30) %>% select(State, N30 = N)
  
  # 30+ → 30..50
  spread_30_50 <- plus30 %>%
    crossing(tail_w) %>%
    mutate(N = N30 * w) %>%
    select(State, ageID, N)
  
  df50 <- base %>%
    filter(ageID <= 29) %>%
    bind_rows(spread_30_50) %>%
    arrange(State, ageID)
  
  stock_turnover_0_49 <- df50 %>%
    filter(ageID <= 49) %>%
    select(State, ageID, N)
  
  vintage_init_by_state <- df50 %>%
    filter(ageID >= 50) %>%
    group_by(State) %>%
    summarise(vintage_2020 = sum(N, na.rm = TRUE), .groups = "drop")
  
  list(stock_turnover_0_49   = stock_turnover_0_49,
       vintage_init_by_state = vintage_init_by_state)
}

init_objs <- build_stock_0_49_and_vintage(stock_2020_state_age, tail_w)
stock_turnover_0_49   <- init_objs$stock_turnover_0_49
vintage_init_by_state <- init_objs$vintage_init_by_state

# ---------------------------
# 4) Retirement-only roll-forward (0..49 participate; 49→vintage; vintage accumulates)
# ---------------------------
simulate_retire_turnover_0_49 <- function(stock_0_49,
                                          surv_tbl_0_49,
                                          years = 2020:2035,
                                          vintage_init = vintage_init_by_state) {
  y_vec <- surv_tbl_0_49$y; names(y_vec) <- as.character(surv_tbl_0_49$ageID)
  q_vec <- 1 - y_vec
  states <- sort(unique(stock_0_49$State))
  
  map_dfr(states, function(st) {
    base <- stock_0_49 %>% filter(State == st) %>% arrange(ageID)
    full <- tibble(ageID = 0:49) %>%
      left_join(base, by = "ageID") %>%
      mutate(State = st, N = coalesce(N, 0))
    N <- full$N
    
    vint0 <- vintage_init %>% filter(State == st) %>% pull(vintage_2020)
    if (!length(vint0)) vint0 <- 0
    vintage <- vint0
    
    out <- vector("list", length(years)); names(out) <- years
    for (i in seq_along(years)) {
      yr <- years[i]
      
      retire_by_age <- N * q_vec[as.character(0:49)]
      retire_total  <- sum(retire_by_age, na.rm = TRUE)
      
      survivors <- N * y_vec[as.character(0:49)]
      N_next <- numeric(length(N))
      N_next[2:50] <- survivors[1:49]    # 0..48 → 1..49
      vintage <- vintage + survivors[50] # 49 → vintage (>=50)
      
      out[[i]] <- tibble(State = st, Year = yr,
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
retire_state_year <- simulate_retire_turnover_0_49(
  stock_turnover_0_49,
  surv_tbl,
  years = 2020:2050,
  vintage_init = vintage_init_by_state
)

retire_US <- retire_state_year %>%
  group_by(Year) %>%
  summarise(retire_total_US = sum(retire_total, na.rm = TRUE),
            vintage_US      = sum(vintage_stock, na.rm = TRUE),
            .groups = "drop")

# Inspect
# head(stock_2020_state_age)
# head(stock_turnover_0_49)
# head(vintage_init_by_state)
# head(retire_state_year)
retire_US