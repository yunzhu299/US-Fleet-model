## 04 — EV survival engine (Logistic/Normal) with LIB reuse/recycling
## Warm-up year by year using REAL sales from EV_historical (2014–2019)
## YZC Sep 2025

library(tibble)
library(dplyr)
library(readr)
library(stringr)

# Historical EV sales (state × segment × propulsion × year)
EV_historical <- read_csv("~/Downloads/historical_state_pt_veh_df.csv")

# -----------------------------
# 0) Parameters
# -----------------------------
ev_age_newLib  <- 8    # reuse is allowed starting at EV age >= 8
max_reuse_lib  <- 0.5  # fraction of good LIBs allowed to be reused
max_ev_age     <- 20
max_lib_age_ev <- 12

life_param <- tibble(
  Vehicle  = c("Car","SUV"),
  mean_ev  = c(17,17),
  sd_ev    = c(4,4),
  mean_lib = c(15,10),
  sd_lib   = c(4,4),
  scen_lifetime = "Baseline"
)

# -----------------------------
# 1) Outflow probability helper
#    Returns fractions: both fail / EV fails / LIB fails / none
# -----------------------------
f.getOutflows <- function(n_veh=1, EV_age, LIB_age,
                          maxEV_age=30, maxLIB_age=30,
                          dist.Age="Logistic",
                          mean_ev=17, sd_ev=4,
                          mean_lib=15, sd_lib=4) {
  
  if (dist.Age=="Normal") {
    y1 <- (1 - pnorm(EV_age+1, mean_ev,  sd_ev)) / (1 - pnorm(EV_age,   mean_ev,  sd_ev))
    y2 <- (1 - pnorm(LIB_age+1, mean_lib, sd_lib)) / (1 - pnorm(LIB_age, mean_lib, sd_lib))
  } else { # Logistic
    sdev <- sd_ev  * sqrt(3)/pi
    sdli <- sd_lib * sqrt(3)/pi
    y1 <- (1 - plogis(EV_age+1, mean_ev,  sdev)) / (1 - plogis(EV_age,   mean_ev,  sdev))
    y2 <- (1 - plogis(LIB_age+1, mean_lib, sdli)) / (1 - plogis(LIB_age, mean_lib, sdli))
  }
  
  if (EV_age>=maxEV_age)  y1 <- 0
  if (LIB_age>=maxLIB_age) y2 <- 0
  
  tibble(
    both_fail = (1-y1)*(1-y2)*n_veh,
    ev_fail   = (1-y1)*y2*n_veh,  # EV fails, LIB ok (good LIB becomes available)
    lib_fail  = y1*(1-y2)*n_veh,  # LIB fails, EV ok (EV needs a replacement LIB)
    none      = y1*y2*n_veh
  )
}

# -----------------------------
# 2) One simulation step (advance stock 1y, handle LIB reuse/recycling)
#    - Updates the cohort matrix
#    - Returns flows + updated engine
# -----------------------------
EV_engine_step <- function(engine, sales_y = 0,
                           ev_age_newLib = 8,
                           max_reuse_lib = 0.5,
                           max_ev_age    = 20,
                           max_lib_age_ev= 12) {
  
  mat <- engine$matrix
  new_matrix <- matrix(0, nrow=31, ncol=31,
                       dimnames=list(paste0("EV_",0:30), paste0("LIB_",0:30)))
  matrix_ev   <- new_matrix
  matrix_lib  <- new_matrix
  matrix_both <- new_matrix
  
  # propagate survival of all cohorts
  for (i in 1:31) {       # EV age index (1 ≡ age 0)
    for (j in 1:31) {     # LIB age index (1 ≡ age 0)
      n <- mat[i,j]; if (n==0) next
      res <- f.getOutflows(n, i-1, j-1,
                           maxEV_age=30, maxLIB_age=30,
                           dist.Age="Logistic",
                           mean_ev=engine$mean_ev,  sd_ev=engine$sd_ev,
                           mean_lib=engine$mean_lib, sd_lib=engine$sd_lib)
      if (i!=31 & j!=31) {
        new_matrix[i+1,j+1] <- new_matrix[i+1,j+1] + res$none
        matrix_ev[i+1,j+1]  <- matrix_ev[i+1,j+1]  + res$lib_fail
        matrix_lib[i+1,j+1] <- matrix_lib[i+1,j+1] + res$ev_fail
        matrix_both[i+1,j+1]<- matrix_both[i+1,j+1]+ res$both_fail
      } else if (j==31 & i!=31) {
        matrix_ev[i+1,j] <- matrix_ev[i+1,j] + res$lib_fail
      } else if (i==31 & j!=31) {
        matrix_lib[i,j+1] <- matrix_lib[i,j+1] + res$ev_fail
      }
    }
  }
  
  # integer vectors
  ev_need_vec_raw   <- as.integer(round(rowSums(matrix_ev)))
  ev_retired_vec    <- as.integer(round(rowSums(matrix_lib) + rowSums(matrix_both)))
  ev_need_vec_raw[(max_ev_age+1):31] <- 0L
  
  lib_failed_vec      <- as.integer(round(colSums(matrix_ev)[-1]))   # ages 1..30
  lib_failed_both_vec <- as.integer(round(colSums(matrix_both)[-1])) # ages 1..30
  lib_recycling_vec   <- lib_failed_vec + lib_failed_both_vec        # ages 1..30
  
  # good LIBs by LIB age (before reuse split) — ages 0..30
  good_libs <- as.integer(round(colSums(matrix_lib)))
  
  # split into allowed-to-reuse and base reserved (PRE-REUSE; available should include reused)
  allowed_for_reuse <- good_libs
  if (max_lib_age_ev < 30) allowed_for_reuse[(max_lib_age_ev+1):31] <- 0L
  allowed_for_reuse <- as.integer(round(allowed_for_reuse * max_reuse_lib))  # ages 0..30
  base_reserved     <- as.integer(round(good_libs - allowed_for_reuse))      # ages 0..30
  
  # PRE-REUSE available (requested): include the portion that may be reused this year
  LIB_available_vector <- base_reserved + allowed_for_reuse                # ages 0..30
  LIB_available        <- sum(LIB_available_vector)
  
  # offset: LIB age a → EV age a + ev_age_newLib
  ev_need_ext   <- c(ev_need_vec_raw, rep(0L, ev_age_newLib))
  reuse_cap_ext <- c(rep(0L, ev_age_newLib), allowed_for_reuse)
  allocation    <- pmin(ev_need_ext, reuse_cap_ext)
  
  reuse_offset_vec <- allocation[-(1:ev_age_newLib)]
  if (length(reuse_offset_vec) < 30)
    reuse_offset_vec <- c(reuse_offset_vec, rep(0L, 30-length(reuse_offset_vec)))
  
  ev_need_ext    <- ev_need_ext   - allocation
  reuse_cap_ext  <- reuse_cap_ext - allocation
  ev_need_vec    <- ev_need_ext[1:31]
  allowed_for_reuse <- reuse_cap_ext[-(1:ev_age_newLib)]  # remaining allowed after offset
  
  # cross-age reuse (youngest LIB first), allow when EV age >= ev_age_newLib
  reuse_loop_by_LIBage <- integer(31)
  for (i in 31:1) {
    if (i >= (ev_age_newLib+1)) {  # i index is age+1, so >= maps to EV age >= ev_age_newLib
      for (j in 1:31) {
        if (ev_need_vec[i] <= 0L) break
        take <- min(ev_need_vec[i], allowed_for_reuse[j])
        if (take > 0L) {
          ev_need_vec[i]           <- ev_need_vec[i] - take
          allowed_for_reuse[j]     <- allowed_for_reuse[j] - take
          new_matrix[i, j]         <- new_matrix[i, j] + take
          reuse_loop_by_LIBage[j]  <- reuse_loop_by_LIBage[j] + take
        }
      }
    }
  }
  
  # reuse vectors (LIB ages 1..30)
  LIB_reuse_vector <- as.integer(reuse_offset_vec + reuse_loop_by_LIBage[-1])  # 1..30
  LIB_reuse_EV     <- sum(LIB_reuse_vector)
  
  # new LIBs for replacement only (NO new-car batteries)
  LIB_newadd_vector <- as.integer(ev_need_vec)   # EV ages 0..30
  LIB_new_add       <- sum(LIB_newadd_vector)
  
  # update stock: add replacement LIBs to LIB_0; add new-car sales to (0,0)
  new_matrix[,1] <- new_matrix[,1] + LIB_newadd_vector
  new_matrix[1,1] <- new_matrix[1,1] + as.integer(round(sales_y))
  
  EV_stock_vector <- as.integer(round(rowSums(new_matrix)))
  EV_stock        <- sum(EV_stock_vector)
  
  engine$matrix <- new_matrix
  
  list(
    engine = engine,
    EV_retired = sum(ev_retired_vec),
    LIB_recycling = sum(lib_recycling_vec),
    LIB_available = LIB_available,                     # PRE-REUSE
    LIB_reuse_EV  = LIB_reuse_EV,
    LIB_new_add   = LIB_new_add,
    EV_stock      = EV_stock,
    LIB_recycling_vector = lib_recycling_vec,          # ages 1..30
    LIB_available_vector = LIB_available_vector,       # ages 0..30 (PRE-REUSE, includes reused)
    LIB_reuse_vector     = LIB_reuse_vector,           # ages 1..30
    LIB_newadd_vector    = LIB_newadd_vector,          # EV ages 0..30
    EV_stock_vector      = EV_stock_vector             # EV ages 0..30
  )
}

# -----------------------------
# 3) Engine initialization with REAL warm-up (2014–2019)
#    - Feeds each year's actual sales through EV_engine_step
# -----------------------------
EV_engine_init <- function(ev_hist_slice, segment, propulsion,
                           lifetime_scen="Baseline",
                           start_year=2014, warmup_last_year=2019) {
  
  mean_ev  <- life_param %>% filter(scen_lifetime==lifetime_scen, Vehicle==segment) %>% pull(mean_ev)
  sd_ev    <- life_param %>% filter(scen_lifetime==lifetime_scen, Vehicle==segment) %>% pull(sd_ev)
  mean_lib <- life_param %>% filter(scen_lifetime==lifetime_scen, Vehicle==segment) %>% pull(mean_lib)
  sd_lib   <- life_param %>% filter(scen_lifetime==lifetime_scen, Vehicle==segment) %>% pull(sd_lib)
  
  mat <- matrix(0, nrow=31, ncol=31,
                dimnames=list(paste0("EV_",0:30), paste0("LIB_",0:30)))
  
  engine <- list(matrix=mat,
                 mean_ev=mean_ev, sd_ev=sd_ev,
                 mean_lib=mean_lib, sd_lib=sd_lib,
                 scen=lifetime_scen,
                 segment=segment,
                 propulsion=propulsion)
  
  for (y in start_year:warmup_last_year) {
    sales_y <- ev_hist_slice %>% filter(`Sale Year`==y) %>% pull(Sales)
    if (length(sales_y)==0) sales_y <- 0
    step   <- EV_engine_step(engine, sales_y=sales_y,
                             ev_age_newLib=ev_age_newLib,
                             max_reuse_lib=max_reuse_lib,
                             max_ev_age=max_ev_age,
                             max_lib_age_ev=max_lib_age_ev)
    engine <- step$engine
  }
  
  engine
}