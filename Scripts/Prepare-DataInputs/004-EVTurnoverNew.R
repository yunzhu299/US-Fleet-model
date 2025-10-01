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
ev_age_newLib  <- 8    # new LIB needed if no reuse after this EV age
max_reuse_lib  <- 0.5  # fraction of LIBs that can be reused
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
    ev_fail   = (1-y1)*y2*n_veh,
    lib_fail  = y1*(1-y2)*n_veh,
    none      = y1*y2*n_veh
  )
}

# -----------------------------
# 2) One simulation step (advance stock 1y, handle LIB reuse/recycling)
#    - Updates cohort matrix and returns integer flows + updated engine
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
  
  # propagate survival for all cohorts
  for (i in 1:31) {
    for (j in 1:31) {
      if (mat[i,j] != 0) {
        res <- f.getOutflows(mat[i,j], i-1, j-1,
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
  }
  
  # integer outflows by age
  ev_need_vec_raw   <- as.integer(round(rowSums(matrix_ev)))
  ev_retired_vec    <- as.integer(round(rowSums(matrix_lib) + rowSums(matrix_both)))
  ev_need_vec_raw[(max_ev_age+1):31] <- 0L
  
  lib_failed_vec      <- as.integer(round(colSums(matrix_ev)[-1]))
  lib_available_vec   <- as.integer(round(colSums(matrix_lib)))
  lib_failed_both_vec <- as.integer(round(colSums(matrix_both)[-1]))
  lib_recycling_vec   <- as.integer(lib_failed_vec + lib_failed_both_vec)
  
  # reuse eligibility & cap by LIB age
  lib_to_EV_vec <- as.integer(round(lib_available_vec * max_reuse_lib))
  if (max_lib_age_ev < 30) lib_to_EV_vec[(max_lib_age_ev+1):31] <- 0L
  lib_available_vec <- lib_available_vec - lib_to_EV_vec
  
  # offset reuse to line up EV ages that need a battery
  ev_need_ext   <- c(ev_need_vec_raw, rep(0L, ev_age_newLib))
  lib_to_EV_ext <- c(rep(0L, ev_age_newLib), lib_to_EV_vec)
  allocation    <- pmin(ev_need_ext, lib_to_EV_ext)
  
  reuse_offset_vec <- allocation[-(1:ev_age_newLib)]
  if (length(reuse_offset_vec) < 30)
    reuse_offset_vec <- c(reuse_offset_vec, rep(0L, 30-length(reuse_offset_vec)))
  
  ev_need_ext    <- ev_need_ext  - allocation
  lib_to_EV_ext  <- lib_to_EV_ext- allocation
  ev_need_vec    <- ev_need_ext[1:31]
  lib_to_EV_vec  <- lib_to_EV_ext[-(1:ev_age_newLib)]
  
  # cross-age greedy matching (older EV ages first, younger LIB ages first)
  lib_to_EV_before <- lib_to_EV_vec
  start_bat <- 1
  for (i in 31:1) {
    if (i > ev_age_newLib) {
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
  reuse_loop_vec  <- (lib_to_EV_before - lib_to_EV_after)[-1]
  if (length(reuse_loop_vec) < 30)
    reuse_loop_vec <- c(reuse_loop_vec, rep(0L, 30-length(reuse_loop_vec)))
  
  LIB_reuse_vector <- as.integer(reuse_offset_vec + reuse_loop_vec)
  LIB_reuse_EV     <- sum(LIB_reuse_vector)
  
  # new LIBs required (unmet needs after reuse)
  LIB_newadd_vector <- as.integer(ev_need_vec)
  LIB_new_add       <- sum(LIB_newadd_vector)
  
  # update stock: leftover LIBs stay in the pool (age tracked by column),
  # unmet needs get new LIBs at column 0; new sales enter at (0,0)
  lib_available_vec  <- lib_available_vec + lib_to_EV_vec
  new_matrix[,1]     <- new_matrix[,1] + as.integer(ev_need_vec)
  new_matrix[1,1]    <- new_matrix[1,1] + as.integer(round(sales_y))
  
  # totals
  EV_retired     <- sum(ev_retired_vec)
  LIB_recycling  <- sum(lib_recycling_vec)
  LIB_available  <- sum(lib_available_vec)
  
  engine$matrix <- new_matrix
  
  list(
    engine                 = engine,
    EV_retired             = EV_retired,
    LIB_recycling_vector   = lib_recycling_vec,
    LIB_recycling          = LIB_recycling,
    LIB_failed_only_vector = lib_failed_vec,
    LIB_bothfail_vector    = lib_failed_both_vec,
    LIB_available_vector   = lib_available_vec,
    LIB_available          = LIB_available,
    LIB_reuse_vector       = LIB_reuse_vector,
    LIB_reuse_EV           = LIB_reuse_EV,
    LIB_new_add            = LIB_new_add,
    LIB_newadd_vector      = LIB_newadd_vector,
    EV_stock_vector        = as.integer(round(rowSums(new_matrix))),
    EV_stock               = as.integer(round(sum(new_matrix)))
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