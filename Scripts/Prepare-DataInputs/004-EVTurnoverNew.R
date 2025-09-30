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
#    Returns fractions that: both fail / EV fails / LIB fails / none
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
#    - Writes a NEW stock matrix based on last year's matrix + new sales
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
  
  # propagate all cohorts through survival
  for (i in 1:31) {       # EV age index (1 ≡ age 0)
    for (j in 1:31) {     # LIB age index (1 ≡ age 0)
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
  
  # outflows & reuse
  ev_need         <- rowSums(matrix_ev)
  ev_retired      <- rowSums(matrix_lib) + rowSums(matrix_both)
  ev_retired_total<- sum(ev_retired)
  
  ev_need[(max_ev_age+1):31] <- 0
  
  lib_failed        <- colSums(matrix_ev)[-1]   # LIB fails (EV survives)
  lib_available     <- colSums(matrix_lib)      # EV fails (LIB ok)
  lib_failed_both   <- colSums(matrix_both)[-1] # EV+LIB fail together
  lib_recycling_vec <- lib_failed + lib_failed_both
  
  # reuse: limit LIB age eligible for EV
  lib_to_EV <- lib_available * max_reuse_lib
  lib_to_EV[(max_lib_age_ev+1):31] <- 0
  lib_available <- lib_available - lib_to_EV
  
  # align reuse with EV need with offset (new LIB needed at ev_age_newLib)
  ev_need    <- c(ev_need, rep(0, ev_age_newLib))
  lib_to_EV  <- c(rep(0, ev_age_newLib), lib_to_EV)
  allocation <- pmin(ev_need, lib_to_EV)
  
  reuse_offset_vec <- allocation[-(1:ev_age_newLib)]
  if (length(reuse_offset_vec) < 30)
    reuse_offset_vec <- c(reuse_offset_vec, rep(0, 30-length(reuse_offset_vec)))
  
  ev_need   <- ev_need - allocation
  lib_to_EV <- lib_to_EV - allocation
  ev_need   <- ev_need[1:31]
  lib_to_EV <- lib_to_EV[-(1:ev_age_newLib)]
  
  lib_to_EV_before <- lib_to_EV
  start_bat <- 1
  for (i in 31:1) {
    if (i > ev_age_newLib) {
      for (j in start_bat:31) {
        allocated   <- min(ev_need[i], lib_to_EV[j])
        ev_need[i]  <- ev_need[i] - allocated
        lib_to_EV[j]<- lib_to_EV[j] - allocated
        new_matrix[i,j] <- new_matrix[i,j] + allocated
        start_bat <- j
        if (ev_need[i] == 0) break
      }
    }
  }
  
  lib_to_EV_after <- lib_to_EV
  reuse_loop_vec  <- (lib_to_EV_before - lib_to_EV_after)[-1]
  if (length(reuse_loop_vec) < 30)
    reuse_loop_vec <- c(reuse_loop_vec, rep(0, 30-length(reuse_loop_vec)))
  
  reuse_vec   <- reuse_offset_vec + reuse_loop_vec
  reuse_total <- sum(reuse_vec)
  
  # add remaining: new LIBs for unmet EV need + new sales at (0,0)
  lib_available  <- lib_available + lib_to_EV
  new_matrix[,1] <- new_matrix[,1] + ev_need
  new_matrix[1,1]<- new_matrix[1,1] + sales_y
  
  engine$matrix <- new_matrix
  
  return(list(
    engine                  = engine,
    EV_retired              = ev_retired_total,
    LIB_recycling_vector    = round(lib_recycling_vec,0),
    LIB_recycling           = sum(lib_recycling_vec),
    LIB_failed_only_vector  = round(lib_failed,0),
    LIB_bothfail_vector     = round(lib_failed_both,0),
    LIB_available_vector    = round(lib_available,0),
    LIB_available           = sum(lib_available),
    LIB_reuse_vector        = round(reuse_vec,0),
    LIB_reuse_EV            = round(reuse_total,0),
    EV_stock_vector         = round(rowSums(new_matrix),0),  # include age 0
    EV_stock_age0           = round(sum(new_matrix[1,]),0),  # explicit age 0
    EV_stock                = round(sum(new_matrix),0)
  ))
}

# -----------------------------
# 3) Engine initialization with REAL warm-up (2014–2019)
#    - Feeds each year's actual sales through EV_engine_step once
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
  
  # REAL warm-up, year by year (no aggregation)
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



