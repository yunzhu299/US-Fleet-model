## Survival Curve based on Logistic/Normal Distribution
# Calculates dynamics for each region based on survival curves
# Results in detailed outflows of EVs and LIBs, additional requirements,
# as well as LIB outflows to EVs, SSPS and recycling
## YZC Sep 2025 (based on PBH Jan 2024 version)

library(tibble)
library(dplyr)
library(readr)
library(stringr)

# -----------------------------
# 0) Parameters
# -----------------------------
ev_age_newLib <- 8     # year when a new battery is needed if no reuse
max_reuse_lib <- 0.5   # fraction of LIBs that can be reused
max_ev_age    <- 20
max_lib_age_ev<- 12

life_param <- tibble(
  Vehicle = c("Car","SUV"),
  mean_ev = c(17,17),
  sd_ev   = c(4,4),
  mean_lib= c(15,10),
  sd_lib  = c(4,4),
  scen_lifetime="Baseline"
)

# -----------------------------
# 1) Outflow probability helper
# -----------------------------
f.getOutflows <- function(n_veh=1, EV_age, LIB_age,
                          maxEV_age=30, maxLIB_age=30,
                          dist.Age="Logistic",
                          mean_ev=17, sd_ev=4,
                          mean_lib=15, sd_lib=4) {
  
  if (dist.Age=="Normal") {
    y1 <- (1-pnorm(EV_age+1, mean_ev, sd_ev)) /
      (1-pnorm(EV_age,   mean_ev, sd_ev))
    y2 <- (1-pnorm(LIB_age+1, mean_lib, sd_lib)) /
      (1-pnorm(LIB_age,   mean_lib, sd_lib))
  } else { # Logistic
    y1 <- (1-plogis(EV_age+1, mean_ev, sd_ev*sqrt(3)/pi)) /
      (1-plogis(EV_age,   mean_ev, sd_ev*sqrt(3)/pi))
    y2 <- (1-plogis(LIB_age+1, mean_lib, sd_lib*sqrt(3)/pi)) /
      (1-plogis(LIB_age,   mean_lib, sd_lib*sqrt(3)/pi))
  }
  
  if (EV_age>=maxEV_age)  y1 <- 0
  if (LIB_age>=maxLIB_age) y2 <- 0
  
  ret <- tibble(
    both_fail=(1-y1)*(1-y2)*n_veh,
    ev_fail  =(1-y1)*y2*n_veh,
    lib_fail =y1*(1-y2)*n_veh,
    none     =y1*y2*n_veh
  )
  return(ret)
}

# -----------------------------
# 2) EV Engine Initialization
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
  
  for (y in start_year:warmup_last_year) {
    sales_y <- ev_hist_slice %>% filter(`Sale Year`==y) %>% pull(Sales)
    if (length(sales_y)==0) sales_y <- 0
    mat[1,1] <- mat[1,1] + sales_y
  }
  
  list(matrix=mat,
       mean_ev=mean_ev, sd_ev=sd_ev,
       mean_lib=mean_lib, sd_lib=sd_lib,
       scen=lifetime_scen,
       segment=segment,
       propulsion=propulsion)
}

# -----------------------------
# 3) EV Engine Step
# -----------------------------
EV_engine_step <- function(engine, sales_y=0,
                           ev_age_newLib=8,
                           max_reuse_lib=0.5,
                           max_ev_age=20,
                           max_lib_age_ev=12) {
  
  mat <- engine$matrix
  new_matrix <- matrix(0, nrow=31, ncol=31,
                       dimnames=list(paste0("EV_",0:30), paste0("LIB_",0:30)))
  matrix_ev <- new_matrix; matrix_lib <- new_matrix; matrix_both <- new_matrix
  
  # --- loop through stock matrix ---
  for (i in 1:31) {
    for (j in 1:31) {
      if (mat[i,j] != 0) {
        result <- f.getOutflows(mat[i,j], i-1, j-1,
                                maxEV_age=30, maxLIB_age=30,
                                dist.Age="Logistic",
                                mean_ev=engine$mean_ev, sd_ev=engine$sd_ev,
                                mean_lib=engine$mean_lib, sd_lib=engine$sd_lib)
        if (i!=31 & j!=31) {
          new_matrix[i+1,j+1] <- new_matrix[i+1,j+1] + result$none
          matrix_ev[i+1,j+1]  <- matrix_ev[i+1,j+1]  + result$lib_fail
          matrix_lib[i+1,j+1] <- matrix_lib[i+1,j+1] + result$ev_fail
          matrix_both[i+1,j+1]<- matrix_both[i+1,j+1]+ result$both_fail
        } else if (j==31 & i!=31) {
          matrix_ev[i+1,j] <- matrix_ev[i+1,j] + result$lib_fail
        } else if (i==31 & j!=31) {
          matrix_lib[i,j+1] <- matrix_lib[i,j+1] + result$ev_fail
        }
      }
    }
  }
  
  # --- outflows ---
  ev_need    <- rowSums(matrix_ev)
  ev_retired <- rowSums(matrix_lib) + rowSums(matrix_both)
  ev_retired_total <- sum(ev_retired)
  
  ev_need[(max_ev_age+1):31] <- 0
  
  lib_failed        <- colSums(matrix_ev)[-1]   # battery failed only
  lib_available     <- colSums(matrix_lib)      # EV failed but LIB ok
  lib_failed_both   <- colSums(matrix_both)[-1] # EV+LIB failed together
  lib_recycling_vec <- lib_failed + lib_failed_both
  
  # --- reuse allocation ---
  lib_to_EV <- lib_available * max_reuse_lib
  lib_to_EV[(max_lib_age_ev+1):31] <- 0
  lib_available <- lib_available - lib_to_EV
  
  ev_need  <- c(ev_need, rep(0, ev_age_newLib))
  lib_to_EV <- c(rep(0, ev_age_newLib), lib_to_EV)
  allocation <- pmin(ev_need, lib_to_EV)
  
  reuse_offset_vec <- allocation[-(1:ev_age_newLib)]
  if (length(reuse_offset_vec)<30) reuse_offset_vec <- c(reuse_offset_vec, rep(0, 30-length(reuse_offset_vec)))
  
  ev_need   <- ev_need - allocation
  lib_to_EV <- lib_to_EV - allocation
  ev_need   <- ev_need[1:31]
  lib_to_EV <- lib_to_EV[-(1:ev_age_newLib)]
  
  lib_to_EV_before <- lib_to_EV
  start_bat <- 1
  for (i in 31:1) {
    if (i > ev_age_newLib) {
      for (j in start_bat:31) {
        allocated <- min(ev_need[i], lib_to_EV[j])
        ev_need[i] <- ev_need[i] - allocated
        lib_to_EV[j] <- lib_to_EV[j] - allocated
        new_matrix[i,j] <- new_matrix[i,j] + allocated
        start_bat <- j
        if (ev_need[i]==0) break
      }
    }
  }
  
  lib_to_EV_after <- lib_to_EV
  reuse_loop_vec  <- (lib_to_EV_before - lib_to_EV_after)[-1]
  if (length(reuse_loop_vec)<30) reuse_loop_vec <- c(reuse_loop_vec, rep(0,30-length(reuse_loop_vec)))
  
  reuse_vec <- reuse_offset_vec + reuse_loop_vec
  reuse_total <- sum(reuse_vec)
  
  # --- update stock ---
  lib_available <- lib_available + lib_to_EV
  new_matrix[,1] <- new_matrix[,1] + ev_need
  new_matrix[1,1] <- new_matrix[1,1] + sales_y
  
  engine$matrix <- new_matrix
  
  return(list(engine=engine,
              EV_retired=ev_retired_total,
              LIB_recycling_vector=round(lib_recycling_vec,0),
              LIB_recycling=sum(lib_recycling_vec),
              LIB_failed_only_vector=round(lib_failed,0),
              LIB_bothfail_vector=round(lib_failed_both,0),
              LIB_available_vector=round(lib_available,0),
              LIB_available=sum(lib_available),
              LIB_reuse_vector=round(reuse_vec,0),
              LIB_reuse_EV=round(reuse_total,0),
              EV_stock_vector=round(rowSums(new_matrix)[-1],0),
              EV_stock=round(sum(new_matrix),0)))
}
