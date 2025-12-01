## 04 — EV survival engine (Logistic/Normal) with LIB reuse / repurpose / recycling
## Warm-up year by year using REAL sales from EV_historical (2014–2019)
## Export adjustment: shrink domestic reuse by factor `reuse_shrink`
## YZC + ChatGPT Nov 2025 - FIXED VERSION

library(tibble)
library(dplyr)
library(readr)
library(stringr)

# Historical EV sales (state × segment × propulsion × year)
EV_historical <- read_csv("~/Downloads/historical_state_pt_veh_df.csv")
X2020_2025sales <- readxl::read_excel("~/Downloads/2020-2025sales.xlsx", sheet = "Sheet1")

# -----------------------------
# 0) Global parameters
# -----------------------------
GLOBAL_ev_age_newLib         <- 8    # new LIB needed if no reuse after this EV age
GLOBAL_max_reuse_lib_share   <- 0.5  # share of only-EV-fail LIB that can be reused in EV (cap)
GLOBAL_max_ev_age            <- 12   # EVs older than this do not receive new LIBs
GLOBAL_max_lib_age_ev        <- 12   # max LIB age that can be reused in EV
GLOBAL_max_lib_age_repurpose <- 12   # max LIB age that can be repurposed to BESS

# Split rules (per origin of the LIB)
GLOBAL_reuse_share_evfail      <- 0.50  # from only EV fail: 50% → reuse (subject to EV demand)
GLOBAL_repurpose_share_evfail  <- 0.25  # from only EV fail: target 25% → repurpose
GLOBAL_recycle_share_evfail    <- 0.25  # from only EV fail: residual → recycling
GLOBAL_repurpose_share_libfail <- 0.50  # from only LIB fail: 50% → repurpose (max age limit)
GLOBAL_recycle_share_libfail   <- 0.50  # from only LIB fail: 50% → recycling

# Lifetime params by segment (same as before)
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
f.getOutflows <- function(n_veh = 1, EV_age, LIB_age,
                          maxEV_age = 30, maxLIB_age = 30,
                          dist.Age  = "Logistic",
                          mean_ev   = 17, sd_ev   = 4,
                          mean_lib  = 15, sd_lib  = 4) {
  
  # Year-to-year survival probabilities for EV and LIB
  if (dist.Age == "Normal") {
    y1 <- (1 - pnorm(EV_age+1, mean_ev,  sd_ev)) /
      (1 - pnorm(EV_age,   mean_ev,  sd_ev))
    y2 <- (1 - pnorm(LIB_age+1, mean_lib, sd_lib)) /
      (1 - pnorm(LIB_age,   mean_lib, sd_lib))
  } else { # Logistic
    sdev <- sd_ev  * sqrt(3)/pi
    sdli <- sd_lib * sqrt(3)/pi
    y1 <- (1 - plogis(EV_age+1, mean_ev,  sdev)) /
      (1 - plogis(EV_age,   mean_ev,  sdev))
    y2 <- (1 - plogis(LIB_age+1, mean_lib, sdli)) /
      (1 - plogis(LIB_age,   mean_lib, sdli))
  }
  
  # Forced retirement at max age
  if (EV_age  >= maxEV_age)  y1 <- 0
  if (LIB_age >= maxLIB_age) y2 <- 0
  
  # Independent events → four mutually exclusive cases
  tibble(
    both_fail = (1-y1) * (1-y2) * n_veh,  # EV and LIB both fail
    ev_fail   = (1-y1) *  y2   * n_veh,   # only EV fails, LIB survives
    lib_fail  =  y1   * (1-y2) * n_veh,   # only LIB fails, EV survives
    none      =  y1   *  y2   * n_veh     # both survive one more year
  )
}

# -----------------------------
# 2) One simulation step
#    - Advance cohort matrix by 1 year
#    - Apply reuse / repurpose / recycling rules
#    - Export adjustment via reuse_shrink (0..1) on reuse flows only
# -----------------------------
EV_engine_step <- function(engine, 
                           sales_y = 0,
                           param_ev_age_newLib  = GLOBAL_ev_age_newLib,
                           param_max_reuse_lib  = GLOBAL_max_reuse_lib_share,
                           param_max_ev_age     = GLOBAL_max_ev_age,
                           param_max_lib_age_ev = GLOBAL_max_lib_age_ev,
                           param_max_lib_age_rep = GLOBAL_max_lib_age_repurpose,
                           reuse_shrink   = 1.0) {
  
  mat <- engine$matrix
  
  # Empty matrices for next-year stock and intermediate flows
  new_matrix <- matrix(0, nrow = 31, ncol = 31,
                       dimnames = list(paste0("EV_",0:30),
                                       paste0("LIB_",0:30)))
  matrix_ev   <- new_matrix  # EV survives, LIB fails (only LIB fail)
  matrix_lib  <- new_matrix  # EV fails, LIB survives (only EV fail)
  matrix_both <- new_matrix  # both fail
  
  # --- 2.1 Propagate survival and classify failures ------------------
  for (i in 1:31) {          # EV age index (0..30)
    for (j in 1:31) {        # LIB age index (0..30)
      if (mat[i,j] != 0) {
        res <- f.getOutflows(
          n_veh   = mat[i,j],
          EV_age  = i-1,     # convert index to age
          LIB_age = j-1,
          maxEV_age  = 30,
          maxLIB_age = 30,
          dist.Age   = "Logistic",
          mean_ev    = engine$mean_ev,  sd_ev  = engine$sd_ev,
          mean_lib   = engine$mean_lib, sd_lib = engine$sd_lib
        )
        
        # Normal interior cell: can age EV and LIB by +1 year
        if (i != 31 & j != 31) {
          new_matrix[i+1, j+1] <- new_matrix[i+1, j+1] + res$none
          matrix_ev[i+1,   j+1] <- matrix_ev[i+1,   j+1] + res$lib_fail
          matrix_lib[i+1,  j+1] <- matrix_lib[i+1,  j+1] + res$ev_fail
          matrix_both[i+1, j+1] <- matrix_both[i+1, j+1] + res$both_fail
          
          # LIB hits max age first → only LIB fail, EV can still age
        } else if (j == 31 & i != 31) {
          matrix_ev[i+1, j] <- matrix_ev[i+1, j] + res$lib_fail
          
          # EV hits max age first → only EV fail, LIB can still age
        } else if (i == 31 & j != 31) {
          matrix_lib[i, j+1] <- matrix_lib[i, j+1] + res$ev_fail
        }
      }
    }
  }
  
  # --- 2.2 Aggregate raw outflows by age -----------------------------
  # EVs that need a new battery (only LIB fail)
  ev_need_vec_raw <- as.integer(round(rowSums(matrix_ev)))   # length 31, EV ages 0..30
  
  # EVs that retire (either EV fails alone or with LIB)
  ev_retired_vec  <- as.integer(round(rowSums(matrix_lib) + rowSums(matrix_both)))
  
  # EVs above param_max_ev_age do not receive a new LIB
  if (param_max_ev_age < 30)
    ev_need_vec_raw[(param_max_ev_age+1):31] <- 0L
  
  # LIB flows by LIB age (1..30) from only LIB fail and both fail
  lib_failed_only_vec <- as.integer(round(colSums(matrix_ev)[-1]))   # only LIB fail
  lib_bothfail_vec    <- as.integer(round(colSums(matrix_both)[-1])) # both fail
  
  # LIB flows from only EV fail (EV dies, LIB survives), ages 0..30
  lib_evfail_total_vec <- as.integer(round(colSums(matrix_lib)))     # length 31
  
  ages_lib_1_30 <- 1:30
  ages_lib_0_30 <- 0:30
  
  # -----------------------------
  # 2.3 Split ONLY LIB FAIL:
  #     50% → repurpose (age ≤ param_max_lib_age_rep)
  #     50% → recycling (plus all age > param_max_lib_age_rep)
  # -----------------------------
  lib_fail_repurpose_vec <- as.integer(round(
    lib_failed_only_vec * GLOBAL_repurpose_share_libfail
  ))
  lib_fail_recycle_vec   <- lib_failed_only_vec - lib_fail_repurpose_vec
  
  # Age limit for repurpose: > param_max_lib_age_rep → everything to recycling
  if (param_max_lib_age_rep < 30) {
    too_old_idx <- which(ages_lib_1_30 > param_max_lib_age_rep)
    if (length(too_old_idx) > 0) {
      lib_fail_recycle_vec[too_old_idx] <- lib_fail_recycle_vec[too_old_idx] +
        lib_fail_repurpose_vec[too_old_idx]
      lib_fail_repurpose_vec[too_old_idx] <- 0L
    }
  }
  
  # -----------------------------
  # 2.4 Prepare ONLY EV FAIL LIBs for reuse / repurpose / recycling
  #     Step 1: decide potential reuse pool by LIB age
  # -----------------------------
  # Potential share for reuse (capped by age and param_max_reuse_lib)
  lib_to_EV_vec <- as.integer(round(
    lib_evfail_total_vec * GLOBAL_reuse_share_evfail
  ))
  # Age cap for reuse
  eligible_reuse <- ages_lib_0_30 <= param_max_lib_age_ev
  if (param_max_lib_age_ev < 30) {
    lib_to_EV_vec[!eligible_reuse] <- 0L
  }
  
  # Save initial reuse candidates by LIB age for later accounting
  lib_to_EV_init_vec <- lib_to_EV_vec
  
  # -----------------------------
  # 2.5 Allocate reuse LIBs to EVs
  #     (same structure as before: offset by param_ev_age_newLib, then greedy loop)
  # -----------------------------
  
  # Extend EV needs and LIB reuse pool to handle the param_ev_age_newLib offset
  ev_need_ext   <- c(ev_need_vec_raw, rep(0L, param_ev_age_newLib))
  lib_to_EV_ext <- c(rep(0L, param_ev_age_newLib), lib_to_EV_vec)
  allocation    <- pmin(ev_need_ext, lib_to_EV_ext)
  
  # Age-offset reuse: this part is recorded by EV age
  reuse_offset_vec <- allocation[-(1:param_ev_age_newLib)]
  if (length(reuse_offset_vec) < 30)
    reuse_offset_vec <- c(reuse_offset_vec, rep(0L, 30 - length(reuse_offset_vec)))
  
  # Update EV needs and LIB reuse pool after offset matching
  ev_need_ext   <- ev_need_ext   - allocation
  lib_to_EV_ext <- lib_to_EV_ext - allocation
  
  ev_need_vec   <- ev_need_ext[1:31]
  lib_to_EV_vec <- lib_to_EV_ext[-(1:param_ev_age_newLib)]
  
  # Greedy cross-age matching: older EVs first, younger LIBs first
  lib_to_EV_before <- lib_to_EV_vec
  start_bat <- 1
  for (i in 31:1) {  # EV age (index)
    if (i > param_ev_age_newLib) {
      for (j in start_bat:31) {  # LIB age (index)
        allocated <- min(ev_need_vec[i], lib_to_EV_vec[j])
        if (allocated > 0L) {
          ev_need_vec[i]   <- ev_need_vec[i]   - allocated
          lib_to_EV_vec[j] <- lib_to_EV_vec[j] - allocated
          new_matrix[i,j]  <- new_matrix[i,j]  + allocated
          start_bat <- j
        }
        if (ev_need_vec[i] == 0L) break
      }
    }
  }
  lib_to_EV_after <- lib_to_EV_vec
  
  # Reuse matched in greedy loop by EV age
  reuse_loop_vec <- (lib_to_EV_before - lib_to_EV_after)[-1]
  if (length(reuse_loop_vec) < 30)
    reuse_loop_vec <- c(reuse_loop_vec, rep(0L, 30 - length(reuse_loop_vec)))
  
  # Reuse by EV age (0..30 mapped into length 31; here we keep 30-length by age≥1)
  LIB_reuse_vector <- as.integer(reuse_offset_vec + reuse_loop_vec)
  LIB_reuse_EV     <- sum(LIB_reuse_vector)
  
  # Actual reuse by LIB age (for mass balance on EV-fail LIBs)
  lib_reuse_byLIB_vec <- lib_to_EV_init_vec - lib_to_EV_after  # length 31, ages 0..30
  
  # -----------------------------
  # 2.6 New LIBs required (unmet EV needs after reuse)
  # -----------------------------
  LIB_newadd_vector <- as.integer(ev_need_vec)   # by EV age (0..30)
  LIB_new_add       <- sum(LIB_newadd_vector)
  
  # -----------------------------
  # 2.7 Apply export adjustment on reuse (reuse_shrink)
  #     - shrink reuse flows
  #     - convert lost reuse into extra new LIB demand
  # -----------------------------
  reuse_shrink <- max(0, min(1, reuse_shrink))
  if (reuse_shrink < 1) {
    lost_reuse_vec <- as.integer(round((1 - reuse_shrink) * LIB_reuse_vector))
    if (sum(lost_reuse_vec) > 0) {
      LIB_reuse_vector  <- LIB_reuse_vector - lost_reuse_vec
      LIB_reuse_EV      <- sum(LIB_reuse_vector)
      
      LIB_newadd_vector <- LIB_newadd_vector + lost_reuse_vec
      LIB_new_add       <- sum(LIB_newadd_vector)
      
      # Also place these lost-reuse batteries as new LIBs in column 0
      new_matrix[,1]    <- new_matrix[,1] + lost_reuse_vec
    }
  }
  
  # -----------------------------
  # 2.8 Split ONLY EV FAIL LIBs after knowing actual reuse
  #     Remaining = total EV-fail LIBs − actual reuse (by LIB age)
  #     Target repurpose share = 25% of total (age-limited)
  # -----------------------------
  remaining_after_reuse_evfail <- lib_evfail_total_vec - lib_reuse_byLIB_vec
  remaining_after_reuse_evfail[remaining_after_reuse_evfail < 0] <- 0L
  
  # Repurpose target based on total EV-fail LIBs
  repurpose_target_evfail_vec <- as.integer(round(
    lib_evfail_total_vec * GLOBAL_repurpose_share_evfail
  ))
  # Age cap for repurpose
  eligible_rep_ev <- ages_lib_0_30 <= param_max_lib_age_rep
  if (param_max_lib_age_rep < 30) {
    repurpose_target_evfail_vec[!eligible_rep_ev] <- 0L
  }
  
  # Actual repurpose from only EV fail 
  lib_evfail_repurpose_vec <- pmin(remaining_after_reuse_evfail,
                                   repurpose_target_evfail_vec)
  # Recycling from only EV fail = remainder
  lib_evfail_recycle_vec   <- remaining_after_reuse_evfail - lib_evfail_repurpose_vec
  
  # -----------------------------
  # 2.9 Aggregate repurpose and recycling flows (all origins)
  # -----------------------------
  # Pad 30-length vectors (ages 1..30) to length 31 for consistent summation
  pad31 <- function(v30) {
    v31 <- integer(31)
    v31[2:31] <- as.integer(v30)
    v31
  }
  
  # Repurpose:
  #  - from only EV fail (all ages 0..30)
  #  - from only LIB fail (ages 1..30, padded)
  lib_fail_repurpose_vec31 <- pad31(lib_fail_repurpose_vec)
  
  LIB_repurpose_vector <- as.integer(
    lib_evfail_repurpose_vec + lib_fail_repurpose_vec31
  )
  LIB_repurpose <- sum(LIB_repurpose_vector)
  
  # Recycling:
  #  - from only EV fail (lib_evfail_recycle_vec, 31)
  #  - from only LIB fail (lib_fail_recycle_vec, 30 → 31)
  #  - from both fail        (lib_bothfail_vec,     30 → 31)
  lib_fail_recycle_vec31 <- pad31(lib_fail_recycle_vec)
  lib_bothfail_vec31     <- pad31(lib_bothfail_vec)
  
  lib_recycling_vec <- as.integer(
    lib_evfail_recycle_vec + lib_fail_recycle_vec31 + lib_bothfail_vec31
  )
  LIB_recycling <- sum(lib_recycling_vec)
  
  # For downstream scripts that expect "available" LIBs as repurpose pool,
  # we set LIB_available_vector = LIB_repurpose_vector
  LIB_available_vector <- LIB_repurpose_vector
  LIB_available        <- LIB_repurpose
  
  # -----------------------------
  # 2.10 Update stock matrix with:
  #       - leftover LIBs (those not reused, repurposed, or recycled)
  #       - new LIBs for unmet EV needs
  #       - new sales entering (0,0)
  # -----------------------------
  # Leftover LIBs after reuse, repurpose, recycling are negligible for flows,
  # but we keep a simple stock balance:
  # lib_residual = all LIB from all cases − reuse − repurpose − recycling
  lib_total_all_30 <- lib_failed_only_vec + lib_bothfail_vec          # 30-length
  lib_total_all_31 <- lib_evfail_total_vec + pad31(lib_total_all_30)  # 31-length
  
  lib_residual_vec <- lib_total_all_31 -
    lib_reuse_byLIB_vec - LIB_repurpose_vector - lib_recycling_vec
  lib_residual_vec[lib_residual_vec < 0] <- 0L
  
  # Add residual LIB stock into corresponding LIB-age columns
  for (j in 1:31) {
    if (lib_residual_vec[j] > 0) {
      # Distribute residual LIB evenly over EV ages that are still in stock
      ev_stock_vec <- rowSums(new_matrix)
      if (sum(ev_stock_vec) > 0) {
        share <- ev_stock_vec / sum(ev_stock_vec)
        addv  <- as.integer(round(lib_residual_vec[j] * share))
        # Correct rounding
        diff  <- lib_residual_vec[j] - sum(addv)
        if (diff != 0) {
          ord <- order(share - addv / max(1, lib_residual_vec[j]), decreasing = TRUE)
          addv[head(ord, abs(diff))] <- addv[head(ord, abs(diff))] + sign(diff)
        }
        new_matrix[,j] <- new_matrix[,j] + addv
      }
    }
  }
  
  # New LIBs for unmet EV needs all enter as LIB age 0 (column 1)
  new_matrix[,1] <- new_matrix[,1] + as.integer(LIB_newadd_vector)
  
  # New vehicle sales enter at (0,0) (EV age 0, LIB age 0)
  new_matrix[1,1] <- new_matrix[1,1] + as.integer(round(sales_y))
  
  # Totals
  EV_retired    <- sum(ev_retired_vec)
  engine$matrix <- new_matrix
  
  list(
    engine                 = engine,
    EV_retired             = EV_retired,
    # LIB flows (by LIB age 0..30; length 31)
    LIB_recycling_vector   = lib_recycling_vec,
    LIB_recycling          = LIB_recycling,
    LIB_failed_only_vector = pad31(lib_failed_only_vec),   # only LIB fail (for debugging)
    LIB_bothfail_vector    = pad31(lib_bothfail_vec),      # both fail (for debugging)
    LIB_available_vector   = LIB_available_vector,         # repurpose pool (BESS)
    LIB_available          = LIB_available,
    LIB_reuse_vector       = LIB_reuse_vector,             # reuse in EV by EV age (length 30)
    LIB_reuse_EV           = LIB_reuse_EV,
    LIB_repurpose_vector   = LIB_repurpose_vector,         # repurpose (BESS) by LIB age
    LIB_repurpose          = LIB_repurpose,
    LIB_new_add            = LIB_new_add,
    LIB_newadd_vector      = LIB_newadd_vector,            # new LIBs by EV age
    EV_stock_vector        = as.integer(round(rowSums(new_matrix))),
    EV_stock               = as.integer(round(sum(new_matrix)))
  )
}

# -----------------------------
# 3) Engine initialization with REAL warm-up (2014–2019)
# -----------------------------
EV_engine_init <- function(ev_hist_slice, segment, propulsion,
                           lifetime_scen   = "Baseline",
                           start_year      = 2014,
                           warmup_last_year = 2019) {
  
  mean_ev  <- life_param %>%
    filter(scen_lifetime == lifetime_scen, Vehicle == segment) %>%
    pull(mean_ev)
  sd_ev    <- life_param %>%
    filter(scen_lifetime == lifetime_scen, Vehicle == segment) %>%
    pull(sd_ev)
  mean_lib <- life_param %>%
    filter(scen_lifetime == lifetime_scen, Vehicle == segment) %>%
    pull(mean_lib)
  sd_lib   <- life_param %>%
    filter(scen_lifetime == lifetime_scen, Vehicle == segment) %>%
    pull(sd_lib)
  
  # Empty stock matrix (EV age 0..30 × LIB age 0..30)
  mat <- matrix(0, nrow = 31, ncol = 31,
                dimnames = list(paste0("EV_",0:30),
                                paste0("LIB_",0:30)))
  
  engine <- list(
    matrix   = mat,
    mean_ev  = mean_ev,  sd_ev  = sd_ev,
    mean_lib = mean_lib, sd_lib = sd_lib,
    scen     = lifetime_scen,
    segment  = segment,
    propulsion = propulsion
  )
  
  # Warm-up with historical sales 2014–2019
  for (y in start_year:warmup_last_year) {
    sales_y <- ev_hist_slice %>% filter(`Sale Year` == y) %>% pull(Sales)
    if (length(sales_y) == 0) sales_y <- 0
    step <- EV_engine_step(
      engine,
      sales_y = sales_y,
      param_ev_age_newLib         = GLOBAL_ev_age_newLib,
      param_max_reuse_lib         = GLOBAL_max_reuse_lib_share,
      param_max_ev_age            = GLOBAL_max_ev_age,
      param_max_lib_age_ev        = GLOBAL_max_lib_age_ev,
      param_max_lib_age_rep       = GLOBAL_max_lib_age_repurpose,
      reuse_shrink                = 1.0   # warm-up years: no export adjustment
    )
    engine <- step$engine
  }
  
  engine
}