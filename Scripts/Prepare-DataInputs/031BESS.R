# ==============================================================================
# Script: 03-BESS_byStateSegProp_ALL.R
# Purpose:
#   Run BESS stock + retire vector simulation by State×Segment×Propulsion
#   for US + Canada, and for scenarios ACCII + Repeal.
#
# Modeling choice:
#   Inflow enters at BEGINNING of year, then retires within the SAME year.
#
# IMPORTANT (match 004-EVTurnover.R battery assumptions):
#   dist: Logistic
#   mean_lib = 15, sd_lib = 4
#   maxLIB_age = 30 (forced retirement)
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)

# ------------------------------------------------------------------------------
# 1) Settings
# ------------------------------------------------------------------------------

START_YEAR <- 2020
END_YEAR   <- 2050
SIM_YEARS  <- START_YEAR:END_YEAR

# ---- Match 004 battery assumptions ----
DIST_AGE   <- "Logistic"
LIB_MEAN   <- 15
LIB_SD     <- 4
MAX_LIB_AGE <- 30   # forced retirement when age at START of year >= 30

# Scenarios to run (default for US/Canada)
SCENARIOS <- c("ACCII", "Repeal")

# Countries / folders to run
# Note: Mexico only has ACCII scenario
COUNTRIES <- tibble(
  Country   = c("US", "Canada", "Mexico"),
  InDir     = c("Outputs", "Outputs/Canada", "Outputs/Mexico"),
  OutDir    = c("Outputs", "Outputs/Canada", "Outputs/Mexico"),
  Scenarios = list(c("ACCII", "Repeal"), c("ACCII", "Repeal"), c("ACCII"))
)

# Output rounding: keep decimals internally, round only at output
ROUND_METHOD <- "round"  # "round" | "floor" | "ceiling"

# Fixed vector length (recommend match 004: 0..30)
MAX_AGE_FIXED <- 30
AGE_BINS <- 0:MAX_AGE_FIXED

# ------------------------------------------------------------------------------
# 2) Helpers
# ------------------------------------------------------------------------------

round_int <- function(x, method = "round") {
  x <- as.numeric(x)
  x[!is.finite(x)] <- 0
  x <- pmax(x, 0)
  if (method == "floor")   return(as.integer(floor(x)))
  if (method == "ceiling") return(as.integer(ceiling(x)))
  as.integer(round(x))
}

# Survival prob S(age) for Logistic
get_survival_prob <- function(age, mean_val, sd_val) {
  scale_val <- sd_val * sqrt(3) / pi
  age2 <- ifelse(age < 0, 0, age)
  1 - plogis(age2, location = mean_val, scale = scale_val)
}

parse_pipe_vec <- function(s) {
  if (is.null(s) || length(s) == 0 || is.na(s)) return(numeric(0))
  s <- as.character(s)
  if (nchar(s) == 0) return(numeric(0))
  parts <- strsplit(s, "\\|")[[1]]
  parts <- trimws(parts)
  parts[parts == "" | is.na(parts) | toupper(parts) == "NA"] <- "0"
  v <- suppressWarnings(as.numeric(parts))
  v[!is.finite(v)] <- 0
  v
}

sum_vec <- function(a, b) {
  a <- as.numeric(a); b <- as.numeric(b)
  a[!is.finite(a)] <- 0
  b[!is.finite(b)] <- 0
  len <- max(length(a), length(b))
  if (len == 0) return(numeric(0))
  a <- c(a, rep(0, len - length(a)))
  b <- c(b, rep(0, len - length(b)))
  a + b
}

yearvec_to_inflow <- function(y, v) {
  if (length(v) == 0) return(tibble(Year=integer(), Vintage=integer(), Inflow=double()))
  age <- 0:(length(v) - 1)
  inflow <- as.numeric(v)
  keep <- is.finite(inflow) & inflow > 0
  if (!any(keep)) return(tibble(Year=integer(), Vintage=integer(), Inflow=double()))
  tibble(
    Year    = as.integer(y),
    Vintage = as.integer(y - age[keep]),
    Inflow  = inflow[keep]
  )
}

# ------------------------------------------------------------------------------
# 3) Core: simulate one group (State×Segment×Propulsion)
# ------------------------------------------------------------------------------

simulate_one_group <- function(df_g) {
  
  df_year <- df_g %>%
    mutate(vec = map(LIB_repurpose_vector, parse_pipe_vec)) %>%
    group_by(Year) %>%
    summarise(vec_sum = list(reduce(vec, sum_vec)), .groups = "drop") %>%
    arrange(Year)
  
  df_inflow <- pmap_dfr(list(df_year$Year, df_year$vec_sum), yearvec_to_inflow)
  if (nrow(df_inflow) == 0) return(list(stock = tibble(), retire = tibble()))
  
  min_vintage  <- min(df_inflow$Vintage, na.rm = TRUE)
  max_vintage  <- END_YEAR
  all_vintages <- min_vintage:max_vintage
  
  stock_matrix  <- matrix(0, nrow = length(all_vintages), ncol = length(SIM_YEARS),
                          dimnames = list(all_vintages, SIM_YEARS))
  retire_matrix <- matrix(0, nrow = length(all_vintages), ncol = length(SIM_YEARS),
                          dimnames = list(all_vintages, SIM_YEARS))
  
  # Inflow at BEGINNING of year -> retire in SAME year
  for (j in seq_along(SIM_YEARS)) {
    y <- SIM_YEARS[j]
    
    # end-of-year age
    ages_end <- y - all_vintages
    # start-of-year age (this is what 004 compares to maxLIB_age)
    ages_start <- ages_end - 1
    
    year_inflow <- df_inflow %>% filter(Year == y)
    inflow_vec <- numeric(length(all_vintages))
    if (nrow(year_inflow) > 0) {
      idx <- match(year_inflow$Vintage, all_vintages)
      ok  <- !is.na(idx)
      inflow_vec[idx[ok]] <- year_inflow$Inflow[ok]
    }
    
    stock_start <- if (j == 1) inflow_vec else (stock_matrix[, j - 1] + inflow_vec)
    
    # survival ratio: S(end_age)/S(start_age)
    prob_end   <- get_survival_prob(ages_end,   LIB_MEAN, LIB_SD)
    prob_start <- get_survival_prob(ages_start, LIB_MEAN, LIB_SD)
    
    decay_factor <- prob_end / prob_start
    decay_factor[is.na(decay_factor) | is.infinite(decay_factor)] <- 0
    decay_factor[prob_start <= 0] <- 0
    
    # ---- Forced retirement at maxLIB_age (match 004) ----
    # In 004: if (LIB_age >= maxLIB_age) y2 <- 0
    # Here LIB_age corresponds to ages_start
    decay_factor[ages_start >= MAX_LIB_AGE] <- 0
    
    stock_end <- stock_start * decay_factor
    retired   <- pmax(stock_start - stock_end, 0)
    
    stock_matrix[, j]  <- stock_end
    retire_matrix[, j] <- retired
  }
  
  # ---- Stock vectors (by total age at end of year) ----
  stock_vec_list <- vector("list", length(SIM_YEARS))
  for (j in seq_along(SIM_YEARS)) {
    y <- SIM_YEARS[j]
    v_stock <- as.numeric(stock_matrix[, j])
    v_age   <- y - all_vintages
    
    tmp <- tibble(Age = v_age, Stock = v_stock) %>%
      filter(Age >= 0, Age <= MAX_AGE_FIXED, Stock > 0) %>%
      group_by(Age) %>%
      summarise(Stock = sum(Stock), .groups = "drop")
    
    age_vec <- numeric(length(AGE_BINS))
    if (nrow(tmp) > 0) age_vec[tmp$Age + 1] <- tmp$Stock
    stock_vec_list[[j]] <- round_int(age_vec, ROUND_METHOD)
  }
  
  df_stock <- tibble(
    Year = SIM_YEARS,
    BESS_stock_vector = I(stock_vec_list),
    BESS_stock_total  = sapply(stock_vec_list, sum)
  ) %>%
    mutate(BESS_stock_vector = sapply(BESS_stock_vector, \(v) paste(v, collapse="|")))
  
  # Retire vectors
  ret_vec_list <- vector("list", length(SIM_YEARS))
  for (j in seq_along(SIM_YEARS)) {
    y <- SIM_YEARS[j]
    v_ret <- as.numeric(retire_matrix[, j])
    
    # IMPORTANT: clamp ages so all forced retire at 30+ count into Age30 bin
    v_age <- pmin(y - all_vintages, MAX_AGE_FIXED)
    
    tmp <- tibble(Age = v_age, Retire = v_ret) %>%
      filter(Age >= 0, Age <= MAX_AGE_FIXED, Retire > 0) %>%
      group_by(Age) %>%
      summarise(Retire = sum(Retire), .groups = "drop")
    
    age_vec <- numeric(length(AGE_BINS))
    if (nrow(tmp) > 0) age_vec[tmp$Age + 1] <- tmp$Retire
    ret_vec_list[[j]] <- round_int(age_vec, ROUND_METHOD)
  }
  df_ret <- tibble(
    Year = SIM_YEARS,
    BESS_retire_vector = I(ret_vec_list),
    BESS_retire_total  = sapply(ret_vec_list, sum)
  ) %>%
    mutate(BESS_retire_vector = sapply(BESS_retire_vector, \(v) paste(v, collapse="|")))
  
  list(stock = df_stock, retire = df_ret)
}

# ------------------------------------------------------------------------------
# 4) Run one input file (one country + one scenario)
# ------------------------------------------------------------------------------

run_one_file <- function(input_file, out_stock, out_ret) {
  if (!file.exists(input_file)) {
    message("Skip (not found): ", input_file)
    return(invisible(NULL))
  }
  
  message("Reading: ", input_file)
  df_raw <- read_csv(input_file, show_col_types = FALSE)
  
  needed <- c("Year", "State", "Segment", "Propulsion", "LIB_repurpose_vector")
  miss <- setdiff(needed, names(df_raw))
  if (length(miss) > 0) stop(paste("Missing required columns:", paste(miss, collapse = ", ")))
  
  df_raw <- df_raw %>%
    mutate(
      Year = as.integer(Year),
      State = as.character(State),
      Segment = as.character(Segment),
      Propulsion = as.character(Propulsion),
      LIB_repurpose_vector = as.character(LIB_repurpose_vector)
    ) %>%
    filter(Year >= START_YEAR, Year <= END_YEAR)
  
  key_cols <- c("State", "Segment", "Propulsion")
  
  message("Simulating groups...")
  df_groups <- df_raw %>%
    group_by(across(all_of(key_cols))) %>%
    group_split()
  
  # Create zero-filled output for groups with no data
  zero_stock_vec <- paste(rep(0, length(AGE_BINS)), collapse = "|")
  zero_retire_vec <- paste(rep(0, length(AGE_BINS)), collapse = "|")
  
  res <- map(df_groups, function(g) {
    keys <- g %>% distinct(across(all_of(key_cols))) %>% slice(1)
    sim  <- simulate_one_group(g)
    
    out_s <- sim$stock
    out_r <- sim$retire
    
    # If simulation returns empty results, create zero-filled rows for all years
    if (nrow(out_s) == 0) {
      out_s <- tibble(
        Year = SIM_YEARS,
        BESS_stock_vector = zero_stock_vec,
        BESS_stock_total = 0L
      )
    }
    if (nrow(out_r) == 0) {
      out_r <- tibble(
        Year = SIM_YEARS,
        BESS_retire_vector = zero_retire_vec,
        BESS_retire_total = 0L
      )
    }
    
    out_s <- bind_cols(keys[rep(1, nrow(out_s)), , drop=FALSE], out_s)
    out_r <- bind_cols(keys[rep(1, nrow(out_r)), , drop=FALSE], out_r)
    
    list(stock = out_s, retire = out_r)
  })
  
  df_stock_all <- bind_rows(map(res, "stock"))
  df_ret_all   <- bind_rows(map(res, "retire"))
  
  write_csv(df_stock_all, out_stock)
  write_csv(df_ret_all,   out_ret)
  
  message("Saved: ", out_stock)
  message("Saved: ", out_ret)
  
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# 5) Loop: US+Canada+Mexico × their respective scenarios
# ------------------------------------------------------------------------------

for (i in seq_len(nrow(COUNTRIES))) {
  country   <- COUNTRIES$Country[i]
  in_dir    <- COUNTRIES$InDir[i]
  out_dir   <- COUNTRIES$OutDir[i]
  scenarios <- COUNTRIES$Scenarios[[i]]
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  for (sc in scenarios) {
    input_file <- file.path(in_dir, paste0("EVLIB_Flows_detail_", sc, ".csv"))
    
    out_stock <- file.path(out_dir, paste0("BESS_Stock_Vector_byStateSegProp_", sc, ".csv"))
    out_ret   <- file.path(out_dir, paste0("BESS_Retire_Vector_byStateSegProp_", sc, ".csv"))
    
    message("\n==============================")
    message("Country: ", country, " | Scenario: ", sc)
    message("==============================")
    
    run_one_file(input_file, out_stock, out_ret)
  }
}

# ------------------------------------------------------------------------------
# 6) Generate combined summary tables (US + Canada + Mexico)
# ------------------------------------------------------------------------------

message("\n==============================")
message("Generating combined summary tables...")
message("==============================")

# Function to read and combine BESS files
combine_bess_files <- function(scenario, file_type) {
  # file_type: "Stock" or "Retire"
  files_to_combine <- list()
  
  # US
  us_file <- file.path("Outputs", paste0("BESS_", file_type, "_Vector_byStateSegProp_", scenario, ".csv"))
  if (file.exists(us_file)) {
    files_to_combine$US <- read_csv(us_file, show_col_types = FALSE)
  }
  
  # Canada
  ca_file <- file.path("Outputs/Canada", paste0("BESS_", file_type, "_Vector_byStateSegProp_", scenario, ".csv"))
  if (file.exists(ca_file)) {
    files_to_combine$Canada <- read_csv(ca_file, show_col_types = FALSE)
  }
  
  # Mexico (only ACCII)
  if (scenario == "ACCII") {
    mx_file <- file.path("Outputs/Mexico", paste0("BESS_", file_type, "_Vector_byStateSegProp_", scenario, ".csv"))
    if (file.exists(mx_file)) {
      files_to_combine$Mexico <- read_csv(mx_file, show_col_types = FALSE)
    }
  }
  
  if (length(files_to_combine) == 0) return(NULL)
  bind_rows(files_to_combine)
}

# Generate combined tables for each scenario
for (sc in SCENARIOS) {
  # Stock
  combined_stock <- combine_bess_files(sc, "Stock")
  if (!is.null(combined_stock)) {
    out_file <- file.path("Outputs", paste0("BESS_Stock_Vector_NorthAmerica_", sc, ".csv"))
    write_csv(combined_stock, out_file)
    message("Saved: ", out_file)
  }
  
  # Retire
  combined_retire <- combine_bess_files(sc, "Retire")
  if (!is.null(combined_retire)) {
    out_file <- file.path("Outputs", paste0("BESS_Retire_Vector_NorthAmerica_", sc, ".csv"))
    write_csv(combined_retire, out_file)
    message("Saved: ", out_file)
  }
}

message("\nAll done.")