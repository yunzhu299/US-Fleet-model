## Mexico EV Data Processing
## 1. Reads Deregistrations (Retirements) -> Calculates Vintage Vectors (Restricted to 2016+)
## 2. Reads Registrations (Sales/New Adds) -> Adds to final output
## YZC Jan 2026

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(readxl)
library(dplyr)

# -----------------------------
# 0) Read Mexico data
# -----------------------------
mex_path <- "Inputs/Mexico data.xlsx"

# --- Part A: Read Deregistration (Retirements) from "Fleet Evo" ---
# Previous logic: col 1=Year, 7=NewEV, 9=SHEV (Million), skip 1 row
mex_dereg <- read_excel(mex_path, sheet = "Fleet Evo", skip = 1, col_names = TRUE) %>%
  dplyr::select(1, 7, 9) %>%
  rlang::set_names(c("Year", "Dereg_NewEV_mil", "Dereg_SHEV_mil")) %>%
  mutate(
    Year = as.integer(Year),
    Dereg_NewEV_mil = suppressWarnings(as.numeric(Dereg_NewEV_mil)),
    Dereg_SHEV_mil  = suppressWarnings(as.numeric(Dereg_SHEV_mil)),
    # Convert Million to Vehicles
    Dereg_NewEV_Count = coalesce(Dereg_NewEV_mil, 0) * 1e6,
    Dereg_SHEV_Count  = coalesce(Dereg_SHEV_mil,  0) * 1e6
  ) %>%
  filter(!is.na(Year)) %>%
  select(Year, Dereg_NewEV_Count, Dereg_SHEV_Count) # Keep only clean columns

# --- Part B: Read Registration (New Sales) from "Yearly Reg and Dereg" ---
# New logic: col 1=Year, 3=NewEV, 5=SHEV, start from row 4 (skip 3)
# usage: col_names=FALSE ensures we select by index safely even if headers are messy
mex_reg <- read_excel(mex_path, sheet = "Yearly Reg and Dereg", skip = 3, col_names = FALSE) %>%
  dplyr::select(1, 3, 5) %>%
  rlang::set_names(c("Year", "Reg_NewEV_mil", "Reg_SHEV_mil")) %>%
  mutate(
    Year = as.integer(Year),
    Reg_NewEV_mil = suppressWarnings(as.numeric(Reg_NewEV_mil)),
    Reg_SHEV_mil  = suppressWarnings(as.numeric(Reg_SHEV_mil)),
    # Convert Million to Vehicles
    Reg_NewEV_Count = coalesce(Reg_NewEV_mil, 0) * 1e6,
    Reg_SHEV_Count  = coalesce(Reg_SHEV_mil, 0) * 1e6
  ) %>%
  filter(!is.na(Year)) %>%
  select(Year, Reg_NewEV_Count, Reg_SHEV_Count)

# --- Part C: Merge Reg and Dereg ---
# Full join to ensure we don't lose years if they exist in one but not other
mex_combined <- full_join(mex_dereg, mex_reg, by = "Year") %>%
  arrange(Year) %>%
  # Fill NAs with 0 just in case
  mutate(across(starts_with(c("Dereg", "Reg")), ~ replace_na(., 0)))

# -----------------------------
# 1) Build standard retirement-age distribution p(age)
# -----------------------------
S_log <- function(age, mu, b) 1 / (1 + exp((age - mu) / b))

make_p_age <- function(max_age = 30, mu = 16, b = 4) {
  S <- S_log(0:(max_age + 1), mu, b)
  f <- S[1:(max_age)] - S[2:(max_age + 1)]
  f <- c(f, S[max_age + 1])
  p <- f / sum(f)
  names(p) <- as.character(0:max_age)
  p
}

p_age_base <- make_p_age(max_age = 30, mu = 16, b = 4)

# -----------------------------
# 2) Helper Functions
# -----------------------------
# Dynamic Adjustment for Year >= 2016
get_adjusted_p <- function(base_p, current_year, start_year = 2016) {
  max_valid_age <- current_year - start_year
  if (max_valid_age < 0) return(base_p * 0) 
  if (max_valid_age >= (length(base_p) - 1)) return(base_p)
  
  new_p <- base_p
  cut_idx <- max_valid_age + 1
  if (cut_idx < length(new_p)) {
    new_p[(cut_idx + 1):length(new_p)] <- 0
  }
  total_p <- sum(new_p)
  if (total_p > 0) new_p <- new_p / total_p
  return(new_p)
}

# Integer allocation
alloc_int_exact <- function(total, p) {
  if (total <= 0) return(rep(0L, length(p)))
  x  <- as.numeric(total) * as.numeric(p)
  xi <- floor(x)
  r  <- as.integer(round(total - sum(xi)))
  if (r > 0) {
    frac <- x - xi
    ord  <- order(frac, decreasing = TRUE)
    xi[ord[1:r]] <- xi[ord[1:r]] + 1
  }
  as.integer(xi)
}

vec_to_string <- function(v) paste(v, collapse = "|")

# -----------------------------
# 3) Process Vectors & Output
# -----------------------------
mex_out <- mex_combined %>%
  rowwise() %>%
  mutate(
    # Dynamically adjust p_age based on the current row's Year
    p_adj = list(get_adjusted_p(p_age_base, Year, start_year = 2016)),
    
    # Calculate Dereg vectors (Retirements split by vintage)
    NewEV_vec_raw = list(alloc_int_exact(Dereg_NewEV_Count, p_adj)),
    SHEV_vec_raw  = list(alloc_int_exact(Dereg_SHEV_Count,  p_adj)),
    
    # Format Strings
    Dereg_NewEV_Vec = vec_to_string(NewEV_vec_raw),
    Dereg_SHEV_Vec  = vec_to_string(SHEV_vec_raw),
    Dereg_Total_Vec = vec_to_string(NewEV_vec_raw + SHEV_vec_raw),
    
    # Calculate Total Dereg (Sum of vectors)
    Total_Dereg_NewEV = sum(NewEV_vec_raw),
    Total_Dereg_SHEV  = sum(SHEV_vec_raw),
    Total_Dereg_All   = Total_Dereg_NewEV + Total_Dereg_SHEV
  ) %>%
  ungroup() %>%
  # --- Reorder Columns ---
  select(
    Year,
    # 1. New Sales / Registration (Added this time)
    Reg_NewEV_Count,
    Reg_SHEV_Count,
    
    # 2. Total Deregistrations (Calculated from input)
    Total_Dereg_NewEV, 
    Total_Dereg_SHEV,
    Total_Dereg_All,
    
    # 3. Vectors
    Dereg_NewEV_Vec, 
    Dereg_SHEV_Vec, 
    Dereg_Total_Vec
  )

# Output
dir.create("Outputs", showWarnings = FALSE)
write.csv(mex_out, "Outputs/Mexico_Dereg_EV_vectors_USsurvival.csv", row.names = FALSE)