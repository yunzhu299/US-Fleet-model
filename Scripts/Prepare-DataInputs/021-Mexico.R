## Mexico EV dereg -> SaleYear vectors using US survival curve ------
## YZC Dec 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(readxl)
library(dplyr)

# -----------------------------
# 0) Read Mexico data
#    You want col 1 = YEAR, col 7 = New EV (dereg, million), col 9 = SH EV (dereg, million)
#    "from 2nd row" -> use skip = 1 so the 2nd row becomes header row
# -----------------------------
## Mexico EV dereg -> vectors using US survival curve ------
## Fix: avoid .data[[1]] numeric indexing

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(readxl)
library(dplyr)

mex_path <- "Inputs/Mexico data.xlsx"

# 1) Read sheet (skip=1 means from 2nd row)
mex_tbl <- read_excel(mex_path, sheet = "Fleet Evo", skip = 1, col_names = TRUE)

# 2) Pick columns by POSITION (1, 7, 9), then rename them
mex_ev <- mex_tbl %>%
  dplyr::select(1, 7, 9) %>%
  rlang::set_names(c("Year", "Dereg_NewEV_mil", "Dereg_SHEV_mil")) %>%
  mutate(
    Year = as.integer(Year),
    Dereg_NewEV_mil = suppressWarnings(as.numeric(Dereg_NewEV_mil)),
    Dereg_SHEV_mil  = suppressWarnings(as.numeric(Dereg_SHEV_mil)),
    Dereg_NewEV_veh = coalesce(Dereg_NewEV_mil, 0) * 1e6,
    Dereg_SHEV_veh  = coalesce(Dereg_SHEV_mil,  0) * 1e6
  ) %>%
  filter(!is.na(Year)) %>%
  arrange(Year)

# -----------------------------
# 1) Build retirement-age distribution p(age) from US logistic survival
#    S(age) = 1/(1+exp((age-mu)/b))
#    retire mass at age a: f(a) = S(a) - S(a+1), for a=0..max_age-1
#    lump 30+ into age=max_age: f(max_age) = S(max_age)
# -----------------------------
S_log <- function(age, mu, b) 1 / (1 + exp((age - mu) / b))

make_p_age <- function(max_age = 30, mu = 16, b = 4) {
  S <- S_log(0:(max_age + 1), mu, b)              # 0..31
  f <- S[1:(max_age)] - S[2:(max_age + 1)]        # 0..29 mass
  f <- c(f, S[max_age + 1])                       # age 30 = 30+ lump
  p <- f / sum(f)
  names(p) <- as.character(0:max_age)
  p
}

# Choose US car-like survival params (edit if you want)
p_age <- make_p_age(max_age = 30, mu = 16, b = 4)

# -----------------------------
# 2) Integer allocation helper (keeps totals exact after rounding)
# -----------------------------
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
# 3) Make vectors for NewEV and SHEV separately, plus Total
#    Vector order matches your Outputs convention:
#    element 1 = Sale_Year = Year (age 0)
#    element 2 = Sale_Year = Year-1 (age 1)
#    ...
#    element 31 = Sale_Year = Year-30 (age 30 / 30+ lump)
# -----------------------------
mex_out <- mex_ev %>%
  rowwise() %>%
  mutate(
    NewEV_vec = vec_to_string(alloc_int_exact(Dereg_NewEV_veh, p_age)),
    SHEV_vec  = vec_to_string(alloc_int_exact(Dereg_SHEV_veh,  p_age)),
    Total_vec = vec_to_string(alloc_int_exact(Dereg_NewEV_veh + Dereg_SHEV_veh, p_age))
  ) %>%
  ungroup() %>%
  select(
    Year,
    NewEV_vec, SHEV_vec, Total_vec,
    Dereg_NewEV_veh, Dereg_SHEV_veh
  )

dir.create("Outputs", showWarnings = FALSE)
write.csv(mex_out, "Outputs/Mexico_Dereg_EV_vectors_USsurvival.csv", row.names = FALSE)
