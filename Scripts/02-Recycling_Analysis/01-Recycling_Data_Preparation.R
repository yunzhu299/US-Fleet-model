## ====================================================================
## 01-Recycling_Data_Preparation.R
## Consolidates: EV Volumes Clean + HMDV + Historical Sales Minerals +
##               Scenarios Setup
##
## Run this FIRST, then 02-Recycling_Analysis.R
## Working directory should be the project root (Fleet model/)
## ====================================================================

## --- 0. Configuration -----------------------------------------------

BASE_DIR   <- getwd()
INPUT_DIR  <- file.path(BASE_DIR, "Inputs")
OUTPUT_DIR <- file.path(BASE_DIR, "Outputs")

FLEET_SCENARIOS <- c("ACCII", "Repeal")

## Required input files (must exist in Inputs/):
##   - Cathode Mix update.xlsx          (EV historical sales + chemistry)
##   - LDV registration and sales.xlsx  (state-level EV registrations)
##   - Mineral_Intensity(2).xlsx        (mineral kg/kWh by chemistry)
##   - Cathode Projections (1).xlsx     (benchmark chemistry projections)
##   - HMDV_EV_Volumes.xlsx            (HDV battery MWh and units)

## --- 1. Libraries ----------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(tibble)
  library(writexl)
  library(ggplot2)
  library(scales)
})

## --- 2. State / Province Maps ----------------------------------------

state_map <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas",
  CA = "California", CO = "Colorado", CT = "Connecticut", DE = "Delaware",
  DC = "District of Columbia",
  FL = "Florida", GA = "Georgia", HI = "Hawaii", ID = "Idaho",
  IL = "Illinois", IN = "Indiana", IA = "Iowa", KS = "Kansas",
  KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland",
  MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi",
  MO = "Missouri", MT = "Montana", NE = "Nebraska", NV = "Nevada",
  NH = "New Hampshire", NJ = "New Jersey", NM = "New Mexico", NY = "New York",
  NC = "North Carolina", ND = "North Dakota", OH = "Ohio", OK = "Oklahoma",
  OR = "Oregon", PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina",
  SD = "South Dakota", TN = "Tennessee", TX = "Texas", UT = "Utah",
  VT = "Vermont", VA = "Virginia", WA = "Washington", WV = "West Virginia",
  WI = "Wisconsin", WY = "Wyoming",
  AB = "Alberta", BC = "British Columbia", MB = "Manitoba", NB = "New Brunswick",
  NL = "Newfoundland and Labrador", NS = "Nova Scotia", ON = "Ontario",
  PE = "Prince Edward Island", QC = "Quebec", SK = "Saskatchewan",
  NT = "Northwest Territories", NU = "Nunavut", YT = "Yukon",
  MX = "Mexico"
)
state_map_rev <- setNames(names(state_map), state_map)

## --- 3. EV Volumes Clean  -------------------------------------------
## (from: EV Volumes Clean.R)

mineral_intensity <- read_excel(
  file.path(INPUT_DIR, "Mineral_Intensity(2).xlsx"), na = ""
) %>%
  filter(!Mineral %in% c("Phosphorus", "Stainless steel")) %>%
  rename(`Cathode Mix` = chemistry)

EV_historical <- read_excel(file.path(INPUT_DIR, "Cathode Mix update.xlsx"))
regs <- read_excel(
  file.path(INPUT_DIR, "LDV registration and sales.xlsx"),
  sheet = "registrations "
)

usa_sales <- EV_historical

# Aggregate monthly Reg -> annual
reg_cols <- grep("^Reg_\\d{4}[-_]\\d{2}[-_]\\d{2}", names(usa_sales), value = TRUE)
reg_year_groups <- split(reg_cols, str_extract(reg_cols, "\\d{4}"))
for (yr in names(reg_year_groups)) {
  cols_yr <- reg_year_groups[[yr]]
  numeric_data <- usa_sales[, cols_yr] %>% mutate(across(everything(), as.numeric))
  usa_sales[[paste0("Total_Reg_", yr)]] <- rowSums(numeric_data, na.rm = TRUE)
}

# Aggregate monthly MWh -> annual
mwh_cols <- grep("^Mwh_\\d{4}[-_]\\d{2}[-_]\\d{2}", names(usa_sales), value = TRUE)
mwh_year_groups <- split(mwh_cols, str_extract(mwh_cols, "\\d{4}"))
for (yr in names(mwh_year_groups)) {
  cols_yr <- mwh_year_groups[[yr]]
  numeric_data <- usa_sales[, cols_yr] %>% mutate(across(everything(), as.numeric))
  usa_sales[[paste0("Total_Mwh_", yr)]] <- rowSums(numeric_data, na.rm = TRUE)
}

usa_sales <- usa_sales %>% select(-all_of(c(reg_cols, mwh_cols)))

usa_sales <- usa_sales %>%
  mutate(`Cathode Mix` = ifelse(
    `Cathode Mix` == "NA",
    paste0(`Cathode Chemistry`, " (unspecified)"),
    `Cathode Mix`
  ))

usa_sales <- usa_sales %>% select(-matches("2013$"))

usa_sales$id <- seq_len(nrow(usa_sales))
usa_sales_long <- usa_sales %>%
  pivot_longer(
    cols      = starts_with("Total_"),
    names_to  = c(".value", "Sale Year"),
    names_pattern = "Total_(.*)_(\\d{4})"
  )

usa_sales_filtered <- usa_sales_long %>%
  select(`Sale Year`, `Battery kWh`, `Cathode Mix`, Propulsion,
         `Global Segment`, Reg, Mwh) %>%
  rename(`Total Sales` = Reg, `Total Mwh` = Mwh)

usa_sales_filtered <- usa_sales_filtered %>%
  mutate(`Global Segment` = case_when(
    str_starts(`Global Segment`, "Car") ~ "Car",
    str_starts(`Global Segment`, "SUV") ~ "SUV",
    str_starts(`Global Segment`, "MPV") ~ "SUV",
    str_starts(`Global Segment`, "SS")  ~ "SUV",
    str_starts(`Global Segment`, "LCV") ~ "SUV",
    str_starts(`Global Segment`, "PUP") ~ "SUV",
    TRUE ~ `Global Segment`
  ))

# State-level registration fractions
powertrain <- c("BEV", "PHEV")
regs_zev <- regs %>%
  select(State, `Electric (EV)`, `Plug-In Hybrid Electric (PHEV)`, Hydrogen, Year) %>%
  rename(BEV = `Electric (EV)`, PHEV = `Plug-In Hybrid Electric (PHEV)`,
         FCEV = Hydrogen, `Sale Year` = Year) %>%
  filter(State != "United States")

regs_zev <- regs_zev %>%
  group_by(`Sale Year`) %>%
  mutate(across(all_of(powertrain), ~ . / sum(., na.rm = TRUE),
                .names = "Fraction_{.col}")) %>%
  ungroup() %>%
  select(State, `Sale Year`, starts_with("Fraction_"))

# Fill missing years (2014-2015 from 2016, 2024 from 2023)
keep_new <- regs_zev %>% filter(`Sale Year` == 2023) %>% mutate(`Sale Year` = 2024)
keep_old <- regs_zev %>% filter(`Sale Year` == 2016)
extended <- bind_rows(
  map_dfr(2014:2015, ~ keep_old %>% mutate(`Sale Year` = .x)),
  regs_zev,
  keep_new
) %>% arrange(`Sale Year`, State)

pt_veh_sales <- usa_sales_filtered %>%
  group_by(`Sale Year`, Propulsion, `Global Segment`) %>%
  summarise(`Total Sales` = sum(`Total Sales`, na.rm = TRUE), .groups = "drop")

pt_veh_sales$`Sale Year` <- as.character(pt_veh_sales$`Sale Year`)
extended$`Sale Year`     <- as.character(extended$`Sale Year`)

historical_state_pt_veh_df <- left_join(extended, pt_veh_sales, by = "Sale Year") %>%
  filter(Propulsion != "FCEV") %>%
  rowwise() %>%
  mutate(Sales = get(paste0("Fraction_", Propulsion)) * `Total Sales`) %>%
  ungroup() %>%
  select(State, `Sale Year`, Propulsion, `Global Segment`, Sales)


## --- 4. HMDV Processing  -------------------------------------------
## (from: HMDV.R)

# HDV battery MWh by chemistry
HDV_chem <- read_xlsx(
  file.path(INPUT_DIR, "HMDV_EV_Volumes.xlsx"),
  sheet = "Batteries - MWh", skip = 10
) %>%
  rename(`2020` = "MWh 2020", `2021` = "MWh 2021", `2022` = "MWh 2022",
         `2023` = "MWh 2023", `2024` = "MWh 2024") %>%
  select(-"MWh 2025 CY") %>%
  pivot_longer(cols = `2020`:`2024`, names_to = "Year", values_to = "MWh") %>%
  mutate(`Cathode Chemistry` = str_replace(`Cathode Chemistry`, "LF`P", "LFP")) %>%
  group_by(Year, `Cathode Chemistry`) %>%
  summarise(MWh = sum(MWh), .groups = "drop")

# HDV battery units
hdv_unit_years <- as.character(2010:2024)
HDV_cap <- read_xlsx(
  file.path(INPUT_DIR, "HMDV_EV_Volumes.xlsx"),
  sheet = "Batteries - Units", skip = 11
) %>%
  select(`OEM Group`, `Cathode Chemistry`, all_of(hdv_unit_years)) %>%
  mutate(`Cathode Chemistry` = replace(`Cathode Chemistry`, 564, "Grand Total")) %>%
  filter(!str_ends(`OEM Group`, "Total") | `OEM Group` == "Grand Total") %>%
  group_by(`Cathode Chemistry`) %>%
  summarise(across(`2010`:`2024`, ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = `2010`:`2024`, names_to = "Year", values_to = "Units") %>%
  filter(Year >= 2020)

# Totals for average capacity calculation
totals_cap <- HDV_cap %>%
  filter(`Cathode Chemistry` == "Grand Total") %>%
  rename(Total_Units = Units) %>%
  select(-`Cathode Chemistry`)

totals_chem <- HDV_chem %>%
  filter(`Cathode Chemistry` == "Grand Total") %>%
  rename(Total_MWh = MWh) %>%
  select(-`Cathode Chemistry`)

HDV_avg_cap <- merge(totals_cap, totals_chem, by = "Year") %>%
  mutate(Year = as.numeric(Year),
         Avg_kwh_unit = Total_MWh / Total_Units * 1000)

# HDV battery capacity projection (trend to 2035, flat after)
HDV_cap_trend <- HDV_avg_cap %>%
  summarise(trend = coef(lm(Avg_kwh_unit ~ Year))[2])

projection_HDV <- HDV_avg_cap %>%
  filter(Year == 2024) %>%
  merge(HDV_cap_trend) %>%
  crossing(hdv_proj_years = 2024:2035)

projection_HDV_full <- projection_HDV %>%
  mutate(HDV_kwh_unit = Avg_kwh_unit + (hdv_proj_years - 2024) * trend) %>%
  select(-c(Year, Total_Units, Total_MWh, trend, Avg_kwh_unit)) %>%
  rename(Sale_Year = hdv_proj_years,
         `Projected Avg Batt Cap (kwh/batt)` = HDV_kwh_unit)

projection_HDV_full <- bind_rows(
  projection_HDV_full,
  projection_HDV_full %>%
    filter(Sale_Year == 2035) %>%
    slice(rep(1:n(), 15)) %>%
    mutate(Sale_Year = 2036:2050)
) %>%
  mutate(Segment = "HDV", Propulsion = "HDV")

# HDV battery capacity scenario 2 (85% of 2024 by 2040, flat after)
HDV_cap_2040 <- HDV_avg_cap %>%
  filter(Year == 2024) %>%
  rename(Sale_Year = Year) %>%
  mutate(Avg_kwh_unit = Avg_kwh_unit * 0.85, Sale_Year = 2040)

HDV_batts <- bind_rows(
  HDV_avg_cap %>%
    filter(Year == 2024) %>%
    rename(Sale_Year = Year) %>%
    mutate(Sale_Year = as.numeric(Sale_Year)),
  HDV_cap_2040
)

HDV_second_trend <- HDV_batts %>%
  reframe(
    cap_2024  = Avg_kwh_unit[Sale_Year == 2024],
    cap_2040  = Avg_kwh_unit[Sale_Year == 2040],
    slope     = (cap_2040 - cap_2024) / (2040 - 2024),
    intercept = cap_2024 - slope * 2024
  )

HDV_batt_cap_15 <- HDV_second_trend %>%
  crossing(Sale_Year = 2024:2050) %>%
  mutate(
    Avg_kwh_unit = case_when(
      Sale_Year <= 2040 ~ intercept + slope * Sale_Year,
      TRUE ~ intercept + slope * 2040
    )
  ) %>%
  select(Sale_Year, Avg_kwh_unit) %>%
  rename(`Projected Avg Batt Cap (kwh/batt)` = Avg_kwh_unit) %>%
  mutate(Segment = "HDV", Propulsion = "HDV")

# HDV chemistry projections (85% LFP, 15% NMC 811)
HDV_chem_project <- crossing(
  Sale_Year    = 2024:2050,
  `Cathode Mix` = c("LFP", "NMC 811")
) %>%
  mutate(`Cathode Mix Share` = ifelse(`Cathode Mix` == "LFP", 0.85, 0.15),
         Segment = "HDV", Propulsion = "HDV")

HDV_chem_hist <- crossing(
  Sale_Year    = 2022:2024,
  `Cathode Mix` = c("LFP", "NMC 811")
) %>%
  mutate(`Share of Avg Chem` = ifelse(`Cathode Mix` == "LFP", 0.85, 0.15),
         Segment = "HDV", Propulsion = "HDV")


## --- 5. Load Fleet Model Outputs (both scenarios) --------------------

safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) { message("  [skip] ", basename(path)); return(NULL) }
  read_csv(path, show_col_types = FALSE, ...)
}

load_fleet_scenario <- function(scen) {
  cat("  Loading fleet data:", scen, "\n")
  sfx <- paste0("_", scen, ".csv")

  evlib_us <- safe_read_csv(file.path(OUTPUT_DIR, paste0("EVLIB_Flows_detail", sfx)))
  evlib_ca <- safe_read_csv(file.path(OUTPUT_DIR, "Canada", paste0("EVLIB_Flows_detail", sfx)))
  evlib_mx <- safe_read_csv(file.path(OUTPUT_DIR, "Mexico", paste0("EVLIB_Flows_detail", sfx)))
  evlib <- bind_rows(evlib_us, evlib_ca, evlib_mx) %>% rename(State_Province = State)

  load_ev <- function(path, has_seg = TRUE) {
    df <- safe_read_csv(path)
    if (is.null(df)) return(NULL)
    cols <- if (has_seg) c("State", "Segment", "Year", "add_BEV", "add_PHEV")
            else         c("State", "Year", "add_BEV", "add_PHEV")
    grp  <- if (has_seg) c("State", "Segment", "Year") else c("State", "Year")
    df %>% select(all_of(cols)) %>%
      group_by(across(all_of(grp))) %>%
      summarise(add_BEV = sum(add_BEV, na.rm = TRUE),
                add_PHEV = sum(add_PHEV, na.rm = TRUE), .groups = "drop") %>%
      rename(BEV = add_BEV, PHEV = add_PHEV, State_Province = State) %>%
      pivot_longer(cols = c(BEV, PHEV), names_to = "Propulsion", values_to = "Add_EV")
  }
  ev_us <- load_ev(file.path(OUTPUT_DIR, paste0("ClosedLoop_AddRetire_byStateSegment", sfx)), TRUE)
  ev_ca <- load_ev(file.path(OUTPUT_DIR, "Canada", paste0("ClosedLoop_AddRetire_byStateSegment", sfx)), TRUE)
  ev_mx <- load_ev(file.path(OUTPUT_DIR, "Mexico", paste0("ClosedLoop_StateTotals", sfx)), FALSE)
  ev <- bind_rows(ev_us, ev_ca, ev_mx)

  load_bess <- function(path) {
    df <- safe_read_csv(path)
    if (is.null(df)) return(NULL)
    df %>% rename(LIB_recycling_vector = BESS_retire_vector, State_Province = State)
  }
  bess_us <- load_bess(file.path(OUTPUT_DIR, paste0("BESS_Retire_Vector_byStateSegProp", sfx)))
  bess_ca <- load_bess(file.path(OUTPUT_DIR, "Canada", paste0("BESS_Retire_Vector_byStateSegProp", sfx)))
  bess_mx <- load_bess(file.path(OUTPUT_DIR, "Mexico", paste0("BESS_Retire_Vector_byStateSegProp", sfx)))
  bess <- bind_rows(bess_us, bess_ca, bess_mx)

  hdv_lib <- safe_read_csv(file.path(OUTPUT_DIR, "HDV", paste0("HDV_EV_Turnover", sfx)))
  if (!is.null(hdv_lib)) hdv_lib <- hdv_lib %>% rename(State_Province = State) %>% mutate(Segment = Vehicle)

  hdv_bess <- safe_read_csv(file.path(OUTPUT_DIR, "HDV", paste0("HDV_BESS_Retire", sfx)))
  if (!is.null(hdv_bess)) hdv_bess <- hdv_bess %>%
    rename(LIB_recycling_vector = BESS_retire_vector, State_Province = State) %>%
    mutate(Segment = Vehicle)

  list(EVLIB_Flows = evlib, EV_Flows = ev, BESSLIB_Flows = bess,
       HDV_LIBFlows = hdv_lib, HDV_BESSLIB_Flows = hdv_bess,
       Fleet_Scenario = scen)
}

fleet_data_list <- lapply(FLEET_SCENARIOS, load_fleet_scenario)
names(fleet_data_list) <- FLEET_SCENARIOS

tag_scen <- function(df, scen) { if (is.null(df)) return(NULL); df %>% mutate(Fleet_Scenario = scen) }

EVLIB_Flows   <- bind_rows(lapply(FLEET_SCENARIOS, function(s) tag_scen(fleet_data_list[[s]]$EVLIB_Flows, s)))
EV_Flows      <- bind_rows(lapply(FLEET_SCENARIOS, function(s) tag_scen(fleet_data_list[[s]]$EV_Flows, s)))
BESSLIB_Flows <- bind_rows(lapply(FLEET_SCENARIOS, function(s) tag_scen(fleet_data_list[[s]]$BESSLIB_Flows, s)))
HDV_LIBFlows  <- bind_rows(lapply(FLEET_SCENARIOS, function(s) tag_scen(fleet_data_list[[s]]$HDV_LIBFlows, s)))
HDV_BESSLIB_Flows <- bind_rows(lapply(FLEET_SCENARIOS, function(s) tag_scen(fleet_data_list[[s]]$HDV_BESSLIB_Flows, s)))

## Default fleet label for logging / one-off runs; do not overwrite if
## caller already set FLEET_SCENARIO (e.g. 03 before source(02)).
if (!exists("FLEET_SCENARIO") || is.null(FLEET_SCENARIO)) {
  FLEET_SCENARIO <- FLEET_SCENARIOS[1]
}


## --- 6. Parse Recycling Vectors & Build Recycling Types ---------------

name_vector_with_years <- function(vec_string, start_year) {
  vec_string <- as.character(vec_string)
  vec <- as.numeric(strsplit(vec_string, "\\|")[[1]])
  names(vec) <- start_year - (seq_along(vec) - 1)
  vec
}

# LDV + BESS vectors
EVLIB_Flows$LIB_recycling_vector <- Map(
  name_vector_with_years,
  EVLIB_Flows$LIB_recycling_vector,
  EVLIB_Flows$Year
)

BESSLIB_Flows$LIB_recycling_vector <- Map(
  name_vector_with_years,
  BESSLIB_Flows$LIB_recycling_vector,
  BESSLIB_Flows$Year
)

# HDV vectors
HDV_LIBFlows$LIB_recycling_vector <- Map(
  name_vector_with_years,
  HDV_LIBFlows$LIB_recycling_vector,
  HDV_LIBFlows$Year
)

HDV_BESSLIB_Flows$LIB_recycling_vector <- Map(
  name_vector_with_years,
  HDV_BESSLIB_Flows$LIB_recycling_vector,
  HDV_BESSLIB_Flows$Year
)

# --- Historical recycling (Sale_Year <= 2025 & Year >= 2025) ---

hist_recycle_type <- EVLIB_Flows %>%
  mutate(recycle_df = map(LIB_recycling_vector, ~ {
    tibble(Sale_Year = as.integer(names(.x)), LIB_recycle_total = as.numeric(.x))
  })) %>%
  select(Fleet_Scenario, State_Province, Segment, Propulsion, Year, recycle_df) %>%
  unnest(cols = recycle_df) %>%
  filter(Sale_Year <= 2025) %>%
  filter(Year >= 2025)

BESS_hist_recycle_type <- BESSLIB_Flows %>%
  mutate(recycle_df = map(LIB_recycling_vector, ~ {
    tibble(Sale_Year = as.integer(names(.x)), LIB_recycle_total = as.numeric(.x))
  })) %>%
  select(Fleet_Scenario, State_Province, Segment, Propulsion, Year, recycle_df) %>%
  unnest(cols = recycle_df) %>%
  filter(Sale_Year <= 2025) %>%
  filter(Year >= 2025)

hist_recycle_type <- full_join(
  hist_recycle_type, BESS_hist_recycle_type,
  by = c("Fleet_Scenario", "State_Province", "Segment", "Propulsion", "Year", "Sale_Year")
) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(LIB_recycle_total = LIB_recycle_total.x + LIB_recycle_total.y) %>%
  select(-LIB_recycle_total.x, -LIB_recycle_total.y)

# HDV historical recycling
HDV_hist_recycle_type <- HDV_LIBFlows %>%
  mutate(recycle_df = map(LIB_recycling_vector, ~ {
    tibble(Sale_Year = as.integer(names(.x)), LIB_recycle_total = as.numeric(.x))
  })) %>%
  select(Fleet_Scenario, State_Province, Segment, Year, recycle_df) %>%
  unnest(cols = recycle_df) %>%
  filter(Sale_Year <= 2025) %>%
  filter(Year >= 2025) %>%
  group_by(Fleet_Scenario, State_Province, Year, Sale_Year) %>%
  summarise(LIB_recycle_total = sum(LIB_recycle_total), .groups = "drop") %>%
  mutate(Propulsion = "HDV", Segment = "HDV")

HDV_BESS_hist_recycle_type <- HDV_BESSLIB_Flows %>%
  mutate(recycle_df = map(LIB_recycling_vector, ~ {
    tibble(Sale_Year = as.integer(names(.x)), LIB_recycle_total = as.numeric(.x))
  })) %>%
  select(Fleet_Scenario, State_Province, Segment, Year, recycle_df) %>%
  unnest(cols = recycle_df) %>%
  filter(Sale_Year <= 2025) %>%
  filter(Year >= 2025) %>%
  group_by(Fleet_Scenario, State_Province, Year, Sale_Year) %>%
  summarise(LIB_recycle_total = sum(LIB_recycle_total), .groups = "drop") %>%
  mutate(Propulsion = "HDV", Segment = "HDV")

hist_recycle_HDV <- full_join(
  HDV_hist_recycle_type, HDV_BESS_hist_recycle_type,
  by = c("Fleet_Scenario", "State_Province", "Segment", "Propulsion", "Year", "Sale_Year")
) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(LIB_recycle_total = LIB_recycle_total.x + LIB_recycle_total.y) %>%
  select(-LIB_recycle_total.x, -LIB_recycle_total.y)

hist_recycle_type <- bind_rows(hist_recycle_type, hist_recycle_HDV)

# --- Future recycling ---

future_recycle_type <- EVLIB_Flows %>%
  mutate(recycle_df = map(LIB_recycling_vector, ~ {
    tibble(Sale_Year = as.integer(names(.x)), LIB_recycle_total = as.numeric(.x))
  })) %>%
  select(Fleet_Scenario, State_Province, Segment, Propulsion, Year, recycle_df) %>%
  unnest(cols = recycle_df)

BESS_future_recycle_type <- BESSLIB_Flows %>%
  mutate(recycle_df = map(LIB_recycling_vector, ~ {
    tibble(Sale_Year = as.integer(names(.x)), LIB_recycle_total = as.numeric(.x))
  })) %>%
  select(Fleet_Scenario, State_Province, Segment, Propulsion, Year, recycle_df) %>%
  unnest(cols = recycle_df)

future_recycle_type <- full_join(
  future_recycle_type, BESS_future_recycle_type,
  by = c("Fleet_Scenario", "State_Province", "Segment", "Propulsion", "Year", "Sale_Year")
) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(LIB_recycle_total = LIB_recycle_total.x + LIB_recycle_total.y) %>%
  select(-LIB_recycle_total.x, -LIB_recycle_total.y)

# HDV future recycling
HDV_future_recycle_type <- HDV_LIBFlows %>%
  mutate(recycle_df = map(LIB_recycling_vector, ~ {
    tibble(Sale_Year = as.integer(names(.x)), LIB_recycle_total = as.numeric(.x))
  })) %>%
  select(Fleet_Scenario, State_Province, Segment, Year, recycle_df) %>%
  unnest(cols = recycle_df) %>%
  filter(Sale_Year >= 2025) %>%
  filter(Year >= 2025) %>%
  group_by(Fleet_Scenario, State_Province, Year, Sale_Year) %>%
  summarise(LIB_recycle_total = sum(LIB_recycle_total), .groups = "drop") %>%
  mutate(Propulsion = "HDV", Segment = "HDV")

HDV_BESS_future_recycle_type <- HDV_BESSLIB_Flows %>%
  mutate(recycle_df = map(LIB_recycling_vector, ~ {
    tibble(Sale_Year = as.integer(names(.x)), LIB_recycle_total = as.numeric(.x))
  })) %>%
  select(Fleet_Scenario, State_Province, Segment, Year, recycle_df) %>%
  unnest(cols = recycle_df) %>%
  filter(Sale_Year >= 2025) %>%
  filter(Year >= 2025) %>%
  group_by(Fleet_Scenario, State_Province, Year, Sale_Year) %>%
  summarise(LIB_recycle_total = sum(LIB_recycle_total), .groups = "drop") %>%
  mutate(Propulsion = "HDV", Segment = "HDV")

future_recycle_HDV <- full_join(
  HDV_future_recycle_type, HDV_BESS_future_recycle_type,
  by = c("Fleet_Scenario", "State_Province", "Segment", "Propulsion", "Year", "Sale_Year")
) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(LIB_recycle_total = LIB_recycle_total.x + LIB_recycle_total.y) %>%
  select(-LIB_recycle_total.x, -LIB_recycle_total.y)

future_recycle_type <- bind_rows(future_recycle_type, future_recycle_HDV)


## --- 7. Historical Chemistry & Battery Capacity ----------------------
## (from: Historical Sales Minerals.R)

# Chemistry shares
chem_Mwh <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion, `Cathode Mix`) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop") %>%
  filter(`Total Mwh` != 0) %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  mutate(`Share of Avg Chem` = `Total Mwh` / sum(`Total Mwh`, na.rm = TRUE)) %>%
  ungroup()
chem_Mwh[is.na(chem_Mwh)] <- 0
chem_Mwh <- chem_Mwh %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), 0, .))) %>%
  rename(Segment = `Global Segment`, Sale_Year = `Sale Year`)

# Battery capacity
batt_cap_sales <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  summarise(`Total Sales` = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  filter(`Total Sales` != 0)

batt_cap_Mwh <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop") %>%
  filter(`Total Mwh` != 0)

batt_cap_merged <- merge(batt_cap_sales, batt_cap_Mwh,
                         by = c("Sale Year", "Global Segment", "Propulsion")) %>%
  mutate(`Avg Batt Cap (kwh/batt)` = (`Total Mwh` / `Total Sales`) * 1000)
batt_cap_merged[is.na(batt_cap_merged)] <- 0
batt_cap_merged <- batt_cap_merged %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), 0, .))) %>%
  rename(Segment = `Global Segment`, Sale_Year = `Sale Year`) %>%
  mutate(Sale_Year = as.integer(Sale_Year))

# Merge HDV average capacity into battery capacity table
HDV_avg_cap_edit <- HDV_avg_cap %>%
  rename(Sale_Year = Year, `Avg Batt Cap (kwh/batt)` = Avg_kwh_unit) %>%
  mutate(Segment = "HDV", Propulsion = "HDV",
         Sale_Year = as.integer(Sale_Year)) %>%
  select(-c(Total_MWh, Total_Units))

batt_cap_merged_w_HDV <- batt_cap_merged %>%
  select(-c(`Total Sales`, `Total Mwh`)) %>%
  bind_rows(HDV_avg_cap_edit) %>%
  arrange(Sale_Year, Segment, Propulsion) %>%
  mutate(`Avg Batt Cap (kwh/batt)` = replace_na(`Avg Batt Cap (kwh/batt)`, 0))

# Apply battery capacity to historical recycling
hist_recycle_cap <- merge(batt_cap_merged_w_HDV, hist_recycle_type,
                          by = c("Sale_Year", "Segment", "Propulsion"), all.x = TRUE)
hist_recycle_cap$LIB_recycle_kwh <- hist_recycle_cap$LIB_recycle_total *
  hist_recycle_cap$`Avg Batt Cap (kwh/batt)`
hist_recycle_cap <- hist_recycle_cap %>%
  select(Fleet_Scenario, Year, Sale_Year, State_Province, Segment, Propulsion, LIB_recycle_kwh)

# Chemistry replacements
replacement <- c(
  'NCA (unspecified)' = 'NCA', 'LFP (unspecified)' = 'LFP',
  'LMO (unspecified)' = 'LMO', 'LTO (unspecified)' = 'LMO-LTO',
  'NMC 111 + NCA' = 'NMCA 89:4:4:3', 'NMC 811 + 111' = 'NMC 811',
  '70 % NMC 111 + 30 % NMC 622' = 'NMC 111', 'NMC 422' = 'NMC 532',
  'NMC 111 + LMO' = 'NMC 111', 'LMO+NMC+NCA' = 'NMCA 89:4:4:3'
)
chem_Mwh$`Cathode Mix` <- recode(chem_Mwh$`Cathode Mix`, !!!replacement)
chem_Mwh <- chem_Mwh %>% filter(Propulsion != "FCEV")

max_values <- chem_Mwh %>%
  group_by(Sale_Year, Segment, Propulsion) %>%
  slice_max(order_by = `Share of Avg Chem`, n = 1, with_ties = FALSE) %>%
  ungroup()

fix_NMC <- chem_Mwh %>%
  filter(str_detect(`Cathode Mix`, "NMC"), !str_detect(`Cathode Mix`, "unspecified"))
max_NMC <- fix_NMC %>%
  group_by(Sale_Year, Segment, Propulsion) %>%
  slice_max(order_by = `Share of Avg Chem`, n = 1, with_ties = FALSE)

NMC_match <- left_join(max_values, max_NMC,
                       by = c("Sale_Year", "Segment", "Propulsion"),
                       suffix = c("_x", "_y"))
NMC_match$`Cathode Mix_x`[NMC_match$`Cathode Mix_x` == "NMC (unspecified)"] <-
  NMC_match$`Cathode Mix_y`[NMC_match$`Cathode Mix_x` == "NMC (unspecified)"]

max_values <- NMC_match %>%
  select(Sale_Year, Segment, `Cathode Mix` = `Cathode Mix_x`, Propulsion)

chem_Mwh <- left_join(chem_Mwh, max_values, by = c("Sale_Year", "Segment", "Propulsion"))
mask_mins <- chem_Mwh$`Cathode Mix.x` %in%
  c("tba (unspecified)", "NiMH (unspecified)", "LMP (unspecified)", "NMC (unspecified)")
chem_Mwh$`Cathode Mix.x`[mask_mins] <- chem_Mwh$`Cathode Mix.y`[mask_mins]
chem_Mwh <- chem_Mwh %>%
  select(-c(`Cathode Mix.y`, `Total Mwh`)) %>%
  rename(`Cathode Mix` = `Cathode Mix.x`) %>%
  mutate(Sale_Year = as.numeric(Sale_Year)) %>%
  bind_rows(HDV_chem_hist)

# Historical recycling chemistry
hist_recycle_chem <- merge(chem_Mwh, hist_recycle_cap,
                           by = c("Sale_Year", "Propulsion", "Segment"), all.x = TRUE)
hist_recycle_chem$Cathode_kwh_state <- hist_recycle_chem$LIB_recycle_kwh *
  hist_recycle_chem$`Share of Avg Chem`
hist_recycle_chem <- hist_recycle_chem %>%
  mutate(Sale_Year = as.integer(Sale_Year)) %>%
  select(Fleet_Scenario, Year, Sale_Year, State_Province, `Cathode Mix`, Cathode_kwh_state, LIB_recycle_kwh)

# hist_final (mineral intensity join) — commented out
# hist_final <- left_join(
#   hist_recycle_chem, mineral_intensity,
#   by = "Cathode Mix", relationship = "many-to-many"
# ) %>%
#   mutate(`Available Recycled Minerals (kg)` = kg_per_kwh * Cathode_kwh_state) %>%
#   select(Sale_Year, State_Province, Mineral, Year, `Available Recycled Minerals (kg)`) %>%
#   group_by(Year, State_Province, Mineral) %>%
#   summarise(`Available Recycled Minerals (kg)` = sum(`Available Recycled Minerals (kg)`,
#                                                      na.rm = TRUE), .groups = "drop") %>%
#   filter(!is.na(Mineral))


## --- 8. Battery Capacity & Chemistry Scenarios -----------------------
## (from: Scenarios_SetUp.R)

cathode_projections <- read_excel(
  file.path(INPUT_DIR, "Cathode Projections (1).xlsx"), sheet = "Sheet1"
)

fixed_batt_cap_merged <- batt_cap_merged %>%
  select(-c(`Total Sales`, `Total Mwh`)) %>% filter(Propulsion != "FCEV")
batt_cap_merged$Sale_Year <- as.numeric(batt_cap_merged$Sale_Year)

batt_cap_2024 <- batt_cap_merged %>%
  filter(Sale_Year == 2024) %>%
  select(Sale_Year, Segment, Propulsion, Base_Capacity = `Avg Batt Cap (kwh/batt)`)

years_batt_cap <- 2025:2035

trend_results <- batt_cap_merged %>%
  filter(!is.na(`Avg Batt Cap (kwh/batt)`), Propulsion != "FCEV") %>%
  group_by(Segment, Propulsion) %>%
  filter(n() >= 3) %>%
  summarise(trend = coef(lm(`Avg Batt Cap (kwh/batt)` ~ Sale_Year))[2],
            .groups = "drop")

projection_base <- batt_cap_2024 %>%
  inner_join(trend_results, by = c("Segment", "Propulsion")) %>%
  crossing(years_batt_cap)

batt_cap_projection <- projection_base %>%
  mutate(`Projected Avg Batt Cap (kwh/batt)` = Base_Capacity + (years_batt_cap - 2024) * trend) %>%
  select(-Sale_Year) %>% rename(Sale_Year = years_batt_cap)

# After 2035 hold constant, then bind HDV
batt_cap_projection <- batt_cap_projection %>%
  group_by(Segment, Propulsion) %>%
  complete(Sale_Year = 2025:2050) %>%
  mutate(`Projected Avg Batt Cap (kwh/batt)` = ifelse(
    Sale_Year > 2035,
    `Projected Avg Batt Cap (kwh/batt)`[Sale_Year == 2035][1],
    `Projected Avg Batt Cap (kwh/batt)`
  )) %>%
  fill(`Projected Avg Batt Cap (kwh/batt)`, .direction = "down") %>%
  ungroup() %>%
  select(-c(Base_Capacity, trend)) %>%
  bind_rows(projection_HDV_full)

# 15% lower battery capacity scenario
batt_cap_2040 <- batt_cap_2024 %>% mutate(Base_Capacity = Base_Capacity * 0.85, Sale_Year = 2040)
batts <- bind_rows(batt_cap_2024, batt_cap_2040)

second_trend_results <- batts %>%
  group_by(Segment, Propulsion) %>%
  filter(n() == 2) %>%
  reframe(
    cap_2024  = Base_Capacity[Sale_Year == 2024],
    cap_2040  = Base_Capacity[Sale_Year == 2040],
    slope     = (cap_2040 - cap_2024) / (2040 - 2024),
    intercept = cap_2024 - slope * 2024
  )

all_batt_cap_years <- 2025:2050

batt_cap_15 <- second_trend_results %>%
  crossing(Sale_Year = all_batt_cap_years) %>%
  mutate(`Projected Avg Batt Cap (kwh/batt)` = case_when(
    Sale_Year <= 2040 ~ intercept + slope * Sale_Year,
    TRUE ~ intercept + slope * 2040
  )) %>%
  filter(Propulsion != "FCEV") %>%
  bind_rows(HDV_batt_cap_15) %>%
  select(-c(cap_2024, cap_2040, intercept, slope))

# Benchmark chemistry projections
cp <- cathode_projections[12:21, ] %>%
  select(-`...2`, -`...3`) %>%
  rename(`Cathode Mix` = `...1`) %>%
  slice(-1)

cp_melted <- cp %>%
  pivot_longer(-`Cathode Mix`, names_to = "Sale_Year", values_to = "Total Mwh") %>%
  mutate(Sale_Year = as.integer(Sale_Year)) %>%
  group_by(Sale_Year) %>%
  mutate(`Cathode Mix Share` = `Total Mwh` / sum(`Total Mwh`, na.rm = TRUE)) %>%
  ungroup()

replacement_future <- c(
  'NCM low nickel'  = 'NMC 111',
  'NCM mid nickel'  = 'NMC 622',
  'NCM high nickel' = 'NMC 811'
)
cp_melted$`Cathode Mix` <- recode(cp_melted$`Cathode Mix`, !!!replacement_future)
cp_melted <- cp_melted %>% filter(`Cathode Mix Share` != 0)

fixed_cp <- cp_melted

max_future <- fixed_cp %>%
  group_by(Sale_Year) %>%
  slice_max(`Cathode Mix Share`, n = 1, with_ties = FALSE) %>%
  ungroup()

future_match <- left_join(fixed_cp, max_future, by = "Sale_Year",
                          suffix = c("_x", "_y"), relationship = "many-to-one")
mask_mins_future <- future_match$`Cathode Mix_x` %in%
  c("4V Ni or Mn based", "5V Mn based", "LCO", "Other")
future_match$`Cathode Mix_x`[mask_mins_future] <- future_match$`Cathode Mix_y`[mask_mins_future]

# Drop Total Mwh after summarisation
future_match <- future_match %>%
  select(Sale_Year, `Cathode Mix` = `Cathode Mix_x`,
         `Cathode Mix Share` = `Cathode Mix Share_x`,
         `Total Mwh` = `Total Mwh_x`) %>%
  group_by(Sale_Year, `Cathode Mix`) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE),
            `Cathode Mix Share` = sum(`Cathode Mix Share`, na.rm = TRUE),
            .groups = "drop") %>%
  select(-`Total Mwh`)

# Extend to 2050
df_2040_proj   <- future_match %>% filter(Sale_Year == 2040)
df_extend_proj <- df_2040_proj %>% mutate(Sale_Year = list(2041:2050)) %>% unnest(Sale_Year)

future_match <- bind_rows(
  future_match %>% filter(Sale_Year <= 2040),
  df_extend_proj
) %>% arrange(`Cathode Mix`, Sale_Year)

# Add LDV Propulsion/Segment combinations and HDV
future_match_HDV <- future_match %>%
  crossing(
    Propulsion = c("BEV", "PHEV"),
    Segment    = c("Car", "SUV")
  ) %>%
  bind_rows(HDV_chem_project)

# High LFP scenario
total_mwh_per_year <- cp_melted %>%
  group_by(Sale_Year) %>%
  summarise(Total_Mwh = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop")

lfp_targets <- tibble(
  Sale_Year = unique(cp_melted$Sale_Year),
  LFP_share_target = scales::rescale(Sale_Year, to = c(0.27, 0.5))
)

lfp_mwh_per_year <- total_mwh_per_year %>%
  left_join(lfp_targets, by = "Sale_Year") %>%
  mutate(LFP_Mwh = Total_Mwh * LFP_share_target)

chem_with_targets <- future_match %>%
  left_join(lfp_mwh_per_year, by = "Sale_Year")

lfp_rows <- chem_with_targets %>%
  filter(`Cathode Mix` == "LFP") %>%
  mutate(Adjusted_Mwh = LFP_Mwh, New_Cathode_Share = LFP_share_target)

other_chems <- chem_with_targets %>% filter(`Cathode Mix` != "LFP")
adjusted_other_chems <- other_chems %>%
  group_by(Sale_Year) %>%
  mutate(
    total_other_share = sum(`Cathode Mix Share`, na.rm = TRUE),
    remaining_mwh     = unique(Total_Mwh) - unique(LFP_Mwh),
    Adjusted_Mwh      = (`Cathode Mix Share` / total_other_share) * remaining_mwh,
    New_Cathode_Share  = Adjusted_Mwh / Total_Mwh
  ) %>% ungroup()

final_adjusted_mix <- bind_rows(lfp_rows, adjusted_other_chems) %>%
  mutate(`Cathode Mix Share` = New_Cathode_Share) %>%
  select(Sale_Year, `Cathode Mix`, `Cathode Mix Share`)

df_2040_adjusted <- final_adjusted_mix %>% filter(Sale_Year == 2040)
df_extend_adjusted <- df_2040_adjusted %>%
  mutate(Sale_Year = list(2041:2050)) %>% unnest(Sale_Year)

final_adjusted_mix_extended <- bind_rows(
  final_adjusted_mix %>% filter(Sale_Year <= 2040),
  df_extend_adjusted
) %>%
  arrange(`Cathode Mix`, Sale_Year) %>%
  crossing(
    Propulsion = c("BEV", "PHEV"),
    Segment    = c("Car", "SUV")
  ) %>%
  bind_rows(HDV_chem_project)


cat("=== 01-Recycling_Data_Preparation.R complete ===\n")
cat("Fleet scenarios loaded:", paste(FLEET_SCENARIOS, collapse = ", "), "\n")
