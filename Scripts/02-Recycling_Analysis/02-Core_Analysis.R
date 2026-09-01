## ====================================================================
## 02-Recycling_Analysis.R
## Consolidates: Manufacturing_Recycling_Demand +
##               Future Recycling Minerals_new +
##               Future Demand Minerals
##
## Run 01-Recycling_Data_Preparation.R FIRST (or source it below)
## Working directory should be the project root (Fleet model/)
##
## Fleet scenario for analysis: set FLEET_SCENARIO in 03 (before source 02)
## or in the console, then source this file. See section 0b after 01 loads.
## ====================================================================

## --- 0. Source Data Preparation Script --------------------------------

source(file.path("Scripts", "02-Recycling_Analysis", "01-Recycling_Data_Preparation.R"))

suppressPackageStartupMessages({
  library(openxlsx)
  library(colorspace)
  library(forcats)
})


## --- 0b. Select Fleet Scenario for Analysis --------------------------
## 01 loads both ACCII and Repeal; select one here for the analysis run.
## Change FLEET_SCENARIO to "Repeal" and re-run for the other scenario.

if (!exists("FLEET_SCENARIO") || is.null(FLEET_SCENARIO)) {
  FLEET_SCENARIO <- FLEET_SCENARIOS[1]
}
cat("Analyzing fleet scenario:", FLEET_SCENARIO, "\n")

filter_to_scenario <- function(df, scen) {
  if ("Fleet_Scenario" %in% names(df)) {
    df %>% filter(Fleet_Scenario == scen | is.na(Fleet_Scenario)) %>% select(-Fleet_Scenario)
  } else {
    df
  }
}

EVLIB_Flows       <- filter_to_scenario(EVLIB_Flows, FLEET_SCENARIO)
EV_Flows          <- filter_to_scenario(EV_Flows, FLEET_SCENARIO)
BESSLIB_Flows     <- filter_to_scenario(BESSLIB_Flows, FLEET_SCENARIO)
HDV_LIBFlows      <- filter_to_scenario(HDV_LIBFlows, FLEET_SCENARIO)
HDV_BESSLIB_Flows <- filter_to_scenario(HDV_BESSLIB_Flows, FLEET_SCENARIO)
future_recycle_type <- filter_to_scenario(future_recycle_type, FLEET_SCENARIO)
hist_recycle_type   <- filter_to_scenario(hist_recycle_type, FLEET_SCENARIO)
hist_recycle_cap    <- filter_to_scenario(hist_recycle_cap, FLEET_SCENARIO)
hist_recycle_chem   <- filter_to_scenario(hist_recycle_chem, FLEET_SCENARIO)


## --- 0c. Country Codes -----------------------------------------------

us_codes <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
  "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK",
  "OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
  "WI","WY"
)

ca_codes <- c(
  "AB","BC","MB","NB","NL","NS","ON","PE","QC","SK","NT","NU","YT"
)


## --- 0d. Extend Chemistry Scenarios with Segment/Propulsion ----------

seg_prop_combos <- bind_rows(
  expand_grid(Segment = c("Car", "SUV"), Propulsion = c("BEV", "PHEV")),
  tibble(Segment = "HDV", Propulsion = "HDV")
)

## future_match_HDV and final_adjusted_mix_extended already include
## Segment × Propulsion (+ HDV) in 01 — no second crossing here.
if (!exists("future_match_HDV")) {
  future_match_HDV <- future_match %>%
    crossing(seg_prop_combos)
}


## =====================================================================
## PART A:  MANUFACTURING & RECYCLING CAPACITY
## (from: Manufacturing_Recycling_Demand.R)
## =====================================================================

## --- 1. Scrap by Mass ------------------------------------------------

scrap_by_mass <- read_excel(
  file.path(INPUT_DIR, "Scrap_by_Mass (-Energy).xlsx"), na = "", skip = 1
) %>%
  select(Chemistry, `Total Mass`, `Cell Mass`, `Pack Mass`) %>%
  filter(!is.na(Chemistry), !is.na(`Total Mass`), !is.na(`Cell Mass`)) %>%
  mutate(
    Total_Cell_Mass_per_Year = `Cell Mass` * 211000000,
    Scrap_rate_kg_per_kg_cell = `Total Mass` / Total_Cell_Mass_per_Year,
    Cell_Pack = `Cell Mass` * 400 / `Pack Mass`
  ) %>%
  mutate(Avg = sum(Scrap_rate_kg_per_kg_cell) / 9,
         Cell_Pack = sum(Cell_Pack) / 9)

## --- 2. Recycling Capacity -------------------------------------------

recycling_cap <- read_excel(
  file.path(INPUT_DIR, "NA Recycling facilities.xlsx")
) %>%
  select("State/ Province", "Year online simplified", "Capacity simplified",
         "Feedstock simplified", "Final product recycling category") %>%
  rename(Year_online = `Year online simplified`,
         Capacity_Mt_yr = `Capacity simplified`,
         Feedstock = `Feedstock simplified`,
         Product_category = `Final product recycling category`) %>%
  mutate(
    Year_online = ifelse(Year_online == "Online", 2025, Year_online),
    Year_online = as.integer(Year_online)
  ) %>%
  filter(!is.na(Capacity_Mt_yr)) %>%
  mutate(Delay_online = case_when(
    Feedstock == "End-of-life battery" &
      Product_category == "Output" & Year_online > 2025 ~ Year_online + 5,
    Feedstock == "Black Mass" &
      Product_category == "Output" & Year_online > 2025 ~ Year_online + 5,
    TRUE ~ Year_online
  ))

black_mass <- recycling_cap %>%
  filter((Product_category == "Output" & Feedstock == "End-of-life battery") |
           Product_category == "Intermediate") %>%
  group_by(`State/ Province`, Year_online) %>%
  summarise(Black_mass_cap = sum(Capacity_Mt_yr, na.rm = TRUE), .groups = "drop") %>%
  arrange(`State/ Province`, Year_online) %>%
  group_by(`State/ Province`) %>%
  mutate(Cumulative_black_mass_cap = cumsum(Black_mass_cap)) %>%
  rename(Year = Year_online) %>% select(-Black_mass_cap)

delay_black_mass <- recycling_cap %>%
  filter((Product_category == "Output" & Feedstock == "End-of-life battery") |
           Product_category == "Intermediate") %>%
  group_by(`State/ Province`, Delay_online) %>%
  summarise(Delay_Black_mass_cap = sum(Capacity_Mt_yr, na.rm = TRUE), .groups = "drop") %>%
  arrange(`State/ Province`, Delay_online) %>%
  group_by(`State/ Province`) %>%
  mutate(Delay_Cumulative_black_mass_cap = cumsum(Delay_Black_mass_cap)) %>%
  rename(Year = Delay_online) %>% select(-Delay_Black_mass_cap)

black_mass_cap <- black_mass %>%
  full_join(delay_black_mass, by = c("Year", "State/ Province")) %>%
  mutate(`State/ Province` = as.character(`State/ Province`), Year = as.integer(Year))

refining <- recycling_cap %>%
  filter(Product_category == "Output") %>%
  group_by(`State/ Province`, Year_online) %>%
  summarise(Refining_cap = sum(Capacity_Mt_yr, na.rm = TRUE), .groups = "drop") %>%
  arrange(`State/ Province`, Year_online) %>%
  group_by(`State/ Province`) %>%
  mutate(Cumulative_refining_cap = cumsum(Refining_cap)) %>%
  rename(Year = Year_online) %>% select(-Refining_cap)

delay_refining <- recycling_cap %>%
  filter(Product_category == "Output") %>%
  group_by(`State/ Province`, Delay_online) %>%
  summarise(Delay_refining_cap = sum(Capacity_Mt_yr, na.rm = TRUE), .groups = "drop") %>%
  arrange(`State/ Province`, Delay_online) %>%
  group_by(`State/ Province`) %>%
  mutate(Delay_Cumulative_refining_cap = cumsum(Delay_refining_cap)) %>%
  rename(Year = Delay_online) %>% select(-Delay_refining_cap)

refining_cap <- refining %>%
  full_join(delay_refining, by = c("Year", "State/ Province")) %>%
  mutate(`State/ Province` = as.character(`State/ Province`), Year = as.integer(Year))

recycling_tonnes_by_state <- full_join(
  black_mass_cap, refining_cap, by = c("State/ Province", "Year")
) %>%
  rename(State_Province = `State/ Province`) %>%
  select(Year, State_Province,
         Cumulative_black_mass_cap, Delay_Cumulative_black_mass_cap,
         Cumulative_refining_cap, Delay_Cumulative_refining_cap) %>%
  mutate(State_Province = as.character(State_Province), Year = as.integer(Year)) %>%
  ungroup() %>%
  complete(State_Province, Year = seq(min(Year, na.rm = TRUE), 2035, 1)) %>%
  arrange(State_Province, Year) %>%
  group_by(State_Province) %>%
  fill(Cumulative_black_mass_cap, Delay_Cumulative_black_mass_cap,
       Cumulative_refining_cap, Delay_Cumulative_refining_cap,
       .direction = "down") %>%
  mutate(across(c(Cumulative_black_mass_cap, Delay_Cumulative_black_mass_cap,
                  Cumulative_refining_cap, Delay_Cumulative_refining_cap),
                ~ replace_na(.x, 0))) %>%
  filter(Year <= 2035) %>%
  ungroup() %>%
  group_by(State_Province) %>%
  complete(Year = 2025:2050) %>%
  fill(Cumulative_black_mass_cap, Cumulative_refining_cap,
       Delay_Cumulative_black_mass_cap, Delay_Cumulative_refining_cap,
       .direction = "down") %>%
  ungroup()

recycling_tonnes_2050_projected <- recycling_tonnes_by_state %>%
  filter(Year == 2050) %>%
  select(-Delay_Cumulative_black_mass_cap, -Delay_Cumulative_refining_cap)

recycling_tonnes_2050_delayed <- recycling_tonnes_by_state %>%
  filter(Year == 2050) %>%
  select(-Cumulative_black_mass_cap, -Cumulative_refining_cap)

recycling_tonnes_total <- recycling_tonnes_by_state %>%
  group_by(Year) %>%
  summarise(
    Cumulative_black_mass_cap       = sum(Cumulative_black_mass_cap, na.rm = TRUE),
    Cumulative_refining_cap         = sum(Cumulative_refining_cap, na.rm = TRUE),
    Delay_Cumulative_black_mass_cap = sum(Delay_Cumulative_black_mass_cap, na.rm = TRUE),
    Delay_Cumulative_refining_cap   = sum(Delay_Cumulative_refining_cap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Full_Recycle = case_when(
      Cumulative_refining_cap > Cumulative_black_mass_cap ~ Cumulative_black_mass_cap,
      TRUE ~ Cumulative_refining_cap),
    Delay_Full_Recycle = case_when(
      Delay_Cumulative_refining_cap > Delay_Cumulative_black_mass_cap ~ Delay_Cumulative_black_mass_cap,
      TRUE ~ Delay_Cumulative_refining_cap)
  )

NA_recycling_tonnes <- recycling_tonnes_total %>%
  select(-Delay_Cumulative_black_mass_cap, -Delay_Cumulative_refining_cap,
         -Full_Recycle, -Delay_Full_Recycle)


## --- 3. Specific Energy ----------------------------------------------

specific_energy <- read_csv(
  file.path(INPUT_DIR, "Specific_Energy (-Energy BatPac).csv"),
  show_col_types = FALSE
) %>% rename(`Cathode Mix` = `Battery Chem`)

specific_energy <- specific_energy %>%
  bind_rows(
    tibble(`Cathode Mix` = "NMCA",
           Pack_kg_kwh = (specific_energy$Pack_kg_kwh[specific_energy$`Cathode Mix` == "NCA"] +
                            specific_energy$Pack_kg_kwh[specific_energy$`Cathode Mix` == "NMC 811"]) / 2,
           Cell_kg_kwh = (specific_energy$Cell_kg_kwh[specific_energy$`Cathode Mix` == "NCA"] +
                            specific_energy$Cell_kg_kwh[specific_energy$`Cathode Mix` == "NMC 811"]) / 2),
    tibble(`Cathode Mix` = "High/Mid NMC",
           Pack_kg_kwh = (specific_energy$Pack_kg_kwh[specific_energy$`Cathode Mix` == "NMC 622"] +
                            specific_energy$Pack_kg_kwh[specific_energy$`Cathode Mix` == "NMC 811"]) / 2,
           Cell_kg_kwh = (specific_energy$Cell_kg_kwh[specific_energy$`Cathode Mix` == "NMC 622"] +
                            specific_energy$Cell_kg_kwh[specific_energy$`Cathode Mix` == "NMC 811"]) / 2),
    tibble(`Cathode Mix` = "LCO", Pack_kg_kwh = 5.85, Cell_kg_kwh = 3.85)
  ) %>%
  mutate(`Cathode Mix` = if_else(`Cathode Mix` == "NMC 333", "NMC 111", `Cathode Mix`))


## --- 4. Manufacturing Data -------------------------------------------

decline_years <- 6

manu_mid_down_path <- file.path(INPUT_DIR, "Manu_Mid_Down.xlsx")
manu_mid_down_sheet <- intersect(
  c("Narrowed Manu facilities", "changed dates Narrowed Manu fac"),
  openxlsx::getSheetNames(manu_mid_down_path)
)[1]
if (is.na(manu_mid_down_sheet)) {
  stop("Manu_Mid_Down.xlsx: expected sheet 'Narrowed Manu facilities' (or legacy ",
       "'changed dates Narrowed Manu fac'). Available sheets include: ",
       paste(head(openxlsx::getSheetNames(manu_mid_down_path), 15), collapse = ", "))
}

all_manufacturing <- read.xlsx(manu_mid_down_path, sheet = manu_mid_down_sheet) %>%
  select(Year.online, Production.Capacity, Company,
         Facility.State.or.Province, Supply.Chain.Segment, Chemistry) %>%
  mutate(Gwh_yr = as.numeric(Production.Capacity)) %>%
  group_by(Supply.Chain.Segment, Facility.State.or.Province, Year.online) %>%
  summarise(Gwh_yr = sum(Gwh_yr, na.rm = TRUE), .groups = "drop") %>%
  rename(Year_Online = Year.online, State_Province = Facility.State.or.Province) %>%
  filter(!is.na(Year_Online), Gwh_yr != 0) %>%
  mutate(Year_Online = as.numeric(Year_Online)) %>%
  pivot_wider(names_from = Supply.Chain.Segment, values_from = Gwh_yr,
              values_fn = sum, values_fill = list(Gwh_yr = 0)) %>%
  mutate(Downstream = Downstream * 0.77, Midstream = Midstream * 0.77)

delayed_manufacturing <- all_manufacturing %>%
  mutate(Year_Online = case_when(Year_Online > 2026 ~ Year_Online + 5, TRUE ~ Year_Online))

calendar <- expand_grid(
  State_Province = unique(all_manufacturing$State_Province),
  Year = 2025:2035
)
calendar_delayed <- expand_grid(
  State_Province = unique(all_manufacturing$State_Province),
  Year = 2025:2040
)

all_manufacturing_expanded <- calendar %>%
  left_join(all_manufacturing, by = "State_Province", relationship = "many-to-many") %>%
  arrange(State_Province, Year) %>%
  group_by(State_Province, Year_Online) %>%
  mutate(
    Production_Adjusted_Down = ifelse(Year < Year_Online, 0, Downstream),
    Production_Adjusted_Mid  = ifelse(Year < Year_Online, 0, Midstream),
    Scrap_Years = ifelse(Year >= Year_Online, Year - Year_Online + 1, NA),
    Scrap_Rate_Mid = if_else(!is.na(Scrap_Years),
                             seq(0.1105567, 0.0772, length.out = decline_years)[
                               pmin(Scrap_Years, decline_years)], 0),
    Scrap_Rate_Down = 0.05,
    Gwh_Scrap_Down = Production_Adjusted_Down * Scrap_Rate_Down,
    Production_After_Scrap_Down = Production_Adjusted_Down * (1 - Scrap_Rate_Down),
    Gwh_Scrap_Mid = Production_Adjusted_Mid * Scrap_Rate_Mid,
    Production_After_Scrap_Mid = Production_Adjusted_Mid * (1 - Scrap_Rate_Mid)
  ) %>% ungroup() %>%
  group_by(Year, State_Province) %>%
  summarise(
    Gwh_Scrap_Down = sum(Gwh_Scrap_Down, na.rm = TRUE),
    Production_After_Scrap_Down = sum(Production_After_Scrap_Down, na.rm = TRUE),
    Gwh_Scrap_Mid = sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Production_After_Scrap_Mid = sum(Production_After_Scrap_Mid, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share_of_Year_Scrap_Down = Gwh_Scrap_Down / sum(Gwh_Scrap_Down, na.rm = TRUE),
    Share_of_Year_Prod_Down  = Production_After_Scrap_Down / sum(Production_After_Scrap_Down, na.rm = TRUE),
    Share_of_Year_Scrap_Mid  = Gwh_Scrap_Mid / sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Share_of_Year_Prod_Mid   = Production_After_Scrap_Mid / sum(Production_After_Scrap_Mid, na.rm = TRUE)
  ) %>% ungroup()

delayed_manufacturing_expanded <- calendar_delayed %>%
  left_join(delayed_manufacturing, by = "State_Province", relationship = "many-to-many") %>%
  arrange(State_Province, Year) %>%
  group_by(State_Province, Year_Online) %>%
  mutate(
    Production_Adjusted_Down = ifelse(Year < Year_Online, 0, Downstream),
    Production_Adjusted_Mid  = ifelse(Year < Year_Online, 0, Midstream),
    Scrap_Rate_Mid  = 0.0767,
    Scrap_Rate_Down = 0.05,
    Gwh_Scrap_Down = Production_Adjusted_Down * Scrap_Rate_Down,
    Production_After_Scrap_Down = Production_Adjusted_Down * (1 - Scrap_Rate_Down),
    Gwh_Scrap_Mid = Production_Adjusted_Mid * Scrap_Rate_Mid,
    Production_After_Scrap_Mid = Production_Adjusted_Mid * (1 - Scrap_Rate_Mid)
  ) %>% ungroup() %>%
  group_by(Year, State_Province) %>%
  summarise(
    Gwh_Scrap_Down = sum(Gwh_Scrap_Down, na.rm = TRUE),
    Production_After_Scrap_Down = sum(Production_After_Scrap_Down, na.rm = TRUE),
    Gwh_Scrap_Mid = sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Production_After_Scrap_Mid = sum(Production_After_Scrap_Mid, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share_of_Year_Scrap_Down = Gwh_Scrap_Down / sum(Gwh_Scrap_Down, na.rm = TRUE),
    Share_of_Year_Prod_Down  = Production_After_Scrap_Down / sum(Production_After_Scrap_Down, na.rm = TRUE),
    Share_of_Year_Scrap_Mid  = Gwh_Scrap_Mid / sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Share_of_Year_Prod_Mid   = Production_After_Scrap_Mid / sum(Production_After_Scrap_Mid, na.rm = TRUE)
  ) %>% ungroup()

all_manu_chem <- read.csv(
  file.path(INPUT_DIR, "total_manufacturing_edited.csv")
) %>%
  filter(Product_Abbrev != "-", Gwh.yr != "-") %>%
  mutate(Gwh.yr = as.numeric(Gwh.yr)) %>%
  select(Year, Product_Abbrev, Gwh.yr) %>%
  group_by(Product_Abbrev) %>%
  summarise(Gwh.yr = sum(Gwh.yr, na.rm = TRUE), .groups = "drop") %>%
  mutate(Chem_Share = Gwh.yr / sum(Gwh.yr, na.rm = TRUE)) %>%
  mutate(Product_Abbrev = recode(Product_Abbrev, "NMC" = "High/Mid NMC")) %>%
  rename(`Cathode Mix` = Product_Abbrev)


## --- 5. State Demand & National Capacity -----------------------------

state_capacity_added <- EVLIB_Flows %>%
  group_by(State_Province, Year, Propulsion, Segment) %>%
  summarise(LIB_new_add = sum(LIB_new_add, na.rm = TRUE), .groups = "drop") %>%
  full_join(EV_Flows, by = c("State_Province", "Year", "Segment", "Propulsion")) %>%
  mutate(LIB_new_add = if_else(is.na(LIB_new_add), 0, LIB_new_add)) %>%
  mutate(Total_Add_LIB = LIB_new_add + Add_EV) %>%
  select(State_Province, Year, Segment, Propulsion, Total_Add_LIB)

HDV_add <- HDV_LIBFlows %>%
  group_by(State_Province, Year) %>%
  summarise(Total_Add_LIB = sum(New_Sales), .groups = "drop") %>%
  mutate(Propulsion = "HDV", Segment = "HDV")

state_capacity_added <- state_capacity_added %>%
  bind_rows(HDV_add)

caps_projected <- batt_cap_projection %>%
  select(Sale_Year, Segment, Propulsion, `Projected Avg Batt Cap (kwh/batt)`) %>%
  rename(Year = Sale_Year, Avg_Cap_Proj = `Projected Avg Batt Cap (kwh/batt)`) %>%
  group_by(Year, Segment, Propulsion) %>%
  summarise(Avg_Cap_Proj = first(Avg_Cap_Proj), .groups = "drop")

caps_15_projected <- batt_cap_15 %>%
  select(Sale_Year, Segment, Propulsion, `Projected Avg Batt Cap (kwh/batt)`) %>%
  rename(Year = Sale_Year, Avg_Cap_15 = `Projected Avg Batt Cap (kwh/batt)`) %>%
  group_by(Year, Segment, Propulsion) %>%
  summarise(Avg_Cap_15 = first(Avg_Cap_15), .groups = "drop")

chem_proj <- future_match_HDV %>%
  rename(Year = Sale_Year, Mix_proj = `Cathode Mix Share`)
chem_15 <- final_adjusted_mix_extended %>%
  rename(Year = Sale_Year, Mix_15 = `Cathode Mix Share`)
chems_proj_15 <- chem_proj %>%
  left_join(chem_15, by = c("Year", "Cathode Mix", "Segment", "Propulsion"),
            relationship = "many-to-many")

state_cap_add <- state_capacity_added %>%
  left_join(caps_projected, by = c("Year", "Segment", "Propulsion")) %>%
  left_join(caps_15_projected, by = c("Year", "Segment", "Propulsion")) %>%
  mutate(Add_LIB_Gwh_proj = Avg_Cap_Proj * Total_Add_LIB / 1e6,
         Add_LIB_Gwh_15   = Avg_Cap_15 * Total_Add_LIB / 1e6) %>%
  filter(Year >= 2025) %>%
  select(State_Province, Year, Propulsion, Segment, Add_LIB_Gwh_15, Add_LIB_Gwh_proj) %>%
  group_by(State_Province, Year, Propulsion, Segment) %>%
  summarise(
    Add_LIB_Gwh_proj = sum(Add_LIB_Gwh_proj, na.rm = TRUE),
    Add_LIB_Gwh_15   = sum(Add_LIB_Gwh_15, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(State_Province = case_when(
    State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
    TRUE ~ State_Province
  ))

state_cap_chem_tonne <- state_cap_add %>%
  left_join(chems_proj_15, by = c("Year", "Propulsion", "Segment"),
            relationship = "many-to-many") %>%
  mutate(
    Add_LIB_Gwh_proj_chem = Add_LIB_Gwh_proj * Mix_proj,
    Add_LIB_Gwh_15_chem   = Add_LIB_Gwh_15 * Mix_proj,
    Add_LIB_Gwh_proj_LFP  = Add_LIB_Gwh_proj * Mix_15,
    Add_LIB_Gwh_15_LFP    = Add_LIB_Gwh_15 * Mix_15
  ) %>%
  left_join(specific_energy, by = "Cathode Mix") %>%
  mutate(Pack_kg_Gwh = Pack_kg_kwh * 1e6) %>%
  mutate(
    Add_LIB_proj_tonnes     = (Add_LIB_Gwh_proj_chem * Pack_kg_Gwh) / 1000,
    Add_LIB_15_tonnes       = (Add_LIB_Gwh_15_chem * Pack_kg_Gwh) / 1000,
    Add_LIB_proj_LFP_tonnes = (Add_LIB_Gwh_proj_LFP * Pack_kg_Gwh) / 1000,
    Add_LIB_15_LFP_tonnes   = (Add_LIB_Gwh_15_LFP * Pack_kg_Gwh) / 1000
  ) %>%
  group_by(Year, State_Province) %>%
  summarise(
    Add_LIB_proj_tonnes     = sum(Add_LIB_proj_tonnes, na.rm = TRUE),
    Add_LIB_15_tonnes       = sum(Add_LIB_15_tonnes, na.rm = TRUE),
    Add_LIB_proj_LFP_tonnes = sum(Add_LIB_proj_LFP_tonnes, na.rm = TRUE),
    Add_LIB_15_LFP_tonnes   = sum(Add_LIB_15_LFP_tonnes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes,
         Add_LIB_proj_LFP_tonnes, Add_LIB_15_LFP_tonnes)

state_demand_tonnes_2050 <- state_cap_chem_tonne %>% filter(Year == 2050)

nat_cap_add <- state_cap_add %>%
  group_by(Year) %>%
  summarise(Add_LIB_Gwh_proj = sum(Add_LIB_Gwh_proj, na.rm = TRUE),
            Add_LIB_Gwh_15   = sum(Add_LIB_Gwh_15, na.rm = TRUE), .groups = "drop")

NA_demand_tonnes <- state_cap_chem_tonne %>%
  group_by(Year) %>%
  summarise(Add_LIB_proj_tonnes = sum(Add_LIB_proj_tonnes, na.rm = TRUE),
            Add_LIB_15_tonnes   = sum(Add_LIB_15_tonnes, na.rm = TRUE),
            .groups = "drop")


## --- 6. Manufacturing Projections ------------------------------------

nat_manu <- all_manufacturing_expanded %>%
  group_by(Year) %>%
  summarise(
    Gwh_Scrap_Down = sum(Gwh_Scrap_Down, na.rm = TRUE),
    Production_After_Scrap_Down = sum(Production_After_Scrap_Down, na.rm = TRUE),
    Gwh_Scrap_Mid = sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Production_After_Scrap_Mid = sum(Production_After_Scrap_Mid, na.rm = TRUE),
    .groups = "drop"
  )

nat_manu_delayed <- delayed_manufacturing_expanded %>%
  group_by(Year) %>%
  summarise(
    Gwh_Scrap_Down = sum(Gwh_Scrap_Down, na.rm = TRUE),
    Production_After_Scrap_Down = sum(Production_After_Scrap_Down, na.rm = TRUE),
    Gwh_Scrap_Mid = sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Production_After_Scrap_Mid = sum(Production_After_Scrap_Mid, na.rm = TRUE),
    .groups = "drop"
  )

cap_vs_manufac       <- left_join(nat_cap_add, nat_manu, by = "Year")
cap_vs_delayed_manu  <- left_join(nat_cap_add, nat_manu_delayed, by = "Year")

manu_projected <- cap_vs_manufac %>%
  fill(Production_After_Scrap_Down, Gwh_Scrap_Down,
       Production_After_Scrap_Mid, Gwh_Scrap_Mid, .direction = "down") %>%
  mutate(
    Production_After_Scrap_Down_proj = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_After_Scrap_Down),
    Gwh_Scrap_Down_Proj = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_proj ~
        Gwh_Scrap_Down + (1 - Production_After_Scrap_Down / Add_LIB_Gwh_proj) * Gwh_Scrap_Down,
      TRUE ~ Gwh_Scrap_Down),
    Production_After_Scrap_Mid_proj = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_After_Scrap_Mid),
    Gwh_Scrap_Mid_Proj = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_proj ~
        Gwh_Scrap_Mid + (1 - Production_After_Scrap_Mid / Add_LIB_Gwh_proj) * Gwh_Scrap_Mid,
      TRUE ~ Gwh_Scrap_Mid),
    Production_After_Scrap_Down_15 = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_After_Scrap_Down),
    Gwh_Scrap_Down_15 = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_15 ~
        Gwh_Scrap_Down + (1 - Production_After_Scrap_Down / Add_LIB_Gwh_15) * Gwh_Scrap_Down,
      TRUE ~ Gwh_Scrap_Down),
    Production_After_Scrap_Mid_15 = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_After_Scrap_Mid),
    Gwh_Scrap_Mid_15 = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_15 ~
        Gwh_Scrap_Mid + (1 - Production_After_Scrap_Mid / Add_LIB_Gwh_15) * Gwh_Scrap_Mid,
      TRUE ~ Gwh_Scrap_Mid)
  ) %>%
  select(-c(Production_After_Scrap_Down, Gwh_Scrap_Down,
            Production_After_Scrap_Mid, Gwh_Scrap_Mid))

manu_delayed <- cap_vs_delayed_manu %>%
  fill(Production_After_Scrap_Down, Gwh_Scrap_Down,
       Production_After_Scrap_Mid, Gwh_Scrap_Mid, .direction = "down") %>%
  mutate(
    Production_After_Scrap_Down_proj = case_when(
      Year > 2030 ~ Add_LIB_Gwh_proj, TRUE ~ Production_After_Scrap_Down),
    Gwh_Scrap_Down_Proj = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_proj ~
        Gwh_Scrap_Down + (1 - Production_After_Scrap_Down / Add_LIB_Gwh_proj) * Gwh_Scrap_Down,
      TRUE ~ Gwh_Scrap_Down - (1 - Add_LIB_Gwh_proj / Production_After_Scrap_Down) * Gwh_Scrap_Down),
    Production_After_Scrap_Mid_proj = case_when(
      Year > 2030 ~ Add_LIB_Gwh_proj, TRUE ~ Production_After_Scrap_Mid),
    Gwh_Scrap_Mid_Proj = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_proj ~
        Gwh_Scrap_Mid + (1 - Production_After_Scrap_Mid / Add_LIB_Gwh_proj) * Gwh_Scrap_Mid,
      TRUE ~ Gwh_Scrap_Mid - (1 - Add_LIB_Gwh_proj / Production_After_Scrap_Mid) * Gwh_Scrap_Mid),
    Production_After_Scrap_Down_15 = case_when(
      Year > 2030 ~ Add_LIB_Gwh_15, TRUE ~ Production_After_Scrap_Down),
    Gwh_Scrap_Down_15 = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_15 ~
        Gwh_Scrap_Down + (1 - Production_After_Scrap_Down / Add_LIB_Gwh_15) * Gwh_Scrap_Down,
      TRUE ~ Gwh_Scrap_Down - (1 - Add_LIB_Gwh_15 / Production_After_Scrap_Down) * Gwh_Scrap_Down),
    Production_After_Scrap_Mid_15 = case_when(
      Year > 2030 ~ Add_LIB_Gwh_15, TRUE ~ Production_After_Scrap_Mid),
    Gwh_Scrap_Mid_15 = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_15 ~
        Gwh_Scrap_Mid + (1 - Production_After_Scrap_Mid / Add_LIB_Gwh_15) * Gwh_Scrap_Mid,
      TRUE ~ Gwh_Scrap_Mid - (1 - Add_LIB_Gwh_15 / Production_After_Scrap_Mid) * Gwh_Scrap_Mid)
  ) %>%
  select(-c(Production_After_Scrap_Down, Gwh_Scrap_Down,
            Production_After_Scrap_Mid, Gwh_Scrap_Mid))

projected_manufac_by_chem <- tidyr::crossing(manu_projected, all_manu_chem) %>%
  mutate(
    Prod_proj_down  = Production_After_Scrap_Down_proj * Chem_Share,
    Prod_15_down    = Production_After_Scrap_Down_15 * Chem_Share,
    Scrap_proj_down = Gwh_Scrap_Down_Proj * Chem_Share,
    Scrap_15_down   = Gwh_Scrap_Down_15 * Chem_Share,
    Prod_proj_mid   = Production_After_Scrap_Mid_proj * Chem_Share,
    Prod_15_mid     = Production_After_Scrap_Mid_15 * Chem_Share,
    Scrap_proj_mid  = Gwh_Scrap_Mid_Proj * Chem_Share,
    Scrap_15_mid    = Gwh_Scrap_Mid_15 * Chem_Share
  ) %>%
  select(Year, `Cathode Mix`,
         Prod_proj_down, Prod_15_down, Scrap_proj_down, Scrap_15_down,
         Prod_proj_mid, Prod_15_mid, Scrap_proj_mid, Scrap_15_mid)

delayed_manufac_by_chem <- tidyr::crossing(manu_delayed, all_manu_chem) %>%
  mutate(
    Prod_proj_down  = Production_After_Scrap_Down_proj * Chem_Share,
    Prod_15_down    = Production_After_Scrap_Down_15 * Chem_Share,
    Scrap_proj_down = Gwh_Scrap_Down_Proj * Chem_Share,
    Scrap_15_down   = Gwh_Scrap_Down_15 * Chem_Share,
    Prod_proj_mid   = Production_After_Scrap_Mid_proj * Chem_Share,
    Prod_15_mid     = Production_After_Scrap_Mid_15 * Chem_Share,
    Scrap_proj_mid  = Gwh_Scrap_Mid_Proj * Chem_Share,
    Scrap_15_mid    = Gwh_Scrap_Mid_15 * Chem_Share
  ) %>%
  select(Year, `Cathode Mix`,
         Prod_proj_down, Prod_15_down, Scrap_proj_down, Scrap_15_down,
         Prod_proj_mid, Prod_15_mid, Scrap_proj_mid, Scrap_15_mid)

tonnes_manufac_projected <- projected_manufac_by_chem %>%
  left_join(specific_energy, by = "Cathode Mix", relationship = "many-to-many") %>%
  drop_na() %>%
  mutate(Pack_kg_Gwh = Pack_kg_kwh * 1e6, Cell_kg_Gwh = Cell_kg_kwh * 1e6) %>%
  mutate(
    Tonnes_Scrap_proj_down = (Scrap_proj_down * Pack_kg_Gwh) / 1000,
    Tonnes_Scrap_15_down   = (Scrap_15_down * Pack_kg_Gwh) / 1000,
    Tonnes_Prod_proj_down  = (Prod_proj_down * Pack_kg_Gwh) / 1000,
    Tonnes_Prod_15_down    = (Prod_15_down * Pack_kg_Gwh) / 1000,
    Tonnes_Scrap_proj_mid  = (Scrap_proj_mid * Cell_kg_Gwh) / 1000,
    Tonnes_Scrap_15_mid    = (Scrap_15_mid * Cell_kg_Gwh) / 1000,
    Tonnes_Prod_proj_mid   = (Prod_proj_mid * Cell_kg_Gwh) / 1000,
    Tonnes_Prod_15_mid     = (Prod_15_mid * Cell_kg_Gwh) / 1000
  ) %>%
  group_by(Year) %>%
  summarise(across(starts_with("Tonnes_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

tonnes_manufac_delayed <- delayed_manufac_by_chem %>%
  left_join(specific_energy, by = "Cathode Mix", relationship = "many-to-many") %>%
  drop_na() %>%
  mutate(Pack_kg_Gwh = Pack_kg_kwh * 1e6, Cell_kg_Gwh = Cell_kg_kwh * 1e6) %>%
  mutate(
    Tonnes_Scrap_proj_down = (Scrap_proj_down * Pack_kg_Gwh) / 1000,
    Tonnes_Scrap_15_down   = (Scrap_15_down * Pack_kg_Gwh) / 1000,
    Tonnes_Prod_proj_down  = (Prod_proj_down * Pack_kg_Gwh) / 1000,
    Tonnes_Prod_15_down    = (Prod_15_down * Pack_kg_Gwh) / 1000,
    Tonnes_Scrap_proj_mid  = (Scrap_proj_mid * Cell_kg_Gwh) / 1000,
    Tonnes_Scrap_15_mid    = (Scrap_15_mid * Cell_kg_Gwh) / 1000,
    Tonnes_Prod_proj_mid   = (Prod_proj_mid * Cell_kg_Gwh) / 1000,
    Tonnes_Prod_15_mid     = (Prod_15_mid * Cell_kg_Gwh) / 1000
  ) %>%
  group_by(Year) %>%
  summarise(across(starts_with("Tonnes_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

## Manufacturing by state (projected) — extend shares to 2050
all_manufacturing_expanded_complete_yrs <- all_manufacturing_expanded %>%
  filter(Year <= 2035) %>%
  group_by(State_Province) %>%
  complete(Year = 2025:2050) %>%
  fill(Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid,
       Share_of_Year_Scrap_Down, Share_of_Year_Scrap_Mid, .direction = "down") %>%
  ungroup() %>%
  select(Year, State_Province, Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid,
         Share_of_Year_Scrap_Down, Share_of_Year_Scrap_Mid)

delayed_all_manufacturing_expanded_complete_yrs <- delayed_manufacturing_expanded %>%
  filter(Year <= 2040) %>%
  group_by(State_Province) %>%
  complete(Year = 2025:2050) %>%
  fill(Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid,
       Share_of_Year_Scrap_Down, Share_of_Year_Scrap_Mid, .direction = "down") %>%
  ungroup() %>%
  select(Year, State_Province, Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid,
         Share_of_Year_Scrap_Down, Share_of_Year_Scrap_Mid)

manufacturing_by_state_projected <- all_manufacturing_expanded_complete_yrs %>%
  mutate(State_Province = if_else(State_Province == "SLP", "MX", State_Province)) %>%
  left_join(tonnes_manufac_projected, by = "Year") %>%
  mutate(
    Tonnes_Scrap_proj_down = Tonnes_Scrap_proj_down * coalesce(Share_of_Year_Scrap_Down, 0),
    Tonnes_Scrap_15_down   = Tonnes_Scrap_15_down * coalesce(Share_of_Year_Scrap_Down, 0),
    Tonnes_Prod_proj_down  = Tonnes_Prod_proj_down * coalesce(Share_of_Year_Prod_Down, 0),
    Tonnes_Prod_15_down    = Tonnes_Prod_15_down * coalesce(Share_of_Year_Prod_Down, 0),
    Tonnes_Scrap_proj_mid  = Tonnes_Scrap_proj_mid * coalesce(Share_of_Year_Scrap_Mid, 0),
    Tonnes_Scrap_15_mid    = Tonnes_Scrap_15_mid * coalesce(Share_of_Year_Scrap_Mid, 0),
    Tonnes_Prod_proj_mid   = Tonnes_Prod_proj_mid * coalesce(Share_of_Year_Prod_Mid, 0),
    Tonnes_Prod_15_mid     = Tonnes_Prod_15_mid * coalesce(Share_of_Year_Prod_Mid, 0)
  ) %>%
  select(Year, State_Province, starts_with("Tonnes_")) %>%
  ungroup()

manufacturing_by_state_delayed <- delayed_all_manufacturing_expanded_complete_yrs %>%
  mutate(State_Province = if_else(State_Province == "SLP", "MX", State_Province)) %>%
  left_join(tonnes_manufac_delayed, by = "Year") %>%
  mutate(
    Tonnes_Scrap_proj_down = Tonnes_Scrap_proj_down * coalesce(Share_of_Year_Scrap_Down, 0),
    Tonnes_Scrap_15_down   = Tonnes_Scrap_15_down * coalesce(Share_of_Year_Scrap_Down, 0),
    Tonnes_Prod_proj_down  = Tonnes_Prod_proj_down * coalesce(Share_of_Year_Prod_Down, 0),
    Tonnes_Prod_15_down    = Tonnes_Prod_15_down * coalesce(Share_of_Year_Prod_Down, 0),
    Tonnes_Scrap_proj_mid  = Tonnes_Scrap_proj_mid * coalesce(Share_of_Year_Scrap_Mid, 0),
    Tonnes_Scrap_15_mid    = Tonnes_Scrap_15_mid * coalesce(Share_of_Year_Scrap_Mid, 0),
    Tonnes_Prod_proj_mid   = Tonnes_Prod_proj_mid * coalesce(Share_of_Year_Prod_Mid, 0),
    Tonnes_Prod_15_mid     = Tonnes_Prod_15_mid * coalesce(Share_of_Year_Prod_Mid, 0)
  ) %>%
  select(Year, State_Province, starts_with("Tonnes_")) %>%
  ungroup()

NA_manu <- manufacturing_by_state_projected %>%
  group_by(Year) %>%
  summarise(
    Tonnes_Scrap_proj_down = sum(Tonnes_Scrap_proj_down),
    Tonnes_Scrap_15_down   = sum(Tonnes_Scrap_15_down),
    Tonnes_Prod_proj_down  = sum(Tonnes_Prod_proj_down),
    Tonnes_Prod_15_down    = sum(Tonnes_Prod_15_down),
    Tonnes_Scrap_proj_mid  = sum(Tonnes_Scrap_proj_mid),
    Tonnes_Scrap_15_mid    = sum(Tonnes_Scrap_15_mid),
    Tonnes_Prod_proj_mid   = sum(Tonnes_Prod_proj_mid),
    Tonnes_Prod_15_mid     = sum(Tonnes_Prod_15_mid),
    .groups = "drop"
  )

manufacturing_tonnes_2050_projected <- manufacturing_by_state_projected %>% filter(Year == 2050)
manufacturing_tonnes_2050_delayed   <- manufacturing_by_state_delayed %>% filter(Year == 2050)


## =====================================================================
## PART B:  FUTURE RECYCLING MINERALS
## (from: Future Recycling Minerals_new.R)
## =====================================================================

## --- 7. BatPac Scrap Minerals ----------------------------------------

batpac_scrap_min <- read_csv(
  file.path(INPUT_DIR, "Mins_in_scrap (-Energy BatPac).csv"),
  show_col_types = FALSE
) %>%
  select(where(~ !all(is.na(.))))

scrap_mass <- read_csv(
  file.path(INPUT_DIR, "Scrap_mass(-Energy BatPac).csv"),
  show_col_types = FALSE
) %>%
  select(where(~ !all(is.na(.)))) %>%
  rename(`Cathode Mix` = `Battery Chem`) %>%
  select(`Cathode Mix`, `Scrap kg/Gwh`)

colnames(batpac_scrap_min) <- c("Product_Abbrev", "Mineral", "Value")
batpac_scrap_min <- batpac_scrap_min %>%
  mutate(`kg/Gwh` = Value / 50)

mineral_map <- c(
  "Li, kg/yr" = "Lithium", "Ni, kg/yr" = "Nickel", "Co, kg/yr" = "Cobalt",
  "Mn, kg/yr" = "Manganese", "C, kg/yr" = "Carbon",
  "Al, kg/yr" = "Aluminum", "Cu, kg/yr" = "Copper"
)
batpac_scrap_min <- batpac_scrap_min %>%
  mutate(Mineral = recode(Mineral, !!!mineral_map),
         Product_Abbrev = str_trim(Product_Abbrev),
         `kg/Gwh` = as.numeric(`kg/Gwh`))

nmca_nmc <- mineral_intensity %>%
  filter(`Cathode Mix` %in% c("NMCA 89:4:4:3", "NMC 622"))
summary_chem <- nmca_nmc %>%
  group_by(`Cathode Mix`, Mineral) %>%
  summarise(total_kg_per_kwh = sum(kg_per_kwh, na.rm = TRUE), .groups = "drop")
ratio_nmca_nmc <- summary_chem %>%
  pivot_wider(names_from = `Cathode Mix`, values_from = total_kg_per_kwh) %>%
  mutate(ratio = as.numeric(`NMCA 89:4:4:3` / `NMC 622`))

nmca_rows <- batpac_scrap_min %>%
  filter(Product_Abbrev == "NMC 622") %>%
  left_join(ratio_nmca_nmc, by = "Mineral") %>%
  mutate(Product_Abbrev = "NMCA", `kg/Gwh` = `kg/Gwh` * ratio) %>%
  select(-ratio, -matches("^NMC 622$|^NMCA 89:4:4:3$"))

batpac_scrap_min_w_nmca <- bind_rows(batpac_scrap_min, nmca_rows)

mins_in_scrap <- batpac_scrap_min_w_nmca %>%
  rename(`Cathode Mix` = Product_Abbrev) %>%
  select(-Value) %>%
  filter(!is.na(`Cathode Mix`)) %>%
  full_join(scrap_mass, by = "Cathode Mix") %>%
  mutate(
    `kg/Gwh` = as.numeric(`kg/Gwh`),
    `Scrap kg/Gwh` = as.numeric(`Scrap kg/Gwh`),
    Min_kg_Scrap_tonne = `kg/Gwh` / (`Scrap kg/Gwh` * 1000)
  )

all_mins <- mineral_intensity %>%
  full_join(mins_in_scrap, by = c("Cathode Mix", "Mineral")) %>%
  mutate(
    kg_per_kwh = as.numeric(kg_per_kwh),
    Min_kg_Scrap_tonne = as.numeric(Min_kg_Scrap_tonne)
  ) %>%
  mutate(
    kg_per_kwh = ifelse(is.na(kg_per_kwh), 0, kg_per_kwh),
    Min_kg_Scrap_tonne = ifelse(is.na(Min_kg_Scrap_tonne), 0, Min_kg_Scrap_tonne)
  )


## --- 8. Recycling Data Clean -----------------------------------------

US_CA_Recycle <- recycling_tonnes_total %>%
  select(-c(Delay_Cumulative_black_mass_cap, Delay_Cumulative_refining_cap, Delay_Full_Recycle)) %>%
  rename(Black_Mass_MT = Cumulative_black_mass_cap, Refining_MT = Cumulative_refining_cap) %>%
  complete(Year = 2025:2050) %>%
  fill(Black_Mass_MT, Refining_MT, Full_Recycle, .direction = "down") %>%
  ungroup()

Delay_US_CA_Recycle <- recycling_tonnes_total %>%
  select(-c(Cumulative_black_mass_cap, Cumulative_refining_cap, Full_Recycle)) %>%
  rename(Black_Mass_MT = Delay_Cumulative_black_mass_cap,
         Refining_MT = Delay_Cumulative_refining_cap,
         Full_Recycle = Delay_Full_Recycle) %>%
  complete(Year = 2025:2050) %>%
  fill(Black_Mass_MT, Refining_MT, Full_Recycle, .direction = "down") %>%
  ungroup()


## --- 9. Production Share Calculations --------------------------------

p_all_manufacturing <- calendar %>%
  left_join(all_manufacturing, by = "State_Province", relationship = "many-to-many") %>%
  arrange(State_Province, Year) %>%
  group_by(State_Province, Year_Online) %>%
  mutate(
    Production_Adjusted_Down = ifelse(Year < Year_Online, 0, Downstream),
    Production_Adjusted_Mid  = ifelse(Year < Year_Online, 0, Midstream)
  ) %>% ungroup() %>%
  group_by(Year, State_Province) %>%
  summarise(
    Production_Adjusted_Down = sum(Production_Adjusted_Down, na.rm = TRUE),
    Production_Adjusted_Mid  = sum(Production_Adjusted_Mid, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share_of_Year_Prod_Down = Production_Adjusted_Down / sum(Production_Adjusted_Down, na.rm = TRUE),
    Share_of_Year_Prod_Mid  = Production_Adjusted_Mid / sum(Production_Adjusted_Mid, na.rm = TRUE)
  ) %>% ungroup() %>%
  mutate(State_Province = if_else(State_Province == "SLP", "MX", State_Province))

p_delayed_manufacturing <- calendar_delayed %>%
  left_join(delayed_manufacturing, by = "State_Province", relationship = "many-to-many") %>%
  arrange(State_Province, Year) %>%
  group_by(State_Province, Year_Online) %>%
  mutate(
    Production_Adjusted_Down = ifelse(Year < Year_Online, 0, Downstream),
    Production_Adjusted_Mid  = ifelse(Year < Year_Online, 0, Midstream)
  ) %>% ungroup() %>%
  group_by(Year, State_Province) %>%
  summarise(
    Production_Adjusted_Down = sum(Production_Adjusted_Down, na.rm = TRUE),
    Production_Adjusted_Mid  = sum(Production_Adjusted_Mid, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share_of_Year_Prod_Down = Production_Adjusted_Down / sum(Production_Adjusted_Down, na.rm = TRUE),
    Share_of_Year_Prod_Mid  = Production_Adjusted_Mid / sum(Production_Adjusted_Mid, na.rm = TRUE)
  ) %>% ungroup() %>%
  mutate(State_Province = if_else(State_Province == "SLP", "MX", State_Province))

p_nat_manu <- p_all_manufacturing %>%
  group_by(Year) %>%
  summarise(Production_Adjusted_Down = sum(Production_Adjusted_Down, na.rm = TRUE),
            Production_Adjusted_Mid  = sum(Production_Adjusted_Mid, na.rm = TRUE), .groups = "drop")
p_nat_manu_delayed <- p_delayed_manufacturing %>%
  group_by(Year) %>%
  summarise(Production_Adjusted_Down = sum(Production_Adjusted_Down, na.rm = TRUE),
            Production_Adjusted_Mid  = sum(Production_Adjusted_Mid, na.rm = TRUE), .groups = "drop")

p_cap_vs_manufac      <- left_join(nat_cap_add, p_nat_manu, by = "Year")
p_cap_vs_delayed_manu <- left_join(nat_cap_add, p_nat_manu_delayed, by = "Year")

p_manu_projected <- p_cap_vs_manufac %>%
  fill(Production_Adjusted_Down, Production_Adjusted_Mid, .direction = "down") %>%
  mutate(
    Production_Adjusted_Down_proj = case_when(
      Production_Adjusted_Down < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_Adjusted_Down),
    Production_Adjusted_Mid_proj = case_when(
      Production_Adjusted_Mid < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_Adjusted_Mid),
    Production_Adjusted_Down_15 = case_when(
      Production_Adjusted_Down < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_Adjusted_Down),
    Production_Adjusted_Mid_15 = case_when(
      Production_Adjusted_Mid < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_Adjusted_Mid)
  ) %>%
  select(-c(Production_Adjusted_Down, Production_Adjusted_Mid))

p_manu_delayed <- p_cap_vs_delayed_manu %>%
  fill(Production_Adjusted_Down, Production_Adjusted_Mid, .direction = "down") %>%
  mutate(
    Production_Adjusted_Down_proj = case_when(
      Production_Adjusted_Down < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_Adjusted_Down),
    Production_Adjusted_Mid_proj = case_when(
      Production_Adjusted_Mid < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_Adjusted_Mid),
    Production_Adjusted_Down_15 = case_when(
      Production_Adjusted_Down < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_Adjusted_Down),
    Production_Adjusted_Mid_15 = case_when(
      Production_Adjusted_Mid < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_Adjusted_Mid)
  ) %>%
  select(-c(Production_Adjusted_Down, Production_Adjusted_Mid))

p_projected_manufac_by_chem <- tidyr::crossing(p_manu_projected, all_manu_chem) %>%
  mutate(
    Prod_proj_down = Production_Adjusted_Down_proj * Chem_Share,
    Prod_15_down   = Production_Adjusted_Down_15 * Chem_Share,
    Prod_proj_mid  = Production_Adjusted_Mid_proj * Chem_Share,
    Prod_15_mid    = Production_Adjusted_Mid_15 * Chem_Share
  ) %>%
  select(Year, `Cathode Mix`, Prod_proj_down, Prod_15_down, Prod_proj_mid, Prod_15_mid)

p_delayed_manufac_by_chem <- tidyr::crossing(p_manu_delayed, all_manu_chem) %>%
  mutate(
    Prod_proj_down = Production_Adjusted_Down_proj * Chem_Share,
    Prod_15_down   = Production_Adjusted_Down_15 * Chem_Share,
    Prod_proj_mid  = Production_Adjusted_Mid_proj * Chem_Share,
    Prod_15_mid    = Production_Adjusted_Mid_15 * Chem_Share
  ) %>%
  select(Year, `Cathode Mix`, Prod_proj_down, Prod_15_down, Prod_proj_mid, Prod_15_mid)

p_all_manufacturing_complete <- p_all_manufacturing %>%
  filter(Year <= 2035) %>%
  group_by(State_Province) %>%
  complete(Year = 2025:2050) %>%
  fill(Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid, .direction = "down") %>%
  ungroup() %>%
  select(Year, State_Province, Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid)

p_delayed_manufacturing_complete <- p_delayed_manufacturing %>%
  filter(Year <= 2040) %>%
  group_by(State_Province) %>%
  complete(Year = 2025:2050) %>%
  fill(Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid, .direction = "down") %>%
  ungroup() %>%
  select(Year, State_Province, Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid)

p_clean_manu_projected_chem_state <- p_projected_manufac_by_chem %>%
  left_join(p_all_manufacturing_complete, by = "Year", relationship = "many-to-many") %>%
  mutate(
    Prod_proj = Prod_proj_mid * Share_of_Year_Prod_Mid,
    Prod_15   = Prod_15_mid * Share_of_Year_Prod_Mid,
    State_Province = if_else(State_Province == "SLP", "MX", State_Province)
  ) %>%
  select(Year, State_Province, `Cathode Mix`, Prod_proj, Prod_15) %>%
  fill(Year, State_Province, Prod_proj, Prod_15, .direction = "down")

p_clean_manu_delayed_chem_state <- p_delayed_manufac_by_chem %>%
  left_join(p_delayed_manufacturing_complete, by = "Year", relationship = "many-to-many") %>%
  mutate(
    Prod_proj = Prod_proj_mid * Share_of_Year_Prod_Mid,
    Prod_15   = Prod_15_mid * Share_of_Year_Prod_Mid,
    State_Province = if_else(State_Province == "SLP", "MX", State_Province)
  ) %>%
  select(Year, State_Province, `Cathode Mix`, Prod_proj, Prod_15) %>%
  fill(Year, State_Province, Prod_proj, Prod_15, .direction = "down")


## --- 10. Scrap tonnes ------------------------------------------------

clean_manu_projected_tonnes <- tonnes_manufac_projected %>%
  mutate(Scrap_proj_tonnes = Tonnes_Scrap_proj_mid, Scrap_15_tonnes = Tonnes_Scrap_15_mid) %>%
  select(Year, Scrap_proj_tonnes, Scrap_15_tonnes)

scrap_proj_tonnes <- clean_manu_projected_tonnes %>%
  select(Year, Scrap_proj_tonnes) %>%
  rename(Scrap_tonnes = Scrap_proj_tonnes) %>% mutate(Sale_Year = Year)
scrap_15_tonnes <- clean_manu_projected_tonnes %>%
  select(Year, Scrap_15_tonnes) %>%
  rename(Scrap_tonnes = Scrap_15_tonnes) %>% mutate(Sale_Year = Year)

clean_manu_delayed_tonnes <- tonnes_manufac_delayed %>%
  mutate(Scrap_proj_tonnes = Tonnes_Scrap_proj_mid, Scrap_15_tonnes = Tonnes_Scrap_15_mid) %>%
  select(Year, Scrap_proj_tonnes, Scrap_15_tonnes)

delay_scrap_proj_tonnes <- clean_manu_delayed_tonnes %>%
  select(Year, Scrap_proj_tonnes) %>%
  rename(Scrap_tonnes = Scrap_proj_tonnes) %>% mutate(Sale_Year = Year)
delay_scrap_15_tonnes <- clean_manu_delayed_tonnes %>%
  select(Year, Scrap_15_tonnes) %>%
  rename(Scrap_tonnes = Scrap_15_tonnes) %>% mutate(Sale_Year = Year)

prod_proj_Gwh_state <- p_clean_manu_projected_chem_state %>%
  select(Year, State_Province, `Cathode Mix`, Prod_proj) %>%
  rename(Prod_Gwh_state = Prod_proj)
prod_15_Gwh_state <- p_clean_manu_projected_chem_state %>%
  select(Year, State_Province, `Cathode Mix`, Prod_15) %>%
  rename(Prod_Gwh_state = Prod_15)

delay_prod_proj_Gwh_state <- p_clean_manu_delayed_chem_state %>%
  select(Year, State_Province, `Cathode Mix`, Prod_proj) %>%
  rename(Prod_Gwh_state = Prod_proj)
delay_prod_15_Gwh_state <- p_clean_manu_delayed_chem_state %>%
  select(Year, State_Province, `Cathode Mix`, Prod_15) %>%
  rename(Prod_Gwh_state = Prod_15)


## --- 11. Battery Cap + Scrap Joining ---------------------------------

batt_cap_project <- batt_cap_projection %>%
  left_join(scrap_proj_tonnes, by = "Sale_Year") %>%
  left_join(prod_proj_Gwh_state, by = c("Year")) %>%
  select(Sale_Year, State_Province, Segment, Propulsion,
         `Projected Avg Batt Cap (kwh/batt)`, `Cathode Mix`, Scrap_tonnes, Prod_Gwh_state)

names(batt_cap_15) <- trimws(names(batt_cap_15))
batt_cap_15_join <- batt_cap_15 %>%
  left_join(scrap_15_tonnes, by = "Sale_Year") %>%
  left_join(prod_15_Gwh_state, by = c("Year")) %>%
  select(Sale_Year, State_Province, Segment, Propulsion,
         `Projected Avg Batt Cap (kwh/batt)`, `Cathode Mix`, Scrap_tonnes, Prod_Gwh_state)

all_states <- tibble(State_Province = unique(c(unname(state_map_rev), "MX")))

combo_cathodes <- batt_cap_project %>%
  distinct(Sale_Year, Segment, Propulsion, `Cathode Mix`)
expanded_grid <- combo_cathodes %>% tidyr::crossing(all_states)

batt_cap_proj_ext <- expanded_grid %>%
  left_join(batt_cap_project,
            by = c("Sale_Year", "Segment", "Propulsion", "State_Province", "Cathode Mix"))

batt_cap_15_ext <- expanded_grid %>%
  left_join(batt_cap_15_join,
            by = c("Sale_Year", "Segment", "Propulsion", "State_Province", "Cathode Mix"))

combo_defaults_proj <- batt_cap_project %>%
  group_by(Sale_Year, Segment, Propulsion, `Cathode Mix`) %>%
  summarise(Scrap_tonnes = first(Scrap_tonnes),
            `Projected Avg Batt Cap (kwh/batt)` = first(`Projected Avg Batt Cap (kwh/batt)`),
            .groups = "drop")

combo_defaults_15 <- batt_cap_15_join %>%
  group_by(Sale_Year, Segment, Propulsion, `Cathode Mix`) %>%
  summarise(Scrap_tonnes = first(Scrap_tonnes),
            `Projected Avg Batt Cap (kwh/batt)` = first(`Projected Avg Batt Cap (kwh/batt)`),
            .groups = "drop")

batt_cap_proj_ext <- batt_cap_proj_ext %>%
  left_join(combo_defaults_proj, by = c("Sale_Year", "Segment", "Propulsion", "Cathode Mix")) %>%
  mutate(
    Scrap_tonnes = coalesce(Scrap_tonnes.x, Scrap_tonnes.y),
    `Projected Avg Batt Cap (kwh/batt)` = coalesce(
      `Projected Avg Batt Cap (kwh/batt).x`, `Projected Avg Batt Cap (kwh/batt).y`),
    Prod_Gwh_state = replace_na(Prod_Gwh_state, 0)
  ) %>%
  select(Sale_Year, Segment, Propulsion, State_Province, `Cathode Mix`,
         Scrap_tonnes, Prod_Gwh_state, `Projected Avg Batt Cap (kwh/batt)`) %>%
  mutate(Year = Sale_Year)

batt_cap_15_ext <- batt_cap_15_ext %>%
  left_join(combo_defaults_15, by = c("Sale_Year", "Segment", "Propulsion", "Cathode Mix")) %>%
  mutate(
    Scrap_tonnes = coalesce(Scrap_tonnes.x, Scrap_tonnes.y),
    `Projected Avg Batt Cap (kwh/batt)` = coalesce(
      `Projected Avg Batt Cap (kwh/batt).x`, `Projected Avg Batt Cap (kwh/batt).y`),
    Prod_Gwh_state = replace_na(Prod_Gwh_state, 0)
  ) %>%
  select(Sale_Year, Segment, Propulsion, State_Province, `Cathode Mix`,
         Scrap_tonnes, Prod_Gwh_state, `Projected Avg Batt Cap (kwh/batt)`) %>%
  mutate(Year = Sale_Year)

batt_scen  <- list(batt_cap_proj_ext, batt_cap_15_ext)
chem_scens <- list(future_match_HDV, final_adjusted_mix_extended)

future_recycle_type_collection <- future_recycle_type %>%
  mutate(State_Province = case_when(
    State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
    TRUE ~ State_Province
  )) %>%
  filter(Sale_Year >= 2025) %>%
  mutate(Sale_Year = as.integer(Sale_Year))


## --- 12. capacity_chem_scenarios Function  ---------------------------

capacity_chem_scenarios <- function(batt_cap_df, chem_df, mineral_intensity,
                                    future_recycle_type_collection) {
  batt_df_collapsed <- batt_cap_df %>%
    group_by(State_Province, Segment, Propulsion, Sale_Year) %>%
    summarise(`Projected Avg Batt Cap (kwh/batt)` = first(`Projected Avg Batt Cap (kwh/batt)`),
              .groups = "drop")

  future_recycle_cap <- future_recycle_type_collection %>%
    left_join(batt_df_collapsed, by = c("State_Province", "Sale_Year", "Segment", "Propulsion"))
  future_recycle_cap$LIB_recycle_kwh <- future_recycle_cap$LIB_recycle_total *
    future_recycle_cap$`Projected Avg Batt Cap (kwh/batt)`
  future_recycle_cap <- future_recycle_cap %>%
    group_by(Year, Sale_Year, State_Province, Propulsion, Segment) %>%
    summarise(LIB_recycle_kwh = sum(LIB_recycle_kwh, na.rm = TRUE), .groups = "drop") %>%
    arrange(State_Province, Year)

  nat_recycle_cap <- future_recycle_cap %>%
    group_by(Year) %>%
    summarise(LIB_recycle_Gwh = sum(LIB_recycle_kwh) / 1e6, .groups = "drop")

  future_recycle_chem_fut <- future_recycle_cap %>%
    left_join(chem_df, by = c("Sale_Year", "Propulsion", "Segment"),
              relationship = "many-to-many") %>%
    mutate(Cathode_kwh_state = LIB_recycle_kwh * `Cathode Mix Share`) %>%
    group_by(Sale_Year, State_Province, Year, `Cathode Mix`) %>%
    summarise(Cathode_kwh_state = sum(Cathode_kwh_state), .groups = "drop")

  future_recycle_chem <- bind_rows(hist_recycle_chem, future_recycle_chem_fut) %>%
    mutate(State_Province = case_when(
      State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
      TRUE ~ State_Province
    )) %>%
    group_by(Year, State_Province, `Cathode Mix`) %>%
    summarise(Cathode_kwh_state = sum(Cathode_kwh_state, na.rm = TRUE),
              LIB_recycle_kwh = sum(LIB_recycle_kwh, na.rm = TRUE), .groups = "drop") %>%
    arrange(State_Province, Year)

  future_mass_recycle_chem <- future_recycle_chem %>%
    inner_join(specific_energy, by = "Cathode Mix", relationship = "many-to-many") %>%
    mutate(Batt_Mass_MT = Cathode_kwh_state * Pack_kg_kwh / 1000)
  future_mass_recycle_total <- future_mass_recycle_chem %>%
    group_by(Year) %>%
    summarise(Batt_Mass_MT = sum(Batt_Mass_MT, na.rm = TRUE), .groups = "drop")

  state_mass_recycle_batt <- future_mass_recycle_chem %>%
    group_by(Year, State_Province) %>%
    summarise(Batt_Mass_MT = sum(Batt_Mass_MT, na.rm = TRUE), .groups = "drop")

  batt_df_nat_scrap <- batt_cap_df %>%
    group_by(Year) %>%
    summarise(Scrap_tonnes = first(Scrap_tonnes), .groups = "drop")

  Available_Recycling_Capacity <- US_CA_Recycle %>%
    inner_join(batt_df_nat_scrap, by = "Year") %>%
    mutate(
      Leftover_blackmass_cap = pmax(Black_Mass_MT - Scrap_tonnes / 0.7078558, 0),
      Leftover_Full_Recycle  = pmax(Full_Recycle - Scrap_tonnes / 0.7078558, 0),
      Scrap_full_recycle_percent   = pmin(Full_Recycle / (Scrap_tonnes / 0.7078558), 1),
      Unprocessed_Scrap            = pmax(Scrap_tonnes / 0.7078558 - Black_Mass_MT, 0),
      Unrefined_Scrap              = pmax(Scrap_tonnes / 0.7078558 - Full_Recycle, 0),
      Unprocessed_Scrap_percent    = 1 - pmin(Black_Mass_MT / (Scrap_tonnes / 0.7078558), 1),
      Exported_BM_Scrap_percent    = pmax((Scrap_tonnes / 0.7078558 - Unprocessed_Scrap - Full_Recycle), 0) /
        (Scrap_tonnes / 0.7078558)
    ) %>%
    inner_join(future_mass_recycle_total, by = "Year") %>%
    mutate(
      Post_consumer_blackmass_percent    = pmin(Leftover_blackmass_cap / Batt_Mass_MT, 1),
      Post_consumer_full_recycle_percent = pmin(Leftover_Full_Recycle / Batt_Mass_MT, 1),
      Unprocessed_Batts         = pmax(Batt_Mass_MT - Leftover_blackmass_cap, 0),
      Unrefined_Batts           = pmax(Batt_Mass_MT - Leftover_Full_Recycle, 0),
      Unprocessed_Batts_percent = 1 - Post_consumer_blackmass_percent,
      Exported_BM_Batts_percent = pmax((Batt_Mass_MT - Unprocessed_Batts - Leftover_Full_Recycle), 0) / Batt_Mass_MT,
      Unused_Black_Mass = pmax(Leftover_blackmass_cap - Batt_Mass_MT, 0),
      Unused_Refining   = pmax(Refining_MT - pmin((Batt_Mass_MT + Scrap_tonnes / 0.7078558) / Full_Recycle, 1) * Full_Recycle, 0),
      Needed_Black_Mass_change = (Unprocessed_Batts + Unprocessed_Scrap - Unused_Black_Mass),
      Needed_Refining_change   = (Unrefined_Batts + Unrefined_Scrap - Unused_Refining)
    ) %>%
    arrange(Year) %>%
    mutate(
      Needed_Black_Mass_level = accumulate(Needed_Black_Mass_change, ~ max(.x + .y, 0), .init = 0)[-1],
      Needed_Refining_level   = accumulate(Needed_Refining_change, ~ max(.x + .y, 0), .init = 0)[-1]
    ) %>%
    select(Year, Post_consumer_full_recycle_percent, Scrap_full_recycle_percent, Scrap_tonnes,
           Unprocessed_Batts_percent, Unprocessed_Scrap_percent,
           Exported_BM_Batts_percent, Exported_BM_Scrap_percent,
           Needed_Black_Mass_level, Needed_Refining_level) %>%
    mutate(across(where(is.numeric), ~ ifelse(abs(.) < 1e-12, 0, .)))

  future_recycle_chem <- future_recycle_chem %>%
    select(-LIB_recycle_kwh) %>%
    inner_join(Available_Recycling_Capacity, by = "Year") %>%
    mutate(
      Recycled_kwh_Batts      = Cathode_kwh_state * Post_consumer_full_recycle_percent,
      Unprocessed_kwh_Batts   = Cathode_kwh_state * Unprocessed_Batts_percent,
      Exported_BM_kwh_Batts   = Cathode_kwh_state * Exported_BM_Batts_percent
    ) %>%
    mutate(
      Recycled_Scrap    = Scrap_tonnes * Scrap_full_recycle_percent,
      Unprocessed_Scrap = Scrap_tonnes * Unprocessed_Scrap_percent,
      Exported_BM_Scrap = Scrap_tonnes * Exported_BM_Scrap_percent
    ) %>%
    group_by(Year, State_Province, `Cathode Mix`) %>%
    summarise(
      Recycled_kwh_Batts    = sum(Recycled_kwh_Batts, na.rm = TRUE),
      Unprocessed_kwh_Batts = sum(Unprocessed_kwh_Batts, na.rm = TRUE),
      Exported_BM_kwh_Batts = sum(Exported_BM_kwh_Batts, na.rm = TRUE),
      Recycled_Scrap        = sum(Recycled_Scrap, na.rm = TRUE),
      Unprocessed_Scrap     = sum(Unprocessed_Scrap, na.rm = TRUE),
      Exported_BM_Scrap     = sum(Exported_BM_Scrap, na.rm = TRUE),
      Cathode_kwh_state     = sum(Cathode_kwh_state, na.rm = TRUE),
      Scrap_tonnes          = sum(Scrap_tonnes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

  everbatt_both  <- c("Nickel", "Cobalt")
  Copper         <- "Copper"
  Lithium        <- "Lithium"
  Graphite       <- "Graphite"
  Manganese      <- "Manganese"
  Not_recovered  <- c("Phosphorus", "Stainless steel", "Steel", "Aluminum", "Carbon")

  EU_Lithium  <- 0.8
  EU_recovery <- 0.95
  recovery_90 <- 0.9

  future_minerals <- future_recycle_chem %>%
    full_join(all_mins, by = "Cathode Mix", relationship = "many-to-many") %>%
    filter(!Mineral %in% Not_recovered) %>%
    mutate(
      `Available Recycled Minerals (w Scrap) (kg)` =
        kg_per_kwh * Recycled_kwh_Batts + Min_kg_Scrap_tonne * Recycled_Scrap,
      `Available Recycled Minerals No R Restraint (kg)` =
        kg_per_kwh * Cathode_kwh_state + Min_kg_Scrap_tonne * Scrap_tonnes,
      `Minerals in Exported Scrap/Batts (kg)` =
        kg_per_kwh * Unprocessed_kwh_Batts + Min_kg_Scrap_tonne * Unprocessed_Scrap,
      `Minerals in Exported BM (kg)` =
        kg_per_kwh * Exported_BM_kwh_Batts + Min_kg_Scrap_tonne * Exported_BM_Scrap,
      Scrap_min = Min_kg_Scrap_tonne * Recycled_Scrap,
      Batt_min  = kg_per_kwh * Recycled_kwh_Batts
    ) %>%
    group_by(Year, State_Province, Mineral) %>%
    summarise(
      `Available Recycled Minerals (w Scrap) (kg)` = sum(`Available Recycled Minerals (w Scrap) (kg)`, na.rm = TRUE),
      `Available Recycled Minerals No R Restraint (kg)` = sum(`Available Recycled Minerals No R Restraint (kg)`, na.rm = TRUE),
      `Minerals in Exported Scrap/Batts (kg)` = sum(`Minerals in Exported Scrap/Batts (kg)`, na.rm = TRUE),
      `Minerals in Exported BM (kg)` = sum(`Minerals in Exported BM (kg)`, na.rm = TRUE),
      Scrap_min = sum(Scrap_min, na.rm = TRUE),
      Batt_min  = sum(Batt_min, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(Year))

  future_final <- future_minerals %>% ungroup() %>%
    mutate(
      Multiplier = case_when(
        Mineral %in% everbatt_both ~ EU_recovery,
        Mineral %in% Copper    ~ ifelse(Year >= 2035, EU_recovery, recovery_90),
        Mineral %in% Lithium   ~ ifelse(Year >= 2035, EU_Lithium, 0),
        Mineral %in% Manganese ~ ifelse(Year >= 2035, EU_recovery, 0),
        Mineral %in% Graphite  ~ ifelse(Year >= 2035, recovery_90, 0),
        Mineral %in% Not_recovered ~ 0,
        TRUE ~ 1
      ),
      Multiplier_no_limit = case_when(
        Mineral %in% everbatt_both ~ EU_recovery,
        Mineral %in% Copper    ~ EU_recovery,
        Mineral %in% Lithium   ~ EU_Lithium,
        Mineral %in% Manganese ~ EU_recovery,
        Mineral %in% Graphite  ~ recovery_90,
        Mineral %in% Not_recovered ~ 0,
        TRUE ~ 1
      )
    ) %>%
    mutate(
      `Available Recycled Minerals (w Scrap) (Tonne)` =
        `Available Recycled Minerals (w Scrap) (kg)` * Multiplier / 1000,
      `Available Recycled Minerals No R Restraint (Tonne)` =
        `Available Recycled Minerals No R Restraint (kg)` * Multiplier_no_limit / 1000,
      `Minerals Recoverable in Exported Scrap/Batts (Tonne)` =
        `Minerals in Exported Scrap/Batts (kg)` * Multiplier_no_limit / 1000,
      `Minerals Recoverable in Exported BM (Tonne)` =
        `Minerals in Exported BM (kg)` * Multiplier_no_limit / 1000,
      `Minerals Lost to Pyrometalurgy (Tonne)` =
        `Available Recycled Minerals (w Scrap) (kg)` * Multiplier_no_limit / 1000 -
        `Available Recycled Minerals (w Scrap) (Tonne)`,
      Scrap_min = Scrap_min * Multiplier_no_limit / 1000,
      Batt_min  = Batt_min * Multiplier_no_limit / 1000
    ) %>%
    filter(!is.na(Mineral)) %>%
    select(Year, State_Province, Mineral,
           `Available Recycled Minerals (w Scrap) (Tonne)`,
           `Available Recycled Minerals No R Restraint (Tonne)`,
           `Minerals Recoverable in Exported Scrap/Batts (Tonne)`,
           `Minerals Recoverable in Exported BM (Tonne)`,
           `Minerals Lost to Pyrometalurgy (Tonne)`,
           Scrap_min, Batt_min)

  capacity_needs <- Available_Recycling_Capacity %>%
    select(Year, Needed_Black_Mass_level, Needed_Refining_level)

  return(list(
    future_final            = future_final,
    capacity_needs          = capacity_needs,
    state_mass_recycle_batt = state_mass_recycle_batt
  ))
}


## --- 13. Run Recycling Scenarios -------------------------------------

names(batt_scen)  <- c("Increasing Batt Cap", "Decreasing Batt Cap")
names(chem_scens) <- c("Benchmark Chemistry", "High LFP Chemistry")

scenario_combos <- crossing(Batt = names(batt_scen), Chem = names(chem_scens))

safe_capacity_chem_scenarios <- function(batt_name, chem_name) {
  tryCatch({
    res <- capacity_chem_scenarios(
      batt_cap_df = batt_scen[[batt_name]],
      chem_df     = chem_scens[[chem_name]],
      mineral_intensity = mineral_intensity,
      future_recycle_type_collection = future_recycle_type_collection
    )
    list(
      future_final = res$future_final %>%
        mutate(Battery_Scenario = batt_name, Chemistry_Scenario = chem_name),
      capacity_needs = res$capacity_needs %>%
        mutate(Battery_Scenario = batt_name, Chemistry_Scenario = chem_name),
      state_mass_recycle_batt = res$state_mass_recycle_batt %>%
        mutate(Battery_Scenario = batt_name, Chemistry_Scenario = chem_name)
    )
  }, error = function(e) {
    message("Error in scenario: ", batt_name, " / ", chem_name, " -> ", conditionMessage(e))
    NULL
  })
}

all_scenarios <- scenario_combos %>%
  mutate(result = pmap(list(Batt, Chem), safe_capacity_chem_scenarios))

cap_chem_results <- all_scenarios %>% pull(result) %>% compact() %>%
  map("future_final") %>% bind_rows() %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - "))

capacity_needs_all <- all_scenarios %>% pull(result) %>% compact() %>%
  map("capacity_needs") %>% bind_rows()

state_mass_recycle_batt <- all_scenarios %>% pull(result) %>% compact() %>%
  map("state_mass_recycle_batt") %>% bind_rows() %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - ")) %>%
  select(-c(Battery_Scenario, Chemistry_Scenario))

needed_cap_results <- capacity_needs_all %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - ")) %>%
  select(-c(Battery_Scenario, Chemistry_Scenario)) %>%
  rename(`Black Mass` = Needed_Black_Mass_level, Refining = Needed_Refining_level) %>%
  pivot_longer(cols = c(`Black Mass`, Refining),
               names_to = "Recycling Step", values_to = "Tonne") %>%
  mutate(Scenario_Recycling = paste(Scenario, `Recycling Step`, sep = " - "),
         Year = as.numeric(Year))

legend_order <- c(
  "Increasing Batt Cap - Benchmark Chemistry",
  "Increasing Batt Cap - High LFP Chemistry",
  "Decreasing Batt Cap - Benchmark Chemistry",
  "Decreasing Batt Cap - High LFP Chemistry"
)

Not_recovered <- c("Phosphorus", "Stainless steel", "Steel", "Aluminum", "Carbon")


## --- 13b. Mass Assembly with Recycled Battery Mass -------------------

recycle_batts_by_state_2050 <- state_mass_recycle_batt %>%
  filter(Year == 2050) %>%
  pivot_wider(names_from = Scenario, values_from = Batt_Mass_MT) %>%
  rename(
    Recycle_Batt_Proj       = `Increasing Batt Cap - Benchmark Chemistry`,
    Recycle_Batt_15         = `Decreasing Batt Cap - Benchmark Chemistry`,
    Recycle_Batt_Proj_LFP   = `Increasing Batt Cap - High LFP Chemistry`,
    Recycle_Batt_15_LFP     = `Decreasing Batt Cap - High LFP Chemistry`
  )

NA_batts <- state_mass_recycle_batt %>%
  pivot_wider(names_from = Scenario, values_from = Batt_Mass_MT) %>%
  rename(
    Recycle_Batt_Proj       = `Increasing Batt Cap - Benchmark Chemistry`,
    Recycle_Batt_15         = `Decreasing Batt Cap - Benchmark Chemistry`,
    Recycle_Batt_Proj_LFP   = `Increasing Batt Cap - High LFP Chemistry`,
    Recycle_Batt_15_LFP     = `Decreasing Batt Cap - High LFP Chemistry`
  ) %>%
  group_by(Year) %>%
  summarise(
    Recycle_Batt_Proj     = sum(Recycle_Batt_Proj, na.rm = TRUE),
    Recycle_Batt_15       = sum(Recycle_Batt_15, na.rm = TRUE),
    Recycle_Batt_Proj_LFP = sum(Recycle_Batt_Proj_LFP, na.rm = TRUE),
    Recycle_Batt_15_LFP   = sum(Recycle_Batt_15_LFP, na.rm = TRUE),
    .groups = "drop"
  )

Mass_all_years <- full_join(state_cap_chem_tonne,
                            manufacturing_by_state_projected,
                            by = c("Year", "State_Province")) %>%
  full_join(manufacturing_by_state_delayed,
            by = c("Year", "State_Province")) %>%
  full_join(recycling_tonnes_by_state,
            by = c("Year", "State_Province")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

Mass_2050_projected <- full_join(state_demand_tonnes_2050,
                                 manufacturing_tonnes_2050_projected,
                                 by = c("Year", "State_Province")) %>%
  full_join(recycling_tonnes_2050_projected,
            by = c("Year", "State_Province")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  full_join(recycle_batts_by_state_2050,
            by = c("Year", "State_Province")) %>%
  mutate(
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX" ~ "MX",
      TRUE ~ NA_character_
    )
  )

Mass_2050_projected_ref <- Mass_2050_projected %>%
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes,
         Tonnes_Prod_proj_down, Tonnes_Prod_15_down, Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid,
         Recycle_Batt_Proj, Recycle_Batt_15,
         Cumulative_black_mass_cap, Cumulative_refining_cap) %>%
  mutate(
    across(
      c(Add_LIB_proj_tonnes, Add_LIB_15_tonnes,
        Tonnes_Prod_proj_down, Tonnes_Prod_15_down,
        Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid,
        Recycle_Batt_Proj, Recycle_Batt_15,
        Cumulative_black_mass_cap, Cumulative_refining_cap),
      ~ .x / 1e6
    )
  ) %>%
  rename(`LIB Demand` = Add_LIB_proj_tonnes,
         `15% Reduced Batt Cap LIB Demand` = Add_LIB_15_tonnes,
         `Pack Manufacturing` = Tonnes_Prod_proj_down,
         `15% Reduced Batt Cap Pack Manufacturing` = Tonnes_Prod_15_down,
         `Cell Manufacturing` = Tonnes_Prod_proj_mid,
         `15% Reduced Batt Cap Cell Manufacturing` = Tonnes_Prod_15_mid,
         `End of Life Batteries` = Recycle_Batt_Proj,
         `15% Reduced Batt Cap End of Life Batteries` = Recycle_Batt_15,
         `Black Mass` = Cumulative_black_mass_cap,
         `Refining` = Cumulative_refining_cap) %>%
  select(-c(`15% Reduced Batt Cap LIB Demand`, `15% Reduced Batt Cap Pack Manufacturing`,
            `15% Reduced Batt Cap Cell Manufacturing`, `15% Reduced Batt Cap End of Life Batteries`)) %>%
  pivot_longer(cols = c(`LIB Demand`, `Pack Manufacturing`, `Cell Manufacturing`,
                        `End of Life Batteries`, `Black Mass`, `Refining`),
               names_to = "Origin",
               values_to = "Metric Tonnes (millions)") %>%
  mutate(
    Origin = factor(Origin,
                    levels = c("LIB Demand", "Pack Manufacturing", "Cell Manufacturing",
                               "End of Life Batteries", "Black Mass", "Refining"))
  )

Mass_2050_delayed <- full_join(state_demand_tonnes_2050,
                               manufacturing_tonnes_2050_delayed,
                               by = c("Year", "State_Province")) %>%
  full_join(recycling_tonnes_2050_delayed,
            by = c("Year", "State_Province")) %>%
  full_join(recycle_batts_by_state_2050,
            by = c("Year", "State_Province")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  rename(Cumulative_black_mass_cap = Delay_Cumulative_black_mass_cap,
         Cumulative_refining_cap = Delay_Cumulative_refining_cap) %>%
  mutate(
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX" ~ "MX",
      TRUE ~ NA_character_
    )
  )

Mass_2050_delayed_ref <- Mass_2050_delayed %>%
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes,
         Tonnes_Prod_proj_down, Tonnes_Prod_15_down, Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid,
         Recycle_Batt_Proj, Recycle_Batt_15,
         Cumulative_black_mass_cap, Cumulative_refining_cap) %>%
  mutate(
    across(
      c(Add_LIB_proj_tonnes, Add_LIB_15_tonnes,
        Tonnes_Prod_proj_down, Tonnes_Prod_15_down,
        Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid,
        Recycle_Batt_Proj, Recycle_Batt_15,
        Cumulative_black_mass_cap, Cumulative_refining_cap),
      ~ .x / 1e6
    )
  ) %>%
  rename(`LIB Demand` = Add_LIB_proj_tonnes,
         `15% Reduced Batt Cap LIB Demand` = Add_LIB_15_tonnes,
         `Pack Manufacturing` = Tonnes_Prod_proj_down,
         `15% Reduced Batt Cap Pack Manufacturing` = Tonnes_Prod_15_down,
         `Cell Manufacturing` = Tonnes_Prod_proj_mid,
         `15% Reduced Batt Cap Cell Manufacturing` = Tonnes_Prod_15_mid,
         `End of Life Batteries` = Recycle_Batt_Proj,
         `15% Reduced Batt Cap End of Life Batteries` = Recycle_Batt_15,
         `Black Mass` = Cumulative_black_mass_cap,
         `Refining` = Cumulative_refining_cap) %>%
  select(-c(`15% Reduced Batt Cap LIB Demand`, `15% Reduced Batt Cap Pack Manufacturing`,
            `15% Reduced Batt Cap Cell Manufacturing`, `15% Reduced Batt Cap End of Life Batteries`)) %>%
  pivot_longer(cols = c(`LIB Demand`, `Pack Manufacturing`, `Cell Manufacturing`,
                        `End of Life Batteries`, `Black Mass`, `Refining`),
               names_to = "Origin",
               values_to = "Metric Tonnes (millions)") %>%
  mutate(
    Origin = factor(Origin,
                    levels = c("LIB Demand", "Pack Manufacturing", "Cell Manufacturing",
                               "End of Life Batteries", "Black Mass", "Refining"))
  )


## --- 14. Recycling Results Summary -----------------------------------

## Match main 6: keep all 6 minerals (Cobalt, Copper, Graphite, Lithium,
## Manganese, Nickel) so the facet wrap matches the reference plot.
nat_cap_chem_rec <- cap_chem_results %>%
  group_by(Year, Scenario, Mineral) %>%
  summarise(
    `Current NA Recycling Capacity` = sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
    `All Material is Recycled in NA` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!Mineral %in% Not_recovered, !is.na(Mineral)) %>%
  pivot_longer(cols = c("Current NA Recycling Capacity", "All Material is Recycled in NA"),
               names_to = "Recycling Scenario", values_to = "Tonne") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year <= 2050) %>%
  mutate(
    Scenario = factor(Scenario, levels = legend_order),
    `Recycling Scenario` = fct_recode(`Recycling Scenario`,
      "Recycling Limited to NA 2025 Online or Planned Facilities" = "Current NA Recycling Capacity")
  )

all_nat_cap_chem_rec <- cap_chem_results %>%
  group_by(Year, Scenario, Mineral) %>%
  summarise(
    `Current NA Recycling Capacity` = sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
    `All Material is Recycled in NA` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Current NA Recycling Capacity", "All Material is Recycled in NA"),
               names_to = "Recycling Scenario", values_to = "Tonne") %>%
  mutate(Year = as.numeric(Year),
         Scenario = factor(Scenario, levels = legend_order),
         `Recycling Scenario` = fct_recode(`Recycling Scenario`,
           "Recycling Limited to NA 2025 Online or Planned Facilities" = "Current NA Recycling Capacity"))

## Cumulative non-recovery losses, grouped by Scenario like main 6 so the
## bar chart at 2035 reflects scenario-by-scenario cumulative loss.
non_recovery_lost <- cap_chem_results %>%
  group_by(Year, Scenario, Mineral) %>%
  summarise(`Minerals Lost From Non-Recovery` = sum(`Minerals Lost to Pyrometalurgy (Tonne)`, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Mineral, Year, Scenario) %>%
  group_by(Mineral, Scenario) %>%
  mutate(Cum_Tonne = cumsum(`Minerals Lost From Non-Recovery`)) %>%
  ungroup() %>%
  filter(Cum_Tonne > 0) %>%
  mutate(
    Year     = as.numeric(Year),
    Scenario = factor(Scenario, levels = legend_order),
    Mineral  = factor(Mineral,
                      levels = c("Manganese", "Copper", "Lithium", "Graphite"))
  )

export_lost <- cap_chem_results %>%
  group_by(Year, Scenario, Mineral) %>%
  summarise(
    `Minerals Recoverable in Exported Scrap/Batts (Tonne)` =
      sum(`Minerals Recoverable in Exported Scrap/Batts (Tonne)`, na.rm = TRUE),
    `Minerals Recoverable in Exported BM (Tonne)` =
      sum(`Minerals Recoverable in Exported BM (Tonne)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Total_Minerals_Exported = `Minerals Recoverable in Exported BM (Tonne)` +
           `Minerals Recoverable in Exported Scrap/Batts (Tonne)`) %>%
  select(Year, Mineral, Total_Minerals_Exported, Scenario) %>%
  filter(Total_Minerals_Exported >= 0, Year >= 2035) %>%
  mutate(Year = as.numeric(Year), Scenario = factor(Scenario, levels = legend_order))


## =====================================================================
## PART C:  FUTURE DEMAND MINERALS
## (from: Future Demand Minerals.R)
## =====================================================================

## --- 15. Future Demand Function & Run --------------------------------

future_demand_type <- state_capacity_added %>%
  mutate(Year = as.integer(Year),
         State_Province = case_when(
           State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
           TRUE ~ State_Province
         )) %>%
  filter(Year > 2025) %>%
  rename(Sale_Year = Year)

demand_capacity_chem_scenarios <- function(batt_cap_df, chem_df,
                                           mineral_intensity, future_demand_type) {
  batt_df_collapsed <- batt_cap_df %>%
    group_by(State_Province, Segment, Propulsion, Sale_Year) %>%
    summarise(`Projected Avg Batt Cap (kwh/batt)` = first(`Projected Avg Batt Cap (kwh/batt)`),
              .groups = "drop")

  future_demand_cap <- future_demand_type %>%
    left_join(batt_df_collapsed, by = c("State_Province", "Sale_Year", "Segment", "Propulsion"))
  future_demand_cap$LIB_demand_kwh <- future_demand_cap$Total_Add_LIB *
    future_demand_cap$`Projected Avg Batt Cap (kwh/batt)`
  future_demand_cap <- future_demand_cap %>%
    group_by(Sale_Year, State_Province, Segment, Propulsion) %>%
    summarise(LIB_demand_kwh = sum(LIB_demand_kwh, na.rm = TRUE), .groups = "drop") %>%
    arrange(State_Province, Sale_Year)

  future_demand_chem <- future_demand_cap %>%
    left_join(chem_df, by = c("Sale_Year", "Segment", "Propulsion"),
              relationship = "many-to-many") %>%
    mutate(Cathode_kwh_state = LIB_demand_kwh * `Cathode Mix Share`) %>%
    select(-`Cathode Mix Share`)

  future_demand_minerals <- left_join(
    future_demand_chem, mineral_intensity,
    by = "Cathode Mix", relationship = "many-to-many"
  ) %>%
    filter(!Mineral %in% Not_recovered) %>%
    mutate(`Demanded Minerals (kg)` = kg_per_kwh * Cathode_kwh_state) %>%
    select(Sale_Year, State_Province, Mineral, `Demanded Minerals (kg)`)

  future_demand_minerals %>%
    group_by(Sale_Year, State_Province, Mineral) %>%
    summarise(`Demanded Minerals (kg)` = sum(`Demanded Minerals (kg)`, na.rm = TRUE),
              .groups = "drop") %>%
    filter(!is.na(Mineral)) %>%
    mutate(`Demand Minerals (Tonne)` = `Demanded Minerals (kg)` / 1000) %>%
    rename(Year = Sale_Year)
}

safe_demand_scenarios <- function(batt_name, chem_name) {
  tryCatch({
    df <- demand_capacity_chem_scenarios(
      batt_cap_df = batt_scen[[batt_name]],
      chem_df     = chem_scens[[chem_name]],
      mineral_intensity  = mineral_intensity,
      future_demand_type = future_demand_type
    )
    df %>% mutate(Battery_Scenario = batt_name, Chemistry_Scenario = chem_name)
  }, error = function(e) {
    message("Error in demand scenario: ", batt_name, " / ", chem_name, " -> ", e$message)
    NULL
  })
}

all_demand_scenarios <- scenario_combos %>%
  mutate(result = pmap(list(Batt, Chem), safe_demand_scenarios))

cap_chem_demand_results <- bind_rows(all_demand_scenarios$result) %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - ")) %>%
  select(-Battery_Scenario, -Chemistry_Scenario)

nat_demand_cap_chem <- cap_chem_demand_results %>%
  group_by(Year, Scenario, Mineral) %>%
  summarise(`Demand Minerals (Tonne)` = sum(`Demand Minerals (Tonne)`, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Year = as.numeric(Year))

recycle_shifted <- all_nat_cap_chem_rec %>%
  arrange(Mineral, Scenario, Year) %>%
  group_by(Mineral, Scenario) %>%
  mutate(Year = as.numeric(Year) + 1) %>%
  ungroup()

ratio_results <- recycle_shifted %>%
  inner_join(nat_demand_cap_chem, by = c("Year", "Mineral", "Scenario")) %>%
  mutate(Recycle_v_Demand = Tonne / `Demand Minerals (Tonne)`) %>%
  select(-c(Tonne, `Demand Minerals (Tonne)`)) %>%
  mutate(Scenario = factor(Scenario, levels = legend_order))


## =====================================================================
## PART D:  VISUALIZATION
## =====================================================================

## --- ggpattern is optional (used by plot 09) -------------------------
if (!exists("has_ggpattern")) {
  has_ggpattern <- requireNamespace("ggpattern", quietly = TRUE)
  if (has_ggpattern) suppressPackageStartupMessages(library(ggpattern))
}

## --- Plot output folder (shared with 03) -----------------------------
PLOT_DIR <- file.path(OUTPUT_DIR, "Recycling_Plots", FLEET_SCENARIO)
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)
save_plot <- function(p, name, w = 14, h = 9, dpi = 300) {
  path <- file.path(PLOT_DIR, paste0(name, "_", FLEET_SCENARIO, ".png"))
  ggsave(path, plot = p, width = w, height = h, dpi = dpi, bg = "white")
  message("  saved: ", path)
}

scenario_base_colors <- c(
  "Increasing Batt Cap - Benchmark Chemistry" = "#d7301f",
  "Increasing Batt Cap - High LFP Chemistry"  = "#fdae85",
  "Decreasing Batt Cap - Benchmark Chemistry" = "#2171b5",
  "Decreasing Batt Cap - High LFP Chemistry"  = "#1b9e77"
)

## --- Plot 1: Recycled Minerals Until 2050 ----------------------------
## Matches main 6 / Future Recycling Minerals_new.R: linetype-encoded
## recycling scenario (solid = Limited, dashed = All Material), 4 chemistry
## colours, faceted by Mineral with free y-axes.

## Harmonise the recycling-scenario label with main 6 once.
rec_scen_levels_main6 <- c(
  "Recycling Limited to NA 2025 Online or Planned",
  "All Material is Recycled in NA"
)
nat_cap_chem_rec <- nat_cap_chem_rec %>%
  mutate(
    `Recycling Scenario` = forcats::fct_recode(
      as.character(`Recycling Scenario`),
      "Recycling Limited to NA 2025 Online or Planned" =
        "Recycling Limited to NA 2025 Online or Planned Facilities"
    ),
    `Recycling Scenario` = factor(`Recycling Scenario`,
                                  levels = rec_scen_levels_main6)
  )

p_minerals_recycled <- ggplot(
    nat_cap_chem_rec,
    aes(x = Year, y = Tonne / 1000,
        color    = Scenario,
        linetype = `Recycling Scenario`,
        group    = interaction(Scenario, `Recycling Scenario`))
  ) +
  scale_y_sqrt(breaks = scales::pretty_breaks(n = 6)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title    = "North America Yearly Recoverable Minerals Until 2050",
    x        = "Year",
    y        = "Recycled Minerals (thousands Metric Tonnes)",
    color    = "Battery Capacity - Chemistry Scenario",
    linetype = "Recycling Scenario"
  ) +
  scale_linetype_manual(values = c(
    "Recycling Limited to NA 2025 Online or Planned" = "solid",
    "All Material is Recycled in NA"                 = "dashed"
  ), drop = FALSE) +
  scale_color_manual(values = scenario_base_colors) +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title       = element_text(size = 20, face = "bold"),
    axis.text.y      = element_text(size = 14),
    axis.text.x      = element_text(angle = 30, hjust = 1, size = 16),
    strip.text       = element_text(size = 20, face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 20, face = "bold"),
    legend.text      = element_text(size = 15),
    legend.box       = "vertical",
    legend.box.just  = "center"
  ) +
  guides(
    color    = guide_legend(title.position = "top", title.hjust = 0.5,
                            nrow = 2, byrow = TRUE, order = 1),
    linetype = guide_legend(title.position = "top", title.hjust = 0.5,
                            nrow = 2, byrow = TRUE, order = 2,
                            override.aes = list(color = "black"))
  )
print(p_minerals_recycled)
save_plot(p_minerals_recycled, "04_Minerals_Recycled_2050", w = 16, h = 10)

## --- Plot 2: Cumulative Minerals Lost to Non-Recovery (2035) ---------
## Main 6 shows a single-year (2035) bar chart per mineral with fill by
## Mineral, summed across scenarios. We aggregate non_recovery_lost (which
## carries cumulative loss per Mineral x Scenario) at Year = 2035 so the
## bars match the main 6 output directly.

non_recovery_lost_2035 <- non_recovery_lost %>%
  filter(Year == 2035) %>%
  group_by(Mineral) %>%
  summarise(Cum_Tonne = sum(Cum_Tonne, na.rm = TRUE), .groups = "drop") %>%
  filter(Cum_Tonne > 0)

p_minerals_lost <- ggplot(
    non_recovery_lost_2035,
    aes(x = Mineral, y = Cum_Tonne / 1000, fill = Mineral)
  ) +
  geom_col() +
  labs(
    title = "Cumulative North America Minerals Lost to Lack of Recovery Standards (2035)",
    x = "Mineral",
    y = "Lost Minerals (thousands Metric Tonnes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title  = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title  = element_text(size = 20, face = "bold"),
    axis.text   = element_text(size = 20),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
print(p_minerals_lost)
save_plot(p_minerals_lost, "05_Cumulative_Minerals_Lost", w = 16, h = 10)

## --- Plot 3: Needed Recycling Capacity  ------------------------------

legend_order_recycle <- c(
  "Increasing Batt Cap - Benchmark Chemistry - Black Mass",
  "Increasing Batt Cap - Benchmark Chemistry - Refining",
  "Increasing Batt Cap - High LFP Chemistry - Black Mass",
  "Increasing Batt Cap - High LFP Chemistry - Refining",
  "Decreasing Batt Cap - Benchmark Chemistry - Black Mass",
  "Decreasing Batt Cap - Benchmark Chemistry - Refining",
  "Decreasing Batt Cap - High LFP Chemistry - Black Mass",
  "Decreasing Batt Cap - High LFP Chemistry - Refining"
)

needed_cap_long <- needed_cap_results %>%
  mutate(
    Scenario_Recycling = paste(Scenario, `Recycling Step`, sep = " - "),
    Scenario_Recycling = factor(trimws(Scenario_Recycling), levels = legend_order_recycle),
    Year = as.numeric(Year),
    Tonne = Tonne / 1e6
  )

## Main 6 styling: bigger fonts, no points, expanded axes, structured legend.
p_needed_capacity <- ggplot(
    needed_cap_long,
    aes(
      x = Year, y = Tonne,
      color    = Scenario,
      linetype = `Recycling Step`,
      group    = interaction(Scenario, `Recycling Step`)
    )
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = scenario_base_colors) +
  scale_linetype_manual(values = c("Black Mass" = "solid", "Refining" = "dashed")) +
  labs(
    title    = "Yearly Deficit in Black Mass and Refining Capacity Until 2050",
    x        = "Year",
    y        = "Needed Recycling (Millions of MT)",
    color    = "Scenario",
    linetype = "Recycling Step"
  ) +
  theme_minimal(base_size = 20) +
  guides(
    color    = guide_legend(nrow = 2, byrow = TRUE, order = 1, title = "Scenario"),
    linetype = guide_legend(
      nrow = 1, order = 2, title = "Recycling Step",
      override.aes = list(color = "black", linewidth = 2, size = 3)
    )
  ) +
  theme(
    plot.title         = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title         = element_text(size = 20, face = "bold"),
    axis.text          = element_text(size = 20),
    axis.text.x        = element_text(angle = 30, hjust = 1),
    strip.text         = element_text(size = 20, face = "bold"),
    legend.text        = element_text(size = 16),
    legend.title       = element_text(size = 20, face = "bold"),
    legend.position    = "bottom",
    legend.box         = "vertical",
    legend.box.just    = "center",
    legend.key.width   = grid::unit(2.5, "cm"),
    legend.key.height  = grid::unit(0.8, "cm")
  )
print(p_needed_capacity)
save_plot(p_needed_capacity, "06_Needed_Recycling_Capacity", w = 16, h = 10)

## --- Plot 4: Exported Minerals  --------------------------------------

## Main 6 styling: sqrt y-axis, points along the lines, mineral facets.
p_exported_minerals <- ggplot(
    export_lost,
    aes(x = Year, y = Total_Minerals_Exported / 1000,
        color = Scenario, group = Scenario)
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = scenario_base_colors) +
  scale_y_sqrt(breaks = scales::pretty_breaks(n = 8)) +
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title = "Exported Mass of Battery Minerals Each Year Under Current NA Recycling Plans",
    x     = "Year",
    y     = "Exported Minerals (thousands of Metric Tonnes)",
    color = "Battery Capacity - Chemistry Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title       = element_text(size = 14, face = "bold"),
    axis.text        = element_text(size = 12),
    axis.text.x      = element_text(angle = 30, hjust = 1),
    strip.text       = element_text(size = 14, face = "bold"),
    legend.box       = "horizontal",
    legend.position  = "bottom",
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 11)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
print(p_exported_minerals)
save_plot(p_exported_minerals, "07_Exported_Minerals", w = 16, h = 10)

## --- Plot 5: Recycled Content Ratio ----------------------------------
## Matches main 6 / Future Demand Minerals.R: linear y-axis with pretty
## breaks (0–NA), linetype-encoded recycling scenario.

ratio_results <- ratio_results %>%
  mutate(
    `Recycling Scenario` = forcats::fct_recode(
      as.character(`Recycling Scenario`),
      "Recycling Limited to NA 2025 Online or Planned" =
        "Recycling Limited to NA 2025 Online or Planned Facilities"
    ),
    `Recycling Scenario` = factor(`Recycling Scenario`,
                                  levels = rec_scen_levels_main6)
  )

p_recycled_content <- ggplot(
    ratio_results,
    aes(x = as.numeric(Year), y = Recycle_v_Demand * 100,
        color    = Scenario,
        linetype = `Recycling Scenario`,
        group    = interaction(Scenario, `Recycling Scenario`))
  ) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = scenario_base_colors) +
  scale_linetype_manual(values = c(
    "Recycling Limited to NA 2025 Online or Planned" = "solid",
    "All Material is Recycled in NA"                 = "dashed"
  )) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 8),
    expand = expansion(mult = c(0, 0.05))
  ) +
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title    = "Maximum Recycled Content Standard in North America",
    x        = "Year",
    y        = "% Recycled Content",
    color    = "Scenario",
    linetype = "Recycling Scenario"
  ) +
  theme_minimal(base_size = 20) +
  guides(
    color    = guide_legend(nrow = 2, byrow = TRUE, order = 1, title = "Scenario"),
    linetype = guide_legend(
      nrow = 1, order = 2, title = "Recycling Scenario",
      override.aes = list(color = "black", linewidth = 2, size = 3)
    )
  ) +
  theme(
    plot.title         = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title         = element_text(size = 20, face = "bold"),
    axis.text          = element_text(size = 20),
    axis.text.x        = element_text(angle = 30, hjust = 1),
    strip.text         = element_text(size = 20, face = "bold"),
    legend.text        = element_text(size = 16),
    legend.title       = element_text(size = 20, face = "bold"),
    legend.position    = "bottom",
    legend.box         = "vertical",
    legend.box.just    = "center",
    legend.key.width   = grid::unit(2.5, "cm"),
    legend.key.height  = grid::unit(0.8, "cm")
  )
print(p_recycled_content)
save_plot(p_recycled_content, "08_Recycled_Content_Ratio", w = 16, h = 10)


## --- Plot 6: Mineral Demand vs Availability by Country (2050) -------
## Matches main 6 / Future Demand Minerals.R "overall_circularity" plot.
## Stacked bar by Country (CA = stripe, US = circle/dots, MX = crosshatch),
## faceted by Mineral, fixed scenario = Increasing Batt Cap - Benchmark Chem.

cap_chem_demand_results <- cap_chem_demand_results %>%
  mutate(
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX"       ~ "MX",
      TRUE                         ~ NA_character_
    )
  )

country_demand_cap_chem <- cap_chem_demand_results %>%
  group_by(Country, Year, Scenario, Mineral) %>%
  summarise(`Minerals Demand` = sum(`Demand Minerals (Tonne)`, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = `Minerals Demand`,
               names_to = "Recycling Scenario", values_to = "Tonnes") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year == 2050,
         Scenario == "Increasing Batt Cap - Benchmark Chemistry",
         !is.na(Country))

country_cap_chem_rec <- cap_chem_results %>%
  mutate(
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX"       ~ "MX",
      TRUE                         ~ NA_character_
    )
  ) %>%
  group_by(Country, Year, Scenario, Mineral) %>%
  summarise(
    `Current NA Recycling Capacity` = sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
    `All Material is Recycled in NA` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Current NA Recycling Capacity", "All Material is Recycled in NA"),
               names_to = "Recycling Scenario", values_to = "Tonne") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year == 2050,
         Scenario == "Increasing Batt Cap - Benchmark Chemistry",
         !is.na(Country))

overall_circularity <- country_demand_cap_chem %>%
  rename(Demand_Tonne = Tonnes) %>%
  full_join(country_cap_chem_rec %>% rename(Recycling_Tonne = Tonne),
            by = c("Country", "Year", "Mineral", "Scenario", "Recycling Scenario")) %>%
  mutate(
    Tonnes = coalesce(Demand_Tonne, Recycling_Tonne),
    Type   = case_when(
      !is.na(Demand_Tonne) & is.na(Recycling_Tonne) ~ "Demand",
      is.na(Demand_Tonne) & !is.na(Recycling_Tonne) ~ "Recycling",
      TRUE                                          ~ "Both"
    )
  ) %>%
  select(-Demand_Tonne, -Recycling_Tonne) %>%
  mutate(
    Country = factor(Country, levels = c("CA", "US", "MX")),
    Mineral = factor(Mineral,
                     levels = c("Cobalt", "Lithium", "Manganese",
                                "Nickel", "Copper", "Graphite")),
    Tonnes  = Tonnes / 1e6
  ) %>%
  filter(!is.na(Mineral))

if (has_ggpattern) {
  p_demand_vs_avail <- ggplot(
      overall_circularity,
      aes(x = `Recycling Scenario`, y = Tonnes,
          pattern = Country, fill = Country)
    ) +
    ggpattern::geom_col_pattern(
      position        = "stack",
      color           = "black",
      pattern_density = 0.2,
      pattern_spacing = 0.05,
      pattern_alpha   = 0.3,
      pattern_size    = 0.2,
      pattern_fill    = "black"
    ) +
    ggpattern::scale_pattern_manual(values = c(
      "US" = "circle", "CA" = "stripe", "MX" = "crosshatch"
    ))
} else {
  p_demand_vs_avail <- ggplot(
      overall_circularity,
      aes(x = `Recycling Scenario`, y = Tonnes, fill = Country)
    ) +
    geom_col(position = "stack", color = "black")
}

p_demand_vs_avail <- p_demand_vs_avail +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  facet_wrap(~ Mineral, scales = "free_y") +
  theme_minimal(base_size = 20) +
  labs(
    title   = "Mineral Demand vs Mineral Availability (2050) ",
    x       = "Recycling Scenario",
    y       = "Tonnes (millions)",
    fill    = "Country",
    pattern = "Country"
  ) +
  theme(
    legend.box       = "vertical",
    legend.position  = "bottom",
    plot.title       = element_text(size = 24, hjust = 0.5, face = "bold"),
    axis.title       = element_text(size = 20),
    axis.text        = element_text(size = 14),
    strip.text       = element_text(size = 20, face = "bold"),
    legend.title     = element_text(size = 20),
    legend.text      = element_text(size = 20),
    legend.key.width = grid::unit(0.8, "cm")
  )
print(p_demand_vs_avail)
save_plot(p_demand_vs_avail, "09_Demand_vs_Availability_2050", w = 16, h = 10)


## --- Summary Metrics -------------------------------------------------

Continent_LIB_Recycle <- future_recycle_type %>%
  group_by(Year) %>%
  summarise(all_recycle = sum(LIB_recycle_total, na.rm = TRUE), .groups = "drop")
Continent_Demand <- state_capacity_added %>%
  group_by(Year) %>%
  summarise(all_demand = sum(Total_Add_LIB, na.rm = TRUE), .groups = "drop")
ratio_in_batts <- Continent_LIB_Recycle %>%
  inner_join(Continent_Demand, by = "Year") %>%
  mutate(percent = all_recycle / all_demand)

cat("=== 02-Recycling_Analysis.R complete ===\n")
cat("Fleet scenario analyzed:", FLEET_SCENARIO, "\n")
