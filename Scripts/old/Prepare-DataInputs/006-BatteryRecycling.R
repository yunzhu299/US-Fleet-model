###############################################################################
# 06–07 — Historical + Future EV Battery Recycling & Demand Scenarios
# Goal:
#  1) Compute historical recycled minerals from retired EV batteries
#  2) Build future recycling scenarios (Battery cap × Chem mix)
#  3) Build future mineral demand scenarios (Battery cap × Chem mix)
#  4) Combine historical + future recycling for each scenario
#  5) Compute Recycle / Demand ratio, and plot California ratios by mineral
###############################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(readxl)
library(ggplot2)

###############################################################################
# 1. Load input data
###############################################################################

# ACCII EVLIB flows (contains LIB_recycling_vector by state/segment/etc.)
EVLIB_Flows_hist <- read_csv(
  "Outputs/EVLIB_Flows_detail_ACCII.csv",
  show_col_types = FALSE
)

# Mineral intensity (kg per kWh per cathode chemistry)
mineral_intensity <- read_excel(
  "Inputs/Mineral_Intensity(2).xlsx",
  na = ""
)

mineral_intensity <- mineral_intensity %>%
  filter(!Mineral %in% c("Phosphorus", "Stainless steel"))

# Cathode mix projections (global, not state-specific)
cathode_projections <- read_excel(
  "Inputs/Cathode Projections (1).xlsx",
  sheet = "Sheet1"
)

# IMPORTANT:
# Assumes `usa_sales_filtered` already exists in the environment with columns:
#  - `Sale Year`, `Global Segment`, Propulsion, `Cathode Mix`,
#    `Total Sales`, `Total Mwh`
#
# Assumes `future_demand_type` already exists with columns:
#  - Year, Sale_Year, State, Segment, Propulsion, EV_stock_total

start_year <- 2020

###############################################################################
# 2. Helper: convert recycling vector string → named numeric vector
#    Names are original sale years: Year, Year-1, Year-2, ...
###############################################################################

name_vector_with_years <- function(vec_string, start_year) {
  vec_string <- as.character(vec_string)
  vec <- as.numeric(strsplit(vec_string, "\\|")[[1]])
  names(vec) <- start_year - (seq_along(vec) - 1)
  vec
}

###############################################################################
# 3. Historical recycling by sale year
###############################################################################

EVLIB_Flows_hist$LIB_recycling_vector <- Map(
  name_vector_with_years,
  EVLIB_Flows_hist$LIB_recycling_vector,
  EVLIB_Flows_hist$Year
)

hist_recycle_type <- EVLIB_Flows_hist %>%
  mutate(
    recycle_df = map(LIB_recycling_vector, ~ {
      tibble(
        Sale_Year         = as.integer(names(.x)),
        LIB_recycle_total = as.numeric(.x)
      )
    })
  ) %>%
  select(State, Segment, Propulsion, Year, recycle_df) %>%
  unnest(recycle_df)
# Year      = recycling year (when the battery is scrapped)
# Sale_Year = original EV sale year

###############################################################################
# 4. Historical average battery capacity & cathode mix shares
###############################################################################

# 4.1 Cathode mix shares by sale year / segment / propulsion
chem_Mwh <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion, `Cathode Mix`) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop") %>%
  filter(`Total Mwh` != 0) %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  mutate(
    `Share of Avg Chem` =
      `Total Mwh` / sum(`Total Mwh`, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rename(
    Segment  = `Global Segment`,
    Sale_Year = `Sale Year`
  )

# 4.2 Historical average battery capacity (kWh per battery)
batt_cap_merged <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  summarise(
    `Total Sales` = sum(`Total Sales`, na.rm = TRUE),
    `Total Mwh`   = sum(`Total Mwh`, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  mutate(
    `Avg Batt Cap (kwh/batt)` =
      (`Total Mwh` / `Total Sales`) * 1000
  ) %>%
  rename(
    Segment   = `Global Segment`,
    Sale_Year = `Sale Year`
  )

###############################################################################
# 5. Historical recycling → kWh → minerals
###############################################################################

# 5.1 Merge historical recycling flows with average battery capacity
hist_recycle_cap <- merge(
  batt_cap_merged,
  hist_recycle_type,
  by    = c("Sale_Year", "Segment", "Propulsion"),
  all.x = TRUE
)

hist_recycle_cap <- hist_recycle_cap %>%
  mutate(
    LIB_recycle_kwh =
      LIB_recycle_total * `Avg Batt Cap (kwh/batt)`
  ) %>%
  select(
    Year, Sale_Year, State, Segment, Propulsion,
    LIB_recycle_kwh
  )

# 5.2 Apply cathode mix shares to split kWh by chemistry
hist_recycle_chem <- merge(
  chem_Mwh,
  hist_recycle_cap,
  by    = c("Sale_Year", "Segment", "Propulsion"),
  all.x = TRUE
)

hist_recycle_chem <- hist_recycle_chem %>%
  mutate(
    Cathode_kwh_state =
      LIB_recycle_kwh * `Share of Avg Chem`
  )

# 5.3 Multiply cathode-kWh by mineral intensity
hist_final <- hist_recycle_chem %>%
  left_join(
    mineral_intensity,
    by = c("Cathode Mix" = "chemistry"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    `Available Recycled Minerals (kg)` =
      kg_per_kwh * Cathode_kwh_state
  ) %>%
  select(
    Year,          # recycling year
    State,
    Mineral,
    `Available Recycled Minerals (kg)`
  ) %>%
  group_by(Year, State, Mineral) %>%
  summarise(
    `Available Recycled Minerals (kg)` =
      sum(`Available Recycled Minerals (kg)`, na.rm = TRUE),
    .groups = "drop"
  )
# hist_final = historical total recycling by Year / State / Mineral

###############################################################################
# 6. Future recycling flows (ACCII) – expand retirement vectors
###############################################################################

EVLIB_Flows <- read_csv(
  "Outputs/EVLIB_Flows_detail_ACCII.csv",
  show_col_types = FALSE
)

EVLIB_Flows$LIB_recycling_vector <- Map(
  name_vector_with_years,
  EVLIB_Flows$LIB_recycling_vector,
  EVLIB_Flows$Year
)

future_recycle_type <- EVLIB_Flows %>%
  mutate(
    recycle_df = map(LIB_recycling_vector, ~ {
      tibble(
        Sale_Year         = as.integer(names(.x)),
        LIB_recycle_total = as.numeric(.x)
      )
    })
  ) %>%
  select(State, Segment, Propulsion, Year, recycle_df) %>%
  unnest(cols = recycle_df)

###############################################################################
# 7. Battery capacity scenarios (Baseline vs. 15% lower by 2040)
###############################################################################

batt_cap_merged <- batt_cap_merged %>%
  mutate(Sale_Year = as.numeric(Sale_Year))

# Anchor year for projections: 2024
batt_cap_2024 <- batt_cap_merged %>%
  filter(Sale_Year == 2024) %>%
  select(
    Sale_Year,
    Segment,
    Propulsion,
    Base_Capacity = `Avg Batt Cap (kwh/batt)`
  )

years_batt_cap <- 2025:2050

# --- Scenario A: Baseline trend ----------------------------------------------

trend_results <- batt_cap_merged %>%
  filter(!is.na(`Avg Batt Cap (kwh/batt)`)) %>%
  group_by(Segment, Propulsion) %>%
  filter(n() >= 3) %>%  # at least 3 data points
  summarise(
    trend  = coef(lm(`Avg Batt Cap (kwh/batt)` ~ Sale_Year))[2],
    .groups = "drop"
  ) %>%
  filter(Propulsion != "FCEV")

projection_base <- batt_cap_2024 %>%
  inner_join(trend_results, by = c("Segment", "Propulsion")) %>%
  tidyr::crossing(years_batt_cap = years_batt_cap)

batt_cap_projection <- projection_base %>%
  mutate(
    `Projected Avg Batt Cap (kwh/batt)` =
      Base_Capacity + (years_batt_cap - 2024) * trend
  ) %>%
  select(-Sale_Year) %>%
  rename(Sale_Year = years_batt_cap) %>%
  select(
    Sale_Year,
    Segment,
    Propulsion,
    `Projected Avg Batt Cap (kwh/batt)`
  )

# --- Scenario B: 15% lower capacity by 2040, then flat -----------------------

batt_cap_2040 <- batt_cap_2024 %>%
  mutate(
    Base_Capacity = Base_Capacity * 0.85,
    Sale_Year     = 2040
  )

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

batt_cap_15 <- second_trend_results %>%
  tidyr::crossing(Sale_Year = years_batt_cap) %>%
  mutate(
    `Projected Avg Batt Cap (kwh/batt)` = dplyr::case_when(
      Sale_Year <= 2040 ~ intercept + slope * Sale_Year,
      TRUE              ~ intercept + slope * 2040
    )
  ) %>%
  filter(Propulsion != "FCEV") %>%
  select(
    Sale_Year,
    Segment,
    Propulsion,
    `Projected Avg Batt Cap (kwh/batt)`
  )

# Pack battery scenarios
batt_scen <- list(
  "Baseline Battery"  = batt_cap_projection,
  "15% Lower Battery" = batt_cap_15
)

###############################################################################
# 8. Cathode chemistry scenarios (Original vs. High LFP)
###############################################################################

# 8.1 Clean cathode projections table
cp <- cathode_projections[12:21, ]
cp <- cp %>%
  select(-`...2`, -`...3`) %>%
  rename(`Cathode Mix` = `...1`) %>%
  slice(-1)   # drop first row in this slice (header-like)

cp_melted <- cp %>%
  pivot_longer(-`Cathode Mix`, names_to = "Sale_Year", values_to = "Total Mwh") %>%
  mutate(`Sale_Year` = as.integer(`Sale_Year`)) %>%
  group_by(`Sale_Year`) %>%
  mutate(`Cathode Mix Share` = `Total Mwh` / sum(`Total Mwh`, na.rm = TRUE)) %>%
  ungroup()

replacement_future <- c(
  "NCM low nickel"  = "NMC 111",
  "NCM mid nickel"  = "NMC 622",
  "NCM high nickel" = "NMC 811"
)

cp_melted$`Cathode Mix` <-
  recode(cp_melted$`Cathode Mix`, !!!replacement_future)

cp_melted <- cp_melted %>%
  filter(`Cathode Mix Share` != 0)

fixed_cp <- cp_melted

max_future <- fixed_cp %>%
  group_by(Sale_Year) %>%
  slice_max(`Cathode Mix Share`, n = 1, with_ties = FALSE) %>%
  ungroup()

# Fill "Other / unspecified" chemistries with the most common mix that year
future_match <- fixed_cp %>%
  left_join(
    max_future,
    by     = "Sale_Year",
    suffix = c("_x", "_y")
  )

mask_mins_future <- future_match$`Cathode Mix_x` %in% c(
  "4V Ni or Mn based", "%V Mn based", "LCO", "Other"
)

future_match$`Cathode Mix_x`[mask_mins_future] <-
  future_match$`Cathode Mix_y`[mask_mins_future]

future_match <- future_match %>%
  select(
    Sale_Year,
    `Cathode Mix`       = `Cathode Mix_x`,
    `Cathode Mix Share` = `Cathode Mix Share_x`,
    `Total Mwh`         = `Total Mwh_x`
  )

# 8.2 High-LFP scenario: ramp LFP share to ~50% by 2040
total_mwh_per_year <- cp_melted %>%
  group_by(Sale_Year) %>%
  summarise(Total_Mwh = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop")

lfp_targets <- tibble(
  Sale_Year        = unique(cp_melted$Sale_Year),
  LFP_share_target = scales::rescale(Sale_Year, to = c(0.27, 0.5))  # ~27%→50%
)

lfp_mwh_per_year <- total_mwh_per_year %>%
  left_join(lfp_targets, by = "Sale_Year") %>%
  mutate(LFP_Mwh = Total_Mwh * LFP_share_target)

chem_with_targets <- future_match %>%
  left_join(lfp_mwh_per_year, by = "Sale_Year")

# LFP rows: force them to LFP_Mwh and target share
lfp_rows <- chem_with_targets %>%
  filter(`Cathode Mix` == "LFP") %>%
  mutate(
    Adjusted_Mwh      = LFP_Mwh,
    New_Cathode_Share = LFP_share_target
  )

# Non-LFP rows: scale down to fill (1 − LFP_share_target)
other_chems <- chem_with_targets %>%
  filter(`Cathode Mix` != "LFP")

adjusted_other_chems <- other_chems %>%
  group_by(Sale_Year) %>%
  mutate(
    total_other_share = sum(`Cathode Mix Share`, na.rm = TRUE),
    remaining_mwh     = unique(Total_Mwh) - unique(LFP_Mwh),
    Adjusted_Mwh      = (`Cathode Mix Share` / total_other_share) * remaining_mwh,
    New_Cathode_Share = Adjusted_Mwh / Total_Mwh
  ) %>%
  ungroup()

final_adjusted_mix <- bind_rows(lfp_rows, adjusted_other_chems) %>%
  mutate(`Cathode Mix Share` = New_Cathode_Share) %>%
  select(Sale_Year, `Cathode Mix`, `Cathode Mix Share`)

# 8.3 Helper: freeze composition at 2040 through 2050
extend_flat_from_2040 <- function(df,
                                  cutoff_year = 2040,
                                  max_year   = 2050) {
  df_2040 <- df %>% filter(Sale_Year == cutoff_year)
  years_post <- tibble(Sale_Year = (cutoff_year + 1):max_year)
  
  df_2040_noyear <- df_2040 %>% select(-Sale_Year)
  
  df_post <- tidyr::crossing(df_2040_noyear, years_post) %>%
    relocate(Sale_Year, .before = everything())
  
  df_pre <- df %>% filter(Sale_Year <= cutoff_year)
  
  bind_rows(df_pre, df_post)
}

# Apply to both chemistry data frames
final_adjusted_mix <- extend_flat_from_2040(final_adjusted_mix) %>%
  group_by(`Cathode Mix`) %>%
  arrange(`Cathode Mix`, Sale_Year, .by_group = TRUE)

future_match <- future_match %>%
  select(-`Total Mwh`)

future_match <- extend_flat_from_2040(future_match) %>%
  group_by(`Cathode Mix`) %>%
  arrange(`Cathode Mix`, Sale_Year, .by_group = TRUE)

chem_scens <- list(
  "Original Chemistry" = future_match,
  "High LFP Chemistry" = final_adjusted_mix
)

###############################################################################
# 9. FUTURE mineral DEMAND: EV stock × battery cap × chem mix
###############################################################################

capacity_chem_demand_scenarios <- function(batt_cap_df,
                                           chem_df,
                                           mineral_intensity,
                                           future_demand_type) {
  
  # Merge projected battery capacity with future EV stock
  future_demand_cap <- merge(
    batt_cap_df,
    future_demand_type,
    by    = c("Sale_Year", "Segment", "Propulsion"),
    all.x = TRUE
  )
  
  # Convert EV stock (batteries) to kWh
  future_demand_cap <- future_demand_cap %>%
    mutate(
      LIB_demand_kwh =
        EV_stock_total * `Projected Avg Batt Cap (kwh/batt)`
    ) %>%
    select(
      Year, Sale_Year, State, Segment, Propulsion,
      LIB_demand_kwh
    )
  
  # Apply chemistry mix (shares by Sale_Year)
  future_demand_chem <- future_demand_cap %>%
    left_join(
      chem_df,
      by = "Sale_Year"
    ) %>%
    mutate(
      Cathode_kwh_state =
        LIB_demand_kwh * `Cathode Mix Share`
    )
  
  # Multiply by mineral intensity to get demanded minerals
  future_demand_minerals <- future_demand_chem %>%
    left_join(
      mineral_intensity,
      by = c("Cathode Mix" = "chemistry"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      `Demanded Minerals (kg)` =
        kg_per_kwh * Cathode_kwh_state
    ) %>%
    select(
      Year, Sale_Year, State, Mineral,
      `Demanded Minerals (kg)`
    ) %>%
    group_by(Year, State, Mineral) %>%
    summarise(
      `Demanded Minerals (kg)` =
        sum(`Demanded Minerals (kg)`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(Mineral))
  
  future_demand_minerals
}

###############################################################################
# 10. FUTURE mineral RECYCLING: retirements × battery cap × chem mix
###############################################################################

capacity_chem_recycle_scenarios <- function(batt_cap_df,
                                            chem_df,
                                            future_recycle_type,
                                            mineral_intensity) {
  
  # Merge future retirement counts with projected battery capacity
  future_recycle_cap <- merge(
    batt_cap_df,
    future_recycle_type,
    by    = c("Sale_Year", "Segment", "Propulsion"),
    all.x = TRUE
  )
  
  future_recycle_cap <- future_recycle_cap %>%
    mutate(
      LIB_recycle_kwh =
        LIB_recycle_total * `Projected Avg Batt Cap (kwh/batt)`
    ) %>%
    select(
      Year, Sale_Year, State, Segment, Propulsion,
      LIB_recycle_kwh
    )
  
  # Apply chemistry mix
  future_recycle_chem <- future_recycle_cap %>%
    left_join(
      chem_df,
      by = "Sale_Year"
    ) %>%
    mutate(
      Cathode_kwh_state =
        LIB_recycle_kwh * `Cathode Mix Share`
    )
  
  # Multiply by mineral intensity
  future_minerals <- future_recycle_chem %>%
    left_join(
      mineral_intensity,
      by = c("Cathode Mix" = "chemistry"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      `Available Recycled Minerals (kg)` =
        kg_per_kwh * Cathode_kwh_state
    ) %>%
    select(
      Year, State, Mineral,
      `Available Recycled Minerals (kg)`
    ) %>%
    group_by(Year, State, Mineral) %>%
    summarise(
      `Available Recycled Minerals (kg)` =
        sum(`Available Recycled Minerals (kg)`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(Mineral))
  
  future_minerals
}

###############################################################################
# 11. Run all scenarios (Battery × Chemistry) for DEMAND & RECYCLING
###############################################################################

names(batt_scen) <- c("Baseline Battery", "15% Lower Battery")
names(chem_scens) <- c("Original Chemistry", "High LFP Chemistry")

scenario_combos <- tidyr::crossing(
  Batt = names(batt_scen),
  Chem = names(chem_scens)
)

# --- Demand scenarios ---------------------------------------------------------

safe_capacity_chem_demand <- function(batt_name, chem_name) {
  tryCatch({
    df <- capacity_chem_demand_scenarios(
      batt_cap_df       = batt_scen[[batt_name]],
      chem_df           = chem_scens[[chem_name]],
      mineral_intensity = mineral_intensity,
      future_demand_type = future_demand_type
    )
    
    if (is.null(df) || nrow(df) == 0) {
      tibble(
        Year  = integer(),
        State = character(),
        Mineral = character(),
        `Demanded Minerals (kg)` = numeric(),
        Battery_Scenario   = character(),
        Chemistry_Scenario = character()
      )
    } else {
      df %>%
        mutate(
          Battery_Scenario   = batt_name,
          Chemistry_Scenario = chem_name
        )
    }
  }, error = function(e) {
    warning("Error in DEMAND scenario: ", batt_name, " / ", chem_name, " -> ", e$message)
    tibble(
      Year  = integer(),
      State = character(),
      Mineral = character(),
      `Demanded Minerals (kg)` = numeric(),
      Battery_Scenario   = character(),
      Chemistry_Scenario = character()
    )
  })
}

all_demand_scenarios <- scenario_combos %>%
  mutate(
    result = pmap(
      list(Batt, Chem),
      safe_capacity_chem_demand
    )
  )

cap_chem_demand_results <- bind_rows(all_demand_scenarios$result) %>%
  mutate(
    Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - ")
  ) %>%
  select(
    Scenario, Year, State, Mineral,
    `Demanded Minerals (kg)`
  )

# --- Recycling scenarios ------------------------------------------------------

safe_capacity_chem_recycle <- function(batt_name, chem_name) {
  tryCatch({
    df <- capacity_chem_recycle_scenarios(
      batt_cap_df         = batt_scen[[batt_name]],
      chem_df             = chem_scens[[chem_name]],
      future_recycle_type = future_recycle_type,
      mineral_intensity   = mineral_intensity
    )
    
    if (is.null(df) || nrow(df) == 0) {
      tibble(
        Year  = integer(),
        State = character(),
        Mineral = character(),
        `Available Recycled Minerals (kg)` = numeric(),
        Battery_Scenario   = character(),
        Chemistry_Scenario = character()
      )
    } else {
      df %>%
        mutate(
          Battery_Scenario   = batt_name,
          Chemistry_Scenario = chem_name
        )
    }
  }, error = function(e) {
    warning("Error in RECYCLING scenario: ", batt_name, " / ", chem_name, " -> ", e$message)
    tibble(
      Year  = integer(),
      State = character(),
      Mineral = character(),
      `Available Recycled Minerals (kg)` = numeric(),
      Battery_Scenario   = character(),
      Chemistry_Scenario = character()
    )
  })
}

all_recycle_scenarios <- scenario_combos %>%
  mutate(
    result = pmap(
      list(Batt, Chem),
      safe_capacity_chem_recycle
    )
  )

future_recycle_results <- bind_rows(all_recycle_scenarios$result) %>%
  mutate(
    Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - ")
  ) %>%
  select(
    Scenario, Year, State, Mineral,
    `Available Recycled Minerals (kg)`
  )

###############################################################################
# 12. Add historical recycling into each scenario (total recycled)
###############################################################################

scenario_names <- unique(future_recycle_results$Scenario)

# Copy historical recycling for each scenario
hist_with_scenarios <- tidyr::crossing(
  Scenario = scenario_names,
  hist_final
)
# hist_final: Year, State, Mineral, Available Recycled Minerals (kg)

# Stack historical + scenario-specific future recycling
final_future_hist <- bind_rows(
  hist_with_scenarios,
  future_recycle_results
)

summary_final_future_hist <- final_future_hist %>%
  group_by(Scenario, Year, State, Mineral) %>%
  summarise(
    `Available Recycled Minerals (kg)` =
      sum(`Available Recycled Minerals (kg)`, na.rm = TRUE),
    .groups = "drop"
  )

###############################################################################
# 13. Compute Recycle / Demand ratio, and plot California-only
###############################################################################

# Merge total recycled (hist + future) with demand
ratio_results <- cap_chem_demand_results %>%
  left_join(
    summary_final_future_hist,
    by = c("Scenario", "Year", "State", "Mineral")
  ) %>%
  mutate(
    Recycle_Demand =
      `Available Recycled Minerals (kg)` / `Demanded Minerals (kg)`
  ) %>%
  filter(!Mineral %in% c("Aluminum", "Steel"))  # optional: remove Al & steel

# Filter California only (change to "CA" if your State codes are abbreviations)
ca_ratio <- ratio_results %>%
  filter(State == "California")

# Plot for California
ggplot(
  ca_ratio,
  aes(
    x       = Year,
    y       = Recycle_Demand,
    color   = Scenario,
    linetype = Scenario
  )
) +
  geom_line() +
  facet_wrap(
    ~ Mineral,
    scales = "free_y",
    ncol   = 2
  ) +
  labs(
    title = "ACCII – Recycled Minerals / Demanded Minerals – California",
    x     = "Year",
    y     = "Ratio (Recycled / Demanded)",
    color = "Scenario",
    linetype = "Scenario"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 9),
    legend.title      = element_text(size = 10),
    legend.key.size   = unit(0.3, "cm"),
    plot.margin       = margin(t = 10, r = 20, b = 20, l = 10),
    legend.box.margin = margin(t = 10)
  ) +
  guides(
    color    = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )
