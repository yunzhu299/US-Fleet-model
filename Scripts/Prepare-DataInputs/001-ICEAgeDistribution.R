## ICE Age Distrubution ------
## YZC Aug 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
# IHS MOVES Data
url_file <- "~/Library/CloudStorage/GoogleDrive-yuzchen@ucdavis.edu/My Drive/US Fleet modeling/%s"
StateID <- read_excel(sprintf(url_file,"2023_Representative_Counties_Analysis_20250430.xlsx"), 
                      sheet = "RepCounties")
AgeTypeDistribution <- read_excel("~/Library/CloudStorage/GoogleDrive-yuzchen@ucdavis.edu/My Drive/US Fleet modeling/2023_Representative_Counties_Analysis_20250430.xlsx", 
                                                              sheet = "agetype")


# Use county level LDV age distribution to represent state ICE age distribution

# --- 0) County → State map (recode D.C. to full name) ---
state_map <- StateID %>%
  rename(countyID = countyid,
         stateID  = stateid,
         state    = `State Name`) %>%
  mutate(state = dplyr::recode(state, "D.C." = "District of Columbia"))

# --- 1) Keep only Car / Truck rows and join to states ---
# sourceTypeID: 21 = passenger car; 31 = passenger truck; 32 = commercial truck
age_state_typed <- AgeTypeDistribution %>%
  mutate(Type = case_when(
    sourceTypeID == 21 ~ "Car",
    sourceTypeID %in% c(31, 32) ~ "Truck",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Type)) %>%
  inner_join(state_map, by = "countyID")

# --- 2) Aggregate to State × Type × Age (counts and fractions) ---
# age_pop = vehicles of given ageID in that state & type
state_age_long_by_type <- age_state_typed %>%
  group_by(stateID, state, Type, yearID, ageID) %>%
  summarise(age_pop = sum(population, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(stateID, state, Type, yearID) %>%
  mutate(state_type_total = sum(age_pop, na.rm = TRUE),
         ageFraction_state_type = if_else(state_type_total > 0, age_pop / state_type_total, NA_real_)) %>%
  ungroup() %>%
  arrange(stateID, Type, yearID, ageID)
# Result: one row per state × type × age with fraction

# --- 3) State-level Car/Truck shares (within LDV = Car + Truck) ---
state_type_share <- state_age_long_by_type %>%
  distinct(stateID, state, Type, yearID, state_type_total) %>%
  group_by(stateID, state, yearID) %>%
  mutate(state_total_ldv = sum(state_type_total, na.rm = TRUE),
         type_share = if_else(state_total_ldv > 0, state_type_total / state_total_ldv, NA_real_)) %>%
  select(stateID, state, yearID, Type, state_type_total, type_share) %>%
  pivot_wider(names_from = Type,
              values_from = c(state_type_total, type_share),
              names_sep = "_") %>%
  ungroup()
# Columns include: state_type_total_Car, state_type_total_Truck, type_share_Car, type_share_Truck

# --- 4) U.S. weighted-average age fractions by Type (to fill AK/HI if missing) ---
us_avg_by_year_age_by_type <- state_age_long_by_type %>%
  group_by(Type, yearID, ageID) %>%
  summarise(us_age_pop = sum(age_pop, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(Type, yearID) %>%
  mutate(us_total = sum(us_age_pop, na.rm = TRUE),
         ageFraction_state_type = if_else(us_total > 0, us_age_pop / us_total, NA_real_)) %>%
  ungroup() %>%
  select(Type, yearID, ageID, ageFraction_state_type)

# Determine if Alaska/Hawaii need to be filled (global presence check)
states_present <- unique(state_age_long_by_type$state)
need_ak <- !"Alaska" %in% states_present
need_hi <- !"Hawaii" %in% states_present

add_rows <- bind_rows(
  if (need_ak) us_avg_by_year_age_by_type %>%
    mutate(stateID = 2L, state = "Alaska") else NULL,
  if (need_hi) us_avg_by_year_age_by_type %>%
    mutate(stateID = 15L, state = "Hawaii") else NULL
) %>%
  mutate(state_type_total = NA_real_, age_pop = NA_real_) %>%
  relocate(stateID, state, Type, yearID, ageID, age_pop, state_type_total, ageFraction_state_type)

state_age_long_filled_by_type <- bind_rows(
  state_age_long_by_type %>%
    select(stateID, state, Type, yearID, ageID, age_pop, state_type_total, ageFraction_state_type),
  add_rows
) %>%
  arrange(stateID, Type, yearID, ageID)

# --- 5) Wide table (one row per state × type × year; columns age_0..age_30) ---
state_age_wide_by_type <- state_age_long_filled_by_type %>%
  select(stateID, state, Type, yearID, ageID, ageFraction_state_type) %>%
  mutate(age_col = paste0("age_", ageID)) %>%
  select(-ageID) %>%
  pivot_wider(names_from = age_col, values_from = ageFraction_state_type)

# --- 6) Sanity check: fractions within each state × type × year should sum ≈ 1 ---
check_sum_by_type <- state_age_long_filled_by_type %>%
  group_by(stateID, state, Type, yearID) %>%
  summarise(sum_frac = sum(ageFraction_state_type, na.rm = TRUE), .groups = "drop")