## ICE Age Distrubution ------
## YZC Aug 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
# IHS MOVES Data
url_file <- "~/Library/CloudStorage/GoogleDrive-yuzchen@ucdavis.edu/My Drive/US Fleet modeling/%s"
StateID <- read_excel(sprintf(url_file,"2023_Representative_Counties_Analysis_20250430.xlsx"), 
                      sheet = "RepCounties")
AgeDistribution <- read_excel(sprintf(url_file, "2023_Representative_Counties_Analysis_20250430.xlsx"), 
                              sheet = "meanLDage", range = "AO03:AT99913")

# Use county level LDV age distribution to represent state ICE age distribution

# 1) Join counties to states by countyID
age_state <- AgeDistribution %>%
  inner_join(
    StateID %>%
      rename(countyID = countyid,
             stateID  = stateid,
             state    = `State Name`) %>%
      mutate(state = dplyr::recode(state,
                                   "D.C." = "District of Columbia")),
    by = "countyID"
  )


# 2) Aggregate vehicle COUNTS to state × age
#    age_pop = vehicles of age 'ageID' in that state
state_age_long <- age_state %>%
  group_by(stateID, state, yearID, ageID) %>%
  summarise(age_pop = sum(population, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(stateID, state, yearID) %>%
  mutate(state_total = sum(age_pop, na.rm = TRUE),
         ageFraction_state = age_pop / state_total) %>%
  ungroup() %>%
  arrange(stateID, yearID, ageID)
# Result (long/tidy): one row per state × year × age with fraction

# 3) Compute a NATIONAL average age fraction by year (COUNT-weighted)
# This will be used to fill Alaska & Hawaii if they are missing.
us_avg_by_year_age <- state_age_long %>%
  group_by(yearID, ageID) %>%
  summarise(us_age_pop = sum(age_pop, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(yearID) %>%
  mutate(us_total = sum(us_age_pop, na.rm = TRUE),
         ageFraction_state = us_age_pop / us_total) %>%
  ungroup() %>%
  select(yearID, ageID, ageFraction_state)
# If Alaska or Hawaii are missing, fill them with the US average
states_present <- unique(state_age_long$state)
need_ak <- !"Alaska" %in% states_present
need_hi <- !"Hawaii" %in% states_present

add_rows <- bind_rows(
  if (need_ak) us_avg_by_year_age %>%
    mutate(stateID = 2L, state = "Alaska") else NULL,
  if (need_hi) us_avg_by_year_age %>%
    mutate(stateID = 15L, state = "Hawaii") else NULL
) %>%
  # add placeholders for counts if you only need fractions
  mutate(state_total = NA_real_, age_pop = NA_real_) %>%
  relocate(stateID, state, yearID, ageID, age_pop, state_total, ageFraction_state)

state_age_long_filled <- bind_rows(state_age_long, add_rows) %>%
  arrange(stateID, yearID, ageID)


# 4) Wide format (one row per state×year; columns are age fractions)

state_age_wide <- state_age_long_filled %>%
  select(stateID, state, yearID, ageID, ageFraction_state) %>%
  pivot_wider(names_from = ageID, values_from = ageFraction_state,
              names_prefix = "age_")

# sanity check — within each state-year, fractions should sum to 1
check_sum <- state_age_long_filled %>%
  group_by(stateID, state, yearID) %>%
  summarise(sum_frac = sum(ageFraction_state, na.rm = TRUE), .groups = "drop")
