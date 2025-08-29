# vehicle per capita & growth demand
# YZC Aug 2025
PopulationProj <- read_excel("~/Library/CloudStorage/GoogleDrive-yuzchen@ucdavis.edu/My Drive/US Fleet modeling/NationalProjections_ProjectedTotalPopulation_2030-2050.xlsx", range = "A3:F56")

# -------------------------------
# 1) Vehicles-per-person (VPP) from 2020 registrations
# -------------------------------
regs_base_2020 <- regs %>%
  dplyr::filter(Year == 2020) %>%
  dplyr::mutate(
    BEV  = dplyr::coalesce(`Electric (EV)`, 0),
    PHEV = dplyr::coalesce(`Plug-In Hybrid Electric (PHEV)`, 0),
    HEV  = dplyr::coalesce(`Hybrid Electric (HEV)`, 0),
    ICE  = dplyr::coalesce(Gasoline, 0)
  ) %>%
  dplyr::transmute(State, TotalVeh_2020 = BEV + PHEV + HEV + ICE)

pop_2020 <- pop_annual %>%
  dplyr::filter(Year == 2020) %>%
  dplyr::select(State, Pop_2020 = Population)

vpp_by_state <- pop_2020 %>%
  dplyr::inner_join(regs_base_2020, by = "State") %>%
  dplyr::mutate(VPP = dplyr::if_else(Pop_2020 > 0, TotalVeh_2020 / Pop_2020, NA_real_))

# -----------------------------------------------
# 2) Turn population growth into annual vehicle additions
#    Growth_from_pop(t) = max(0, (Pop_t - Pop_{t-1}) * VPP_state)
#    Remove pmax(0, ·) to allow negative demand when population falls.
# -----------------------------------------------
growth_from_pop <- pop_annual %>%
  left_join(vpp_by_state %>% select(State, VPP), by = "State") %>%
  arrange(State, Year) %>%
  group_by(State) %>%
  mutate(
    DeltaPop        = Population - lag(Population),
    Growth_from_pop = pmax(0, coalesce(DeltaPop, 0) * VPP),
    TargetStock     = Population * VPP
  ) %>%
  ungroup() %>%
  select(State, Year, Growth_from_pop, TargetStock)

# national totals
growth_US <- growth_from_pop %>%
  group_by(Year) %>%
  summarise(
    Growth_from_pop_US = sum(Growth_from_pop, na.rm = TRUE),
    TargetStock_US     = sum(TargetStock, na.rm = TRUE),
    .groups = "drop"
  )

# quick peek
head(vpp_by_state)
head(growth_from_pop)
head(growth_US)