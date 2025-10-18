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

pop_long <- PopulationProj %>%
  # Keep state name only; drop US aggregate if present
  rename(State = `Geography Name`) %>%
  filter(!is.na(State), State != "United States") %>%
  # Keep all year columns like "2020","2030",...
  pivot_longer(cols = matches("^20\\d{2}$"),
               names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year)) %>%
  arrange(State, Year)

# Make it annual by linear interpolation within each state
pop_annual <- pop_long %>%
  group_by(State) %>%
  complete(Year = full_seq(2020:2050, 1)) %>%   # create 2020..2050 for each state
  arrange(State, Year) %>%
  mutate(
    Population = approx(
      x    = Year[!is.na(Population)],
      y    = Population[!is.na(Population)],
      xout = Year, method = "linear", rule = 2
    )$y
  ) %>%
  ungroup()

pop_2020 <- pop_annual %>%
  dplyr::filter(Year == 2020) %>%
  dplyr::select(State, Pop_2020 = Population)

vpp_by_state <- pop_2020 %>%
  dplyr::inner_join(regs_base_2020, by = "State") %>%
  dplyr::mutate(VPP = dplyr::if_else(Pop_2020 > 0, TotalVeh_2020 / Pop_2020, NA_real_))

# -----------------------------------------------
# 2) Turn population growth into annual vehicle additions
#    Growth_from_pop(t) = (Pop_t - Pop_{t-1}) * VPP_state
#    Allow negative demand when population declines.
# -----------------------------------------------
growth_from_pop <- pop_annual %>%
  left_join(vpp_by_state %>% select(State, VPP), by = "State") %>%
  arrange(State, Year) %>%
  group_by(State) %>%
  mutate(
    DeltaPop        = Population - lag(Population),
    # allows negative growth if population decreases
    Growth_from_pop = coalesce(DeltaPop, 0) * VPP,
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

## ------------------------------------------------------------
## 03b — Used-vehicle exports/imports → Net export ratio
##   - Read export/import tables (by partner, wide years)
##   - Replace Mexico 2020–2024 exports with SH ICE series
##   - Build net exports by year (2020–2024)
##   - Join with realized retirements to get export ratio:
##       ratio_y = max(0, Net_Export_y) / max(1, Ret_ALL_y)
##   - Compute weighted-average shrink factor (2025+ use avg)
## YZC Oct 2025
## ------------------------------------------------------------

library(stringr)
library(readr)

# 1) Read both datasets
export_df <- read_excel(
  "~/Library/CloudStorage/GoogleDrive-yuzchen@ucdavis.edu/My Drive/Fleet Model/Fleet model/Inputs/Used PV Countries Export Volume.xlsx"
)
import_df <- read_excel(
  "~/Library/CloudStorage/GoogleDrive-yuzchen@ucdavis.edu/My Drive/Fleet Model/Fleet model/Inputs/Used PV Countries Import Volume.xlsx"
)

# Mexico override: use "Yearly Reg and Dereg" sheet, SH ICE (million units)
mex_raw <- read_excel(
  "~/Library/CloudStorage/GoogleDrive-yuzchen@ucdavis.edu/My Drive/Fleet Model/Fleet model/Inputs/Mexico data.xlsx",
  sheet = "Yearly Reg and Dereg",
  skip  = 2,
  col_names = TRUE
)

# Robust column detection; fallback to known positions if needed
year_col  <- names(mex_raw)[str_which(names(mex_raw), "^YEAR$")[1]]
shice_col <- names(mex_raw)[str_which(names(mex_raw), regex("^SH\\s*ICE", ignore_case = TRUE))[1]]

if (is.na(year_col) || is.na(shice_col)) {
  # Fallback to first and fourth columns as in your preview
  mex_sh_ice <- mex_raw %>%
    transmute(
      Year   = as.integer(.data[[1]]),
      Export = as.numeric(.data[[4]]) * 1e6  # millions → units
    )
} else {
  mex_sh_ice <- mex_raw %>%
    transmute(
      Year   = as.integer(.data[[year_col]]),
      Export = as.numeric(.data[[shice_col]]) * 1e6  # millions → units
    )
}

mex_sh_ice <- mex_sh_ice %>%
  filter(Year >= 2020, Year <= 2024, !is.na(Export))

# 2) Wide → long (Year, Volume)
export_long <- export_df %>%
  pivot_longer(cols = -Partner, names_to = "Year", values_to = "Export") %>%
  mutate(Year = as.integer(Year))

import_long <- import_df %>%
  pivot_longer(cols = -Partner, names_to = "Year", values_to = "Import") %>%
  mutate(Year = as.integer(Year))

# Replace Mexico 2020–2024 in export table with SH ICE series
export_replaced <- export_long %>%
  filter(!(Partner == "Mexico" & Year >= 2020 & Year <= 2024)) %>%
  bind_rows(mex_sh_ice %>% mutate(Partner = "Mexico"))

# 3) Yearly totals (2020–2024)
export_yearly <- export_replaced %>%
  filter(Year >= 2020, Year <= 2024) %>%
  group_by(Year) %>%
  summarise(Total_Export = sum(as.numeric(Export), na.rm = TRUE), .groups = "drop")

import_yearly <- import_long %>%
  filter(Year >= 2020, Year <= 2024) %>%
  group_by(Year) %>%
  summarise(Total_Import = sum(as.numeric(Import), na.rm = TRUE), .groups = "drop")

net_export_by_year <- full_join(export_yearly, import_yearly, by = "Year") %>%
  mutate(Net_Export = Total_Export - Total_Import) %>%
  arrange(Year)

# 4) Join with realized retirements from ACCII outputs (2020–2024)
#    We only need national totals to derive the export ratio by year.
totals <- readr::read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv") %>%
  filter(Year >= 2020, Year <= 2024) %>%
  group_by(Year) %>%
  summarise(
    Ret_ICE  = sum(ret_ICE,  na.rm = TRUE),
    Ret_BEV  = sum(ret_BEV,  na.rm = TRUE),
    Ret_PHEV = sum(ret_PHEV, na.rm = TRUE),
    Ret_ALL  = Ret_ICE + Ret_BEV + Ret_PHEV,
    .groups  = "drop"
  )

# 5) Export ratio by year (cap at [0,1])
ratio_tbl <- net_export_by_year %>%
  select(Year, Net_Export) %>%
  left_join(totals, by = "Year") %>%
  mutate(
    Net_Export = pmax(0, Net_Export),  # treat net import as 0 export for factor
    Ret_ALL    = pmax(1, Ret_ALL),     # avoid divide-by-zero
    ratio      = Net_Export / Ret_ALL
  )

# 6) Weighted-average export factor for 2025+ (weight = annual retirements)
shrink_ratio_avg <- with(ratio_tbl, sum(ratio * Ret_ALL) / sum(Ret_ALL))
