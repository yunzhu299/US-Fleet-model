# Disaggregate EV Data
# Source: EV Volume + LDV Registration https://afdc.energy.gov/vehicle-registration
# YZC July 2025

# Load libraries
source("Scripts/00-Libraries.R", encoding = "UTF-8")
usa_sales_filtered <- read_csv("Parameters/EVSales.csv")
regs <- read_excel(file.path(data_folder, "LDV registration and sales.xlsx"), sheet = "registrations ")

# Define powertrain categories
powertrain <- c("BEV", "PHEV", "FCEV")

# 1. Weighted average battery chemistry share -----
chem_Mwh <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion, `Cathode Mix`) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop") %>%
  filter(`Total Mwh` > 0) %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  mutate(`Share of Avg Chem` = `Total Mwh` / sum(`Total Mwh`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = Propulsion, values_from = `Share of Avg Chem`, values_fill = 0)

# 2. Weighted average battery capacity (kWh per vehicle) -----
batt_cap_sales <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  summarise(`Total Sales` = sum(`Total Sales`, na.rm = TRUE), .groups = "drop")

batt_cap_mwh <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop")

batt_cap_merged <- inner_join(batt_cap_sales, batt_cap_mwh,
                              by = c("Sale Year", "Global Segment", "Propulsion")) %>%
  mutate(`Avg Batt Cap (kwh/batt)` = (`Total Mwh` / `Total Sales`) * 1000) %>%
  pivot_wider(names_from = Propulsion, values_from = `Avg Batt Cap (kwh/batt)`, values_fill = 0)

# 3. Vehicle category ratio (Car/SUV) per powertrain -----
veh_cat <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  summarise(`Total Sales` = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  group_by(`Sale Year`, Propulsion) %>%
  mutate(`Share Vehicle Category` = `Total Sales` / sum(`Total Sales`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = Propulsion, values_from = `Share Vehicle Category`, values_fill = 0)

# 4. State-level EV registration shares -----
regs_zev <- regs %>%
  select(State, `Electric (EV)`, `Plug-In Hybrid Electric (PHEV)`, Hydrogen, Year) %>%
  rename(BEV = `Electric (EV)`,
         PHEV = `Plug-In Hybrid Electric (PHEV)`,
         FCEV = Hydrogen,
         `Reg Year` = Year) %>%
  filter(State != "United States")

regs_zev <- regs_zev %>%
  group_by(`Reg Year`) %>%
  mutate(across(all_of(powertrain), ~ . / sum(., na.rm = TRUE), .names = "Fraction_{.col}")) %>%
  ungroup() %>%
  select(State, `Reg Year`, starts_with("Fraction_"))

# Expand missing years (2014–2015 and 2024) -----
keep_new <- regs_zev %>% filter(`Reg Year` == 2023) %>% mutate(`Reg Year` = 2024)
keep_old <- regs_zev %>% filter(`Reg Year` == 2016)
old_years <- 2014:2015

extended <- bind_rows(
  map_dfr(old_years, ~ keep_old %>% mutate(`Reg Year` = .x)),
  regs_zev,
  keep_new
) %>%
  arrange(`Reg Year`, State)

# 5. Powertrain sales share by year -----
pt_sales <- usa_sales_filtered %>%
  group_by(`Sale Year`, Propulsion) %>%
  summarise(`Total Sales` = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Propulsion, values_from = `Total Sales`, values_fill = 0)

pt_sales <- pt_sales %>%
  mutate(Total = rowSums(across(all_of(powertrain)))) %>%
  mutate(across(all_of(powertrain), ~ . / Total, .names = "Share_{.col}"))

# 6. Total sales by year (for survival modeling) -----
total_sales_for_survival <- usa_sales_filtered %>%
  group_by(`Sale Year`) %>%
  summarise(`Total Sales` = sum(`Total Sales`, na.rm = TRUE), .groups = "drop")

write_csv(chem_Mwh, "Parameters/ChemistryShares.csv")           # chemistry share by year / segment / powertrain
write_csv(batt_cap_merged, "Parameters/AvgBatteryCapacity.csv") # average battery capacity (kWh) per vehicle
write_csv(veh_cat, "Parameters/VehicleCategoryShare.csv")        # Car/SUV share by powertrain and year
write_csv(extended, "Parameters/EVRegistrationsExtended.csv") # processed registration fractions by state/year
write_csv(pt_sales, "Parameters/PowertrainShares.csv")           # powertrain-level sales shares
write_csv(total_sales_for_survival, "Parameters/TotalSalesSurvival.csv") # total sales per year for survival simulation
