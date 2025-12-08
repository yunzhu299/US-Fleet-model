# Prepare EV Sales Data
# Source: EV Volumes 
# YZC July 2025

# Load libraries and path -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")
data_folder <- "Inputs/"

# Read raw data -----
ev_volumes <- read_csv(file.path(data_folder, "evvolumenew.csv"), na = "")
raw_usa_sales <- ev_volumes %>% filter(`Sales Country` == "USA")
regs <- read_excel(file.path(data_folder, "LDV registration and sales.xlsx"), sheet = "registrations ")
mineral_intensity <- read_excel(file.path(data_folder, "Mineral_Intensity(2).xlsx"), na = "")

# Aggregate monthly Reg to annual -----
usa_sales <- raw_usa_sales
reg_cols <- grep("^Reg_\\d{4}[-_]\\d{2}[-_]\\d{2}", names(usa_sales), value = TRUE)
reg_year_groups <- split(reg_cols, str_extract(reg_cols, "\\d{4}"))
for (yr in names(reg_year_groups)) {
  cols_this_year <- reg_year_groups[[yr]]
  numeric_data <- usa_sales[, cols_this_year] %>%
    mutate(across(everything(), ~as.numeric(.)))
  usa_sales[[paste0("Total_Reg_", yr)]] <- rowSums(numeric_data, na.rm = TRUE)
}

# Aggregate monthly MWh to annual -----
mwh_cols <- grep("^Mwh_\\d{4}[-_]\\d{2}[-_]\\d{2}", names(usa_sales), value = TRUE)
mwh_year_groups <- split(mwh_cols, str_extract(mwh_cols, "\\d{4}"))

for (yr in names(mwh_year_groups)) {
  cols_this_year <- mwh_year_groups[[yr]]
  numeric_data <- usa_sales[, cols_this_year] %>%
    mutate(across(everything(), ~as.numeric(.)))
  usa_sales[[paste0("Total_Mwh_", yr)]] <- rowSums(numeric_data, na.rm = TRUE)
}

# Drop raw Reg and Mwh monthly columns -----
usa_sales <- usa_sales %>% select(-all_of(c(reg_cols, mwh_cols)))

# Replace "NA" string in Cathode Mix -----
usa_sales <- usa_sales %>%
 mutate(`Cathode Mix` = ifelse(`Cathode Mix` == "NA",
                               paste0(`Cathode Chemistry`, " (unspecified)"),
                                `Cathode Mix`))

# Drop 2013 data -----
usa_sales <- usa_sales %>% select(-matches("2013$"))

# Reshape to long format -----
usa_sales$id <- 1:nrow(usa_sales)
usa_sales_long <- usa_sales %>%
  pivot_longer(cols = starts_with("Total_"),
               names_to = c(".value", "Sale Year"),
               names_pattern = "Total_(.*)_(\\d{4})")

# Select and rename final columns -----
usa_sales_filtered <- usa_sales_long %>%
  select(`Sale Year`, `Battery kWh`, `Cathode Mix`, Propulsion,
         `Global Segment`, Reg, Mwh) %>%
  rename(`Total Sales` = Reg,
         `Total Mwh` = Mwh)



# Normalize segment names to Car/SUV/PUT -----
usa_sales_filtered <- usa_sales_filtered %>%
  mutate(`Global Segment` = case_when(
    str_starts(`Global Segment`, "Car") ~ "Car",
    str_starts(`Global Segment`, "SUV") ~ "SUV",
    str_starts(`Global Segment`, "MPV") ~ "SUV",
    str_starts(`Global Segment`, "SS") ~ "SUV",
    str_starts(`Global Segment`, "LCV") ~ "SUV",
    str_starts(`Global Segment`, "PUP") ~ "PUT",
    TRUE ~ `Global Segment`
  ))

# Make sure year is numeric
usa_sales_filtered <- usa_sales_filtered %>%
  mutate(`Sale Year` = as.integer(`Sale Year`))

# 1) One tidy summary for both BEV & PHEV
by_seg_year <- usa_sales_filtered %>%
  filter(Propulsion %in% c("BEV", "PHEV")) %>%
  group_by(Propulsion, `Sale Year`, `Global Segment`) %>%
  summarise(Sales = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  arrange(Propulsion, `Sale Year`, `Global Segment`)

# 2) Split into two data frames (as requested)
bev_sales_by_seg_year  <- by_seg_year %>% filter(Propulsion == "BEV")  %>% select(-Propulsion)
phev_sales_by_seg_year <- by_seg_year %>% filter(Propulsion == "PHEV") %>% select(-Propulsion)

# Wide view: segments as columns for each propulsion
bev_sales_wide  <- bev_sales_by_seg_year  %>% 
  pivot_wider(names_from = `Global Segment`, values_from = Sales, values_fill = 0)

phev_sales_wide <- phev_sales_by_seg_year %>% 
  pivot_wider(names_from = `Global Segment`, values_from = Sales, values_fill = 0)


# EoF

bev_levels <- regs %>%
  filter(State != "United States") %>%
  transmute(State,
            Year,
            BEV = dplyr::coalesce(`Electric (EV)`, 0)) %>%
  arrange(State, Year)

# Compute YoY deltas by state
bev_delta <- bev_levels %>%
  group_by(State) %>%
  mutate(BEV_delta = BEV - lag(BEV)) %>%
  ungroup()

# Build the allocation base: use positive deltas; for first year (NA lag) use level
alloc_base <- bev_delta %>%
  mutate(
    base_delta = if_else(is.na(BEV_delta), BEV, pmax(BEV_delta, 0))
  )

# Provisional shares from deltas
shares_from_delta <- alloc_base %>%
  group_by(Year) %>%
  mutate(US_base = sum(base_delta, na.rm = TRUE),
         share_delta = if_else(US_base > 0, base_delta / US_base, 0)) %>%
  ungroup() %>%
  select(State, Year, share_delta)

# Level shares as fallback
shares_from_level <- bev_levels %>%
  group_by(Year) %>%
  mutate(US_BEV = sum(BEV, na.rm = TRUE),
         share_level = if_else(US_BEV > 0, BEV / US_BEV, 0)) %>%
  ungroup() %>%
  select(State, Year, share_level)

# Use delta shares when they are well-defined; otherwise fall back to level shares
state_share_16_22 <- shares_from_delta %>%
  inner_join(shares_from_level, by = c("State","Year")) %>%
  mutate(share = if_else(share_delta > 0 & is.finite(share_delta), share_delta, share_level)) %>%
  select(State, Year, share) %>%
  # keep the 2016–2022 window as你的原需求
  filter(Year %in% 2016:2022)

# -------------------------------
# 3) Extend shares to 2014–2015 (use 2016 result) and 2023–2024 (use 2022 result)
# -------------------------------
share_2014_2015 <- state_share_16_22 %>%
  filter(Year == 2016) %>%
  select(State, share) %>%
  tidyr::crossing(Year = c(2014L, 2015L))

share_2023_2024 <- state_share_16_22 %>%
  filter(Year == 2022) %>%
  select(State, share) %>%
  tidyr::crossing(Year = c(2023L, 2024L))

state_share_2014_2024 <- bind_rows(
  state_share_16_22,
  share_2014_2015,
  share_2023_2024
) %>%
  filter(Year %in% 2014:2024)

# -------------------------------
# 4) Allocate national Car/Truck BEV to states via the *delta-based* shares
# -------------------------------
bev_state_year_class_sales <- state_share_2014_2024 %>%
  inner_join(nat_bev_year_class, by = "Year") %>%
  mutate(BEV_Sales = share * national_bev) %>%
  select(State, Year, Class, BEV_Sales) %>%
  arrange(State, Year, Class)

# (optional) wide view
bev_state_year_wide <- bev_state_year_class_sales %>%
  tidyr::pivot_wider(names_from = Class, values_from = BEV_Sales, values_fill = 0) %>%
  arrange(State, Year)
# 1) National BEV by Year × {Car, Truck} (Truck = SUV + PUT)
nat_bev_year_class <- bev_sales_by_seg_year %>%
  mutate(Class = case_when(
    `Global Segment` == "Car"              ~ "Car",
    `Global Segment` %in% c("SUV", "PUT")  ~ "Truck",
    TRUE                                   ~ NA_character_
  )) %>%
  filter(!is.na(Class), `Sale Year` %in% 2014:2024) %>%
  group_by(Year = `Sale Year`, Class) %>%
  summarise(national_bev = sum(Sales, na.rm = TRUE), .groups = "drop")

# 2) State BEV share by year from regs (2016–2022)
state_share_16_22 <- regs %>%
  filter(Year %in% 2016:2022) %>%
  transmute(State,
            Year,
            BEV = coalesce(`Electric (EV)`, 0)) %>%
  group_by(Year) %>%
  mutate(US_BEV = sum(BEV, na.rm = TRUE),
         share  = if_else(US_BEV > 0, BEV / US_BEV, 0)) %>%
  ungroup() %>%
  select(State, Year, share)

# 3) Extend shares to 2014–2015 (use 2016) and 2023–2024 (use 2022)
share_2014_2015 <- state_share_16_22 %>%
  filter(Year == 2016) %>%
  select(State, share) %>%
  crossing(Year = c(2014L, 2015L))

share_2023_2024 <- state_share_16_22 %>%
  filter(Year == 2022) %>%
  select(State, share) %>%
  crossing(Year = c(2023L, 2024L))

state_share_2014_2024 <- bind_rows(
  state_share_16_22,
  share_2014_2015,
  share_2023_2024
) %>%
  filter(Year %in% 2014:2024)


# 4) Allocate national Car/Truck BEV to states via shares
bev_state_year_class_sales <- state_share_2014_2024 %>%
  inner_join(nat_bev_year_class, by = "Year") %>%
  mutate(BEV_Sales = share * national_bev) %>%
  select(State, Year, Class, BEV_Sales) %>%
  arrange(State, Year, Class)

# 5) (optional) wide table: one row per State×Year with Car and Truck columns
bev_state_year_wide <- bev_state_year_class_sales %>%
  tidyr::pivot_wider(names_from = Class, values_from = BEV_Sales, values_fill = 0) %>%
  arrange(State, Year)

# Results:
# - bev_state_year_class_sales : State, Year, Class ∈ {Car, Truck}, BEV_Sales
# - bev_state_year_wide        : State, Year, Car, Truck

