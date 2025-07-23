# Prepare EV Sales Data
# Source: EV Volumes 
# YZC July 2025

# Load libraries and path -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")
data_folder <- "Inputs/"

# Read raw data -----
ev_volumes <- read_csv(file.path(data_folder, "USA_BatteryInstallation-Tracker-January_2025_Data.csv"), na = "")
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

# Filter out non-LDV segments (e.g., PUP) -----
usa_sales_filtered <- usa_sales_filtered %>%
  filter(!str_starts(`Global Segment`, "PUP"))

# Normalize segment names to Car/SUV -----
usa_sales_filtered <- usa_sales_filtered %>%
  mutate(`Global Segment` = case_when(
    str_starts(`Global Segment`, "Car") ~ "Car",
    str_starts(`Global Segment`, "SUV") ~ "SUV",
    str_starts(`Global Segment`, "MPV") ~ "SUV",
    str_starts(`Global Segment`, "SS") ~ "SUV",
    str_starts(`Global Segment`, "LCV") ~ "SUV",
    TRUE ~ `Global Segment`
  ))

write_csv(usa_sales_filtered, "Parameters/EVSales.csv")

# EoF