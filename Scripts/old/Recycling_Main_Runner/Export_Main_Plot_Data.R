## ====================================================================
## Export_Main_Plot_Data.R
## Export plot data only (Fig01-Fig09) from main parity outputs.
## No master sheets.
## ====================================================================

if (!exists("FLEET_SCEN")) FLEET_SCEN <- Sys.getenv("FLEET_SCEN", "ACCII")
if (!FLEET_SCEN %in% c("ACCII", "Repeal")) {
  stop("FLEET_SCEN must be 'ACCII' or 'Repeal'; got '", FLEET_SCEN, "'")
}

PROJECT_ROOT <- getwd()
PARITY_DIR <- file.path(PROJECT_ROOT, "Outputs", "Recycling_Plots_main", FLEET_SCEN, "_parity")
OUT_DIR <- file.path(PROJECT_ROOT, "Outputs", "Recycling_Plot_Data_main", FLEET_SCEN)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

safe_read_parity <- function(file_name, required = TRUE) {
  p <- file.path(PARITY_DIR, file_name)
  if (!file.exists(p)) {
    if (required) stop("Missing required parity file: ", file_name)
    message("  [skip] missing parity file: ", file_name)
    return(NULL)
  }
  read.csv(p, check.names = FALSE)
}

add_fig_sheet <- function(wb, sheet_name, fig_title, data_df) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data.frame(Title = fig_title, stringsAsFactors = FALSE), startRow = 1, colNames = FALSE)
  writeData(wb, sheet_name, data_df, startRow = 2, colNames = TRUE)
  if (nrow(data_df) > 0 && ncol(data_df) > 0) {
    addFilter(wb, sheet = sheet_name, rows = 2, cols = 1:ncol(data_df))
  }
  setColWidths(wb, sheet_name, cols = 1:ncol(data_df), widths = "auto")
}

## ---------- Read parity ----------
mass_2050 <- safe_read_parity("Mass_2050_projected.csv")
state_master_all_years <- safe_read_parity("state_master_all_years.csv")
na_overtime <- safe_read_parity("NA_overtime_data.csv", required = FALSE)
na_cap_chem_rec <- safe_read_parity("NA_cap_chem_rec.csv")
non_recovery_lost <- safe_read_parity("non_recovery_lost.csv")
needed_cap_long <- safe_read_parity("needed_cap_long.csv")
export_lost <- safe_read_parity("export_lost.csv")
ratio_results <- safe_read_parity("ratio_results.csv")
overall_circularity <- safe_read_parity("overall_circularity.csv")

## ---------- Build state long ----------
us_codes <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC")
ca_codes <- c("AB","BC","MB","NB","NL","NT","NS","NU","ON","PE","QC","SK","YT")
region_mapping <- c(
  "WA"="US-West","OR"="US-West","CA"="US-West","NV"="US-West","ID"="US-West","HI"="US-West","AK"="US-West",
  "MT"="US-Mountain","WY"="US-Mountain","UT"="US-Mountain","CO"="US-Mountain","AZ"="US-Mountain","NM"="US-Mountain",
  "OH"="US-Midwest","IN"="US-Midwest","IL"="US-Midwest","MI"="US-Midwest","WI"="US-Midwest","MN"="US-Midwest",
  "IA"="US-Midwest","MO"="US-Midwest","ND"="US-Midwest","SD"="US-Midwest","NE"="US-Midwest","KS"="US-Midwest",
  "TX"="US-South","OK"="US-South","AR"="US-South","LA"="US-South","KY"="US-South","TN"="US-South","MS"="US-South","AL"="US-South",
  "ME"="US-East","NH"="US-East","VT"="US-East","MA"="US-East","RI"="US-East","CT"="US-East","NY"="US-East","NJ"="US-East","PA"="US-East","DE"="US-East","MD"="US-East","DC"="US-East","VA"="US-East","WV"="US-East","NC"="US-East","SC"="US-East","GA"="US-East","FL"="US-East",
  "BC"="Canada-West","YT"="Canada-West","AB"="Canada-Mountain","MB"="Canada-Midwest","SK"="Canada-Midwest",
  "ON"="Canada-East","QC"="Canada-East","NB"="Canada-East","NS"="Canada-East","PE"="Canada-East","NL"="Canada-East","NT"="Canada-East","NU"="Canada-East","MX"="Mexico"
)
metric_key <- tibble::tibble(
  internal = c("Add_LIB_proj_tonnes","Add_LIB_15_tonnes","Tonnes_Prod_proj_down","Tonnes_Prod_15_down","Tonnes_Prod_proj_mid","Tonnes_Prod_15_mid","Recycle_Batt_Proj","Recycle_Batt_15","Cumulative_black_mass_cap","Cumulative_refining_cap"),
  Origin = c("LIB Demand","LIB Demand","Pack Manufacturing","Pack Manufacturing","Cell Manufacturing","Cell Manufacturing","End of Life Batteries","End of Life Batteries","Black Mass","Refining"),
  Legend = c("LIB Demand (Increasing Batt Cap - Benchmark Chemistry)","LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)","Pack Manufacturing","Decreasing Batt Cap Pack Manufacturing","Cell Manufacturing","Decreasing Batt Cap Cell Manufacturing","EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)","EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)","Black Mass","Refining")
)

state_long <- state_master_all_years %>%
  mutate(
    Year = as.integer(Year),
    State_Province = toupper(trimws(as.character(State_Province))),
    State_Province = if_else(State_Province == "SLP", "MX", State_Province)
  ) %>%
  filter(State_Province != "PR") %>%
  pivot_longer(cols = any_of(metric_key$internal), names_to = "internal", values_to = "Metric Tonnes") %>%
  left_join(metric_key, by = "internal") %>%
  mutate(
    Country = case_when(State_Province %in% us_codes ~ "US", State_Province %in% ca_codes ~ "CA", State_Province == "MX" ~ "MX", TRUE ~ NA_character_),
    Region = unname(region_mapping[State_Province]),
    `Metric Tonnes` = as.numeric(`Metric Tonnes`)
  ) %>%
  filter(!is.na(Country), !is.na(Origin), !is.na(Legend))

## Use parity-corrected 2050 values where available.
mass_2050_long <- mass_2050 %>%
  mutate(
    Year = as.integer(Year),
    State_Province = toupper(trimws(as.character(State_Province))),
    State_Province = if_else(State_Province == "SLP", "MX", State_Province)
  ) %>%
  filter(State_Province != "PR") %>%
  pivot_longer(cols = any_of(metric_key$internal), names_to = "internal", values_to = "Metric Tonnes") %>%
  left_join(metric_key, by = "internal") %>%
  mutate(
    Country = case_when(State_Province %in% us_codes ~ "US", State_Province %in% ca_codes ~ "CA", State_Province == "MX" ~ "MX", TRUE ~ NA_character_),
    Region = unname(region_mapping[State_Province]),
    `Metric Tonnes` = as.numeric(`Metric Tonnes`)
  ) %>%
  filter(!is.na(Country), !is.na(Origin), !is.na(Legend))

state_long <- bind_rows(
  state_long %>% filter(Year < 2050),
  mass_2050_long %>% filter(Year == 2050)
)

## ---------- Fig01 ----------
fig01_data <- state_long %>%
  filter(Year == 2050) %>%
  select(Country, Region, State_Province, Origin, Year, Legend, `Metric Tonnes`) %>%
  mutate(`Metric Tonnes (millions)` = `Metric Tonnes` / 1e6) %>%
  select(Country, Region, State_Province, Origin, Year, Legend, `Metric Tonnes (millions)`) %>%
  arrange(Country, State_Province, Origin)

## ---------- Fig01b ----------
fig01b_data <- fig01_data %>%
  group_by(Country, Region, Origin, Year, Legend) %>%
  summarise(`Metric Tonnes (millions)` = sum(`Metric Tonnes (millions)`, na.rm = TRUE), .groups = "drop") %>%
  arrange(Country, Region, Origin)

## ---------- Fig02 ----------
fig02_data <- state_long %>%
  filter(Year == 2050) %>%
  group_by(Country, Legend) %>%
  summarise(Tonnes = sum(`Metric Tonnes`, na.rm = TRUE) / 1e6, .groups = "drop") %>%
  transmute(
    Country,
    Region = Country,
    Metric = Legend,
    pattern_type = case_when(Country == "US" ~ "circle", Country == "CA" ~ "stripe", Country == "MX" ~ "crosshatch", TRUE ~ "none"),
    Tonnes
  ) %>%
  arrange(Country, Metric)

## ---------- Fig03 ----------
fig03_data <- if (!is.null(na_overtime) && all(c("Year","Metric","Tonnes") %in% names(na_overtime))) {
  na_overtime %>%
    transmute(Year = as.integer(Year), Metric = as.character(Metric), Tonnes = as.numeric(Tonnes)) %>%
    filter(Year >= 2025, Year <= 2050) %>%
    arrange(Year, Metric)
} else {
  state_long %>%
    group_by(Year, Legend) %>%
    summarise(Tonnes = sum(`Metric Tonnes`, na.rm = TRUE) / 1e6, .groups = "drop") %>%
    transmute(Year, Metric = Legend, Tonnes) %>%
    filter(Year >= 2025, Year <= 2050) %>%
    arrange(Year, Metric)
}

## ---------- Fig04 ----------
fig04_data <- na_cap_chem_rec %>%
  transmute(
    Year = as.integer(Year),
    Scenario = as.character(Scenario),
    Mineral = as.character(Mineral),
    `Recycling Scenario` = as.character(`Recycling Scenario`),
    Tonne = as.numeric(Tonne) / 1000
  ) %>%
  arrange(Year, Scenario, Mineral, `Recycling Scenario`)

## ---------- Fig05 ----------
fig05_data <- non_recovery_lost %>%
  transmute(
    Scenario = as.character(Scenario),
    Mineral = as.character(Mineral),
    Year = as.integer(Year),
    Cum_Tonne = as.numeric(Cum_Tonne) / 1000
  ) %>%
  filter(Year == 2035) %>%
  arrange(Scenario, Mineral)

## ---------- Fig06 ----------
fig06_data <- needed_cap_long %>%
  transmute(
    Year = as.integer(Year),
    Scenario = as.character(Scenario),
    `Recycling Step` = as.character(`Recycling Step`),
    Tonne = as.numeric(Tonne),
    Scenario_Recycling = as.character(Scenario_Recycling)
  ) %>%
  arrange(Year, Scenario, `Recycling Step`)

## ---------- Fig07 ----------
fig07_data <- export_lost %>%
  transmute(
    Year = as.integer(Year),
    Mineral = as.character(Mineral),
    Scenario = as.character(Scenario),
    Total_Minerals_Exported = as.numeric(Total_Minerals_Exported) / 1000
  ) %>%
  arrange(Year, Mineral, Scenario)

## ---------- Fig08 ----------
fig08_data <- ratio_results %>%
  transmute(
    Year = as.integer(Year),
    Scenario = as.character(Scenario),
    Mineral = as.character(Mineral),
    `Recycling Scenario` = as.character(`Recycling Scenario`),
    Recycle_v_Demand = as.numeric(Recycle_v_Demand) * 100
  ) %>%
  arrange(Year, Scenario, Mineral, `Recycling Scenario`)

## ---------- Fig09 ----------
fig09_data <- overall_circularity %>%
  transmute(
    Country = as.character(Country),
    Year = as.integer(Year),
    Scenario = as.character(Scenario),
    Mineral = as.character(Mineral),
    `Recycling Scenario` = as.character(`Recycling Scenario`),
    Type = as.character(Type),
    pattern_type = as.character(pattern_type),
    Tonnes = as.numeric(Tonnes)
  ) %>%
  arrange(Country, Scenario, Mineral, Type)

## ---------- Build workbook ----------
wb <- createWorkbook()

addWorksheet(wb, "README")
readme <- data.frame(
  Sheet = c(
    "Fig01_Data","Fig01b_Data","Fig02_Data","Fig03_Data","Fig04_Data",
    "Fig05_Data","Fig06_Data","Fig07_Data","Fig08_Data","Fig09_Data"
  ),
  Figure_Name = c(
    "01 Geofacet by State (2050)",
    "01b Aggregated by Region (2050)",
    "02 National Stacked (2050)",
    "03 North America Timeseries (2025-2050)",
    "04 Yearly Recoverable Minerals",
    "05 Cumulative Minerals Lost (2035)",
    "06 Yearly Deficit in Black Mass and Refining Capacity",
    "07 Exported Minerals Timeseries",
    "08 Maximum Recycled Content Standard",
    "09 Mineral Demand vs Availability (2050)"
  ),
  Units = c(
    "millions metric tonnes",
    "millions metric tonnes",
    "millions metric tonnes",
    "millions metric tonnes",
    "thousands metric tonnes",
    "thousands metric tonnes",
    "metric tonnes",
    "thousands metric tonnes",
    "percent (%)",
    "metric tonnes"
  ),
  stringsAsFactors = FALSE
)
writeData(wb, "README", data.frame(Notes = paste0("Scenario: ", FLEET_SCEN), stringsAsFactors = FALSE), startRow = 1, colNames = FALSE)
writeData(wb, "README", data.frame(Notes = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), stringsAsFactors = FALSE), startRow = 2, colNames = FALSE)
writeData(wb, "README", readme, startRow = 4, colNames = TRUE)
addFilter(wb, "README", rows = 4, cols = 1:3)
setColWidths(wb, "README", cols = 1:3, widths = "auto")

add_fig_sheet(wb, "Fig01_Data", "Figure 01 - Geofacet by State (2050)", fig01_data)
add_fig_sheet(wb, "Fig01b_Data", "Figure 01b - Aggregated by Region (2050)", fig01b_data)
add_fig_sheet(wb, "Fig02_Data", "Figure 02 - National Stacked (2050)", fig02_data)
add_fig_sheet(wb, "Fig03_Data", "Figure 03 - North America Timeseries (2025-2050)", fig03_data)
add_fig_sheet(wb, "Fig04_Data", "Figure 04 - Yearly Recoverable Minerals", fig04_data)
add_fig_sheet(wb, "Fig05_Data", "Figure 05 - Cumulative Minerals Lost (2035)", fig05_data)
add_fig_sheet(wb, "Fig06_Data", "Figure 06 - Yearly Deficit in Black Mass and Refining Capacity", fig06_data)
add_fig_sheet(wb, "Fig07_Data", "Figure 07 - Exported Minerals Timeseries", fig07_data)
add_fig_sheet(wb, "Fig08_Data", "Figure 08 - Maximum Recycled Content Standard", fig08_data)
add_fig_sheet(wb, "Fig09_Data", "Figure 09 - Mineral Demand vs Availability (2050)", fig09_data)

out_xlsx <- file.path(OUT_DIR, paste0("Main_Plot_Data_Workbook_", FLEET_SCEN, ".xlsx"))
saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Wrote workbook: ", out_xlsx)
message("Included sheets: ", paste(sheets(wb), collapse = ", "))
