## North America New Sales, Retirement, and Cumulative Battery Retirement
## US + Canada (BEV/PHEV/ICE) + Mexico (EV + ICE)
## YZC Jan 2026

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(geofacet)
library(scales)

# -----------------------------
# 1) Load US data
# -----------------------------
us_accii <- read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv", show_col_types = FALSE) %>%
  mutate(Scenario = "ACCII", Country = "US")
us_repeal <- read_csv("Outputs/ClosedLoop_StateTotals_Repeal.csv", show_col_types = FALSE) %>%
  mutate(Scenario = "Repeal", Country = "US")

us_combined <- bind_rows(us_accii, us_repeal)

# Sales data
us_sales <- us_combined %>%
  select(State, Year, Scenario, Country, add_BEV, add_PHEV, add_ICE) %>%
  pivot_longer(cols = c(add_BEV, add_PHEV, add_ICE),
               names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = gsub("add_", "", Powertrain), Type = "Sales")

# Retirement data
us_retire <- us_combined %>%
  select(State, Year, Scenario, Country, ret_BEV, ret_PHEV, ret_ICE) %>%
  pivot_longer(cols = c(ret_BEV, ret_PHEV, ret_ICE),
               names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = gsub("ret_", "", Powertrain), Type = "Retirement")

# -----------------------------
# 2) Load Canada data
# -----------------------------
ca_accii <- read_csv("Outputs/Canada/ClosedLoop_StateTotals_ACCII.csv", show_col_types = FALSE) %>%
  mutate(Scenario = "ACCII", Country = "CA")
ca_repeal <- read_csv("Outputs/Canada/ClosedLoop_StateTotals_Repeal.csv", show_col_types = FALSE) %>%
  mutate(Scenario = "Repeal", Country = "CA")

ca_combined <- bind_rows(ca_accii, ca_repeal)

# Sales data
ca_sales <- ca_combined %>%
  select(State, Year, Scenario, Country, add_BEV, add_PHEV, add_ICE) %>%
  pivot_longer(cols = c(add_BEV, add_PHEV, add_ICE),
               names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = gsub("add_", "", Powertrain), Type = "Sales")

# Retirement data
ca_retire <- ca_combined %>%
  select(State, Year, Scenario, Country, ret_BEV, ret_PHEV, ret_ICE) %>%
  pivot_longer(cols = c(ret_BEV, ret_PHEV, ret_ICE),
               names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = gsub("ret_", "", Powertrain), Type = "Retirement")

# -----------------------------
# 3) Load Mexico data
# -----------------------------
mex_raw <- read_csv("Outputs/Mexico_Dereg_EV_vectors_USsurvival.csv", show_col_types = FALSE)

# Mexico Sales - EV
mex_sales_ev <- mex_raw %>%
  select(Year, NewEV_Sales_veh) %>%
  filter(!is.na(NewEV_Sales_veh), NewEV_Sales_veh > 0) %>%
  mutate(State = "Mexico", Scenario = "Historical", Country = "MX",
         Powertrain = "EV(MEX Only)", Value = NewEV_Sales_veh, Type = "Sales")

# Mexico Sales - ICE
mex_sales_ice <- mex_raw %>%
  select(Year, NewICE_Sales_veh) %>%
  filter(!is.na(NewICE_Sales_veh), NewICE_Sales_veh > 0) %>%
  mutate(State = "Mexico", Scenario = "Historical", Country = "MX",
         Powertrain = "ICE", Value = NewICE_Sales_veh, Type = "Sales")

# Mexico Retirement - EV
mex_retire_ev <- mex_raw %>%
  select(Year, Dereg_NewEV_veh, Dereg_SHEV_veh) %>%
  mutate(EV_Ret = coalesce(Dereg_NewEV_veh, 0) + coalesce(Dereg_SHEV_veh, 0)) %>%
  filter(EV_Ret > 0) %>%
  mutate(State = "Mexico", Scenario = "Historical", Country = "MX",
         Powertrain = "EV(MEX Only)", Value = EV_Ret, Type = "Retirement")

# Mexico Retirement - ICE
mex_retire_ice <- mex_raw %>%
  select(Year, ICE_Retirement_veh) %>%
  filter(!is.na(ICE_Retirement_veh), ICE_Retirement_veh > 0) %>%
  mutate(State = "Mexico", Scenario = "Historical", Country = "MX",
         Powertrain = "ICE", Value = ICE_Retirement_veh, Type = "Retirement")

mex_data <- bind_rows(mex_sales_ev, mex_sales_ice, mex_retire_ev, mex_retire_ice)

# -----------------------------
# 4) Combine all data
# -----------------------------
all_data <- bind_rows(us_sales, us_retire, ca_sales, ca_retire, mex_data) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  select(State, Year, Scenario, Country, Powertrain, Value, Type)

# -----------------------------
# 5) Create custom grid
# -----------------------------
us_grid <- geofacet::us_state_grid1

us_grid_df <- data.frame(
  code = us_grid$code,
  name = us_grid$name,
  row = us_grid$row + 4,
  col = us_grid$col,
  stringsAsFactors = FALSE
)

ca_provinces <- data.frame(
  code = c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL", "YT", "NT"),
  name = c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario",
           "Quebec", "New Brunswick", "Nova Scotia", "Prince Edward Island",
           "Newfoundland and Labrador", "Yukon", "Northwest Territories"),
  row = c(2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 1, 1),
  col = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2),
  stringsAsFactors = FALSE
)

grid_df <- rbind(ca_provinces, us_grid_df)
max_row <- max(grid_df$row, na.rm = TRUE)
grid_df <- rbind(grid_df, data.frame(
  code = "MX", name = "Mexico", row = max_row + 1, col = 4, stringsAsFactors = FALSE
))

# State name mapping
us_state_names <- data.frame(
  code = c(state.abb, "DC"),
  name = c(state.name, "District of Columbia"),
  stringsAsFactors = FALSE
)

ca_province_names <- data.frame(
  code = c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL", "YT", "NT"),
  name = c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario",
           "Quebec", "New Brunswick", "Nova Scotia", "Prince Edward Island",
           "Newfoundland and Labrador", "Yukon", "Northwest Territories"),
  stringsAsFactors = FALSE
)

mx_name <- data.frame(code = "MX", name = "Mexico", stringsAsFactors = FALSE)
state_to_code <- rbind(us_state_names, ca_province_names, mx_name)

# Map names to codes
all_data <- all_data %>%
  left_join(state_to_code, by = c("State" = "name")) %>%
  mutate(code = coalesce(code, State)) %>%
  filter(code %in% grid_df$code)

# -----------------------------
# 6) Prepare plot data
# -----------------------------
# Define colors
powertrain_colors <- c(
  "BEV" = "#2E8B57",
  "PHEV" = "#4169E1",
  "ICE" = "#DC143C",
  "EV(MEX Only)" = "#6B8E23"
)

scenario_linetypes <- c(
  "ACCII" = "solid",
  "Repeal" = "dashed",
  "Historical" = "solid"
)

all_data <- all_data %>%
  mutate(
    Group = paste(Powertrain, Scenario, sep = "_"),
    Value_thousands = Value / 1000
  )

# Split data
sales_data <- all_data %>% filter(Type == "Sales")
retire_data <- all_data %>% filter(Type == "Retirement")

# -----------------------------
# 7) Create SALES plot (free Y-axis per panel)
# -----------------------------
p_sales <- ggplot(sales_data, aes(x = Year, y = Value_thousands,
                                   color = Powertrain, linetype = Scenario, group = Group)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  facet_geo(~ code, grid = grid_df, label = "code", scales = "free_y") +
  scale_color_manual(values = powertrain_colors) +
  scale_linetype_manual(values = scenario_linetypes) +
  scale_x_continuous(breaks = seq(2020, 2050, 15)) +
  scale_y_continuous(labels = function(x) ifelse(x >= 1000, paste0(x/1000, "K"), x)) +
  labs(
    title = "North America Annual New Vehicle Sales by Powertrain (2020-2050)",
    x = NULL,
    y = "New Sales",
    color = "Powertrain",
    linetype = "Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_blank(),  # Remove y-axis text for clarity
    axis.ticks.y = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
    axis.title.y = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines")
  ) +
  guides(
    color = guide_legend(nrow = 1, title.position = "left"),
    linetype = guide_legend(nrow = 1, title.position = "left")
  )

# -----------------------------
# 8) Create RETIREMENT plot (free Y-axis per panel)
# -----------------------------
p_retire <- ggplot(retire_data, aes(x = Year, y = Value_thousands,
                                     color = Powertrain, linetype = Scenario, group = Group)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  facet_geo(~ code, grid = grid_df, label = "code", scales = "free_y") +
  scale_color_manual(values = powertrain_colors) +
  scale_linetype_manual(values = scenario_linetypes) +
  scale_x_continuous(breaks = seq(2020, 2050, 15)) +
  scale_y_continuous(labels = function(x) ifelse(x >= 1000, paste0(x/1000, "K"), x)) +
  labs(
    title = "North America Annual Vehicle Retirement by Powertrain (2020-2050)",
    x = NULL,
    y = "Retirement",
    color = "Powertrain",
    linetype = "Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_blank(),  # Remove y-axis text for clarity
    axis.ticks.y = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
    axis.title.y = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines")
  ) +
  guides(
    color = guide_legend(nrow = 1, title.position = "left"),
    linetype = guide_legend(nrow = 1, title.position = "left")
  )

# -----------------------------
# 9) Save Sales and Retirement plots
# -----------------------------
ggsave("Outputs/NorthAmerica_NewSales_Geofacet.png", p_sales,
       width = 14, height = 10, dpi = 300)
cat("Sales plot saved to Outputs/NorthAmerica_NewSales_Geofacet.png\n")

ggsave("Outputs/NorthAmerica_Retirement_Geofacet.png", p_retire,
       width = 14, height = 10, dpi = 300)
cat("Retirement plot saved to Outputs/NorthAmerica_Retirement_Geofacet.png\n")

print(p_sales)
print(p_retire)

# -----------------------------
# 10) Cumulative Battery Retirement by State (Overlapping Area)
# -----------------------------
cat("\n=== Creating Cumulative Battery Retirement Plot ===\n")

# Prepare US cumulative data
us_cumul <- us_combined %>%
  select(State, Year, Scenario, ret_BEV, ret_PHEV) %>%
  mutate(Total_EV_Retire = ret_BEV + ret_PHEV) %>%
  group_by(State, Scenario) %>%
  arrange(Year) %>%
  mutate(Cumulative = cumsum(Total_EV_Retire)) %>%
  ungroup()

# Prepare Canada cumulative data
ca_cumul <- ca_combined %>%
  select(State, Year, Scenario, ret_BEV, ret_PHEV) %>%
  mutate(Total_EV_Retire = ret_BEV + ret_PHEV) %>%
  group_by(State, Scenario) %>%
  arrange(Year) %>%
  mutate(Cumulative = cumsum(Total_EV_Retire)) %>%
  ungroup()

# Prepare Mexico cumulative data (use ACCII scenario label for color)
mx_cumul <- mex_raw %>%
  select(Year, Dereg_NewEV_veh, Dereg_SHEV_veh) %>%
  mutate(
    Total_EV_Retire = coalesce(Dereg_NewEV_veh, 0) + coalesce(Dereg_SHEV_veh, 0),
    State = "Mexico",
    Scenario = "ACCII"
  ) %>%
  group_by(State, Scenario) %>%
  arrange(Year) %>%
  mutate(Cumulative = cumsum(Total_EV_Retire)) %>%
  ungroup() %>%
  select(State, Year, Scenario, Total_EV_Retire, Cumulative)

# Combine all cumulative data
cumul_data <- bind_rows(us_cumul, ca_cumul, mx_cumul) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  left_join(state_to_code, by = c("State" = "name")) %>%
  mutate(
    code = coalesce(code, State),
    Cumulative_thousands = Cumulative / 1000
  ) %>%
  filter(code %in% grid_df$code)

# Define fill colors - ACCII blue, Repeal red
scenario_fill_colors <- c(
  "ACCII" = "#4169E1",
  "Repeal" = "#DC143C"
)

# Create cumulative overlapping area plot
p_cumul <- ggplot(cumul_data, aes(x = Year, y = Cumulative_thousands,
                                   fill = Scenario)) +
  geom_area(alpha = 0.4, position = "identity") +
  geom_line(aes(color = Scenario), linewidth = 0.5, alpha = 0.8) +
  facet_geo(~ code, grid = grid_df, label = "code", scales = "free_y") +
  scale_fill_manual(values = scenario_fill_colors) +
  scale_color_manual(values = scenario_fill_colors) +
  scale_x_continuous(breaks = seq(2020, 2050, 15)) +
  scale_y_continuous(labels = function(x) ifelse(x >= 1000, paste0(x/1000, "K"), x)) +
  labs(
    title = "North America Cumulative EV Battery Retirement (2020-2050)",
    x = NULL,
    y = "Cumulative Retired Batteries (thousands)",
    fill = "Scenario",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_blank(),  # Remove y-axis text for clarity
    axis.ticks.y = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
    axis.title.y = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines")
  ) +
  guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1))

ggsave("Outputs/NorthAmerica_Cumulative_BatteryRetirement_Geofacet.png", p_cumul,
       width = 14, height = 10, dpi = 300)
cat("Cumulative battery retirement plot saved to Outputs/NorthAmerica_Cumulative_BatteryRetirement_Geofacet.png\n")

print(p_cumul)

cat("\nAll plots completed!\n")
