## North America New Sales, Retirement, and Cumulative EV Battery Retirement (Geofacet)
## US + Canada (BEV/PHEV/ICE) + Mexico (EV + ICE)
## 2020-2050 only; drop Canada NU; no subtitle; remove Historical legend
## Axis (thousand):
##   NewSales: non-MX max=2000, MX max=4000
##   Retirement: non-MX max=1700, MX max=3000
##   Cumulative: non-MX max=7500, MX max=15000
## YZC Jan 2026

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(geofacet)
library(scales)
library(patchwork)

# -----------------------------
# 0) Settings
# -----------------------------
YEAR_MIN <- 2020
YEAR_MAX <- 2050

# y-axis limits (in thousand)
Y_NEWSALES_MAX_NONMX <- 2000
Y_NEWSALES_MAX_MX    <- 4000

Y_RETIRE_MAX_NONMX   <- 1700
Y_RETIRE_MAX_MX      <- 1500   # MX annual retirement tops near 1.2M; tighter cap
                                # (was 3000) makes the ACCII vs Repeal gap visible

Y_CUMUL_MAX_NONMX    <- 7500
Y_CUMUL_MAX_MX       <- 8000   # MX cumulative tops out near 7.2M; tighter cap makes
                                # the ACCII vs Repeal gap visible (was 15000 -> data
                                # only filled ~47% of the panel)

DROP_CODES <- c("NU")  # Nunavut

# Plot-only scenario labels (data still keyed as ACCII / Repeal)
SCEN_LABELS <- c(
  "ACCII"  = "Policy Baseline",
  "Repeal" = "Policy Rollback"
)

scale_scenario_linetype <- function() {
  scale_linetype_manual(
    values = c("ACCII" = "solid", "Repeal" = "dashed"),
    breaks = c("ACCII", "Repeal"),
    labels = SCEN_LABELS[c("ACCII", "Repeal")]
  )
}

scale_scenario_color <- function() {
  scale_color_manual(
    values = c("ACCII" = "#4F81BD", "Repeal" = "#C0504D"),
    breaks = c("ACCII", "Repeal"),
    labels = SCEN_LABELS[c("ACCII", "Repeal")]
  )
}

scale_scenario_fill <- function() {
  scale_fill_manual(
    values = c("ACCII" = "#4F81BD", "Repeal" = "#C0504D"),
    breaks = c("ACCII", "Repeal"),
    labels = SCEN_LABELS[c("ACCII", "Repeal")]
  )
}

# output size (more square, bigger)
OUT_W <- 20
OUT_H <- 15
OUT_DPI <- 1200

theme_academic <- function(base_size = 16) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 17, face = "bold"),
      legend.key.width = unit(2, "cm"),
      strip.background = element_rect(fill = "grey95", color = "grey80", linewidth = 0.3),
      strip.text = element_text(size = 17, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.ticks = element_line(linewidth = 0.40),
      panel.spacing = unit(0.25, "lines")
    )
}

scale_x_std <- function() {
  scale_x_continuous(
    limits = c(YEAR_MIN, YEAR_MAX),
    breaks = c(2020, 2030, 2040, 2050),
    labels = c("20", "30", "40", "50"),
    expand = expansion(mult = c(0.01, 0.01))
  )
}

# Y-axis label function - always show integers
y_lab_k <- function(x) {
  ifelse(x >= 1000, paste0(x / 1000, "k"), as.character(as.integer(round(x))))
}

# -----------------------------
# 1) Load US
# -----------------------------
us_accii <- read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv", show_col_types = FALSE) %>%
  mutate(Scenario = "ACCII", Country = "US")

us_repeal <- read_csv("Outputs/ClosedLoop_StateTotals_Repeal.csv", show_col_types = FALSE) %>%
  mutate(Scenario = "Repeal", Country = "US")

us_combined <- bind_rows(us_accii, us_repeal) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX)

us_sales <- us_combined %>%
  select(State, Year, Scenario, Country, add_BEV, add_PHEV, add_ICE) %>%
  pivot_longer(c(add_BEV, add_PHEV, add_ICE), names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = sub("^add_", "", Powertrain),
         Type = "NewSales")

us_retire <- us_combined %>%
  select(State, Year, Scenario, Country, ret_BEV, ret_PHEV, ret_ICE) %>%
  pivot_longer(c(ret_BEV, ret_PHEV, ret_ICE), names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = sub("^ret_", "", Powertrain),
         Type = "Retirement")

# -----------------------------
# 2) Load Canada
# -----------------------------
ca_accii <- read_csv("Outputs/Canada/ClosedLoop_StateTotals_ACCII.csv", show_col_types = FALSE) %>%
  mutate(Scenario = "ACCII", Country = "CA")

ca_repeal <- read_csv("Outputs/Canada/ClosedLoop_StateTotals_Repeal.csv", show_col_types = FALSE) %>%
  mutate(Scenario = "Repeal", Country = "CA")

ca_combined <- bind_rows(ca_accii, ca_repeal) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX)

ca_sales <- ca_combined %>%
  select(State, Year, Scenario, Country, add_BEV, add_PHEV, add_ICE) %>%
  pivot_longer(c(add_BEV, add_PHEV, add_ICE), names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = sub("^add_", "", Powertrain),
         Type = "NewSales")

ca_retire <- ca_combined %>%
  select(State, Year, Scenario, Country, ret_BEV, ret_PHEV, ret_ICE) %>%
  pivot_longer(c(ret_BEV, ret_PHEV, ret_ICE), names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = sub("^ret_", "", Powertrain),
         Type = "Retirement")

# -----------------------------
# 3) Load Mexico (both ACCII + Repeal scenarios)
# -----------------------------
# Historical sales (same for both scenarios, 2020–2025)
mex_hist_sales <- read_excel("Inputs/MX_Sales.xlsx", sheet = 2) %>%
  filter(Year >= YEAR_MIN, Year <= 2025) %>%
  transmute(Year, add_BEV = BEV_units, add_PHEV = PHEV_units,
            add_ICE = coalesce(ICE_units, `ice table`))

load_mexico_scenario <- function(fleet_file, scenario_tag) {
  mex_fleet <- read_csv(fleet_file, show_col_types = FALSE) %>%
    filter(Year >= 2026, Year <= YEAR_MAX) %>%
    select(Year, add_BEV, add_PHEV, add_ICE, ret_BEV, ret_PHEV, ret_ICE)
  
  mex_fleet_full <- read_csv(fleet_file, show_col_types = FALSE) %>%
    filter(Year >= YEAR_MIN, Year <= YEAR_MAX)
  
  mex_sales_combined <- bind_rows(
    mex_hist_sales,
    mex_fleet %>% select(Year, add_BEV, add_PHEV, add_ICE)
  )
  
  sales <- mex_sales_combined %>%
    pivot_longer(c(add_BEV, add_PHEV, add_ICE), names_to = "Powertrain", values_to = "Value") %>%
    mutate(Powertrain = sub("^add_", "", Powertrain),
           State = "Mexico", Scenario = scenario_tag, Country = "MX", Type = "NewSales") %>%
    filter(!is.na(Value), !(Powertrain %in% c("BEV","PHEV") & Value == 0))
  
  retire <- mex_fleet_full %>%
    select(Year, ret_BEV, ret_PHEV, ret_ICE) %>%
    pivot_longer(c(ret_BEV, ret_PHEV, ret_ICE), names_to = "Powertrain", values_to = "Value") %>%
    mutate(Powertrain = sub("^ret_", "", Powertrain),
           State = "Mexico", Scenario = scenario_tag, Country = "MX", Type = "Retirement") %>%
    filter(!is.na(Value), !(Powertrain %in% c("BEV","PHEV") & Value == 0))
  
  bind_rows(sales, retire)
}

mex_data <- bind_rows(
  load_mexico_scenario("Outputs/Mexico/Mexico_FleetTurnover_ACCII.csv",  "ACCII"),
  load_mexico_scenario("Outputs/Mexico/Mexico_FleetTurnover_Repeal.csv", "Repeal")
)

# -----------------------------
# 4) Combine all (Sales + Retirement)
# -----------------------------
all_data <- bind_rows(us_sales, us_retire, ca_sales, ca_retire, mex_data) %>%
  mutate(
    Value = coalesce(Value, 0),
    Value_thousands = Value / 1000,
    Group = paste(Powertrain, Scenario, sep = "_")
  )

# -----------------------------
# 5) Build custom geofacet grid (no NU)
# -----------------------------
us_grid <- geofacet::us_state_grid1

us_grid_df <- data.frame(
  code = us_grid$code,
  name = us_grid$name,
  row  = us_grid$row + 4,
  col  = us_grid$col,
  stringsAsFactors = FALSE
)

# Keep AK and HI in original positions (just shifted by +4 rows like other states)
# No special position changes needed

ca_provinces <- data.frame(
  code = c("YT","NT","BC","AB","SK","MB","ON","QC","NB","NS","PE","NL"),
  name = c("Yukon","Northwest Territories","British Columbia","Alberta","Saskatchewan","Manitoba",
           "Ontario","Quebec","New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador"),
  row  = c(1,1,2,2,2,2,3,3,3,3,3,3),
  col  = c(1,2,1,2,3,4,5,6,7,8,9,10),
  stringsAsFactors = FALSE
)

grid_df <- bind_rows(ca_provinces, us_grid_df) %>%
  filter(!(code %in% DROP_CODES))

max_row <- max(grid_df$row, na.rm = TRUE)
grid_df <- bind_rows(
  grid_df,
  data.frame(code = "MX", name = "Mexico", row = max_row + 1, col = 4, stringsAsFactors = FALSE)
)

us_state_names <- data.frame(
  code = c(state.abb, "DC"),
  name = c(state.name, "District of Columbia"),
  stringsAsFactors = FALSE
)

state_to_code <- bind_rows(
  us_state_names,
  ca_provinces %>% select(code, name),
  data.frame(code = "MX", name = "Mexico", stringsAsFactors = FALSE)
) %>%
  filter(!(code %in% DROP_CODES))

all_data <- all_data %>%
  left_join(state_to_code, by = c("State" = "name")) %>%
  mutate(code = coalesce(code, State)) %>%
  filter(code %in% grid_df$code)

# -----------------------------
# 6) Scales
# -----------------------------
powertrain_colors <- c(
  "BEV" = "#2E8B57",
  "PHEV" = "#4169E1",
  "ICE" = "#DC143C"
)

scenario_linetypes <- c("ACCII" = "solid", "Repeal" = "dashed")

# -----------------------------
# 7) Plot: New Sales (MX has bigger y-range via free_y + anchors)
# -----------------------------
sales_data <- all_data %>% filter(Type == "NewSales")

sales_anchor <- data.frame(
  code = grid_df$code,
  Year = YEAR_MAX,
  Powertrain = "BEV",
  Scenario = "ACCII",
  Group = "BEV_ACCII",
  Value_thousands = ifelse(grid_df$code == "MX", Y_NEWSALES_MAX_MX, Y_NEWSALES_MAX_NONMX),
  stringsAsFactors = FALSE
)

p_sales <- ggplot(
  sales_data,
  aes(x = Year, y = Value_thousands, color = Powertrain, linetype = Scenario, group = Group)
) +
  geom_line(linewidth = 0.75, alpha = 0.9, na.rm = TRUE) +
  geom_blank(data = sales_anchor, aes(x = Year, y = Value_thousands), inherit.aes = FALSE) +
  geom_blank(data = data.frame(code = grid_df$code, Year = YEAR_MIN, Value_thousands = 0), 
             aes(x = Year, y = Value_thousands), inherit.aes = FALSE) +  # force Y to start at 0
  facet_geo(~ code, grid = grid_df, label = "code", scales = "free_y") +
  scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
  scale_scenario_linetype() +
  scale_x_std() +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "North America Annual New Vehicle Sales by Powertrain",
    y = "New sales (thousand vehicles)",
    color = "Powertrain",
    linetype = "Scenario"
  ) +
  theme_academic(14)

ggsave("Outputs/NorthAmerica_NewSales_Geofacet.png", p_sales,
       width = OUT_W, height = OUT_H, dpi = OUT_DPI)

# -----------------------------
# 8) Plot: Retirement (MX has bigger y-range via free_y + anchors)
# -----------------------------
retire_data <- all_data %>% filter(Type == "Retirement")

retire_anchor <- data.frame(
  code = grid_df$code,
  Year = YEAR_MAX,
  Powertrain = "BEV",
  Scenario = "ACCII",
  Group = "BEV_ACCII",
  Value_thousands = ifelse(grid_df$code == "MX", Y_RETIRE_MAX_MX, Y_RETIRE_MAX_NONMX),
  stringsAsFactors = FALSE
)

p_retire <- ggplot(
  retire_data,
  aes(x = Year, y = Value_thousands, color = Powertrain, linetype = Scenario, group = Group)
) +
  geom_line(linewidth = 0.75, alpha = 0.9, na.rm = TRUE) +
  geom_blank(data = retire_anchor, aes(x = Year, y = Value_thousands), inherit.aes = FALSE) +
  geom_blank(data = data.frame(code = grid_df$code, Year = YEAR_MIN, Value_thousands = 0), 
             aes(x = Year, y = Value_thousands), inherit.aes = FALSE) +  # force Y to start at 0
  facet_geo(~ code, grid = grid_df, label = "code", scales = "free_y") +
  scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
  scale_scenario_linetype() +
  scale_x_std() +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "North America Annual Vehicle Retirement by Powertrain",
    y = "Retirements (thousand vehicles)",
    color = "Powertrain",
    linetype = "Scenario"
  ) +
  theme_academic(14)

ggsave("Outputs/NorthAmerica_Retirement_Geofacet.png", p_retire,
       width = OUT_W, height = OUT_H, dpi = OUT_DPI)

# -----------------------------
# 9) Plot: Cumulative Battery Retirement (LDV + HDV: Recycling + BESS)
# -----------------------------

# === Helper: load and sum annual battery retirement by State × Year × Scenario ===
load_annual_retire <- function(evlib_file, bess_file, scenario_name, state_col = "State") {
  evlib <- read_csv(evlib_file, show_col_types = FALSE) %>%
    group_by(State = .data[[state_col]], Year) %>%
    summarise(LIB_recycling = sum(coalesce(LIB_recycling, 0), na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scenario_name)

  if (file.exists(bess_file)) {
    bess <- read_csv(bess_file, show_col_types = FALSE) %>%
      group_by(State = .data[[state_col]], Year) %>%
      summarise(BESS_retire = sum(coalesce(BESS_retire_total, 0), na.rm = TRUE), .groups = "drop") %>%
      mutate(Scenario = scenario_name)
  } else {
    bess <- tibble(State = character(), Year = integer(), BESS_retire = double(), Scenario = character())
  }

  evlib %>%
    left_join(bess %>% select(-Scenario), by = c("State", "Year")) %>%
    mutate(Annual_ret = coalesce(LIB_recycling, 0) + coalesce(BESS_retire, 0)) %>%
    select(State, Year, Scenario, Annual_ret)
}

# --- LDV: US ---
ldv_us <- bind_rows(
  load_annual_retire("Outputs/EVLIB_Flows_detail_ACCII.csv",
                     "Outputs/BESS_Retire_Vector_byStateSegProp_ACCII.csv", "ACCII"),
  load_annual_retire("Outputs/EVLIB_Flows_detail_Repeal.csv",
                     "Outputs/BESS_Retire_Vector_byStateSegProp_Repeal.csv", "Repeal")
)

# --- LDV: Canada ---
ldv_ca <- bind_rows(
  load_annual_retire("Outputs/Canada/EVLIB_Flows_detail_ACCII.csv",
                     "Outputs/Canada/BESS_Retire_Vector_byStateSegProp_ACCII.csv", "ACCII"),
  load_annual_retire("Outputs/Canada/EVLIB_Flows_detail_Repeal.csv",
                     "Outputs/Canada/BESS_Retire_Vector_byStateSegProp_Repeal.csv", "Repeal")
)

# --- LDV: Mexico (ACCII + Repeal) ---
load_mx_ldv_retire <- function(evlib_file, bess_file, scenario_name) {
  evlib <- read_csv(evlib_file, show_col_types = FALSE) %>%
    group_by(Year) %>%
    summarise(LIB_recycling = sum(coalesce(LIB_recycling, 0), na.rm = TRUE), .groups = "drop")
  if (file.exists(bess_file)) {
    bess <- read_csv(bess_file, show_col_types = FALSE) %>%
      group_by(Year) %>%
      summarise(BESS_retire = sum(coalesce(BESS_retire_total, 0), na.rm = TRUE), .groups = "drop")
  } else {
    bess <- tibble(Year = integer(), BESS_retire = double())
  }
  evlib %>%
    left_join(bess, by = "Year") %>%
    mutate(Annual_ret = coalesce(LIB_recycling, 0) + coalesce(BESS_retire, 0),
           State = "Mexico", Scenario = scenario_name) %>%
    select(State, Year, Scenario, Annual_ret)
}

ldv_mx <- bind_rows(
  load_mx_ldv_retire("Outputs/Mexico/EVLIB_Flows_detail_ACCII.csv",
                     "Outputs/Mexico/BESS_Retire_Vector_byStateSegProp_ACCII.csv", "ACCII"),
  load_mx_ldv_retire("Outputs/Mexico/EVLIB_Flows_detail_Repeal.csv",
                     "Outputs/Mexico/BESS_Retire_Vector_byStateSegProp_Repeal.csv", "Repeal")
)

# --- HDV: all countries (by state) ---
hdv_load_retire <- function(turnover_file, bess_file, scenario_name) {
  hdv_rec <- read_csv(turnover_file, show_col_types = FALSE) %>%
    group_by(State, Year) %>%
    summarise(LIB_recycling = sum(coalesce(LIB_recycling, 0), na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scenario_name)

  hdv_bess <- read_csv(bess_file, show_col_types = FALSE) %>%
    group_by(State, Year) %>%
    summarise(BESS_retire = sum(coalesce(BESS_retire_total, 0), na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scenario_name)

  hdv_rec %>%
    left_join(hdv_bess %>% select(-Scenario), by = c("State", "Year")) %>%
    mutate(Annual_ret = coalesce(LIB_recycling, 0) + coalesce(BESS_retire, 0)) %>%
    select(State, Year, Scenario, Annual_ret)
}

hdv_all_annual <- bind_rows(
  hdv_load_retire("Outputs/HDV/HDV_EV_Turnover_ACCII.csv",
                  "Outputs/HDV/HDV_BESS_Retire_ACCII.csv", "ACCII"),
  hdv_load_retire("Outputs/HDV/HDV_EV_Turnover_Repeal.csv",
                  "Outputs/HDV/HDV_BESS_Retire_Repeal.csv", "Repeal")
)

# --- Combine LDV + HDV annual retirement ---
all_annual <- bind_rows(ldv_us, ldv_ca, ldv_mx, hdv_all_annual) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  group_by(State, Year, Scenario) %>%
  summarise(Annual_ret = sum(Annual_ret, na.rm = TRUE), .groups = "drop")

# Cumulative sum
cumul_all <- all_annual %>%
  arrange(State, Scenario, Year) %>%
  group_by(State, Scenario) %>%
  mutate(Cumulative_thousands = cumsum(Annual_ret) / 1000) %>%
  ungroup()

cumul_data <- cumul_all %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  left_join(state_to_code, by = c("State" = "name")) %>%
  mutate(code = coalesce(code, State)) %>%
  filter(code %in% grid_df$code)

cumul_anchor <- data.frame(
  code = grid_df$code,
  Year = YEAR_MAX,
  Scenario = "ACCII",
  Cumulative_thousands = ifelse(grid_df$code == "MX", Y_CUMUL_MAX_MX, Y_CUMUL_MAX_NONMX),
  stringsAsFactors = FALSE
)

p_cumul <- ggplot(
  cumul_data,
  aes(x = Year, y = Cumulative_thousands, fill = Scenario)
) +
  geom_area(alpha = 0.35, position = "identity", na.rm = TRUE) +
  geom_line(aes(color = Scenario), linewidth = 0.5, alpha = 0.9, na.rm = TRUE) +
  geom_blank(data = cumul_anchor, aes(x = Year, y = Cumulative_thousands), inherit.aes = FALSE) +
  geom_blank(data = data.frame(code = grid_df$code, Year = YEAR_MIN, Cumulative_thousands = 0), 
             aes(x = Year, y = Cumulative_thousands), inherit.aes = FALSE) +
  facet_geo(~ code, grid = grid_df, label = "code", scales = "free_y") +
  scale_scenario_fill() +
  scale_scenario_color() +
  scale_x_std() +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "North America Cumulative Battery Retirement",
    y = "Cumulative retired batteries (thousand)",
    fill = "Scenario",
    color = "Scenario"
  ) +
  theme_academic(14)

ggsave("Outputs/NorthAmerica_Cumulative_BatteryRetirement_Geofacet.png", p_cumul,
       width = OUT_W, height = OUT_H, dpi = OUT_DPI)

# -----------------------------
# 10) Print
# -----------------------------
print(p_sales)
print(p_retire)
print(p_cumul)

cat("\nSaved:\n")
cat("  Outputs/NorthAmerica_NewSales_Geofacet.png\n")
cat("  Outputs/NorthAmerica_Retirement_Geofacet.png\n")
cat("  Outputs/NorthAmerica_Cumulative_BatteryRetirement_Geofacet.png\n")

# -----------------------------
# 11) California New Sales & Retirement (standalone, Figure 5 style)
# -----------------------------
ca_accii_raw <- read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv", show_col_types = FALSE) %>%
  filter(State == "California", Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  mutate(Scenario = "ACCII")

ca_repeal_raw <- read_csv("Outputs/ClosedLoop_StateTotals_Repeal.csv", show_col_types = FALSE) %>%
  filter(State == "California", Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  mutate(Scenario = "Repeal")

ca_raw <- bind_rows(ca_accii_raw, ca_repeal_raw)

ca_plot_theme <- function(p) {
  p +
    scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
    scale_scenario_linetype() +
    scale_x_continuous(
      limits = c(YEAR_MIN, YEAR_MAX),
      breaks = seq(2020, 2050, 5),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
    guides(
      color = guide_legend(
        order = 1,
        nrow = 1,
        byrow = TRUE,
        title.position = "left",
        override.aes = list(linetype = "solid", linewidth = 1.2)
      ),
      linetype = guide_legend(
        order = 2,
        nrow = 1,
        byrow = TRUE,
        title.position = "left",
        keywidth = unit(2.8, "cm"),
        override.aes = list(color = "black", linewidth = 1.2)
      )
    ) +
    theme_bw(base_size = 16) +
    theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.just = "left",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14, face = "bold"),
      legend.key.width = unit(2.8, "cm"),
      legend.key.height = unit(0.9, "cm"),
      legend.spacing.y = unit(0.35, "cm"),
      legend.margin = margin(t = 4, b = 4),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      plot.margin = margin(t = 10, r = 15, b = 18, l = 15)
    )
}

ca_sales_long <- ca_raw %>%
  select(Year, Scenario, add_BEV, add_PHEV, add_ICE) %>%
  pivot_longer(c(add_BEV, add_PHEV, add_ICE), names_to = "Powertrain", values_to = "Value") %>%
  mutate(
    Powertrain = sub("^add_", "", Powertrain),
    Value_thousands = Value / 1000,
    Group = paste(Powertrain, Scenario, sep = "_")
  )

p_ca_sales <- ca_plot_theme(
  ggplot(
    ca_sales_long,
    aes(x = Year, y = Value_thousands, color = Powertrain, linetype = Scenario, group = Group)
  ) +
    geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) +
    labs(
      title = "California Annual New LDV Sales Projection by Powertrain",
      y = "New sales (thousand vehicles)",
      color = "Powertrain",
      linetype = "Scenario"
    )
)

ggsave("Outputs/California_NewSales.png", p_ca_sales,
       width = 13, height = 8.5, dpi = 300)

ca_retire_long <- ca_raw %>%
  select(Year, Scenario, ret_BEV, ret_PHEV, ret_ICE) %>%
  pivot_longer(c(ret_BEV, ret_PHEV, ret_ICE), names_to = "Powertrain", values_to = "Value") %>%
  mutate(
    Powertrain = sub("^ret_", "", Powertrain),
    Value_thousands = Value / 1000,
    Group = paste(Powertrain, Scenario, sep = "_")
  )

p_ca_retire <- ca_plot_theme(
  ggplot(
    ca_retire_long,
    aes(x = Year, y = Value_thousands, color = Powertrain, linetype = Scenario, group = Group)
  ) +
    geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) +
    labs(
      title = "California Annual LDV Retirement Projection by Powertrain",
      y = "Retirements (thousand vehicles)",
      color = "Powertrain",
      linetype = "Scenario"
    )
)

ggsave("Outputs/California_Retirement.png", p_ca_retire,
       width = 13, height = 8.5, dpi = 300)

ca_tag_theme <- theme(
  plot.tag = element_text(size = 22, face = "bold"),
  plot.tag.position = c(0.015, 0.98)
)

p_ca_combo <- (
  p_ca_sales +
    labs(tag = "(a)") +
    theme(legend.position = "none") +
    ca_tag_theme
) / (
  p_ca_retire +
    labs(tag = "(b)") +
    ca_tag_theme
) +
  plot_layout(heights = c(1, 1), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left"
  )

ggsave("Outputs/California_Sales_Retirement.png", p_ca_combo,
       width = 13, height = 16, dpi = 300)
ggsave("Outputs/California_Sales_Retirement.pdf", p_ca_combo,
       width = 13, height = 16, device = "pdf")

print(p_ca_sales)
print(p_ca_retire)
print(p_ca_combo)
cat("  Outputs/California_NewSales.png\n")
cat("  Outputs/California_Retirement.png\n")
cat("  Outputs/California_Sales_Retirement.png\n")
cat("  Outputs/California_Sales_Retirement.pdf\n")