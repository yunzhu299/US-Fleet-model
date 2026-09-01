## New Sales: US / Canada / Mexico — three separate geofacet PNGs for PPT assembly
## Y-axis label only on the US plot (leftmost when arranged in PPT)
## YZC Feb 2026

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(geofacet)
library(scales)

# -----------------------------
# 0) Settings
# -----------------------------
YEAR_MIN <- 2020
YEAR_MAX <- 2050

Y_NEWSALES_MAX_US <- 2000
Y_NEWSALES_MAX_CA <- 600
Y_NEWSALES_MAX_MX <- 4000

DROP_CODES <- c("NU")

OUT_DPI <- 600

y_lab_k <- function(x) {
  ifelse(x >= 1000, paste0(x / 1000, "k"), as.character(as.integer(round(x))))
}

scale_x_std <- function() {
  scale_x_continuous(
    limits = c(YEAR_MIN, YEAR_MAX),
    breaks = c(2020, 2030, 2040, 2050),
    labels = c("20", "30", "40", "50"),
    expand = expansion(mult = c(0.01, 0.01))
  )
}

powertrain_colors <- c("BEV" = "#2E8B57", "PHEV" = "#4169E1", "ICE" = "#DC143C")
scenario_linetypes <- c("ACCII" = "solid", "Repeal" = "dashed")
SCEN_LABELS <- c("ACCII" = "Policy Baseline", "Repeal" = "Policy Rollback")

theme_geo <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.title     = element_text(size = 13, face = "bold"),
      legend.text      = element_text(size = 13),
      legend.key.width = unit(1.8, "cm"),
      strip.background = element_rect(fill = "grey95", color = "grey80", linewidth = 0.3),
      strip.text    = element_text(size = 13, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      axis.title.x  = element_blank(),
      axis.title.y  = element_text(size = 16, face = "bold"),
      axis.text.x   = element_text(size = 11),
      axis.text.y   = element_text(size = 11),
      axis.ticks     = element_line(linewidth = 0.35),
      panel.spacing  = unit(0.25, "lines")
    )
}

# ============================================================
# 1) US
# ============================================================
us_accii  <- read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv",  show_col_types = FALSE) %>% mutate(Scenario = "ACCII")
us_repeal <- read_csv("Outputs/ClosedLoop_StateTotals_Repeal.csv", show_col_types = FALSE) %>% mutate(Scenario = "Repeal")

us_sales <- bind_rows(us_accii, us_repeal) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  select(State, Year, Scenario, add_BEV, add_PHEV, add_ICE) %>%
  pivot_longer(c(add_BEV, add_PHEV, add_ICE), names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = sub("^add_", "", Powertrain),
         Value_thousands = coalesce(Value, 0) / 1000,
         Group = paste(Powertrain, Scenario, sep = "_"))

us_grid <- geofacet::us_state_grid1
us_state_names <- data.frame(
  code = c(state.abb, "DC"),
  name = c(state.name, "District of Columbia"),
  stringsAsFactors = FALSE
)

us_sales <- us_sales %>%
  left_join(us_state_names, by = c("State" = "name")) %>%
  mutate(code = coalesce(code, State)) %>%
  filter(code %in% us_grid$code)

us_anchor <- data.frame(code = us_grid$code, Year = YEAR_MAX, Value_thousands = Y_NEWSALES_MAX_US)
us_zero   <- data.frame(code = us_grid$code, Year = YEAR_MIN, Value_thousands = 0)

p_us <- ggplot(us_sales, aes(x = Year, y = Value_thousands, color = Powertrain,
                              linetype = Scenario, group = Group)) +
  geom_line(linewidth = 0.7, alpha = 0.9, na.rm = TRUE) +
  geom_blank(data = us_anchor, aes(x = Year, y = Value_thousands), inherit.aes = FALSE) +
  geom_blank(data = us_zero,   aes(x = Year, y = Value_thousands), inherit.aes = FALSE) +
  facet_geo(~ code, grid = us_grid, label = "code", scales = "free_y") +
  scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
  scale_linetype_manual(values = scenario_linetypes, breaks = names(scenario_linetypes),
                        labels = SCEN_LABELS[names(scenario_linetypes)]) +
  scale_x_std() +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "United States — New Vehicle Sales", y = "New sales (thousand)",
       color = "Powertrain", linetype = "Scenario") +
  theme_geo(14)

ggsave("Outputs/NewSales_US.png", p_us, width = 20, height = 14, dpi = OUT_DPI)
cat("Saved: Outputs/NewSales_US.png\n")

# ============================================================
# 2) Canada
# ============================================================
ca_accii  <- read_csv("Outputs/Canada/ClosedLoop_StateTotals_ACCII.csv",  show_col_types = FALSE) %>% mutate(Scenario = "ACCII")
ca_repeal <- read_csv("Outputs/Canada/ClosedLoop_StateTotals_Repeal.csv", show_col_types = FALSE) %>% mutate(Scenario = "Repeal")

ca_sales <- bind_rows(ca_accii, ca_repeal) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  select(State, Year, Scenario, add_BEV, add_PHEV, add_ICE) %>%
  pivot_longer(c(add_BEV, add_PHEV, add_ICE), names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = sub("^add_", "", Powertrain),
         Value_thousands = coalesce(Value, 0) / 1000,
         Group = paste(Powertrain, Scenario, sep = "_"))

ca_provinces <- data.frame(
  code = c("YT","NT","BC","AB","SK","MB","ON","QC","NB","NS","PE","NL"),
  name = c("Yukon","Northwest Territories","British Columbia","Alberta",
           "Saskatchewan","Manitoba","Ontario","Quebec",
           "New Brunswick","Nova Scotia","Prince Edward Island",
           "Newfoundland and Labrador"),
  row  = c(1,1,2,2,2,2,3,3,3,3,3,3),
  col  = c(1,2,1,2,3,4,5,6,7,8,9,10),
  stringsAsFactors = FALSE
) %>% filter(!(code %in% DROP_CODES))

ca_sales <- ca_sales %>%
  left_join(ca_provinces %>% select(code, name), by = c("State" = "name")) %>%
  mutate(code = coalesce(code, State)) %>%
  filter(code %in% ca_provinces$code)

ca_anchor <- data.frame(code = ca_provinces$code, Year = YEAR_MAX, Value_thousands = Y_NEWSALES_MAX_CA)
ca_zero   <- data.frame(code = ca_provinces$code, Year = YEAR_MIN, Value_thousands = 0)

p_ca <- ggplot(ca_sales, aes(x = Year, y = Value_thousands, color = Powertrain,
                              linetype = Scenario, group = Group)) +
  geom_line(linewidth = 0.7, alpha = 0.9, na.rm = TRUE) +
  geom_blank(data = ca_anchor, aes(x = Year, y = Value_thousands), inherit.aes = FALSE) +
  geom_blank(data = ca_zero,   aes(x = Year, y = Value_thousands), inherit.aes = FALSE) +
  facet_geo(~ code, grid = ca_provinces, label = "code", scales = "free_y") +
  scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
  scale_linetype_manual(values = scenario_linetypes, breaks = names(scenario_linetypes),
                        labels = SCEN_LABELS[names(scenario_linetypes)]) +
  scale_x_std() +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Canada — New Vehicle Sales", y = "New sales (thousand)",
       color = "Powertrain", linetype = "Scenario") +
  theme_geo(14)

ggsave("Outputs/NewSales_Canada.png", p_ca, width = 20, height = 8, dpi = OUT_DPI)
cat("Saved: Outputs/NewSales_Canada.png\n")

# ============================================================
# 3) Mexico (single panel)
# ============================================================
mex_hist_sales <- read_excel("Inputs/MX_Sales.xlsx", sheet = 2) %>%
  filter(Year >= YEAR_MIN, Year <= 2025) %>%
  transmute(Year, add_BEV = BEV_units, add_PHEV = PHEV_units,
            add_ICE = coalesce(ICE_units, `ice table`))

mex_fleet <- read_csv("Outputs/Mexico/Mexico_FleetTurnover_2022_2050.csv", show_col_types = FALSE) %>%
  filter(Year >= 2026, Year <= YEAR_MAX) %>%
  select(Year, add_BEV, add_PHEV, add_ICE)

mex_sales <- bind_rows(mex_hist_sales, mex_fleet) %>%
  pivot_longer(c(add_BEV, add_PHEV, add_ICE), names_to = "Powertrain", values_to = "Value") %>%
  mutate(Powertrain = sub("^add_", "", Powertrain),
         Value_thousands = coalesce(Value, 0) / 1000,
         Scenario = "ACCII",
         Group = paste(Powertrain, Scenario, sep = "_"))

p_mx <- ggplot(mex_sales, aes(x = Year, y = Value_thousands, color = Powertrain,
                               linetype = Scenario, group = Group)) +
  geom_line(linewidth = 1, alpha = 0.9, na.rm = TRUE) +
  scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
  scale_linetype_manual(values = scenario_linetypes, breaks = names(scenario_linetypes),
                        labels = SCEN_LABELS[names(scenario_linetypes)]) +
  scale_x_std() +
  scale_y_continuous(labels = y_lab_k, limits = c(0, Y_NEWSALES_MAX_MX),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Mexico — New Vehicle Sales", y = "New sales (thousand)",
       color = "Powertrain", linetype = "Scenario") +
  theme_geo(14)

ggsave("Outputs/NewSales_Mexico.png", p_mx, width = 8, height = 6, dpi = OUT_DPI)
cat("Saved: Outputs/NewSales_Mexico.png\n")

cat("\nAll 3 plots saved. Assemble in PowerPoint as needed.\n")
