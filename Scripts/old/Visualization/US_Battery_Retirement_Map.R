## US Battery Retirement Geographic Visualization
## Cumulative kWh retired by state, 2025-2050

library(tidyverse)
library(usmap)      # For US state maps
library(scales)
library(viridis)
library(patchwork)  # For combining plots
install.packages("gtable")
# ==============================================================================
# 1. Load Data
# ==============================================================================

# Load EVLIB flows data
evlib_accii <- read_csv("Outputs/EVLIB_Flows_detail_ACCII.csv", show_col_types = FALSE)
evlib_repeal <- read_csv("Outputs/EVLIB_Flows_detail_Repeal.csv", show_col_types = FALSE)

# Load battery capacity data
battery_cap <- read_csv("Inputs/Parameters/AvgBatteryCapacity.csv", show_col_types = FALSE) %>%
  rename(Year = `Sale Year`, Segment = `Global Segment`) %>%
  # Calculate weighted average capacity for BEV and PHEV
  filter(!is.na(BEV) | !is.na(PHEV)) %>%
  mutate(
    BEV_kWh = ifelse(BEV > 0, BEV, NA),
    PHEV_kWh = ifelse(PHEV > 0, PHEV, NA)
  ) %>%
  group_by(Year, Segment) %>%
  summarise(
    BEV_kWh = mean(BEV_kWh, na.rm = TRUE),
    PHEV_kWh = mean(PHEV_kWh, na.rm = TRUE),
    .groups = "drop"
  )

# Fill future years with 2025 values (assume constant after 2025)
future_years <- tibble(Year = 2026:2050)
battery_cap_extended <- battery_cap %>%
  filter(Year == 2025) %>%
  select(-Year) %>%
  crossing(future_years) %>%
  bind_rows(battery_cap) %>%
  arrange(Year, Segment)

# ==============================================================================
# 2. Calculate Retired kWh by State and Year
# ==============================================================================

library(tidyverse)

# 把向量字符串 "a|b|c|..." 变成：带名字的数值向量，名字=Sale_Year
name_vector_with_years <- function(vec_string, recycle_year) {
  v <- as.numeric(strsplit(as.character(vec_string), "\\|")[[1]])
  names(v) <- recycle_year - (seq_along(v) - 1)   # Sale_Year = Year, Year-1, ...
  v
}

# 读入 AvgBatteryCapacity，并做成 (Sale_Year, Segment, Propulsion) -> kWh/veh
build_capacity_long <- function(cap_file, need_sale_years) {

  cap <- read_csv(cap_file, show_col_types = FALSE) %>%
    rename(Sale_Year = `Sale Year`,
           Segment   = `Global Segment`) %>%
    mutate(Sale_Year = as.integer(Sale_Year)) %>%
    filter(!is.na(Sale_Year))

  # 你这个文件通常列名是 BEV / PHEV（值就是kWh），先做干净一点
  cap2 <- cap %>%
    transmute(
      Sale_Year, Segment,
      BEV  = ifelse(!is.na(BEV)  & BEV  > 0, as.numeric(BEV),  NA_real_),
      PHEV = ifelse(!is.na(PHEV) & PHEV > 0, as.numeric(PHEV), NA_real_)
    ) %>%
    pivot_longer(c(BEV, PHEV), names_to = "Propulsion", values_to = "kWh_per_vehicle") %>%
    filter(!is.na(kWh_per_vehicle))

  # 关键：为了覆盖所有需要的 Sale_Year（向量里会出现很早/很晚的年份），对每个 Segment×Propulsion 做插值+外推
  minY <- min(need_sale_years, na.rm = TRUE)
  maxY <- max(need_sale_years, na.rm = TRUE)

  cap_filled <- cap2 %>%
    group_by(Segment, Propulsion) %>%
    complete(Sale_Year = minY:maxY) %>%
    arrange(Sale_Year) %>%
    mutate(
      kWh_per_vehicle = approx(
        x    = Sale_Year[!is.na(kWh_per_vehicle)],
        y    = kWh_per_vehicle[!is.na(kWh_per_vehicle)],
        xout = Sale_Year,
        rule = 2   # 两端用边界值外推（如果你想用趋势外推，后面我也能改）
      )$y
    ) %>%
    ungroup()

  cap_filled
}

# 用“生产年匹配容量”的方式算：按 回收年Year 汇总，但保留 Segment
calculate_retired_kwh_prodYearCapacity <- function(evlib_file, cap_file, scenario_name) {

  evlib <- read_csv(evlib_file, show_col_types = FALSE) %>%
    # 这里 Year 是回收年（scrap year）
    mutate(
      LIB_recycling_vector = Map(name_vector_with_years, LIB_recycling_vector, Year)
    )

  # 展开成 long：每行=（回收年Year, Sale_Year, count）
  long <- evlib %>%
    mutate(vdf = map(LIB_recycling_vector, ~ tibble(
      Sale_Year = as.integer(names(.x)),
      retired_count = as.numeric(.x)
    ))) %>%
    select(State, Segment, Propulsion, Year, vdf) %>%
    unnest(vdf) %>%
    filter(!is.na(Sale_Year), !is.na(retired_count), retired_count > 0)

  # 按向量里出现的 Sale_Year 范围，准备 capacity
  cap_long <- build_capacity_long(cap_file, need_sale_years = long$Sale_Year)

  # 生产年匹配容量：用 Sale_Year join（而不是 Year）
  out <- long %>%
    left_join(cap_long, by = c("Sale_Year", "Segment", "Propulsion")) %>%
    mutate(retired_kWh = retired_count * kWh_per_vehicle) %>%
    group_by(State, Segment, Year) %>%
    summarise(
      annual_retired_kWh   = sum(retired_kWh, na.rm = TRUE),
      annual_retired_count = sum(retired_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(State, Segment, Year) %>%
    group_by(State, Segment) %>%
    mutate(
      cumulative_kWh   = cumsum(annual_retired_kWh),
      cumulative_count = cumsum(annual_retired_count)
    ) %>%
    ungroup() %>%
    mutate(Scenario = scenario_name)

  out
}
# ==============================================================================
# 3. Create Geographic Map Function
# ==============================================================================

create_state_map <- function(data, year_to_plot, scenario, value_col = "cumulative_kWh") {
  
  plot_data <- data %>%
    filter(Year == year_to_plot, Scenario == scenario) %>%
    mutate(
      state = State,
      # Convert to GWh for readability
      value_GWh = get(value_col) / 1e6
    )
  
  # Map using usmap
  plot_usmap(data = plot_data, values = "value_GWh", regions = "states") +
    scale_fill_viridis_c(
      name = "Cumulative\nRetired (GWh)",
      option = "plasma",
      trans = "sqrt",  # sqrt transform for better color distribution
      labels = comma
    ) +
    labs(
      title = paste0("Cumulative Battery Retirement by State - ", scenario),
      subtitle = paste0("Year: ", year_to_plot)
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

# ==============================================================================
# 4. Generate Maps for Key Years
# ==============================================================================

key_years <- c(2030, 2040, 2050)

# Create individual maps
maps_accii <- map(key_years, ~create_state_map(retired_all, .x, "ACCII"))
maps_repeal <- map(key_years, ~create_state_map(retired_all, .x, "Repeal"))

# Combine maps
combined_accii <- wrap_plots(maps_accii, ncol = 1) +
  plot_annotation(title = "ACCII Scenario: Cumulative Battery Retirement (GWh)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

combined_repeal <- wrap_plots(maps_repeal, ncol = 1) +
  plot_annotation(title = "Repeal Scenario: Cumulative Battery Retirement (GWh)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

# Save maps
ggsave("Outputs/US_Battery_Retirement_Map_ACCII.png", combined_accii, 
       width = 12, height = 18, dpi = 300)
ggsave("Outputs/US_Battery_Retirement_Map_Repeal.png", combined_repeal, 
       width = 12, height = 18, dpi = 300)

cat("Saved: Outputs/US_Battery_Retirement_Map_ACCII.png\n")
cat("Saved: Outputs/US_Battery_Retirement_Map_Repeal.png\n")

# ==============================================================================
# 5. Create Animated-Style Timeline Plot (Faceted by Year)
# ==============================================================================

# Select years for faceted view
timeline_years <- seq(2025, 2050, by = 5)

timeline_data <- retired_all %>%
  filter(Year %in% timeline_years, Scenario == "ACCII") %>%
  mutate(
    state = State,
    value_GWh = cumulative_kWh / 1e6
  )

# Create faceted map
facet_map <- plot_usmap(data = timeline_data, values = "value_GWh", regions = "states") +
  facet_wrap(~Year, ncol = 3) +
  scale_fill_viridis_c(
    name = "Cumulative\nRetired (GWh)",
    option = "plasma",
    trans = "sqrt",
    labels = comma
  ) +
  labs(
    title = "Evolution of Cumulative Battery Retirement by State (ACCII)",
    subtitle = "2025-2050 in 5-year intervals"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  )

ggsave("Outputs/US_Battery_Retirement_Timeline_ACCII.png", facet_map,
       width = 15, height = 12, dpi = 300)

cat("Saved: Outputs/US_Battery_Retirement_Timeline_ACCII.png\n")

# ==============================================================================
# 6. Side-by-Side Comparison: ACCII vs Repeal for 2050
# ==============================================================================

comparison_2050 <- retired_all %>%
  filter(Year == 2050) %>%
  mutate(
    state = State,
    value_GWh = cumulative_kWh / 1e6
  )

p1 <- plot_usmap(data = comparison_2050 %>% filter(Scenario == "ACCII"), 
                 values = "value_GWh", regions = "states") +
  scale_fill_viridis_c(
    name = "GWh",
    option = "plasma",
    trans = "sqrt",
    labels = comma,
    limits = c(0, max(comparison_2050$value_GWh))
  ) +
  labs(title = "ACCII Scenario (2050)") +
  theme(legend.position = "bottom")

p2 <- plot_usmap(data = comparison_2050 %>% filter(Scenario == "Repeal"), 
                 values = "value_GWh", regions = "states") +
  scale_fill_viridis_c(
    name = "GWh",
    option = "plasma",
    trans = "sqrt",
    labels = comma,
    limits = c(0, max(comparison_2050$value_GWh))
  ) +
  labs(title = "Repeal Scenario (2050)") +
  theme(legend.position = "bottom")

comparison_plot <- p1 + p2 +
  plot_annotation(
    title = "Cumulative Battery Retirement by 2050: ACCII vs Repeal",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("Outputs/US_Battery_Retirement_Comparison_2050.png", comparison_plot,
       width = 16, height = 8, dpi = 300)

cat("Saved: Outputs/US_Battery_Retirement_Comparison_2050.png\n")

# ==============================================================================
# 7. Print Summary Statistics
# ==============================================================================

cat("\n========================================\n")
cat("Summary: Cumulative Battery Retirement by 2050\n")
cat("========================================\n")

summary_2050 <- retired_all %>%
  filter(Year == 2050) %>%
  group_by(Scenario) %>%
  summarise(
    Total_GWh = sum(cumulative_kWh) / 1e6,
    Total_Batteries = sum(cumulative_count),
    Top_State = State[which.max(cumulative_kWh)],
    Top_State_GWh = max(cumulative_kWh) / 1e6,
    .groups = "drop"
  )

print(summary_2050)

cat("\n========================================\n")
cat("Top 10 States by Cumulative Battery Retirement (2050, ACCII)\n")
cat("========================================\n")

top_states <- retired_all %>%
  filter(Year == 2050, Scenario == "ACCII") %>%
  arrange(desc(cumulative_kWh)) %>%
  head(10) %>%
  mutate(cumulative_GWh = cumulative_kWh / 1e6) %>%
  select(State, cumulative_GWh, cumulative_count)

print(top_states)

# ==============================================================================
# 8. Save Data for Further Analysis
# ==============================================================================

write_csv(retired_all, "Outputs/US_Battery_Retirement_byState_Year.csv")
cat("\nSaved: Outputs/US_Battery_Retirement_byState_Year.csv\n")


