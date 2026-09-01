#!/usr/bin/env Rscript
# Figure 7: cumulative North American battery retirements from LDVs and HDVs.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(geofacet)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
root <- normalizePath(file.path(script_dir, "..", ".."))
out_dir <- file.path(root, "Results", "Figures")
data_dir <- file.path(root, "Results", "Data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

year_min <- 2020
year_max <- 2050
y_max_non_mx <- 7500
y_max_mx <- 8000

load_ldv_retirement <- function(country_dir, scenario, state_col = "State") {
  base <- file.path(root, "Outputs", country_dir)
  ev_file <- file.path(base, paste0("EVLIB_Flows_detail_", scenario, ".csv"))
  bess_file <- file.path(base, paste0("BESS_Retire_Vector_byStateSegProp_", scenario, ".csv"))

  ev <- read_csv(ev_file, show_col_types = FALSE) %>%
    group_by(State = .data[[state_col]], Year) %>%
    summarise(LIB_recycling = sum(coalesce(LIB_recycling, 0), na.rm = TRUE),
              .groups = "drop")
  bess <- read_csv(bess_file, show_col_types = FALSE) %>%
    group_by(State = .data[[state_col]], Year) %>%
    summarise(BESS_retire = sum(coalesce(BESS_retire_total, 0), na.rm = TRUE),
              .groups = "drop")

  ev %>%
    left_join(bess, by = c("State", "Year")) %>%
    mutate(
      Annual_ret = coalesce(LIB_recycling, 0) + coalesce(BESS_retire, 0),
      Scenario = scenario
    ) %>%
    select(State, Year, Scenario, Annual_ret)
}

load_mexico_ldv <- function(scenario) {
  base <- file.path(root, "Outputs", "Mexico")
  ev <- read_csv(
    file.path(base, paste0("EVLIB_Flows_detail_", scenario, ".csv")),
    show_col_types = FALSE
  ) %>%
    group_by(Year) %>%
    summarise(LIB_recycling = sum(coalesce(LIB_recycling, 0), na.rm = TRUE),
              .groups = "drop")
  bess <- read_csv(
    file.path(base, paste0("BESS_Retire_Vector_byStateSegProp_", scenario, ".csv")),
    show_col_types = FALSE
  ) %>%
    group_by(Year) %>%
    summarise(BESS_retire = sum(coalesce(BESS_retire_total, 0), na.rm = TRUE),
              .groups = "drop")

  ev %>%
    left_join(bess, by = "Year") %>%
    mutate(
      Annual_ret = coalesce(LIB_recycling, 0) + coalesce(BESS_retire, 0),
      State = "Mexico", Scenario = scenario
    ) %>%
    select(State, Year, Scenario, Annual_ret)
}

load_hdv_retirement <- function(scenario) {
  base <- file.path(root, "Outputs", "HDV")
  ev <- read_csv(
    file.path(base, paste0("HDV_EV_Turnover_", scenario, ".csv")),
    show_col_types = FALSE
  ) %>%
    group_by(State, Year) %>%
    summarise(LIB_recycling = sum(coalesce(LIB_recycling, 0), na.rm = TRUE),
              .groups = "drop")
  bess <- read_csv(
    file.path(base, paste0("HDV_BESS_Retire_", scenario, ".csv")),
    show_col_types = FALSE
  ) %>%
    group_by(State, Year) %>%
    summarise(BESS_retire = sum(coalesce(BESS_retire_total, 0), na.rm = TRUE),
              .groups = "drop")

  ev %>%
    left_join(bess, by = c("State", "Year")) %>%
    mutate(
      Annual_ret = coalesce(LIB_recycling, 0) + coalesce(BESS_retire, 0),
      Scenario = scenario
    ) %>%
    select(State, Year, Scenario, Annual_ret)
}

annual <- bind_rows(
  load_ldv_retirement("", "ACCII"),
  load_ldv_retirement("", "Repeal"),
  load_ldv_retirement("Canada", "ACCII"),
  load_ldv_retirement("Canada", "Repeal"),
  load_mexico_ldv("ACCII"),
  load_mexico_ldv("Repeal"),
  load_hdv_retirement("ACCII"),
  load_hdv_retirement("Repeal")
) %>%
  filter(between(Year, year_min, year_max)) %>%
  group_by(State, Year, Scenario) %>%
  summarise(Annual_ret = sum(Annual_ret, na.rm = TRUE), .groups = "drop") %>%
  arrange(State, Scenario, Year) %>%
  group_by(State, Scenario) %>%
  mutate(Cumulative_thousands = cumsum(Annual_ret) / 1000) %>%
  ungroup()

us_grid <- geofacet::us_state_grid1
us_grid_df <- data.frame(
  code = us_grid$code, name = us_grid$name,
  row = us_grid$row + 4, col = us_grid$col, stringsAsFactors = FALSE
)
canada_grid <- data.frame(
  code = c("YT", "NT", "BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL"),
  name = c("Yukon", "Northwest Territories", "British Columbia", "Alberta",
           "Saskatchewan", "Manitoba", "Ontario", "Quebec", "New Brunswick",
           "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador"),
  row = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3),
  col = c(1, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  stringsAsFactors = FALSE
)
grid_df <- bind_rows(canada_grid, us_grid_df)
grid_df <- bind_rows(
  grid_df,
  data.frame(code = "MX", name = "Mexico", row = max(grid_df$row) + 1,
             col = 4, stringsAsFactors = FALSE)
)
state_codes <- bind_rows(
  data.frame(code = c(state.abb, "DC"),
             name = c(state.name, "District of Columbia")),
  canada_grid %>% select(code, name),
  data.frame(code = "MX", name = "Mexico")
)

plot_data <- annual %>%
  left_join(state_codes, by = c("State" = "name")) %>%
  mutate(code = coalesce(code, State)) %>%
  filter(code %in% grid_df$code)

write_csv(
  plot_data,
  file.path(data_dir, "Fig07_North_American_Cumulative_Battery_Retirements.csv")
)

anchors <- data.frame(
  code = grid_df$code, Year = year_max,
  Cumulative_thousands = ifelse(grid_df$code == "MX", y_max_mx, y_max_non_mx)
)
scenario_labels <- c("ACCII" = "Policy Baseline", "Repeal" = "Policy Rollback")
y_lab_k <- function(x) ifelse(x >= 1000, paste0(x / 1000, "k"), as.character(round(x)))

figure <- ggplot(plot_data, aes(Year, Cumulative_thousands, fill = Scenario)) +
  geom_area(alpha = 0.35, position = "identity", na.rm = TRUE) +
  geom_line(aes(color = Scenario), linewidth = 0.5, alpha = 0.9, na.rm = TRUE) +
  geom_blank(data = anchors, aes(Year, Cumulative_thousands), inherit.aes = FALSE) +
  geom_blank(
    data = data.frame(code = grid_df$code, Year = year_min, Cumulative_thousands = 0),
    aes(Year, Cumulative_thousands), inherit.aes = FALSE
  ) +
  facet_geo(~code, grid = grid_df, label = "code", scales = "free_y") +
  scale_fill_manual(
    values = c("ACCII" = "#4F81BD", "Repeal" = "#C0504D"),
    breaks = c("ACCII", "Repeal"), labels = scenario_labels
  ) +
  scale_color_manual(
    values = c("ACCII" = "#4F81BD", "Repeal" = "#C0504D"),
    breaks = c("ACCII", "Repeal"), labels = scenario_labels
  ) +
  scale_x_continuous(
    limits = c(year_min, year_max), breaks = c(2020, 2030, 2040, 2050),
    labels = c("20", "30", "40", "50"), expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "North America Cumulative Battery Retirement",
    y = "Cumulative retired batteries (thousand)", fill = "Scenario", color = "Scenario"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 17, face = "bold"),
    legend.key.width = unit(2, "cm"),
    strip.background = element_rect(fill = "grey95", color = "grey80", linewidth = 0.3),
    strip.text = element_text(size = 17, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
    axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.spacing = unit(0.25, "lines")
  )

output <- file.path(out_dir, "Fig07_North_American_Cumulative_Battery_Retirements.png")
ggsave(output, figure, width = 20, height = 15, dpi = 300, bg = "white")
cat(output, "\n")
