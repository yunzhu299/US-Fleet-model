#!/usr/bin/env Rscript
# Figure 4: annual North American new vehicle sales by powertrain.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(ggplot2)
  library(geofacet)
  library(scales)
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
plot_type <- "NewSales"
value_prefix <- "add_"
figure_title <- "North America Annual New Vehicle Sales by Powertrain"
y_title <- "New sales (thousand vehicles)"
output_name <- "Fig04_North_American_New_Vehicle_Sales.png"
y_max_non_mx <- 2000
y_max_mx <- 4000

load_country <- function(country_dir, scenario, country) {
  filename <- paste0("ClosedLoop_StateTotals_", scenario, ".csv")
  path <- if (nzchar(country_dir)) {
    file.path(root, "Outputs", country_dir, filename)
  } else {
    file.path(root, "Outputs", filename)
  }
  read_csv(path, show_col_types = FALSE) %>%
    filter(between(Year, year_min, year_max)) %>%
    select(State, Year, all_of(paste0(value_prefix, c("BEV", "PHEV", "ICE")))) %>%
    pivot_longer(starts_with(value_prefix), names_to = "Powertrain", values_to = "Value") %>%
    mutate(
      Powertrain = sub(paste0("^", value_prefix), "", Powertrain),
      Scenario = scenario, Country = country
    )
}

us_canada <- bind_rows(
  load_country("", "ACCII", "US"),
  load_country("", "Repeal", "US"),
  load_country("Canada", "ACCII", "CA"),
  load_country("Canada", "Repeal", "CA")
)

mexico_history <- read_excel(file.path(root, "Inputs", "MX_Sales.xlsx"), sheet = 2) %>%
  filter(between(Year, year_min, 2025)) %>%
  transmute(
    Year, add_BEV = BEV_units, add_PHEV = PHEV_units,
    add_ICE = coalesce(ICE_units, `ice table`)
  )

load_mexico <- function(scenario) {
  fleet <- read_csv(
    file.path(root, "Outputs", "Mexico", paste0("Mexico_FleetTurnover_", scenario, ".csv")),
    show_col_types = FALSE
  )
  mexico_data <- if (plot_type == "NewSales") {
    bind_rows(
      mexico_history,
      fleet %>% filter(between(Year, 2026, year_max)) %>%
        select(Year, add_BEV, add_PHEV, add_ICE)
    )
  } else {
    fleet %>%
      filter(between(Year, year_min, year_max)) %>%
      select(Year, ret_BEV, ret_PHEV, ret_ICE)
  }
  mexico_data %>%
    pivot_longer(starts_with(value_prefix), names_to = "Powertrain", values_to = "Value") %>%
    mutate(
      Powertrain = sub(paste0("^", value_prefix), "", Powertrain),
      State = "Mexico", Scenario = scenario, Country = "MX"
    )
}

all_data <- bind_rows(us_canada, load_mexico("ACCII"), load_mexico("Repeal")) %>%
  mutate(Value_thousands = coalesce(Value, 0) / 1000)

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

plot_data <- all_data %>%
  left_join(state_codes, by = c("State" = "name")) %>%
  mutate(code = coalesce(code, State), Group = interaction(Powertrain, Scenario)) %>%
  filter(code %in% grid_df$code)

write_csv(
  plot_data,
  file.path(data_dir, "Fig04_North_American_New_Vehicle_Sales.csv")
)

anchors <- data.frame(
  code = grid_df$code, Year = year_max,
  Value_thousands = ifelse(grid_df$code == "MX", y_max_mx, y_max_non_mx)
)

scenario_labels <- c("ACCII" = "Policy Baseline", "Repeal" = "Policy Rollback")
powertrain_colors <- c("BEV" = "#2E8B57", "PHEV" = "#4169E1", "ICE" = "#DC143C")
y_lab_k <- function(x) ifelse(x >= 1000, paste0(x / 1000, "k"), as.character(round(x)))

figure <- ggplot(
  plot_data,
  aes(Year, Value_thousands, color = Powertrain, linetype = Scenario, group = Group)
) +
  geom_line(linewidth = 0.75, alpha = 0.9, na.rm = TRUE) +
  geom_blank(data = anchors, aes(Year, Value_thousands), inherit.aes = FALSE) +
  geom_blank(
    data = data.frame(code = grid_df$code, Year = year_min, Value_thousands = 0),
    aes(Year, Value_thousands), inherit.aes = FALSE
  ) +
  facet_geo(~code, grid = grid_df, label = "code", scales = "free_y") +
  scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
  scale_linetype_manual(
    values = c("ACCII" = "solid", "Repeal" = "dashed"),
    breaks = c("ACCII", "Repeal"), labels = scenario_labels
  ) +
  scale_x_continuous(
    limits = c(year_min, year_max), breaks = c(2020, 2030, 2040, 2050),
    labels = c("20", "30", "40", "50"), expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
  labs(title = figure_title, y = y_title, color = "Powertrain", linetype = "Scenario") +
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

output <- file.path(out_dir, output_name)
ggsave(output, figure, width = 20, height = 15, dpi = 300, bg = "white")
cat(output, "\n")
