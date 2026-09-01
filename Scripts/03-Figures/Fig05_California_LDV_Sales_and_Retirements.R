#!/usr/bin/env Rscript
# Figure 5: California annual LDV sales and retirements by powertrain.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(patchwork)
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
scenario_labels <- c("ACCII" = "Policy Baseline", "Repeal" = "Policy Rollback")
powertrain_colors <- c("BEV" = "#2E8B57", "PHEV" = "#4169E1", "ICE" = "#DC143C")

load_scenario <- function(filename, scenario) {
  read_csv(file.path(root, "Outputs", filename), show_col_types = FALSE) %>%
    filter(State == "California", between(Year, year_min, year_max)) %>%
    mutate(Scenario = scenario)
}

ca_raw <- bind_rows(
  load_scenario("ClosedLoop_StateTotals_ACCII.csv", "ACCII"),
  load_scenario("ClosedLoop_StateTotals_Repeal.csv", "Repeal")
)

make_long <- function(prefix) {
  columns <- paste0(prefix, c("BEV", "PHEV", "ICE"))
  ca_raw %>%
    select(Year, Scenario, all_of(columns)) %>%
    pivot_longer(all_of(columns), names_to = "Powertrain", values_to = "Value") %>%
    mutate(
      Powertrain = sub(paste0("^", prefix), "", Powertrain),
      Value_thousands = Value / 1000,
      Group = paste(Powertrain, Scenario, sep = "_")
    )
}

theme_ca <- function() {
  theme_bw(base_size = 16) +
    theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      legend.position = "bottom", legend.box = "vertical", legend.box.just = "left",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14, face = "bold"),
      legend.key.width = unit(2.8, "cm"), legend.key.height = unit(0.9, "cm"),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      plot.margin = margin(t = 10, r = 15, b = 18, l = 15)
    )
}

make_plot <- function(data, title, y_label) {
  y_lab_k <- function(x) {
    ifelse(x >= 1000, paste0(format(x / 1000, trim = TRUE), "k"), as.character(round(x)))
  }
  ggplot(data, aes(Year, Value_thousands, color = Powertrain,
                   linetype = Scenario, group = Group)) +
    geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) +
    scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
    scale_linetype_manual(
      values = c("ACCII" = "solid", "Repeal" = "dashed"),
      breaks = c("ACCII", "Repeal"), labels = scenario_labels
    ) +
    scale_x_continuous(
      limits = c(year_min, year_max), breaks = seq(2020, 2050, 5),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
    guides(
      color = guide_legend(
        order = 1, nrow = 1, byrow = TRUE, title.position = "left",
        override.aes = list(linetype = "solid", linewidth = 1.2)
      ),
      linetype = guide_legend(
        order = 2, nrow = 1, byrow = TRUE, title.position = "left",
        keywidth = unit(2.8, "cm"),
        override.aes = list(color = "black", linewidth = 1.2)
      )
    ) +
    labs(title = title, x = "Year", y = y_label,
         color = "Powertrain", linetype = "Scenario") +
    theme_ca()
}

sales_data <- make_long("add_") %>% mutate(Flow = "New sales")
retirement_data <- make_long("ret_") %>% mutate(Flow = "Retirements")
write_csv(
  bind_rows(sales_data, retirement_data),
  file.path(data_dir, "Fig05_California_LDV_Sales_and_Retirements.csv")
)

p_sales <- make_plot(
  sales_data, "California Annual New LDV Sales Projection by Powertrain",
  "New sales (thousand vehicles)"
)
p_retire <- make_plot(
  retirement_data, "California Annual LDV Retirement Projection by Powertrain",
  "Retirements (thousand vehicles)"
)

tag_theme <- theme(
  plot.tag = element_text(size = 22, face = "bold"),
  plot.tag.position = c(0.015, 0.98)
)

figure <- (
  p_sales + labs(tag = "(a)") + theme(legend.position = "none") + tag_theme
) / (
  p_retire + labs(tag = "(b)") + tag_theme
) +
  plot_layout(heights = c(1, 1), guides = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical", legend.box.just = "left")

output <- file.path(out_dir, "Fig05_California_LDV_Sales_and_Retirements.png")
ggsave(output, figure, width = 13, height = 16, dpi = 300, bg = "white")
cat(output, "\n")
