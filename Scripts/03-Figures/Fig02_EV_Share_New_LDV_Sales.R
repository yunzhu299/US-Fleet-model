#!/usr/bin/env Rscript
# Figure 2: EV share of new light-duty vehicle sales under both policy scenarios.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
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

section177_states <- c(
  "California", "Colorado", "Connecticut", "Delaware", "Maine", "Maryland",
  "Massachusetts", "New Jersey", "New Mexico", "New York", "Oregon",
  "Rhode Island", "Vermont", "Washington", "Pennsylvania", "Nevada",
  "Minnesota", "Virginia", "District of Columbia"
)

ev_share_df <- function(data, scenario_label) {
  data %>%
    mutate(
      State = str_trim(State), Year = as.integer(Year),
      Total_add = pmax(0, add_BEV + add_PHEV + add_ICE),
      EV_share = if_else(Total_add > 0, (add_BEV + add_PHEV) / Total_add, 0),
      Group = if_else(State %in% section177_states, "Section 177", "Non-177"),
      Scenario = scenario_label
    ) %>%
    select(State, Year, EV_share, Group, Scenario)
}

df_base <- read_csv(
  file.path(root, "Outputs", "ClosedLoop_StateTotals_ACCII.csv"),
  show_col_types = FALSE
) %>% ev_share_df("PolicyBaseline")
df_roll <- read_csv(
  file.path(root, "Outputs", "ClosedLoop_StateTotals_Repeal.csv"),
  show_col_types = FALSE
) %>% ev_share_df("PolicyRollback")

write_csv(
  bind_rows(df_base, df_roll),
  file.path(data_dir, "Fig02_EV_Share_New_LDV_Sales.csv")
)

theme_pub <- theme_bw(base_size = 21) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 19, face = "bold", color = "black"),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.35),
    plot.margin = margin(t = 12, r = 20, b = 12, l = 20)
  )

avg_colors <- c(
  "PolicyBaseline-Section 177" = "#0050A4",
  "PolicyBaseline-Non-177" = "#7FB3D5",
  "PolicyRollback-Section 177" = "#D55E00",
  "PolicyRollback-Non-177" = "#FDB863"
)

make_panel <- function(data, scenario, group_label) {
  panel_data <- data %>% filter(Scenario == scenario, Group == group_label)
  mean_data <- panel_data %>%
    group_by(Year) %>%
    summarise(EV_share = mean(EV_share, na.rm = TRUE), .groups = "drop") %>%
    mutate(Key = paste0(scenario, "-", group_label))

  ggplot() +
    geom_line(
      data = panel_data, aes(Year, EV_share, group = State),
      linewidth = 0.6, color = "gray65", alpha = 0.6
    ) +
    geom_line(data = mean_data, aes(Year, EV_share, color = Key), linewidth = 2) +
    scale_color_manual(values = avg_colors, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1), limits = c(0, 1),
      expand = expansion(mult = c(0.02, 0.03))
    ) +
    labs(title = paste0(scenario, " - ", group_label), x = "Year", y = "EV New Sales Share") +
    theme_pub
}

figure <- (
  make_panel(df_base, "PolicyBaseline", "Section 177") |
    make_panel(df_base, "PolicyBaseline", "Non-177")
) / (
  make_panel(df_roll, "PolicyRollback", "Section 177") |
    make_panel(df_roll, "PolicyRollback", "Non-177")
) +
  plot_annotation(
    title = "EV Share of New Light-Duty Vehicle Sales under PolicyBaseline and PolicyRollback Scenarios",
    subtitle = "Gray lines represent individual states, while bold colored lines represent the group mean.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 26, hjust = 0),
      plot.subtitle = element_text(size = 18, hjust = 0.01, color = "gray30")
    )
  )

output <- file.path(out_dir, "Fig02_EV_Share_New_LDV_Sales.png")
ggsave(output, figure, width = 11.69, height = 8.27, dpi = 450, bg = "white")
cat(output, "\n")
