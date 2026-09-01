## HDV Cumulative Battery Retirement (LIB Recycling + BESS Retirement)
## North America: United States, Canada, Mexico (by state/province)
## Medium trucks + Heavy trucks combined
## 2020-2050, ACCII vs Repeal

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)

YEAR_MIN <- 2020
YEAR_MAX <- 2050

OUT_W   <- 14
OUT_H   <- 5
OUT_DPI <- 600

theme_academic <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.title     = element_text(size = 14, face = "bold"),
      legend.text      = element_text(size = 13, face = "bold"),
      legend.key.width = unit(2, "cm"),
      strip.background = element_rect(fill = "grey95", color = "grey80", linewidth = 0.3),
      strip.text       = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      axis.title.x     = element_blank(),
      axis.title.y     = element_text(size = 16, face = "bold"),
      axis.text.x      = element_text(size = 12, face = "bold"),
      axis.text.y      = element_text(size = 12, face = "bold"),
      axis.ticks       = element_line(linewidth = 0.40),
      panel.spacing    = unit(1, "lines")
    )
}

y_lab_k <- function(x) {
  ifelse(x >= 1000, paste0(x / 1000, "k"), as.character(as.integer(round(x))))
}

load_hdv_retire <- function(turnover_file, bess_file, scenario_name) {
  hdv_rec <- read_csv(turnover_file, show_col_types = FALSE) %>%
    group_by(Country, Year) %>%
    summarise(LIB_recycling = sum(coalesce(LIB_recycling, 0), na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scenario_name)

  hdv_bess <- read_csv(bess_file, show_col_types = FALSE) %>%
    mutate(Scenario = scenario_name) %>%
    group_by(Country, Year) %>%
    summarise(BESS_retire = sum(coalesce(BESS_retire_total, 0), na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scenario_name)

  hdv_rec %>%
    left_join(hdv_bess %>% select(-Scenario), by = c("Country", "Year")) %>%
    mutate(Annual_ret = coalesce(LIB_recycling, 0) + coalesce(BESS_retire, 0)) %>%
    select(Country, Year, Scenario, Annual_ret)
}

hdv_all <- bind_rows(
  load_hdv_retire("Outputs/HDV/HDV_EV_Turnover_ACCII.csv",
                  "Outputs/HDV/HDV_BESS_Retire_ACCII.csv", "ACCII"),
  load_hdv_retire("Outputs/HDV/HDV_EV_Turnover_Repeal.csv",
                  "Outputs/HDV/HDV_BESS_Retire_Repeal.csv", "Repeal")
) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  arrange(Country, Scenario, Year) %>%
  group_by(Country, Scenario) %>%
  mutate(Cumulative_thousands = cumsum(Annual_ret) / 1000) %>%
  ungroup() %>%
  mutate(Country = factor(Country, levels = c("United States", "Canada", "Mexico")))

scenario_colors <- c(
  "ACCII"  = "#4F81BD",
  "Repeal" = "#C0504D"
)

p <- ggplot(
  hdv_all,
  aes(x = Year, y = Cumulative_thousands, fill = Scenario)
) +
  geom_area(alpha = 0.35, position = "identity", na.rm = TRUE) +
  geom_line(aes(color = Scenario), linewidth = 0.7, alpha = 0.9, na.rm = TRUE) +
  facet_wrap(~ Country, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = scenario_colors, breaks = c("ACCII", "Repeal")) +
  scale_color_manual(values = scenario_colors, breaks = c("ACCII", "Repeal")) +
  scale_x_continuous(
    limits = c(YEAR_MIN, YEAR_MAX),
    breaks = c(2020, 2030, 2040, 2050),
    labels = c("20", "30", "40", "50"),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "North America HDV Cumulative Battery Retirement",
    y     = "Cumulative retired batteries (thousand)",
    fill  = "Scenario",
    color = "Scenario"
  ) +
  theme_academic(14)

ggsave("Outputs/NorthAmerica_HDV_Cumulative_BatteryRetirement.png", p,
       width = OUT_W, height = OUT_H, dpi = OUT_DPI)

print(p)
cat("\nSaved: Outputs/NorthAmerica_HDV_Cumulative_BatteryRetirement.png\n")
