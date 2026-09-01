## LDV new sales — national totals for US, Canada, Mexico + North America (US+CA+MX)
## Source: ClosedLoop_StateTotals (same as state-level plots), summed by country;
## North America = sum of the three national series per Year × Scenario × Powertrain.
## Outputs/ LDV_National_Sales_Projection/:
##   LDV_National_Sales_Projection.png — by country
##   LDV_North_America_Total_Sales_Projection.png — regional aggregate
##   LDV_National_Sales_Projection.csv   — long table including Country = North America
## Run from project root or: Rscript Scripts/Visualization/LDV_National_Sales_Projection.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
})

## Resolve project root (getwd or walk up to folder that has Outputs/ClosedLoop...)
find_project_root <- function() {
  for (d in c(getwd(), normalizePath("..", winslash = "/"),
              normalizePath("../..", winslash = "/"),
              normalizePath("../../..", winslash = "/"))) {
    if (file.exists(file.path(d, "Outputs", "ClosedLoop_StateTotals_ACCII.csv")))
      return(d)
  }
  if (file.exists("Outputs/ClosedLoop_StateTotals_ACCII.csv")) return(getwd())
  NULL
}
BASE_DIR <- find_project_root()
if (is.null(BASE_DIR) || !dir.exists(file.path(BASE_DIR, "Outputs"))) {
  stop("Could not find fleet model Outputs/ (e.g. ClosedLoop_StateTotals_ACCII.csv).\n",
       "  Set working directory to the project root, then re-run.")
}
OUTPUT_DIR <- file.path(BASE_DIR, "Outputs")
PLOT_DIR   <- file.path(OUTPUT_DIR, "LDV_National_Sales_Projection")
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

YEAR_MIN <- 2020L
YEAR_MAX <- 2050L
OUT_DPI  <- 300L

## Fleet scenarios (must match file suffix: _ACCII.csv, _Repeal.csv)
FLEET_SCENARIOS <- c("ACCII", "Repeal")

powertrain_colors <- c(
  "BEV"  = "#2E8B57",
  "PHEV" = "#4169E1",
  "ICE"  = "#DC143C"
)
scenario_linetypes <- c("ACCII" = "solid", "Repeal" = "dashed")

y_lab_k <- function(x) {
  ifelse(x >= 1000, paste0(x / 1000, "k"), as.character(as.integer(round(x))))
}

## --- load one scenario: national annual LDV new sales (thousand) ----------------
national_from_closedloop <- function(path_us, path_ca, path_mx, scen) {
  read_sum <- function(path, country) {
    if (!file.exists(path)) {
      message("  [skip missing] ", path)
      return(NULL)
    }
    readr::read_csv(path, show_col_types = FALSE) %>%
      filter(Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
      group_by(Year) %>%
      summarise(
        BEV  = sum(add_BEV,  na.rm = TRUE),
        PHEV = sum(add_PHEV, na.rm = TRUE),
        ICE  = sum(add_ICE,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Country = country, Scenario = scen)
  }

  bind_rows(
    read_sum(path_us, "United States"),
    read_sum(path_ca, "Canada"),
    read_sum(path_mx, "Mexico")
  ) %>%
    pivot_longer(
      c(BEV, PHEV, ICE),
      names_to  = "Powertrain",
      values_to = "Sales_thousand"
    ) %>%
    mutate(
      Sales_thousand = coalesce(Sales_thousand, 0) / 1000,
      Group = paste(Powertrain, Scenario, sep = " | ")
    )
}

sfx <- function(scen) paste0("_", scen, ".csv")
paths <- function(scen) {
  list(
    us = file.path(OUTPUT_DIR,            paste0("ClosedLoop_StateTotals", sfx(scen))),
    ca = file.path(OUTPUT_DIR, "Canada",  paste0("ClosedLoop_StateTotals", sfx(scen))),
    mx = file.path(OUTPUT_DIR, "Mexico",  paste0("ClosedLoop_StateTotals", sfx(scen)))
  )
}

nat_long <- lapply(FLEET_SCENARIOS, function(scen) {
  p <- paths(scen)
  national_from_closedloop(p$us, p$ca, p$mx, scen)
}) %>% bind_rows()

if (nrow(nat_long) == 0) {
  stop("No data loaded. Check Outputs/ClosedLoop_StateTotals_*.csv exist.")
}

## North America = US + Canada + Mexico (per Year × Scenario × Powertrain)
na_total <- nat_long %>%
  group_by(Year, Scenario, Powertrain) %>%
  summarise(Sales_thousand = sum(Sales_thousand, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Country = "North America",
    Group   = paste(Powertrain, Scenario, sep = " | ")
  )

## Country order for facets (three countries only; NA is separate figure)
nat_long$Country <- factor(
  nat_long$Country,
  levels = c("United States", "Canada", "Mexico")
)

p_national <- ggplot(
  nat_long,
  aes(
    x = Year,
    y = Sales_thousand,
    color = Powertrain,
    linetype = Scenario,
    group = Group
  )
) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  facet_wrap(~ Country, scales = "free_y", nrow = 1) +
  scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
  scale_linetype_manual(values = scenario_linetypes, breaks = names(scenario_linetypes)) +
  scale_x_continuous(
    limits = c(YEAR_MIN, YEAR_MAX),
    breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050)
  ) +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "North America: projected LDV new sales by country",
    x = "Year",
    y = "New sales (thousand vehicles)",
    color = "Powertrain",
    linetype = "Scenario"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title     = element_text(face = "bold", hjust = 0.5, size = 15),
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    strip.text       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

out_png <- file.path(PLOT_DIR, "LDV_National_Sales_Projection.png")
ggsave(out_png, p_national, width = 12, height = 5, dpi = OUT_DPI, bg = "white")
message("Saved: ", out_png)

p_north_america <- ggplot(
  na_total,
  aes(
    x = Year,
    y = Sales_thousand,
    color = Powertrain,
    linetype = Scenario,
    group = Group
  )
) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_color_manual(values = powertrain_colors, breaks = names(powertrain_colors)) +
  scale_linetype_manual(values = scenario_linetypes, breaks = names(scenario_linetypes)) +
  scale_x_continuous(
    limits = c(YEAR_MIN, YEAR_MAX),
    breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050)
  ) +
  scale_y_continuous(labels = y_lab_k, expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "North America: aggregate projected LDV new sales",
    x = "Year",
    y = "New sales (thousand vehicles)",
    color = "Powertrain",
    linetype = "Scenario"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title     = element_text(face = "bold", hjust = 0.5, size = 15),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

out_png_na <- file.path(PLOT_DIR, "LDV_North_America_Total_Sales_Projection.png")
ggsave(out_png_na, p_north_america, width = 8, height = 5, dpi = OUT_DPI, bg = "white")
message("Saved: ", out_png_na)

## CSV: country rows + North America total rows
out_csv <- file.path(PLOT_DIR, "LDV_National_Sales_Projection.csv")
bind_rows(
  nat_long %>% select(Country, Year, Scenario, Powertrain, Sales_thousand),
  na_total %>% select(Country, Year, Scenario, Powertrain, Sales_thousand)
) %>%
  mutate(Country = factor(Country, levels = c("North America", "United States", "Canada", "Mexico"))) %>%
  arrange(Country, Year, Scenario, Powertrain) %>%
  write_csv(out_csv)
message("Saved: ", out_csv)
