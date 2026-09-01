## ====================================================================
## 03-Recycling_R_Plots_and_Exports.R
## Visualization & Plotting for Battery Demand, Manufacturing,
## and Recycling Tonnage (North America)
##
## Sources 02-Recycling_Analysis.R (which sources 01-Recycling_Data_Preparation.R)
##
## Expects the following objects from upstream scripts:
##   Mass_2050_projected_ref  – geofacet-ready long data (Origin x State)
##   Mass_2050_projected      – wide state-level demand/manu/recycle 2050
##   NA_demand_tonnes         – national yearly demand tonnes
##   NA_manu                  – national yearly manufacturing tonnes
##   NA_batts                 – national yearly EOL battery mass
##   NA_recycling_tonnes      – national yearly recycling capacity
##   ca_us_prov_state_grid1   – geofacet grid (from geofacet package)
##
## Optional objects (Section 4 only — crosswalk export):
##   Naatbatt_Gwh             – NAATBatt facility GWh lookup (Company)
##                              If missing, Section 4 is skipped.
##
## Data files read (from Inputs/):
##   total_manufacturing_edited.csv
##   Ontario_Naatbatt.csv
##
## Working directory should be the project root (Fleet model/)
## ====================================================================

## --- Fleet turnover scenario (set BEFORE sourcing 02) ------------------
## 01-Recycling_Data_Preparation.R already loads ACCII and Repeal together; you do
## NOT need to re-run 01 alone for Repeal. Set FLEET_SCENARIO in the
## calling environment before source() if needed; otherwise default ACCII.
if (!exists("FLEET_SCENARIO") || is.null(FLEET_SCENARIO)) {
  FLEET_SCENARIO <- "ACCII"   # "ACCII" or "Repeal"
}

## --- 0. Source Upstream ------------------------------------------------

required_upstream_objects <- c(
  "Mass_2050_projected",
  "Mass_2050_projected_ref",
  "NA_manu",
  "state_mass_recycle_batt",
  "recycling_tonnes_by_state",
  "needed_cap_long",
  "ratio_results",
  "OUTPUT_DIR"
)
missing_upstream_objects <- required_upstream_objects[
  !vapply(
    required_upstream_objects,
    function(obj) exists(obj, envir = environment(), inherits = TRUE),
    logical(1)
  )
]
if (length(missing_upstream_objects) > 0) {
  source(file.path("Scripts", "02-Recycling_Analysis", "02-Recycling_Analysis.R"))
} else {
  message("Using existing 02-Recycling_Analysis.R objects for plotting.")
}

has_ggpattern <- requireNamespace("ggpattern", quietly = TRUE)
if (!has_ggpattern) {
  message("Note: package 'ggpattern' is not installed. Section 2d will use facet_wrap instead of patterned fills. ",
          "Optional: install.packages(\"ggpattern\")")
}
suppressPackageStartupMessages({
  if (has_ggpattern) library(ggpattern)
  library(geofacet)
  library(scales)
})

## --- Optional parity override from main runner ------------------------
## If main-runner parity files are present, use them so consolidated values
## match main outputs exactly for shared figures.
USE_MAIN_PARITY <- !isTRUE(get0("DISABLE_PARITY_OVERRIDE", ifnotfound = FALSE))
PARITY_DIR_MAIN <- file.path(OUTPUT_DIR, "Recycling_Plots_main", FLEET_SCENARIO, "_parity")
parity_mass_file <- file.path(PARITY_DIR_MAIN, "Mass_2050_projected.csv")
parity_nat_long_file <- file.path(PARITY_DIR_MAIN, "Nat_Mass_2050_long.csv")
parity_export_lost_file <- file.path(PARITY_DIR_MAIN, "export_lost.csv")
parity_nat_cap_chem_rec_file <- file.path(PARITY_DIR_MAIN, "NA_cap_chem_rec.csv")
parity_non_recovery_lost_file <- file.path(PARITY_DIR_MAIN, "non_recovery_lost.csv")
parity_needed_cap_long_file <- file.path(PARITY_DIR_MAIN, "needed_cap_long.csv")
parity_ratio_results_file <- file.path(PARITY_DIR_MAIN, "ratio_results.csv")
parity_overall_circularity_file <- file.path(PARITY_DIR_MAIN, "overall_circularity.csv")

if (USE_MAIN_PARITY && file.exists(parity_mass_file)) {
  message("Using main parity data: ", parity_mass_file)
  Mass_2050_projected <- read.csv(parity_mass_file, check.names = FALSE)
}
if (USE_MAIN_PARITY && file.exists(parity_nat_long_file)) {
  message("Using main parity data: ", parity_nat_long_file)
  Nat_Mass_2050_long <- read.csv(parity_nat_long_file, check.names = FALSE)
}
if (USE_MAIN_PARITY && file.exists(parity_export_lost_file)) {
  message("Using main parity data: ", parity_export_lost_file)
  export_lost <- read.csv(parity_export_lost_file, check.names = FALSE) %>%
    mutate(
      Year = as.numeric(Year),
      Scenario = factor(Scenario, levels = legend_order)
    )
}
if (USE_MAIN_PARITY && file.exists(parity_nat_cap_chem_rec_file)) {
  message("Using main parity data: ", parity_nat_cap_chem_rec_file)
  nat_cap_chem_rec <- read.csv(parity_nat_cap_chem_rec_file, check.names = FALSE)
}
if (USE_MAIN_PARITY && file.exists(parity_non_recovery_lost_file)) {
  message("Using main parity data: ", parity_non_recovery_lost_file)
  non_recovery_lost <- read.csv(parity_non_recovery_lost_file, check.names = FALSE)
}
if (USE_MAIN_PARITY && file.exists(parity_needed_cap_long_file)) {
  message("Using main parity data: ", parity_needed_cap_long_file)
  needed_cap_long <- read.csv(parity_needed_cap_long_file, check.names = FALSE)
}
if (USE_MAIN_PARITY && file.exists(parity_ratio_results_file)) {
  message("Using main parity data: ", parity_ratio_results_file)
  ratio_results <- read.csv(parity_ratio_results_file, check.names = FALSE)
}
if (USE_MAIN_PARITY && file.exists(parity_overall_circularity_file)) {
  message("Using main parity data: ", parity_overall_circularity_file)
  overall_circularity <- read.csv(parity_overall_circularity_file, check.names = FALSE)
}

## --- Plot output folder (inherits from 02 if already set) ------------
if (!exists("PLOT_DIR")) {
  PLOT_DIR <- file.path(OUTPUT_DIR, "Recycling_Plots", FLEET_SCENARIO)
  if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)
}
if (!exists("save_plot", mode = "function")) {
  save_plot <- function(p, name, w = 14, h = 9, dpi = 300) {
    path <- file.path(PLOT_DIR, paste0(name, "_", FLEET_SCENARIO, ".png"))
    ggsave(path, plot = p, width = w, height = h, dpi = dpi, bg = "white")
    message("  saved: ", path)
  }
}

region_mapping <- c(
  # US - WEST
  "WA" = "US-West", "OR" = "US-West", "CA" = "US-West", "NV" = "US-West",
  "ID" = "US-West", "HI" = "US-West", "AK" = "US-West",
  # US - MOUNTAIN
  "MT" = "US-Mountain", "WY" = "US-Mountain", "UT" = "US-Mountain",
  "CO" = "US-Mountain", "AZ" = "US-Mountain", "NM" = "US-Mountain",
  # US - MIDWEST
  "OH" = "US-Midwest", "IN" = "US-Midwest", "IL" = "US-Midwest", "MI" = "US-Midwest",
  "WI" = "US-Midwest", "MN" = "US-Midwest", "IA" = "US-Midwest", "MO" = "US-Midwest",
  "ND" = "US-Midwest", "SD" = "US-Midwest", "NE" = "US-Midwest", "KS" = "US-Midwest",
  # US - SOUTH
  "TX" = "US-South", "OK" = "US-South", "AR" = "US-South", "LA" = "US-South",
  "KY" = "US-South", "TN" = "US-South", "MS" = "US-South", "AL" = "US-South",
  # US - EAST
  "ME" = "US-East", "NH" = "US-East", "VT" = "US-East", "MA" = "US-East",
  "RI" = "US-East", "CT" = "US-East", "NY" = "US-East", "NJ" = "US-East",
  "PA" = "US-East", "DE" = "US-East", "MD" = "US-East", "DC" = "US-East",
  "VA" = "US-East", "WV" = "US-East", "NC" = "US-East", "SC" = "US-East",
  "GA" = "US-East", "FL" = "US-East",
  # CANADA - WEST
  "BC" = "Canada-West", "YT" = "Canada-West",
  # CANADA - MOUNTAIN
  "AB" = "Canada-Mountain",
  # CANADA - MIDWEST
  "MB" = "Canada-Midwest", "SK" = "Canada-Midwest",
  # CANADA - EAST
  "ON" = "Canada-East", "QC" = "Canada-East", "NB" = "Canada-East", "NS" = "Canada-East",
  "PE" = "Canada-East", "NL" = "Canada-East", "NT" = "Canada-East", "NU" = "Canada-East",
  # MEXICO
  "MX" = "Mexico"
)

add_plot_geography <- function(df) {
  if ("State_Province" %in% names(df)) {
    df <- df %>%
      mutate(
        State_Province = trimws(as.character(State_Province)),
        State_Province = if_else(State_Province == "SLP", "MX", State_Province),
        Country = case_when(
          State_Province %in% us_codes ~ "US",
          State_Province %in% ca_codes ~ "CA",
          State_Province == "MX" ~ "MX",
          TRUE ~ NA_character_
        ),
        Region = region_mapping[State_Province]
      ) %>%
      filter(!is.na(Country))
  } else if ("Country" %in% names(df)) {
    df <- df %>%
      mutate(
        Country = trimws(as.character(Country)),
        Country = na_if(Country, ""),
        Region = if ("Region" %in% names(.)) trimws(as.character(Region)) else Country,
        Region = coalesce(na_if(Region, ""), Country)
      ) %>%
      filter(!is.na(Country))
  } else if ("Region" %in% names(df)) {
    df <- df %>%
      mutate(
        Region = trimws(as.character(Region)),
        Country = dplyr::case_when(
          startsWith(Region, "US-")     ~ "US",
          startsWith(Region, "Canada-") ~ "CA",
          Region == "Mexico"            ~ "MX",
          TRUE                          ~ "North America"
        )
      )
  } else {
    df <- df %>% mutate(Country = "North America", Region = "North America")
  }
  df
}


## =====================================================================
## SECTION 1:  GEOFACET — State-Level Bar Chart (2050)
## =====================================================================

## --- 1a. Prepare colour palette --------------------------------------

origin_colors <- c(
  "LIB Demand" = "#1b7fb3",
  "Decreasing Batt Cap LIB Demand" = "#6ba8d4",
  "Pack Manufacturing" = "#D77FBF",
  "Decreasing Batt Cap Pack Manufacturing" = "#EEC3DE",
  "Cell Manufacturing" = "#FC8D62",
  "Decreasing Batt Cap Cell Manufacturing" = "#FDD0B5",
  "End of Life Batteries" = "#66A61E",
  "Decreasing Batt Cap End of Life Batteries" = "#C7E9A8",
  "Black Mass" = "#000000",
  "Refining" = "#FFD700"
)

origin_colors <- origin_colors[names(origin_colors) %in%
                                 unique(Mass_2050_projected_ref$Origin)]

## --- 1b. Complete grid codes so every state has a bar ----------------

valid_geo_codes <- setdiff(unique(ca_us_prov_state_grid1$code), "PR")
## Ensure Mexico code is present even if base grid omits it.
valid_geo_codes <- union(valid_geo_codes, "MX")

Mass_2050_projected_ref <- Mass_2050_projected_ref %>%
  mutate(
    State_Province = trimws(State_Province),
    State_Province = if_else(State_Province == "SLP", "MX", State_Province)
  ) %>%
  filter(State_Province %in% valid_geo_codes) %>%
  complete(
    State_Province = valid_geo_codes,
    Origin,
    fill = list(`Metric Tonnes (millions)` = 0)
  ) %>%
  add_plot_geography() %>%
  group_by(State_Province, Origin, Year, Country, Region) %>%
  summarise(`Metric Tonnes (millions)` = sum(`Metric Tonnes (millions)`, na.rm = TRUE),
            .groups = "drop")

## --- 1c. Modify geofacet grid: drop PR, add Mexico -------------------

grid_df <- as_tibble(ca_us_prov_state_grid1) %>%
  filter(code != "PR") %>%
  filter(code != "MX") %>%
  distinct(code, .keep_all = TRUE)

grid_df <- grid_df %>%
  add_row(
    code = "MX",
    name = "Mexico",
    row  = max(grid_df$row, na.rm = TRUE) + 1,
    col  = 4
  )

class(grid_df) <- c("geofacet_grid", "data.frame")
ca_us_prov_state_grid1 <- grid_df

## --- 1d. Geofacet bar chart -----------------------------------------

## Match main 6 (Plotting_Demand_Recycle_Manu.R) sizing exactly.
p_geofacet_2050 <- ggplot(
    Mass_2050_projected_ref,
    aes(x = Origin, y = `Metric Tonnes (millions)`, fill = Origin)
  ) +
  geom_col() +
  facet_geo(~ State_Province, grid = ca_us_prov_state_grid1) +
  scale_fill_manual(values = origin_colors) +
  labs(
    title = "North American Battery Demand, Manufacturing and Recycling Tonnage (2050)",
    y = "Metric Tonnes (millions)",
    x = "Supply Chain Segment (Increasing Battery Capacity and Benchmark Chemistry Projections)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title       = element_text(size = 16, face = "bold"),
    axis.text        = element_text(size = 14),
    strip.text       = element_text(size = 16),
    legend.title     = element_text(size = 14, face = "bold"),
    legend.text      = element_text(size = 14),
    panel.grid.major = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey80", linewidth = 0.4)
  )
print(p_geofacet_2050)
save_plot(p_geofacet_2050, "01_Geofacet_StateMass_2050", w = 18, h = 12)


## =====================================================================
## SECTION 1b:  GEOFACET — Region-Level Bar Chart (2050)
## =====================================================================
## Aggregates the state-level Mass_2050_projected_ref into 10 regions
## (US: West / Mountain / Midwest / South / East;
##  Canada: West / Mountain / Midwest / East;  + Mexico).

region_levels <- c(
  "US-West", "US-Mountain", "US-Midwest", "US-South", "US-East",
  "Canada-West", "Canada-Mountain", "Canada-Midwest", "Canada-East", "Mexico"
)

region_grid <- data.frame(
  code = region_levels,
  name = c("US West", "US Mountain", "US Midwest", "US South", "US East",
           "Canada West", "Canada Mountain", "Canada Midwest", "Canada East", "Mexico"),
  col  = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
  row  = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
)

if (!("Region" %in% names(Mass_2050_projected_ref))) {
  Mass_2050_projected_ref <- add_plot_geography(Mass_2050_projected_ref)
}

Mass_2050_region_ref <- Mass_2050_projected_ref %>%
  mutate(
    Region = case_when(
      Country == "MX" ~ "Mexico",
      TRUE ~ as.character(Region)
    )
  ) %>%
  filter(!is.na(Region), Region != "") %>%
  group_by(Region, Origin) %>%
  summarise(`Metric Tonnes (millions)` = sum(`Metric Tonnes (millions)`, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Region = factor(Region, levels = region_levels))

p_geofacet_region_2050 <- ggplot(
  Mass_2050_region_ref,
  aes(x = Origin, y = `Metric Tonnes (millions)`, fill = Origin)
) +
  geom_col() +
  facet_geo(~ Region, grid = region_grid) +
  scale_fill_manual(values = origin_colors) +
  labs(
    title = "North American Battery Demand, Manufacturing and Recycling Tonnage by Region (2050)",
    y = "Metric Tonnes (millions)",
    x = "Supply Chain Segment (Increasing Battery Capacity and Benchmark Chemistry Projections)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    legend.position = "bottom",
    plot.title     = element_text(size = 24, face = "bold", hjust = 0.5),
    axis.title     = element_text(size = 20, face = "bold"),
    axis.text      = element_text(size = 20),
    strip.text     = element_text(size = 20),
    legend.title   = element_text(size = 20, face = "bold"),
    legend.text    = element_text(size = 20),
    panel.grid.major = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey80", linewidth = 0.4)
  )
print(p_geofacet_region_2050)
save_plot(p_geofacet_region_2050, "01b_Geofacet_RegionMass_2050", w = 20, h = 12)


## =====================================================================
## SECTION 2:  NATIONAL STACKED BAR — Country Comparison (2050)
## =====================================================================

## --- 2a. Summarise to national level ---------------------------------

Mass_2050_projected <- Mass_2050_projected %>% add_plot_geography()

Nat_Mass_2050 <- Mass_2050_projected %>%
  filter(!is.na(Country), Country != "") %>%
  group_by(Year, Country) %>%
  summarise(
    Add_LIB_proj_tonnes       = sum(Add_LIB_proj_tonnes, na.rm = TRUE),
    Add_LIB_15_tonnes         = sum(Add_LIB_15_tonnes, na.rm = TRUE),
    Tonnes_Prod_proj_down     = sum(Tonnes_Prod_proj_down, na.rm = TRUE),
    Tonnes_Prod_15_down       = sum(Tonnes_Prod_15_down, na.rm = TRUE),
    Tonnes_Prod_proj_mid      = sum(Tonnes_Prod_proj_mid, na.rm = TRUE),
    Tonnes_Prod_15_mid        = sum(Tonnes_Prod_15_mid, na.rm = TRUE),
    Recycle_Batt_Proj         = sum(Recycle_Batt_Proj, na.rm = TRUE),
    Recycle_Batt_15           = sum(Recycle_Batt_15, na.rm = TRUE),
    Cumulative_black_mass_cap = sum(Cumulative_black_mass_cap, na.rm = TRUE),
    Cumulative_refining_cap   = sum(Cumulative_refining_cap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)"     = Add_LIB_proj_tonnes,
    "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)"     = Add_LIB_15_tonnes,
    "Pack Manufacturing"                                         = Tonnes_Prod_proj_down,
    "Decreasing Batt Cap Pack Manufacturing"                     = Tonnes_Prod_15_down,
    "Cell Manufacturing"                                         = Tonnes_Prod_proj_mid,
    "Decreasing Batt Cap Cell Manufacturing"                     = Tonnes_Prod_15_mid,
    "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)"  = Recycle_Batt_Proj,
    "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)"  = Recycle_Batt_15,
    "Black Mass"                                                 = Cumulative_black_mass_cap,
    "Refining"                                                   = Cumulative_refining_cap
  )

## --- 2b. Pivot to long format ----------------------------------------

metric_levels <- c(
  "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)",
  "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)",
  "Pack Manufacturing",
  "Decreasing Batt Cap Pack Manufacturing",
  "Cell Manufacturing",
  "Decreasing Batt Cap Cell Manufacturing",
  "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)",
  "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)",
  "Black Mass",
  "Refining"
)

if (!(exists("Nat_Mass_2050_long", inherits = TRUE) && is.data.frame(Nat_Mass_2050_long))) {
  Nat_Mass_2050_long <- Nat_Mass_2050 %>%
    pivot_longer(
      cols      = -c(Year, Country),
      names_to  = "Metric",
      values_to = "Tonnes"
    ) %>%
    select(-Year) %>%
    mutate(
      Tonnes  = Tonnes / 1e6,
      Metric  = factor(Metric, levels = metric_levels),
      Country = factor(Country, levels = c("CA", "US", "MX"))
    )
} else {
  Nat_Mass_2050_long <- Nat_Mass_2050_long %>%
    mutate(
      Tonnes  = as.numeric(Tonnes),
      Metric  = factor(as.character(Metric), levels = metric_levels),
      Country = factor(as.character(Country), levels = c("CA", "US", "MX"))
    )
}

## --- 2c. Colour + fill palettes for stacked bar ---------------------

bar_fill_colors <- c(
  "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)"   = "#1b7fb3",
  "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)"   = "#6ba8d4",
  "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)"= "#66A61E",
  "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)"= "#C7E9A8",
  "Pack Manufacturing"                                       = "#D77FBF",
  "Decreasing Batt Cap Pack Manufacturing"                   = "#EEC3DE",
  "Cell Manufacturing"                                       = "#FC8D62",
  "Decreasing Batt Cap Cell Manufacturing"                   = "#FDD0B5",
  "Black Mass"                                               = "#808080",
  "Refining"                                                 = "#E6AB02"
)

## --- 2d. National stacked bar (ggpattern if available) ---------------

## Main 6 styling: base_size 20, 24pt bold title, 20pt axes/strip.
nat_mass_bar_theme <- theme_minimal(base_size = 20) +
  theme(
    legend.box       = "vertical",
    legend.box.just  = "left",
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(size = 24, face = "bold", hjust = 0.5),
    axis.title       = element_text(size = 20, face = "bold"),
    axis.text        = element_text(size = 20),
    strip.text       = element_text(size = 20, face = "bold"),
    legend.title     = element_text(size = 20, face = "bold"),
    legend.text      = element_text(size = 15)
  )

nat_mass_bar_labs <- labs(
  x     = "Supply Chain Segment (Battery Capacity - Chemistry Scenario)",
  y     = "Metric Tonnes Batteries (millions)",
  fill  = NULL,
  title = "North American Demand, Manufacturing and Recycling Tonnage by Country (2050)"
)

if (has_ggpattern) {
  p_national_stacked <- ggplot(Nat_Mass_2050_long,
         aes(x = Metric, y = Tonnes, fill = Metric, pattern = Country)) +
    geom_col_pattern(
      position        = "stack",
      color           = "black",
      pattern_density = 0.2,
      pattern_spacing = 0.05,
      pattern_alpha   = 0.3,
      pattern_size    = 0.2,
      pattern_fill    = "black"
    ) +
    scale_pattern_manual(
      values = c("US" = "circle", "CA" = "stripe", "MX" = "crosshatch")
    ) +
    scale_fill_manual(
      values = bar_fill_colors,
      labels = function(x) stringr::str_wrap(x, width = 30)
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(pattern = "none"),
        nrow = 2, byrow = FALSE
      ),
      pattern = guide_legend(
        title = "Country",
        nrow = 1,
        byrow = TRUE,
        override.aes = list(fill = "white", color = "black")
      )
    ) +
    scale_y_continuous(labels = comma) +
    nat_mass_bar_labs +
    nat_mass_bar_theme
} else {
  p_national_stacked <- ggplot(Nat_Mass_2050_long, aes(x = Metric, y = Tonnes, fill = Metric)) +
    geom_col(position = "stack", color = "black") +
    facet_wrap(~ Country, nrow = 1) +
    scale_fill_manual(
      values = bar_fill_colors,
      labels = function(x) stringr::str_wrap(x, width = 30)
    ) +
    guides(fill = guide_legend(title = "Metric", ncol = 2, byrow = FALSE)) +
    scale_y_continuous(labels = comma) +
    nat_mass_bar_labs +
    nat_mass_bar_theme
}
print(p_national_stacked)
save_plot(p_national_stacked, "02_National_Stacked_2050", w = 16, h = 12)


## =====================================================================
## SECTION 3:  NATIONAL TIME SERIES — Demand / Manu / Recycle Over Time
## =====================================================================

## --- 3a. Join national demand, manufacturing, EOL, recycling ----------

NA_plot_data <- NA_demand_tonnes %>%
  full_join(NA_manu, by = "Year") %>%
  full_join(NA_batts, by = "Year") %>%
  full_join(NA_recycling_tonnes, by = "Year") %>%
  select(-Tonnes_Scrap_proj_down, -Tonnes_Scrap_15_down,
         -Tonnes_Scrap_proj_mid, -Tonnes_Scrap_15_mid,
         -Recycle_Batt_15_LFP, -Recycle_Batt_Proj_LFP) %>%
  rename(
    "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)"    = Add_LIB_proj_tonnes,
    "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)"    = Add_LIB_15_tonnes,
    "Pack Manufacturing"                                       = Tonnes_Prod_proj_down,
    "Decreasing Batt Cap Pack Manufacturing"                   = Tonnes_Prod_15_down,
    "Cell Manufacturing"                                       = Tonnes_Prod_proj_mid,
    "Decreasing Batt Cap Cell Manufacturing"                   = Tonnes_Prod_15_mid,
    "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)"= Recycle_Batt_Proj,
    "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)"= Recycle_Batt_15,
    "Black Mass"                                               = Cumulative_black_mass_cap,
    "Refining"                                                 = Cumulative_refining_cap
  ) %>%
  pivot_longer(
    cols      = -c(Year),
    names_to  = "Metric",
    values_to = "Tonnes"
  ) %>%
  mutate(
    Tonnes = Tonnes / 1e6,
    Metric = factor(Metric, levels = metric_levels),
    Year = as.numeric(Year)
  ) %>%
  filter(Year >= 2025)

## --- 3b. Time series line chart --------------------------------------
## Match main 6 styling: pair Increasing (solid) and Decreasing (dashed)
## variants of the same metric on a shared hue, drop legend titles.

ts_color_values <- c(
  "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)"     = "#1b7fb3",
  "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)"     = "#1b7fb3",
  "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)"  = "#66A61E",
  "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)"  = "#66A61E",
  "Pack Manufacturing"                                         = "#D77FBF",
  "Decreasing Batt Cap Pack Manufacturing"                     = "#D77FBF",
  "Cell Manufacturing"                                         = "#FC8D62",
  "Decreasing Batt Cap Cell Manufacturing"                     = "#FC8D62",
  "Black Mass"                                                 = "#808080",
  "Refining"                                                   = "#E6AB02"
)

ts_linetype_values <- c(
  "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)"     = "solid",
  "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)"     = "dashed",
  "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)"  = "solid",
  "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)"  = "dashed",
  "Pack Manufacturing"                                         = "solid",
  "Decreasing Batt Cap Pack Manufacturing"                     = "dashed",
  "Cell Manufacturing"                                         = "solid",
  "Decreasing Batt Cap Cell Manufacturing"                     = "dashed",
  "Black Mass"                                                 = "solid",
  "Refining"                                                   = "solid"
)

p_timeseries <- ggplot(
    NA_plot_data,
    aes(x = Year, y = Tonnes, color = Metric, linetype = Metric)
  ) +
  geom_line(linewidth = 2) +
  scale_color_manual(
    values = ts_color_values,
    labels = function(x) stringr::str_wrap(x, width = 30)
  ) +
  scale_linetype_manual(
    values = ts_linetype_values,
    labels = function(x) stringr::str_wrap(x, width = 30)
  ) +
  guides(
    color    = guide_legend(title = NULL, nrow = 5, ncol = 2, byrow = TRUE),
    linetype = guide_legend(title = NULL)
  ) +
  labs(
    x     = "Year",
    y     = "Metric Tonnes Batteries (millions)",
    title = "North American Demand, Manufacturing and Recycling Tonnage Over Time"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.box       = "vertical",
    legend.position  = "bottom",
    plot.title       = element_text(size = 24, face = "bold", hjust = 0.5),
    axis.title       = element_text(size = 20, face = "bold"),
    axis.text        = element_text(size = 20),
    strip.text       = element_text(size = 20, face = "bold"),
    legend.text      = element_text(size = 14),
    legend.key.width = grid::unit(2.5, "cm")
  )
print(p_timeseries)
save_plot(p_timeseries, "03_National_Timeseries", w = 16, h = 9)

## =====================================================================
## SECTION 3c:  EXPORT PLOT DATA TABLES (EXCEL)
## =====================================================================
## Exports the source data used in plots 01-08 so figures can be rebuilt
## in Excel without re-running transformations manually.

PLOT_DATA_DIR <- file.path(OUTPUT_DIR, "Recycling_Plot_Data", FLEET_SCENARIO)
if (!dir.exists(PLOT_DATA_DIR)) dir.create(PLOT_DATA_DIR, recursive = TRUE)

write_plot_data_xlsx <- function(df, filename) {
  out <- file.path(PLOT_DATA_DIR, paste0(filename, "_", FLEET_SCENARIO, ".xlsx"))
  export_df <- add_plot_geography(df) %>%
    mutate(across(where(is.factor), as.character)) %>%
    relocate(any_of(c("Country", "Region", "State_Province")))
  writexl::write_xlsx(list(data = as.data.frame(export_df)), path = out)
  message("  data: ", out)
}

## Export table units must match the plotted y-values exactly.
plot04_export <- nat_cap_chem_rec %>%
  mutate(Tonne = Tonne / 1000)                        # thousands MT in plot

non_recovery_lost_2035_ref <- non_recovery_lost %>%
  filter(
    as.numeric(Year) == 2035,
    as.character(Scenario) == "Increasing Batt Cap - Benchmark Chemistry"
  ) %>%
  group_by(Mineral) %>%
  summarise(Cum_Tonne = sum(as.numeric(Cum_Tonne), na.rm = TRUE), .groups = "drop") %>%
  filter(Cum_Tonne > 0)

plot05_export <- non_recovery_lost_2035_ref %>%
  mutate(Cum_Tonne = Cum_Tonne / 1000)                # thousands MT in plot

plot07_export <- export_lost %>%
  mutate(Total_Minerals_Exported = Total_Minerals_Exported / 1000)  # thousands MT

plot08_export <- ratio_results %>%
  mutate(Recycle_v_Demand = Recycle_v_Demand * 100)   # percent in plot

write_plot_data_xlsx(Mass_2050_projected_ref, "01_Geofacet_StateMass_2050_data")
write_plot_data_xlsx(Mass_2050_region_ref, "01b_Geofacet_RegionMass_2050_data")
write_plot_data_xlsx(Nat_Mass_2050_long, "02_National_Stacked_2050_data")
write_plot_data_xlsx(NA_plot_data, "03_National_Timeseries_data")
write_plot_data_xlsx(plot04_export, "04_Minerals_Recycled_2050_data")
write_plot_data_xlsx(plot05_export, "05_Cumulative_Minerals_Lost_data")
write_plot_data_xlsx(needed_cap_long, "06_Needed_Recycling_Capacity_data")
write_plot_data_xlsx(plot07_export, "07_Exported_Minerals_data")
write_plot_data_xlsx(plot08_export, "08_Recycled_Content_Ratio_data")
if (exists("overall_circularity")) {
  write_plot_data_xlsx(overall_circularity, "09_Demand_vs_Availability_2050_data")
}

## Re-render plot 04 from the active nat_cap_chem_rec object (parity-aware).
rec_scen_levels_main6 <- c(
  "Recycling Limited to NA 2025 Online or Planned",
  "All Material is Recycled in NA"
)
plot04_plot_df <- nat_cap_chem_rec %>%
  mutate(
    `Recycling Scenario` = forcats::fct_recode(
      as.character(`Recycling Scenario`),
      "Recycling Limited to NA 2025 Online or Planned" =
        "Recycling Limited to NA 2025 Online or Planned Facilities"
    ),
    `Recycling Scenario` = factor(`Recycling Scenario`, levels = rec_scen_levels_main6)
  )
p_minerals_recycled_ref <- ggplot(
    plot04_plot_df,
    aes(x = Year, y = Tonne / 1000,
        color = Scenario,
        linetype = `Recycling Scenario`,
        group = interaction(Scenario, `Recycling Scenario`))
  ) +
  scale_y_sqrt(breaks = scales::pretty_breaks(n = 6)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title = "North America Yearly Recoverable Minerals Until 2050",
    x = "Year",
    y = "Recycled Minerals (thousands Metric Tonnes)",
    color = "Battery Capacity - Chemistry Scenario",
    linetype = "Recycling Scenario"
  ) +
  scale_linetype_manual(values = c(
    "Recycling Limited to NA 2025 Online or Planned" = "solid",
    "All Material is Recycled in NA" = "dashed"
  ), drop = FALSE) +
  scale_color_manual(values = scenario_base_colors) +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title       = element_text(size = 20, face = "bold"),
    axis.text.y      = element_text(size = 14),
    axis.text.x      = element_text(angle = 30, hjust = 1, size = 16),
    strip.text       = element_text(size = 20, face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 20, face = "bold"),
    legend.text      = element_text(size = 15),
    legend.box       = "vertical",
    legend.box.just  = "center"
  ) +
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5,
                         nrow = 2, byrow = TRUE, order = 1),
    linetype = guide_legend(title.position = "top", title.hjust = 0.5,
                            nrow = 2, byrow = TRUE, order = 2,
                            override.aes = list(color = "black"))
  )
save_plot(p_minerals_recycled_ref, "04_Minerals_Recycled_2050", w = 16, h = 10)

## Re-render plot 05 from parity-aware non_recovery_lost.
p_minerals_lost_ref <- ggplot(
    non_recovery_lost_2035_ref,
    aes(x = Mineral, y = Cum_Tonne / 1000, fill = Mineral)
  ) +
  geom_col() +
  labs(
    title = "Cumulative North America Minerals Lost to Lack of Recovery Standards (2035)",
    x = "Mineral",
    y = "Lost Minerals (thousands Metric Tonnes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title  = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title  = element_text(size = 20, face = "bold"),
    axis.text   = element_text(size = 20),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
save_plot(p_minerals_lost_ref, "05_Cumulative_Minerals_Lost", w = 16, h = 10)

## Re-render plot 07 from the active export_lost object (parity-aware) so
## consolidated output remains 1:1 with main when parity CSV is available.
p_exported_minerals_ref <- ggplot(
    export_lost,
    aes(x = Year, y = Total_Minerals_Exported / 1000,
        color = Scenario, group = Scenario)
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = scenario_base_colors) +
  scale_y_sqrt(breaks = scales::pretty_breaks(n = 8)) +
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title = "Exported Mass of Battery Minerals Each Year Under Current NA Recycling Plans",
    x     = "Year",
    y     = "Exported Minerals (thousands of Metric Tonnes)",
    color = "Battery Capacity - Chemistry Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title       = element_text(size = 14, face = "bold"),
    axis.text        = element_text(size = 12),
    axis.text.x      = element_text(angle = 30, hjust = 1),
    strip.text       = element_text(size = 14, face = "bold"),
    legend.box       = "horizontal",
    legend.position  = "bottom",
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 11)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
save_plot(p_exported_minerals_ref, "07_Exported_Minerals", w = 16, h = 10)

cat("Plot data tables saved to:", PLOT_DATA_DIR, "\n")


## =====================================================================
## SECTION 3d:  SINGLE WORKBOOK — UNIFIED EXCEL FORMULA NET
## =====================================================================
## Layout (every formula uses 'SheetName'!$Col$row plain cell ranges,
## NOT structured table refs, per user request):
##
##   README              text overview
##   Bridges             3 mini mapping tables side-by-side
##   State_Master        long: Year, State, Country, Region, Legend, Metric Tonnes
##   Mineral_Master      long: Year, State, Scenario, Mineral, Variable, Metric Tonnes
##   Capacity_Master     long: Year, Scenario, Step, Variable, Value
##   EoL_Chain_Source    long: Country, Year, Battery_Scenario,
##                              Retired_Count, LIB_Recycle_GWh, Batt_Mass_MT
##   Dashboard           ALL formula blocks vertically:
##                         B1  Main Flow   (Year x Scenario x Path)
##                         B2  EoL Chain   (Country x Year x BattScen) — units chain
##                         B3  Fig01       (2050 State x Origin)
##                         B4  Fig02       (2050 Country x Metric)
##                         B5  Fig03       (Year x Metric)
##                         B6  Fig04       (Year x Scen x Mineral x RecScen)
##                         B7  Fig05       (Year x Scen x Mineral, cum)
##                         B8  Fig06       (Year x Scen x Step)
##                         B9  Fig07       (Year x Scen x Mineral)
##                         B10 Fig08       (Year x Scen x Mineral x RecScen, ratio)

prep_workbook_df <- function(df) {
  add_plot_geography(df) %>%
    mutate(across(where(is.factor), as.character)) %>%
    relocate(any_of(c("Country", "Region", "State_Province"))) %>%
    as.data.frame()
}

metric_key_fig02 <- data.frame(
  internal = c(
    "Add_LIB_proj_tonnes", "Add_LIB_15_tonnes",
    "Tonnes_Prod_proj_down", "Tonnes_Prod_15_down",
    "Tonnes_Prod_proj_mid", "Tonnes_Prod_15_mid",
    "Recycle_Batt_Proj", "Recycle_Batt_15",
    "Cumulative_black_mass_cap", "Cumulative_refining_cap"
  ),
  Legend = metric_levels,
  stringsAsFactors = FALSE
)

## --- Fig01: Origin (6) maps to one plot-metric label each (2050 geofacet)
bridge_orig_metric <- data.frame(
  Origin = c(
    "LIB Demand", "Pack Manufacturing", "Cell Manufacturing",
    "End of Life Batteries", "Black Mass", "Refining"
  ),
  Metric = c(
    "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)",
    "Pack Manufacturing",
    "Cell Manufacturing",
    "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)",
    "Black Mass",
    "Refining"
  ),
  stringsAsFactors = FALSE
)

fig01_keys <- Mass_2050_projected_ref %>%
  mutate(
    State_Province = as.character(State_Province),
    Origin = as.character(Origin)
  ) %>%
  distinct(State_Province, Origin)

## --- Fig02 keys: all country x metric combinations (same order as plot)
fig02_keys <- tidyr::expand_grid(
  Country = c("CA", "US", "MX"),
  Metric = metric_levels
) %>%
  mutate(
    Country = as.character(Country),
    Metric = as.character(Metric)
  ) %>%
  as.data.frame()

## --- Fig03: state x year x metric
## Keep full 2025-2050 coverage from all-year sources, and overwrite 2050
## using parity-aware Mass_2050_projected so 2050 aligns with Fig02.
state_na_plot_wide_all_years <- state_cap_chem_tonne %>%
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes) %>%
  full_join(
    manufacturing_by_state_projected %>%
      select(Year, State_Province,
             Tonnes_Prod_proj_down, Tonnes_Prod_15_down,
             Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid),
    by = c("Year", "State_Province")
  ) %>%
  full_join(
    recycling_tonnes_by_state %>%
      select(Year, State_Province,
             Cumulative_black_mass_cap, Cumulative_refining_cap),
    by = c("Year", "State_Province")
  ) %>%
  full_join(
    state_mass_recycle_batt %>%
      filter(Scenario %in% c(
        "Increasing Batt Cap - Benchmark Chemistry",
        "Decreasing Batt Cap - Benchmark Chemistry"
      )) %>%
      transmute(
        Year = as.integer(Year),
        State_Province = as.character(State_Province),
        Scenario = as.character(Scenario),
        Batt_Mass_MT = as.numeric(Batt_Mass_MT)
      ) %>%
      tidyr::pivot_wider(
        id_cols = c(Year, State_Province),
        names_from = Scenario,
        values_from = Batt_Mass_MT,
        values_fill = 0
      ) %>%
      rename(
        Recycle_Batt_Proj = `Increasing Batt Cap - Benchmark Chemistry`,
        Recycle_Batt_15   = `Decreasing Batt Cap - Benchmark Chemistry`
      ),
    by = c("Year", "State_Province")
  ) %>%
  transmute(
    Year = as.integer(Year),
    State_Province = if_else(
      toupper(trimws(as.character(State_Province))) == "SLP",
      "MX",
      toupper(trimws(as.character(State_Province)))
    ),
    Add_LIB_proj_tonnes = as.numeric(Add_LIB_proj_tonnes),
    Add_LIB_15_tonnes = as.numeric(Add_LIB_15_tonnes),
    Tonnes_Prod_proj_down = as.numeric(Tonnes_Prod_proj_down),
    Tonnes_Prod_15_down = as.numeric(Tonnes_Prod_15_down),
    Tonnes_Prod_proj_mid = as.numeric(Tonnes_Prod_proj_mid),
    Tonnes_Prod_15_mid = as.numeric(Tonnes_Prod_15_mid),
    Recycle_Batt_Proj = as.numeric(Recycle_Batt_Proj),
    Recycle_Batt_15 = as.numeric(Recycle_Batt_15),
    Cumulative_black_mass_cap = as.numeric(Cumulative_black_mass_cap),
    Cumulative_refining_cap = as.numeric(Cumulative_refining_cap)
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  group_by(Year, State_Province) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

state_na_plot_wide_2050 <- Mass_2050_projected %>%
  mutate(
    Year = as.integer(Year),
    State_Province = if_else(
      toupper(trimws(as.character(State_Province))) == "SLP",
      "MX",
      toupper(trimws(as.character(State_Province)))
    )
  )

for (nm in metric_key_fig02$internal) {
  if (!(nm %in% names(state_na_plot_wide_2050))) {
    state_na_plot_wide_2050[[nm]] <- 0
  }
}

state_na_plot_wide_2050 <- state_na_plot_wide_2050 %>%
  select(Year, State_Province, all_of(metric_key_fig02$internal)) %>%
  mutate(across(all_of(metric_key_fig02$internal), ~ as.numeric(.x))) %>%
  mutate(across(all_of(metric_key_fig02$internal), ~ replace_na(.x, 0))) %>%
  filter(Year == 2050) %>%
  group_by(Year, State_Province) %>%
  summarise(across(all_of(metric_key_fig02$internal), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

state_na_plot_wide <- bind_rows(
  state_na_plot_wide_all_years %>% filter(Year != 2050),
  state_na_plot_wide_2050
) %>%
  group_by(Year, State_Province) %>%
  summarise(across(all_of(metric_key_fig02$internal), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

state_master_long <- state_na_plot_wide %>%
  select(Year, State_Province, all_of(metric_key_fig02$internal)) %>%
  mutate(Year = as.integer(Year)) %>%
  pivot_longer(
    cols      = all_of(metric_key_fig02$internal),
    names_to  = "internal",
    values_to = "Metric Tonnes"
  ) %>%
  left_join(metric_key_fig02, by = "internal") %>%
  mutate(
    State_Province = trimws(as.character(State_Province)),
    State_Province = if_else(State_Province == "SLP", "MX", State_Province),
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX" ~ "MX",
      TRUE ~ NA_character_
    ),
    Region = region_mapping[State_Province],
    Legend = as.character(Legend),
    `Metric Tonnes` = as.numeric(`Metric Tonnes`)
  ) %>%
  filter(!is.na(Country)) %>%
  select(Year, State_Province, Country, Region, Legend, `Metric Tonnes`) %>%
  as.data.frame()

fig03_keys <- NA_plot_data %>%
  mutate(
    Year = as.integer(Year),
    Metric = as.character(Metric)
  ) %>%
  distinct(Year, Metric) %>%
  arrange(Year, match(Metric, metric_levels))

workbook_path <- file.path(
  PLOT_DATA_DIR,
  paste0("Recycling_Figures_Workbook_", FLEET_SCENARIO, ".xlsx")
)

## ---------------------------------------------------------------------
## DATA PREP — Mineral_Master, Capacity_Master, EoL_Chain, Bridges
## ---------------------------------------------------------------------

cap_chem_vars_keep <- c(
  "Available Recycled Minerals (w Scrap) (Tonne)",
  "Available Recycled Minerals No R Restraint (Tonne)",
  "Minerals Recoverable in Exported Scrap/Batts (Tonne)",
  "Minerals Recoverable in Exported BM (Tonne)",
  "Minerals Lost to Pyrometalurgy (Tonne)"
)

cap_chem_long <- cap_chem_results %>%
  select(Year, State_Province, Scenario, Mineral, all_of(cap_chem_vars_keep)) %>%
  pivot_longer(cols = all_of(cap_chem_vars_keep), names_to = "Variable", values_to = "Value")

cap_chem_demand_long <- cap_chem_demand_results %>%
  transmute(Year, State_Province, Scenario, Mineral,
            Variable = "Demand Minerals (Tonne)",
            Value = `Demand Minerals (Tonne)`)

mineral_master_long <- bind_rows(cap_chem_long, cap_chem_demand_long) %>%
  mutate(
    Year = as.integer(Year),
    State_Province = as.character(State_Province),
    Scenario = as.character(Scenario),
    Mineral = as.character(Mineral),
    Variable = as.character(Variable),
    `Metric Tonnes` = as.numeric(Value)
  ) %>%
  filter(!is.na(Year), !is.na(Mineral), !is.na(`Metric Tonnes`)) %>%
  select(Year, State_Province, Scenario, Mineral, Variable, `Metric Tonnes`) %>%
  as.data.frame()

cap_capacity_part <- US_CA_Recycle %>%
  select(Year, Black_Mass_MT, Full_Recycle, Refining_MT) %>%
  mutate(Year = as.integer(Year)) %>%
  pivot_longer(cols = c(Black_Mass_MT, Full_Recycle, Refining_MT),
               names_to = "Step", values_to = "Value") %>%
  mutate(
    Step = dplyr::recode(Step,
      "Black_Mass_MT" = "Black Mass",
      "Full_Recycle"  = "Refining",
      "Refining_MT"   = "Refining (Raw Nameplate)"),
    Scenario = "ALL", Variable = "Capacity_MT", Value = as.numeric(Value)
  ) %>%
  select(Year, Scenario, Step, Variable, Value)

cap_needed_part <- needed_cap_results %>%
  rename(Step = `Recycling Step`) %>%
  mutate(
    Year = as.integer(Year), Scenario = as.character(Scenario),
    Step = as.character(Step), Variable = "Cumulative_Needed_MT",
    Value = as.numeric(Tonne)
  ) %>%
  select(Year, Scenario, Step, Variable, Value)

nat_scrap_by_battery <- bind_rows(
  batt_scen[["Increasing Batt Cap"]] %>%
    group_by(Year) %>% summarise(Scrap_tonnes = first(Scrap_tonnes), .groups = "drop") %>%
    mutate(Battery_Scenario = "Increasing Batt Cap"),
  batt_scen[["Decreasing Batt Cap"]] %>%
    group_by(Year) %>% summarise(Scrap_tonnes = first(Scrap_tonnes), .groups = "drop") %>%
    mutate(Battery_Scenario = "Decreasing Batt Cap")
) %>%
  mutate(Year = as.integer(Year), Scrap_tonnes = as.numeric(Scrap_tonnes))

nat_batt_mass <- state_mass_recycle_batt %>%
  group_by(Year, Scenario) %>%
  summarise(Batt_Mass_MT = sum(Batt_Mass_MT, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Year = as.integer(Year), Scenario = as.character(Scenario),
    Battery_Scenario = dplyr::case_when(
      grepl("^Increasing Batt Cap", Scenario) ~ "Increasing Batt Cap",
      grepl("^Decreasing Batt Cap", Scenario) ~ "Decreasing Batt Cap",
      TRUE ~ NA_character_
    )
  )

cap_retired_part <- nat_batt_mass %>%
  transmute(Year = as.integer(Year), Scenario = as.character(Scenario),
            Step = "ALL", Variable = "Retired_Battery_MT",
            Value = as.numeric(Batt_Mass_MT))

cap_scrap_part <- nat_batt_mass %>%
  left_join(nat_scrap_by_battery, by = c("Battery_Scenario", "Year")) %>%
  transmute(Year = as.integer(Year), Scenario = as.character(Scenario),
            Step = "ALL", Variable = "Mfg_Scrap_ProcEq_MT",
            Value = as.numeric(Scrap_tonnes / 0.7078558))

capacity_master_long <- bind_rows(
  cap_capacity_part, cap_needed_part, cap_retired_part, cap_scrap_part
) %>%
  filter(!is.na(Year), !is.na(Value)) %>%
  as.data.frame()

bridge_rec_scen <- data.frame(
  Recycling_Scenario = c(
    "Recycling Limited to NA 2025 Online or Planned Facilities",
    "All Material is Recycled in NA"),
  Variable = c(
    "Available Recycled Minerals (w Scrap) (Tonne)",
    "Available Recycled Minerals No R Restraint (Tonne)"),
  stringsAsFactors = FALSE
)

bridge_scen_metrics <- data.frame(
  Scenario = legend_order,
  DemandMetric = c(
    "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)",
    "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)",
    "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)",
    "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)"),
  PackMetric = c(
    "Pack Manufacturing", "Pack Manufacturing",
    "Decreasing Batt Cap Pack Manufacturing", "Decreasing Batt Cap Pack Manufacturing"),
  CellMetric = c(
    "Cell Manufacturing", "Cell Manufacturing",
    "Decreasing Batt Cap Cell Manufacturing", "Decreasing Batt Cap Cell Manufacturing"),
  EOLMetric = c(
    "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)",
    "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)",
    "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)",
    "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)"),
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------------------
## EoL TONNES CHAIN — per (Country × Year × Battery_Scenario)
##   Re-derive (Retired_Count, LIB_Recycle_GWh) from future_recycle_type
##   + battery cap projection so Excel can show:
##     Count × Avg_Cap (kWh/batt) ÷ 1e6 = GWh
##     GWh × Pack_kg/kWh × 1000 / 1000 = MT  (i.e., MT = GWh × kg/kWh)
##   The two implied multipliers (Avg_Cap, Pack_kg/kWh) are computed in
##   Excel as ratios of the 3 raw values.
## ---------------------------------------------------------------------

batt_scen_caps <- list(
  `Increasing Batt Cap - Benchmark Chemistry` = batt_cap_proj_ext,
  `Decreasing Batt Cap - Benchmark Chemistry` = batt_cap_15_ext
)

eol_chain_pieces <- map_dfr(names(batt_scen_caps), function(scen_name) {
  bc <- batt_scen_caps[[scen_name]] %>%
    group_by(State_Province, Segment, Propulsion, Sale_Year) %>%
    summarise(`Projected Avg Batt Cap (kwh/batt)` = first(`Projected Avg Batt Cap (kwh/batt)`),
              .groups = "drop")
  future_recycle_type_collection %>%
    left_join(bc, by = c("State_Province", "Sale_Year", "Segment", "Propulsion")) %>%
    mutate(LIB_recycle_kwh = LIB_recycle_total * `Projected Avg Batt Cap (kwh/batt)`) %>%
    mutate(
      State_Province = if_else(State_Province == "SLP", "MX", State_Province),
      Country = case_when(
        State_Province %in% us_codes ~ "US",
        State_Province %in% ca_codes ~ "CA",
        State_Province == "MX" ~ "MX",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Country)) %>%
    group_by(Country, Year) %>%
    summarise(
      Retired_Count   = sum(LIB_recycle_total, na.rm = TRUE),
      LIB_Recycle_GWh = sum(LIB_recycle_kwh, na.rm = TRUE) / 1e6,
      .groups = "drop"
    ) %>%
    mutate(Battery_Scenario = scen_name)
})

mass_country <- state_mass_recycle_batt %>%
  filter(Scenario %in% names(batt_scen_caps)) %>%
  mutate(
    State_Province = if_else(State_Province == "SLP", "MX", State_Province),
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX" ~ "MX",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Country)) %>%
  group_by(Country, Year, Scenario) %>%
  summarise(Batt_Mass_MT = sum(Batt_Mass_MT, na.rm = TRUE), .groups = "drop") %>%
  rename(Battery_Scenario = Scenario)

eol_chain_country <- eol_chain_pieces %>%
  left_join(mass_country, by = c("Country", "Year", "Battery_Scenario")) %>%
  mutate(
    Year = as.integer(Year),
    Retired_Count   = as.numeric(Retired_Count),
    LIB_Recycle_GWh = as.numeric(LIB_Recycle_GWh),
    Batt_Mass_MT    = as.numeric(Batt_Mass_MT)
  ) %>%
  filter(Year >= 2025) %>%
  arrange(Battery_Scenario, Country, Year) %>%
  select(Country, Year, Battery_Scenario, Retired_Count, LIB_Recycle_GWh, Batt_Mass_MT) %>%
  as.data.frame()


## ---------------------------------------------------------------------
## ROW COUNTS + EXCEL CELL RANGES (used in every formula)
## ---------------------------------------------------------------------

n_state <- nrow(state_master_long)
n_min   <- nrow(mineral_master_long)
n_cap   <- nrow(capacity_master_long)
n_eol   <- nrow(eol_chain_country)
n_brid  <- nrow(bridge_orig_metric)
n_brrs  <- nrow(bridge_rec_scen)
n_brsm  <- nrow(bridge_scen_metrics)

## State_Master cols: A=Year, B=State_Province, C=Country, D=Region, E=Legend, F=Metric Tonnes
sm_year   <- sprintf("'State_Master'!$A$2:$A$%d",  n_state + 1L)
sm_state  <- sprintf("'State_Master'!$B$2:$B$%d",  n_state + 1L)
sm_ctry   <- sprintf("'State_Master'!$C$2:$C$%d",  n_state + 1L)
sm_region <- sprintf("'State_Master'!$D$2:$D$%d",  n_state + 1L)
sm_legend <- sprintf("'State_Master'!$E$2:$E$%d",  n_state + 1L)
sm_tonn   <- sprintf("'State_Master'!$F$2:$F$%d",  n_state + 1L)

## Mineral_Master cols: A=Year, B=State, C=Scenario, D=Mineral, E=Variable, F=Metric Tonnes
mm_year   <- sprintf("'Mineral_Master'!$A$2:$A$%d", n_min + 1L)
mm_state  <- sprintf("'Mineral_Master'!$B$2:$B$%d", n_min + 1L)
mm_scen   <- sprintf("'Mineral_Master'!$C$2:$C$%d", n_min + 1L)
mm_min    <- sprintf("'Mineral_Master'!$D$2:$D$%d", n_min + 1L)
mm_var    <- sprintf("'Mineral_Master'!$E$2:$E$%d", n_min + 1L)
mm_val    <- sprintf("'Mineral_Master'!$F$2:$F$%d", n_min + 1L)

## Capacity_Master cols: A=Year, B=Scenario, C=Step, D=Variable, E=Value
cm_year   <- sprintf("'Capacity_Master'!$A$2:$A$%d", n_cap + 1L)
cm_scen   <- sprintf("'Capacity_Master'!$B$2:$B$%d", n_cap + 1L)
cm_step   <- sprintf("'Capacity_Master'!$C$2:$C$%d", n_cap + 1L)
cm_var    <- sprintf("'Capacity_Master'!$D$2:$D$%d", n_cap + 1L)
cm_val    <- sprintf("'Capacity_Master'!$E$2:$E$%d", n_cap + 1L)

## EoL_Chain_Source cols: A=Country, B=Year, C=Battery_Scenario,
##                         D=Retired_Count, E=LIB_Recycle_GWh, F=Batt_Mass_MT
el_ctry   <- sprintf("'EoL_Chain_Source'!$A$2:$A$%d", n_eol + 1L)
el_year   <- sprintf("'EoL_Chain_Source'!$B$2:$B$%d", n_eol + 1L)
el_scen   <- sprintf("'EoL_Chain_Source'!$C$2:$C$%d", n_eol + 1L)
el_count  <- sprintf("'EoL_Chain_Source'!$D$2:$D$%d", n_eol + 1L)
el_gwh    <- sprintf("'EoL_Chain_Source'!$E$2:$E$%d", n_eol + 1L)
el_mt     <- sprintf("'EoL_Chain_Source'!$F$2:$F$%d", n_eol + 1L)

## Bridges cols: tblBridge      A:B  rows 4..(3+n)
##                tblBridgeRec   D:E  rows 4..(3+n)
##                tblBridgeScen  G:K  rows 4..(3+n)
br_origin <- sprintf("'Bridges'!$A$4:$A$%d", n_brid + 3L)
br_metric <- sprintf("'Bridges'!$B$4:$B$%d", n_brid + 3L)
brs_scen  <- sprintf("'Bridges'!$D$4:$D$%d", n_brrs + 3L)
brs_var   <- sprintf("'Bridges'!$E$4:$E$%d", n_brrs + 3L)
bm_scen   <- sprintf("'Bridges'!$G$4:$G$%d", n_brsm + 3L)
bm_dem    <- sprintf("'Bridges'!$H$4:$H$%d", n_brsm + 3L)
bm_pack   <- sprintf("'Bridges'!$I$4:$I$%d", n_brsm + 3L)
bm_cell   <- sprintf("'Bridges'!$J$4:$J$%d", n_brsm + 3L)
bm_eol    <- sprintf("'Bridges'!$K$4:$K$%d", n_brsm + 3L)


## ---------------------------------------------------------------------
## CREATE WORKBOOK
## ---------------------------------------------------------------------

wb_all <- openxlsx::createWorkbook()

## ---------------------------------------------------------------------
## (mineral_master_long, capacity_master_long, bridge_rec_scen,
##  bridge_scen_metrics, eol_chain_country are all already built in the
##  pre-workbook DATA PREP block above — see the top of Section 3d.)
##
##  We now build the per-figure key tables and lay out the unified
##  Dashboard sheet (10 vertical formula blocks) using plain
##  'SheetName'!$Col$row references (no structured table refs).
## ---------------------------------------------------------------------

## ---------------------------------------------------------------------
## FIG KEY TABLES (rows of each block in Dashboard)
## ---------------------------------------------------------------------

fig04_keys <- nat_cap_chem_rec %>%
  mutate(
    Year = as.integer(Year),
    Scenario = as.character(Scenario),
    Mineral = as.character(Mineral),
    Recycling_Scenario = as.character(`Recycling Scenario`)
  ) %>%
  distinct(Year, Scenario, Mineral, Recycling_Scenario) %>%
  arrange(Mineral, Year, Scenario, Recycling_Scenario) %>%
  as.data.frame()

## Fig05 in plotting is a 2035 mineral bar chart (aggregated across scenarios),
## using Cum_Tonne/1000 from plot05_export.
fig05_keys <- plot05_export %>%
  mutate(Mineral = as.character(Mineral)) %>%
  arrange(Mineral) %>%
  mutate(Sort_Order = row_number()) %>%
  transmute(Mineral, Sort_Order) %>%
  as.data.frame()

fig06_keys <- cap_needed_part %>%
  distinct(Year, Scenario, Step) %>%
  arrange(Year, Scenario, Step) %>%
  as.data.frame()

fig07_keys <- export_lost %>%
  mutate(
    Year = as.integer(Year),
    Scenario = as.character(Scenario),
    Mineral = as.character(Mineral)
  ) %>%
  distinct(Year, Scenario, Mineral) %>%
  arrange(Mineral, Year, Scenario) %>%
  as.data.frame()

fig08_keys <- ratio_results %>%
  mutate(
    Year = as.integer(Year),
    Scenario = as.character(Scenario),
    Mineral = as.character(Mineral),
    Recycling_Scenario = as.character(`Recycling Scenario`)
  ) %>%
  distinct(Year, Scenario, Mineral, Recycling_Scenario) %>%
  arrange(Mineral, Scenario, Year, Recycling_Scenario) %>%
  as.data.frame()

battery_flow_keys <- expand.grid(
  Year = sort(unique(cap_capacity_part$Year)),
  Scenario = legend_order,
  Recycling_Path = c("Black Mass", "Refining"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>%
  mutate(
    Year = as.integer(Year),
    Scenario = as.character(Scenario),
    Recycling_Path = as.character(Recycling_Path)
  ) %>%
  arrange(Year, Scenario, Recycling_Path) %>%
  as.data.frame()


## ---------------------------------------------------------------------
## DASHBOARD ROW POSITIONS — track where each block sits.
##   Each block has: label_row, header_row, first_data, last_data
## ---------------------------------------------------------------------

GAP <- 2L  # blank rows between blocks

dash_cur <- 1L
dash_title_row <- dash_cur
dash_cur <- dash_cur + 2L  # title + 1 blank

mk_block <- function(n_rows) {
  out <- list(
    label  = dash_cur,
    header = dash_cur + 1L,
    first  = dash_cur + 2L,
    last   = dash_cur + 1L + n_rows
  )
  dash_cur <<- out$last + 1L + GAP
  out
}

dash_b1 <- mk_block(nrow(battery_flow_keys))
dash_b2 <- mk_block(nrow(eol_chain_country))
dash_f1 <- mk_block(nrow(fig01_keys))
dash_f2 <- mk_block(nrow(fig02_keys))
dash_f3 <- mk_block(nrow(fig03_keys))
dash_f4 <- mk_block(nrow(fig04_keys))
dash_f5 <- mk_block(nrow(fig05_keys))
dash_f6 <- mk_block(nrow(fig06_keys))
dash_f7 <- mk_block(nrow(fig07_keys))
dash_f8 <- mk_block(nrow(fig08_keys))


## ---------------------------------------------------------------------
## README — relationship overview
## ---------------------------------------------------------------------

readme_lines <- c(
  paste0("Fleet scenario: ", FLEET_SCENARIO),
  "",
  "============================================================",
  "  WORKBOOK STRUCTURE",
  "============================================================",
  "",
  "  Source sheets (read-only data; every Dashboard formula reads from these):",
  "    State_Master       Year, State, Country, Region, Legend, Metric Tonnes",
  "    Mineral_Master     Year, State, Scenario, Mineral, Variable, Metric Tonnes",
  "    Capacity_Master    Year, Scenario, Step, Variable, Value",
  "    EoL_Chain_Source   Country, Year, Battery_Scenario,",
  "                        Retired_Count, LIB_Recycle_GWh, Batt_Mass_MT",
  "    Bridges            three small mapping tables (Origin->Metric, etc.)",
  "",
  "  Dashboard (single sheet) — 10 vertical formula blocks:",
  "    Block 1   Main Flow            Year x Scenario x Path (Demand/Manu/EoL/Cap/Gap)",
  "    Block 2   EoL Tonnes Chain     Country x Year x BattScen — units chain:",
  "                                    Count x Avg_Cap_kWh = GWh x Pack_kg/kWh = MT",
  "    Block 3   Fig01  Geofacet      2050 State x Origin (millions of MT)",
  "    Block 4   Fig02  National      2050 Country x Metric (millions of MT)",
  "    Block 5   Fig03  Time series   Year x Metric (millions of MT)",
  "    Block 6   Fig04  Recycled mins Year x Scen x Mineral x RecScen (Tonne)",
  "    Block 7   Fig05  Lost mins     Year x Scen x Mineral, Annual + Cum (Tonne)",
  "    Block 8   Fig06  Needed cap    Year x Scen x Step (millions of MT)",
  "    Block 9   Fig07  Exported mins Year x Scen x Mineral (Tonne)",
  "    Block 10  Fig08  Recycled ratio Year x Scen x Mineral x RecScen (Year-1 / Year)",
  "",
  "  All formulas use plain 'SheetName'!$Col$row ranges (no structured table",
  "  references). Open Name Manager: there are no defined names.",
  "",
  "============================================================",
  "  FIGURE MEANINGS",
  "============================================================",
  "  01 Geofacet         — 2050 state/province tonnes by supply-chain origin.",
  "  02 National stacked — 2050 state mass aggregated to CA / US / MX.",
  "  03 NA time series   — annual tonnes for demand, manu, EoL, BM, refining.",
  "  04 Minerals recycled — recovery under two recycling-scenario assumptions.",
  "  05 Cumulative lost  — pyrometallurgy losses cumulated per mineral.",
  "  06 Needed capacity  — black mass / refining gap vs flows.",
  "  07 Exported minerals — recoverable mass leaving NA.",
  "  08 Recycled ratio   — last-year recycled / this-year demand."
)


## ---------------------------------------------------------------------
## WRITE README
## ---------------------------------------------------------------------

openxlsx::addWorksheet(wb_all, "README")
openxlsx::writeData(
  wb_all, "README",
  data.frame(Description = readme_lines, stringsAsFactors = FALSE),
  startRow = 1L
)


## ---------------------------------------------------------------------
## WRITE Bridges (3 mini tables side-by-side, plain — no Tables)
## ---------------------------------------------------------------------

openxlsx::addWorksheet(wb_all, "Bridges")
openxlsx::writeData(
  wb_all, "Bridges",
  data.frame(Section = "Mapping tables used by Dashboard formulas",
             stringsAsFactors = FALSE),
  startRow = 1L, startCol = 1L, colNames = FALSE
)
openxlsx::writeData(wb_all, "Bridges", bridge_orig_metric,
                    startRow = 3L, startCol = 1L)
openxlsx::writeData(wb_all, "Bridges", bridge_rec_scen,
                    startRow = 3L, startCol = 4L)
openxlsx::writeData(wb_all, "Bridges", bridge_scen_metrics,
                    startRow = 3L, startCol = 7L)


## ---------------------------------------------------------------------
## WRITE source long-format master sheets (plain — no Tables)
## ---------------------------------------------------------------------

openxlsx::addWorksheet(wb_all, "State_Master")
openxlsx::writeData(wb_all, "State_Master", state_master_long, startRow = 1L)

openxlsx::addWorksheet(wb_all, "Mineral_Master")
openxlsx::writeData(wb_all, "Mineral_Master", mineral_master_long, startRow = 1L)

openxlsx::addWorksheet(wb_all, "Capacity_Master")
openxlsx::writeData(wb_all, "Capacity_Master", capacity_master_long, startRow = 1L)

openxlsx::addWorksheet(wb_all, "EoL_Chain_Source")
openxlsx::writeData(wb_all, "EoL_Chain_Source", eol_chain_country, startRow = 1L)

## ---------------------------------------------------------------------
## FIGURE DATA SHEETS (one sheet per figure, exact plotted values)
## ---------------------------------------------------------------------
## User-requested layout: after master sheets, include each figure's data
## in its own worksheet. These tables use the same transformed units that
## appear in the plotted axes.

fig01_data_from_state <- state_master_long %>%
  filter(Year == 2050) %>%
  inner_join(
    bridge_orig_metric %>% select(Origin, Metric),
    by = c("Legend" = "Metric")
  ) %>%
  transmute(
    Country,
    Region,
    State_Province,
    Origin,
    Year,
    `Metric Tonnes (millions)` = `Metric Tonnes` / 1e6
  )

fig02_data_from_state <- state_master_long %>%
  filter(Year == 2050) %>%
  group_by(Country, Legend) %>%
  summarise(`Metric Tonnes` = sum(`Metric Tonnes`, na.rm = TRUE), .groups = "drop") %>%
  transmute(
    Country,
    Region = Country,
    Metric = Legend,
    Tonnes = `Metric Tonnes` / 1e6,
    pattern_type = case_when(
      Country == "US" ~ "circle",
      Country == "CA" ~ "stripe",
      Country == "MX" ~ "crosshatch",
      TRUE ~ "none"
    )
  )

fig03_data_from_state <- state_master_long %>%
  filter(Year >= 2025) %>%
  group_by(Year, Legend) %>%
  summarise(`Metric Tonnes` = sum(`Metric Tonnes`, na.rm = TRUE), .groups = "drop") %>%
  transmute(
    Year = as.numeric(Year),
    Metric = Legend,
    Tonnes = `Metric Tonnes` / 1e6
  )

fig_sheet_data <- list(
  "Fig01_Data" = fig01_data_from_state,
  "Fig01b_Data" = Mass_2050_region_ref,
  "Fig02_Data" = fig02_data_from_state,
  "Fig03_Data" = fig03_data_from_state,
  "Fig04_Data" = plot04_export,
  "Fig05_Data" = plot05_export,
  "Fig06_Data" = needed_cap_long,
  "Fig07_Data" = plot07_export,
  "Fig08_Data" = plot08_export
)
if (exists("overall_circularity")) {
  fig_sheet_data[["Fig09_Data"]] <- overall_circularity
}

for (sheet_nm in names(fig_sheet_data)) {
  openxlsx::addWorksheet(wb_all, sheet_nm)
  openxlsx::writeData(
    wb_all, sheet_nm,
    prep_workbook_df(fig_sheet_data[[sheet_nm]]),
    startRow = 1L
  )
}


## ---------------------------------------------------------------------
## WRITE Dashboard — 10 vertical formula blocks
## ---------------------------------------------------------------------

openxlsx::addWorksheet(wb_all, "Dashboard")

write_label_cell <- function(row, text) {
  openxlsx::writeData(
    wb_all, "Dashboard",
    data.frame(X = text, stringsAsFactors = FALSE),
    startRow = row, startCol = 1L, colNames = FALSE
  )
}

write_label_cell(dash_title_row,
  paste0("RECYCLING DASHBOARD — Fleet scenario: ", FLEET_SCENARIO))


## ===== Block 1: Main Flow (Year x Scenario x Path) =====
write_label_cell(dash_b1$label,
  "BLOCK 1 - MAIN FLOW (Year x Scenario x Recycling Path)")

b1_data <- data.frame(
  Year                     = battery_flow_keys$Year,
  Scenario                 = battery_flow_keys$Scenario,
  Recycling_Path           = battery_flow_keys$Recycling_Path,
  LIB_Demand_MT            = NA_real_,
  Pack_Manufacturing_MT    = NA_real_,
  Cell_Manufacturing_MT    = NA_real_,
  EoL_Battery_MT           = NA_real_,
  Retired_Battery_MT       = NA_real_,
  Mfg_Scrap_ProcEq_MT      = NA_real_,
  Total_Recycling_Input_MT = NA_real_,
  Capacity_MT              = NA_real_,
  Surplus_MT               = NA_real_,
  Deficit_yr_MT            = NA_real_,
  NeededCap_Cumulative_MT  = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", b1_data,
                    startRow = dash_b1$header, startCol = 1L)

rb1 <- seq.int(dash_b1$first, dash_b1$last)
f_dem <- sprintf(
  "=SUMIFS(%s,%s,$A%i,%s,INDEX(%s,MATCH($B%i,%s,0)))",
  sm_tonn, sm_year, rb1, sm_legend, bm_dem, rb1, bm_scen)
f_pack <- sprintf(
  "=SUMIFS(%s,%s,$A%i,%s,INDEX(%s,MATCH($B%i,%s,0)))",
  sm_tonn, sm_year, rb1, sm_legend, bm_pack, rb1, bm_scen)
f_cell <- sprintf(
  "=SUMIFS(%s,%s,$A%i,%s,INDEX(%s,MATCH($B%i,%s,0)))",
  sm_tonn, sm_year, rb1, sm_legend, bm_cell, rb1, bm_scen)
f_eol <- sprintf(
  "=SUMIFS(%s,%s,$A%i,%s,INDEX(%s,MATCH($B%i,%s,0)))",
  sm_tonn, sm_year, rb1, sm_legend, bm_eol, rb1, bm_scen)
f_ret <- sprintf(
  '=SUMIFS(%s,%s,$A%i,%s,$B%i,%s,"Retired_Battery_MT")',
  cm_val, cm_year, rb1, cm_scen, rb1, cm_var)
f_scrap <- sprintf(
  '=SUMIFS(%s,%s,$A%i,%s,$B%i,%s,"Mfg_Scrap_ProcEq_MT")',
  cm_val, cm_year, rb1, cm_scen, rb1, cm_var)
f_total <- sprintf("=H%i+I%i", rb1, rb1)
f_cap <- sprintf(
  '=SUMIFS(%s,%s,$A%i,%s,$C%i,%s,"Capacity_MT")',
  cm_val, cm_year, rb1, cm_step, rb1, cm_var)
f_sur <- sprintf("=K%i-J%i", rb1, rb1)
f_def <- sprintf("=MAX(J%i-K%i,0)", rb1, rb1)
f_need <- sprintf(
  '=SUMIFS(%s,%s,$A%i,%s,$B%i,%s,$C%i,%s,"Cumulative_Needed_MT")',
  cm_val, cm_year, rb1, cm_scen, rb1, cm_step, rb1, cm_var)

openxlsx::writeFormula(wb_all, "Dashboard", f_dem,   startRow = dash_b1$first, startCol = 4L)
openxlsx::writeFormula(wb_all, "Dashboard", f_pack,  startRow = dash_b1$first, startCol = 5L)
openxlsx::writeFormula(wb_all, "Dashboard", f_cell,  startRow = dash_b1$first, startCol = 6L)
openxlsx::writeFormula(wb_all, "Dashboard", f_eol,   startRow = dash_b1$first, startCol = 7L)
openxlsx::writeFormula(wb_all, "Dashboard", f_ret,   startRow = dash_b1$first, startCol = 8L)
openxlsx::writeFormula(wb_all, "Dashboard", f_scrap, startRow = dash_b1$first, startCol = 9L)
openxlsx::writeFormula(wb_all, "Dashboard", f_total, startRow = dash_b1$first, startCol = 10L)
openxlsx::writeFormula(wb_all, "Dashboard", f_cap,   startRow = dash_b1$first, startCol = 11L)
openxlsx::writeFormula(wb_all, "Dashboard", f_sur,   startRow = dash_b1$first, startCol = 12L)
openxlsx::writeFormula(wb_all, "Dashboard", f_def,   startRow = dash_b1$first, startCol = 13L)
openxlsx::writeFormula(wb_all, "Dashboard", f_need,  startRow = dash_b1$first, startCol = 14L)


## ===== Block 2: EoL Tonnes Chain =====
write_label_cell(dash_b2$label,
  "BLOCK 2 - EoL TONNES CHAIN (Country x Year x BattScen):  Count x Avg_Cap = GWh x Pack_kg/kWh = MT")

b2_data <- data.frame(
  Country                       = eol_chain_country$Country,
  Year                          = eol_chain_country$Year,
  Battery_Scenario              = eol_chain_country$Battery_Scenario,
  Retired_Count                 = NA_real_,
  Implied_Avg_Cap_kWh_per_batt  = NA_real_,
  LIB_Recycle_GWh               = NA_real_,
  Implied_Pack_kg_per_kWh       = NA_real_,
  Batt_Mass_MT                  = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", b2_data,
                    startRow = dash_b2$header, startCol = 1L)

rb2 <- seq.int(dash_b2$first, dash_b2$last)
f_cnt <- sprintf(
  "=SUMIFS(%s,%s,$A%i,%s,$B%i,%s,$C%i)",
  el_count, el_ctry, rb2, el_year, rb2, el_scen, rb2)
f_avg <- sprintf("=IFERROR(F%i*1000000/D%i,0)", rb2, rb2)
f_gwh <- sprintf(
  "=SUMIFS(%s,%s,$A%i,%s,$B%i,%s,$C%i)",
  el_gwh, el_ctry, rb2, el_year, rb2, el_scen, rb2)
f_kgkwh <- sprintf("=IFERROR(H%i/F%i/1000,0)", rb2, rb2)
f_mt <- sprintf(
  "=SUMIFS(%s,%s,$A%i,%s,$B%i,%s,$C%i)",
  el_mt, el_ctry, rb2, el_year, rb2, el_scen, rb2)

openxlsx::writeFormula(wb_all, "Dashboard", f_cnt,   startRow = dash_b2$first, startCol = 4L)
openxlsx::writeFormula(wb_all, "Dashboard", f_avg,   startRow = dash_b2$first, startCol = 5L)
openxlsx::writeFormula(wb_all, "Dashboard", f_gwh,   startRow = dash_b2$first, startCol = 6L)
openxlsx::writeFormula(wb_all, "Dashboard", f_kgkwh, startRow = dash_b2$first, startCol = 7L)
openxlsx::writeFormula(wb_all, "Dashboard", f_mt,    startRow = dash_b2$first, startCol = 8L)


## ===== Block 3: Fig01 — 2050 State x Origin =====
write_label_cell(dash_f1$label,
  "BLOCK 3 - Fig01: 2050 State/Province x Origin (millions of MT)")

bf1_data <- data.frame(
  State_Province = fig01_keys$State_Province,
  Origin         = fig01_keys$Origin,
  Tonnes_million = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", bf1_data,
                    startRow = dash_f1$header, startCol = 1L)

rf1 <- seq.int(dash_f1$first, dash_f1$last)
f_fig01 <- sprintf(
  "=SUMIFS(%s,%s,2050,%s,$A%i,%s,INDEX(%s,MATCH($B%i,%s,0)))/1000000",
  sm_tonn, sm_year, sm_state, rf1, sm_legend, br_metric, rf1, br_origin)
openxlsx::writeFormula(wb_all, "Dashboard", f_fig01,
                       startRow = dash_f1$first, startCol = 3L)


## ===== Block 4: Fig02 — 2050 Country x Metric =====
write_label_cell(dash_f2$label,
  "BLOCK 4 - Fig02: 2050 Country x Metric (millions of MT)")

bf2_data <- data.frame(
  Country = fig02_keys$Country,
  Metric  = fig02_keys$Metric,
  Tonnes_million = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", bf2_data,
                    startRow = dash_f2$header, startCol = 1L)

rf2 <- seq.int(dash_f2$first, dash_f2$last)
f_fig02 <- sprintf(
  "=SUMIFS(%s,%s,2050,%s,$A%i,%s,$B%i)/1000000",
  sm_tonn, sm_year, sm_ctry, rf2, sm_legend, rf2)
openxlsx::writeFormula(wb_all, "Dashboard", f_fig02,
                       startRow = dash_f2$first, startCol = 3L)


## ===== Block 5: Fig03 — Year x Metric =====
write_label_cell(dash_f3$label,
  "BLOCK 5 - Fig03: Year x Metric, North America total (millions of MT)")

bf3_data <- data.frame(
  Year   = fig03_keys$Year,
  Metric = fig03_keys$Metric,
  Tonnes_million = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", bf3_data,
                    startRow = dash_f3$header, startCol = 1L)

rf3 <- seq.int(dash_f3$first, dash_f3$last)
f_fig03 <- sprintf(
  "=SUMIFS(%s,%s,$A%i,%s,$B%i)/1000000",
  sm_tonn, sm_year, rf3, sm_legend, rf3)
openxlsx::writeFormula(wb_all, "Dashboard", f_fig03,
                       startRow = dash_f3$first, startCol = 3L)


## ===== Block 6: Fig04 — Recycled minerals =====
write_label_cell(dash_f4$label,
  "BLOCK 6 - Fig04: Year x Scenario x Mineral x Recycling_Scenario (Tonne recovered)")

bf4_data <- data.frame(
  Year     = fig04_keys$Year,
  Scenario = fig04_keys$Scenario,
  Mineral  = fig04_keys$Mineral,
  Recycling_Scenario = fig04_keys$Recycling_Scenario,
  Tonne    = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", bf4_data,
                    startRow = dash_f4$header, startCol = 1L)

rf4 <- seq.int(dash_f4$first, dash_f4$last)
f_fig04 <- sprintf(
  "=SUMIFS(%s,%s,$A%i,%s,$B%i,%s,$C%i,%s,INDEX(%s,MATCH($D%i,%s,0)))",
  mm_val, mm_year, rf4, mm_scen, rf4, mm_min, rf4,
  mm_var, brs_var, rf4, brs_scen)
openxlsx::writeFormula(wb_all, "Dashboard", f_fig04,
                       startRow = dash_f4$first, startCol = 5L)


## ===== Block 7: Fig05 — Lost minerals (2035 bar, thousands MT) =====
write_label_cell(dash_f5$label,
  "BLOCK 7 - Fig05: Mineral x Cumulative Lost at 2035 (thousands MT, aggregated across scenarios)")

bf5_data <- data.frame(
  Mineral = fig05_keys$Mineral,
  Sort_Order = fig05_keys$Sort_Order,
  Cum_Lost_2035_Thousand_MT = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", bf5_data,
                    startRow = dash_f5$header, startCol = 1L)

rf5 <- seq.int(dash_f5$first, dash_f5$last)
f_fig05 <- sprintf(
  '=SUMIFS(%s,%s,2035,%s,A%i,%s,"Minerals Lost From Non-Recovery")/1000',
  mm_val, mm_year, mm_min, rf5, mm_var
)
openxlsx::writeFormula(wb_all, "Dashboard", f_fig05,
                       startRow = dash_f5$first, startCol = 3L)


## ===== Block 8: Fig06 — Needed capacity =====
write_label_cell(dash_f6$label,
  "BLOCK 8 - Fig06: Year x Scenario x Recycling Step, Cumulative Needed Capacity (millions of MT)")

bf6_data <- data.frame(
  Year     = fig06_keys$Year,
  Scenario = fig06_keys$Scenario,
  Recycling_Step = fig06_keys$Step,
  Tonne_million  = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", bf6_data,
                    startRow = dash_f6$header, startCol = 1L)

rf6 <- seq.int(dash_f6$first, dash_f6$last)
f_fig06 <- sprintf(
  '=SUMIFS(%s,%s,$A%i,%s,$B%i,%s,$C%i,%s,"Cumulative_Needed_MT")/1000000',
  cm_val, cm_year, rf6, cm_scen, rf6, cm_step, rf6, cm_var)
openxlsx::writeFormula(wb_all, "Dashboard", f_fig06,
                       startRow = dash_f6$first, startCol = 4L)


## ===== Block 9: Fig07 — Exported minerals =====
write_label_cell(dash_f7$label,
  "BLOCK 9 - Fig07: Year x Scenario x Mineral, Exported Minerals (Tonne, Scrap+BM)")

bf7_data <- data.frame(
  Year     = fig07_keys$Year,
  Scenario = fig07_keys$Scenario,
  Mineral  = fig07_keys$Mineral,
  Total_Minerals_Exported = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", bf7_data,
                    startRow = dash_f7$header, startCol = 1L)

rf7 <- seq.int(dash_f7$first, dash_f7$last)
f_fig07 <- sprintf(
  paste0(
    '=SUMIFS(%s,%s,$A%i,%s,$B%i,%s,$C%i,%s,"Minerals Recoverable in Exported Scrap/Batts (Tonne)")',
    '+SUMIFS(%s,%s,$A%i,%s,$B%i,%s,$C%i,%s,"Minerals Recoverable in Exported BM (Tonne)")'),
  mm_val, mm_year, rf7, mm_scen, rf7, mm_min, rf7, mm_var,
  mm_val, mm_year, rf7, mm_scen, rf7, mm_min, rf7, mm_var)
openxlsx::writeFormula(wb_all, "Dashboard", f_fig07,
                       startRow = dash_f7$first, startCol = 4L)


## ===== Block 10: Fig08 — Recycled content ratio =====
write_label_cell(dash_f8$label,
  "BLOCK 10 - Fig08: Year x Scenario x Mineral x Recycling_Scenario, Recycle/Demand ratio")

bf8_data <- data.frame(
  Year     = fig08_keys$Year,
  Scenario = fig08_keys$Scenario,
  Mineral  = fig08_keys$Mineral,
  Recycling_Scenario = fig08_keys$Recycling_Scenario,
  Recycle_v_Demand   = NA_real_,
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb_all, "Dashboard", bf8_data,
                    startRow = dash_f8$header, startCol = 1L)

rf8 <- seq.int(dash_f8$first, dash_f8$last)
f_fig08 <- sprintf(
  paste0(
    '=IFERROR(SUMIFS(%s,%s,$A%i-1,%s,$B%i,%s,$C%i,%s,INDEX(%s,MATCH($D%i,%s,0)))',
    '/SUMIFS(%s,%s,$A%i,%s,$B%i,%s,$C%i,%s,"Demand Minerals (Tonne)"),NA())'),
  mm_val, mm_year, rf8, mm_scen, rf8, mm_min, rf8, mm_var, brs_var, rf8, brs_scen,
  mm_val, mm_year, rf8, mm_scen, rf8, mm_min, rf8, mm_var)
openxlsx::writeFormula(wb_all, "Dashboard", f_fig08,
                       startRow = dash_f8$first, startCol = 5L)


## ---------------------------------------------------------------------
## SAVE WORKBOOK
## ---------------------------------------------------------------------
openxlsx::saveWorkbook(wb_all, workbook_path, overwrite = TRUE)
message("  combined workbook: ", workbook_path)



## =====================================================================
## SECTION 4:  NAATBATT CROSSWALK & MIDSTREAM EXPORT  (optional)
## =====================================================================
## This section requires an external `Naatbatt_Gwh` object (a tibble with
## a Company column listing NAATBatt members). It is NOT shipped with the
## repo, so this section is skipped unless the caller has defined it in
## the global environment before sourcing 03. To enable: construct e.g.
##   Naatbatt_Gwh <- readxl::read_excel("path/to/Naatbatt.xlsx")
## then source this script.

## Auto-fallback so this section does not get skipped in normal runs:
## if `Naatbatt_Gwh` is not pre-defined by caller, build a Company lookup
## from Inputs/Ontario_Naatbatt.csv.
naatbatt_lookup <- NULL
if (exists("Naatbatt_Gwh", inherits = TRUE)) {
  naatbatt_lookup <- get("Naatbatt_Gwh", inherits = TRUE)
} else {
  fallback_naat <- file.path(INPUT_DIR, "Ontario_Naatbatt.csv")
  if (file.exists(fallback_naat)) {
    naatbatt_lookup <- read.csv(fallback_naat, check.names = FALSE) %>%
      dplyr::select(dplyr::any_of("Company")) %>%
      dplyr::transmute(Company = as.character(Company)) %>%
      dplyr::filter(!is.na(Company), Company != "") %>%
      dplyr::distinct()
    message("Section 4: `Naatbatt_Gwh` not found; using Company list from Inputs/Ontario_Naatbatt.csv.")
  } else {
    naatbatt_lookup <- data.frame(Company = character(), stringsAsFactors = FALSE)
    message("Section 4: `Naatbatt_Gwh` not found and no Ontario fallback file found; writing Ontario-only output.")
  }
}

csv_list_manufac <- read.csv(
  file.path(INPUT_DIR, "total_manufacturing_edited.csv")
) %>%
  rename("State/ Province" = State..Province)

cross_compare <- csv_list_manufac %>%
  semi_join(naatbatt_lookup, by = "Company") %>%
  mutate(Gwh.yr = as.numeric(Gwh.yr))

ontario_naat_batt <- read.csv(
  file.path(INPUT_DIR, "Ontario_Naatbatt.csv")
) %>%
  rename("State/ Province" = State..Province)
if ("X.1" %in% names(ontario_naat_batt)) {
  ontario_naat_batt <- ontario_naat_batt %>% rename(Info = X.1)
}

drop_rows <- c(31, 44, 41, 6, 5, 7, 26, 28, 50, 48, 47, 49)
drop_rows <- drop_rows[drop_rows <= nrow(cross_compare)]
if (length(drop_rows) > 0) {
  cross_compare <- cross_compare[-drop_rows, ]
}
cross_compare <- cross_compare %>% bind_rows(ontario_naat_batt)

write.xlsx(cross_compare, "Outputs/Naatbatt_Gwh_midstream.xlsx", rowNames = FALSE)
message("Section 4: wrote Naatbatt_Gwh_midstream.xlsx")


cat("=== 03-Recycling_R_Plots_and_Exports.R complete ===\n")
cat("Plots saved to:", PLOT_DIR, "\n")
