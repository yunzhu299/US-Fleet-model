## ====================================================================
## Update the "Figure UCD2 cumulative EV battery retirements 2020-2050"
## paragraph with current fleet-turnover model outputs.
##
## Reports cumulative spent (EoL) batteries by:
##   * LDV   (BEV / PHEV) by country and scenario
##   * HDV   (BEV only)   by country and scenario
##   * BESS  (LDV + HDV second-life) by country and scenario
##
## Two metrics are reported because both are defensible interpretations of
## "spent EV batteries":
##   * LIB_available  = cumulative pool of batteries reaching EoL
##                      (incl. those reused/repurposed/exported/recycled)
##   * LIB_recycling  = subset that actually flows into recycling
## The original 73.47 M number was generated from `LIB_recycling` (verified
## downstream — see DIAGNOSTIC printout).
##
## Outputs:
##   Outputs/Visualization/Cumulative_2020_2050_Summary.csv
##   Outputs/Visualization/Updated_Paragraph_UCD2.txt
## ====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

OUT_DIR <- "Outputs/Visualization"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

YR_MIN <- 2020
YR_MAX <- 2050

## ---- helpers ----
fmt_M <- function(x) sprintf("%.2f", x / 1e6)

read_ldv <- function(path, country) {
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) %>%
    filter(Year >= YR_MIN, Year <= YR_MAX) %>%
    mutate(Country = country) %>%
    group_by(Country, Propulsion) %>%
    summarise(
      LIB_recycling_cum = sum(coalesce(LIB_recycling, 0), na.rm = TRUE),
      LIB_available_cum = sum(coalesce(LIB_available, 0), na.rm = TRUE),
      .groups = "drop"
    )
}

read_hdv <- function(path) {
  if (!file.exists(path)) return(NULL)
  d <- read_csv(path, show_col_types = FALSE) %>%
    filter(Year >= YR_MIN, Year <= YR_MAX) %>%
    mutate(Country_short = case_when(
      Country == "United States" ~ "US",
      Country == "Canada"        ~ "CA",
      Country == "Mexico"        ~ "MX",
      TRUE                       ~ Country
    ))
  # HDV has no scalar LIB_available; derive it from sub-flows
  d %>%
    mutate(
      LIB_recycling = coalesce(LIB_recycling, 0),
      LIB_repurpose = coalesce(LIB_repurpose, 0),
      LIB_reuse_EV  = coalesce(LIB_reuse_EV,  0),
      LIB_available_derived = LIB_recycling + LIB_repurpose + LIB_reuse_EV
    ) %>%
    group_by(Country = Country_short) %>%
    summarise(
      LIB_recycling_cum = sum(LIB_recycling, na.rm = TRUE),
      LIB_available_cum = sum(LIB_available_derived, na.rm = TRUE),
      .groups = "drop"
    )
}

read_bess <- function(path, country) {
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) %>%
    filter(Year >= YR_MIN, Year <= YR_MAX) %>%
    summarise(
      Country = country,
      BESS_retire_cum = sum(coalesce(BESS_retire_total, 0), na.rm = TRUE)
    )
}

read_hdv_bess <- function(path) {
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) %>%
    filter(Year >= YR_MIN, Year <= YR_MAX) %>%
    mutate(Country_short = case_when(
      Country == "United States" ~ "US",
      Country == "Canada"        ~ "CA",
      Country == "Mexico"        ~ "MX",
      TRUE                       ~ Country
    )) %>%
    group_by(Country = Country_short) %>%
    summarise(
      BESS_retire_cum = sum(coalesce(BESS_retire_total, 0), na.rm = TRUE),
      .groups = "drop"
    )
}

## ---- 1.  Build cumulative tables -----------------------------------
ldv_paths <- list(
  list(country = "US", scen = "ACCII",
       p = "Outputs/EVLIB_Flows_detail_ACCII.csv"),
  list(country = "US", scen = "Repeal",
       p = "Outputs/EVLIB_Flows_detail_Repeal.csv"),
  list(country = "CA", scen = "ACCII",
       p = "Outputs/Canada/EVLIB_Flows_detail_ACCII.csv"),
  list(country = "CA", scen = "Repeal",
       p = "Outputs/Canada/EVLIB_Flows_detail_Repeal.csv"),
  list(country = "MX", scen = "ACCII",
       p = "Outputs/Mexico/EVLIB_Flows_detail_ACCII.csv"),
  list(country = "MX", scen = "Repeal",
       p = "Outputs/Mexico/EVLIB_Flows_detail_Repeal.csv")
)

ldv_all <- purrr::map_dfr(ldv_paths, ~ read_ldv(.x$p, .x$country) %>%
                             mutate(Scenario = .x$scen))

hdv_all <- bind_rows(
  read_hdv("Outputs/HDV/HDV_EV_Turnover_ACCII.csv")  %>% mutate(Scenario = "ACCII"),
  read_hdv("Outputs/HDV/HDV_EV_Turnover_Repeal.csv") %>% mutate(Scenario = "Repeal")
)

bess_paths <- list(
  list(country = "US", scen = "ACCII",
       p = "Outputs/BESS_Retire_Vector_byStateSegProp_ACCII.csv"),
  list(country = "US", scen = "Repeal",
       p = "Outputs/BESS_Retire_Vector_byStateSegProp_Repeal.csv"),
  list(country = "CA", scen = "ACCII",
       p = "Outputs/Canada/BESS_Retire_Vector_byStateSegProp_ACCII.csv"),
  list(country = "CA", scen = "Repeal",
       p = "Outputs/Canada/BESS_Retire_Vector_byStateSegProp_Repeal.csv"),
  list(country = "MX", scen = "ACCII",
       p = "Outputs/Mexico/BESS_Retire_Vector_byStateSegProp_ACCII.csv"),
  list(country = "MX", scen = "Repeal",
       p = "Outputs/Mexico/BESS_Retire_Vector_byStateSegProp_Repeal.csv")
)

bess_ldv_all <- purrr::map_dfr(bess_paths,
  ~ read_bess(.x$p, .x$country) %>% mutate(Scenario = .x$scen)
)

hdv_bess_all <- bind_rows(
  read_hdv_bess("Outputs/HDV/HDV_BESS_Retire_ACCII.csv")  %>% mutate(Scenario = "ACCII"),
  read_hdv_bess("Outputs/HDV/HDV_BESS_Retire_Repeal.csv") %>% mutate(Scenario = "Repeal")
)

## ---- 2.  Make a single tidy summary table --------------------------
ldv_wide <- ldv_all %>%
  pivot_wider(
    names_from  = Propulsion,
    values_from = c(LIB_recycling_cum, LIB_available_cum),
    values_fill = 0
  ) %>%
  mutate(
    LDV_recycling_total = LIB_recycling_cum_BEV + LIB_recycling_cum_PHEV,
    LDV_available_total = LIB_available_cum_BEV + LIB_available_cum_PHEV
  )

bess_combined <- bess_ldv_all %>%
  rename(BESS_LDV_cum = BESS_retire_cum) %>%
  full_join(hdv_bess_all %>% rename(BESS_HDV_cum = BESS_retire_cum),
            by = c("Country", "Scenario")) %>%
  mutate(
    BESS_LDV_cum = coalesce(BESS_LDV_cum, 0),
    BESS_HDV_cum = coalesce(BESS_HDV_cum, 0),
    BESS_total   = BESS_LDV_cum + BESS_HDV_cum
  )

summary_tbl <- ldv_wide %>%
  full_join(hdv_all %>% rename(HDV_recycling_cum = LIB_recycling_cum,
                                HDV_available_cum = LIB_available_cum),
            by = c("Country", "Scenario")) %>%
  full_join(bess_combined, by = c("Country", "Scenario")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  arrange(Country, Scenario)

write_csv(summary_tbl, file.path(OUT_DIR, "Cumulative_2020_2050_Summary.csv"))

## ---- 3.  Print a formatted summary ---------------------------------
cat("\n\n#####################################################################\n")
cat("##   CUMULATIVE SPENT EV BATTERIES, 2020-2050  (millions of batteries)\n")
cat("#####################################################################\n\n")

print_block <- function(country_name) {
  rows <- summary_tbl %>% filter(Country == country_name) %>% arrange(Scenario)
  cat(sprintf("---- %s ----\n", country_name))
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    cat(sprintf("  Scenario: %s\n", r$Scenario))
    cat(sprintf("    LDV  BEV  : recycling=%s M | available=%s M\n",
                fmt_M(r$LIB_recycling_cum_BEV), fmt_M(r$LIB_available_cum_BEV)))
    cat(sprintf("    LDV  PHEV : recycling=%s M | available=%s M\n",
                fmt_M(r$LIB_recycling_cum_PHEV), fmt_M(r$LIB_available_cum_PHEV)))
    cat(sprintf("    LDV  TOTAL: recycling=%s M | available=%s M\n",
                fmt_M(r$LDV_recycling_total), fmt_M(r$LDV_available_total)))
    cat(sprintf("    HDV  total: recycling=%s M | available=%s M\n",
                fmt_M(r$HDV_recycling_cum), fmt_M(r$HDV_available_cum)))
    cat(sprintf("    BESS LDV  : retire=%s M\n",   fmt_M(r$BESS_LDV_cum)))
    cat(sprintf("    BESS HDV  : retire=%s M\n",   fmt_M(r$BESS_HDV_cum)))
    cat(sprintf("    BESS TOTAL: retire=%s M\n\n", fmt_M(r$BESS_total)))
  }
}
print_block("US")
print_block("CA")
print_block("MX")

## ---- 4.  Build the updated paragraph (LIB_recycling basis) ---------
get <- function(country, scen, col) {
  v <- summary_tbl %>% filter(Country == country, Scenario == scen) %>%
    pull(!!sym(col))
  if (length(v) == 0) 0 else v
}

us_ac_bev   <- get("US", "ACCII",  "LIB_recycling_cum_BEV")  / 1e6
us_ac_phev  <- get("US", "ACCII",  "LIB_recycling_cum_PHEV") / 1e6
us_re_bev   <- get("US", "Repeal", "LIB_recycling_cum_BEV")  / 1e6
us_re_phev  <- get("US", "Repeal", "LIB_recycling_cum_PHEV") / 1e6
ca_ac_bev   <- get("CA", "ACCII",  "LIB_recycling_cum_BEV")  / 1e6
ca_ac_phev  <- get("CA", "ACCII",  "LIB_recycling_cum_PHEV") / 1e6
ca_re_bev   <- get("CA", "Repeal", "LIB_recycling_cum_BEV")  / 1e6
ca_re_phev  <- get("CA", "Repeal", "LIB_recycling_cum_PHEV") / 1e6
mx_ac_bev   <- get("MX", "ACCII",  "LIB_recycling_cum_BEV")  / 1e6
mx_ac_phev  <- get("MX", "ACCII",  "LIB_recycling_cum_PHEV") / 1e6

us_hdv_ac   <- get("US", "ACCII",  "HDV_recycling_cum") / 1e6
us_hdv_re   <- get("US", "Repeal", "HDV_recycling_cum") / 1e6
ca_hdv_ac   <- get("CA", "ACCII",  "HDV_recycling_cum") / 1e6
ca_hdv_re   <- get("CA", "Repeal", "HDV_recycling_cum") / 1e6
mx_hdv_ac   <- get("MX", "ACCII",  "HDV_recycling_cum") / 1e6
mx_hdv_re   <- get("MX", "Repeal", "HDV_recycling_cum") / 1e6

us_bess_ac  <- get("US", "ACCII",  "BESS_total") / 1e6
us_bess_re  <- get("US", "Repeal", "BESS_total") / 1e6
ca_bess_ac  <- get("CA", "ACCII",  "BESS_total") / 1e6
ca_bess_re  <- get("CA", "Repeal", "BESS_total") / 1e6
mx_bess_ac  <- get("MX", "ACCII",  "BESS_total") / 1e6
mx_bess_re  <- get("MX", "Repeal", "BESS_total") / 1e6

us_ac_total <- us_ac_bev + us_ac_phev
us_re_total <- us_re_bev + us_re_phev
us_diff     <- us_ac_total - us_re_total
ca_ac_total <- ca_ac_bev + ca_ac_phev
ca_re_total <- ca_re_bev + ca_re_phev
ca_diff     <- ca_ac_total - ca_re_total
mx_ac_total <- mx_ac_bev + mx_ac_phev

f1 <- function(x) sprintf("%.2f", x)

paragraph <- sprintf(
"Figure UCD2 shows cumulative EV battery retirements over 2020 to 2050, indicating both the potential availability of end-of-life (EoL) LIBs and where collection and EoL management needs for LIBs will be required.

Light-duty vehicles (LDV).
The United States generates %s million spent LDV batteries under conditions where state-level policies (namely the Advanced Clean Car II rule, or ACCII) supporting EV adoption are permitted to continue, including %s million BEV and %s million PHEV, compared with %s million (including %s million BEV and %s million PHEV) under policy repeal. This difference of nearly %s million LDV batteries reflects higher EV adoption under sustained policy support. Canada generates %s million spent LDV batteries under continued ACCII policy (%s million BEV; %s million PHEV), compared with %s million under repeal (%s million BEV; %s million PHEV), a difference of %s million driven by continued EV policy support. Mexico generates %s million spent LDV batteries under current EV policy targets, ACCII (%s million BEV; %s million PHEV).

Heavy-duty vehicles (HDV).
The United States generates %s million spent HDV batteries under ACCII (vs. %s million under repeal); Canada %s million under ACCII (vs. %s million under repeal); Mexico %s million under ACCII (vs. %s million under repeal).

Battery energy storage systems (BESS).
Cumulative second-life battery retirements (LDV BESS + HDV BESS combined) are %s million in the United States under ACCII (vs. %s million under repeal), %s million in Canada under ACCII (vs. %s million under repeal), and %s million in Mexico under ACCII (vs. %s million under repeal).
",
  f1(us_ac_total),
  f1(us_ac_bev),  f1(us_ac_phev),
  f1(us_re_total),
  f1(us_re_bev),  f1(us_re_phev),
  f1(us_diff),
  f1(ca_ac_total),
  f1(ca_ac_bev),  f1(ca_ac_phev),
  f1(ca_re_total),
  f1(ca_re_bev),  f1(ca_re_phev),
  f1(ca_diff),
  f1(mx_ac_total),
  f1(mx_ac_bev),  f1(mx_ac_phev),
  f1(us_hdv_ac),  f1(us_hdv_re),
  f1(ca_hdv_ac),  f1(ca_hdv_re),
  f1(mx_hdv_ac),  f1(mx_hdv_re),
  f1(us_bess_ac), f1(us_bess_re),
  f1(ca_bess_ac), f1(ca_bess_re),
  f1(mx_bess_ac), f1(mx_bess_re)
)

cat("#####################################################################\n")
cat("##   UPDATED PARAGRAPH (LIB_recycling basis, M batteries 2020-2050)\n")
cat("#####################################################################\n")
cat(paragraph)

writeLines(paragraph, file.path(OUT_DIR, "Updated_Paragraph_UCD2.txt"))
cat("\n\n[Saved] ", file.path(OUT_DIR, "Updated_Paragraph_UCD2.txt"), "\n", sep = "")
cat("[Saved] ", file.path(OUT_DIR, "Cumulative_2020_2050_Summary.csv"), "\n", sep = "")

## ---- 5.  Verify graph numbers --------------------------------------
##
## The geofacet graph (graph.R) sums ALL of the following per State × Year:
##   LIB_recycling (LDV)  +  BESS_retire_total (LDV)  +  LIB_recycling (HDV)
##   +  BESS_retire_total (HDV)
## ...then cumulates by state.  The country-level totals must therefore equal:
##   LDV_recycling_total + HDV_recycling_cum + BESS_total
graph_check <- summary_tbl %>%
  mutate(GraphTotal_M = (LDV_recycling_total + HDV_recycling_cum + BESS_total) / 1e6) %>%
  select(Country, Scenario,
         LDV_recycling_M = LDV_recycling_total,
         HDV_recycling_M = HDV_recycling_cum,
         BESS_M          = BESS_total,
         GraphTotal_M) %>%
  mutate(across(c(LDV_recycling_M, HDV_recycling_M, BESS_M),
                ~ .x / 1e6))

cat("\n#####################################################################\n")
cat("##   GRAPH CROSS-CHECK\n")
cat("##   (Outputs/NorthAmerica_Cumulative_BatteryRetirement_Geofacet.png)\n")
cat("#####################################################################\n")
print(graph_check)
write_csv(graph_check, file.path(OUT_DIR, "Graph_CrossCheck_2050.csv"))
cat("\n[Saved] ", file.path(OUT_DIR, "Graph_CrossCheck_2050.csv"), "\n", sep = "")

## ---- 6.  Country-level bar chart (visual cross-check of paragraph) -
suppressPackageStartupMessages({
  library(ggplot2); library(scales); library(forcats)
})

bar_data <- summary_tbl %>%
  transmute(
    Country, Scenario,
    LDV  = LDV_recycling_total / 1e6,
    HDV  = HDV_recycling_cum   / 1e6,
    BESS = BESS_total          / 1e6
  ) %>%
  pivot_longer(c(LDV, HDV, BESS), names_to = "Source", values_to = "Million") %>%
  mutate(
    Country  = factor(Country,  levels = c("US", "CA", "MX")),
    Scenario = factor(Scenario, levels = c("ACCII", "Repeal")),
    Source   = factor(Source,   levels = c("LDV", "HDV", "BESS"))
  )

totals_lab <- bar_data %>%
  group_by(Country, Scenario) %>%
  summarise(Total = sum(Million), .groups = "drop") %>%
  mutate(label = sprintf("%.1f M", Total))

p_bar <- ggplot(bar_data, aes(x = Scenario, y = Million, fill = Source)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.4) +
  geom_text(data = totals_lab, aes(x = Scenario, y = Total, label = label),
            vjust = -0.4, size = 5.0, fontface = "bold", inherit.aes = FALSE) +
  facet_wrap(~ Country, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "LDV"  = "#2E86AB",
    "HDV"  = "#E07A5F",
    "BESS" = "#9DAA8B"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Cumulative spent EV batteries, 2020-2050",
    subtitle = "ACCII vs Repeal  |  LDV (BEV+PHEV) + HDV (BEV) + BESS (LDV+HDV second-life)",
    y        = "Million batteries",
    x        = NULL,
    fill     = "Source"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey30"),
    strip.background = element_rect(fill = "grey92", color = "grey75"),
    strip.text       = element_text(face = "bold", size = 14),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

bar_path <- file.path(OUT_DIR, "Cumulative_BatteryRetirement_Country_Bar.png")
ggsave(bar_path, p_bar, width = 12, height = 6, dpi = 300, bg = "white")
cat("[Saved] ", bar_path, "\n", sep = "")

## ---- 7.  Copy refreshed geofacet plot into Visualization/ ----------
src_geofacet <- "Outputs/NorthAmerica_Cumulative_BatteryRetirement_Geofacet.png"
if (file.exists(src_geofacet)) {
  file.copy(src_geofacet,
            file.path(OUT_DIR, basename(src_geofacet)),
            overwrite = TRUE)
  cat("[Copied] ", src_geofacet, " -> ", OUT_DIR, "/\n", sep = "")
}
