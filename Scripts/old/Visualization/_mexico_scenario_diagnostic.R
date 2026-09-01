## ====================================================================
## Why do MX ACCII and MX Repeal look identical on the geofacet?
##
## Diagnostic: plot the underlying PR curves used by 024-MexicoTurnover.R
## alongside the resulting annual EV retirement, to make the cause visible.
##
## Output: Outputs/Visualization/Mexico_Scenario_Diagnostic.png
## ====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
  library(scales); library(patchwork)
})

OUT_DIR <- "Outputs/Visualization"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## ---- 1.  Reconstruct PR scenarios used by 024-MexicoTurnover.R -----
pr_milestones <- tibble(
  Year  = c(2025, 2030, 2035, 2040, 2045, 2050),
  EV_PR = c(0.10, 0.30, 0.65, 1.00, 1.00, 1.00)
)
pr_accii <- tibble(Year = 2020:2050) %>%
  left_join(pr_milestones, by = "Year") %>% arrange(Year) %>%
  mutate(EV_PR = approx(Year[!is.na(EV_PR)], EV_PR[!is.na(EV_PR)], Year, rule = 2)$y) %>%
  mutate(Scenario = "ACCII (ENME)")

PR_REPEAL_PATH <- "~/Downloads/PR_Repeal.csv"
pr_repeal <- if (file.exists(path.expand(PR_REPEAL_PATH))) {
  raw <- read_csv(path.expand(PR_REPEAL_PATH), show_col_types = FALSE) %>%
    mutate(State = trimws(State))
  ca_re <- raw %>% filter(State == "California", Propulsion %in% c("BEV","PHEV")) %>%
    select(Year, Propulsion, Fraction) %>%
    pivot_wider(names_from = Propulsion, values_from = Fraction) %>%
    mutate(BEV = coalesce(BEV, 0), PHEV = coalesce(PHEV, 0))
  out <- tibble(Year = 2020:2050) %>% left_join(ca_re, by = "Year") %>% arrange(Year) %>%
    mutate(EV_PR = coalesce(BEV, 0) + coalesce(PHEV, 0))
  first_pr_yr <- min(ca_re$Year, na.rm = TRUE)
  fill_v <- out %>% filter(Year == first_pr_yr)
  out %>% mutate(EV_PR = if_else(Year < first_pr_yr, fill_v$EV_PR, EV_PR)) %>%
    select(Year, EV_PR) %>% mutate(Scenario = "Repeal (CA pattern)")
} else {
  warning("PR_Repeal.csv not found"); tibble()
}

pr_long <- bind_rows(pr_accii %>% select(Year, EV_PR, Scenario), pr_repeal)

## ---- 2.  Read MX retirement (annual) -------------------------------
read_mx <- function(scen) {
  ldv <- read_csv(file.path("Outputs/Mexico",
    paste0("EVLIB_Flows_detail_", scen, ".csv")), show_col_types = FALSE) %>%
    group_by(Year) %>% summarise(LDV = sum(LIB_recycling, na.rm = TRUE), .groups = "drop")
  bess <- read_csv(file.path("Outputs/Mexico",
    paste0("BESS_Retire_Vector_byStateSegProp_", scen, ".csv")), show_col_types = FALSE) %>%
    group_by(Year) %>% summarise(BESS = sum(BESS_retire_total, na.rm = TRUE), .groups = "drop")
  full_join(ldv, bess, by = "Year") %>%
    mutate(LDV = coalesce(LDV, 0), BESS = coalesce(BESS, 0),
           Total = LDV + BESS, Scenario = scen)
}

mx_annual <- bind_rows(read_mx("ACCII"), read_mx("Repeal")) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  arrange(Scenario, Year) %>%
  group_by(Scenario) %>%
  mutate(Cumulative = cumsum(Total)) %>%
  ungroup()

## ---- 3.  Read MX EV stock (to show divergence) ---------------------
read_stock <- function(scen) {
  read_csv(file.path("Outputs/Mexico",
    paste0("ClosedLoop_StateTotals_", scen, ".csv")), show_col_types = FALSE) %>%
    group_by(Year) %>%
    summarise(Stock = sum(stock_BEV + stock_PHEV, na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scen)
}
mx_stock <- bind_rows(read_stock("ACCII"), read_stock("Repeal"))

## ---- 4.  Three-panel diagnostic plot --------------------------------
scen_colors <- c(
  "ACCII (ENME)"        = "#1F4E79",
  "Repeal (CA pattern)" = "#C0504D",
  "ACCII"               = "#1F4E79",
  "Repeal"              = "#C0504D"
)

p1 <- ggplot(pr_long, aes(Year, EV_PR, color = Scenario)) +
  geom_line(linewidth = 1.4) +
  geom_vline(xintercept = 2032, linetype = "dashed", color = "grey60") +
  annotate("text", x = 2032, y = 0.95, label = "curves cross\nat 2032",
           hjust = -0.1, vjust = 1, size = 3.5, color = "grey40", fontface = "italic") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05)) +
  scale_color_manual(values = scen_colors) +
  labs(title = "(a) PR curves used for Mexico new-vehicle sales",
       y = "EV penetration rate", x = NULL, color = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

p2 <- ggplot(mx_stock, aes(Year, Stock / 1e6, color = Scenario)) +
  geom_line(linewidth = 1.4) +
  scale_color_manual(values = scen_colors) +
  scale_y_continuous(labels = comma) +
  labs(title = "(b) Mexico EV stock (BEV + PHEV)",
       y = "Million vehicles", x = NULL, color = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

p3 <- ggplot(mx_annual, aes(Year, Cumulative / 1e6, color = Scenario)) +
  geom_line(linewidth = 1.4) +
  geom_area(aes(fill = Scenario), alpha = 0.18, position = "identity") +
  scale_color_manual(values = scen_colors) +
  scale_fill_manual(values = scen_colors) +
  labs(title = "(c) Mexico cumulative spent batteries (LDV + BESS)",
       y = "Million batteries", x = NULL, color = NULL, fill = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

p_combo <- (p1 | p2 | p3) +
  plot_annotation(
    title = "Why MX ACCII and MX Repeal look nearly identical on the geofacet",
    subtitle = paste0(
      "MX-ACCII uses ENME milestones (10/30/65/100% in 2025/30/35/40+).  ",
      "MX-Repeal borrows California's Repeal-scenario PR (already 12-19% in 2020-2024).  ",
      "\nRepeal is HIGHER than ACCII through 2031, then ACCII pulls ahead.  ",
      "Retirement lag (~12-15 yr) means the two effects cancel by 2050."
    ),
    theme = theme(
      plot.title    = element_text(size = 17, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey25")
    )
  )

out_path <- file.path(OUT_DIR, "Mexico_Scenario_Diagnostic.png")
ggsave(out_path, p_combo, width = 18, height = 6.5, dpi = 220, bg = "white")
cat("[Saved] ", out_path, "\n", sep = "")

## ---- 5.  Crossover summary ----
cat("\n=== PR cross-over (where Repeal stops being > ACCII) ===\n")
crossover <- pr_long %>% pivot_wider(names_from = Scenario, values_from = EV_PR) %>%
  mutate(diff = .[[3]] - .[[2]])  # ACCII - Repeal
print(crossover)
