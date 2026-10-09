## Check the actual CSVs after running the fleet pipeline from the repo root.
suppressPackageStartupMessages(library(dplyr))

for (scenario in c("ACCII", "Repeal")) {
  detail <- read.csv(paste0("Outputs/ClosedLoop_AddRetire_byStateSegment_", scenario, ".csv"))
  totals <- read.csv(paste0("Outputs/ClosedLoop_StateTotals_", scenario, ".csv"))
  expected_states <- c(state.name, "District of Columbia")
  stopifnot(setequal(unique(detail$State), expected_states),
            setequal(unique(totals$State), expected_states),
            setequal(unique(detail$Segment), c("Car", "SUV")),
            setequal(unique(detail$Year), 2020:2050),
            setequal(unique(totals$Year), 2020:2050),
            nrow(detail) == length(expected_states) * 2L * 31L,
            nrow(totals) == length(expected_states) * 31L,
            !anyDuplicated(detail[c("State", "Segment", "Year")]),
            !anyDuplicated(totals[c("State", "Year")]))

  ca <- totals %>% filter(State == "California", Year == 2025)
  stopifnot(nrow(ca) == 1L,
            abs(ca$add_BEV - 351043) < 1e-6,
            abs(ca$add_PHEV - 57325) < 1e-6)

  allocated <- detail %>% group_by(State, Year) %>%
    summarise(BEV = sum(add_BEV), PHEV = sum(add_PHEV), .groups = "drop")
  check <- full_join(allocated, totals, by = c("State", "Year"))
  stopifnot(all(is.finite(check$BEV)), all(is.finite(check$PHEV)),
            all(is.finite(check$add_BEV)), all(is.finite(check$add_PHEV)),
            all(check$BEV >= 0), all(check$PHEV >= 0),
            all(abs(check$BEV - check$add_BEV) < 1e-6),
            all(abs(check$PHEV - check$add_PHEV) < 1e-6))

  exports <- read.csv(paste0("Outputs/Exports_byYear_", scenario, ".csv"))
  expected_exports <- detail %>% group_by(Year) %>%
    summarise(Export_ICE = sum(exp_ICE), Export_BEV = sum(exp_BEV),
              Export_PHEV = sum(exp_PHEV),
              Export_All = Export_ICE + Export_BEV + Export_PHEV, .groups = "drop")
  stopifnot(!anyDuplicated(exports$Year), setequal(exports$Year, 2020:2050))
  exports <- exports[match(expected_exports$Year, exports$Year), names(expected_exports)]
  stopifnot(all(abs(as.matrix(exports[-1]) - as.matrix(expected_exports[-1])) < 1e-6))
  cat("Passed generated outputs:", scenario, "\n")
  print(totals %>% filter(State == "California", Year %in% 2024:2026) %>%
          select(Year, add_BEV, add_PHEV))
}
