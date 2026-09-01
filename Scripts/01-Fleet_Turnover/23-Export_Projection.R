## 023 — Scenario-specific U.S. export projections for Mexico turnover
## Exports are produced by 005 from internally derived net-export factors.

library(dplyr)
library(readr)

SCENARIOS <- c("ACCII", "Repeal")

build_export_projection <- function(scenario) {
  input_file <- file.path(
    "Outputs", paste0("ClosedLoop_StateTotals_", scenario, ".csv")
  )
  if (!file.exists(input_file)) {
    stop("Missing U.S. fleet output: ", input_file,
         ". Run 05-US_Fleet_Simulation.R first.")
  }

  projection <- read_csv(input_file, show_col_types = FALSE) %>%
    group_by(Year) %>%
    summarise(
      Ret_ICE = sum(ret_ICE, na.rm = TRUE),
      Ret_BEV = sum(ret_BEV, na.rm = TRUE),
      Ret_PHEV = sum(ret_PHEV, na.rm = TRUE),
      Export_ICE = sum(exp_ICE, na.rm = TRUE),
      Export_BEV = sum(exp_BEV, na.rm = TRUE),
      Export_PHEV = sum(exp_PHEV, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Ret_EV = Ret_BEV + Ret_PHEV,
      Ret_Total = Ret_ICE + Ret_EV,
      Export_EV = Export_BEV + Export_PHEV,
      Export_Total = Export_ICE + Export_EV,
      export_factor = if_else(Ret_Total > 0, Export_Total / Ret_Total, 0),
      domestic_factor = 1 - export_factor,
      Domestic_ICE = Ret_ICE - Export_ICE,
      Domestic_BEV = Ret_BEV - Export_BEV,
      Domestic_PHEV = Ret_PHEV - Export_PHEV,
      Domestic_EV = Domestic_BEV + Domestic_PHEV,
      Domestic_Total = Ret_Total - Export_Total,
      Scenario = scenario
    ) %>%
    select(
      Year, Scenario, export_factor, domestic_factor,
      Ret_ICE, Ret_BEV, Ret_PHEV, Ret_EV, Ret_Total,
      Export_ICE, Export_BEV, Export_PHEV, Export_EV, Export_Total,
      Domestic_ICE, Domestic_BEV, Domestic_PHEV, Domestic_EV, Domestic_Total
    )

  output_file <- file.path(
    "Outputs", paste0("US_Export_Projection_", scenario, "_2020_2050.csv")
  )
  write_csv(projection, output_file)
  message("Saved: ", output_file)
  projection
}

export_results <- lapply(SCENARIOS, build_export_projection)
names(export_results) <- SCENARIOS

# Backward-compatible ACCII alias for downstream scripts not yet migrated.
write_csv(
  export_results[["ACCII"]] %>% select(-Scenario),
  "Outputs/US_Export_Projection_2020_2050.csv"
)
