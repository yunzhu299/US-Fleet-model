## Export state-level inputs required to decouple feedstock origin from
## recycling-facility network boundaries.

if (!exists("FLEET_SCENARIO") || is.null(FLEET_SCENARIO)) {
  FLEET_SCENARIO <- Sys.getenv("FLEET_SCENARIO", "ACCII")
}
if (!FLEET_SCENARIO %in% c("ACCII", "Repeal")) {
  stop("FLEET_SCENARIO must be ACCII or Repeal")
}

required_objects <- c(
  "cap_chem_results",
  "cap_chem_demand_results",
  "state_mass_recycle_batt",
  "manufacturing_by_state_projected",
  "recycling_tonnes_by_state"
)
missing_objects <- required_objects[
  !vapply(
    required_objects,
    function(obj) exists(obj, envir = environment(), inherits = TRUE),
    logical(1)
  )
]
if (length(missing_objects) > 0) {
  source(file.path("Scripts", "02-Recycling_Analysis", "02-Recycling_Analysis.R"))
}

out_dir <- file.path(
  "Outputs", "Recycling_Plots_main", FLEET_SCENARIO, "_boundary"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  cap_chem_results,
  file.path(out_dir, "mineral_components_state.csv")
)
readr::write_csv(
  cap_chem_demand_results,
  file.path(out_dir, "mineral_demand_state.csv")
)
readr::write_csv(
  state_mass_recycle_batt,
  file.path(out_dir, "eol_battery_mass_state.csv")
)
readr::write_csv(
  manufacturing_by_state_projected %>%
    select(
      Year, State_Province,
      `Increasing Batt Cap` = Tonnes_Scrap_proj_mid,
      `Decreasing Batt Cap` = Tonnes_Scrap_15_mid
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Increasing Batt Cap`, `Decreasing Batt Cap`),
      names_to = "Battery_Scenario",
      values_to = "Tonnes_Scrap"
    ) %>%
    tidyr::crossing(
      Chemistry_Scenario = c("Benchmark Chemistry", "High LFP Chemistry")
    ) %>%
    mutate(
      Scenario = paste(
        Battery_Scenario, Chemistry_Scenario, sep = " - "
      )
    ) %>%
    select(Year, State_Province, Scenario, Tonnes_Scrap),
  file.path(out_dir, "manufacturing_scrap_state.csv")
)
readr::write_csv(
  recycling_tonnes_by_state %>%
    select(
      Year, State_Province,
      Cumulative_black_mass_cap, Cumulative_refining_cap
    ),
  file.path(out_dir, "installed_capacity_state.csv")
)
