#!/usr/bin/env Rscript

## Rebuild the recycling-derived figure input files efficiently.
##
## For each policy scenario, this sources 02-Recycling_Analysis.R once, then
## exports both:
##   - national manufacturing scrap for overtime figures
##   - state-level boundary inputs for geographic/capacity-needed figures

message("=== Rebuilding recycling data exports ===")

project_root <- normalizePath(getwd(), mustWork = TRUE)
scripts_dir <- file.path(project_root, "Scripts", "02-Recycling_Analysis")
if (!dir.exists(scripts_dir)) {
  stop("Run this script from the Fleet model project root.")
}

scenarios <- c("ACCII", "Repeal")

write_parity <- function(obj_name, file_name, out_dir, envir = parent.frame()) {
  if (!exists(obj_name, envir = envir, inherits = TRUE)) {
    message("  [parity skip] object not found: ", obj_name)
    return(invisible(FALSE))
  }

  obj <- get(obj_name, envir = envir, inherits = TRUE)
  if (!is.data.frame(obj)) {
    message("  [parity skip] object not data.frame: ", obj_name)
    return(invisible(FALSE))
  }

  readr::write_csv(obj, file.path(out_dir, file_name))
  message("  [parity] wrote ", file_name)
  invisible(TRUE)
}

build_region_mass_2050 <- function(state_ref) {
  region_mapping <- c(
    WA = "US-West", OR = "US-West", CA = "US-West", NV = "US-West",
    ID = "US-West", HI = "US-West", AK = "US-West",
    MT = "US-Mountain", WY = "US-Mountain", UT = "US-Mountain",
    CO = "US-Mountain", AZ = "US-Mountain", NM = "US-Mountain",
    OH = "US-Midwest", IN = "US-Midwest", IL = "US-Midwest", MI = "US-Midwest",
    WI = "US-Midwest", MN = "US-Midwest", IA = "US-Midwest", MO = "US-Midwest",
    ND = "US-Midwest", SD = "US-Midwest", NE = "US-Midwest", KS = "US-Midwest",
    TX = "US-South", OK = "US-South", AR = "US-South", LA = "US-South",
    KY = "US-South", TN = "US-South", MS = "US-South", AL = "US-South",
    ME = "US-East", NH = "US-East", VT = "US-East", MA = "US-East",
    RI = "US-East", CT = "US-East", NY = "US-East", NJ = "US-East",
    PA = "US-East", DE = "US-East", MD = "US-East", DC = "US-East",
    VA = "US-East", WV = "US-East", NC = "US-East", SC = "US-East",
    GA = "US-East", FL = "US-East",
    BC = "Canada-West", YT = "Canada-West",
    AB = "Canada-Mountain",
    MB = "Canada-Midwest", SK = "Canada-Midwest",
    ON = "Canada-East", QC = "Canada-East", NB = "Canada-East",
    NS = "Canada-East", PE = "Canada-East", NL = "Canada-East",
    NT = "Canada-East", NU = "Canada-East",
    MX = "Mexico", SLP = "Mexico"
  )

  state_ref %>%
    dplyr::mutate(
      State_Province = trimws(as.character(State_Province)),
      Region = unname(region_mapping[State_Province])
    ) %>%
    dplyr::filter(!is.na(Region), Region != "") %>%
    dplyr::group_by(Region, Origin) %>%
    dplyr::summarise(
      `Metric Tonnes (millions)` = sum(`Metric Tonnes (millions)`, na.rm = TRUE),
      .groups = "drop"
    )
}

for (scenario in scenarios) {
  message("")
  message("---- ", scenario, " ----")

  run_env <- new.env(parent = environment())
  run_env$FLEET_SCENARIO <- scenario
  run_env$DISABLE_PARITY_OVERRIDE <- TRUE

  source(
    file.path("Scripts", "02-Recycling_Analysis", "02-Recycling_Analysis.R"),
    local = run_env
  )

  parity_dir <- file.path("Outputs", "Recycling_Plots_main", scenario, "_parity")
  dir.create(parity_dir, recursive = TRUE, showWarnings = FALSE)

  write_parity("Mass_2050_projected", "Mass_2050_projected.csv", parity_dir, run_env)
  write_parity("Mass_2050_projected_ref", "States_Mass_2050_projected_ref.csv", parity_dir, run_env)
  if (exists("Mass_2050_projected_ref", envir = run_env, inherits = TRUE)) {
    readr::write_csv(
      build_region_mass_2050(get("Mass_2050_projected_ref", envir = run_env, inherits = TRUE)),
      file.path(parity_dir, "Regions_Mass_2050_projected_ref.csv")
    )
    message("  [parity] wrote Regions_Mass_2050_projected_ref.csv")
  }
  write_parity("cap_chem_results", "cap_chem_results.csv", parity_dir, run_env)
  write_parity("cap_chem_demand_results", "cap_chem_demand_results.csv", parity_dir, run_env)
  write_parity("non_recovery_lost", "non_recovery_lost.csv", parity_dir, run_env)
  write_parity("needed_cap_long", "needed_cap_long.csv", parity_dir, run_env)
  write_parity("export_lost", "export_lost.csv", parity_dir, run_env)
  write_parity("ratio_results", "ratio_results.csv", parity_dir, run_env)
  write_parity("overall_circularity", "overall_circularity.csv", parity_dir, run_env)
  write_parity("NA_manu", "NA_manu.csv", parity_dir, run_env)
  write_parity("manufacturing_by_state_projected", "manufacturing_by_state_projected.csv", parity_dir, run_env)
  write_parity("state_mass_recycle_batt", "state_mass_recycle_batt.csv", parity_dir, run_env)
  write_parity("recycling_tonnes_by_state", "recycling_tonnes_by_state.csv", parity_dir, run_env)

  source(
    file.path("Scripts", "02-Recycling_Analysis", "03-Recycling_R_Plots_and_Exports.R"),
    local = run_env
  )

  write_parity("Nat_Mass_2050_long", "Nat_Mass_2050_long.csv", parity_dir, run_env)
  write_parity("NA_plot_data", "NA_plot_data.csv", parity_dir, run_env)
  write_parity("NA_plot_data", "NA_overtime_data.csv", parity_dir, run_env)
  write_parity("nat_cap_chem_rec", "NA_cap_chem_rec.csv", parity_dir, run_env)
  write_parity("Mass_2050_region_ref", "Regions_Mass_2050_projected_ref.csv", parity_dir, run_env)
  write_parity("state_master_all_years", "state_master_all_years.csv", parity_dir, run_env)

  source(
    file.path("Scripts", "02-Recycling_Analysis", "11-Export_National_Mfg_Scrap.R"),
    local = run_env
  )
  source(
    file.path("Scripts", "02-Recycling_Analysis", "22-Export_Geographic_Boundary_Inputs.R"),
    local = run_env
  )
}

message("")
message("Export folders updated:")
message(" - ", file.path(project_root, "Outputs", "Recycling_Plots_main", "ACCII", "_parity"))
message(" - ", file.path(project_root, "Outputs", "Recycling_Plots_main", "Repeal", "_parity"))
message(" - ", file.path(project_root, "Outputs", "Recycling_Plots_main", "ACCII", "_boundary"))
message(" - ", file.path(project_root, "Outputs", "Recycling_Plots_main", "Repeal", "_boundary"))
message("=== Done ===")
