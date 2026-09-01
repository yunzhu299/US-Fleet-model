## Export exact national annual cell-manufacturing scrap from the upstream model.

if (!exists("FLEET_SCENARIO") || is.null(FLEET_SCENARIO)) {
  FLEET_SCENARIO <- Sys.getenv("FLEET_SCENARIO", "ACCII")
}
if (!FLEET_SCENARIO %in% c("ACCII", "Repeal")) {
  stop("FLEET_SCENARIO must be ACCII or Repeal")
}

if (!exists("NA_manu", envir = environment(), inherits = TRUE) ||
    !is.data.frame(get("NA_manu", envir = environment(), inherits = TRUE))) {
  source(file.path("Scripts", "02-Recycling_Analysis", "02-Recycling_Analysis.R"))
}

out_dir <- file.path(
  "Outputs", "Recycling_Plots_main", FLEET_SCENARIO, "_parity"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  NA_manu %>%
    dplyr::select(
      Year,
      Tonnes_Scrap_proj_mid,
      Tonnes_Scrap_15_mid
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Tonnes_"), ~ .x / 1e6)
    ),
  file.path(out_dir, "NA_mfg_scrap_overtime.csv")
)
