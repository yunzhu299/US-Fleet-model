## =====================================================================
## Run_Main_New.R   —   Minimal wrapper around colleague's main7 pipeline.
##
## DESIGN GOAL (per user request):
##   * Only do PATH REMAPPING — Elsa's /Users/elsawefes-potter/.../Pablo/...
##     paths are rewritten to point at THIS user's Inputs/ and Outputs/.
##   * NO data-altering wrappers (no read.csv shim, no sheet remap helpers,
##     no Scrap_mass shim "from xlsx"). All transformations are visible as
##     pure-text substitutions on the colleague's R source.
##   * Follow Elsa's documented run order verbatim (from the comments at the
##     top of  Future Recycling Minerals_new.R ).
##   * Save every printed ggplot into a NEW folder:
##         Outputs/Recycling_Plots_main_new/<FLEET_SCEN>/
##
## RUN ORDER  (copied from  Future Recycling Minerals_new.R  L16-L31):
##   Phase 1 — one-time setup:
##     EV Volumes Clean.R
##     HMDV.R
##     Historical Sales Minerals.R
##     Scenarios_SetUp.R
##   Phase 2 — main pipeline (run with FLEET_SCEN=ACCII):
##     Manufacturing_Recycling_Demand.R
##     Future Recycling Minerals_new.R
##     Plotting_Demand_Recycle_Manu.R
##     Future Demand Minerals.R
##   Phase 3 — Delay variant (requires manual code edits inside the
##             colleague's scripts; this wrapper does NOT auto-toggle it):
##     Manufacturing_Recycling_Demand.R
##     Future Recycling Minerals_new.R
##     Future Demand Minerals.R
##   Phase 4 — Repeal (re-run everything from Manufacturing onward):
##     FLEET_SCEN=Repeal Rscript Scripts/Recycling_Main_Runner/Run_Main_New.R
##
## USAGE:
##   Rscript Scripts/Recycling_Main_Runner/Run_Main_New.R                 # ACCII
##   FLEET_SCEN=Repeal Rscript Scripts/Recycling_Main_Runner/Run_Main_New.R
##
## REQUIRED INPUT FILES (in Inputs/ — see PREREQ FILES block below):
##   The colleague reads two CSV files that do NOT exist in this user's
##   Inputs/ today.  If they are missing this script STOPS with a clear
##   message rather than silently substituting a derived version (that was
##   the source of the discrepancy you saw vs Elsa's numbers):
##       Scrap_by_Mass (-Energy).csv         (Elsa exported from xlsx)
##       Scrap_mass(-Energy BatPac).csv      (Elsa's per-cathode scrap)
##   Drop the two files into Inputs/ before re-running.
## =====================================================================


## --- 0.  Configuration -----------------------------------------------

if (!exists("FLEET_SCEN")) {
  FLEET_SCEN <- Sys.getenv("FLEET_SCEN", "ACCII")
}
if (!FLEET_SCEN %in% c("ACCII", "Repeal")) {
  stop("FLEET_SCEN must be 'ACCII' or 'Repeal'; got '", FLEET_SCEN, "'")
}

PROJECT_ROOT <- getwd()
MAIN_DIR     <- file.path(PROJECT_ROOT, "Scripts", "Recycling",
                          "US-fleet-modeling-main 7")
INPUTS_DIR   <- file.path(PROJECT_ROOT, "Inputs")
OUTPUTS_DIR  <- file.path(PROJECT_ROOT, "Outputs")
PLOT_ROOT    <- file.path(OUTPUTS_DIR, "Recycling_Plots_main_new")
PLOT_DIR     <- file.path(PLOT_ROOT, FLEET_SCEN)
TMP_DIR      <- file.path(PLOT_ROOT, "_tmp")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TMP_DIR,  recursive = TRUE, showWarnings = FALSE)

cat("=== Run_Main_New.R ===\n")
cat("  FLEET_SCEN  :", FLEET_SCEN, "\n")
cat("  MAIN_DIR    :", MAIN_DIR,   "\n")
cat("  INPUTS_DIR  :", INPUTS_DIR, "\n")
cat("  OUTPUTS_DIR :", OUTPUTS_DIR,"\n")
cat("  PLOT_DIR    :", PLOT_DIR,   "\n\n")
stopifnot(dir.exists(MAIN_DIR), dir.exists(INPUTS_DIR), dir.exists(OUTPUTS_DIR))


## --- 1.  Libraries (replaces Elsa's 00-Libraries.R) ------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(colorspace)
  library(ggpattern)
  library(patchwork)
  library(scales)
  library(geofacet)
  library(data.table)
  library(ggforce)
})

## Colleague scripts call install.packages / library(devtools) at runtime.
## These are environment setup only — they do not change data values.
install.packages <- function(pkgs, ...) {
  message("  (skip install.packages: ", paste(pkgs, collapse = ", "), ")")
  invisible(NULL)
}
.orig_library <- base::library
library <- function(package, ...) {
  pkg <- as.character(substitute(package))
  if (pkg %in% c("devtools") && !requireNamespace(pkg, quietly = TRUE)) {
    message("  (skip library: ", pkg, " — not installed, optional)")
    return(invisible(NULL))
  }
  .orig_library(pkg, character.only = TRUE, ...)
}
View <- function(x, title = NULL, ...) invisible(NULL)


## --- 2.  Pre-flight: ensure the two colleague-only CSV files exist ---
##
## (a) Scrap_by_Mass (-Energy).csv
##     Manufacturing_Recycling_Demand.R L293 reads this via read_csv()
##     but the user (and colleague's repo) only ships the .xlsx version.
##     We rebuild the CSV from the XLSX with skip = 1 — this is a pure
##     format conversion and the numeric values are identical.
##
## (b) Scrap_mass(-Energy BatPac).csv
##     Future Recycling Minerals_new.R L43 reads this via read_csv() but
##     the file is NOT in Elsa's public repo and the schema (Battery Chem,
##     Scrap kg/Gwh) cannot be derived deterministically from anything in
##     Inputs/.  The previous Run_Main.R synthesised it as
##     `Scrap kg/Gwh = Total Mass / 211`, which is almost certainly the
##     reason your recycling minerals numbers diverged from Elsa's.
##     STOP loudly until the user supplies Elsa's actual file.

## (a) Build Scrap_by_Mass (-Energy).csv from .xlsx if missing
scrap_by_mass_csv <- file.path(INPUTS_DIR, "Scrap_by_Mass (-Energy).csv")
scrap_by_mass_xlsx <- file.path(INPUTS_DIR, "Scrap_by_Mass (-Energy).xlsx")
if (!file.exists(scrap_by_mass_csv)) {
  if (!file.exists(scrap_by_mass_xlsx)) {
    stop("Missing both Scrap_by_Mass (-Energy).csv AND .xlsx in Inputs/.")
  }
  cat("  Converting xlsx -> csv: ", basename(scrap_by_mass_csv), "\n", sep = "")
  readxl::read_excel(scrap_by_mass_xlsx, skip = 1, na = "") |>
    readr::write_csv(scrap_by_mass_csv, na = "")
}

## (b) Refuse to fabricate Scrap_mass(-Energy BatPac).csv
scrap_mass_csv <- file.path(INPUTS_DIR, "Scrap_mass(-Energy BatPac).csv")
if (!file.exists(scrap_mass_csv)) {
  message("\n[Run_Main_New.R] Missing Elsa-only CSV in Inputs/:")
  message("    - ", basename(scrap_mass_csv))
  message("This file is NOT in the colleague's public repo and CANNOT be ",
          "derived without an assumption that previously broke parity ",
          "with Elsa's numbers.\n",
          "Ask Elsa for the file and drop it into:\n    ", INPUTS_DIR, "\n",
          "then rerun.")
  stop("Aborting: ", basename(scrap_mass_csv), " missing.")
}


## --- 3.  Path remapping (PURE TEXT SUBSTITUTION on colleague source) --

.PREFIX <- "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo"

remap_script <- function(text, scenario = FLEET_SCEN) {

  ## (0) Drop 00-Libraries.R sourcing BEFORE prefix remap turns it into
  ##     Inputs/00-Libraries.R (which does not exist on this machine).
  text <- gsub('source\\([^)]*00-Libraries\\.R[^)]*\\)',
               '## (00-Libraries.R removed by Run_Main_New.R)',
               text, perl = TRUE)

  ## (a) Final_Data/<COUNTRY>_<file> -> Outputs/[<COUNTRY>/]<file>
  text <- gsub(paste0(.PREFIX, "/Final_Data/US_"),
               paste0(OUTPUTS_DIR, "/"),          text, fixed = TRUE)
  text <- gsub(paste0(.PREFIX, "/Final_Data/Canada_"),
               paste0(OUTPUTS_DIR, "/Canada/"),   text, fixed = TRUE)
  text <- gsub(paste0(.PREFIX, "/Final_Data/Mexico_"),
               paste0(OUTPUTS_DIR, "/Mexico/"),   text, fixed = TRUE)
  text <- gsub(paste0(.PREFIX, "/Final_Data/HDV_"),
               paste0(OUTPUTS_DIR, "/HDV/HDV_"),  text, fixed = TRUE)
  text <- gsub(paste0(.PREFIX, "/Final_Data/"),
               paste0(OUTPUTS_DIR, "/"),          text, fixed = TRUE)

  ## (b) Everything else under Elsa's prefix -> Inputs/
  text <- gsub(paste0('"', .PREFIX, '/'),
               paste0('"', INPUTS_DIR, '/'),      text, fixed = TRUE)
  text <- gsub(paste0('"', .PREFIX, '"'),
               paste0('"', INPUTS_DIR, '"'),      text, fixed = TRUE)

  ## (c) Filename mismatches (Elsa's name -> user's actual filename)
  text <- gsub("Cathode Mix Update.xlsx",      "Cathode Mix update.xlsx",      text, fixed = TRUE)
  text <- gsub("Cathode Projections.xlsx",     "Cathode Projections (1).xlsx", text, fixed = TRUE)
  text <- gsub("Manu_Down_Mid.xlsx",           "Manu_Mid_Down.xlsx",           text, fixed = TRUE)
  text <- gsub("NA recycling facilities.xlsx", "NA Recycling facilities.xlsx", text, fixed = TRUE)
  text <- gsub('sheet = "changed dates Narrowed Manu fac"',
               'sheet = "Narrowed Manu facilities"',                           text, fixed = TRUE)

  ## (d) Scenario suffix substitution on Final_Data CSV reads.
  ##     Colleague's main7 hard-codes "_Repeal" everywhere; for ACCII we
  ##     just swap the suffix.  For Repeal we leave the file as-is.
  if (scenario != "Repeal") {
    text <- gsub("_Repeal.csv", paste0("_", scenario, ".csv"),
                 text, fixed = TRUE)
  }

  ## (e) source("...00-Libraries.R") — already handled in step (0)
  text <- gsub('source\\([^)]*Inputs/00-Libraries\\.R[^)]*\\)',
               '## (00-Libraries.R removed by Run_Main_New.R)',
               text, perl = TRUE)

  ## (f) install.packages(...) / devtools::install_github(...) -> comment
  ##     These attempt internet/CRAN/devtools at runtime; required packages
  ##     are already loaded in step 1.  This is code-only and does not
  ##     change data values.
  text <- gsub('^\\s*install\\.packages\\([^)]*\\)\\s*$',
               '## (install.packages call removed by Run_Main_New.R)',
               text, perl = TRUE)
  text <- gsub('^\\s*devtools::install_github\\([^)]*\\)\\s*$',
               '## (devtools::install_github call removed by Run_Main_New.R)',
               text, perl = TRUE)

  ## (g) write_csv(historical_state_pt_veh_df, "...historical_state_pt_veh_df.csv")
  ##     EV Volumes Clean.R writes this CSV (consumed nowhere else here).
  ##     Redirect Elsa's write target to TMP_DIR so the run does not litter
  ##     Inputs/.
  text <- gsub(paste0('"', INPUTS_DIR, '/historical_state_pt_veh_df.csv"'),
               paste0('"', file.path(TMP_DIR, 'historical_state_pt_veh_df.csv'), '"'),
               text, fixed = TRUE)

  text
}


## --- 4.  Plot capture (the ONLY runtime wrapper) ---------------------
##
## Reason: Rscript batch mode does NOT auto-print top-level expressions,
## so naked `ggplot(...) + ...` calls would silently vanish.  We mimic the
## interactive REPL by parsing each script and, for any visible top-level
## value that inherits "ggplot", ggsave() it into PLOT_DIR.
##
## NOTE: we deliberately ggsave() directly on the value instead of relying
## on an S3 print.ggplot override, because some plot subclasses (e.g.
## geofacet's `facet_geo`, class c("facet_geo","gg","ggplot")) ship their
## OWN print method that takes priority over print.ggplot and would bypass
## the save.  Checking inherits(value, "ggplot") catches every subclass.
## This affects PLOT OUTPUT ONLY, never data.

.plot_counter   <- 0L
.current_script <- ""

.save_ggplot <- function(x) {
  .plot_counter <<- .plot_counter + 1L
  base  <- if (nzchar(.current_script)) sub("\\.R$", "", .current_script) else "plot"
  fname <- file.path(PLOT_DIR, sprintf("%s_%02d.png", base, .plot_counter))
  tryCatch({
    ggplot2::ggsave(fname, plot = x, width = 12, height = 8, dpi = 150,
                    bg = "white", limitsize = FALSE)
    message("    [saved] ", basename(fname))
  }, error = function(e) {
    message("    [ggsave failed] plot ", .plot_counter, ": ", conditionMessage(e))
  })
}


## --- 5.  Script runner (parse + remap + eval per top-level expr) -----

run_colleague <- function(script_name) {
  full <- file.path(MAIN_DIR, script_name)
  if (!file.exists(full)) {
    message("[skip] not found: ", full); return(invisible())
  }
  cat("\n==== Running:", script_name, "====\n")
  .current_script <<- script_name

  text  <- paste(readLines(full, warn = FALSE), collapse = "\n")
  text  <- remap_script(text)

  exprs <- tryCatch(parse(text = text, keep.source = TRUE),
                    error = function(e) {
                      message("  [parse error] ", conditionMessage(e)); NULL
                    })
  if (is.null(exprs)) return(invisible())

  for (i in seq_along(exprs)) {
    res <- tryCatch(
      withVisible(eval(exprs[[i]], envir = globalenv())),
      error = function(e) {
        message("  [error] ", script_name, " expr#", i, ": ",
                conditionMessage(e))
        list(value = NULL, visible = FALSE)
      }
    )
    if (isTRUE(res$visible) && !is.null(res$value) &&
        inherits(res$value, "ggplot")) {
      .save_ggplot(res$value)
    }
  }
}


## --- 6.  Run Elsa's documented order ---------------------------------

scripts_in_order <- c(
  ## Phase 1 — one-time setup
  "EV Volumes Clean.R",
  "HMDV.R",
  "Historical Sales Minerals.R",
  "Scenarios_SetUp.R",
  ## Phase 2 — main pipeline (ACCII)  OR  Phase 4 — Repeal rerun
  "Manufacturing_Recycling_Demand.R",
  "Future Recycling Minerals_new.R",
  "Plotting_Demand_Recycle_Manu.R",
  "Future Demand Minerals.R"
)

for (s in scripts_in_order) run_colleague(s)

## --- 7. Export result CSVs (parity-style outputs) ---------------------
## Keep the previously exported analysis tables so downstream comparison
## workflows still work with Run_Main_New.
PARITY_DIR <- file.path(PLOT_DIR, "_parity")
if (!dir.exists(PARITY_DIR)) dir.create(PARITY_DIR, recursive = TRUE)

.safe_write_parity <- function(obj_name, file_name) {
  if (!exists(obj_name, inherits = TRUE)) {
    message("  [parity skip] object not found: ", obj_name)
    return(invisible(FALSE))
  }
  obj <- get(obj_name, inherits = TRUE)
  if (!is.data.frame(obj)) {
    message("  [parity skip] object not data.frame: ", obj_name)
    return(invisible(FALSE))
  }
  out <- file.path(PARITY_DIR, file_name)
  utils::write.csv(obj, out, row.names = FALSE)
  message("  [parity] wrote ", basename(out))
  invisible(TRUE)
}

.safe_write_parity("Mass_2050_projected", "Mass_2050_projected.csv")
.safe_write_parity("States_Mass_2050_projected_ref", "States_Mass_2050_projected_ref.csv")
.safe_write_parity("Regions_Mass_2050_projected_ref", "Regions_Mass_2050_projected_ref.csv")
.safe_write_parity("NA_Mass_2050_long", "Nat_Mass_2050_long.csv")
.safe_write_parity("NA_plot_data", "NA_plot_data.csv")
.safe_write_parity("NA_overtime_data", "NA_overtime_data.csv")
.safe_write_parity("NA_manu", "NA_manu.csv")
.safe_write_parity("NA_cap_chem_rec", "NA_cap_chem_rec.csv")
.safe_write_parity("cap_chem_results", "cap_chem_results.csv")
.safe_write_parity("cap_chem_demand_results", "cap_chem_demand_results.csv")
.safe_write_parity("non_recovery_lost", "non_recovery_lost.csv")
.safe_write_parity("needed_cap_long", "needed_cap_long.csv")
.safe_write_parity("export_lost", "export_lost.csv")
.safe_write_parity("ratio_results", "ratio_results.csv")
.safe_write_parity("overall_circularity", "overall_circularity.csv")
.safe_write_parity("state_mass_recycle_batt", "state_mass_recycle_batt.csv")
.safe_write_parity("manufacturing_by_state_projected", "manufacturing_by_state_projected.csv")
.safe_write_parity("recycling_tonnes_by_state", "recycling_tonnes_by_state.csv")

## Build all-year state master source (2025-2050) when available.
try({
  if (exists("state_cap_chem_tonne", inherits = TRUE) &&
      exists("manufacturing_by_state_projected", inherits = TRUE) &&
      exists("recycling_tonnes_by_state", inherits = TRUE) &&
      exists("state_mass_recycle_batt", inherits = TRUE)) {
    state_master_all_years <- state_cap_chem_tonne %>%
      dplyr::select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes) %>%
      dplyr::full_join(
        manufacturing_by_state_projected %>%
          dplyr::select(
            Year, State_Province,
            Tonnes_Prod_proj_down, Tonnes_Prod_15_down,
            Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid
          ),
        by = c("Year", "State_Province")
      ) %>%
      dplyr::full_join(
        recycling_tonnes_by_state %>%
          dplyr::select(Year, State_Province, Cumulative_black_mass_cap, Cumulative_refining_cap),
        by = c("Year", "State_Province")
      ) %>%
      dplyr::full_join(
        state_mass_recycle_batt %>%
          dplyr::filter(Scenario %in% c(
            "Increasing Batt Cap - Benchmark Chemistry",
            "Decreasing Batt Cap - Benchmark Chemistry"
          )) %>%
          dplyr::transmute(
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
          dplyr::rename(
            Recycle_Batt_Proj = `Increasing Batt Cap - Benchmark Chemistry`,
            Recycle_Batt_15   = `Decreasing Batt Cap - Benchmark Chemistry`
          ),
        by = c("Year", "State_Province")
      ) %>%
      dplyr::transmute(
        Year = as.integer(Year),
        State_Province = dplyr::if_else(
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
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
      dplyr::group_by(Year, State_Province) %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE)),
                       .groups = "drop")

    .safe_write_parity("state_master_all_years", "state_master_all_years.csv")
  } else {
    message("  [parity skip] state_master_all_years prerequisites not found")
  }
}, silent = TRUE)

cat("\n==== Run_Main_New.R complete ====\n")
cat("  FLEET_SCEN :", FLEET_SCEN, "\n")
cat("  Saved", .plot_counter, "plots to:\n    ", PLOT_DIR, "\n")
cat("\n[Phase 3 - Delay variant]\n",
    "  Delay outputs require manual code edits inside the colleague's\n",
    "  scripts (toggle US_CA_Recycle -> Delay_US_CA_Recycle and similar).\n",
    "  This wrapper does NOT automate that; rerun this script after the\n",
    "  edits with FLEET_SCEN unchanged.\n", sep = "")
