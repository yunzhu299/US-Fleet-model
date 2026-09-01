## ====================================================================
## Run_Main.R  —  Run the colleague's main5 recycling pipeline AS-IS
##                and dump every plot into a dedicated folder.
##
## What this wrapper does (without modifying the main5 scripts):
##   1.  Loads the same R libraries as the colleague's 00-Libraries.R
##   2.  Replaces colleague-specific absolute paths
##         (/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/...)
##       with the corresponding files in:
##           Scripts/Recycling/US-fleet-modeling-main 5/
##           Inputs/
##           Outputs/[Canada/Mexico/HDV]/...
##   3.  Forces every top-level ggplot expression to print AND ggsave a
##       PNG into Outputs/Recycling_Plots_main/<scenario>/
##       (Rscript batch mode normally drops top-level ggplots silently.)
##
## Usage:
##   Rscript Scripts/Recycling_Main_Runner/Run_Main.R                # ACCII
##   FLEET_SCEN=Repeal Rscript Scripts/Recycling_Main_Runner/Run_Main.R
##
## All plots end up in:  Outputs/Recycling_Plots_main/<FLEET_SCEN>/
## ====================================================================

## --- 0.  Configuration ----------------------------------------------

if (!exists("FLEET_SCEN")) {
  FLEET_SCEN <- Sys.getenv("FLEET_SCEN", "ACCII")
}
if (!FLEET_SCEN %in% c("ACCII", "Repeal")) {
  stop("FLEET_SCEN must be 'ACCII' or 'Repeal'; got '", FLEET_SCEN, "'")
}

PROJECT_ROOT <- getwd()
MAIN5_DIR    <- file.path(PROJECT_ROOT, "Scripts", "Recycling",
                          "US-fleet-modeling-main 7")
INPUTS_DIR   <- file.path(PROJECT_ROOT, "Inputs")
OUTPUTS_DIR  <- file.path(PROJECT_ROOT, "Outputs")
PLOT_DIR     <- file.path(OUTPUTS_DIR, "Recycling_Plots_main", FLEET_SCEN)
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=== Run_Main.R ===\n")
cat("  FLEET_SCEN  :", FLEET_SCEN, "\n")
cat("  MAIN5_DIR   :", MAIN5_DIR,  "\n")
cat("  PLOT_DIR    :", PLOT_DIR,   "\n\n")

stopifnot(dir.exists(MAIN5_DIR))


## --- 1.  Libraries (replaces colleague's 00-Libraries.R) -------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(forcats)
  library(tibble)
  library(ggplot2)
  library(scales)
  library(openxlsx)
  library(geofacet)
  library(writexl)
  if (requireNamespace("colorspace", quietly = TRUE)) library(colorspace)
  if (requireNamespace("patchwork",  quietly = TRUE)) library(patchwork)
  if (requireNamespace("ggforce",    quietly = TRUE)) library(ggforce)
  if (requireNamespace("ggpattern",  quietly = TRUE)) library(ggpattern)
  if (requireNamespace("tidyverse",  quietly = TRUE)) library(tidyverse)
})

## install.packages calls inside main5 try to write to a read-only lib
## dir on this machine. All needed packages are already installed, so
## intercept install.packages as a no-op.
install.packages <- function(pkgs, ...) {
  message("  (skip install.packages: ", paste(pkgs, collapse = ", "), ")")
  invisible(NULL)
}

## library() may be called for optional packages we don't have (devtools).
## Wrap to silently skip missing optional packages.
.OPTIONAL_LIBS <- c("devtools")
.orig_library <- base::library
library <- function(package, ...) {
  pkg <- as.character(substitute(package))
  if (pkg %in% .OPTIONAL_LIBS && !requireNamespace(pkg, quietly = TRUE)) {
    message("  (skip library: ", pkg, " — not installed, optional)")
    return(invisible(NULL))
  }
  .orig_library(pkg, character.only = TRUE, ...)
}


## --- 1b. Build shim files for colleague-only inputs ------------------
## Scrap_mass(-Energy BatPac).csv  is referenced in main5 but doesn't
## exist locally. We derive an equivalent table from the local
## Scrap_by_Mass (-Energy).xlsx, where Total Mass / 211 (GWh) yields
## the per-cathode "Scrap kg/Gwh" used by Future Recycling Minerals_new.R.

SHIM_DIR <- file.path(OUTPUTS_DIR, "Recycling_Plots_main", "_shim")
dir.create(SHIM_DIR, recursive = TRUE, showWarnings = FALSE)

scrap_shim_path <- file.path(SHIM_DIR, "Scrap_mass(-Energy BatPac).csv")
if (!file.exists(scrap_shim_path)) {
  cat("  Building shim:", scrap_shim_path, "\n")
  scrap_src <- readxl::read_excel(
    file.path(INPUTS_DIR, "Scrap_by_Mass (-Energy).xlsx"),
    skip = 1, na = ""
  ) %>%
    dplyr::select(Chemistry, `Total Mass`) %>%
    dplyr::filter(!is.na(Chemistry), !is.na(`Total Mass`)) %>%
    dplyr::transmute(
      `Battery Chem` = Chemistry,
      `Scrap kg/Gwh` = `Total Mass` / 211    # 211 GWh assumption (matches Consolidated)
    )
  readr::write_csv(scrap_src, scrap_shim_path)
}


## --- 2.  Path remapping ---------------------------------------------

.PREFIX <- "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/"

.path_map <- function(p) {
  if (!is.character(p) || length(p) != 1L) return(p)
  if (is.na(p)) return(p)
  if (!startsWith(p, .PREFIX)) return(p)

  rest <- substr(p, nchar(.PREFIX) + 1L, nchar(p))

  ## (a) 00-Libraries.R — already loaded above, skip
  if (rest == "00-Libraries.R") return(NA_character_)

  ## (b) Final_Data/<US|Canada|Mexico|HDV>_<file>_<scen>.csv  →  Outputs/...
  ## v6 of main hard-codes "_Repeal" in some file paths; v5 used "_ACCII".
  ## Normalize EITHER suffix to FLEET_SCEN so the wrapper drives the run.
  if (startsWith(rest, "Final_Data/")) {
    fname <- sub("^Final_Data/", "", rest)
    fname <- sub("_(ACCII|Repeal)\\.csv$",
                 paste0("_", FLEET_SCEN, ".csv"), fname)
    if (startsWith(fname, "US_"))
      return(file.path(OUTPUTS_DIR,           sub("^US_",     "", fname)))
    if (startsWith(fname, "Canada_"))
      return(file.path(OUTPUTS_DIR, "Canada", sub("^Canada_", "", fname)))
    if (startsWith(fname, "Mexico_"))
      return(file.path(OUTPUTS_DIR, "Mexico", sub("^Mexico_", "", fname)))
    if (startsWith(fname, "HDV_"))
      return(file.path(OUTPUTS_DIR, "HDV", fname))
    return(file.path(OUTPUTS_DIR, fname))
  }

  ## (c) Hand-built shims for files that don't exist locally
  if (rest == "Scrap_mass(-Energy BatPac).csv") return(scrap_shim_path)

  ## (d) Other files — search main5/, Inputs/, with common name swaps
  swap_candidates <- c(
    rest,
    sub("Manu_Down_Mid", "Manu_Mid_Down", rest, fixed = TRUE),
    sub("\\.csv$", ".xlsx", rest)
  )
  search_dirs <- c(MAIN5_DIR, INPUTS_DIR)
  for (d in search_dirs) {
    for (cand in swap_candidates) {
      full <- file.path(d, cand)
      if (file.exists(full)) return(full)
    }
  }

  warning("path_map: cannot resolve '", p, "' (rest='", rest, "')",
          call. = FALSE)
  p   # last resort — let downstream fail loudly
}


## --- 3.  Wrapper readers ---------------------------------------------
## Park them in a dedicated environment that we attach to the search
## path with HIGHER priority than any package, so subsequent
## library(tidyverse) / library(readr) calls cannot mask them.

.shim_env <- new.env(parent = emptyenv())

.shim_env$read_csv <- function(file, ...) {
  np <- .path_map(file)
  if (is.na(np)) return(invisible(NULL))
  if (endsWith(tolower(np), ".xlsx") &&
      !endsWith(tolower(file), ".xlsx")) {
    return(readxl::read_excel(np))
  }
  readr::read_csv(np, show_col_types = FALSE, ...)
}

.shim_env$read.csv <- function(file, ...) {
  np <- .path_map(file)
  if (is.na(np)) return(invisible(NULL))
  if (endsWith(tolower(np), ".xlsx") &&
      !endsWith(tolower(file), ".xlsx")) {
    return(as.data.frame(readxl::read_excel(np)))
  }
  utils::read.csv(np, ...)
}

## Sheet-name remap: scripts ask for sheets that don't exist in our
## local Manu_Mid_Down.xlsx; redirect to closest matching sheet.
.SHEET_REMAP <- list(
  "changed dates Narrowed Manu fac" = "Narrowed Manu facilities"
)

.remap_sheet <- function(sheet, np) {
  if (missing(sheet) || is.null(sheet) || is.numeric(sheet)) return(sheet)
  if (!is.character(sheet) || length(sheet) != 1L) return(sheet)
  if (sheet %in% names(.SHEET_REMAP)) {
    avail <- tryCatch(readxl::excel_sheets(np), error = function(e) character())
    if (!sheet %in% avail) return(.SHEET_REMAP[[sheet]])
  }
  sheet
}

.shim_env$read_xlsx <- function(path, sheet = 1, ...) {
  np <- .path_map(path)
  if (is.na(np)) return(invisible(NULL))
  readxl::read_xlsx(np, sheet = .remap_sheet(sheet, np), ...)
}

.shim_env$read_excel <- function(path, sheet = NULL, ...) {
  np <- .path_map(path)
  if (is.na(np)) return(invisible(NULL))
  readxl::read_excel(np, sheet = .remap_sheet(sheet, np), ...)
}

## write_*  -> redirect colleague-only paths to SHIM_DIR.
.redirect_write_path <- function(path) {
  if (!is.character(path) || length(path) != 1L) return(path)
  if (startsWith(path, .PREFIX)) {
    new_path <- file.path(SHIM_DIR, basename(path))
    message("  (redirect write -> ", basename(new_path), ")")
    return(new_path)
  }
  path
}

.shim_env$write_xlsx <- function(x, path, ...) {
  writexl::write_xlsx(x, .redirect_write_path(path), ...)
}

.shim_env$write_csv <- function(x, file, ...) {
  readr::write_csv(x, .redirect_write_path(file), ...)
}

.shim_env$write.csv <- function(x, file = "", ...) {
  utils::write.csv(x, .redirect_write_path(file), ...)
}

.shim_env$write.xlsx <- function(x, file, ...) {
  openxlsx::write.xlsx(x, .redirect_write_path(file), ...)
}

.shim_env$source <- function(file, ...) {
  np <- .path_map(file)
  if (is.na(np)) {
    message("  (skip source: ", file, ")")
    return(invisible(NULL))
  }
  base::source(np, ...)
}

## View() is interactive-only; in batch mode it tries to open X11
## ("X11 library is missing") which torpedoes tryCatch'd scenario loops
## in main5. Stub it (and graphics device openers) to no-ops.
.shim_env$View <- function(x, title = NULL, ...) invisible(NULL)
.shim_env$x11  <- function(...) invisible(NULL)
.shim_env$X11  <- function(...) invisible(NULL)

## Re-attach the shim env right before each script runs so that any
## library(...) call inside a script cannot bury our masks.
attach_shims <- function() {
  while ("RunMainShims" %in% search()) detach("RunMainShims", character.only = TRUE)
  do.call(base::attach, list(.shim_env, name = "RunMainShims", warn.conflicts = FALSE))
}
attach_shims()


## --- 4.  Plot capture ------------------------------------------------
## Override print.ggplot in globalenv so every printed ggplot also gets
## ggsave'd into PLOT_DIR.

.plot_counter   <- 0L
.current_script <- ""
.original_print_ggplot <- ggplot2:::print.ggplot

print.ggplot <- function(x, ...) {
  .plot_counter <<- .plot_counter + 1L
  base <- if (nzchar(.current_script)) {
    sub("\\.R$", "", .current_script)
  } else "plot"
  fname <- file.path(PLOT_DIR, sprintf("%s_%02d.png", base, .plot_counter))
  tryCatch({
    ggplot2::ggsave(fname, plot = x, width = 12, height = 8, dpi = 150,
                    bg = "white", limitsize = FALSE)
    message("    [saved] ", basename(fname))
  }, error = function(e) {
    message("    [ggsave failed] plot ", .plot_counter, ": ",
            conditionMessage(e))
  })
  .original_print_ggplot(x, ...)
}
registerS3method("print", "ggplot", print.ggplot, envir = globalenv())


## --- 5.  Force top-level expression auto-print -----------------------
## Rscript batch mode does NOT auto-print top-level expressions, so
## naked `ggplot(...) + ...` calls vanish. We mimic interactive REPL by
## parsing the script and explicitly printing visible top-level results.

run_script_with_print <- function(path) {
  exprs <- tryCatch(
    parse(file = path, keep.source = TRUE, encoding = "UTF-8"),
    error = function(e) {
      message("PARSE ERROR in ", basename(path), ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(exprs)) return(invisible(NULL))

  for (i in seq_along(exprs)) {
    res <- tryCatch(
      withVisible(eval(exprs[[i]], envir = globalenv())),
      error = function(e) {
        message("  [error] ", basename(path), " expr#", i, ": ",
                conditionMessage(e))
        list(value = NULL, visible = FALSE)
      }
    )
    if (isTRUE(res$visible) && !is.null(res$value)) {
      try(print(res$value), silent = TRUE)
    }
  }
}


## --- 6.  Run main5 scripts in dependency order -----------------------

## Run order — matches colleague's protocol:
##   Phase 1 (one-time setup):
##     EV Volumes Clean.R           — preps sales data
##     HMDV.R                        — creates HDV_chem_*, HDV_avg_cap*
##     Historical Sales Minerals.R   — creates batt_cap_merged
##     Scenarios_SetUp.R             — uses batt_cap_merged + HDV_chem_project
##   Phase 2 (main pipeline, ACCII):
##     Manufacturing_Recycling_Demand.R
##     Future Recycling Minerals_new.R
##     Plotting_Demand_Recycle_Manu.R
##     Future Demand Minerals.R
##   (Phase 3 'Delay' variant and Phase 4 'Repeal' rerun are NOT automated
##    here — they require in-script edits. Run separately if needed.)
scripts_in_order <- c(
  "EV Volumes Clean.R",
  "HMDV.R",
  "Historical Sales Minerals.R",
  "Scenarios_SetUp.R",
  "Manufacturing_Recycling_Demand.R",
  "Future Recycling Minerals_new.R",
  "Plotting_Demand_Recycle_Manu.R",
  "Future Demand Minerals.R"
)

for (s in scripts_in_order) {
  cat("\n==== Running:", s, "====\n")
  .current_script <- s
  full <- file.path(MAIN5_DIR, s)
  if (!file.exists(full)) {
    message("  [skip] not found: ", full)
    next
  }
  attach_shims()                # re-mask in case prior library() buried us
  run_script_with_print(full)
}

## --- 7. Export parity data for consolidated pipeline -----------------
## These CSVs let Recycling_Consolidated reuse the exact same objects that
## main plotting consumed, so figure totals can be kept 1:1 with main.
PARITY_DIR <- file.path(OUTPUTS_DIR, "Recycling_Plots_main", FLEET_SCEN, "_parity")
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
.safe_write_parity("NA_cap_chem_rec", "NA_cap_chem_rec.csv")
.safe_write_parity("cap_chem_results", "cap_chem_results.csv")
.safe_write_parity("cap_chem_demand_results", "cap_chem_demand_results.csv")
.safe_write_parity("non_recovery_lost", "non_recovery_lost.csv")
.safe_write_parity("needed_cap_long", "needed_cap_long.csv")
.safe_write_parity("export_lost", "export_lost.csv")
.safe_write_parity("ratio_results", "ratio_results.csv")
.safe_write_parity("overall_circularity", "overall_circularity.csv")

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

cat("\n==== Run_Main.R complete ====\n")
cat("  Saved", .plot_counter, "plots to:\n    ", PLOT_DIR, "\n")
