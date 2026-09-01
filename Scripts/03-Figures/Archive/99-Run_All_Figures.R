#!/usr/bin/env Rscript

# One-click runner for the current recycling manuscript figures.
# This R script calls the Python plotting scripts in the correct order.

message("=== Running recycling figure scripts ===")

find_script_path <- function() {
  from_source <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
                          error = function(e) NA_character_)
  if (!is.na(from_source) && nzchar(from_source)) {
    return(from_source)
  }

  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE))
  }

  NA_character_
}

script_path <- find_script_path()
project_root <- if (!is.na(script_path) && file.exists(script_path)) {
  normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}

scripts_dir <- file.path(project_root, "Scripts", "03-Figures")
if (!dir.exists(scripts_dir)) {
  stop(
    "Cannot find Scripts/03-Figures. ",
    "Please run this from the Fleet model project root, or call this script by path."
  )
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(project_root)

detect_python <- function() {
  candidates <- c(
    Sys.getenv("PYTHON", unset = ""),
    "/opt/anaconda3/bin/python",
    Sys.which("python3"),
    Sys.which("python")
  )
  candidates <- unique(candidates[nzchar(candidates)])
  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    stop(
      "No Python executable found. Set PYTHON=/path/to/python before running this script."
    )
  }

  existing[[1]]
}

python <- detect_python()
message("Project root: ", project_root)
message("Python: ", python)

# Keep matplotlib cache out of synced project folders.
mpl_cache <- file.path(tempdir(), "matplotlib-cache")
dir.create(mpl_cache, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(MPLCONFIGDIR = mpl_cache)

py <- function(name) file.path("Scripts", "03-Figures", name)
out_acc <- file.path("Outputs", "Recycling_Plots_main", "ACCII")
out_cmp <- file.path("Outputs", "Recycling_Plots_main", "Scenario_Comparison")

figure_jobs <- list(
  list(
    label = "Regional 2050 heatmaps",
    args = c(py("04-Regional_Heatmaps.py")),
    outputs = c(file.path(out_acc, "Regional_Supply_Chain_Heatmaps_2050_ACCII.png"))
  ),
  list(
    label = "Figure 2 regional 2050 bars",
    args = c(py("08-Figure2_Regional_2050.py")),
    outputs = c(file.path(out_acc, "Plotting_Demand_Recycle_Manu_07.png"))
  ),
  list(
    label = "Figure 2 policy/capacity/country bars",
    args = c(py("05-Figure2_2050_Scenario_Comparison.py")),
    outputs = c(file.path(out_cmp, "Figure2_2050_Policy_Capacity_Country.png"))
  ),
  list(
    label = "Figure 2 policy/capacity bars without country",
    args = c(py("06-Figure2_2050_Scenario_Comparison_No_Country.py")),
    outputs = c(file.path(out_cmp, "Figure2_2050_Policy_Capacity_No_Country.png"))
  ),
  list(
    label = "Figure 2 country-pattern bars",
    args = c(py("07-Figure2_2050_Country_Patterns.py")),
    outputs = c(file.path(out_cmp, "Figure2_2050_Policy_Capacity_Country_Patterns.png"))
  ),
  list(
    label = "National time series, ACCII/Baseline only",
    args = c(py("09-Figure3_National_Timeseries.py")),
    outputs = c(file.path(out_acc, "Plotting_Demand_Recycle_Manu_08.png"))
  ),
  list(
    label = "Figure 3 national overtime policy comparison",
    args = c(py("13-Figure_National_Overtime_Policy_Comparison.py")),
    outputs = c(file.path(out_cmp, "Figure3_National_Overtime_Baseline_vs_Rollback.png"))
  ),
  list(
    label = "Figure 3 cumulative recycling capacity deficit",
    args = c(py("10-Figure_Needed_Capacity_Policy_Comparison.py")),
    outputs = c(file.path(out_cmp, "Figure3_Needed_Recycling_Capacity_Policy_Comparison.png"))
  ),
  list(
    label = "Figure 3 annual recycling capacity deficit",
    args = c(py("10-Figure_Needed_Capacity_Policy_Comparison.py"), "--annual"),
    outputs = c(file.path(out_cmp, "Figure3_Yearly_Recycling_Capacity_Deficit_Policy_Comparison.png"))
  ),
  list(
    label = "Figure 4 recoverable minerals policy panels",
    args = c(py("15-Figure_Recoverable_Minerals_Policy_Panels.py")),
    outputs = c(file.path(out_cmp, "Figure4_Recoverable_Minerals_Baseline_vs_Rollback.png"))
  ),
  list(
    label = "Figure 5 minerals lost in 2035",
    args = c(py("19-Figure_Minerals_Lost_2035_Policy_Bars.py")),
    outputs = c(file.path(out_cmp, "Figure5_Minerals_Lost_2035_Baseline_vs_Rollback.png"))
  ),
  list(
    label = "Figure 6 recycled content policy panels",
    args = c(py("20-Figure_Recycled_Content_Policy_Panels.py")),
    outputs = c(file.path(out_cmp, "Figure6_Maximum_Recycled_Content_Baseline_vs_Rollback.png"))
  ),
  list(
    label = "Figure 6 recycled content baseline only",
    args = c(py("21-Figure_Recycled_Content_Baseline.py")),
    outputs = c(file.path(out_cmp, "Figure6_Maximum_Recycled_Content_Baseline_Policy.png"))
  ),
  list(
    label = "Figure 7 geographic boundary analysis",
    args = c(py("23-Figure_Geographic_Boundary_Analysis.py")),
    outputs = c(
      file.path(out_cmp, "Geographic_Boundary_RCS.csv"),
      file.path(out_cmp, "Figure7G_a_RCS_Reference_Values_Landscape.png"),
      file.path(out_cmp, "Figure7G_b_RCS_Scenario_Effects_Landscape.png")
    )
  ),
  list(
    label = "Figure 8 additional recycling capacity needed",
    args = c(py("24-Figure_Recycling_Capacity_Needed.py")),
    outputs = c(file.path(out_cmp, "Figure8_Additional_Recycling_Capacity_Needed.png"))
  ),
  list(
    label = "Figure 8 regional capacity-needed scenario table/heatmap",
    args = c(py("25-Figure_Regional_Capacity_Needed_Scenarios.py")),
    outputs = c(
      file.path(out_cmp, "Regional_Additional_Recycling_Capacity_Needed.csv"),
      file.path(out_cmp, "Figure16_Cumulative_Additional_Capacity_Required_Heatmap.png")
    )
  ),
  list(
    label = "Figure 8B regional capacity-needed dotplot",
    args = c(py("26-Figure_Regional_Capacity_Needed_Dotplot.py")),
    outputs = c(file.path(out_cmp, "Figure8B_Regional_Capacity_Needed_Faceted_Dotplot.png"))
  )
)

# Older exploratory variants have been moved to Scripts/old and are not part
# of the manuscript figure runner.
run_archived_variants <- isTRUE(as.logical(Sys.getenv("RUN_ARCHIVED_FIGURE_VARIANTS", "FALSE")))
if (run_archived_variants) {
  stop("Archived figure variants are stored in Scripts/old and are not run by 99-Run_All_Figures.R.")
}

run_job <- function(job, index, total) {
  message("")
  message(sprintf("[%02d/%02d] %s", index, total, job$label))
  status <- system2(python, args = job$args)
  if (!identical(status, 0L)) {
    stop("Failed while running: ", paste(job$args, collapse = " "))
  }
}

for (i in seq_along(figure_jobs)) {
  run_job(figure_jobs[[i]], i, length(figure_jobs))
}

expected_outputs <- unique(unlist(lapply(figure_jobs, `[[`, "outputs")))
missing_outputs <- expected_outputs[!file.exists(expected_outputs)]

message("")
if (length(missing_outputs) > 0) {
  warning(
    "The scripts finished, but these expected outputs were not found:\n",
    paste(" -", missing_outputs, collapse = "\n")
  )
} else {
  message("All expected outputs were found.")
}

message("")
message("Main output folders:")
message(" - ", file.path(project_root, out_acc))
message(" - ", file.path(project_root, out_cmp))
message("=== Done ===")
