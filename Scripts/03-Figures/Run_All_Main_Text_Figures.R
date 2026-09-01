#!/usr/bin/env Rscript

## Run CARB report Figures 2–17 from a single R entry point.
##
## Figures 2–7 are drawn by R scripts. Figures 8–17 retain their established
## Python/matplotlib implementations, but Python discovery, dependency checks,
## execution order, and error handling are managed here in R.

required_r_packages <- c(
  "dplyr", "geofacet", "ggplot2", "patchwork", "readr", "readxl",
  "scales", "stringr", "tidyr"
)
required_python_packages <- c("matplotlib", "numpy", "pandas")

find_this_script <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
  }

  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      value <- get0("ofile", envir = frame, inherits = FALSE, ifnotfound = "")
      if (is.null(value)) "" else as.character(value)
    },
    character(1)
  )
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files)) {
    return(normalizePath(tail(frame_files, 1), mustWork = TRUE))
  }

  fallback <- file.path(
    getwd(), "Scripts", "03-Figures", "Run_All_Main_Text_Figures.R"
  )
  normalizePath(fallback, mustWork = TRUE)
}

python_has_packages <- function(python) {
  package_literal <- paste0(
    "[", paste(sprintf("'%s'", required_python_packages), collapse = ","), "]"
  )
  check_code <- paste0(
    "import importlib.util,sys; packages=", package_literal, "; ",
    "missing=[p for p in packages if importlib.util.find_spec(p) is None]; ",
    "sys.exit(1 if missing else 0)"
  )
  status <- suppressWarnings(
    system2(
      python,
      args = c("-c", shQuote(check_code)),
      stdout = FALSE,
      stderr = FALSE
    )
  )
  is.numeric(status) && length(status) == 1 && status == 0
}

python_candidates <- function() {
  override <- Sys.getenv("FIGURE_PYTHON", unset = "")
  if (nzchar(override)) return(path.expand(override))

  path_dirs <- strsplit(
    Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE
  )[[1]]
  executable_names <- if (.Platform$OS.type == "windows") {
    c("python.exe", "python3.exe")
  } else {
    c("python3", "python")
  }

  candidates <- as.vector(outer(path_dirs, executable_names, file.path))
  conda_prefix <- Sys.getenv("CONDA_PREFIX", unset = "")
  if (nzchar(conda_prefix)) {
    candidates <- c(
      candidates,
      file.path(conda_prefix, "bin", "python3"),
      file.path(conda_prefix, "bin", "python"),
      file.path(conda_prefix, "python.exe")
    )
  }

  candidates <- c(
    candidates,
    path.expand("~/anaconda3/bin/python3"),
    path.expand("~/miniconda3/bin/python3"),
    "/opt/anaconda3/bin/python3"
  )
  candidates <- unique(candidates[file.exists(candidates)])
  unique(normalizePath(candidates, mustWork = TRUE))
}

select_python <- function() {
  override <- Sys.getenv("FIGURE_PYTHON", unset = "")
  candidates <- python_candidates()
  for (candidate in candidates) {
    if (python_has_packages(candidate)) return(candidate)
  }

  packages <- paste(required_python_packages, collapse = " ")
  if (nzchar(override)) {
    stop(
      "FIGURE_PYTHON does not contain all required packages: ", packages,
      "\nInstall them with:\n  ", override, " -m pip install ", packages,
      call. = FALSE
    )
  }
  stop(
    "No Python installation with all required figure packages was found.\n",
    "Required packages: ", packages, "\n",
    "Install them and optionally set FIGURE_PYTHON to that Python executable.",
    call. = FALSE
  )
}

run_command <- function(executable, arguments, label, environment = character()) {
  message("Running ", label)
  status <- system2(
    executable,
    args = arguments,
    stdout = "",
    stderr = "",
    env = environment
  )
  if (!is.numeric(status) || length(status) != 1 || status != 0) {
    stop(label, " failed with exit status ", status, ".", call. = FALSE)
  }
}

main <- function() {
  script_path <- find_this_script()
  project_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."), mustWork = TRUE
  )
  old_wd <- setwd(project_root)
  on.exit(setwd(old_wd), add = TRUE)

  missing_r <- required_r_packages[
    !vapply(required_r_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_r)) {
    stop(
      "Missing R packages: ", paste(missing_r, collapse = ", "),
      "\nInstall them before running the figure pipeline.",
      call. = FALSE
    )
  }

  python <- select_python()
  rscript <- file.path(R.home("bin"), "Rscript")
  if (!file.exists(rscript)) stop("Rscript was not found.", call. = FALSE)

  message("Project root: ", project_root)
  message("Rscript: ", rscript)
  message("Figure Python: ", python)

  check_only <- "--check" %in% commandArgs(trailingOnly = TRUE)
  if (check_only) {
    message("All figure dependencies are available.")
    return(invisible(TRUE))
  }

  figure_dir <- file.path("Scripts", "03-Figures")
  r_scripts <- c(
    "Fig02_EV_Share_New_LDV_Sales.R",
    "Fig03_Vehicle_Survival_Curves.R",
    "Fig04_North_American_New_Vehicle_Sales.R",
    "Fig05_California_LDV_Sales_and_Retirements.R",
    "Fig06_North_American_Vehicle_Retirements.R",
    "Fig07_North_American_Cumulative_Battery_Retirements.R"
  )
  python_scripts <- list(
    c("Fig08_Regional_Supply_Chain_2050.py"),
    c("Fig09_Country_Supply_Chain_2050.py"),
    c("Fig10_North_American_Flows_Over_Time.py"),
    c("Fig11_Annual_Recycling_Capacity_Deficit.py", "--annual"),
    c("Fig12_Recoverable_Minerals.py"),
    c("Fig13_Cumulative_MRR_Losses_Through_2035.py"),
    c("Fig14_Maximum_RCS_North_America.py"),
    c("Supporting/RCS_Geographic_Analysis.py"),
    c("Supporting/Prepare_Fig17_Additional_Recycling_Capacity_Data.py"),
    c("Fig17_Additional_Recycling_Capacity_Required.py")
  )

  for (script in r_scripts) {
    run_command(
      rscript,
      file.path(figure_dir, script),
      script
    )
  }

  matplotlib_cache <- file.path(tempdir(), "matplotlib")
  dir.create(matplotlib_cache, recursive = TRUE, showWarnings = FALSE)
  python_environment <- c(
    "RCS_MAIN_FIGURE=both",
    paste0("MPLCONFIGDIR=", matplotlib_cache)
  )
  for (script_args in python_scripts) {
    script <- script_args[[1]]
    arguments <- c(file.path(figure_dir, script), script_args[-1])
    run_command(
      python,
      arguments,
      basename(script),
      environment = python_environment
    )
  }

  message("All main-text figures completed successfully.")
  message("Figure data folder: ", file.path(project_root, "Results", "Data"))
  message("Final PNG folder: ", file.path(project_root, "Results", "Figures"))
  invisible(TRUE)
}

main()
