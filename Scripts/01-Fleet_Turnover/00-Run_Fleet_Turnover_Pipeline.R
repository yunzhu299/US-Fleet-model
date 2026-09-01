## Complete North American fleet and second-life BESS pipeline
## Run from the repository root with:
##   Rscript Scripts/01-Fleet_Turnover/00-Run_Fleet_Turnover_Pipeline.R

steps <- c(
  "01-ICE_Age_Distribution.R",
  "02-Retirement_Demand.R",
  "03-Population_Demand.R",
  "04-EV_Turnover.R",
  "05-US_Fleet_Simulation.R",
  "10-Canada_Population.R",
  "11-Canada_Fleet_Simulation.R",
  "21-Mexico_Inputs.R",
  "23-Export_Projection.R",
  "24-Mexico_Turnover.R",
  "31-HDV_Turnover.R",
  "41-BESS_Second_Life.R"
)

base_dir <- file.path("Scripts", "01-Fleet_Turnover")
missing_steps <- steps[!file.exists(file.path(base_dir, steps))]
if (length(missing_steps) > 0) {
  stop("Missing pipeline scripts: ", paste(missing_steps, collapse = ", "))
}

for (step in steps) {
  cat("\n============================================================\n")
  cat("Running ", step, "\n", sep = "")
  cat("============================================================\n")
  source(file.path(base_dir, step), local = FALSE, chdir = FALSE)
}

cat("\nComplete fleet pipeline finished successfully.\n")
