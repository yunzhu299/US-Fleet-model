## Regression test for California's actual-sales allocation.
## Run from the repository root: Rscript Tests/test_ca_2025_sales.R
## Optional first argument: another copy of script 05 (to check the regression).
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
model_file <- if (length(args)) args[1] else "Scripts/01-Fleet_Turnover/05-US_Fleet_Simulation.R"
expressions <- parse(model_file)
allocation_names <- c("ca_totals_actual", "ca_seg_ratio", "ca_seg_ratio_2025", "ca_actual_rows")
allocation <- Filter(function(e) {
  is.call(e) && identical(e[[1]], as.name("<-")) &&
    is.symbol(e[[2]]) && as.character(e[[2]]) %in% allocation_names
}, as.list(expressions))
stopifnot(length(allocation) == 5L)
# Synthetic history deliberately includes the 2025 zero-sales placeholder rows
# that triggered the production defect. No private/external input is needed.
history <- expand.grid(`Sale Year` = 2014:2025, State = "California",
                       Propulsion = c("BEV", "PHEV"), `Global Segment` = c("Car", "SUV"),
                       stringsAsFactors = FALSE, check.names = FALSE)
history$Sales <- ifelse(history$`Sale Year` == 2025, 0,
                        ifelse(history$`Global Segment` == "Car", 20, 80))

for (case in c("zero_2025_placeholders", "no_2025_rows", "nonzero_2025_rows")) {
  h <- history
  if (case == "no_2025_rows") h <- h %>% filter(`Sale Year` != 2025)
  if (case == "nonzero_2025_rows") {
    h <- h %>% mutate(Sales = if_else(State == "California" & `Sale Year` == 2025,
                                      100, Sales))
  }
  env <- new.env(parent = globalenv())
  env$EV_historical <- h
  for (e in allocation) eval(e, envir = env)
  actual <- env$ca_actual_rows
  stopifnot(!anyDuplicated(actual[c("Sale Year", "Propulsion", "Global Segment")]))
  totals <- actual %>% filter(`Sale Year` == 2025) %>%
    group_by(Propulsion) %>% summarise(Sales = sum(Sales), .groups = "drop")
  stopifnot(abs(totals$Sales[totals$Propulsion == "BEV"] - 351043) < 1e-6,
            abs(totals$Sales[totals$Propulsion == "PHEV"] - 57325) < 1e-6)
  shares <- h %>% filter(State == "California", `Sale Year` == 2024) %>%
    group_by(Propulsion) %>% mutate(Expected_Share = Sales / sum(Sales)) %>%
    select(Propulsion, `Global Segment`, Expected_Share)
  check <- actual %>% filter(`Sale Year` == 2025) %>%
    group_by(Propulsion) %>% mutate(Actual_Share = Sales / sum(Sales)) %>%
    left_join(shares, by = c("Propulsion", "Global Segment"))
  stopifnot(nrow(check) == 4L, all(abs(check$Actual_Share - check$Expected_Share) < 1e-12))
  historic <- actual %>% filter(`Sale Year` <= 2024) %>%
    group_by(`Sale Year`, Propulsion) %>% summarise(Sales = sum(Sales), .groups = "drop") %>%
    left_join(env$ca_totals_actual, by = c("Sale Year", "Propulsion"))
  stopifnot(nrow(historic) == 22L, all(abs(historic$Sales - historic$total) < 1e-6))
  cat("Passed:", case, "\n")
}
