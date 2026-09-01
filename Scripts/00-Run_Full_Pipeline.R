#!/usr/bin/env Rscript

## Full active pipeline:
##   01 Fleet turnover -> 02 Recycling analysis -> 03 Figures
##
## Run from project root:
##   Rscript Scripts/00-Run_Full_Pipeline.R

message("=== 01 Fleet turnover ===")
source(file.path("Scripts", "01-Fleet_Turnover", "00-Run_Fleet_Turnover_Pipeline.R"))

message("=== 02 Recycling analysis ===")
source(file.path("Scripts", "02-Recycling_Analysis", "00-Run_Recycling_Pipeline.R"))

message("=== 03 Figures ===")
source(file.path("Scripts", "03-Figures", "Run_All_Main_Text_Figures.R"))

message("=== Full pipeline complete ===")
