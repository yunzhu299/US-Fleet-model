# Main-text figure scripts

The scripts in this folder follow the figure numbers in **CARB Final Report _ADA.docx**. Figure 1 is the model flowchart and is intentionally excluded. Each run writes the figure-ready CSV tables to `Results/Data` and the final PNG files to `Results/Figures`.

| Figure | Script |
|---|---|
| 2 | `Fig02_EV_Share_New_LDV_Sales.R` |
| 3 | `Fig03_Vehicle_Survival_Curves.R` |
| 4 | `Fig04_North_American_New_Vehicle_Sales.R` |
| 5 | `Fig05_California_LDV_Sales_and_Retirements.R` |
| 6 | `Fig06_North_American_Vehicle_Retirements.R` |
| 7 | `Fig07_North_American_Cumulative_Battery_Retirements.R` |
| 8 | `Fig08_Regional_Supply_Chain_2050.py` |
| 9 | `Fig09_Country_Supply_Chain_2050.py` |
| 10 | `Fig10_North_American_Flows_Over_Time.py` |
| 11 | `Fig11_Annual_Recycling_Capacity_Deficit.py --annual` |
| 12 | `Fig12_Recoverable_Minerals.py` |
| 13 | `Fig13_Cumulative_MRR_Losses_Through_2035.py` |
| 14 | `Fig14_Maximum_RCS_North_America.py` |
| 15 | `Fig15_Maximum_RCS_Geographic_Boundaries.py` |
| 16 | `Fig16_Maximum_RCS_Scenario_Effects.py` |
| 17 | `Fig17_Additional_Recycling_Capacity_Required.py` |

Figures 2–7 are independent R scripts. Each script directly reads model inputs or outputs, performs its own calculation and plotting, and writes its corresponding source-data CSV and PNG without sourcing another plotting script or copying a reference image.

Figures 15 and 16 share the calculation in `Supporting/RCS_Geographic_Analysis.py`. Both use `Available Recycled Minerals No R Restraint (Tonne)`, so recycling-facility capacity does not limit maximum RCS. Their geographic labels refer only to feedstock origin and LIB demand. Figure 17 retains the recycling-network boundary because it evaluates processing-capacity requirements.

Run every main-text figure except the flowchart from R with:

```sh
Rscript Scripts/03-Figures/Run_All_Main_Text_Figures.R
```

From an interactive R/RStudio console, use:

```r
source("Scripts/03-Figures/Run_All_Main_Text_Figures.R")
```

The R runner is the recommended entry point. It checks the R dependencies,
finds a Python environment for the established matplotlib figures 8–17, and
stops immediately with a clear message if a required dependency is missing.
It refreshes both `Results/Data` and `Results/Figures`; `Outputs` remains the
location for complete model and intermediate analysis results.

The equivalent Python entry point remains available:

```sh
python Scripts/03-Figures/Run_All_Main_Text_Figures.py
```

Both runners check all required R packages and automatically search for a
Python interpreter with the packages listed in `requirements.txt`. To check the
environment without drawing figures, run:

```sh
Rscript Scripts/03-Figures/Run_All_Main_Text_Figures.R --check
```

If automatic Python discovery is not appropriate, set `FIGURE_PYTHON` to the
desired interpreter before running the script.

Alternative and superseded plotting scripts are retained in `Archive`.
