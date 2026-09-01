# Script run order

This project is now organized into three active stages plus one archive folder.

## 01 — Fleet turnover

Folder: `Scripts/01-Fleet_Turnover`

Purpose: build the upstream fleet, retirement, export, HDV, and second-life BESS outputs used by recycling.

Run the full upstream fleet pipeline:

```bash
Rscript Scripts/01-Fleet_Turnover/00-Run_Fleet_Turnover_Pipeline.R
```

If only the second-life BESS lifetime assumption changed, it is usually enough to rerun:

```bash
Rscript Scripts/01-Fleet_Turnover/41-BESS_Second_Life.R
```

`90-Build_Master_Tables.R` in the same folder is an optional standalone
utility that packs the fleet outputs into one Excel workbook. Nothing in the
pipeline depends on it.

## 02 — Recycling analysis

Folder: `Scripts/02-Recycling_Analysis`

Purpose: rebuild recycling mass flows, manufacturing scrap, mineral recovery, recycled-content, capacity-needed, and geographic-boundary input tables.

Run after the fleet outputs have changed:

```bash
Rscript Scripts/02-Recycling_Analysis/00-Run_Recycling_Pipeline.R
```

This runner updates both policy scenarios:

- `ACCII` = Policy Baseline
- `Repeal` = Policy Rollback

`00-Run_Recycling_Pipeline.R` is the only supported entry point. It runs, per
scenario, `02` -> `03` -> `04` -> `05` in an isolated environment:

| Script | Role |
| --- | --- |
| `01-Data_Preparation.R` | sourced automatically by `02`; do not run alone |
| `02-Core_Analysis.R` | the model — mass flows, capacity constraints, minerals |
| `03-Plots_and_Derived_Exports.R` | R plots **and** `Nat_Mass_2050_long` / `NA_plot_data` / `Mass_2050_region_ref`, which `03-Figures` needs |
| `04-Export_National_Mfg_Scrap.R` | national manufacturing-scrap export |
| `05-Export_Geographic_Boundary_Inputs.R` | state-level boundary exports |

Sourcing `03-Plots_and_Derived_Exports.R` on its own is fine — it will source
`02` for whatever it is missing and always uses freshly computed values.

The legacy parity override is opt-in and off by default. Only set
`ENABLE_MAIN_PARITY <- TRUE` before sourcing `03` when you deliberately want
to reproduce old main-runner numbers; it replaces computed objects with the
cached CSVs in `Outputs/Recycling_Plots_main/<scenario>/_parity/`, which means
changes to `02` will appear to have no effect.

## 03 — Figures

Folder: `Scripts/03-Figures`

Purpose: rebuild the manuscript source-data tables in `Results/Data` and redraw
the final PNG figures in `Results/Figures` from the refreshed model outputs.

Run after `02-Recycling_Analysis`:

```bash
Rscript Scripts/03-Figures/Run_All_Main_Text_Figures.R
```

## Normal workflows

Full rebuild:

```bash
Rscript Scripts/01-Fleet_Turnover/00-Run_Fleet_Turnover_Pipeline.R
Rscript Scripts/02-Recycling_Analysis/00-Run_Recycling_Pipeline.R
Rscript Scripts/03-Figures/Run_All_Main_Text_Figures.R
```

BESS lifetime only:

```bash
Rscript Scripts/01-Fleet_Turnover/41-BESS_Second_Life.R
Rscript Scripts/02-Recycling_Analysis/00-Run_Recycling_Pipeline.R
Rscript Scripts/03-Figures/Run_All_Main_Text_Figures.R
```

Figure style only:

```bash
Rscript Scripts/03-Figures/Run_All_Main_Text_Figures.R
```

## Archive

Folder: `Scripts/old`

This folder contains older raw scripts, previous main-runner versions, exploratory figure variants, and visualization scripts that are not part of the current main pipeline.
