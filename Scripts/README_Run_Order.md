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

## 02 — Recycling analysis

Folder: `Scripts/02-Recycling_Analysis`

Purpose: rebuild recycling mass flows, manufacturing scrap, mineral recovery, recycled-content, capacity-needed, and geographic-boundary input tables.

Run after the fleet outputs have changed:

```bash
Rscript Scripts/02-Recycling_Analysis/98-Run_Recycling_Data_Exports.R
```

This runner updates both policy scenarios:

- `ACCII` = Policy Baseline
- `Repeal` = Policy Rollback

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
Rscript Scripts/02-Recycling_Analysis/98-Run_Recycling_Data_Exports.R
Rscript Scripts/03-Figures/Run_All_Main_Text_Figures.R
```

BESS lifetime only:

```bash
Rscript Scripts/01-Fleet_Turnover/41-BESS_Second_Life.R
Rscript Scripts/02-Recycling_Analysis/98-Run_Recycling_Data_Exports.R
Rscript Scripts/03-Figures/Run_All_Main_Text_Figures.R
```

Figure style only:

```bash
Rscript Scripts/03-Figures/Run_All_Main_Text_Figures.R
```

## Archive

Folder: `Scripts/old`

This folder contains older raw scripts, previous main-runner versions, exploratory figure variants, and visualization scripts that are not part of the current main pipeline.
