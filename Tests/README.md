# California actual-sales regression

The California allocation must use historical segment shares through 2024,
then append the 2024 shares once for 2025. Previously, existing 2025 rows and
the appended shares were both joined to the 2025 actual totals. Zero-sales
placeholders therefore doubled 2025 BEV sales from 351,043 to 702,086 and
PHEV sales from 57,325 to 114,650.

Run the input-independent regression from the repository root:

```sh
Rscript Tests/test_ca_2025_sales.R
```

It exercises the production allocation using synthetic historical data with
zero, absent, and nonzero 2025 rows. It verifies unique allocations, the
2024 Car/SUV shares, and unchanged actual totals for 2014–2024.

After rebuilding the fleet outputs, check both scenario CSVs:

```sh
Rscript Scripts/01-Fleet_Turnover/00-Run_Fleet_Turnover_Pipeline.R
Rscript Tests/test_ca_2025_outputs.R
```

The output check verifies 50 states plus the District of Columbia, unique
state/segment/year keys, consistent segment and state sales totals, and
2025 California BEV/PHEV additions of 351,043/57,325 in both scenarios.
Future sales are still determined by each scenario; the fix does not force
sales to increase from one year to the next.

Rebuild recycling and figures after changing fleet outputs, following
`Scripts/README_Run_Order.md`.

## Verification of this correction

California was simulated with the unchanged state engine before and after
the allocation correction. The original run reproduced all California
sales, retirement, stock, battery-flow, and age-vector records in the
committed ACCII and Repeal outputs (numeric tolerance 1e-7; vectors exact).
The national export calibration uses 2020–2024, so it is unchanged by this
2025 correction. The refreshed national tables retain the original rows
for other states and for 2020–2024; export totals are recomputed from the
corrected flows. Downstream models and main-text figures are then rebuilt.

The existing EV battery engine emits a vector-length recycling warning at
`reuse_offset_vec + reuse_loop_vec` during both the original and corrected
runs. This allocation fix does not change that separate battery algorithm.
