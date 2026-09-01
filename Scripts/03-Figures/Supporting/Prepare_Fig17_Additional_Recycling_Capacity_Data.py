#!/usr/bin/env python3
"""Cumulative recycling-capacity buildout required across network boundaries."""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[3]
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
OUT = DATA_ROOT / "Scenario_Comparison"
OUT.mkdir(parents=True, exist_ok=True)
DATA_OUT = ROOT / "Results" / "Data"
DATA_OUT.mkdir(parents=True, exist_ok=True)

US_CODES = set(
    "AL AK AZ AR CA CO CT DE DC FL GA HI ID IL IN IA KS KY LA ME MD MA "
    "MI MN MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX "
    "UT VT VA WA WV WI WY".split()
)
SCRAP_YIELD = 0.7078558
YEARS = [2030, 2035, 2040, 2045, 2050]
POLICIES = {"ACCII": "Baseline", "Repeal": "Rollback"}
CAPACITIES = ["Increasing Batt Cap", "Decreasing Batt Cap"]
CHEMISTRIES = ["Benchmark Chemistry", "High LFP Chemistry"]
SCOPES = {
    "California": (lambda state: state == "CA", lambda state: state in US_CODES),
    "United States": (
        lambda state: state in US_CODES, lambda state: state in US_CODES
    ),
    "North America": (lambda state: True, lambda state: True),
}
GEOGRAPHY_TITLES = {
    "California": "California\nCA-origin feedstock / US recycling network",
    "United States":
        "United States\nUS-origin feedstock / US recycling network",
    "North America":
        "North America\nNA-origin feedstock / NA recycling network",
}


def aggregate(frame, column, state_filter):
    selected = frame[frame["State_Province"].astype(str).map(state_filter)]
    return selected.groupby("Year")[column].sum()


def calculate_levels(eol, scrap, capacity, origin_filter, network_filter):
    battery = aggregate(eol, "Batt_Mass_MT", origin_filter)
    scrap_equivalent = aggregate(scrap, "Scrap_tonnes", origin_filter) / SCRAP_YIELD
    black = aggregate(capacity, "Cumulative_black_mass_cap", network_filter)
    refining = aggregate(capacity, "Cumulative_refining_cap", network_filter)

    black_level = 0.0
    refining_level = 0.0
    rows = []
    for year in range(2025, 2051):
        batt = battery.get(year, 0.0)
        scrap_eq = scrap_equivalent.get(year, 0.0)
        black_cap = black.get(year, 0.0)
        refining_cap = refining.get(year, 0.0)
        full_recycle = min(black_cap, refining_cap)

        leftover_black = max(black_cap - scrap_eq, 0.0)
        leftover_full = max(full_recycle - scrap_eq, 0.0)
        black_change = (
            max(batt - leftover_black, 0.0)
            + max(scrap_eq - black_cap, 0.0)
            - max(leftover_black - batt, 0.0)
        )
        refining_change = (
            max(batt - leftover_full, 0.0)
            + max(scrap_eq - full_recycle, 0.0)
            - max(refining_cap - min(batt + scrap_eq, full_recycle), 0.0)
        )
        black_level = max(black_level + black_change, 0.0)
        refining_level = max(refining_level + refining_change, 0.0)
        rows.extend(
            [
                (year, "Preprocessing", black_level / 1e6),
                (year, "Materials recovery", refining_level / 1e6),
            ]
        )
    return rows


result_rows = []
for folder, policy in POLICIES.items():
    base = DATA_ROOT / folder / "_parity"
    eol_all = pd.read_csv(base / "state_mass_recycle_batt.csv")
    manufacturing = pd.read_csv(base / "manufacturing_by_state_projected.csv")
    facilities = pd.read_csv(base / "recycling_tonnes_by_state.csv")

    for capacity_name in CAPACITIES:
        scrap_column = (
            "Tonnes_Scrap_proj_mid"
            if capacity_name.startswith("Increasing")
            else "Tonnes_Scrap_15_mid"
        )
        scrap = manufacturing[
            ["Year", "State_Province", scrap_column]
        ].rename(columns={scrap_column: "Scrap_tonnes"})

        for chemistry in CHEMISTRIES:
            scenario = f"{capacity_name} - {chemistry}"
            eol = eol_all[eol_all["Scenario"] == scenario]
            for geography, (origin_filter, network_filter) in SCOPES.items():
                for year, stage, value in calculate_levels(
                    eol, scrap, facilities, origin_filter, network_filter
                ):
                    result_rows.append(
                        {
                            "Policy": policy,
                            "Capacity": capacity_name,
                            "Chemistry": chemistry,
                            "Geography": geography,
                            "Year": year,
                            "Stage": stage,
                            "Additional capacity needed (million MT/year)": value,
                        }
                    )

results = pd.DataFrame(result_rows)
results["Additional capacity needed (thousand MT/year)"] = (
    results["Additional capacity needed (million MT/year)"] * 1000
)
results.to_csv(
    OUT / "Regional_Additional_Recycling_Capacity_Needed.csv", index=False
)
results.to_csv(
    DATA_OUT / "Fig17_Additional_Recycling_Capacity_Required.csv", index=False
)

scenario_rows = [
    (policy, capacity, chemistry)
    for capacity in CAPACITIES
    for chemistry in CHEMISTRIES
    for policy in POLICIES.values()
]
scenario_labels = [
    " · ".join(
        [
            "Increase" if capacity.startswith("Increasing") else "Decrease",
            "Benchmark" if chemistry.startswith("Benchmark") else "High-LFP",
            policy,
        ]
    )
    for policy, capacity, chemistry in scenario_rows
]

plot_max = np.ceil(
    results["Additional capacity needed (thousand MT/year)"].max() / 5000
) * 5000
plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 10.5,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axes = plt.subplots(
    2, 3, figsize=(17.8, 10.5),
    gridspec_kw={"hspace": 0.35, "wspace": 0.10},
)
mesh = None
for row_index, stage in enumerate(["Preprocessing", "Materials recovery"]):
    for col_index, geography in enumerate(SCOPES):
        axis = axes[row_index, col_index]
        matrix = np.full((len(scenario_rows), len(YEARS)), np.nan)
        subset = results[
            (results["Stage"] == stage)
            & (results["Geography"] == geography)
        ]
        for scenario_index, (policy, capacity, chemistry) in enumerate(
            scenario_rows
        ):
            values = subset[
                (subset["Policy"] == policy)
                & (subset["Capacity"] == capacity)
                & (subset["Chemistry"] == chemistry)
            ].set_index("Year")[
                "Additional capacity needed (thousand MT/year)"
            ]
            matrix[scenario_index, :] = [
                values.get(year, np.nan) for year in YEARS
            ]

        mesh = axis.imshow(
            matrix, cmap="YlOrBr", vmin=0, vmax=plot_max,
            origin="upper", aspect="auto",
        )
        axis.set_title(
            GEOGRAPHY_TITLES[geography],
            fontsize=12.5,
            fontweight="bold",
            pad=7,
            linespacing=1.25,
        )
        axis.set_xticks(np.arange(len(YEARS)))
        axis.set_xticklabels(YEARS, fontsize=10.5)
        axis.set_yticks(np.arange(len(scenario_rows)))
        axis.set_yticklabels(
            scenario_labels if col_index == 0 else [], fontsize=8.5
        )
        axis.set_xticks(np.arange(-0.5, len(YEARS), 1), minor=True)
        axis.set_yticks(
            np.arange(-0.5, len(scenario_rows), 1), minor=True
        )
        axis.grid(which="minor", color="white", linewidth=1.0)
        axis.tick_params(which="both", length=0)
        axis.axhline(3.5, color="#111111", linewidth=2.0)
        axis.axhline(1.5, color="#777777", linewidth=0.9)
        axis.axhline(5.5, color="#777777", linewidth=0.9)

        for i in range(matrix.shape[0]):
            for j in range(matrix.shape[1]):
                value = matrix[i, j]
                axis.text(
                    j, i, f"{value:,.0f}",
                    ha="center", va="center", fontsize=8.2,
                    fontweight="bold",
                    color="white" if value > plot_max * 0.58 else "#302C24",
                )

fig.suptitle(
    "Cumulative Additional Recycling Capacity Required by Year",
    fontsize=19, fontweight="bold", y=0.985,
)
fig.text(
    0.02, 0.89, "a) Preprocessing (black mass production)",
    fontsize=15.5, fontweight="bold",
)
fig.text(
    0.02, 0.45, "b) Materials recovery (refining)",
    fontsize=15.5, fontweight="bold",
)
fig.text(
    0.5, 0.925,
    "Panel titles identify feedstock origin / accessible recycling network; "
    "boundaries are alternative, not additive",
    ha="center", fontsize=11, color="#4F5552",
)
colorbar_axis = fig.add_axes([0.205, 0.048, 0.785, 0.022])
colorbar = fig.colorbar(mesh, cax=colorbar_axis, orientation="horizontal")
colorbar.set_label(
    "Cumulative additional capacity required by year "
    "(thousand metric tonnes/year)",
    fontsize=12.5, fontweight="bold",
)
colorbar.ax.tick_params(labelsize=10.5)
fig.subplots_adjust(left=0.205, right=0.99, top=0.84, bottom=0.15)

stem = OUT / "Figure16_Cumulative_Additional_Capacity_Required_Heatmap"
fig.savefig(f"{stem}.png", dpi=400, bbox_inches="tight", facecolor="white")
plt.close(fig)
print(f"{stem}.png")
