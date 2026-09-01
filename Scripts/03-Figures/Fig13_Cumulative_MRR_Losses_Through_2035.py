#!/usr/bin/env python3
"""Figure 13: cumulative mineral losses through 2035 without MRR requirements."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
DATA_OUT = ROOT / "Results" / "Data"
OUT = ROOT / "Results" / "Figures"
DATA_OUT.mkdir(parents=True, exist_ok=True)
OUT.mkdir(parents=True, exist_ok=True)
STEM = OUT / "Fig13_Cumulative_MRR_Losses_Through_2035"

SCENARIO = "Increasing Batt Cap - Benchmark Chemistry"
POLICIES = {"ACCII": "Policy Baseline", "Repeal": "Policy Rollback"}
MINERALS = ["Manganese", "Copper", "Lithium", "Graphite"]

BASE_COLORS = {
    "Manganese": "#789D4A",
    "Copper": "#C47758",
    "Lithium": "#3F91B5",
    "Graphite": "#6D628A",
}
ROLLBACK_COLORS = {
    "Manganese": "#B8C99B",
    "Copper": "#DDB29F",
    "Lithium": "#9CC5D6",
    "Graphite": "#B5B0C5",
}


def load_policy(folder: str) -> pd.Series:
    raw = pd.read_csv(DATA_ROOT / folder / "_parity" / "cap_chem_results.csv")
    annual = (
        raw[
            raw["Year"].between(2025, 2035)
            & (raw["Scenario"] == SCENARIO)
            & raw["Mineral"].isin(MINERALS)
        ]
        .groupby(["Year", "Mineral"], as_index=False)[
            "Minerals Lost to Pyrometalurgy (Tonne)"
        ]
        .sum()
        .sort_values(["Mineral", "Year"])
    )
    annual["Cumulative loss"] = (
        annual.groupby("Mineral")["Minerals Lost to Pyrometalurgy (Tonne)"]
        .cumsum() / 1000
    )
    return (
        annual[annual["Year"] == 2035]
        .set_index("Mineral")["Cumulative loss"]
        .reindex(MINERALS)
        .fillna(0)
    )


DATA = {folder: load_policy(folder) for folder in POLICIES}
pd.concat(
    [
        values.rename("Cumulative loss (thousand metric tonnes)")
        .reset_index()
        .assign(Year=2035, **{"Policy scenario": POLICIES[folder]})
        for folder, values in DATA.items()
    ],
    ignore_index=True,
).to_csv(
    DATA_OUT / "Fig13_Cumulative_MRR_Losses_Through_2035.csv", index=False
)
global_max = max(values.max() for values in DATA.values())

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 11,
        "axes.titlesize": 16,
        "axes.labelsize": 13,
        "xtick.labelsize": 11.5,
        "ytick.labelsize": 11,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axis = plt.subplots(figsize=(10.2, 6.2))
x = np.arange(len(MINERALS))

bar_width = 0.34
baseline_values = DATA["ACCII"].to_numpy()
rollback_values = DATA["Repeal"].to_numpy()
baseline_bars = axis.bar(
    x - bar_width / 2,
    baseline_values,
    width=bar_width,
    color=[BASE_COLORS[mineral] for mineral in MINERALS],
    edgecolor="#4F5552",
    linewidth=0.65,
    zorder=3,
)
rollback_bars = axis.bar(
    x + bar_width / 2,
    rollback_values,
    width=bar_width,
    color=[ROLLBACK_COLORS[mineral] for mineral in MINERALS],
    edgecolor="#4F5552",
    linewidth=0.65,
    zorder=3,
)
for bars, values in (
    (baseline_bars, baseline_values),
    (rollback_bars, rollback_values),
):
    axis.bar_label(
        bars,
        labels=[f"{value:.1f}" for value in values],
        padding=4,
        fontsize=10.5,
        fontweight="bold",
    )

axis.set_xticks(x, MINERALS)
axis.set_ylim(0, global_max * 1.17)
axis.grid(axis="y", color="#D8D8D8", linewidth=0.7, zorder=0)
axis.spines[["top", "right"]].set_visible(False)
axis.set_xlabel("Mineral", fontweight="bold")
axis.set_ylabel(
    "Cumulative lost minerals (thousand metric tonnes)",
    fontweight="bold",
)
fig.suptitle(
    "Cumulative North American Minerals Lost to Lack of Recovery Standards (2035)",
    fontsize=18, fontweight="bold", y=0.975,
)
handles = [
    Patch(facecolor="#5D6670", edgecolor="#4F5552", label="Policy Baseline"),
    Patch(facecolor="#C5CBD0", edgecolor="#4F5552", label="Policy Rollback"),
]
legend = axis.legend(
    handles=handles,
    title="Policy scenario",
    loc="upper left",
    bbox_to_anchor=(0.02, 0.98),
    frameon=False,
    ncol=1,
    handlelength=2.0,
)
legend.get_title().set_fontweight("bold")
legend.get_title().set_fontsize(12)

fig.subplots_adjust(left=0.12, right=0.985, top=0.87, bottom=0.15)
fig.savefig(STEM.with_suffix(".png"), dpi=400, bbox_inches="tight", facecolor="white")

print(STEM.with_suffix(".png"))
