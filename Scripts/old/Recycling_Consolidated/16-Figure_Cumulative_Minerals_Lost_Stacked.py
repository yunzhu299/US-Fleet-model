#!/usr/bin/env python3
"""Cumulative mineral losses, 2025–2050, shown as stacked annual bars."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA = (
    ROOT / "Outputs" / "Recycling_Plots_main" / "ACCII"
    / "_parity" / "cap_chem_results.csv"
)
OUT = ROOT / "Outputs" / "Recycling_Plots_main" / "Scenario_Comparison"
OUT.mkdir(parents=True, exist_ok=True)
STEM = OUT / "Figure5_Cumulative_Minerals_Lost_2025_2050_Stacked"

SCENARIO = "Increasing Batt Cap - Benchmark Chemistry"
STACK_ORDER = ["Graphite", "Lithium", "Manganese", "Copper"]
COLORS = {
    "Graphite": "#6F5A8A",
    "Lithium": "#439BC0",
    "Manganese": "#7FAE3E",
    "Copper": "#C57B57",
}

raw = pd.read_csv(DATA)
annual = (
    raw[raw["Scenario"] == SCENARIO]
    .groupby(["Year", "Mineral"], as_index=False)[
        "Minerals Lost to Pyrometalurgy (Tonne)"
    ]
    .sum()
)
annual = annual[annual["Mineral"].isin(STACK_ORDER)].copy()
annual = annual.sort_values(["Mineral", "Year"])
annual["Cumulative loss"] = annual.groupby("Mineral")[
    "Minerals Lost to Pyrometalurgy (Tonne)"
].cumsum() / 1000

years = np.arange(2025, 2051)
wide = (
    annual.pivot(index="Year", columns="Mineral", values="Cumulative loss")
    .reindex(years)
    .fillna(0)
)

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 11,
        "axes.titlesize": 18,
        "axes.labelsize": 14,
        "xtick.labelsize": 11,
        "ytick.labelsize": 11,
        "legend.fontsize": 11.5,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axis = plt.subplots(figsize=(10.8, 6.2))
bottom = np.zeros(len(years))
for mineral in STACK_ORDER:
    values = wide[mineral].to_numpy()
    axis.bar(
        years,
        values,
        bottom=bottom,
        width=0.82,
        color=COLORS[mineral],
        edgecolor="white",
        linewidth=0.35,
        label=mineral,
        zorder=3,
    )
    bottom += values

axis.set_xlim(2024.35, 2050.65)
axis.set_ylim(bottom=0)
axis.set_xticks(range(2025, 2051, 5))
axis.set_xlabel("Year", fontweight="bold")
axis.set_ylabel(
    "Cumulative lost minerals (thousand metric tonnes)",
    fontweight="bold",
)
axis.set_title(
    "Cumulative North American Minerals Lost to Lack of Recovery Standards",
    fontweight="bold",
    pad=14,
)
axis.grid(axis="y", color="#D8D8D8", linewidth=0.7, zorder=0)
axis.spines[["top", "right"]].set_visible(False)

handles = [
    Patch(facecolor=COLORS[mineral], edgecolor="none", label=mineral)
    for mineral in ["Manganese", "Copper", "Lithium", "Graphite"]
]
legend = axis.legend(
    handles=handles,
    title="Mineral",
    loc="upper left",
    bbox_to_anchor=(0.015, 0.965),
    frameon=False,
    ncol=1,
)
legend.get_title().set_fontweight("bold")

axis.text(
    0.99,
    1.01,
    "Increase LIB Capacity · Benchmark chemistry · Baseline Policy",
    transform=axis.transAxes,
    ha="right",
    va="bottom",
    fontsize=10.5,
    color="#4F5552",
)

fig.tight_layout()
for extension in ("png", "pdf", "svg"):
    fig.savefig(
        STEM.with_suffix(f".{extension}"),
        dpi=400 if extension == "png" else None,
        bbox_inches="tight",
        facecolor="white",
    )

print(STEM.with_suffix(".png"))
