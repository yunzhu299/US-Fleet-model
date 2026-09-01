#!/usr/bin/env python3
"""2035 cumulative mineral losses as stacked scenario-comparison bars."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
OUT = DATA_ROOT / "Scenario_Comparison"
OUT.mkdir(parents=True, exist_ok=True)
STEM = OUT / "Figure5_Cumulative_Minerals_Lost_2035_Stacked_Bars"

POLICIES = {"ACCII": "Baseline Policy", "Repeal": "Rollback Policy"}
SCENARIOS = [
    ("Increasing Batt Cap - Benchmark Chemistry", "Increase Cap.\nBenchmark"),
    ("Increasing Batt Cap - High LFP Chemistry", "Increase Cap.\nHigh LFP"),
    ("Decreasing Batt Cap - Benchmark Chemistry", "Decrease Cap.\nBenchmark"),
    ("Decreasing Batt Cap - High LFP Chemistry", "Decrease Cap.\nHigh LFP"),
]
STACK_ORDER = ["Graphite", "Lithium", "Manganese", "Copper"]
COLORS = {
    "Graphite": "#6F5A8A",
    "Lithium": "#439BC0",
    "Manganese": "#7FAE3E",
    "Copper": "#C57B57",
}


def load_policy(folder: str) -> pd.DataFrame:
    raw = pd.read_csv(DATA_ROOT / folder / "_parity" / "cap_chem_results.csv")
    annual = (
        raw[raw["Year"].between(2025, 2035)]
        .groupby(["Year", "Scenario", "Mineral"], as_index=False)[
            "Minerals Lost to Pyrometalurgy (Tonne)"
        ]
        .sum()
        .sort_values(["Scenario", "Mineral", "Year"])
    )
    annual["Cumulative loss"] = (
        annual.groupby(["Scenario", "Mineral"])[
            "Minerals Lost to Pyrometalurgy (Tonne)"
        ].cumsum() / 1000
    )
    return annual[
        (annual["Year"] == 2035) & annual["Mineral"].isin(STACK_ORDER)
    ]


DATA = {folder: load_policy(folder) for folder in POLICIES}
global_max = max(
    frame.groupby("Scenario")["Cumulative loss"].sum().max()
    for frame in DATA.values()
)

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 11,
        "axes.titlesize": 15,
        "axes.labelsize": 13,
        "xtick.labelsize": 10.5,
        "ytick.labelsize": 11,
        "legend.fontsize": 11,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axes = plt.subplots(
    1, 2, figsize=(11.8, 6.5), sharey=True,
    gridspec_kw={"wspace": 0.12},
)
x = np.arange(len(SCENARIOS))

for axis, (folder, policy_label) in zip(axes, POLICIES.items()):
    frame = DATA[folder]
    bottom = np.zeros(len(SCENARIOS))
    for mineral in STACK_ORDER:
        values = np.array(
            [
                frame.loc[
                    (frame["Scenario"] == scenario)
                    & (frame["Mineral"] == mineral),
                    "Cumulative loss",
                ].sum()
                for scenario, _ in SCENARIOS
            ]
        )
        axis.bar(
            x, values, bottom=bottom, width=0.72,
            color=COLORS[mineral], edgecolor="white", linewidth=0.55,
            zorder=3,
        )
        bottom += values

    for xpos, total in zip(x, bottom):
        axis.text(
            xpos, total + global_max * 0.018, f"{total:.1f}",
            ha="center", va="bottom", fontsize=10, fontweight="bold",
        )

    axis.set_title(policy_label, loc="left", fontweight="bold", pad=10)
    axis.set_xticks(x, [label for _, label in SCENARIOS])
    axis.set_ylim(0, global_max * 1.13)
    axis.grid(axis="y", color="#D8D8D8", linewidth=0.7, zorder=0)
    axis.spines[["top", "right"]].set_visible(False)
    axis.set_xlabel("Capacity and chemistry scenario", fontweight="bold")

axes[0].set_ylabel(
    "Cumulative lost minerals (thousand metric tonnes)",
    fontweight="bold",
)
fig.suptitle(
    "Cumulative North American Minerals Lost to Lack of Recovery Standards (2035)",
    fontsize=18, fontweight="bold", y=0.975,
)

handles = [
    Patch(facecolor=COLORS[mineral], edgecolor="none", label=mineral)
    for mineral in ["Manganese", "Copper", "Lithium", "Graphite"]
]
legend = fig.legend(
    handles=handles,
    title="Mineral",
    loc="lower center",
    bbox_to_anchor=(0.5, 0.015),
    frameon=False,
    ncol=4,
    columnspacing=1.8,
    handlelength=2.0,
)
legend.get_title().set_fontweight("bold")
legend.get_title().set_fontsize(12)

fig.text(
    0.5, 0.105,
    "Cobalt and Nickel have zero modeled non-recovery loss.",
    ha="center", fontsize=10.5, color="#4F5552",
)
fig.subplots_adjust(left=0.085, right=0.985, top=0.87, bottom=0.24)

for extension in ("png", "pdf", "svg"):
    fig.savefig(
        STEM.with_suffix(f".{extension}"),
        dpi=400 if extension == "png" else None,
        bbox_inches="tight",
        facecolor="white",
    )

print(STEM.with_suffix(".png"))
