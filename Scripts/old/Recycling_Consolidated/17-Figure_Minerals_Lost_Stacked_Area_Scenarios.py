#!/usr/bin/env python3
"""Cumulative mineral-loss stacked areas across policy/capacity/chemistry scenarios."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
OUT = DATA_ROOT / "Scenario_Comparison"
OUT.mkdir(parents=True, exist_ok=True)
STEM = OUT / "Figure5_Cumulative_Minerals_Lost_2025_2035_Stacked_Area"

POLICIES = {"ACCII": "Baseline Policy", "Repeal": "Rollback Policy"}
SCENARIOS = [
    ("Increasing Batt Cap - Benchmark Chemistry", "Increase LIB Cap.\nBenchmark chemistry"),
    ("Increasing Batt Cap - High LFP Chemistry", "Increase LIB Cap.\nHigh LFP chemistry"),
    ("Decreasing Batt Cap - Benchmark Chemistry", "Decrease LIB Cap.\nBenchmark chemistry"),
    ("Decreasing Batt Cap - High LFP Chemistry", "Decrease LIB Cap.\nHigh LFP chemistry"),
]
STACK_ORDER = ["Graphite", "Lithium", "Manganese", "Copper", "Cobalt", "Nickel"]
COLORS = {
    "Graphite": "#6F5A8A",
    "Lithium": "#439BC0",
    "Manganese": "#7FAE3E",
    "Copper": "#C57B57",
    "Cobalt": "#8A8F91",
    "Nickel": "#3F4D49",
}
YEARS = np.arange(2025, 2036)


def load_policy(folder: str) -> dict[str, pd.DataFrame]:
    raw = pd.read_csv(DATA_ROOT / folder / "_parity" / "cap_chem_results.csv")
    annual = (
        raw.groupby(["Year", "Scenario", "Mineral"], as_index=False)[
            "Minerals Lost to Pyrometalurgy (Tonne)"
        ]
        .sum()
    )
    annual = annual[
        annual["Year"].between(2025, 2035)
        & annual["Mineral"].isin(STACK_ORDER)
    ].copy()
    annual = annual.sort_values(["Scenario", "Mineral", "Year"])
    annual["Cumulative loss"] = (
        annual.groupby(["Scenario", "Mineral"])[
            "Minerals Lost to Pyrometalurgy (Tonne)"
        ].cumsum() / 1000
    )

    result = {}
    for scenario, _ in SCENARIOS:
        wide = (
            annual[annual["Scenario"] == scenario]
            .pivot(index="Year", columns="Mineral", values="Cumulative loss")
            .reindex(index=YEARS, columns=STACK_ORDER)
            .fillna(0)
        )
        result[scenario] = wide
    return result


DATA = {folder: load_policy(folder) for folder in POLICIES}
global_max = max(
    frame.sum(axis=1).max()
    for policy_data in DATA.values()
    for frame in policy_data.values()
)

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 10,
        "axes.titlesize": 11.5,
        "axes.labelsize": 12,
        "xtick.labelsize": 9.5,
        "ytick.labelsize": 9.5,
        "legend.fontsize": 10.5,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axes = plt.subplots(
    2, 4, figsize=(14.8, 7.4), sharex=True, sharey=True,
    gridspec_kw={"hspace": 0.26, "wspace": 0.10},
)

for row, (folder, policy_label) in enumerate(POLICIES.items()):
    for col, (scenario, scenario_label) in enumerate(SCENARIOS):
        axis = axes[row, col]
        frame = DATA[folder][scenario]
        axis.stackplot(
            YEARS,
            *[frame[mineral].to_numpy() for mineral in STACK_ORDER],
            colors=[COLORS[mineral] for mineral in STACK_ORDER],
            edgecolor="white",
            linewidth=0.35,
            alpha=0.96,
            zorder=3,
        )
        if row == 0:
            axis.set_title(scenario_label, fontweight="bold", pad=8)
        axis.set_xlim(2025, 2035)
        axis.set_ylim(0, global_max * 1.05)
        axis.set_xticks([2025, 2030, 2035])
        axis.grid(axis="y", color="#D8D8D8", linewidth=0.65, zorder=0)
        axis.spines[["top", "right"]].set_visible(False)
        if row == 1:
            axis.set_xlabel("Year", fontweight="bold")

fig.suptitle(
    "Cumulative North American Minerals Lost to Lack of Recovery Standards",
    fontsize=18, fontweight="bold", y=0.98,
)
fig.text(
    0.052, 0.895, "Baseline Policy",
    ha="left", fontsize=14, fontweight="bold",
)
fig.text(
    0.052, 0.485, "Rollback Policy",
    ha="left", fontsize=14, fontweight="bold",
)
fig.supylabel(
    "Cumulative lost minerals (thousand metric tonnes)",
    x=0.012, fontsize=13, fontweight="bold",
)

legend_labels = {
    "Graphite": "Graphite",
    "Lithium": "Lithium",
    "Manganese": "Manganese",
    "Copper": "Copper",
    "Cobalt": "Cobalt (zero loss)",
    "Nickel": "Nickel (zero loss)",
}
handles = [
    Patch(facecolor=COLORS[mineral], edgecolor="none", label=legend_labels[mineral])
    for mineral in ["Manganese", "Copper", "Lithium", "Graphite", "Cobalt", "Nickel"]
]
legend = fig.legend(
    handles=handles,
    title="Mineral",
    loc="lower center",
    bbox_to_anchor=(0.5, 0.015),
    frameon=False,
    ncol=6,
    columnspacing=1.4,
    handlelength=1.8,
)
legend.get_title().set_fontweight("bold")
legend.get_title().set_fontsize(11.5)

fig.subplots_adjust(left=0.065, right=0.99, top=0.84, bottom=0.16)
for extension in ("png", "pdf", "svg"):
    fig.savefig(
        STEM.with_suffix(f".{extension}"),
        dpi=400 if extension == "png" else None,
        bbox_inches="tight",
        facecolor="white",
    )

print(STEM.with_suffix(".png"))
