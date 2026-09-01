#!/usr/bin/env python3
"""Recoverable-minerals figure with independently encoded scenario dimensions."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA = (
    ROOT / "Outputs" / "Recycling_Plots_main" / "ACCII"
    / "_parity" / "NA_cap_chem_rec.csv"
)
OUT = ROOT / "Outputs" / "Recycling_Plots_main" / "Scenario_Comparison"
OUT.mkdir(parents=True, exist_ok=True)
STEM = OUT / "Figure4_Recoverable_Minerals_Baseline_Policy"

MINERALS = ["Nickel", "Manganese", "Cobalt", "Lithium", "Graphite", "Copper"]
CAPACITY = {
    "Increasing Batt Cap": ("Increase LIB Capacity", "#439BC0"),
    "Decreasing Batt Cap": ("Decrease LIB Capacity", "#7FAE3E"),
}
CHEMISTRY = {
    "Benchmark Chemistry": ("Benchmark chemistry", None),
    "High LFP Chemistry": ("High LFP chemistry", "o"),
}
RECYCLING = {
    "Recycling Limited to NA 2025 Online or Planned": (
        "Limited to existing/planned NA capacity", "-"
    ),
    "All Material is Recycled in NA": (
        "All material recycled in North America", (0, (5, 3))
    ),
}

data = pd.read_csv(DATA)
data["Capacity key"] = data["Scenario"].apply(
    lambda value: "Increasing Batt Cap"
    if value.startswith("Increasing Batt Cap")
    else "Decreasing Batt Cap"
)
data["Chemistry key"] = data["Scenario"].apply(
    lambda value: "High LFP Chemistry"
    if "High LFP Chemistry" in value
    else "Benchmark Chemistry"
)
data["Value"] = data["Tonne"] / 1000

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 10.5,
        "axes.titlesize": 13.5,
        "axes.labelsize": 13,
        "xtick.labelsize": 10.5,
        "ytick.labelsize": 10.5,
        "legend.fontsize": 11,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axes = plt.subplots(
    2, 3, figsize=(11.2, 7.2), sharex=True,
    gridspec_kw={"hspace": 0.32, "wspace": 0.28},
)

for axis, mineral in zip(axes.flat, MINERALS):
    subset = data[data["Mineral"] == mineral]
    for capacity_key, (_, color) in CAPACITY.items():
        for chemistry_key, (_, marker) in CHEMISTRY.items():
            for recycling_scenario, (_, linestyle) in RECYCLING.items():
                series = subset[
                    (subset["Capacity key"] == capacity_key)
                    & (subset["Chemistry key"] == chemistry_key)
                    & (subset["Recycling Scenario"] == recycling_scenario)
                ].sort_values("Year")
                axis.plot(
                    series["Year"],
                    series["Value"],
                    color=color,
                    linestyle=linestyle,
                    marker=marker,
                    markevery=4,
                    markersize=4.2,
                    markerfacecolor="white",
                    markeredgewidth=1.1,
                    linewidth=2.05,
                    zorder=3,
                )

    axis.set_title(mineral, fontweight="bold", pad=7)
    axis.set_xlim(2025, 2050)
    axis.set_ylim(bottom=0)
    axis.set_xticks(range(2025, 2051, 5))
    axis.tick_params(axis="x", rotation=30)
    axis.grid(axis="y", color="#D8D8D8", linewidth=0.7)
    axis.grid(axis="x", color="#EEEEEE", linewidth=0.55)
    axis.spines[["top", "right"]].set_visible(False)

fig.suptitle(
    "North American Yearly Recoverable Minerals Until 2050",
    fontsize=18,
    fontweight="bold",
    y=0.975,
)
fig.text(
    0.5, 0.180, "Year", ha="center", va="center",
    fontsize=13, fontweight="bold",
)
fig.supylabel(
    "Recoverable minerals (thousand metric tonnes)",
    x=0.018, fontsize=13, fontweight="bold",
)

capacity_handles = [
    Line2D([0], [0], color=color, lw=2.7, label=label)
    for label, color in CAPACITY.values()
]
chemistry_handles = [
    Line2D(
        [0], [0], color="#333333", lw=2.2, marker=marker,
        markersize=5.5, markerfacecolor="white", markeredgewidth=1.2,
        label=label,
    )
    for label, marker in CHEMISTRY.values()
]
recycling_handles = [
    Line2D([0], [0], color="#333333", lw=2.5, linestyle=linestyle, label=label)
    for label, linestyle in RECYCLING.values()
]

capacity_legend = fig.legend(
    capacity_handles,
    [handle.get_label() for handle in capacity_handles],
    title="Capacity scenario",
    loc="lower center",
    bbox_to_anchor=(0.20, 0.025),
    frameon=False,
    ncol=1,
    handlelength=3.2,
)
chemistry_legend = fig.legend(
    chemistry_handles,
    [handle.get_label() for handle in chemistry_handles],
    title="Chemistry scenario",
    loc="lower center",
    bbox_to_anchor=(0.49, 0.025),
    frameon=False,
    ncol=1,
    handlelength=3.2,
)
recycling_legend = fig.legend(
    recycling_handles,
    [handle.get_label() for handle in recycling_handles],
    title="Recycling availability",
    loc="lower center",
    bbox_to_anchor=(0.79, 0.025),
    frameon=False,
    ncol=1,
    handlelength=3.2,
)
for legend in (capacity_legend, chemistry_legend, recycling_legend):
    legend.get_title().set_fontweight("bold")
    legend.get_title().set_fontsize(12)

fig.text(
    0.5, 0.922, "Baseline Policy",
    ha="center", fontsize=12.5, fontweight="bold", color="#4F5552",
)
fig.subplots_adjust(left=0.085, right=0.985, top=0.87, bottom=0.25)

for extension in ("png", "pdf", "svg"):
    fig.savefig(
        STEM.with_suffix(f".{extension}"),
        dpi=400 if extension == "png" else None,
        bbox_inches="tight",
        facecolor="white",
    )

print(STEM.with_suffix(".png"))
