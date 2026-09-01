#!/usr/bin/env python3
"""Additional North American recycling capacity needed through 2050."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
OUT = DATA_ROOT / "Scenario_Comparison"
OUT.mkdir(parents=True, exist_ok=True)

POLICIES = {
    "ACCII": "Baseline Policy",
    "Repeal": "Rollback Policy",
}
SCENARIO = "Increasing Batt Cap - Benchmark Chemistry"
STAGES = {
    "Black Mass": (
        "Preprocessing (black mass production)", "#3F4D49", "-"
    ),
    "Refining": (
        "Materials recovery (refining)", "#E7BD3F", (0, (5, 2.5))
    ),
}


def load_policy(folder, label):
    path = DATA_ROOT / folder / "_parity" / "needed_cap_long.csv"
    data = pd.read_csv(path)
    data = data[data["Scenario"] == SCENARIO].copy()
    data["Policy"] = label
    return data


data = pd.concat(
    [load_policy(folder, label) for folder, label in POLICIES.items()],
    ignore_index=True,
)

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 11,
        "axes.titlesize": 15,
        "axes.labelsize": 13,
        "xtick.labelsize": 11,
        "ytick.labelsize": 11,
        "legend.fontsize": 11.5,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axes = plt.subplots(
    1, 2, figsize=(12.6, 6.4), sharex=True, sharey=True,
    gridspec_kw={"wspace": 0.10},
)

for axis, policy in zip(axes, POLICIES.values()):
    subset = data[data["Policy"] == policy]
    for stage, (_, color, linestyle) in STAGES.items():
        series = subset[subset["Recycling Step"] == stage].sort_values("Year")
        axis.plot(
            series["Year"], series["Tonne"],
            color=color, linestyle=linestyle, linewidth=2.8,
        )
        report = series[series["Year"].isin([2030, 2035, 2040, 2045, 2050])]
        axis.scatter(
            report["Year"], report["Tonne"],
            s=25, facecolor="white", edgecolor=color,
            linewidth=1.3, zorder=3,
        )

    axis.set_title(policy, fontweight="bold", pad=9)
    axis.set_xlim(2025, 2050)
    axis.set_ylim(bottom=0)
    axis.set_xticks(range(2025, 2051, 5))
    axis.set_xlabel("Year", fontweight="bold")
    axis.grid(axis="y", color="#D8D8D8", linewidth=0.75)
    axis.grid(axis="x", color="#EEEEEE", linewidth=0.55)
    axis.spines[["top", "right"]].set_visible(False)

axes[0].set_ylabel(
    "Additional capacity needed\n(million metric tonnes/year)",
    fontweight="bold",
)

fig.suptitle(
    "Additional North American Recycling Capacity Needed",
    fontsize=18, fontweight="bold", y=0.985,
)
fig.text(
    0.5, 0.925,
    "Increase LIB Cap · Benchmark Chemistry · "
    "Includes EoL batteries and manufacturing scrap",
    ha="center", fontsize=11.5, color="#3F4D49",
)

handles = [
    Line2D(
        [0], [0], color=color, linestyle=linestyle, linewidth=2.8,
        label=label,
    )
    for label, color, linestyle in STAGES.values()
]
fig.legend(
    handles=handles, loc="lower center", bbox_to_anchor=(0.5, 0.02),
    ncol=2, frameon=False, handlelength=3.2,
)
fig.subplots_adjust(left=0.105, right=0.985, top=0.82, bottom=0.19)

stem = OUT / "Figure8_Additional_Recycling_Capacity_Needed"
for extension in ("png", "pdf", "svg"):
    fig.savefig(
        f"{stem}.{extension}",
        dpi=400 if extension == "png" else None,
        bbox_inches="tight", facecolor="white",
    )
plt.close(fig)

print(f"{stem}.png")
