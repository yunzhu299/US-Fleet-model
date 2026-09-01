#!/usr/bin/env python3
"""Figure 11: annual recycling-capacity deficit by policy and LIB capacity."""

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
DATA_OUT = ROOT / "Results" / "Data"
OUT_DIR = ROOT / "Results" / "Figures"
DATA_OUT.mkdir(parents=True, exist_ok=True)
OUT_DIR.mkdir(parents=True, exist_ok=True)
parser = argparse.ArgumentParser()
parser.add_argument(
    "--annual",
    action="store_true",
    help="Plot the yearly deficit recovered as the first difference of the cumulative level.",
)
args = parser.parse_args()

OUT_BASE = OUT_DIR / (
    "Fig11_Annual_Recycling_Capacity_Deficit"
    if args.annual
    else "Fig11_Cumulative_Recycling_Capacity_Deficit"
)

POLICIES = {
    "ACCII": "Policy Baseline",
    "Repeal": "Policy Rollback",
}
CAPACITY_LABELS = {
    "Increasing Batt Cap - Benchmark Chemistry": "Increase LIB Capacity",
    "Decreasing Batt Cap - Benchmark Chemistry": "Decrease LIB Capacity",
}
STEP_LABELS = {
    "Black Mass": "Preprocessing (black mass prod.)",
    "Refining": "Materials recovery (refining)",
}

# Consistent with the existing figure family: blue for higher capacity and
# golden yellow for lower capacity; recycling steps retain redundant line styles.
CAPACITY_COLORS = {
    "Increase LIB Capacity": "#439BC0",
    "Decrease LIB Capacity": "#7FAE3E",
}
STEP_STYLES = {
    "Preprocessing (black mass prod.)": "-",
    "Materials recovery (refining)": (0, (5, 3)),
}


frames = []
for folder, policy in POLICIES.items():
    source = DATA_ROOT / folder / "_parity" / "needed_cap_long.csv"
    frame = pd.read_csv(source)
    frame = frame[frame["Scenario"].isin(CAPACITY_LABELS)].copy()
    frame["Policy scenario"] = policy
    frame["Capacity scenario"] = frame["Scenario"].map(CAPACITY_LABELS)
    frame["Recycling step"] = frame["Recycling Step"].map(STEP_LABELS)
    frames.append(frame)

data = pd.concat(frames, ignore_index=True)
assert len(data) == 208, "Expected 2 policies × 2 capacities × 2 steps × 26 years."
assert not data[["Policy scenario", "Capacity scenario", "Recycling step", "Tonne"]].isna().any().any()

if args.annual:
    group_columns = ["Policy scenario", "Capacity scenario", "Recycling step"]
    data = data.sort_values(group_columns + ["Year"])
    data["Plot value"] = data.groupby(group_columns)["Tonne"].diff().fillna(data["Tonne"])
    # The source recurrence floors the cumulative level at zero. Negative
    # differences would indicate backlog reduction rather than a new deficit.
    data["Plot value"] = data["Plot value"].clip(lower=0)
else:
    data["Plot value"] = data["Tonne"]

data.to_csv(
    DATA_OUT
    / (
        "Fig11_Annual_Recycling_Capacity_Deficit.csv"
        if args.annual
        else "Fig11_Cumulative_Recycling_Capacity_Deficit.csv"
    ),
    index=False,
)

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 12,
        "axes.titlesize": 15,
        "axes.labelsize": 14,
        "xtick.labelsize": 12,
        "ytick.labelsize": 12,
        "legend.fontsize": 12,
        "axes.linewidth": 0.8,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axes = plt.subplots(
    1, 2, figsize=(10.8, 6.2), sharex=True, sharey=True,
    gridspec_kw={"wspace": 0.14},
)

for ax, policy in zip(axes, POLICIES.values()):
    subset = data[data["Policy scenario"] == policy]
    for capacity, color in CAPACITY_COLORS.items():
        for step, linestyle in STEP_STYLES.items():
            series = subset[
                (subset["Capacity scenario"] == capacity)
                & (subset["Recycling step"] == step)
            ].sort_values("Year")
            ax.plot(
                series["Year"],
                series["Plot value"],
                color=color,
                linestyle=linestyle,
                linewidth=2.1,
                solid_capstyle="round",
            )

    ax.set_title(policy, fontweight="bold", pad=9)
    ax.set_xlim(2025, 2050)
    ax.set_ylim(bottom=0)
    ax.set_xticks(range(2025, 2051, 5))
    ax.grid(axis="y", color="#D8D8D8", linewidth=0.75)
    ax.grid(axis="x", color="#EEEEEE", linewidth=0.6)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_color("#666666")
    ax.spines["bottom"].set_color("#666666")
    ax.tick_params(length=3.5, color="#666666")
    ax.set_xlabel("Year", fontweight="bold")

axes[0].set_ylabel(
    (
        "Annual capacity deficit (million metric tonnes/year)"
        if args.annual
        else "Cumulative capacity deficit (million metric tonnes)"
    ),
    fontweight="bold",
)

fig.suptitle(
    (
        "Annual Recycling Capacity Deficit Until 2050"
        if args.annual
        else "Cumulative Recycling Capacity Deficit Until 2050"
    ),
    fontsize=19,
    fontweight="bold",
    y=0.97,
)

capacity_handles = [
    Line2D([0], [0], color=color, lw=2.5, label=label)
    for label, color in CAPACITY_COLORS.items()
]
step_handles = [
    Line2D([0], [0], color="#333333", lw=2.5, linestyle=style, label=label)
    for label, style in STEP_STYLES.items()
]

capacity_legend = fig.legend(
    handles=capacity_handles,
    title="Capacity scenario",
    loc="lower center",
    bbox_to_anchor=(0.29, 0.025),
    frameon=False,
    ncol=1,
    handlelength=3.0,
    alignment="left",
)
capacity_legend.get_title().set_fontweight("bold")

step_legend = fig.legend(
    handles=step_handles,
    title="Recycling step",
    loc="lower center",
    bbox_to_anchor=(0.72, 0.025),
    frameon=False,
    ncol=1,
    handlelength=3.0,
    alignment="left",
)
step_legend.get_title().set_fontweight("bold")

fig.subplots_adjust(left=0.09, right=0.985, top=0.86, bottom=0.255)

fig.savefig(OUT_BASE.with_suffix(".png"), dpi=400, bbox_inches="tight", facecolor="white")

print(OUT_BASE.with_suffix(".png"))
