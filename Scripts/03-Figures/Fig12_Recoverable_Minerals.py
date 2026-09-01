#!/usr/bin/env python3
"""Figure 12: recoverable minerals under policy and technology scenarios."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
DATA_OUT = ROOT / "Results" / "Data"
OUT = ROOT / "Results" / "Figures"
DATA_OUT.mkdir(parents=True, exist_ok=True)
OUT.mkdir(parents=True, exist_ok=True)
STEM = OUT / "Fig12_Recoverable_Minerals"

POLICIES = {"ACCII": "Policy Baseline", "Repeal": "Policy Rollback"}
ROWS = [
    ["Nickel", "Manganese", "Cobalt"],
    ["Lithium", "Graphite", "Copper"],
]
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


def load_policy(folder: str) -> pd.DataFrame:
    data = pd.read_csv(DATA_ROOT / folder / "_parity" / "NA_cap_chem_rec.csv")
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
    return data


DATA = {folder: load_policy(folder) for folder in POLICIES}
all_data = pd.concat(
    [
        frame.assign(**{"Policy scenario": POLICIES[folder]})
        for folder, frame in DATA.items()
    ],
    ignore_index=True,
)
all_data.to_csv(DATA_OUT / "Fig12_Recoverable_Minerals.csv", index=False)
Y_MAX = {
    mineral: all_data.loc[all_data["Mineral"] == mineral, "Value"].max() * 1.07
    for row in ROWS for mineral in row
}

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 9.5,
        "axes.titlesize": 12,
        "xtick.labelsize": 9,
        "ytick.labelsize": 9,
        "legend.fontsize": 10.5,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axes = plt.subplots(
    2, 6, figsize=(15.8, 7.4), sharex=True,
    gridspec_kw={"hspace": 0.34, "wspace": 0.30},
)

for policy_index, (folder, policy_label) in enumerate(POLICIES.items()):
    data = DATA[folder]
    col_offset = policy_index * 3
    for row_index, minerals in enumerate(ROWS):
        for local_col, mineral in enumerate(minerals):
            axis = axes[row_index, col_offset + local_col]
            subset = data[data["Mineral"] == mineral]

            for capacity_key, (_, color) in CAPACITY.items():
                for chemistry_key, (_, marker) in CHEMISTRY.items():
                    for recycling_key, (_, linestyle) in RECYCLING.items():
                        series = subset[
                            (subset["Capacity key"] == capacity_key)
                            & (subset["Chemistry key"] == chemistry_key)
                            & (subset["Recycling Scenario"] == recycling_key)
                        ].sort_values("Year")
                        axis.plot(
                            series["Year"], series["Value"],
                            color=color, linestyle=linestyle,
                            marker=marker, markevery=4, markersize=3.5,
                            markerfacecolor="white", markeredgewidth=0.95,
                            linewidth=1.75, zorder=3,
                        )

            axis.set_title(mineral, fontweight="bold", pad=6)
            axis.set_xlim(2025, 2050)
            axis.set_ylim(0, Y_MAX[mineral])
            axis.set_xticks(range(2025, 2051, 5))
            axis.tick_params(axis="x", rotation=35)
            axis.grid(axis="y", color="#D8D8D8", linewidth=0.65)
            axis.grid(axis="x", color="#EEEEEE", linewidth=0.5)
            axis.spines[["top", "right"]].set_visible(False)

fig.suptitle(
    "North American Yearly Recoverable Minerals Until 2050",
    fontsize=18, fontweight="bold", y=0.98,
)
fig.text(0.055, 0.925, "Policy Baseline", ha="left", fontsize=14, fontweight="bold")
fig.text(0.545, 0.925, "Policy Rollback", ha="left", fontsize=14, fontweight="bold")
fig.supylabel(
    "Recoverable minerals (thousand metric tonnes)",
    x=0.012, fontsize=13, fontweight="bold",
)
fig.text(0.5, 0.178, "Year", ha="center", fontsize=13, fontweight="bold")

capacity_handles = [
    Line2D([0], [0], color=color, lw=2.6, label=label)
    for label, color in CAPACITY.values()
]
chemistry_handles = [
    Line2D(
        [0], [0], color="#333333", lw=2.2, marker=marker,
        markersize=5.2, markerfacecolor="white", markeredgewidth=1.1,
        label=label,
    )
    for label, marker in CHEMISTRY.values()
]
recycling_handles = [
    Line2D([0], [0], color="#333333", lw=2.4, linestyle=style, label=label)
    for label, style in RECYCLING.values()
]

legends = [
    fig.legend(
        capacity_handles, [h.get_label() for h in capacity_handles],
        title="Capacity scenario", loc="lower center",
        bbox_to_anchor=(0.20, 0.043), frameon=False, ncol=1,
        handlelength=3.0,
    ),
    fig.legend(
        chemistry_handles, [h.get_label() for h in chemistry_handles],
        title="Chemistry scenario", loc="lower center",
        bbox_to_anchor=(0.49, 0.043), frameon=False, ncol=1,
        handlelength=3.0,
    ),
    fig.legend(
        recycling_handles, [h.get_label() for h in recycling_handles],
        title="Recycling availability", loc="lower center",
        bbox_to_anchor=(0.79, 0.043), frameon=False, ncol=1,
        handlelength=3.0,
    ),
]
for legend in legends:
    legend.get_title().set_fontweight("bold")
    legend.get_title().set_fontsize(11.5)

# Visual separator between policy blocks.
fig.add_artist(
    Line2D([0.505, 0.505], [0.245, 0.89], transform=fig.transFigure,
           color="#C8C8C8", linewidth=1.0)
)
fig.subplots_adjust(left=0.055, right=0.99, top=0.86, bottom=0.25)

fig.savefig(STEM.with_suffix(".png"), dpi=400, bbox_inches="tight", facecolor="white")

print(STEM.with_suffix(".png"))
