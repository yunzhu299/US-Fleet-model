#!/usr/bin/env python3
"""Figure 10: North American LIB supply-chain flows from 2025 to 2050."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
MAIN = ROOT / "Outputs" / "Recycling_Plots_main"
DATA_OUT = ROOT / "Results" / "Data"
OUT = ROOT / "Results" / "Figures"
DATA_OUT.mkdir(parents=True, exist_ok=True)
OUT.mkdir(parents=True, exist_ok=True)
STEM = OUT / "Fig10_North_American_Flows_Over_Time"

POLICIES = {"ACCII": "Policy Baseline", "Repeal": "Policy Rollback"}
CAPACITY_STYLES = {
    "Increase LIB Capacity": "-",
    "Decrease LIB Capacity": (0, (5, 3)),
}
COLORS = {
    "LIB demand": "#439BC0",
    "Pack Mfg.": "#E3AD17",
    "Cell Mfg.": "#EFD58A",
    "EoL batteries": "#A9C84F",
    "Mfg. Scrap": "#DDEFA1",
    "Preprocessing (black mass prod.)": "#3F4D49",
    "Materials recovery (refining)": "#9F998C",
}

METRIC_MAP = {
    "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)": ("LIB demand", "Increase LIB Capacity"),
    "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)": ("LIB demand", "Decrease LIB Capacity"),
    "Pack Manufacturing": ("Pack Mfg.", "Increase LIB Capacity"),
    "Decreasing Batt Cap Pack Manufacturing": ("Pack Mfg.", "Decrease LIB Capacity"),
    "Cell Manufacturing": ("Cell Mfg.", "Increase LIB Capacity"),
    "Decreasing Batt Cap Cell Manufacturing": ("Cell Mfg.", "Decrease LIB Capacity"),
    "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)": ("EoL batteries", "Increase LIB Capacity"),
    "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)": ("EoL batteries", "Decrease LIB Capacity"),
    "Black Mass": ("Preprocessing (black mass prod.)", "Installed"),
    "Refining": ("Materials recovery (refining)", "Installed"),
}


def load_policy(folder: str) -> pd.DataFrame:
    overtime = pd.read_csv(MAIN / folder / "_parity" / "NA_overtime_data.csv")
    mapped = overtime["Metric"].map(METRIC_MAP).apply(pd.Series)
    overtime[["Display metric", "Capacity scenario"]] = mapped

    scrap = pd.read_csv(MAIN / folder / "_parity" / "NA_mfg_scrap_overtime.csv").melt(
        id_vars="Year",
        value_vars=["Tonnes_Scrap_proj_mid", "Tonnes_Scrap_15_mid"],
        var_name="source",
        value_name="Tonnes",
    )
    scrap["Display metric"] = "Mfg. Scrap"
    scrap["Capacity scenario"] = scrap["source"].map(
        {
            "Tonnes_Scrap_proj_mid": "Increase LIB Capacity",
            "Tonnes_Scrap_15_mid": "Decrease LIB Capacity",
        }
    )
    return pd.concat(
        [
            overtime[["Year", "Tonnes", "Display metric", "Capacity scenario"]],
            scrap[["Year", "Tonnes", "Display metric", "Capacity scenario"]],
        ],
        ignore_index=True,
    )


data = {folder: load_policy(folder) for folder in POLICIES}
pd.concat(
    [
        frame.assign(**{"Policy scenario": POLICIES[folder]})
        for folder, frame in data.items()
    ],
    ignore_index=True,
).to_csv(DATA_OUT / "Fig10_North_American_Flows_Over_Time.csv", index=False)

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 11,
        "axes.titlesize": 16,
        "axes.labelsize": 14,
        "xtick.labelsize": 12,
        "ytick.labelsize": 12,
        "legend.fontsize": 11,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axes = plt.subplots(
    1, 2, figsize=(12.0, 6.7), sharex=True, sharey=True,
    gridspec_kw={"wspace": 0.12},
)

for axis, (folder, policy) in zip(axes, POLICIES.items()):
    frame = data[folder]
    for metric, color in COLORS.items():
        subset = frame[frame["Display metric"] == metric]
        if metric in {
            "Preprocessing (black mass prod.)",
            "Materials recovery (refining)",
        }:
            series = subset.sort_values("Year")
            axis.plot(series["Year"], series["Tonnes"], color=color, linewidth=2.4, zorder=4)
        else:
            for scenario, linestyle in CAPACITY_STYLES.items():
                series = subset[subset["Capacity scenario"] == scenario].sort_values("Year")
                axis.plot(
                    series["Year"], series["Tonnes"],
                    color=color, linestyle=linestyle, linewidth=2.1, zorder=3,
                )

    axis.set_title(policy, fontweight="bold", pad=10)
    axis.set_xlim(2025, 2050)
    axis.set_ylim(0, 16.2)
    axis.set_xticks(range(2025, 2051, 5))
    axis.set_yticks(range(0, 17, 2))
    axis.set_xlabel("Year", fontweight="bold")
    axis.grid(axis="y", color="#D8D8D8", linewidth=0.7)
    axis.grid(axis="x", color="#EEEEEE", linewidth=0.55)
    axis.spines[["top", "right"]].set_visible(False)

axes[0].set_ylabel("Million metric tonnes", fontweight="bold")
fig.suptitle(
    "North American Demand, Manufacturing and Recycling Quantities Over Time",
    fontsize=19, fontweight="bold", y=0.975,
)

capacity_handles = [
    Line2D([0], [0], color="#444444", lw=2.4, linestyle=style, label=label)
    for label, style in CAPACITY_STYLES.items()
]
flow_handles = [
    Line2D([0], [0], color=COLORS[label], lw=2.7, label=label)
    for label in ["LIB demand", "Pack Mfg.", "Cell Mfg."]
]
feedstock_handles = [
    Line2D([0], [0], color=COLORS[label], lw=2.7, label=label)
    for label in ["EoL batteries", "Mfg. Scrap"]
]
recycling_handles = [
    Line2D([0], [0], color=COLORS[label], lw=2.7, label=label)
    for label in [
        "Preprocessing (black mass prod.)",
        "Materials recovery (refining)",
    ]
]

legends = [
    fig.legend(
        capacity_handles, [h.get_label() for h in capacity_handles],
        title="Capacity scenario", loc="lower center",
        bbox_to_anchor=(0.14, 0.015), frameon=False, ncol=1,
    ),
    fig.legend(
        flow_handles, [h.get_label() for h in flow_handles],
        title="Battery mass flows", loc="lower center",
        bbox_to_anchor=(0.39, 0.015), frameon=False, ncol=1,
    ),
    fig.legend(
        feedstock_handles, [h.get_label() for h in feedstock_handles],
        title="Recycling feedstock", loc="lower center",
        bbox_to_anchor=(0.61, 0.015), frameon=False, ncol=1,
    ),
    fig.legend(
        recycling_handles, [h.get_label() for h in recycling_handles],
        title="Installed recycling capacity", loc="lower center",
        bbox_to_anchor=(0.85, 0.015), frameon=False, ncol=1,
    ),
]
for legend in legends:
    legend.get_title().set_fontweight("bold")

fig.subplots_adjust(left=0.075, right=0.985, top=0.86, bottom=0.25)
fig.savefig(STEM.with_suffix(".png"), dpi=400, bbox_inches="tight", facecolor="white")

print(STEM.with_suffix(".png"))
