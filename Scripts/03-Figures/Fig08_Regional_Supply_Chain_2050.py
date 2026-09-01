#!/usr/bin/env python3
"""Figure 8: regional 2050 LIB demand, manufacturing, feedstock, and capacity."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_FILE = ROOT / "Outputs" / "Recycling_Plots_main" / "ACCII" / "_parity" / "Mass_2050_projected.csv"
DATA_OUT = ROOT / "Results" / "Data"
OUTPUT_DIR = ROOT / "Results" / "Figures"
DATA_OUT.mkdir(parents=True, exist_ok=True)
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
OUTPUT_STEM = OUTPUT_DIR / "Fig08_Regional_Supply_Chain_2050"

REGIONS = [
    "US-West", "US-Mountain", "US-Midwest", "US-South", "US-East",
    "Canada-West", "Canada-Mountain", "Canada-Midwest", "Canada-East", "Mexico",
]
SEGMENTS = [
    "LIB Demand",
    "Pack Manufacturing",
    "Cell Manufacturing",
    "End of Life Batteries",
    "Manufacturing Scrap",
    "Black Mass",
    "Refining",
]
COLUMNS = [
    "Add_LIB_proj_tonnes",
    "Tonnes_Prod_proj_down",
    "Tonnes_Prod_proj_mid",
    "Recycle_Batt_Proj",
    "Tonnes_Scrap_proj_mid",
    "Cumulative_black_mass_cap",
    "Cumulative_refining_cap",
]
SHORT_LABELS = [
    "LIB demand",
    "Pack Mfg.",
    "Cell Mfg.",
    "EoL batteries",
    "Mfg. Scrap",
    "Preprocessing (black mass prod.)",
    "Materials recovery (refining)",
]
COLORS = ["#439BC0", "#E7BD3F", "#EFD36F", "#A9C84F", "#DDEFA1", "#3F4D49", "#9F998C"]
BACKGROUND = "#FFFFFF"
# Extra spacing makes the functional groups readable even without x-axis labels.
X_POSITIONS = np.array([0.0, 1.30, 2.08, 3.38, 4.18, 5.38, 6.16])

REGION_GROUPS = {
    "US-West": "WA OR CA NV ID HI AK".split(),
    "US-Mountain": "MT WY UT CO AZ NM".split(),
    "US-Midwest": "OH IN IL MI WI MN IA MO ND SD NE KS".split(),
    "US-South": "TX OK AR LA KY TN MS AL".split(),
    "US-East": "ME NH VT MA RI CT NY NJ PA DE MD DC VA WV NC SC GA FL".split(),
    "Canada-West": "BC YT".split(),
    "Canada-Mountain": ["AB"],
    "Canada-Midwest": "MB SK".split(),
    "Canada-East": "ON QC NB NS PE NL NT NU".split(),
    "Mexico": ["MX", "SLP"],
}
REGION_LOOKUP = {
    state: region for region, states in REGION_GROUPS.items() for state in states
}


def main() -> None:
    raw = pd.read_csv(DATA_FILE)
    raw["Region"] = raw["State_Province"].astype(str).str.strip().map(REGION_LOOKUP)
    data = (
        raw.dropna(subset=["Region"])
        .groupby("Region", as_index=False)[COLUMNS]
        .sum()
    )
    source_data = data.melt(
        id_vars="Region",
        value_vars=COLUMNS,
        var_name="Source metric",
        value_name="Metric tonnes",
    )
    source_data["Display metric"] = source_data["Source metric"].map(
        dict(zip(COLUMNS, SHORT_LABELS))
    )
    source_data["Million metric tonnes"] = source_data["Metric tonnes"] / 1e6
    source_data.to_csv(
        DATA_OUT / "Fig08_Regional_Supply_Chain_2050.csv", index=False
    )

    plt.rcParams.update(
        {
            "font.family": "DejaVu Sans",
            "font.size": 9,
            "axes.titlesize": 10.5,
            "axes.titleweight": "normal",
            "axes.labelsize": 11,
            "xtick.labelsize": 7,
            "ytick.labelsize": 9,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
            "axes.unicode_minus": False,
        }
    )

    fig, axes = plt.subplots(
        2,
        5,
        figsize=(9.6, 5.35),
        sharex=True,
        sharey=True,
        constrained_layout=False,
    )
    fig.patch.set_facecolor(BACKGROUND)

    for axis, region in zip(axes.flat, REGIONS):
        axis.set_facecolor(BACKGROUND)
        row = data.loc[data["Region"] == region, COLUMNS]
        values = row.iloc[0].to_numpy(dtype=float) / 1e6 if len(row) else np.zeros(len(COLUMNS))
        axis.bar(
            X_POSITIONS,
            values,
            width=0.66,
            color=COLORS,
            edgecolor="black",
            linewidth=0.45,
            zorder=3,
        )
        axis.set_title(region, pad=5)
        axis.set_xlim(-0.55, 6.72)
        axis.set_ylim(0, 5.15)
        axis.set_xticks([])
        axis.set_yticks(np.arange(0, 5.1, 1))
        axis.grid(axis="y", color="#d8d3c8", linewidth=0.65, zorder=0)
        axis.spines[["top", "right", "bottom"]].set_visible(False)
        axis.spines["left"].set_color("#777777")
        axis.spines["left"].set_linewidth(0.55)
        axis.tick_params(axis="y", length=2.5, width=0.55, color="#777777")

    fig.suptitle(
        "North American Demand, Manufacturing and Recycling Quantities by Region in 2050",
        fontsize=13,
        fontweight="bold",
        y=0.975,
    )
    fig.supylabel("Million metric tonnes", x=0.032, fontsize=12, fontweight="bold")

    production_handles = [
        Patch(facecolor=color, edgecolor="black", linewidth=0.45, label=label)
        for color, label in zip(COLORS[:3], SHORT_LABELS[:3])
    ]
    feedstock_handles = [
        Patch(facecolor=color, edgecolor="black", linewidth=0.45, label=label)
        for color, label in zip(COLORS[3:5], SHORT_LABELS[3:5])
    ]
    capacity_handles = [
        Patch(facecolor=color, edgecolor="black", linewidth=0.45, label=label)
        for color, label in zip(COLORS[5:], SHORT_LABELS[5:])
    ]
    flow_legend = fig.legend(
        handles=production_handles,
        title="Battery mass flows",
        loc="lower center",
        bbox_to_anchor=(0.50, 0.105),
        ncol=3,
        frameon=False,
        fontsize=9.5,
        handlelength=1.15,
        columnspacing=1.0,
    )
    flow_legend.get_title().set_fontweight("bold")
    flow_legend.get_title().set_fontsize(10.5)
    feedstock_legend = fig.legend(
        handles=feedstock_handles,
        title="Recycling feedstock",
        loc="lower center",
        bbox_to_anchor=(0.27, 0.018),
        ncol=2,
        frameon=False,
        fontsize=9.5,
        handlelength=1.0,
        columnspacing=0.9,
    )
    feedstock_legend.get_title().set_fontweight("bold")
    feedstock_legend.get_title().set_fontsize(10.5)
    capacity_legend = fig.legend(
        handles=capacity_handles,
        title="Installed recycling capacity",
        loc="lower center",
        bbox_to_anchor=(0.70, 0.018),
        ncol=2,
        frameon=False,
        fontsize=9.5,
        handlelength=1.0,
        columnspacing=0.9,
    )
    capacity_legend.get_title().set_fontweight("bold")
    capacity_legend.get_title().set_fontsize(10.5)

    fig.subplots_adjust(left=0.075, right=0.988, top=0.875, bottom=0.195, wspace=0.075, hspace=0.16)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    fig.savefig(f"{OUTPUT_STEM}.png", dpi=300, bbox_inches="tight", facecolor=BACKGROUND)


if __name__ == "__main__":
    main()
