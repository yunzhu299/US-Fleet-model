#!/usr/bin/env python3
"""Publication-style national demand/manufacturing/recycling time series."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_FILE = ROOT / "Outputs" / "Recycling_Plots_main" / "ACCII" / "_parity" / "NA_overtime_data.csv"
OUTPUT_DIR = ROOT / "Outputs" / "Recycling_Plots_main" / "ACCII"
OUTPUT_STEM = OUTPUT_DIR / "Plotting_Demand_Recycle_Manu_08"

SERIES = {
    "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)": ("LIB demand", "#439BC0", "-"),
    "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)": ("LIB demand", "#439BC0", "--"),
    "Pack Manufacturing": ("Pack Mfg.", "#E3AD17", "-"),
    "Decreasing Batt Cap Pack Manufacturing": ("Pack Mfg.", "#E3AD17", "--"),
    "Cell Manufacturing": ("Cell Mfg.", "#EFD58A", "-"),
    "Decreasing Batt Cap Cell Manufacturing": ("Cell Mfg.", "#EFD58A", "--"),
    "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)": ("EoL batteries", "#A9C84F", "-"),
    "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)": ("EoL batteries", "#A9C84F", "--"),
    "Black Mass": ("Preprocessing (black mass prod.)", "#3F4D49", "-"),
    "Refining": ("Materials recovery (refining)", "#9F998C", "-"),
}


def main() -> None:
    data = pd.read_csv(DATA_FILE)

    plt.rcParams.update(
        {
            "font.family": "DejaVu Sans",
            "font.size": 9,
            "axes.titlesize": 13,
            "axes.titleweight": "bold",
            "axes.labelsize": 11,
            "xtick.labelsize": 9,
            "ytick.labelsize": 9,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
        }
    )

    fig, axis = plt.subplots(figsize=(9.6, 5.5))
    for metric, (_, color, linestyle) in SERIES.items():
        subset = data.loc[data["Metric"] == metric].sort_values("Year")
        axis.plot(
            subset["Year"],
            subset["Tonnes"],
            color=color,
            linestyle=linestyle,
            linewidth=2.0,
            solid_capstyle="round",
            dash_capstyle="butt",
            zorder=3,
        )

    axis.set_xlim(2024.5, 2050.5)
    axis.set_ylim(bottom=0)
    axis.set_xticks([2025, 2030, 2035, 2040, 2045, 2050])
    axis.set_xlabel("Year", fontweight="bold")
    axis.set_ylabel("Million metric tonnes", fontweight="bold")
    axis.set_title(
        "North American Demand, Manufacturing and Recycling Quantities Over Time",
        pad=12,
    )
    axis.grid(axis="y", color="#d9d9d9", linewidth=0.7, zorder=0)
    axis.grid(axis="x", color="#eeeeee", linewidth=0.55, zorder=0)
    axis.spines[["top", "right"]].set_visible(False)

    scenario_handles = [
        Line2D([0], [0], color="#444444", linewidth=2.0, linestyle="-", label="Increasing LIB Capacity"),
        Line2D([0], [0], color="#444444", linewidth=2.0, linestyle="--", label="Decreasing LIB Capacity"),
    ]
    flow_handles = [
        Line2D([0], [0], color=color, linewidth=2.5, label=label)
        for label, color in [
            ("LIB demand", "#439BC0"),
            ("Pack Mfg.", "#E3AD17"),
            ("Cell Mfg.", "#EFD58A"),
            ("EoL batteries", "#A9C84F"),
        ]
    ]
    capacity_handles = [
        Line2D([0], [0], color="#3F4D49", linewidth=2.5, label="Preprocessing (black mass prod.)"),
        Line2D([0], [0], color="#9F998C", linewidth=2.5, label="Materials recovery (refining)"),
    ]

    scenario_legend = fig.legend(
        handles=scenario_handles,
        title="Capacity scenario",
        loc="lower center",
        bbox_to_anchor=(0.20, 0.015),
        ncol=1,
        frameon=False,
        fontsize=8.5,
        handlelength=2.6,
    )
    scenario_legend.get_title().set_fontweight("bold")
    flow_legend = fig.legend(
        handles=flow_handles,
        title="Battery mass flows",
        loc="lower center",
        bbox_to_anchor=(0.50, 0.015),
        ncol=2,
        frameon=False,
        fontsize=8.5,
        handlelength=2.2,
        columnspacing=1.0,
    )
    flow_legend.get_title().set_fontweight("bold")
    capacity_legend = fig.legend(
        handles=capacity_handles,
        title="Installed recycling capacity",
        loc="lower center",
        bbox_to_anchor=(0.82, 0.015),
        ncol=1,
        frameon=False,
        fontsize=8.5,
        handlelength=2.2,
    )
    capacity_legend.get_title().set_fontweight("bold")

    fig.subplots_adjust(left=0.095, right=0.985, top=0.88, bottom=0.25)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    fig.savefig(f"{OUTPUT_STEM}.png", dpi=300, bbox_inches="tight", facecolor="white")
    fig.savefig(f"{OUTPUT_STEM}.pdf", bbox_inches="tight", facecolor="white")
    fig.savefig(f"{OUTPUT_STEM}.svg", bbox_inches="tight", facecolor="white")


if __name__ == "__main__":
    main()
