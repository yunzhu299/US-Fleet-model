#!/usr/bin/env python3
"""Figure 2: 2050 policy effects under two LIB-capacity scenarios."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.colors import to_rgb
from matplotlib.patches import Patch, Rectangle
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
OUTPUT_DIR = DATA_ROOT / "Scenario_Comparison"
OUTPUT_STEM = OUTPUT_DIR / "Figure2_2050_Policy_Capacity_Country"

FLOW_SEGMENTS = ["LIB demand", "Pack Mfg.", "Cell Mfg.", "EoL batteries"]
CAPACITY_SEGMENTS = ["Black mass", "Refining"]
COUNTRIES = ["US", "CA", "MX"]
COUNTRY_LABELS = ["US", "Canada", "Mexico"]
COUNTRY_COLORS = ["#3D5A80", "#8E6C88", "#D9A441"]
GROUP_LABEL_COLOR = "#444444"

CAPACITY_SCENARIOS = [
    ("Increasing LIB Capacity", "increasing"),
    ("Decreasing LIB Capacity", "decreasing"),
]


def read_policy_data(policy_key: str) -> pd.DataFrame:
    source = DATA_ROOT / policy_key / "_parity" / "Mass_2050_projected.csv"
    return pd.read_csv(source)


def total(data: pd.DataFrame, column: str) -> float:
    return data[column].sum() / 1e6


def country_values(data: pd.DataFrame, column: str) -> list[float]:
    return [
        data.loc[data["Country"] == country, column].sum() / 1e6
        for country in COUNTRIES
    ]


def lighten(color: str, amount: float = 0.52) -> tuple[float, float, float]:
    rgb = np.array(to_rgb(color))
    return tuple(rgb + (1 - rgb) * amount)


def flow_columns(capacity_key: str) -> list[str]:
    if capacity_key == "increasing":
        return [
            "Add_LIB_proj_tonnes",
            "Tonnes_Prod_proj_down",
            "Tonnes_Prod_proj_mid",
            "Recycle_Batt_Proj",
        ]
    return [
        "Add_LIB_15_tonnes",
        "Tonnes_Prod_15_down",
        "Tonnes_Prod_15_mid",
        "Recycle_Batt_15",
    ]


def add_total_label(axis, x: float, value: float, offset: float, bold: bool = False) -> None:
    axis.text(
        x,
        value + offset,
        f"{value:.1f}",
        ha="center",
        va="bottom",
        fontsize=6.8,
        fontweight="bold" if bold else "normal",
        zorder=6,
    )


def stacked_bar(
    axis,
    x: float,
    values: list[float],
    width: float,
    rollback: bool = False,
) -> float:
    bottom = 0.0
    for value, country_color in zip(values, COUNTRY_COLORS):
        axis.bar(
            x,
            value,
            width=width,
            bottom=bottom,
            color=lighten(country_color) if rollback else country_color,
            edgecolor="white",
            linewidth=0.75,
            zorder=3,
        )
        bottom += value
    axis.add_patch(
        Rectangle(
            (x - width / 2, 0),
            width,
            bottom,
            fill=False,
            edgecolor="#333333",
            linewidth=1.0 if rollback else 0.75,
            hatch="////" if rollback else None,
            zorder=4,
        )
    )
    return bottom


def style_axis(axis) -> None:
    axis.grid(axis="y", color="#dddddd", linewidth=0.7, zorder=0)
    axis.spines[["top", "right"]].set_visible(False)
    axis.tick_params(axis="x", length=0)


def main() -> None:
    baseline = read_policy_data("ACCII")
    rollback = read_policy_data("Repeal")

    plt.rcParams.update(
        {
            "font.family": "DejaVu Sans",
            "font.size": 8,
            "axes.titlesize": 9,
            "axes.titleweight": "bold",
            "axes.labelsize": 8,
            "xtick.labelsize": 7,
            "ytick.labelsize": 7,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
            "axes.unicode_minus": False,
        }
    )

    fig = plt.figure(figsize=(7.2, 5.7), constrained_layout=True)
    grid = fig.add_gridspec(2, 2, height_ratios=[3.0, 1.2], hspace=0.10, wspace=0.08)
    flow_axes = [fig.add_subplot(grid[0, column]) for column in range(2)]
    recycling_axis = fig.add_subplot(grid[1, :])

    flow_x = np.array([0.0, 1.45, 2.45, 3.90])
    width = 0.34

    for panel_index, ((title, capacity_key), axis) in enumerate(
        zip(CAPACITY_SCENARIOS, flow_axes)
    ):
        columns = flow_columns(capacity_key)
        for index, column in enumerate(columns):
            baseline_x = flow_x[index] - width / 2
            rollback_x = flow_x[index] + width / 2
            baseline_total = stacked_bar(
                axis,
                baseline_x,
                country_values(baseline, column),
                width,
            )
            rollback_total = stacked_bar(
                axis,
                rollback_x,
                country_values(rollback, column),
                width,
                rollback=True,
            )
            add_total_label(axis, baseline_x - 0.035, baseline_total, 0.35, bold=True)
            add_total_label(axis, rollback_x + 0.035, rollback_total, 0.35)
        axis.set_title(f"{chr(97 + panel_index)}   {title}", loc="left", pad=7)
        axis.set_ylim(0, 17.5)
        axis.set_xticks(flow_x)
        axis.set_xticklabels(FLOW_SEGMENTS)
        style_axis(axis)

        axis.text(
            flow_x[0],
            -0.18,
            "DEMAND",
            transform=axis.get_xaxis_transform(),
            ha="center",
            va="top",
            fontsize=6,
            fontweight="bold",
            color=GROUP_LABEL_COLOR,
        )
        axis.text(
            np.mean(flow_x[1:3]),
            -0.18,
            "MANUFACTURING",
            transform=axis.get_xaxis_transform(),
            ha="center",
            va="top",
            fontsize=6,
            fontweight="bold",
            color=GROUP_LABEL_COLOR,
        )
        axis.text(
            flow_x[3],
            -0.18,
            "RECYCLING FEEDSTOCK",
            transform=axis.get_xaxis_transform(),
            ha="center",
            va="top",
            fontsize=6,
            fontweight="bold",
            color=GROUP_LABEL_COLOR,
        )

    flow_axes[0].set_ylabel("Annual battery mass flow\n(million metric tonnes)")

    capacity_columns = ["Cumulative_black_mass_cap", "Cumulative_refining_cap"]
    recycling_x = np.array([-0.28, 0.28])
    for x_value, column in zip(recycling_x, capacity_columns):
        value = stacked_bar(
            recycling_axis,
            x_value,
            country_values(baseline, column),
            0.30,
        )
        add_total_label(recycling_axis, x_value, value, 0.04, bold=True)
    recycling_axis.set_xlim(-1.0, 1.0)
    recycling_axis.set_ylim(0, 1.65)
    recycling_axis.set_xticks(recycling_x)
    recycling_axis.set_xticklabels(CAPACITY_SEGMENTS)
    recycling_axis.set_title("Installed recycling capacity", loc="left", fontsize=8, pad=4)
    recycling_axis.set_ylabel("Processing capacity\n(million metric tonnes/year)")
    style_axis(recycling_axis)

    policy_handles = [
        Patch(facecolor="#777777", edgecolor="black", label="Baseline Policy"),
        Patch(
            facecolor="#d9d9d9",
            edgecolor="#777777",
            hatch="////",
            label="Rollback Policy",
        ),
    ]
    country_handles = [
        Patch(facecolor=color, edgecolor="white", label=label)
        for color, label in zip(COUNTRY_COLORS, COUNTRY_LABELS)
    ]
    policy_legend = fig.legend(
        handles=policy_handles,
        title="Policy Scenario",
        loc="lower center",
        bbox_to_anchor=(0.32, -0.105),
        ncol=2,
        frameon=False,
        fontsize=7.5,
        handlelength=1.7,
    )
    policy_legend.get_title().set_fontweight("bold")
    country_legend = fig.legend(
        handles=country_handles,
        title="Country",
        loc="lower center",
        bbox_to_anchor=(0.73, -0.105),
        ncol=3,
        frameon=False,
        fontsize=7.5,
        handlelength=1.7,
    )
    country_legend.get_title().set_fontweight("bold")
    fig.suptitle(
        "North American Demand, Manufacturing and Recycling Quantities in 2050",
        fontsize=11,
        fontweight="bold",
    )

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    fig.savefig(f"{OUTPUT_STEM}.png", dpi=300, bbox_inches="tight", facecolor="white")
    fig.savefig(f"{OUTPUT_STEM}.pdf", bbox_inches="tight", facecolor="white")
    fig.savefig(f"{OUTPUT_STEM}.svg", bbox_inches="tight", facecolor="white")


if __name__ == "__main__":
    main()
