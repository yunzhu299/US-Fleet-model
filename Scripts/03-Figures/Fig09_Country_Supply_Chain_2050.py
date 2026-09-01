#!/usr/bin/env python3
"""Figure 9: country-level North American LIB supply-chain quantities in 2050."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.colors import to_rgb
from matplotlib.patches import Patch, Rectangle
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
DATA_OUT = ROOT / "Results" / "Data"
OUTPUT_DIR = ROOT / "Results" / "Figures"
DATA_OUT.mkdir(parents=True, exist_ok=True)
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
OUTPUT_STEM = OUTPUT_DIR / "Fig09_Country_Supply_Chain_2050"

FLOW_SEGMENTS = ["LIB demand", "Pack Mfg.", "Cell Mfg.", "EoL batteries", "Mfg. Scrap"]
FLOW_COLORS = ["#439BC0", "#E7BD3F", "#EFD36F", "#A9C84F", "#DDEFA1"]
CAPACITY_SEGMENTS = [
    "Preprocessing\n(black mass production)",
    "Materials recovery\n(refining)",
]
CAPACITY_COLORS = ["#3F4D49", "#9F998C"]
# Bottom-to-top order within each stacked bar: Mexico, US, Canada.
COUNTRIES = ["MX", "US", "CA"]
COUNTRY_HATCHES = ["...", "", "///"]
COUNTRY_LEGEND = [
    ("US", ""),
    ("Canada", "///"),
    ("Mexico", "..."),
]
HATCH_COLOR = "#777777"
CAPACITY_SCENARIOS = [
    ("Increasing LIB Capacity", "increasing"),
    ("Decreasing LIB Capacity", "decreasing"),
]


def read_policy_data(policy_key: str) -> pd.DataFrame:
    source = DATA_ROOT / policy_key / "_parity" / "Mass_2050_projected.csv"
    return pd.read_csv(source)


def country_values(data: pd.DataFrame, column: str) -> list[float]:
    return [
        data.loc[data["Country"] == country, column].sum() / 1e6
        for country in COUNTRIES
    ]


def lighten(color: str, amount: float = 0.64) -> tuple[float, float, float]:
    rgb = np.array(to_rgb(color))
    return tuple(rgb + (1 - rgb) * amount)


def flow_columns(capacity_key: str) -> list[str]:
    if capacity_key == "increasing":
        return [
            "Add_LIB_proj_tonnes",
            "Tonnes_Prod_proj_down",
            "Tonnes_Prod_proj_mid",
            "Recycle_Batt_Proj",
            "Tonnes_Scrap_proj_mid",
        ]
    return [
        "Add_LIB_15_tonnes",
        "Tonnes_Prod_15_down",
        "Tonnes_Prod_15_mid",
        "Recycle_Batt_15",
        "Tonnes_Scrap_15_mid",
    ]


def add_label(
    axis,
    x: float,
    value: float,
    offset: float,
    bold: bool = False,
    decimals: int = 1,
) -> None:
    axis.text(
        x,
        value + offset,
        f"{value:.{decimals}f}",
        ha="center",
        va="bottom",
        fontsize=6.8,
        fontweight="bold" if bold else "normal",
        zorder=6,
    )


def patterned_stack(
    axis,
    x: float,
    values: list[float],
    color: str,
    width: float,
    rollback: bool = False,
) -> float:
    bottom = 0.0
    facecolor = lighten(color) if rollback else color
    for value, hatch in zip(values, COUNTRY_HATCHES):
        axis.bar(
            x,
            value,
            width=width,
            bottom=bottom,
            facecolor=facecolor,
            edgecolor="black",
            linewidth=0.65,
            zorder=3,
        )
        if hatch:
            axis.bar(
                x,
                value,
                width=width,
                bottom=bottom,
                facecolor="none",
                edgecolor=HATCH_COLOR,
                linewidth=0.01,
                hatch=hatch,
                zorder=4,
            )
        bottom += value
    axis.add_patch(
        Rectangle(
            (x - width / 2, 0),
            width,
            bottom,
            fill=False,
            edgecolor="#444444",
            linewidth=0.8,
            zorder=5,
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

    source_rows = []
    policy_frames = {
        "Policy Baseline": baseline,
        "Policy Rollback": rollback,
    }
    for capacity_title, capacity_key in CAPACITY_SCENARIOS:
        for policy_label, frame in policy_frames.items():
            for segment, column in zip(FLOW_SEGMENTS, flow_columns(capacity_key)):
                for country, value in zip(COUNTRIES, country_values(frame, column)):
                    source_rows.append(
                        {
                            "Panel": capacity_title,
                            "Policy scenario": policy_label,
                            "Country": country,
                            "Metric": segment,
                            "Million metric tonnes": value,
                        }
                    )
    for segment, column in zip(
        CAPACITY_SEGMENTS,
        ["Cumulative_black_mass_cap", "Cumulative_refining_cap"],
    ):
        for country, value in zip(COUNTRIES, country_values(baseline, column)):
            source_rows.append(
                {
                    "Panel": "Installed recycling capacity",
                    "Policy scenario": "Policy Baseline",
                    "Country": country,
                    "Metric": segment.replace("\n", " "),
                    "Million metric tonnes": value,
                }
            )
    pd.DataFrame(source_rows).to_csv(
        DATA_OUT / "Fig09_Country_Supply_Chain_2050.csv", index=False
    )

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
            "hatch.linewidth": 0.75,
        }
    )

    fig = plt.figure(figsize=(8.4, 5.7), constrained_layout=True)
    grid = fig.add_gridspec(2, 2, height_ratios=[3.0, 1.2], hspace=0.02, wspace=0.08)
    flow_axes = [fig.add_subplot(grid[0, column]) for column in range(2)]
    recycling_axis = fig.add_subplot(grid[1, :])
    flow_x = np.array([0.0, 1.45, 2.55, 3.90, 5.30])
    width = 0.34

    for panel_index, ((title, capacity_key), axis) in enumerate(
        zip(CAPACITY_SCENARIOS, flow_axes)
    ):
        for index, (column, color) in enumerate(zip(flow_columns(capacity_key), FLOW_COLORS)):
            baseline_x = flow_x[index] - width / 2
            rollback_x = flow_x[index] + width / 2
            baseline_total = patterned_stack(
                axis, baseline_x, country_values(baseline, column), color, width
            )
            rollback_total = patterned_stack(
                axis, rollback_x, country_values(rollback, column), color, width, True
            )
            label_decimals = 2 if index == 4 else 1
            label_x_offset = 0.075 if index == 4 else 0.035
            add_label(
                axis,
                baseline_x - label_x_offset,
                baseline_total,
                0.35,
                True,
                label_decimals,
            )
            add_label(
                axis,
                rollback_x + label_x_offset,
                rollback_total,
                0.35,
                decimals=label_decimals,
            )

        axis.set_title(f"{chr(97 + panel_index)}   {title}", loc="left", pad=7)
        axis.set_ylim(0, 17.5)
        axis.set_xticks(flow_x)
        axis.set_xticklabels(FLOW_SEGMENTS)
        style_axis(axis)
        axis.text(
            flow_x[0], -0.11, "DEMAND",
            transform=axis.get_xaxis_transform(), ha="center", va="top",
            fontsize=6, fontweight="bold", color=FLOW_COLORS[0],
        )
        axis.text(
            np.mean(flow_x[1:3]), -0.11, "MANUFACTURING",
            transform=axis.get_xaxis_transform(), ha="center", va="top",
            fontsize=6, fontweight="bold", color=FLOW_COLORS[1],
        )
        axis.text(
            np.mean(flow_x[3:]), -0.105, "RECYCLING\nFEEDSTOCK",
            transform=axis.get_xaxis_transform(), ha="center", va="top",
            fontsize=5.8, linespacing=1.18, fontweight="bold", color=FLOW_COLORS[3],
        )

    flow_axes[0].set_ylabel("Annual battery mass flow\n(million metric tonnes)")

    capacity_columns = ["Cumulative_black_mass_cap", "Cumulative_refining_cap"]
    recycling_x = np.array([-0.28, 0.28])
    for x_value, column, color in zip(recycling_x, capacity_columns, CAPACITY_COLORS):
        value = patterned_stack(
            recycling_axis,
            x_value,
            country_values(baseline, column),
            color,
            0.15,
        )
        add_label(recycling_axis, x_value, value, 0.04, True)

    recycling_axis.set_xlim(-1.0, 1.0)
    recycling_axis.set_ylim(0, 1.65)
    recycling_axis.set_xticks(recycling_x)
    recycling_axis.set_xticklabels(CAPACITY_SEGMENTS)
    recycling_axis.set_title("Installed recycling capacity", loc="left", fontsize=8, pad=4)
    recycling_axis.set_ylabel("Processing capacity\n(million metric tonnes/year)")
    style_axis(recycling_axis)

    policy_handles = [
        Patch(facecolor="#777777", edgecolor="#333333", label="Baseline Policy"),
        Patch(facecolor="#cfcfcf", edgecolor="#333333", label="Rollback Policy"),
    ]
    country_handles = [
        Patch(
            facecolor="white",
            edgecolor="black" if not hatch else HATCH_COLOR,
            linewidth=0.8,
            hatch=hatch,
            label=label,
        )
        for label, hatch in COUNTRY_LEGEND
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
        handlelength=2.0,
        handleheight=1.25,
        columnspacing=1.1,
    )
    country_legend.get_title().set_fontweight("bold")

    fig.suptitle(
        "North American Demand, Manufacturing and Recycling Quantities in 2050",
        fontsize=11,
        fontweight="bold",
    )

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    fig.savefig(f"{OUTPUT_STEM}.png", dpi=300, bbox_inches="tight", facecolor="white")


if __name__ == "__main__":
    main()
