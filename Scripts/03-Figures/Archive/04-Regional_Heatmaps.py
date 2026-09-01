#!/usr/bin/env python3
"""Create two publication-style regional battery supply-chain heatmaps."""

from pathlib import Path

import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns


ROOT = Path(__file__).resolve().parents[2]
INPUT = (
    ROOT
    / "Outputs"
    / "Recycling_Plots_main"
    / "ACCII"
    / "_parity"
    / "Regions_Mass_2050_projected_ref.csv"
)
OUTPUT_DIR = ROOT / "Outputs" / "Recycling_Plots_main" / "ACCII"
OUTPUT_STEM = OUTPUT_DIR / "Regional_Supply_Chain_Heatmaps_2050_ACCII"

REGIONS = [
    "US-West",
    "US-Mountain",
    "US-Midwest",
    "US-South",
    "US-East",
    "Canada-West",
    "Canada-Mountain",
    "Canada-Midwest",
    "Canada-East",
    "Mexico",
]

PANELS = [
    (
        "a   Demand and manufacturing",
        ["LIB Demand", "Pack Manufacturing", "Cell Manufacturing"],
        ["LIB demand", "Pack manufacturing", "Cell manufacturing"],
    ),
    (
        "b   End-of-life supply and recycling capacity",
        ["End of Life Batteries", "Black Mass", "Refining"],
        ["End-of-life batteries", "Black-mass capacity", "Refining capacity"],
    ),
]


def format_value(value: float) -> str:
    if value == 0:
        return "0"
    if value < 0.01:
        return "<0.01"
    return f"{value:.2f}"


def main() -> None:
    data = pd.read_csv(INPUT)
    data = data.dropna(subset=["Region"])
    regional = (
        data.groupby(["Region", "Origin"], as_index=False)["Metric Tonnes (millions)"]
        .sum()
    )

    matrices = []
    for _, columns, _ in PANELS:
        matrix = (
            regional.pivot(index="Region", columns="Origin", values="Metric Tonnes (millions)")
            .reindex(index=REGIONS, columns=columns)
            .fillna(0)
        )
        matrices.append(matrix)

    vmax = max(matrix.to_numpy().max() for matrix in matrices)
    norm = mcolors.SymLogNorm(linthresh=0.05, linscale=0.7, vmin=0, vmax=vmax)
    cmap = plt.get_cmap("cividis")

    sns.set_theme(context="paper", style="white", font="DejaVu Sans")
    plt.rcParams.update(
        {
            "font.size": 8,
            "axes.titlesize": 10,
            "axes.titleweight": "bold",
            "figure.titlesize": 12,
            "figure.titleweight": "bold",
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
            "axes.unicode_minus": False,
        }
    )

    fig = plt.figure(figsize=(7.2, 6.6), constrained_layout=True)
    grid = fig.add_gridspec(2, 2, width_ratios=[1, 0.035], hspace=0.04, wspace=0.04)
    axes = [fig.add_subplot(grid[row, 0]) for row in range(2)]
    colorbar_axis = fig.add_subplot(grid[:, 1])

    for panel_index, (axis, matrix, panel) in enumerate(zip(axes, matrices, PANELS)):
        title, _, labels = panel
        sns.heatmap(
            matrix,
            ax=axis,
            cmap=cmap,
            norm=norm,
            cbar=panel_index == 0,
            cbar_ax=colorbar_axis if panel_index == 0 else None,
            linewidths=0.8,
            linecolor="white",
            annot=False,
            xticklabels=labels,
            yticklabels=REGIONS,
        )

        for row_index, region in enumerate(REGIONS):
            for column_index, origin in enumerate(matrix.columns):
                value = matrix.loc[region, origin]
                rgba = cmap(norm(value))
                luminance = 0.2126 * rgba[0] + 0.7152 * rgba[1] + 0.0722 * rgba[2]
                axis.text(
                    column_index + 0.5,
                    row_index + 0.5,
                    format_value(value),
                    ha="center",
                    va="center",
                    color="black" if luminance > 0.56 else "white",
                    fontsize=7.5,
                    fontweight="bold" if value >= 1 else "normal",
                )

        axis.set_title(title, loc="left", pad=7)
        axis.set_xlabel("")
        axis.set_ylabel("")
        axis.tick_params(axis="x", labelrotation=0, length=0, pad=4)
        axis.tick_params(axis="y", labelrotation=0, length=0, pad=4)

        for separator in (5, 9):
            axis.axhline(separator, color="#333333", linewidth=1.2)

    ticks = [0, 0.05, 0.1, 0.25, 0.5, 1, 2, 5]
    colorbar_axis.set_yticks(ticks)
    colorbar_axis.set_yticklabels(["0", "0.05", "0.1", "0.25", "0.5", "1", "2", "5"])
    colorbar_axis.set_ylabel("Annual mass or capacity (million metric tonnes)", labelpad=8)
    colorbar_axis.tick_params(length=2, labelsize=7)

    fig.suptitle("Regional alignment of North American battery supply chains in 2050")
    fig.text(
        0.01,
        -0.055,
        "ACCII fleet scenario; increasing battery capacity and benchmark chemistry.\n"
        "Cells show annual battery mass or installed annual processing capacity. "
        "Color uses a shared symlog scale to preserve low-capacity differences; labels show absolute values.",
        ha="left",
        va="bottom",
        fontsize=6.5,
        color="#4d4d4d",
    )

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    fig.savefig(f"{OUTPUT_STEM}.png", dpi=300, bbox_inches="tight", facecolor="white")
    fig.savefig(f"{OUTPUT_STEM}.pdf", bbox_inches="tight", facecolor="white")
    fig.savefig(f"{OUTPUT_STEM}.svg", bbox_inches="tight", facecolor="white")

    grayscale = plt.get_cmap("Greys")
    for axis, matrix in zip(axes, matrices):
        axis.collections[0].set_cmap(grayscale)
    fig.savefig(f"{OUTPUT_STEM}_grayscale.png", dpi=200, bbox_inches="tight", facecolor="white")


if __name__ == "__main__":
    main()
