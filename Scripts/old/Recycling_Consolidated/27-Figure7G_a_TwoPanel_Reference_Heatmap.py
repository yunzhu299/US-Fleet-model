#!/usr/bin/env python3
"""Two-panel Figure 7g(a)-style RCS reference heatmap.

This compact export matches the manuscript-style reference panel with:
California-origin feedstock / US recycling network and
US-origin feedstock / US recycling network.
"""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA = (
    ROOT
    / "Outputs"
    / "Recycling_Plots_main"
    / "Scenario_Comparison"
    / "Geographic_Boundary_RCS.csv"
)
OUT = ROOT / "Outputs" / "Recycling_Plots_main" / "Scenario_Comparison"
STEM = OUT / "Figure7G_a_RCS_Reference_Values_TwoPanel"

POLICY = "Baseline Policy"
BATTERY_CAPACITY = "Increasing Batt Cap"
CHEMISTRY = "Benchmark Chemistry"

REPORT_YEARS = [2030, 2035, 2040, 2045, 2050]
MINERALS = ["Nickel", "Manganese", "Cobalt", "Lithium", "Graphite", "Copper"]
MINERAL_LABELS = {
    "Nickel": "Ni",
    "Manganese": "Mn",
    "Cobalt": "Co",
    "Lithium": "Li",
    "Graphite": "Gr",
    "Copper": "Cu",
}
PANELS = [
    (
        "California",
        "CA-origin feedstock / US recycling network",
        "California-origin / US network",
    ),
    (
        "United States",
        "US-origin feedstock / US recycling network",
        "US-origin / US network",
    ),
]


def build_matrix(data: pd.DataFrame, scope: str) -> np.ndarray:
    scope_data = data[data["Scope"] == scope]
    matrix = np.full((len(MINERALS), len(REPORT_YEARS)), np.nan)
    for mineral_index, mineral in enumerate(MINERALS):
        values = scope_data[
            scope_data["Mineral"] == mineral
        ].set_index("Year")["Maximum RCS (%)"]
        matrix[mineral_index, :] = [values.get(year, np.nan) for year in REPORT_YEARS]
    return matrix


def save_figure(fig: plt.Figure) -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    for extension in ("png", "pdf", "svg"):
        fig.savefig(
            STEM.with_suffix(f".{extension}"),
            dpi=400 if extension == "png" else None,
            bbox_inches="tight",
            facecolor="white",
        )


def main() -> None:
    rcs = pd.read_csv(DATA)
    reference = rcs[
        (rcs["Policy"] == POLICY)
        & (rcs["Battery Capacity"] == BATTERY_CAPACITY)
        & (rcs["Chemistry"] == CHEMISTRY)
    ]
    matrices = [build_matrix(reference, scope) for _, _, scope in PANELS]
    vmax = np.ceil(max(np.nanmax(matrix) for matrix in matrices) / 5) * 5

    plt.rcParams.update(
        {
            "font.family": "Arial",
            "font.size": 8,
            "axes.titlesize": 9.5,
            "axes.titleweight": "bold",
            "xtick.labelsize": 8,
            "ytick.labelsize": 8,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
            "axes.unicode_minus": False,
        }
    )

    fig, axes = plt.subplots(
        1,
        2,
        figsize=(7.25, 3.55),
        sharey=False,
        gridspec_kw={"wspace": 0.09},
    )

    mesh = None
    cmap = plt.get_cmap("YlGnBu")
    for panel_index, (axis, (title, subtitle, _), matrix) in enumerate(
        zip(axes, PANELS, matrices)
    ):
        mesh = axis.imshow(
            matrix,
            cmap=cmap,
            vmin=0,
            vmax=vmax,
            origin="upper",
            aspect="auto",
        )
        axis.set_title(
            f"{title}\n{subtitle}",
            pad=5,
            linespacing=1.1,
            fontweight="bold",
        )
        axis.set_xticks(np.arange(len(REPORT_YEARS)))
        axis.set_xticklabels(REPORT_YEARS)
        axis.set_yticks(np.arange(len(MINERALS)))
        axis.set_yticklabels([MINERAL_LABELS[mineral] for mineral in MINERALS])
        if panel_index > 0:
            axis.tick_params(axis="y", labelleft=False)
        axis.set_xticks(np.arange(-0.5, len(REPORT_YEARS), 1), minor=True)
        axis.set_yticks(np.arange(-0.5, len(MINERALS), 1), minor=True)
        axis.grid(which="minor", color="white", linewidth=0.75)
        axis.tick_params(which="both", length=0)

        for row_index in range(matrix.shape[0]):
            for column_index in range(matrix.shape[1]):
                value = matrix[row_index, column_index]
                text_color = "white" if value > vmax * 0.56 else "#25302F"
                axis.text(
                    column_index,
                    row_index,
                    f"{value:.1f}",
                    ha="center",
                    va="center",
                    fontsize=7.5,
                    color=text_color,
                    fontweight="bold",
                )

    colorbar = fig.colorbar(
        mesh,
        ax=axes,
        orientation="horizontal",
        fraction=0.085,
        pad=0.18,
        aspect=45,
    )
    colorbar.set_label(
        "Maximum feasible recycled content (%)",
        fontsize=8.2,
        fontweight="bold",
    )
    colorbar.ax.tick_params(labelsize=7.5, length=2)

    fig.subplots_adjust(left=0.07, right=0.99, top=0.82, bottom=0.27)
    save_figure(fig)
    print(STEM.with_suffix(".png"))


if __name__ == "__main__":
    main()
