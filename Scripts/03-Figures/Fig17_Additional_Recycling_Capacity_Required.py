#!/usr/bin/env python3
"""Figure 17: additional recycling capacity required by geographic network."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.ticker import AutoMinorLocator, FuncFormatter, MaxNLocator
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "Results" / "Data" / "Fig17_Additional_Recycling_Capacity_Required.csv"
FIGURE_OUT = ROOT / "Results" / "Figures"
FIGURE_OUT.mkdir(parents=True, exist_ok=True)
YEARS = [2030, 2035, 2040, 2045, 2050]
GEOGRAPHIES = ["California", "United States", "North America"]
GEOGRAPHY_TITLES = {
    "California": "California\nCA-origin feedstock / US recycling network",
    "United States":
        "United States\nUS-origin feedstock / US recycling network",
    "North America":
        "North America\nNA-origin feedstock / NA recycling network",
}
STAGES = ["Preprocessing", "Materials recovery"]
STAGE_TITLES = {
    "Preprocessing": "a) Preprocessing (black mass production)",
    "Materials recovery": "b) Materials recovery (refining)",
}
SCENARIOS = [
    ("Baseline", "Increasing Batt Cap", "Benchmark Chemistry"),
    ("Rollback", "Increasing Batt Cap", "Benchmark Chemistry"),
    ("Baseline", "Increasing Batt Cap", "High LFP Chemistry"),
    ("Rollback", "Increasing Batt Cap", "High LFP Chemistry"),
    ("Baseline", "Decreasing Batt Cap", "Benchmark Chemistry"),
    ("Rollback", "Decreasing Batt Cap", "Benchmark Chemistry"),
    ("Baseline", "Decreasing Batt Cap", "High LFP Chemistry"),
    ("Rollback", "Decreasing Batt Cap", "High LFP Chemistry"),
]
SCENARIO_LABELS = [
    f"{'Increase' if capacity.startswith('Increasing') else 'Decrease'} · "
    f"{'Benchmark' if chemistry.startswith('Benchmark') else 'High-LFP'} · "
    f"{policy}"
    for policy, capacity, chemistry in SCENARIOS
]
YEAR_COLORS = {
    2030: "#B9D7E3",
    2035: "#7FB9CE",
    2040: "#4696B4",
    2045: "#16749B",
    2050: "#084C6C",
}
YEAR_MARKERS = {2030: "o", 2035: "s", 2040: "D", 2045: "^", 2050: "P"}
ROW_SPACING = 1.28
SCENARIO_Y = np.arange(len(SCENARIOS)) * ROW_SPACING
YEAR_OFFSETS = dict(zip(YEARS, np.linspace(-0.31, 0.31, len(YEARS))))


data = pd.read_csv(DATA)
value_column = "Additional capacity needed (thousand MT/year)"
data = data[data["Year"].isin(YEARS)].copy()

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 23,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

fig, axes = plt.subplots(
    2, 3, figsize=(33.0, 18.0),
    gridspec_kw={"hspace": 0.90, "wspace": 0.30},
)

for row, stage in enumerate(STAGES):
    for col, geography in enumerate(GEOGRAPHIES):
        ax = axes[row, col]
        panel = data[
            (data["Stage"] == stage) & (data["Geography"] == geography)
        ]

        for scenario_index, (policy, capacity, chemistry) in enumerate(SCENARIOS):
            scenario = panel[
                (panel["Policy"] == policy)
                & (panel["Capacity"] == capacity)
                & (panel["Chemistry"] == chemistry)
            ].set_index("Year")
            for year in YEARS:
                value = scenario[value_column].get(year, np.nan)
                ax.scatter(
                    value,
                    SCENARIO_Y[scenario_index] + YEAR_OFFSETS[year],
                    s=250,
                    marker=YEAR_MARKERS[year],
                    color=YEAR_COLORS[year],
                    edgecolor="#FFFFFF",
                    linewidth=0.95,
                    zorder=3,
                )

        for band in range(len(SCENARIOS)):
            if band % 2 == 0:
                ax.axhspan(
                    SCENARIO_Y[band] - ROW_SPACING / 2,
                    SCENARIO_Y[band] + ROW_SPACING / 2,
                    color="#F5F5F2", zorder=0,
                )
        ax.axhline(3.5 * ROW_SPACING, color="#8F918B", linewidth=1.15, zorder=1)
        ax.axhline(1.5 * ROW_SPACING, color="#A6A197", linewidth=0.8, zorder=1)
        ax.axhline(5.5 * ROW_SPACING, color="#A6A197", linewidth=0.8, zorder=1)
        ax.grid(axis="x", color="#D8D8D4", linewidth=0.75)
        ax.set_axisbelow(True)
        ax.set_ylim(SCENARIO_Y[-1] + ROW_SPACING * 0.64, -ROW_SPACING * 0.64)
        ax.set_yticks(SCENARIO_Y)
        ax.set_yticklabels([])
        if col == 0:
            for (policy, capacity, chemistry), y_position in zip(
                SCENARIOS, SCENARIO_Y
            ):
                capacity_label = (
                    "Increase"
                    if capacity.startswith("Increasing")
                    else "Decrease"
                )
                chemistry_label = (
                    "Benchmark"
                    if chemistry.startswith("Benchmark")
                    else "High-LFP"
                )
                ax.text(
                    -1.03, y_position, capacity_label,
                    transform=ax.get_yaxis_transform(),
                    ha="left", va="center",
                    fontsize=23, fontfamily="Arial",
                    clip_on=False,
                )
                ax.text(
                    -0.70, y_position, "·",
                    transform=ax.get_yaxis_transform(),
                    ha="center", va="center",
                    fontsize=23, fontfamily="Arial",
                    clip_on=False,
                )
                ax.text(
                    -0.64, y_position, chemistry_label,
                    transform=ax.get_yaxis_transform(),
                    ha="left", va="center",
                    fontsize=23, fontfamily="Arial",
                    clip_on=False,
                )
                ax.text(
                    -0.30, y_position, "·",
                    transform=ax.get_yaxis_transform(),
                    ha="center", va="center",
                    fontsize=23, fontfamily="Arial",
                    clip_on=False,
                )
                ax.text(
                    -0.24, y_position, policy,
                    transform=ax.get_yaxis_transform(),
                    ha="left", va="center",
                    fontsize=23, fontfamily="Arial",
                    clip_on=False,
                )
        ax.tick_params(axis="y", length=0)
        ax.tick_params(axis="x", which="major", labelsize=23, length=5)
        ax.tick_params(axis="x", which="minor", length=3)
        ax.xaxis.set_major_locator(MaxNLocator(nbins=4, integer=True))
        ax.xaxis.set_minor_locator(AutoMinorLocator(2))
        ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:,.0f}"))
        ax.grid(
            axis="x", which="minor", color="#E9E9E6",
            linewidth=0.55, linestyle=":",
        )
        ax.set_xlim(left=-panel[value_column].max() * 0.018)
        geography_title, network_subtitle = GEOGRAPHY_TITLES[
            geography
        ].split("\n", maxsplit=1)
        ax.set_title(
            geography_title,
            fontsize=29,
            fontweight="bold",
            pad=50,
        )
        ax.text(
            0.5,
            1.035,
            network_subtitle,
            transform=ax.transAxes,
            ha="center",
            va="bottom",
            fontsize=19.5,
            fontweight="bold",
            clip_on=False,
        )
        for spine in ("top", "right", "left"):
            ax.spines[spine].set_visible(False)
        ax.spines["bottom"].set_color("#555555")

fig.suptitle(
    "Cumulative Additional Recycling Capacity Required by Year",
    fontsize=39,
    fontweight="bold",
    y=0.985,
)
fig.text(
    0.018, 0.895, STAGE_TITLES["Preprocessing"],
    fontsize=34, fontweight="bold",
)
fig.text(
    0.018, 0.445, STAGE_TITLES["Materials recovery"],
    fontsize=34, fontweight="bold",
)
fig.supxlabel(
    "Cumulative additional capacity required by year "
    "(thousand metric tonnes/year)",
    fontsize=28,
    fontweight="bold",
    y=0.035,
)
legend_handles = [
    Line2D(
        [0], [0], marker=YEAR_MARKERS[year], linestyle="none",
        markerfacecolor=YEAR_COLORS[year], markeredgecolor="white",
        markeredgewidth=1.0, markersize=25, label=str(year),
    )
    for year in YEARS
]
legend = fig.legend(
    handles=legend_handles,
    title="Year",
    ncol=5,
    loc="center",
    bbox_to_anchor=(0.63, 0.505),
    frameon=False,
    fancybox=False,
    columnspacing=1.6,
    handletextpad=0.5,
    fontsize=25,
    title_fontsize=26,
)
legend.get_title().set_fontweight("bold")
fig.subplots_adjust(left=0.34, right=0.99, top=0.82, bottom=0.17)

stems = [FIGURE_OUT / "Fig17_Additional_Recycling_Capacity_Required"]
for stem in stems:
    fig.savefig(
        f"{stem}.png",
        dpi=400,
        bbox_inches="tight",
        facecolor="white",
    )
plt.close(fig)
print(f"{stems[0]}.png")
