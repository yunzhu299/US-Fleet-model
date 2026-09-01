#!/usr/bin/env python3
"""Two publication-style alternatives for policy/capacity recycling trends."""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
MAIN = ROOT / "Outputs" / "Recycling_Plots_main"
MAIN_NEW = ROOT / "Outputs" / "Recycling_Plots_main_new"
OUT = MAIN / "Scenario_Comparison"
OUT.mkdir(parents=True, exist_ok=True)

POLICIES = {"ACCII": "Baseline Policy", "Repeal": "Rollback Policy"}
CAPS = {
    "Increasing Batt Cap - Benchmark Chemistry": ("Increase LIB Capacity", "-", "#439BC0"),
    "Decreasing Batt Cap - Benchmark Chemistry": ("Decrease LIB Capacity", (0, (5, 3)), "#D95F59"),
}
METRICS = {
    "LIB Demand": "#439BC0",
    "Pack Mfg.": "#E3AD17",
    "Cell Mfg.": "#EFD58A",
    "EoL batteries": "#A9C84F",
    "Mfg. Scrap": "#DDEFA1",
    "Preprocessing capacity": "#3F4D49",
    "Materials recovery capacity": "#9F998C",
}
STEP_INFO = {
    "Black Mass": ("Preprocessing (black mass prod.)", "#3F4D49"),
    "Refining": ("Materials recovery (refining)", "#9F998C"),
}


def load_policy(folder: str) -> tuple[pd.DataFrame, pd.DataFrame]:
    overtime = pd.read_csv(MAIN / folder / "_parity" / "NA_overtime_data.csv")
    scrap = pd.read_csv(MAIN_NEW / folder / "_parity" / "NA_manu.csv")
    needed = pd.read_csv(MAIN / folder / "_parity" / "needed_cap_long.csv")

    metric_map = {
        "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)": ("LIB Demand", "Increase LIB Capacity"),
        "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)": ("LIB Demand", "Decrease LIB Capacity"),
        "Pack Manufacturing": ("Pack Mfg.", "Increase LIB Capacity"),
        "Decreasing Batt Cap Pack Manufacturing": ("Pack Mfg.", "Decrease LIB Capacity"),
        "Cell Manufacturing": ("Cell Mfg.", "Increase LIB Capacity"),
        "Decreasing Batt Cap Cell Manufacturing": ("Cell Mfg.", "Decrease LIB Capacity"),
        "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)": ("EoL batteries", "Increase LIB Capacity"),
        "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)": ("EoL batteries", "Decrease LIB Capacity"),
        "Black Mass": ("Preprocessing capacity", "Installed"),
        "Refining": ("Materials recovery capacity", "Installed"),
    }
    overtime[["Display metric", "Capacity scenario"]] = overtime["Metric"].map(metric_map).apply(pd.Series)

    scrap_long = scrap.melt(
        id_vars="Year",
        value_vars=["Tonnes_Scrap_proj_mid", "Tonnes_Scrap_15_mid"],
        var_name="source",
        value_name="Tonnes",
    )
    scrap_long["Tonnes"] /= 1e6
    scrap_long["Display metric"] = "Mfg. Scrap"
    scrap_long["Capacity scenario"] = scrap_long["source"].map(
        {
            "Tonnes_Scrap_proj_mid": "Increase LIB Capacity",
            "Tonnes_Scrap_15_mid": "Decrease LIB Capacity",
        }
    )
    overtime = pd.concat(
        [
            overtime[["Year", "Tonnes", "Display metric", "Capacity scenario"]],
            scrap_long[["Year", "Tonnes", "Display metric", "Capacity scenario"]],
        ],
        ignore_index=True,
    )

    needed = needed[needed["Scenario"].isin(CAPS)].copy()
    needed["Capacity scenario"] = needed["Scenario"].map(
        {key: value[0] for key, value in CAPS.items()}
    )
    needed = needed.sort_values(["Capacity scenario", "Recycling Step", "Year"])
    needed["Annual deficit"] = (
        needed.groupby(["Capacity scenario", "Recycling Step"])["Tonne"]
        .diff()
        .fillna(needed["Tonne"])
        .clip(lower=0)
    )
    return overtime, needed


DATA = {folder: load_policy(folder) for folder in POLICIES}

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 10,
        "axes.titlesize": 13,
        "axes.labelsize": 12,
        "xtick.labelsize": 10,
        "ytick.labelsize": 10,
        "legend.fontsize": 10,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)


def style_axis(ax, show_xlabel=False):
    ax.set_xlim(2025, 2050)
    ax.set_xticks(range(2025, 2051, 5))
    ax.set_ylim(bottom=0)
    ax.grid(axis="y", color="#D9D9D9", linewidth=0.65)
    ax.grid(axis="x", color="#EEEEEE", linewidth=0.5)
    ax.spines[["top", "right"]].set_visible(False)
    if show_xlabel:
        ax.set_xlabel("Year", fontweight="bold")


# ---------------------------------------------------------------------
# Alternative A: complete overtime trends above annual deficit areas.
# ---------------------------------------------------------------------
fig, axes = plt.subplots(
    2, 2, figsize=(11.4, 8.4), sharex=True,
    gridspec_kw={"height_ratios": [1.35, 1], "hspace": 0.16, "wspace": 0.12},
)

for col, (folder, policy) in enumerate(POLICIES.items()):
    overtime, needed = DATA[folder]
    top, bottom = axes[0, col], axes[1, col]
    top.set_title(policy, fontweight="bold", fontsize=15, pad=9)

    for metric, color in METRICS.items():
        subset = overtime[overtime["Display metric"] == metric]
        if metric.endswith("capacity"):
            series = subset.sort_values("Year")
            top.plot(series["Year"], series["Tonnes"], color=color, lw=2.0, zorder=4)
        else:
            for cap_label, linestyle, _ in CAPS.values():
                series = subset[subset["Capacity scenario"] == cap_label].sort_values("Year")
                top.plot(
                    series["Year"], series["Tonnes"], color=color, linestyle=linestyle,
                    lw=1.8, zorder=3,
                )

    for scenario, linestyle, _ in CAPS.values():
        for step, (step_label, color) in STEP_INFO.items():
            series = needed[
                (needed["Capacity scenario"] == scenario)
                & (needed["Recycling Step"] == step)
            ].sort_values("Year")
            bottom.fill_between(
                series["Year"], 0, series["Annual deficit"],
                color=color, alpha=0.10 if scenario.startswith("Increase") else 0.045,
                zorder=1,
            )
            bottom.plot(
                series["Year"], series["Annual deficit"], color=color,
                linestyle=linestyle, lw=2.0, zorder=3,
            )

    style_axis(top)
    style_axis(bottom, show_xlabel=True)
    if col == 0:
        top.set_ylabel("Quantities (million metric tonnes)", fontweight="bold")
        bottom.set_ylabel("Annual deficit\n(million metric tonnes/year)", fontweight="bold")
    top.text(0.01, 0.96, "a" if col == 0 else "b", transform=top.transAxes,
             va="top", fontweight="bold", fontsize=13)
    bottom.text(0.01, 0.96, "c" if col == 0 else "d", transform=bottom.transAxes,
                va="top", fontweight="bold", fontsize=13)

fig.suptitle(
    "North American Battery Mass Flows and Recycling Capacity Deficits",
    fontsize=18, fontweight="bold", y=0.985,
)

cap_handles = [
    Line2D([0], [0], color="#444444", lw=2.2, linestyle=ls, label=label)
    for label, ls, _ in CAPS.values()
]
flow_handles = [
    Line2D([0], [0], color=METRICS[label], lw=2.5, label=label)
    for label in ["LIB Demand", "Pack Mfg.", "Cell Mfg.", "EoL batteries", "Mfg. Scrap"]
]
recycle_handles = [
    Line2D([0], [0], color=METRICS[label], lw=2.5, label=label)
    for label in ["Preprocessing capacity", "Materials recovery capacity"]
]
leg1 = fig.legend(cap_handles, [h.get_label() for h in cap_handles], title="Capacity scenario",
                  loc="lower center", bbox_to_anchor=(0.18, 0.012), frameon=False, ncol=1)
leg2 = fig.legend(flow_handles, [h.get_label() for h in flow_handles], title="Battery mass flows",
                  loc="lower center", bbox_to_anchor=(0.49, 0.012), frameon=False, ncol=2)
leg3 = fig.legend(recycle_handles, [h.get_label() for h in recycle_handles],
                  title="Recycling capacity / deficit", loc="lower center",
                  bbox_to_anchor=(0.82, 0.012), frameon=False, ncol=1)
for legend in (leg1, leg2, leg3):
    legend.get_title().set_fontweight("bold")

fig.subplots_adjust(left=0.085, right=0.985, top=0.91, bottom=0.20)
base = OUT / "Figure4A_Overtime_and_Annual_Deficit_Panels"
for ext in ("png", "pdf", "svg"):
    fig.savefig(base.with_suffix(f".{ext}"), dpi=400 if ext == "png" else None,
                bbox_inches="tight", facecolor="white")
plt.close(fig)


# ---------------------------------------------------------------------
# Alternative B: required throughput vs installed capacity, shaded gap.
# ---------------------------------------------------------------------
fig, axes = plt.subplots(
    2, 2, figsize=(11.2, 7.6), sharex=True, sharey="row",
    gridspec_kw={"hspace": 0.20, "wspace": 0.12},
)

for col, (folder, policy) in enumerate(POLICIES.items()):
    overtime, needed = DATA[folder]
    for row, (step, (step_label, installed_color)) in enumerate(STEP_INFO.items()):
        ax = axes[row, col]
        installed_metric = (
            "Preprocessing capacity" if step == "Black Mass"
            else "Materials recovery capacity"
        )
        installed = (
            overtime[overtime["Display metric"] == installed_metric]
            .sort_values("Year")
            .set_index("Year")["Tonnes"]
        )
        ax.plot(installed.index, installed, color=installed_color, lw=2.4, zorder=5)

        for scenario, _, scenario_color in CAPS.values():
            deficit = (
                needed[
                    (needed["Capacity scenario"] == scenario)
                    & (needed["Recycling Step"] == step)
                ]
                .sort_values("Year")
                .set_index("Year")["Annual deficit"]
            )
            required = installed.add(deficit, fill_value=0)
            ax.fill_between(
                required.index, installed.values, required.values,
                color=scenario_color, alpha=0.20, zorder=2,
            )
            ax.plot(required.index, required, color=scenario_color, lw=2.0, zorder=4)

        style_axis(ax, show_xlabel=row == 1)
        if row == 0:
            ax.set_title(policy, fontweight="bold", fontsize=15, pad=9)
        if col == 0:
            ax.set_ylabel(f"{step_label}\n(million metric tonnes/year)", fontweight="bold")

fig.suptitle(
    "Required and Installed Recycling Throughput Until 2050",
    fontsize=18, fontweight="bold", y=0.985,
)

shade_handles = [
    Patch(facecolor=color, edgecolor=color, alpha=0.25, label=f"{label} deficit")
    for label, _, color in CAPS.values()
]
installed_handles = [
    Line2D([0], [0], color=color, lw=2.7, label=label)
    for _, (label, color) in STEP_INFO.items()
]
required_handles = [
    Line2D([0], [0], color=color, lw=2.2, label=f"{label} required throughput")
    for label, _, color in CAPS.values()
]
leg1 = fig.legend(required_handles + shade_handles,
                  [h.get_label() for h in required_handles + shade_handles],
                  title="Capacity scenario", loc="lower center",
                  bbox_to_anchor=(0.35, 0.01), frameon=False, ncol=2)
leg2 = fig.legend(installed_handles, [h.get_label() for h in installed_handles],
                  title="Installed recycling capacity", loc="lower center",
                  bbox_to_anchor=(0.76, 0.01), frameon=False, ncol=1)
for legend in (leg1, leg2):
    legend.get_title().set_fontweight("bold")

fig.subplots_adjust(left=0.12, right=0.985, top=0.90, bottom=0.20)
base = OUT / "Figure4B_Recycling_Deficit_Shaded_Gaps"
for ext in ("png", "pdf", "svg"):
    fig.savefig(base.with_suffix(f".{ext}"), dpi=400 if ext == "png" else None,
                bbox_inches="tight", facecolor="white")
plt.close(fig)

print(OUT / "Figure4A_Overtime_and_Annual_Deficit_Panels.png")
print(OUT / "Figure4B_Recycling_Deficit_Shaded_Gaps.png")
