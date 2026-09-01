#!/usr/bin/env python3
"""Figures 15 and 16: unconstrained maximum RCS by feedstock/demand boundary.

Maximum RCS uses the no-recycling-restraint mineral field and therefore does
not apply a recycling-facility capacity constraint. Facility capacity is
evaluated separately in Figure 17.
"""

from pathlib import Path
import os

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from matplotlib.colors import TwoSlopeNorm
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[3]
MAIN_FIGURE = os.environ.get("RCS_MAIN_FIGURE", "both")
DATA_ROOT = ROOT / "Outputs" / "Recycling_Plots_main"
OUT = DATA_ROOT / "Scenario_Comparison"
OUT.mkdir(parents=True, exist_ok=True)
DATA_OUT = ROOT / "Results" / "Data"
FIGURE_OUT = ROOT / "Results" / "Figures"
DATA_OUT.mkdir(parents=True, exist_ok=True)
FIGURE_OUT.mkdir(parents=True, exist_ok=True)

US_CODES = set(
    "AL AK AZ AR CA CO CT DE DC FL GA HI ID IL IN IA KS KY LA ME MD MA "
    "MI MN MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX "
    "UT VT VA WA WV WI WY".split()
)
MINERALS = ["Nickel", "Manganese", "Cobalt", "Lithium", "Graphite", "Copper"]
REPORT_YEARS = [2030, 2035, 2040, 2045, 2050]
SCENARIOS = [
    "Increasing Batt Cap - Benchmark Chemistry",
    "Increasing Batt Cap - High LFP Chemistry",
    "Decreasing Batt Cap - Benchmark Chemistry",
    "Decreasing Batt Cap - High LFP Chemistry",
]
SCRAP_YIELD = 0.7078558
SCOPE_COLORS = {
    "California-origin / US network": "#0077A3",
    "US-origin / US network": "#9ACD32",
    "North America-origin / NA network": "#765A9A",
}
SCOPES = {
    "California-origin / US network": (
        lambda state: state == "CA",
        lambda state: state in US_CODES,
    ),
    "US-origin / US network": (
        lambda state: state in US_CODES,
        lambda state: state in US_CODES,
    ),
    "North America-origin / NA network": (
        lambda state: True,
        lambda state: True,
    ),
}


def processing_fractions(scrap_t, eol_t, black_t, refining_t):
    scrap_throughput = scrap_t / SCRAP_YIELD
    full_recycle = min(black_t, refining_t)
    leftover_full = max(full_recycle - scrap_throughput, 0)
    scrap_fraction = min(full_recycle / scrap_throughput, 1) if scrap_throughput > 0 else 1
    eol_fraction = min(leftover_full / eol_t, 1) if eol_t > 0 else 1
    return scrap_fraction, eol_fraction


def load_policy(folder):
    base = DATA_ROOT / folder / "_boundary"
    return {
        "components": pd.read_csv(base / "mineral_components_state.csv"),
        "demand": pd.read_csv(base / "mineral_demand_state.csv"),
        "eol": pd.read_csv(base / "eol_battery_mass_state.csv"),
        "scrap": pd.read_csv(base / "manufacturing_scrap_state.csv"),
        "capacity": pd.read_csv(base / "installed_capacity_state.csv"),
    }


def analyze_rcs_from_mainrunner(folder, policy_label):
    """Calculate RCS directly from Run_Main_New parity outputs.

    This preserves exact MainRunner values for the US and North America.
    The no-restraint mineral column already contains manufacturing scrap,
    EoL minerals, and mineral-specific recovery efficiencies, so it must not
    be reconstructed from the capacity-constrained component columns.
    """
    parity = DATA_ROOT / folder / "_parity"
    components = pd.read_csv(parity / "cap_chem_results.csv")
    demand_source = pd.read_csv(parity / "cap_chem_demand_results.csv")
    rows = []

    for scenario in SCENARIOS:
        battery_capacity, chemistry = scenario.split(" - ", maxsplit=1)
        scenario_components = components[components["Scenario"] == scenario]
        scenario_demand = demand_source[demand_source["Scenario"] == scenario]

        for scope, (origin_filter, _) in SCOPES.items():
            origin_components = scenario_components[
                scenario_components["State_Province"].astype(str).map(
                    origin_filter
                )
            ]
            available = origin_components.groupby(
                ["Year", "Mineral"]
            )["Available Recycled Minerals No R Restraint (Tonne)"].sum()

            origin_demand = scenario_demand[
                scenario_demand["State_Province"].astype(str).map(
                    origin_filter
                )
            ]
            demand = origin_demand.groupby(
                ["Year", "Mineral"]
            )["Demand Minerals (Tonne)"].sum()

            for report_year in REPORT_YEARS:
                availability_year = report_year - 1
                for mineral in MINERALS:
                    numerator = available.get((availability_year, mineral), 0)
                    denominator = demand.get((report_year, mineral), np.nan)
                    rows.append(
                        {
                            "Policy": policy_label,
                            "Battery Capacity": battery_capacity,
                            "Chemistry": chemistry,
                            "Scenario": scenario,
                            "Scope": scope,
                            "Year": report_year,
                            "Mineral": mineral,
                            "Maximum RCS (%)": (
                                numerator / denominator * 100
                                if denominator and not np.isnan(denominator)
                                else np.nan
                            ),
                        }
                    )
    return pd.DataFrame(rows)


def aggregate_by_year(frame, value, state_filter):
    subset = frame[frame["State_Province"].astype(str).map(state_filter)]
    return subset.groupby("Year")[value].sum()


def analyze_policy(folder, policy_label):
    source = load_policy(folder)
    comp = source["components"].copy()
    eol = source["eol"]
    scrap = source["scrap"]
    cap = source["capacity"]

    na_black = aggregate_by_year(cap, "Cumulative_black_mass_cap", lambda _: True)
    na_refine = aggregate_by_year(cap, "Cumulative_refining_cap", lambda _: True)
    # Recover unconstrained mineral components separately for every
    # capacity-chemistry scenario by reversing the original North-American
    # processing fractions used by the upstream model.
    potential_components = []
    for scenario in SCENARIOS:
        scenario_comp = comp[comp["Scenario"] == scenario].copy()
        scenario_eol = eol[eol["Scenario"] == scenario]
        scenario_scrap = scrap[scrap["Scenario"] == scenario]
        na_eol = aggregate_by_year(
            scenario_eol, "Batt_Mass_MT", lambda _: True
        )
        na_scrap = aggregate_by_year(
            scenario_scrap, "Tonnes_Scrap", lambda _: True
        )
        original_fractions = {}
        for year in sorted(scenario_comp["Year"].unique()):
            original_fractions[year] = processing_fractions(
                na_scrap.get(year, 0), na_eol.get(year, 0),
                na_black.get(year, 0), na_refine.get(year, 0),
            )
        scenario_comp["original_scrap_fraction"] = scenario_comp["Year"].map(
            lambda year: original_fractions[int(year)][0]
        )
        scenario_comp["original_eol_fraction"] = scenario_comp["Year"].map(
            lambda year: original_fractions[int(year)][1]
        )
        scenario_comp["Potential scrap mineral"] = np.where(
            scenario_comp["original_scrap_fraction"] > 0,
            scenario_comp["Scrap_min"]
            / scenario_comp["original_scrap_fraction"],
            0,
        )
        scenario_comp["Potential eol mineral"] = np.where(
            scenario_comp["original_eol_fraction"] > 0,
            scenario_comp["Batt_min"]
            / scenario_comp["original_eol_fraction"],
            0,
        )
        potential_components.append(scenario_comp)
    comp = pd.concat(potential_components, ignore_index=True)

    rcs_rows, gap_rows = [], []
    for scenario in SCENARIOS:
        battery_capacity, chemistry = scenario.split(" - ", maxsplit=1)
        scenario_eol = eol[eol["Scenario"] == scenario]
        scenario_scrap = scrap[scrap["Scenario"] == scenario]
        scenario_demand = source["demand"][
            source["demand"]["Scenario"] == scenario
        ]
        scenario_comp = comp[comp["Scenario"] == scenario]

        for scope, (origin_filter, network_filter) in SCOPES.items():
            origin_eol = aggregate_by_year(
                scenario_eol, "Batt_Mass_MT", origin_filter
            )
            origin_scrap = aggregate_by_year(
                scenario_scrap, "Tonnes_Scrap", origin_filter
            )
            network_black = aggregate_by_year(
                cap, "Cumulative_black_mass_cap", network_filter
            )
            network_refine = aggregate_by_year(
                cap, "Cumulative_refining_cap", network_filter
            )

            for year in range(2025, 2051):
                required = (
                    origin_eol.get(year, 0)
                    + origin_scrap.get(year, 0) / SCRAP_YIELD
                )
                common = {
                    "Policy": policy_label,
                    "Battery Capacity": battery_capacity,
                    "Chemistry": chemistry,
                    "Scenario": scenario,
                    "Scope": scope,
                    "Year": year,
                }
                gap_rows.extend(
                    [
                        {
                            **common,
                            "Stage": "Preprocessing (black mass production)",
                            "Annual capacity gap (MT/year)": max(
                                required - network_black.get(year, 0), 0
                            ),
                        },
                        {
                            **common,
                            "Stage": "Materials recovery (refining)",
                            "Annual capacity gap (MT/year)": max(
                                required
                                - min(
                                    network_black.get(year, 0),
                                    network_refine.get(year, 0),
                                ),
                                0,
                            ),
                        },
                    ]
                )

            origin_comp = scenario_comp[
                scenario_comp["State_Province"].astype(str).map(origin_filter)
            ].copy()
            # Maximum RCS follows the table's "all material recycled / no
            # facility delays" assumption. Facility constraints are reported
            # separately in the capacity-gap figure below.
            origin_comp["Available mineral"] = (
                origin_comp["Potential scrap mineral"]
                + origin_comp["Potential eol mineral"]
            )
            available = (
                origin_comp.groupby(["Year", "Mineral"])["Available mineral"]
                .sum()
            )

            demand = scenario_demand[
                scenario_demand["State_Province"].astype(str).map(origin_filter)
            ]
            demand = demand.groupby(
                ["Year", "Mineral"]
            )["Demand Minerals (Tonne)"].sum()

            for report_year in REPORT_YEARS:
                # Match the upstream RCS convention: recycling availability is
                # shifted forward one year before comparison with demand.
                availability_year = report_year - 1
                for mineral in MINERALS:
                    numerator = available.get((availability_year, mineral), 0)
                    denominator = demand.get((report_year, mineral), np.nan)
                    rcs_rows.append(
                        {
                            "Policy": policy_label,
                            "Battery Capacity": battery_capacity,
                            "Chemistry": chemistry,
                            "Scenario": scenario,
                            "Scope": scope,
                            "Year": report_year,
                            "Mineral": mineral,
                            "Maximum RCS (%)": (
                                numerator / denominator * 100
                                if denominator and not np.isnan(denominator)
                                else np.nan
                            ),
                        }
                    )

    return pd.DataFrame(rcs_rows), pd.DataFrame(gap_rows)


all_rcs = []
for folder, policy in {"ACCII": "Baseline Policy", "Repeal": "Rollback Policy"}.items():
    all_rcs.append(analyze_rcs_from_mainrunner(folder, policy))

rcs = pd.concat(all_rcs, ignore_index=True)
rcs.to_csv(OUT / "Geographic_Boundary_RCS.csv", index=False)

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 10.5,
        "axes.titlesize": 13,
        "axes.labelsize": 12,
        "xtick.labelsize": 10,
        "ytick.labelsize": 10,
        "legend.fontsize": 10.5,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)


def save_figure(fig, stem):
    stem = Path(stem)
    target = stem if stem.is_absolute() else OUT / stem
    target.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(
        target.parent / f"{target.name}.png",
        dpi=400,
        bbox_inches="tight",
        facecolor="white",
    )


# Four heatmaps keep all 24 combinations legible. Each figure fixes one
# capacity-chemistry combination and compares 2 policies x 3 boundaries.
scope_short = {
    "California-origin / US network": "CA origin / US network",
    "US-origin / US network": "US origin / US network",
    "North America-origin / NA network": "NA origin / NA network",
}
capacity_order = ["Increasing Batt Cap", "Decreasing Batt Cap"]
chemistry_order = ["Benchmark Chemistry", "High LFP Chemistry"]
policy_order = ["Baseline Policy", "Rollback Policy"]
heatmap_max = np.ceil(rcs["Maximum RCS (%)"].max() / 5) * 5
cmap = plt.get_cmap("YlGnBu")
heatmap_outputs = []
for capacity in capacity_order:
    for chemistry in chemistry_order:
        row_keys = [
            (policy, scope) for policy in policy_order for scope in SCOPES
        ]
        row_labels = [
            f"{policy.replace(' Policy', '')} | {scope_short[scope]}"
            for policy, scope in row_keys
        ]
        fig, axes = plt.subplots(
            2, 3, figsize=(11.8, 6.9), sharex=True, sharey=True,
            gridspec_kw={"hspace": 0.22, "wspace": 0.10},
        )
        mesh = None
        for axis, mineral in zip(axes.flat, MINERALS):
            mineral_data = rcs[
                (rcs["Mineral"] == mineral)
                & (rcs["Battery Capacity"] == capacity)
                & (rcs["Chemistry"] == chemistry)
            ]
            matrix = np.full((len(row_keys), len(REPORT_YEARS)), np.nan)
            for row_index, (policy, scope) in enumerate(row_keys):
                values = mineral_data[
                    (mineral_data["Policy"] == policy)
                    & (mineral_data["Scope"] == scope)
                ].set_index("Year")["Maximum RCS (%)"]
                matrix[row_index, :] = [
                    values.get(year, np.nan) for year in REPORT_YEARS
                ]

            mesh = axis.imshow(
                matrix, cmap=cmap, vmin=0, vmax=heatmap_max,
                origin="upper", aspect="auto",
            )
            axis.set_title(mineral, fontweight="bold", pad=6)
            axis.set_xticks(np.arange(len(REPORT_YEARS)))
            axis.set_xticklabels(REPORT_YEARS)
            axis.set_yticks(np.arange(len(row_keys)))
            axis.set_yticklabels(row_labels, fontsize=8.4)
            axis.set_xticks(
                np.arange(-0.5, len(REPORT_YEARS), 1), minor=True
            )
            axis.set_yticks(np.arange(-0.5, len(row_keys), 1), minor=True)
            axis.grid(which="minor", color="white", linewidth=1.0)
            axis.tick_params(which="both", length=0)
            axis.axhline(2.5, color="#111111", linewidth=2.0)

            for row_index in range(matrix.shape[0]):
                for column_index in range(matrix.shape[1]):
                    value = matrix[row_index, column_index]
                    if np.isnan(value):
                        continue
                    text_color = (
                        "white" if value > heatmap_max * 0.56 else "#25302F"
                    )
                    axis.text(
                        column_index, row_index, f"{value:.1f}",
                        ha="center", va="center", fontsize=8.0,
                        color=text_color, fontweight="bold",
                    )

        capacity_title = (
            "Increase LIB Cap" if capacity.startswith("Increasing")
            else "Decrease LIB Cap"
        )
        chemistry_title = (
            "Benchmark Chemistry" if chemistry.startswith("Benchmark")
            else "High-LFP Chemistry"
        )
        fig.suptitle(
            "Maximum Feasible Recycled Content\n"
            f"{capacity_title} · {chemistry_title}",
            fontsize=16.5, fontweight="bold", y=0.985,
        )
        colorbar = fig.colorbar(
            mesh, ax=axes, orientation="horizontal",
            fraction=0.035, pad=0.11, aspect=42,
        )
        colorbar.set_label(
            "Maximum feasible recycled content (%)",
            fontsize=11.5, fontweight="bold",
        )
        fig.subplots_adjust(
            left=0.24, right=0.985, top=0.86, bottom=0.20
        )
        capacity_slug = (
            "Increase" if capacity.startswith("Increasing") else "Decrease"
        )
        chemistry_slug = (
            "Benchmark" if chemistry.startswith("Benchmark") else "HighLFP"
        )
        stem = (
            "Figure7A_Geographic_RCS_Heatmap_"
            f"{capacity_slug}_{chemistry_slug}"
        )
        save_figure(fig, stem)
        heatmap_outputs.append(OUT / f"{stem}.png")
        plt.close(fig)


# Geography-first view requested for direct comparison of all eight
# policy-capacity-chemistry combinations within California at a given year.
california_scope = "California-origin / US network"
scenario_rows = [
    (capacity, chemistry, policy)
    for capacity in capacity_order
    for chemistry in chemistry_order
    for policy in policy_order
]
scenario_labels = [
    " · ".join(
        [
            "Increase Cap" if capacity.startswith("Increasing") else "Decrease Cap",
            "Benchmark" if chemistry.startswith("Benchmark") else "High-LFP",
            policy.replace(" Policy", ""),
        ]
    )
    for capacity, chemistry, policy in scenario_rows
]

fig, axes = plt.subplots(
    2, 3, figsize=(12.3, 7.7), sharex=True, sharey=True,
    gridspec_kw={"hspace": 0.22, "wspace": 0.10},
)
mesh = None
for axis, mineral in zip(axes.flat, MINERALS):
    mineral_data = rcs[
        (rcs["Scope"] == california_scope)
        & (rcs["Mineral"] == mineral)
    ]
    matrix = np.full((len(scenario_rows), len(REPORT_YEARS)), np.nan)
    for row_index, (capacity, chemistry, policy) in enumerate(scenario_rows):
        values = mineral_data[
            (mineral_data["Battery Capacity"] == capacity)
            & (mineral_data["Chemistry"] == chemistry)
            & (mineral_data["Policy"] == policy)
        ].set_index("Year")["Maximum RCS (%)"]
        matrix[row_index, :] = [
            values.get(year, np.nan) for year in REPORT_YEARS
        ]

    mesh = axis.imshow(
        matrix, cmap=cmap, vmin=0, vmax=heatmap_max,
        origin="upper", aspect="auto",
    )
    axis.set_title(mineral, fontweight="bold", pad=6)
    axis.set_xticks(np.arange(len(REPORT_YEARS)))
    axis.set_xticklabels(REPORT_YEARS)
    axis.set_yticks(np.arange(len(scenario_rows)))
    axis.set_yticklabels(scenario_labels, fontsize=8.6)
    axis.set_xticks(np.arange(-0.5, len(REPORT_YEARS), 1), minor=True)
    axis.set_yticks(np.arange(-0.5, len(scenario_rows), 1), minor=True)
    axis.grid(which="minor", color="white", linewidth=1.0)
    axis.tick_params(which="both", length=0)
    axis.axhline(1.5, color="#737B78", linewidth=1.0)
    axis.axhline(3.5, color="#111111", linewidth=2.1)
    axis.axhline(5.5, color="#737B78", linewidth=1.0)

    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            value = matrix[row_index, column_index]
            if np.isnan(value):
                continue
            text_color = "white" if value > heatmap_max * 0.56 else "#25302F"
            axis.text(
                column_index, row_index, f"{value:.1f}",
                ha="center", va="center", fontsize=7.8,
                color=text_color, fontweight="bold",
            )

fig.suptitle(
    "Maximum Feasible Recycled Content — California\n"
    "California-origin feedstock / US facility network",
    fontsize=16.5, fontweight="bold", y=0.985,
)
colorbar = fig.colorbar(
    mesh, ax=axes, orientation="horizontal",
    fraction=0.034, pad=0.105, aspect=45,
)
colorbar.set_label(
    "Maximum feasible recycled content (%)",
    fontsize=11.5, fontweight="bold",
)
fig.subplots_adjust(left=0.265, right=0.985, top=0.855, bottom=0.19)
california_stem = "Figure7C_California_RCS_All_Scenarios_Heatmap"
save_figure(fig, california_stem)
heatmap_outputs.append(OUT / f"{california_stem}.png")
plt.close(fig)


# Compact effect-size heatmap. Each cell is a mean paired difference across
# the two remaining scenario dimensions, not a difference between unpaired
# group averages.
effect_labels = [
    "ΔPolicy:\nRollback − Baseline",
    "ΔCapacity:\nDecrease − Increase",
    "ΔChemistry:\nHigh-LFP − Benchmark",
]


def paired_effects(mineral_data, year):
    data = mineral_data[mineral_data["Year"] == year]

    policy_differences = []
    for capacity in capacity_order:
        for chemistry in chemistry_order:
            pair = data[
                (data["Battery Capacity"] == capacity)
                & (data["Chemistry"] == chemistry)
            ].set_index("Policy")["Maximum RCS (%)"]
            policy_differences.append(
                pair.get("Rollback Policy", np.nan)
                - pair.get("Baseline Policy", np.nan)
            )

    capacity_differences = []
    for policy in policy_order:
        for chemistry in chemistry_order:
            pair = data[
                (data["Policy"] == policy)
                & (data["Chemistry"] == chemistry)
            ].set_index("Battery Capacity")["Maximum RCS (%)"]
            capacity_differences.append(
                pair.get("Decreasing Batt Cap", np.nan)
                - pair.get("Increasing Batt Cap", np.nan)
            )

    chemistry_differences = []
    for policy in policy_order:
        for capacity in capacity_order:
            pair = data[
                (data["Policy"] == policy)
                & (data["Battery Capacity"] == capacity)
            ].set_index("Chemistry")["Maximum RCS (%)"]
            chemistry_differences.append(
                pair.get("High LFP Chemistry", np.nan)
                - pair.get("Benchmark Chemistry", np.nan)
            )

    return [
        np.nanmean(policy_differences),
        np.nanmean(capacity_differences),
        np.nanmean(chemistry_differences),
    ]


def paired_effects_and_finals(mineral_data, year):
    """Return one-factor-at-a-time deltas from the fixed reference case."""
    data = mineral_data[mineral_data["Year"] == year]

    def get_value(policy, capacity, chemistry):
        match = data[
            (data["Policy"] == policy)
            & (data["Battery Capacity"] == capacity)
            & (data["Chemistry"] == chemistry)
        ]["Maximum RCS (%)"]
        return match.iloc[0] if len(match) else np.nan

    reference = get_value(
        "Baseline Policy", "Increasing Batt Cap", "Benchmark Chemistry"
    )
    policy_final = get_value(
        "Rollback Policy", "Increasing Batt Cap", "Benchmark Chemistry"
    )
    capacity_final = get_value(
        "Baseline Policy", "Decreasing Batt Cap", "Benchmark Chemistry"
    )
    chemistry_final = get_value(
        "Baseline Policy", "Increasing Batt Cap", "High LFP Chemistry"
    )
    finals = [policy_final, capacity_final, chemistry_final]
    deltas = [value - reference for value in finals]
    return deltas, finals


effect_matrices = {}
for mineral in MINERALS:
    mineral_data = rcs[
        (rcs["Scope"] == california_scope)
        & (rcs["Mineral"] == mineral)
    ]
    effect_matrices[mineral] = np.array(
        [paired_effects(mineral_data, year) for year in REPORT_YEARS]
    ).T

effect_limit = max(
    5,
    np.ceil(
        max(np.nanmax(np.abs(matrix)) for matrix in effect_matrices.values())
        / 2
    )
    * 2,
)
effect_norm = TwoSlopeNorm(vmin=-effect_limit, vcenter=0, vmax=effect_limit)
fig, axes = plt.subplots(
    2, 3, figsize=(11.8, 6.7), sharex=True, sharey=True,
    gridspec_kw={"hspace": 0.25, "wspace": 0.10},
)
mesh = None
for axis, mineral in zip(axes.flat, MINERALS):
    matrix = effect_matrices[mineral]
    mesh = axis.imshow(
        matrix, cmap="RdBu_r", norm=effect_norm,
        origin="upper", aspect="auto",
    )
    axis.set_title(mineral, fontweight="bold", pad=6)
    axis.set_xticks(np.arange(len(REPORT_YEARS)))
    axis.set_xticklabels(REPORT_YEARS)
    axis.set_yticks(np.arange(len(effect_labels)))
    axis.set_yticklabels(effect_labels, fontsize=8.8)
    axis.set_xticks(np.arange(-0.5, len(REPORT_YEARS), 1), minor=True)
    axis.set_yticks(np.arange(-0.5, len(effect_labels), 1), minor=True)
    axis.grid(which="minor", color="white", linewidth=1.2)
    axis.tick_params(which="both", length=0)

    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            value = matrix[row_index, column_index]
            text_color = (
                "white"
                if abs(value) > effect_limit * 0.58
                else "#25302F"
            )
            axis.text(
                column_index, row_index, f"{value:+.1f}",
                ha="center", va="center", fontsize=8.4,
                color=text_color, fontweight="bold",
            )

fig.suptitle(
    "Scenario Effects on Maximum Feasible Recycled Content — California",
    fontsize=16.2, fontweight="bold", y=0.975,
)
colorbar = fig.colorbar(
    mesh, ax=axes, orientation="horizontal",
    fraction=0.04, pad=0.15, aspect=42,
)
colorbar.set_label(
    "Change in maximum feasible recycled content (percentage points)",
    fontsize=11.2, fontweight="bold",
)
fig.subplots_adjust(left=0.205, right=0.985, top=0.89, bottom=0.22)
california_delta_stem = "Figure7D_California_RCS_Scenario_Effects_Heatmap"
save_figure(fig, california_delta_stem)
heatmap_outputs.append(OUT / f"{california_delta_stem}.png")
plt.close(fig)


# Combined A/B/C geography figure. Each panel is one geographic scope;
# minerals form row blocks and each block contains the three delta effects.
scope_panels = [
    ("a", "California", "California-origin / US network"),
    ("b", "United States", "US-origin / US network"),
    ("c", "North America", "North America-origin / NA network"),
]
scope_network_subtitles = {
    "California-origin / US network":
        "California-origin feedstock / California LIB demand",
    "US-origin / US network":
        "US-origin feedstock / US LIB demand",
    "North America-origin / NA network":
        "NA-origin feedstock / NA LIB demand",
}
effect_names_short = ["ΔPolicy", "ΔCapacity", "ΔChemistry"]
mineral_abbreviations = {
    "Nickel": "Ni",
    "Manganese": "Mn",
    "Cobalt": "Co",
    "Lithium": "Li",
    "Graphite": "Gr",
    "Copper": "Cu",
}
combined_matrices = {}
combined_final_matrices = {}
all_effect_values = []
for _, _, scope in scope_panels:
    rows, final_rows = [], []
    for mineral in MINERALS:
        mineral_data = rcs[
            (rcs["Scope"] == scope)
            & (rcs["Mineral"] == mineral)
        ]
        paired = [
            paired_effects_and_finals(mineral_data, year)
            for year in REPORT_YEARS
        ]
        mineral_matrix = np.array([item[0] for item in paired]).T
        final_matrix = np.array([item[1] for item in paired]).T
        rows.append(mineral_matrix)
        final_rows.append(final_matrix)
        all_effect_values.append(mineral_matrix)
    combined_matrices[scope] = np.vstack(rows)
    combined_final_matrices[scope] = np.vstack(final_rows)

combined_limit = max(
    5,
    np.ceil(
        max(np.nanmax(np.abs(matrix)) for matrix in all_effect_values) / 2
    ) * 2,
)
combined_norm = TwoSlopeNorm(
    vmin=-combined_limit, vcenter=0, vmax=combined_limit
)
combined_row_labels = effect_names_short * len(MINERALS)

effect_source_rows = []
for _, geography, scope in scope_panels:
    delta_matrix = combined_matrices[scope]
    final_matrix = combined_final_matrices[scope]
    for mineral_index, mineral in enumerate(MINERALS):
        for effect_index, effect_name in enumerate(effect_names_short):
            row_index = mineral_index * len(effect_names_short) + effect_index
            for year_index, year in enumerate(REPORT_YEARS):
                delta = delta_matrix[row_index, year_index]
                final_value = final_matrix[row_index, year_index]
                effect_source_rows.append(
                    {
                        "Geography": geography,
                        "Scope": scope,
                        "Mineral": mineral,
                        "Scenario effect": effect_name,
                        "Year": year,
                        "Reference maximum RCS (%)": final_value - delta,
                        "Change (percentage points)": delta,
                        "Resulting maximum RCS (%)": final_value,
                    }
                )
pd.DataFrame(effect_source_rows).to_csv(
    DATA_OUT / "Fig16_Maximum_RCS_Scenario_Effects.csv", index=False
)

fig, axes = plt.subplots(
    1, 3, figsize=(19.6, 11.2),
    gridspec_kw={"wspace": 0.10},
)
mesh = None
for panel_index, (axis, (panel_letter, geography, scope)) in enumerate(
    zip(axes, scope_panels)
):
    matrix = combined_matrices[scope]
    final_matrix = combined_final_matrices[scope]
    mesh = axis.imshow(
        matrix, cmap="RdBu_r", norm=combined_norm,
        origin="upper", aspect="auto",
    )
    axis.set_title(
        f"{panel_letter}) {geography}",
        loc="left", fontsize=18, fontweight="bold", pad=11,
    )
    axis.set_xticks(np.arange(len(REPORT_YEARS)))
    axis.set_xticklabels(REPORT_YEARS, fontsize=12)
    axis.set_yticks(np.arange(len(combined_row_labels)))
    axis.set_yticklabels(
        combined_row_labels if panel_index == 0 else [],
        fontsize=10.5,
    )
    axis.set_xticks(np.arange(-0.5, len(REPORT_YEARS), 1), minor=True)
    axis.set_yticks(
        np.arange(-0.5, len(combined_row_labels), 1), minor=True
    )
    axis.grid(which="minor", color="white", linewidth=0.9)
    axis.tick_params(which="both", length=0)
    for boundary in [2.5, 5.5, 8.5, 11.5, 14.5]:
        axis.axhline(boundary, color="#111111", linewidth=1.6)

    if panel_index == 0:
        for mineral_index, mineral in enumerate(MINERALS):
            axis.annotate(
                mineral_abbreviations[mineral],
                xy=(-1.72, mineral_index * 3 + 1),
                xycoords="data",
                ha="center", va="center",
                fontsize=13, fontweight="bold", color="#25302F",
                fontfamily="DejaVu Sans Mono",
                annotation_clip=False,
            )

    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            value = matrix[row_index, column_index]
            text_color = (
                "white"
                if abs(value) > combined_limit * 0.58
                else "#25302F"
            )
            axis.text(
                column_index, row_index,
                f"{value:+.1f}\n({final_matrix[row_index, column_index]:.1f})",
                ha="center", va="center", fontsize=8.6,
                linespacing=1.05,
                color=text_color, fontweight="bold",
            )

fig.suptitle(
    "Scenario Effects on Maximum Feasible Recycled Content",
    fontsize=21, fontweight="bold", y=0.98,
)

fig.text(
    0.5, 0.93,
    "ΔPolicy = Rollback − Baseline   |   "
    "ΔCapacity = Decrease − Increase   |   "
    "ΔChemistry = High-LFP − Benchmark   |   "
    "Cell: Δ change (Reference + Δ)",
    ha="center", fontsize=13, color="#3F4D49",
)
colorbar = fig.colorbar(
    mesh, ax=axes, orientation="horizontal",
    fraction=0.028, pad=0.09, aspect=55,
)
colorbar.set_label(
    "Δ maximum feasible recycled content (percentage points)",
    fontsize=14.5, fontweight="bold",
)
colorbar.ax.tick_params(labelsize=12)
fig.subplots_adjust(left=0.14, right=0.99, top=0.88, bottom=0.16)
combined_delta_stem = "Figure7E_RCS_Scenario_Effects_Geographies_ABC"
save_figure(fig, combined_delta_stem)
heatmap_outputs.append(OUT / f"{combined_delta_stem}.png")
plt.close(fig)


# Complementary absolute-value figure. Scenario effects are communicated in
# Figure 7E, so this panel deliberately fixes one transparent reference case.
reference_data = rcs[
    (rcs["Policy"] == "Baseline Policy")
    & (rcs["Battery Capacity"] == "Increasing Batt Cap")
    & (rcs["Chemistry"] == "Benchmark Chemistry")
]
reference_data.to_csv(
    DATA_OUT / "Fig15_Maximum_RCS_Geographic_Boundaries.csv", index=False
)
reference_matrices = {}
for _, _, scope in scope_panels:
    matrix = np.full((len(MINERALS), len(REPORT_YEARS)), np.nan)
    scope_data = reference_data[reference_data["Scope"] == scope]
    for mineral_index, mineral in enumerate(MINERALS):
        values = scope_data[
            scope_data["Mineral"] == mineral
        ].set_index("Year")["Maximum RCS (%)"]
        matrix[mineral_index, :] = [
            values.get(year, np.nan) for year in REPORT_YEARS
        ]
    reference_matrices[scope] = matrix

reference_max = (
    np.ceil(
        max(np.nanmax(matrix) for matrix in reference_matrices.values()) / 5
    )
    * 5
)
fig, axes = plt.subplots(
    1, 3, figsize=(16.8, 6.7), sharey=False,
    gridspec_kw={"wspace": 0.10},
)
mesh = None
for panel_index, (axis, (panel_letter, geography, scope)) in enumerate(
    zip(axes, scope_panels)
):
    matrix = reference_matrices[scope]
    mesh = axis.imshow(
        matrix, cmap="YlGnBu", vmin=0, vmax=reference_max,
        origin="upper", aspect="auto",
    )
    axis.set_title(
        f"{panel_letter}) {geography}",
        loc="left", fontsize=16, fontweight="bold", pad=10,
    )
    axis.set_xticks(np.arange(len(REPORT_YEARS)))
    axis.set_xticklabels(REPORT_YEARS, fontsize=11.5)
    axis.set_yticks(np.arange(len(MINERALS)))
    axis.set_yticklabels(
        [mineral_abbreviations[mineral] for mineral in MINERALS]
        if panel_index == 0 else [],
        fontsize=12, fontfamily="DejaVu Sans Mono", fontweight="bold",
    )
    axis.set_xticks(np.arange(-0.5, len(REPORT_YEARS), 1), minor=True)
    axis.set_yticks(np.arange(-0.5, len(MINERALS), 1), minor=True)
    axis.grid(which="minor", color="white", linewidth=1.2)
    axis.tick_params(which="both", length=0)

    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            value = matrix[row_index, column_index]
            text_color = (
                "white" if value > reference_max * 0.56 else "#25302F"
            )
            axis.text(
                column_index, row_index, f"{value:.1f}",
                ha="center", va="center", fontsize=10.5,
                color=text_color, fontweight="bold",
            )

fig.suptitle(
    "Maximum Feasible Recycled Content",
    fontsize=20, fontweight="bold", y=0.98,
)
fig.text(
    0.5, 0.92,
    "Policy Baseline · Increase LIB Cap · Benchmark Chemistry",
    ha="center", fontsize=13, color="#3F4D49", fontfamily="Arial",
)
colorbar = fig.colorbar(
    mesh, ax=axes, orientation="horizontal",
    fraction=0.045, pad=0.15, aspect=48,
)
colorbar.set_label(
    "Maximum feasible recycled content (%)",
    fontsize=14, fontweight="bold",
)
colorbar.ax.tick_params(labelsize=11.5)
fig.subplots_adjust(left=0.075, right=0.99, top=0.84, bottom=0.22)
reference_stem = (
    "Figure7F_Maximum_Feasible_RCS_"
    "Baseline_Increase_Benchmark_Geographies_ABC"
)
save_figure(fig, reference_stem)
heatmap_outputs.append(OUT / f"{reference_stem}.png")
plt.close(fig)


# Two-panel composite: (a) absolute reference values and (b) scenario
# changes with the resulting RCS shown in parentheses.
fig = plt.figure(figsize=(24.0, 22.0))
x_positions = [0.12, 0.415, 0.71]
panel_width = 0.27
top_axes = [
    fig.add_axes([x, 0.70, panel_width, 0.18]) for x in x_positions
]
bottom_axes = [
    fig.add_axes([x, 0.105, panel_width, 0.43]) for x in x_positions
]

reference_mesh = None
for panel_index, (axis, (_, geography, scope)) in enumerate(
    zip(top_axes, scope_panels)
):
    matrix = reference_matrices[scope]
    reference_mesh = axis.imshow(
        matrix, cmap="YlGnBu", vmin=0, vmax=reference_max,
        origin="upper", aspect="auto",
    )
    axis.set_title(
        f"{geography}\n{scope_network_subtitles[scope]}",
        fontsize=18, fontweight="bold", pad=10, linespacing=1.25,
        fontfamily="Arial",
    )
    axis.set_xticks(np.arange(len(REPORT_YEARS)))
    axis.set_xticklabels(REPORT_YEARS, fontsize=15)
    axis.set_yticks(np.arange(len(MINERALS)))
    axis.set_yticklabels(
        [mineral_abbreviations[mineral] for mineral in MINERALS]
        if panel_index == 0 else [],
        fontsize=16, fontfamily="Arial", fontweight="bold",
    )
    axis.set_xticks(np.arange(-0.5, len(REPORT_YEARS), 1), minor=True)
    axis.set_yticks(np.arange(-0.5, len(MINERALS), 1), minor=True)
    axis.grid(which="minor", color="white", linewidth=1.1)
    axis.tick_params(which="both", length=0)
    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            value = matrix[row_index, column_index]
            axis.text(
                column_index, row_index, f"{value:.1f}",
                ha="center", va="center", fontsize=14.5,
                color=(
                    "white"
                    if value > reference_max * 0.56
                    else "#25302F"
                ),
                fontweight="bold",
            )

effect_mesh = None
for panel_index, (axis, (_, geography, scope)) in enumerate(
    zip(bottom_axes, scope_panels)
):
    matrix = combined_matrices[scope]
    final_matrix = combined_final_matrices[scope]
    effect_mesh = axis.imshow(
        matrix, cmap="RdBu_r", norm=combined_norm,
        origin="upper", aspect="auto",
    )
    axis.set_title(
        f"{geography}\n{scope_network_subtitles[scope]}",
        fontsize=18.5, fontweight="bold", pad=10, linespacing=1.25,
        fontfamily="Arial",
    )
    axis.set_xticks(np.arange(len(REPORT_YEARS)))
    axis.set_xticklabels(REPORT_YEARS, fontsize=15.5)
    axis.set_yticks(np.arange(len(combined_row_labels)))
    axis.set_yticklabels(
        combined_row_labels if panel_index == 0 else [],
        fontsize=14.5, fontfamily="Arial",
    )
    axis.set_xticks(np.arange(-0.5, len(REPORT_YEARS), 1), minor=True)
    axis.set_yticks(
        np.arange(-0.5, len(combined_row_labels), 1), minor=True
    )
    axis.grid(which="minor", color="white", linewidth=0.9)
    axis.tick_params(which="both", length=0)
    for boundary in [2.5, 5.5, 8.5, 11.5, 14.5]:
        axis.axhline(boundary, color="#111111", linewidth=1.5)

    if panel_index == 0:
        for mineral_index, mineral in enumerate(MINERALS):
            axis.annotate(
                mineral_abbreviations[mineral],
                xy=(-1.72, mineral_index * 3 + 1),
                xycoords="data", ha="center", va="center",
                fontsize=17, fontweight="bold", color="#25302F",
                fontfamily="Arial", annotation_clip=False,
            )

    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            delta = matrix[row_index, column_index]
            result = final_matrix[row_index, column_index]
            axis.text(
                column_index, row_index, f"{delta:+.1f} ({result:.1f})",
                ha="center", va="center", fontsize=13.5,
                color=(
                    "white"
                    if abs(delta) > combined_limit * 0.58
                    else "#25302F"
                ),
                fontweight="bold",
            )

fig.suptitle(
    "Maximum Feasible Recycled Content and Scenario Effects",
    fontsize=27, fontweight="bold", y=0.985, fontfamily="Arial",
)
fig.text(
    0.035, 0.925,
    "a) Reference values",
    fontsize=22, fontweight="bold", fontfamily="Arial",
)
fig.text(
    0.5, 0.925,
    "Policy Baseline · Increase LIB Cap · Benchmark Chemistry",
    ha="center", fontsize=17, color="#3F4D49", fontfamily="Arial",
)
fig.text(
    0.035, 0.615,
    "b) Scenario changes",
    fontsize=22, fontweight="bold", fontfamily="Arial",
)
fig.text(
    0.5, 0.575,
    "ΔPolicy = Rollback − Baseline   |   "
    "ΔCapacity = Decrease − Increase   |   "
    "ΔChemistry = High-LFP − Benchmark   |   "
    "Cell: Δ change (Reference + Δ)",
    ha="center", fontsize=15.5, color="#3F4D49", fontfamily="Arial",
)

reference_cax = fig.add_axes([0.25, 0.645, 0.55, 0.016])
reference_colorbar = fig.colorbar(
    reference_mesh, cax=reference_cax, orientation="horizontal"
)
reference_colorbar.set_label(
    "Maximum feasible recycled content (%)",
    fontsize=16.5, fontweight="bold", fontfamily="Arial",
)
reference_colorbar.ax.tick_params(labelsize=14.5)

effect_cax = fig.add_axes([0.25, 0.035, 0.55, 0.016])
effect_colorbar = fig.colorbar(
    effect_mesh, cax=effect_cax, orientation="horizontal"
)
effect_colorbar.set_label(
    "Δ maximum feasible recycled content (percentage points)",
    fontsize=16.5, fontweight="bold", fontfamily="Arial",
)
effect_colorbar.ax.tick_params(labelsize=14.5)

composite_stem = (
    "Figure15_Maximum_Feasible_Recycled_Content_and_Scenario_Effects"
)
save_figure(fig, composite_stem)
heatmap_outputs.append(OUT / f"{composite_stem}.png")
plt.close(fig)


# Split Figure 7G into two landscape panels for manuscript readability.
# Panel a: absolute reference values only.
fig, axes = plt.subplots(
    1, 3, figsize=(19.8, 6.9),
    gridspec_kw={"wspace": 0.10},
)
reference_mesh = None
for panel_index, (axis, (_, geography, scope)) in enumerate(
    zip(axes, scope_panels)
):
    matrix = reference_matrices[scope]
    reference_mesh = axis.imshow(
        matrix, cmap="YlGnBu", vmin=0, vmax=reference_max,
        origin="upper", aspect="auto",
    )
    axis.set_title(
        geography,
        fontsize=18.5, fontweight="bold", pad=34,
        fontfamily="Arial",
    )
    axis.text(
        0.5, 1.035,
        scope_network_subtitles[scope],
        transform=axis.transAxes,
        ha="center", va="bottom",
        fontsize=15.5, fontweight="bold",
        fontfamily="Arial", clip_on=False,
    )
    axis.set_xticks(np.arange(len(REPORT_YEARS)))
    axis.set_xticklabels(REPORT_YEARS, fontsize=17.5)
    axis.set_yticks(np.arange(len(MINERALS)))
    axis.set_yticklabels(
        [mineral_abbreviations[mineral] for mineral in MINERALS]
        if panel_index == 0 else [],
        fontsize=19, fontfamily="Arial", fontweight="bold",
    )
    axis.set_xticks(np.arange(-0.5, len(REPORT_YEARS), 1), minor=True)
    axis.set_yticks(np.arange(-0.5, len(MINERALS), 1), minor=True)
    axis.grid(which="minor", color="white", linewidth=1.25)
    axis.tick_params(which="both", length=0)
    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            value = matrix[row_index, column_index]
            axis.text(
                column_index, row_index, f"{value:.1f}",
                ha="center", va="center", fontsize=17,
                color=(
                    "white"
                    if value > reference_max * 0.56
                    else "#25302F"
                ),
                fontweight="bold",
            )

fig.suptitle(
    "Maximum Feasible Recycled Content",
    fontsize=27, fontweight="bold", y=0.985, fontfamily="Arial",
)
fig.text(
    0.5, 0.895,
    "Policy Baseline · Increase LIB Cap · Benchmark Chemistry",
    ha="center", fontsize=18, color="#3F4D49", fontfamily="Arial",
)
reference_colorbar = fig.colorbar(
    reference_mesh, ax=axes, orientation="horizontal",
    fraction=0.045, pad=0.18, aspect=50,
)
reference_colorbar.set_label(
    "Maximum feasible recycled content (%)",
    fontsize=18, fontweight="bold", fontfamily="Arial",
)
reference_colorbar.ax.tick_params(labelsize=16)
fig.subplots_adjust(left=0.075, right=0.99, top=0.76, bottom=0.24)
reference_split_stem = FIGURE_OUT / "Fig15_Maximum_RCS_Geographic_Boundaries"
if MAIN_FIGURE in {"15", "both"}:
    save_figure(fig, reference_split_stem)
    heatmap_outputs.append(reference_split_stem.with_suffix(".png"))
plt.close(fig)


# Panel b: scenario effects only. Kept as a separate wide landscape figure so
# each cell can carry both the delta and the resulting recycled-content value.
fig, axes = plt.subplots(
    1, 3, figsize=(11.0, 8.5),
    gridspec_kw={"wspace": 0.22},
)
effect_mesh = None
for panel_index, (axis, (_, geography, scope)) in enumerate(
    zip(axes, scope_panels)
):
    matrix = combined_matrices[scope]
    final_matrix = combined_final_matrices[scope]
    effect_mesh = axis.imshow(
        matrix, cmap="RdBu_r", norm=combined_norm,
        origin="upper", aspect="auto",
    )
    axis.set_title(
        geography,
        fontsize=11.2, fontweight="bold", pad=24,
        fontfamily="Arial",
    )
    axis.text(
        0.5, 1.025,
        scope_network_subtitles[scope],
        transform=axis.transAxes,
        ha="center", va="bottom",
        fontsize=8.5, fontweight="bold",
        fontfamily="Arial", clip_on=False,
    )
    axis.set_xticks(np.arange(len(REPORT_YEARS)))
    axis.set_xticklabels(REPORT_YEARS, fontsize=8.6)
    axis.set_yticks(np.arange(len(combined_row_labels)))
    axis.set_yticklabels(
        combined_row_labels if panel_index == 0 else [],
        fontsize=7.5, fontfamily="Arial",
    )
    axis.set_xticks(np.arange(-0.5, len(REPORT_YEARS), 1), minor=True)
    axis.set_yticks(
        np.arange(-0.5, len(combined_row_labels), 1), minor=True
    )
    axis.grid(which="minor", color="white", linewidth=0.55)
    axis.tick_params(which="both", length=0)
    for boundary in [2.5, 5.5, 8.5, 11.5, 14.5]:
        axis.axhline(boundary, color="#111111", linewidth=0.75)

    if panel_index == 0:
        for mineral_index, mineral in enumerate(MINERALS):
            axis.annotate(
                mineral_abbreviations[mineral],
                xy=(-1.72, mineral_index * 3 + 1),
                xycoords="data", ha="center", va="center",
                fontsize=9.3, fontweight="bold", color="#25302F",
                fontfamily="Arial", annotation_clip=False,
            )

    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            delta = matrix[row_index, column_index]
            result = final_matrix[row_index, column_index]
            axis.text(
                column_index, row_index, f"{delta:+.1f} ({result:.1f})",
                ha="center", va="center", fontsize=6.6,
                color=(
                    "white"
                    if abs(delta) > combined_limit * 0.58
                    else "#25302F"
                ),
                fontweight="bold",
            )

fig.suptitle(
    "Scenario Effects on Maximum Feasible Recycled Content",
    fontsize=14.5, fontweight="bold", y=0.985, fontfamily="Arial",
)
fig.text(
    0.5, 0.915,
    "ΔPolicy = Rollback − Baseline   |   "
    "ΔCapacity = Decrease − Increase   |   "
    "ΔChemistry = High-LFP − Benchmark   |   "
    "Cell: Δ change (Reference + Δ)",
    ha="center", fontsize=8.5, color="#3F4D49", fontfamily="Arial",
)
effect_colorbar = fig.colorbar(
    effect_mesh, ax=axes, orientation="horizontal",
    fraction=0.04, pad=0.13, aspect=55,
)
effect_colorbar.set_label(
    "Δ maximum feasible recycled content (percentage points)",
    fontsize=9.0, fontweight="bold", fontfamily="Arial",
)
effect_colorbar.ax.tick_params(labelsize=8.0)
fig.subplots_adjust(left=0.115, right=0.99, top=0.84, bottom=0.17)
effect_split_stem = FIGURE_OUT / "Fig16_Maximum_RCS_Scenario_Effects"
if MAIN_FIGURE in {"16", "both"}:
    save_figure(fig, effect_split_stem)
    heatmap_outputs.append(effect_split_stem.with_suffix(".png"))
plt.close(fig)


# Legacy line-chart exports are intentionally disabled; the deterministic
# scenario grid is now communicated with the heatmap above.
for policy in []:
    policy_rcs = rcs[rcs["Policy"] == policy]
    fig, axes = plt.subplots(
        2, 3, figsize=(11.2, 7.0), sharex=True, sharey=True,
        gridspec_kw={"hspace": 0.30, "wspace": 0.18},
    )
    for axis, mineral in zip(axes.flat, MINERALS):
        subset = policy_rcs[policy_rcs["Mineral"] == mineral]
        for scope, color in SCOPE_COLORS.items():
            series = subset[subset["Scope"] == scope].sort_values("Year")
            axis.plot(
                series["Year"], series["Maximum RCS (%)"],
                color=color, marker="o", markersize=4.5, linewidth=2.1,
            )
        axis.set_title(mineral, fontweight="bold")
        axis.set_xlim(2035, 2050)
        axis.set_xticks(REPORT_YEARS)
        axis.grid(axis="y", color="#D8D8D8", linewidth=0.7)
        axis.spines[["top", "right"]].set_visible(False)

    ymax = np.nanmax(policy_rcs["Maximum RCS (%)"]) * 1.10
    axes[0, 0].set_ylim(0, max(10, ymax))
    fig.suptitle(
        f"Maximum Feasible Recycled Content by Feedstock and Facility Boundary\n{policy}",
        fontsize=17, fontweight="bold", y=0.98,
    )
    fig.supylabel("Maximum feasible recycled content (%)", x=0.018,
                  fontsize=13, fontweight="bold")
    fig.text(0.5, 0.18, "Year", ha="center", fontsize=13, fontweight="bold")
    handles = [
        Line2D([0], [0], color=color, marker="o", lw=2.4, label=scope)
        for scope, color in SCOPE_COLORS.items()
    ]
    legend = fig.legend(
        handles=handles, title="Feedstock origin / facility network",
        loc="lower center", bbox_to_anchor=(0.5, 0.02),
        frameon=False, ncol=3,
    )
    legend.get_title().set_fontweight("bold")
    fig.subplots_adjust(left=0.085, right=0.985, top=0.86, bottom=0.24)
    suffix = "Baseline" if policy.startswith("Baseline") else "Rollback"
    save_figure(fig, f"Figure7A_Geographic_RCS_{suffix}")
    plt.close(fig)

    policy_gaps = gaps[gaps["Policy"] == policy].copy()
    policy_gaps["Gap million"] = (
        policy_gaps["Annual capacity gap (MT/year)"] / 1e6
    )
    fig, axes = plt.subplots(1, 2, figsize=(10.8, 5.8), sharex=True)
    for axis, stage in zip(axes, policy_gaps["Stage"].unique()):
        subset = policy_gaps[policy_gaps["Stage"] == stage]
        for scope, color in SCOPE_COLORS.items():
            series = subset[subset["Scope"] == scope].sort_values("Year")
            axis.plot(
                series["Year"], series["Gap million"],
                color=color, linewidth=2.2, label=scope,
            )
        axis.set_title(stage, fontweight="bold")
        axis.set_xlim(2025, 2050)
        axis.set_ylim(bottom=0)
        axis.set_xticks(range(2025, 2051, 5))
        axis.set_xlabel("Year", fontweight="bold")
        axis.grid(axis="y", color="#D8D8D8", linewidth=0.7)
        axis.spines[["top", "right"]].set_visible(False)
    axes[0].set_ylabel("Annual capacity gap (million metric tonnes/year)",
                       fontweight="bold")
    fig.suptitle(
        f"Annual Recycling Capacity Gap by Feedstock and Facility Boundary\n{policy}",
        fontsize=17, fontweight="bold", y=0.98,
    )
    handles = [
        Line2D([0], [0], color=color, lw=2.5, label=scope)
        for scope, color in SCOPE_COLORS.items()
    ]
    legend = fig.legend(
        handles=handles, title="Feedstock origin / facility network",
        loc="lower center", bbox_to_anchor=(0.5, 0.02),
        frameon=False, ncol=3,
    )
    legend.get_title().set_fontweight("bold")
    fig.subplots_adjust(left=0.09, right=0.985, top=0.82, bottom=0.22, wspace=0.16)
    save_figure(fig, f"Figure7B_Geographic_Capacity_Gap_{suffix}")
    plt.close(fig)

for output in heatmap_outputs:
    print(output)
