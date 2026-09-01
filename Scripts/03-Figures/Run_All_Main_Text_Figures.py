#!/usr/bin/env python3
"""Run the R and Python scripts for CARB report Figures 2–17 in article order.

The runner can be launched with any Python 3 installation. Before plotting, it
finds a Python interpreter that has the required plotting packages. Set the
FIGURE_PYTHON environment variable to override automatic discovery.
"""

from pathlib import Path
import argparse
import os
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
PROJECT_ROOT = HERE.parents[1]
FIGURE_SCRIPTS = Path("Scripts") / "03-Figures"
RSCRIPT = os.environ.get("RSCRIPT", "Rscript")
PYTHON_PACKAGES = ("matplotlib", "numpy", "pandas")
R_PACKAGES = (
    "dplyr", "geofacet", "ggplot2", "patchwork", "readr", "readxl",
    "scales", "stringr", "tidyr",
)


def python_has_packages(executable):
    """Return True when an interpreter can locate every plotting dependency."""
    package_list = repr(PYTHON_PACKAGES)
    check_code = (
        "import importlib.util, sys; "
        f"packages={package_list}; "
        "missing=[p for p in packages if importlib.util.find_spec(p) is None]; "
        "sys.exit(1 if missing else 0)"
    )
    try:
        result = subprocess.run(
            [str(executable), "-c", check_code],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )
    except OSError:
        return False
    return result.returncode == 0


def python_candidates():
    """Yield unique Python executables, preferring an explicit override."""
    override = os.environ.get("FIGURE_PYTHON")
    if override:
        yield Path(override).expanduser()
        return

    seen = set()
    candidates = [Path(sys.executable)]
    executable_names = ("python3", "python", "python.exe")
    for directory in os.environ.get("PATH", "").split(os.pathsep):
        if not directory:
            continue
        for name in executable_names:
            candidates.append(Path(directory) / name)

    conda_prefix = os.environ.get("CONDA_PREFIX")
    if conda_prefix:
        candidates.extend(
            [Path(conda_prefix) / "bin" / "python3", Path(conda_prefix) / "python.exe"]
        )

    for candidate in candidates:
        if not candidate.is_file() or not os.access(candidate, os.X_OK):
            continue
        resolved = str(candidate.resolve())
        if resolved in seen:
            continue
        seen.add(resolved)
        yield Path(resolved)


def select_figure_python():
    override = os.environ.get("FIGURE_PYTHON")
    for candidate in python_candidates():
        if python_has_packages(candidate):
            return candidate

    package_text = " ".join(PYTHON_PACKAGES)
    if override:
        raise SystemExit(
            f"FIGURE_PYTHON={override!r} does not provide all required packages: "
            f"{package_text}.\nInstall them with:\n  {override} -m pip install {package_text}"
        )
    raise SystemExit(
        "No Python interpreter with all required plotting packages was found.\n"
        f"Required packages: {package_text}\n"
        "Install them in a Python environment, then set FIGURE_PYTHON to that "
        "interpreter."
    )


def check_r_environment():
    rscript_path = shutil.which(RSCRIPT)
    if rscript_path is None:
        raise SystemExit(
            f"Rscript executable {RSCRIPT!r} was not found. Set RSCRIPT to its path."
        )

    package_vector = ",".join(f'"{package}"' for package in R_PACKAGES)
    check_code = (
        f"required <- c({package_vector}); "
        "missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]; "
        "if (length(missing)) {"
        "cat('Missing R packages:', paste(missing, collapse=', '), '\\n'); quit(status=2)}"
    )
    result = subprocess.run(
        [rscript_path, "-e", check_code],
        cwd=PROJECT_ROOT,
        check=False,
    )
    if result.returncode != 0:
        raise SystemExit(
            "Install the missing R packages before running the figure pipeline."
        )
    return rscript_path


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument(
    "--check",
    action="store_true",
    help="Check Python and R plotting dependencies without generating figures.",
)
args = parser.parse_args()

FIGURE_PYTHON = select_figure_python()
RSCRIPT = check_r_environment()
print(f"Figure Python: {FIGURE_PYTHON}", flush=True)
print(f"Rscript: {RSCRIPT}", flush=True)
if args.check:
    print("All figure dependencies are available.", flush=True)
    raise SystemExit(0)

commands = [
    [RSCRIPT, FIGURE_SCRIPTS / "Fig02_EV_Share_New_LDV_Sales.R"],
    [RSCRIPT, FIGURE_SCRIPTS / "Fig03_Vehicle_Survival_Curves.R"],
    [RSCRIPT, FIGURE_SCRIPTS / "Fig04_North_American_New_Vehicle_Sales.R"],
    [RSCRIPT, FIGURE_SCRIPTS / "Fig05_California_LDV_Sales_and_Retirements.R"],
    [RSCRIPT, FIGURE_SCRIPTS / "Fig06_North_American_Vehicle_Retirements.R"],
    [RSCRIPT, FIGURE_SCRIPTS / "Fig07_North_American_Cumulative_Battery_Retirements.R"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Fig08_Regional_Supply_Chain_2050.py"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Fig09_Country_Supply_Chain_2050.py"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Fig10_North_American_Flows_Over_Time.py"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Fig11_Annual_Recycling_Capacity_Deficit.py", "--annual"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Fig12_Recoverable_Minerals.py"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Fig13_Cumulative_MRR_Losses_Through_2035.py"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Fig14_Maximum_RCS_North_America.py"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Supporting" / "RCS_Geographic_Analysis.py"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Supporting" / "Prepare_Fig17_Additional_Recycling_Capacity_Data.py"],
    [FIGURE_PYTHON, FIGURE_SCRIPTS / "Fig17_Additional_Recycling_Capacity_Required.py"],
]

environment = os.environ.copy()
environment["RCS_MAIN_FIGURE"] = "both"
for command in commands:
    print(f"Running {Path(command[1]).name}", flush=True)
    subprocess.run(
        [str(item) for item in command],
        check=True,
        cwd=PROJECT_ROOT,
        env=environment,
    )
