#!/usr/bin/env python3
"""Figure 15: unconstrained maximum RCS by feedstock and demand boundary."""

import os
import runpy
from pathlib import Path

os.environ["RCS_MAIN_FIGURE"] = "15"
runpy.run_path(
    str(Path(__file__).resolve().parent / "Supporting" / "RCS_Geographic_Analysis.py"),
    run_name="__main__",
)
