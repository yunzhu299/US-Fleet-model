#!/usr/bin/env python3
"""Figure 16: scenario effects on unconstrained maximum RCS by boundary."""

import os
import runpy
from pathlib import Path

os.environ["RCS_MAIN_FIGURE"] = "16"
runpy.run_path(
    str(Path(__file__).resolve().parent / "Supporting" / "RCS_Geographic_Analysis.py"),
    run_name="__main__",
)
