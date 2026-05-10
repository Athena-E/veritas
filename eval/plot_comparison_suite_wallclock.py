#!/usr/bin/env python3
"""Generate a grouped bar chart for the cross-system comparison suite."""

from __future__ import annotations

import csv
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import LogFormatterSciNotation, LogLocator


REPO_ROOT = Path(__file__).resolve().parents[1]
VERITAS_CSV = REPO_ROOT / "eval" / "comparison_suite" / "results" / "veritas_wallclock.csv"
EXTERNAL_CSV = REPO_ROOT / "eval" / "comparison_suite" / "results" / "external_metrics.csv"
OUTPUT_DIR = REPO_ROOT / "docs" / "veritas-dissertation" / "figures"
OUTPUT_BASENAME = "chapter4_comparison_suite_wallclock"

TASK_ORDER = [
    "C01",
    "C02",
    "C03",
    "C04",
    "C05",
    "C06",
]

TASK_LABELS = {
    "C01": "safe_division",
    "C02": "bounded_read_offset",
    "C03": "fill_with_ones",
    "C04": "binary_search",
    "C05": "sorted_head",
    "C06": "safe_midpoint",
}

SERIES = [
    ("veritas", "compile_only", "Veritas compile-only", "#9FB3C8"),
    ("veritas", "compile_plus_verify", "Veritas compile+verify", "#1D4E89"),
    ("external", "dafny", "Dafny", "#7FB069"),
    ("external", "liquid_haskell", "Liquid Haskell", "#C45B12"),
    ("external", "verus", "Verus", "#7A4EAB"),
]


def load_veritas(path: Path) -> dict[tuple[str, str], tuple[float, float]]:
    with path.open(newline="", encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    return {
        (row["task_id"], row["mode"]): (float(row["mean_s"]), float(row["stddev_s"]))
        for row in rows
    }


def load_external(path: Path) -> dict[tuple[str, str], tuple[float, float]]:
    with path.open(newline="", encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    return {
        (row["task_id"], row["system"]): (float(row["mean_s"]), float(row["stddev_s"]))
        for row in rows
    }


def main() -> None:
    veritas = load_veritas(VERITAS_CSV)
    external = load_external(EXTERNAL_CSV)

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    x = np.arange(len(TASK_ORDER))
    width = 0.15

    fig, ax = plt.subplots(figsize=(12.4, 5.3), constrained_layout=True)
    offsets = np.linspace(-2 * width, 2 * width, len(SERIES))

    for offset, (family, key, label, color) in zip(offsets, SERIES):
        values: list[float] = []
        errors: list[float] = []
        for task_id in TASK_ORDER:
            if family == "veritas":
                mean_s, stddev_s = veritas[(task_id, key)]
            else:
                mean_s, stddev_s = external[(task_id, key)]
            values.append(mean_s)
            errors.append(stddev_s)

        ax.bar(
            x + offset,
            values,
            width=width,
            label=label,
            color=color,
            yerr=errors,
            error_kw={"elinewidth": 0.9, "capsize": 2.5, "capthick": 0.9, "ecolor": "#444444"},
            linewidth=0,
        )

    ax.set_xticks(x)
    ax.set_xticklabels([TASK_LABELS[task_id] for task_id in TASK_ORDER], rotation=18, ha="right")
    ax.set_yscale("log")
    ax.set_ylabel("mean wall-clock time (s, log scale)", fontsize=10)
    ax.set_title("Cross-System Verification-Command Wall-Clock Times", fontsize=12, pad=10)
    ax.yaxis.set_major_locator(LogLocator(base=10, subs=(1.0, 2.0, 5.0)))
    ax.yaxis.set_major_formatter(LogFormatterSciNotation(base=10))
    ax.grid(axis="y", alpha=0.2, which="both")
    ax.legend(frameon=False, fontsize=8, ncol=5, loc="upper left")

    svg_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.svg"
    png_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.png"
    fig.savefig(svg_path, bbox_inches="tight")
    fig.savefig(png_path, dpi=220, bbox_inches="tight")
    print(svg_path)
    print(png_path)


if __name__ == "__main__":
    main()
