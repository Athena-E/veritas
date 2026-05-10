#!/usr/bin/env python3
"""Generate a grouped horizontal bar chart for annotation-to-code ratios."""

from __future__ import annotations

import csv
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


REPO_ROOT = Path(__file__).resolve().parents[1]
INPUT_CSV = REPO_ROOT / "eval" / "comparison_suite" / "results" / "loc_metrics.csv"
OUTPUT_DIR = REPO_ROOT / "docs" / "veritas-dissertation" / "figures"
OUTPUT_BASENAME = "chapter4_annotation_burden"

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

SYSTEMS = [
    ("veritas", "Veritas", "#1D4E89"),
    ("dafny", "Dafny", "#7FB069"),
    ("liquid_haskell", "Liquid Haskell", "#C45B12"),
    ("verus", "Verus", "#7A4EAB"),
]


def load_ratios(path: Path) -> dict[tuple[str, str], float]:
    with path.open(newline="", encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    return {
        (row["task_id"], row["system"]): float(row["annotation_to_code_ratio"])
        for row in rows
    }


def main() -> None:
    ratios = load_ratios(INPUT_CSV)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    y = np.arange(len(TASK_ORDER))
    height = 0.18
    offsets = np.linspace(-1.5 * height, 1.5 * height, len(SYSTEMS))

    fig, ax = plt.subplots(figsize=(9.4, 4.8), constrained_layout=True)

    for offset, (system, label, color) in zip(offsets, SYSTEMS):
        values = [ratios[(task_id, system)] for task_id in TASK_ORDER]
        ax.barh(y + offset, values, height=height, label=label, color=color)

    ax.set_yticks(y)
    ax.set_yticklabels([TASK_LABELS[task_id] for task_id in TASK_ORDER], fontsize=9)
    ax.invert_yaxis()
    ax.set_xlabel("annotation / executable code ratio", fontsize=10)
    ax.set_title("Cross-System Annotation Burden", fontsize=12, pad=10)
    ax.grid(axis="x", alpha=0.2)
    ax.legend(frameon=False, fontsize=8, ncol=4, loc="lower right")

    svg_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.svg"
    png_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.png"
    fig.savefig(svg_path, bbox_inches="tight")
    fig.savefig(png_path, dpi=220, bbox_inches="tight")
    print(svg_path)
    print(png_path)


if __name__ == "__main__":
    main()
