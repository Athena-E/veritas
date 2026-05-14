#!/usr/bin/env python3
"""Generate a stacked bar chart for verification-stress suite overheads."""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


REPO_ROOT = Path(__file__).resolve().parents[1]
OUTPUT_DIR = REPO_ROOT / "docs" / "veritas-dissertation" / "figures"
OUTPUT_BASENAME = "chapter4_verification_stress_overhead"

PROGRAMS = [
    "array_init",
    "array_assignment",
    "quantifier_showcase",
    "binary_search",
    "bubble_sort",
    "sortedness",
]

COMPILE_ONLY = np.array([0.0702, 0.1029, 0.1319, 0.2809, 0.1310, 0.0404])
COMPILE_PLUS_VERIFY = np.array([0.3143, 0.1418, 0.1957, 0.3810, 0.1976, 0.0523])
VERIFY_OVERHEAD = COMPILE_PLUS_VERIFY - COMPILE_ONLY


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    x = np.arange(len(PROGRAMS))

    fig, ax = plt.subplots(figsize=(8.8, 4.8), constrained_layout=True)
    ax.bar(x, COMPILE_ONLY, color="#9FB3C8", label="compile only")
    ax.bar(x, VERIFY_OVERHEAD, bottom=COMPILE_ONLY, color="#1D4E89", label="added DTAL verification")

    ax.set_xticks(x)
    ax.set_xticklabels(PROGRAMS, rotation=18, ha="right")
    ax.set_ylabel("wall-clock time (s)", fontsize=10)
    ax.set_title("Verification-Stress Suite Wall-Clock Overhead", fontsize=12, pad=10)
    ax.grid(axis="y", alpha=0.2)
    ax.legend(frameon=False, fontsize=9, ncol=2, loc="upper left")

    for i, total in enumerate(COMPILE_PLUS_VERIFY):
        ax.text(i, total + 0.004, f"{total:.3f}s", ha="center", va="bottom", fontsize=8)

    svg_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.svg"
    png_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.png"
    fig.savefig(svg_path, bbox_inches="tight")
    fig.savefig(png_path, dpi=220, bbox_inches="tight")
    print(svg_path)
    print(png_path)


if __name__ == "__main__":
    main()
