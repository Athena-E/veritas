#!/usr/bin/env python3
"""Generate Chapter 4 PolyBench runtime figures."""

from __future__ import annotations

import csv
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import LogFormatterSciNotation, LogLocator


REPO_ROOT = Path(__file__).resolve().parents[1]
INPUT_CSV = (
    REPO_ROOT
    / "eval"
    / "runs"
    / "20260505_145814_runtime"
    / "derived"
    / "algorithm_runtime_summary.csv"
)
TIMING_CSV = (
    REPO_ROOT
    / "eval"
    / "runs"
    / "20260505_145814_runtime"
    / "derived"
    / "runtime_timing.csv"
)
OUTPUT_DIR = REPO_ROOT / "docs" / "veritas-dissertation" / "figures"
ABS_BASENAME = "chapter4_polybench_runtime_absolute"
RATIO_BASENAME = "chapter4_polybench_runtime_ratios"

KERNEL_ORDER = [
    "floyd_warshall_500",
    "jacobi_2d_250_100",
    "seidel_2d_400_100",
    "jacobi_1d_400_100",
    "mvt_400",
    "atax_390_410",
    "gesummv_250",
]

LABELS = {
    "floyd_warshall_500": "floyd_warshall",
    "jacobi_1d_400_100": "jacobi_1d",
    "jacobi_2d_250_100": "jacobi_2d",
    "seidel_2d_400_100": "seidel_2d",
    "mvt_400": "mvt",
    "atax_390_410": "atax",
    "gesummv_250": "gesummv",
}


def load_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    return [row for row in rows if row["benchmark"] in KERNEL_ORDER]


def load_stddevs(path: Path) -> dict[tuple[str, str], float]:
    with path.open(newline="", encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    result: dict[tuple[str, str], float] = {}
    for row in rows:
        benchmark = row["benchmark"]
        implementation = row["implementation"]
        if benchmark not in KERNEL_ORDER:
            continue
        result[(benchmark, implementation)] = float(row["stddev_s"])
    return result


def order_rows(rows: list[dict[str, str]]) -> list[dict[str, str]]:
    index = {name: i for i, name in enumerate(KERNEL_ORDER)}
    return sorted(rows, key=lambda row: index[row["benchmark"]])


def build_absolute_figure(rows: list[dict[str, str]], stddevs: dict[tuple[str, str], float]) -> None:
    labels = [LABELS[row["benchmark"]] for row in rows]
    implementations = [
        ("veritas", "veritas_mean_s", "Veritas", "#0B3C5D"),
        ("gcc_o3", "gcc_o3_mean_s", "GCC -O3", "#7FB069"),
        ("checked_c_o3", "checked_c_o3_mean_s", "checked C -O3", "#C45B12"),
    ]

    y = np.arange(len(labels))
    height = 0.22

    fig, ax = plt.subplots(figsize=(8.4, 4.8), constrained_layout=True)

    offsets = np.linspace(-height, height, len(implementations))
    for offset, (impl_key, key, label, color) in zip(offsets, implementations):
        values = [float(row[key]) if row.get(key) else np.nan for row in rows]
        errors = [stddevs.get((row["benchmark"], impl_key), np.nan) for row in rows]
        ax.barh(
            y + offset,
            values,
            height=height,
            label=label,
            color=color,
            xerr=errors,
            error_kw={"elinewidth": 0.9, "capsize": 2.5, "capthick": 0.9, "ecolor": "#444444"},
        )
        ax.scatter(
            values,
            y + offset,
            marker="x",
            s=26,
            linewidths=1.0,
            color="#111111",
            zorder=3,
        )

    ax.set_yticks(y)
    ax.set_yticklabels(labels, fontsize=9)
    ax.invert_yaxis()
    ax.set_xscale("log")
    ax.set_xlabel("mean runtime (s, log scale)", fontsize=10)
    ax.xaxis.set_major_locator(LogLocator(base=10, subs=(1.0, 2.0, 5.0)))
    ax.xaxis.set_major_formatter(LogFormatterSciNotation(base=10))
    ax.set_title("PolyBench Runtime by Implementation", fontsize=12, pad=10)
    ax.grid(axis="x", alpha=0.2, which="both")
    ax.legend(frameon=False, fontsize=8, ncol=1, loc="lower right")

    svg_path = OUTPUT_DIR / f"{ABS_BASENAME}.svg"
    png_path = OUTPUT_DIR / f"{ABS_BASENAME}.png"
    fig.savefig(svg_path, bbox_inches="tight")
    fig.savefig(png_path, dpi=220, bbox_inches="tight")
    print(svg_path)
    print(png_path)


def build_ratio_figure(rows: list[dict[str, str]]) -> None:
    labels = [LABELS[row["benchmark"]] for row in rows]
    ratio_keys = [
        ("veritas_vs_gcc_o2", "vs GCC -O2", "#328CC1"),
        ("veritas_vs_gcc_o3", "vs GCC -O3", "#7FB069"),
        ("veritas_vs_checked_c_o2", "vs checked C -O2", "#D9A441"),
        ("veritas_vs_checked_c_o3", "vs checked C -O3", "#C45B12"),
    ]

    y = np.arange(len(labels))
    height = 0.18

    fig, ax = plt.subplots(figsize=(8.4, 4.8), constrained_layout=True)

    offsets = np.linspace(-1.5 * height, 1.5 * height, len(ratio_keys))
    for offset, (key, label, color) in zip(offsets, ratio_keys):
        values = [float(row[key]) if row.get(key) else np.nan for row in rows]
        ax.barh(y + offset, values, height=height, label=label, color=color)

    ax.axvline(1.0, color="#444444", linewidth=1.0, linestyle="--")
    ax.set_yticks(y)
    ax.set_yticklabels(labels, fontsize=9)
    ax.invert_yaxis()
    ax.set_xlabel("runtime ratio (Veritas / baseline)", fontsize=10)
    ax.set_title("PolyBench Runtime Ratios for Veritas", fontsize=12, pad=10)
    ax.grid(axis="x", alpha=0.2)
    ax.legend(frameon=False, fontsize=8, ncol=2, loc="lower right")

    svg_path = OUTPUT_DIR / f"{RATIO_BASENAME}.svg"
    png_path = OUTPUT_DIR / f"{RATIO_BASENAME}.png"
    fig.savefig(svg_path, bbox_inches="tight")
    fig.savefig(png_path, dpi=220, bbox_inches="tight")
    print(svg_path)
    print(png_path)


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    rows = order_rows(load_rows(INPUT_CSV))
    stddevs = load_stddevs(TIMING_CSV)
    build_absolute_figure(rows, stddevs)
    build_ratio_figure(rows)


if __name__ == "__main__":
    main()
