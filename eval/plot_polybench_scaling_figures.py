#!/usr/bin/env python3
"""Plot PolyBench scaling experiment figures."""

from __future__ import annotations

import csv
import sys
from collections import defaultdict
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import LogFormatterSciNotation, LogLocator


REPO_ROOT = Path(__file__).resolve().parents[1]
OUTPUT_DIR = REPO_ROOT / "docs" / "veritas-dissertation" / "figures"

KERNEL_ORDER = [
    "floyd_warshall",
    "jacobi_2d",
    "seidel_2d",
    "mvt",
    "gesummv",
]

LABELS = {
    "floyd_warshall": "Floyd-Warshall",
    "jacobi_2d": "Jacobi-2D",
    "seidel_2d": "Seidel-2D",
    "mvt": "MVT",
    "gesummv": "GESUMMV",
}

IMPL_STYLES = {
    "veritas": ("Veritas", "#0B3C5D", "o"),
    "gcc_o3": ("GCC -O3", "#328CC1", "s"),
    "checked_c_o3": ("checked C -O3", "#C45B12", "^"),
}


def load_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def group_rows(rows: list[dict[str, str]]) -> dict[str, dict[str, list[dict[str, str]]]]:
    grouped: dict[str, dict[str, list[dict[str, str]]]] = defaultdict(lambda: defaultdict(list))
    for row in rows:
        if row["implementation"] not in IMPL_STYLES:
            continue
        grouped[row["kernel"]][row["implementation"]].append(row)
    for kernel in grouped:
        for impl in grouped[kernel]:
            grouped[kernel][impl].sort(key=lambda row: int(row["size"]))
    return grouped


def plot_absolute(grouped: dict[str, dict[str, list[dict[str, str]]]], run_label: str) -> None:
    fig, axes = plt.subplots(2, 3, figsize=(10.2, 6.5))
    axes = axes.flatten()

    for idx, kernel in enumerate(KERNEL_ORDER):
        ax = axes[idx]
        for impl_key, (label, color, marker) in IMPL_STYLES.items():
            rows = grouped.get(kernel, {}).get(impl_key, [])
            if not rows:
                continue
            x = [int(row["size"]) for row in rows]
            y = [float(row["mean_s"]) for row in rows]
            yerr = [float(row["stddev_s"]) for row in rows]
            ax.errorbar(x, y, yerr=yerr, marker=marker, color=color, label=label, linewidth=1.7, capsize=2.5)

        ax.set_title(LABELS[kernel], fontsize=10)
        ax.set_yscale("log")
        ax.yaxis.set_major_locator(LogLocator(base=10, subs=(1.0, 2.0, 5.0)))
        ax.yaxis.set_major_formatter(LogFormatterSciNotation(base=10))
        size_ticks = sorted({int(row["size"]) for impl_rows in grouped.get(kernel, {}).values() for row in impl_rows})
        ax.set_xticks(size_ticks)
        ax.set_xticklabels([str(v) for v in size_ticks], fontsize=9)
        ax.grid(alpha=0.2)
        if idx % 3 == 0:
            ax.set_ylabel("mean runtime (s, log)")
        ax.set_xlabel("problem size parameter (N)")

    for idx in range(len(KERNEL_ORDER), len(axes)):
        axes[idx].set_visible(False)

    handles, labels = axes[0].get_legend_handles_labels()
    fig.suptitle(f"PolyBench Runtime Scaling by Kernel ({run_label})", fontsize=13, y=0.962)
    fig.legend(handles, labels, frameon=False, fontsize=9, loc="upper center", bbox_to_anchor=(0.5, 0.915), ncol=3)
    fig.tight_layout(rect=(0, 0.01, 1, 0.93))
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUTPUT_DIR / "chapter4_polybench_scaling_absolute.svg", bbox_inches="tight")
    fig.savefig(OUTPUT_DIR / "chapter4_polybench_scaling_absolute.png", dpi=220, bbox_inches="tight")


def plot_ratios(grouped: dict[str, dict[str, list[dict[str, str]]]], run_label: str) -> None:
    fig, axes = plt.subplots(2, 3, figsize=(10.2, 6.5))
    axes = axes.flatten()

    for idx, kernel in enumerate(KERNEL_ORDER):
        ax = axes[idx]
        gcc_rows = grouped.get(kernel, {}).get("gcc_o3", [])
        checked_rows = grouped.get(kernel, {}).get("checked_c_o3", [])
        veritas_rows = grouped.get(kernel, {}).get("veritas", [])
        if gcc_rows and veritas_rows:
            x = [int(row["size"]) for row in gcc_rows]
            veritas_by_size = {int(row["size"]): float(row["mean_s"]) for row in veritas_rows}
            gcc_ratio = [veritas_by_size[s] / float(row["mean_s"]) for s, row in zip(x, gcc_rows)]
            ax.plot(x, gcc_ratio, marker="o", color="#328CC1", linewidth=1.7, label="Veritas / GCC -O3")
        if gcc_rows and checked_rows:
            x = [int(row["size"]) for row in gcc_rows]
            checked_by_size = {int(row["size"]): float(row["mean_s"]) for row in checked_rows}
            checked_ratio = [checked_by_size[s] / float(row["mean_s"]) for s, row in zip(x, gcc_rows)]
            ax.plot(x, checked_ratio, marker="^", color="#C45B12", linewidth=1.7, label="checked C -O3 / GCC -O3")

        ax.axhline(1.0, color="#666666", linewidth=1.0, linestyle="--")
        ax.set_title(LABELS[kernel], fontsize=10)
        size_ticks = sorted({int(row["size"]) for impl_rows in grouped.get(kernel, {}).values() for row in impl_rows})
        ax.set_xticks(size_ticks)
        ax.set_xticklabels([str(v) for v in size_ticks], fontsize=9)
        ax.grid(alpha=0.2)
        if idx % 3 == 0:
            ax.set_ylabel("runtime ratio")
        ax.set_xlabel("problem size parameter (N)")

    for idx in range(len(KERNEL_ORDER), len(axes)):
        axes[idx].set_visible(False)

    handles, labels = axes[0].get_legend_handles_labels()
    fig.suptitle(f"PolyBench Runtime Ratios by Kernel ({run_label})", fontsize=13, y=0.962)
    fig.legend(handles, labels, frameon=False, fontsize=9, loc="upper center", bbox_to_anchor=(0.5, 0.915), ncol=2)
    fig.tight_layout(rect=(0, 0.01, 1, 0.93))
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUTPUT_DIR / "chapter4_polybench_scaling_ratios.svg", bbox_inches="tight")
    fig.savefig(OUTPUT_DIR / "chapter4_polybench_scaling_ratios.png", dpi=220, bbox_inches="tight")


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit(f"usage: {Path(sys.argv[0]).name} <polybench_scaling_runtime.csv>")
    csv_path = Path(sys.argv[1]).resolve()
    run_label = csv_path.parent.parent.name
    rows = load_rows(csv_path)
    grouped = group_rows(rows)
    plot_absolute(grouped, run_label)
    plot_ratios(grouped, run_label)


if __name__ == "__main__":
    main()
