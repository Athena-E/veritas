#!/usr/bin/env python3
"""Generate dissertation-ready feature-suite compiler metric plots."""

from __future__ import annotations

import csv
from pathlib import Path

import matplotlib.pyplot as plt


REPO_ROOT = Path(__file__).resolve().parents[1]
INPUT_CSV = (
    REPO_ROOT
    / "eval"
    / "runs"
    / "20260501_150813_chapter-compile-curated"
    / "derived"
    / "compilation.csv"
)
OUTPUT_DIR = REPO_ROOT / "docs" / "veritas-dissertation" / "figures"
OUTPUT_BASENAME = "feature_suite_compiler_metrics"


def load_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def sort_metric(rows: list[dict[str, str]], field: str) -> tuple[list[str], list[float]]:
    filtered = [(row["file"], float(row[field])) for row in rows]
    filtered.sort(key=lambda item: item[1], reverse=True)
    labels = [name for name, _ in filtered]
    values = [value for _, value in filtered]
    return labels, values


def plot_barh(ax, labels: list[str], values: list[float], title: str, xlabel: str, color: str) -> None:
    ax.barh(labels, values, color=color)
    ax.invert_yaxis()
    ax.set_title(title, fontsize=11, pad=8)
    ax.set_xlabel(xlabel, fontsize=9)
    ax.tick_params(axis="y", labelsize=8)
    ax.tick_params(axis="x", labelsize=8)
    ax.grid(axis="x", alpha=0.2)


def main() -> None:
    rows = load_rows(INPUT_CSV)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    fig, axes = plt.subplots(2, 2, figsize=(14, 13), constrained_layout=True)

    plot_specs = [
        ("compile_ms", "Compile Time Across Feature Suite", "compile time (ms)", "#2A6F97"),
        ("frontend_smt_ms", "Frontend SMT Time Across Feature Suite", "frontend SMT time (ms)", "#C77DFF"),
        (
            "frontend_smt_queries",
            "Frontend SMT Query Count Across Feature Suite",
            "frontend SMT queries",
            "#E76F51",
        ),
        ("binary_bytes", "Binary Size Across Feature Suite", "binary size (bytes)", "#4D908E"),
    ]

    for ax, (field, title, xlabel, color) in zip(axes.flat, plot_specs):
        labels, values = sort_metric(rows, field)
        plot_barh(ax, labels, values, title, xlabel, color)

    fig.suptitle("Feature-Suite Compiler Metrics", fontsize=15, y=1.01)

    svg_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.svg"
    png_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.png"
    fig.savefig(svg_path, bbox_inches="tight")
    fig.savefig(png_path, dpi=220, bbox_inches="tight")
    print(svg_path)
    print(png_path)


if __name__ == "__main__":
    main()
