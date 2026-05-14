#!/usr/bin/env python3
"""Generate a Chapter 4 PolyBench compile cost structure figure."""

from __future__ import annotations

import csv
from pathlib import Path

import matplotlib.pyplot as plt


REPO_ROOT = Path(__file__).resolve().parents[1]
INPUT_TSV = REPO_ROOT / "eval" / "polybench_compile_metrics.tsv"
OUTPUT_DIR = REPO_ROOT / "docs" / "veritas-dissertation" / "figures"
OUTPUT_BASENAME = "chapter4_polybench_compile_cost"


def load_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh, delimiter="\t"))


def short_label(name: str) -> str:
    mapping = {
        "floyd_warshall_500": "floyd_warshall",
        "jacobi_1d_400_100": "jacobi_1d",
        "jacobi_2d_250_100": "jacobi_2d",
        "seidel_2d_400_100": "seidel_2d",
        "mvt_400": "mvt",
        "atax_390_410": "atax",
        "gesummv_250": "gesummv",
    }
    return mapping.get(name, name)


def main() -> None:
    rows = load_rows(INPUT_TSV)
    rows.sort(key=lambda row: float(row["compile_ms"]), reverse=True)

    labels = [short_label(row["kernel"]) for row in rows]
    compile_ms = [float(row["compile_ms"]) for row in rows]
    smt_ms = [float(row["frontend_smt_ms"]) for row in rows]
    nonsmt_ms = [max(total - smt, 0.0) for total, smt in zip(compile_ms, smt_ms)]
    smt_shares = [(smt / total * 100.0) if total > 0.0 else 0.0 for total, smt in zip(compile_ms, smt_ms)]

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    fig, ax = plt.subplots(figsize=(9.2, 4.6), constrained_layout=True)

    y = list(range(len(labels)))
    bar_height = 0.34
    ax.barh(
        [yi - bar_height / 2 for yi in y],
        nonsmt_ms,
        height=bar_height,
        color="#B8C5D6",
        label="non-SMT compile time",
    )
    ax.barh(
        [yi + bar_height / 2 for yi in y],
        smt_ms,
        height=bar_height,
        color="#184E77",
        label="frontend SMT time",
    )

    for yi, share in zip(y, smt_shares):
        other_share = max(100.0 - share, 0.0)
        ax.text(
            1.01,
            yi,
            f"SMT {share:.0f}% / other {other_share:.0f}%",
            transform=ax.get_yaxis_transform(),
            va="center",
            ha="left",
            fontsize=8,
            color="#243447",
        )

    ax.set_yticks(y)
    ax.set_yticklabels(labels, fontsize=9)
    ax.invert_yaxis()
    ax.set_xscale("log")
    ax.set_xlabel("compile time (ms)", fontsize=10)
    ax.set_title("PolyBench Kernel Compile Cost Structure", fontsize=12, pad=10)
    ax.grid(axis="x", alpha=0.2)
    ax.legend(frameon=False, fontsize=9, loc="lower right")

    svg_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.svg"
    png_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.png"
    fig.savefig(svg_path, bbox_inches="tight")
    fig.savefig(png_path, dpi=220, bbox_inches="tight")
    print(svg_path)
    print(png_path)


if __name__ == "__main__":
    main()
