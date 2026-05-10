#!/usr/bin/env python3
"""Generate a Chapter 4 binary-size figure for the full feature suite."""

from __future__ import annotations

import csv
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


REPO_ROOT = Path(__file__).resolve().parents[1]
INPUT_CSV = (
    REPO_ROOT
    / "eval"
    / "runs"
    / "20260505_155605_binary-size"
    / "derived"
    / "binary_size_summary.csv"
)
OUTPUT_DIR = REPO_ROOT / "docs" / "veritas-dissertation" / "figures"
OUTPUT_BASENAME = "chapter4_feature_suite_binary_sizes"


def short_label(program: str) -> str:
    return program.replace("_", r"\_")


def load_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def main() -> None:
    rows = load_rows(INPUT_CSV)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    veritas_rows = sorted(
        rows,
        key=lambda row: int(row["veritas_elf_bytes"]),
        reverse=True,
    )

    subset_rows = [row for row in rows if row["gcc_o2_elf_bytes"]]
    subset_rows.sort(key=lambda row: row["program"])

    fig = plt.figure(figsize=(10.0, 8.0), constrained_layout=True)
    gs = fig.add_gridspec(2, 2, height_ratios=[1.7, 1.0])

    ax_top = fig.add_subplot(gs[0, :])
    ax_elf = fig.add_subplot(gs[1, 0])
    ax_text = fig.add_subplot(gs[1, 1])

    # Top: full feature-suite Veritas ELF/code-bearing size distribution.
    labels = [row["program"] for row in veritas_rows]
    values = [int(row["veritas_elf_bytes"]) for row in veritas_rows]
    y = np.arange(len(labels))
    ax_top.barh(y, values, color="#184E77")
    ax_top.set_yticks(y)
    ax_top.set_yticklabels(labels, fontsize=8)
    ax_top.invert_yaxis()
    ax_top.set_xlabel("Veritas binary size (bytes)")
    ax_top.set_title("Full Feature-Suite Veritas Binary Sizes", fontsize=11)
    ax_top.grid(axis="x", alpha=0.2)

    # Bottom-left: representative ELF comparison against GCC O2.
    subset_labels = [row["program"] for row in subset_rows]
    veritas_elf = [int(row["veritas_elf_bytes"]) for row in subset_rows]
    gcc_elf = [int(row["gcc_o2_elf_bytes"]) for row in subset_rows]
    y2 = np.arange(len(subset_labels))
    h = 0.34
    ax_elf.barh(y2 - h / 2, veritas_elf, height=h, color="#184E77", label="Veritas ELF")
    ax_elf.barh(y2 + h / 2, gcc_elf, height=h, color="#B8C5D6", label="GCC -O2 ELF")
    ax_elf.set_yticks(y2)
    ax_elf.set_yticklabels(subset_labels, fontsize=8)
    ax_elf.invert_yaxis()
    ax_elf.set_xlabel("ELF size (bytes)")
    ax_elf.set_title("Representative Total ELF Comparison", fontsize=10)
    ax_elf.grid(axis="x", alpha=0.2)
    ax_elf.legend(frameon=False, fontsize=8, loc="lower right")

    # Bottom-right: representative code-bearing size comparison.
    veritas_text = [int(row["veritas_text_bytes"]) for row in subset_rows]
    gcc_text = [int(row["gcc_o2_text_bytes"]) for row in subset_rows]
    ax_text.barh(y2 - h / 2, veritas_text, height=h, color="#184E77", label="Veritas code-bearing")
    ax_text.barh(y2 + h / 2, gcc_text, height=h, color="#7FB069", label="GCC -O2 .text")
    ax_text.set_yticks(y2)
    ax_text.set_yticklabels(subset_labels, fontsize=8)
    ax_text.invert_yaxis()
    ax_text.set_xlabel("Code size (bytes)")
    ax_text.set_title("Representative Code-Size Comparison", fontsize=10)
    ax_text.grid(axis="x", alpha=0.2)
    ax_text.legend(frameon=False, fontsize=8, loc="lower right")

    svg_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.svg"
    png_path = OUTPUT_DIR / f"{OUTPUT_BASENAME}.png"
    fig.savefig(svg_path, bbox_inches="tight")
    fig.savefig(png_path, dpi=220, bbox_inches="tight")
    print(svg_path)
    print(png_path)


if __name__ == "__main__":
    main()
