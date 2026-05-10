#!/usr/bin/env python3
"""Generate Chapter 4 Figure 1: feature-suite compile cost structure."""

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
OUTPUT_BASENAME = "chapter4_feature_suite_compile_cost"


def load_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def short_label(name: str) -> str:
    mapping = {
        "01_simple": "simple",
        "02_conditionals": "conditionals",
        "03_arrays": "arrays",
        "04_references": "references",
        "05_comparisons": "comparisons",
        "07_function_calls": "function_calls",
        "08_mutable": "mutable",
        "10_advanced_types": "advanced_types",
        "14_for_loops": "for_loops",
        "15_array_init": "array_init",
        "16_array_assignment": "array_assignment",
        "17_preconditions": "preconditions",
        "18_precondition_use": "precondition_use",
        "19_loop_invariant": "loop_invariant",
        "19_quantifier_showcase": "quantifier_showcase",
        "20_array_loop_invariant": "array_loop_invariant",
        "20_binary_search": "binary_search",
        "21_safe_division": "safe_division",
        "22_bubble_sort": "bubble_sort",
        "24_while_loop": "while_loop",
        "30_i64_safe_midpoint": "i64_safe_midpoint",
        "32_shared_borrow": "shared_borrow",
        "33_mutable_borrow": "mutable_borrow",
        "34_shared_scalar_deref": "shared_scalar_deref",
        "35_mutable_scalar_deref": "mutable_scalar_deref",
        "36_shared_scalar_borrow_call": "shared_borrow_call",
        "37_mutable_scalar_borrow_call": "mutable_borrow_call",
        "sortedness": "sortedness",
    }
    return mapping.get(name, name)


def main() -> None:
    rows = load_rows(INPUT_CSV)
    rows.sort(key=lambda row: float(row["compile_ms"]), reverse=True)

    labels = [short_label(row["file"]) for row in rows]
    compile_ms = [float(row["compile_ms"]) for row in rows]
    smt_ms = [float(row["frontend_smt_ms"]) for row in rows]
    nonsmt_ms = [max(total - smt, 0.0) for total, smt in zip(compile_ms, smt_ms)]

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    fig, ax = plt.subplots(figsize=(8.2, 8.8), constrained_layout=True)

    y = range(len(labels))
    ax.barh(y, nonsmt_ms, color="#9FB3C8", label="non-SMT compile time")
    ax.barh(y, smt_ms, left=nonsmt_ms, color="#1D4E89", label="frontend SMT time")

    ax.set_yticks(list(y))
    ax.set_yticklabels(labels, fontsize=8)
    ax.invert_yaxis()
    ax.set_xlabel("compile time (ms)", fontsize=10)
    ax.set_title("Feature-Suite Compile Cost Structure", fontsize=12, pad=10)
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
