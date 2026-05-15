#!/usr/bin/env python3
"""Run PolyBench scaling experiments across multiple problem sizes."""

from __future__ import annotations

import csv
import json
import os
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
VERITAS = REPO_ROOT / "target" / "release" / "veritas"
RUNS = 30
WARMUP = 5


@dataclass(frozen=True)
class KernelSpec:
    key: str
    label: str
    veritas_source: Path
    c_source: Path
    checked_c_source: Path
    size_field: str
    sizes: tuple[int, ...]
    extra_fields: dict[str, int]


KERNELS: tuple[KernelSpec, ...] = (
    KernelSpec(
        key="floyd_warshall",
        label="floyd_warshall",
        veritas_source=REPO_ROOT / "eval" / "benchmarks" / "floyd-warshall" / "floyd_warshall.veri",
        c_source=REPO_ROOT / "eval" / "benchmarks" / "floyd-warshall" / "floyd_warshall.c",
        checked_c_source=REPO_ROOT / "eval" / "benchmarks" / "floyd-warshall" / "floyd_warshall_checked.c",
        size_field="N",
        sizes=(180, 300, 500, 700),
        extra_fields={},
    ),
    KernelSpec(
        key="jacobi_2d",
        label="jacobi_2d",
        veritas_source=REPO_ROOT / "eval" / "benchmarks" / "jacobi-2d-imper" / "jacobi_2d_imper.veri",
        c_source=REPO_ROOT / "eval" / "benchmarks" / "jacobi-2d-imper" / "jacobi_2d_imper.c",
        checked_c_source=REPO_ROOT / "eval" / "benchmarks" / "jacobi-2d-imper" / "jacobi_2d_imper_checked.c",
        size_field="N",
        sizes=(90, 170, 250, 330),
        extra_fields={"TSTEPS": 100},
    ),
    KernelSpec(
        key="seidel_2d",
        label="seidel_2d",
        veritas_source=REPO_ROOT / "eval" / "benchmarks" / "seidel-2d" / "seidel_2d.veri",
        c_source=REPO_ROOT / "eval" / "benchmarks" / "seidel-2d" / "seidel_2d.c",
        checked_c_source=REPO_ROOT / "eval" / "benchmarks" / "seidel-2d" / "seidel_2d_checked.c",
        size_field="N",
        sizes=(120, 250, 400, 550),
        extra_fields={"TSTEPS": 100},
    ),
    KernelSpec(
        key="mvt",
        label="mvt",
        veritas_source=REPO_ROOT / "eval" / "benchmarks" / "mvt" / "mvt.veri",
        c_source=REPO_ROOT / "eval" / "benchmarks" / "mvt" / "mvt.c",
        checked_c_source=REPO_ROOT / "eval" / "benchmarks" / "mvt" / "mvt_checked.c",
        size_field="N",
        sizes=(120, 250, 400, 550),
        extra_fields={},
    ),
    KernelSpec(
        key="gesummv",
        label="gesummv",
        veritas_source=REPO_ROOT / "eval" / "benchmarks" / "gesummv" / "gesummv.veri",
        c_source=REPO_ROOT / "eval" / "benchmarks" / "gesummv" / "gesummv.c",
        checked_c_source=REPO_ROOT / "eval" / "benchmarks" / "gesummv" / "gesummv_checked.c",
        size_field="N",
        sizes=(90, 170, 250, 330),
        extra_fields={},
    ),
)

IMPLEMENTATIONS = (
    ("veritas", "Veritas", None),
    ("gcc_o2", "GCC -O2", ["gcc", "-O2"]),
    ("gcc_o3", "GCC -O3", ["gcc", "-O3"]),
    ("checked_c_o2", "checked C -O2", ["gcc", "-O2"]),
    ("checked_c_o3", "checked C -O3", ["gcc", "-O3"]),
)


def run(cmd: list[str], *, cwd: Path | None = None, env: dict[str, str] | None = None, capture: bool = False) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        cmd,
        cwd=str(cwd) if cwd else None,
        env=env,
        text=True,
        capture_output=capture,
        check=True,
    )


def create_run_dir() -> Path:
    ts = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    run_dir = REPO_ROOT / "eval" / "runs" / f"{ts}_polybench_scaling"
    (run_dir / "generated").mkdir(parents=True, exist_ok=True)
    (run_dir / "bin").mkdir(parents=True, exist_ok=True)
    (run_dir / "raw" / "hyperfine").mkdir(parents=True, exist_ok=True)
    (run_dir / "derived").mkdir(parents=True, exist_ok=True)
    return run_dir


def write_metadata(run_dir: Path) -> None:
    metadata = run_dir / "metadata.env"
    with metadata.open("w", encoding="utf-8") as fh:
        fh.write(f"RUN_TIMESTAMP_UTC={datetime.now(timezone.utc).strftime('%Y%m%d_%H%M%S')}\n")
        fh.write(f"BENCH_RUNS={RUNS}\n")
        fh.write(f"BENCH_WARMUP={WARMUP}\n")
        fh.write("EXPERIMENT=polybench_scaling\n")
        fh.write(f"GIT_COMMIT={subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=REPO_ROOT, text=True).strip()}\n")


def render_veritas_variant(spec: KernelSpec, size: int) -> str:
    text = spec.veritas_source.read_text(encoding="utf-8")

    if spec.key == "floyd_warshall":
        return text.replace("500", str(size))
    if spec.key == "jacobi_2d":
        return text.replace("249", str(size - 1)).replace("250", str(size))
    if spec.key == "seidel_2d":
        return text.replace("399", str(size - 1)).replace("400", str(size))
    if spec.key == "mvt":
        return text.replace("400", str(size))
    if spec.key == "gesummv":
        return text.replace("250", str(size))
    raise ValueError(f"unsupported kernel {spec.key}")


def c_define_flags(spec: KernelSpec, size: int) -> list[str]:
    flags = [f"-D{spec.size_field}={size}"]
    for key, value in spec.extra_fields.items():
        flags.append(f"-D{key}={value}")
    return flags


def observe_checksum(binary: Path) -> str:
    proc = subprocess.run([str(binary)], text=True, capture_output=True)
    first_line = proc.stderr.splitlines()[0] if proc.stderr.strip() else proc.stdout.splitlines()[0]
    return first_line if first_line.startswith("checksum=") else f"checksum={first_line}"


def ensure_matching_checksum(label: str, expected: str, observed: str) -> None:
    if observed != expected:
        raise RuntimeError(f"{label}: expected {expected}, observed {observed}")


def compile_veritas(source: Path, output: Path) -> None:
    run([str(VERITAS), str(source), "-o", str(output)])


def compile_c(source: Path, output: Path, flags: list[str]) -> None:
    run(["gcc", *flags, "-o", str(output), str(source)])


def time_binary(binary: Path, export_json: Path) -> dict[str, float]:
    run(
        [
            "hyperfine",
            "--shell=none",
            "--ignore-failure",
            "--warmup",
            str(WARMUP),
            "--runs",
            str(RUNS),
            "--export-json",
            str(export_json),
            str(binary),
        ]
    )
    data = json.loads(export_json.read_text(encoding="utf-8"))
    result = data["results"][0]
    return {
        "mean_s": float(result["mean"]),
        "stddev_s": float(result["stddev"]),
        "min_s": float(result["min"]),
        "max_s": float(result["max"]),
        "user_s": float(result["user"]),
        "system_s": float(result["system"]),
    }


def main() -> None:
    if not VERITAS.exists():
        run(["cargo", "build", "--release", "-q"], cwd=REPO_ROOT)

    run_dir = create_run_dir()
    write_metadata(run_dir)

    csv_path = run_dir / "derived" / "polybench_scaling_runtime.csv"
    with csv_path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh)
        writer.writerow(
            [
                "kernel",
                "size",
                "implementation",
                "compiler",
                "expected_checksum",
                "mean_s",
                "stddev_s",
                "min_s",
                "max_s",
                "user_s",
                "system_s",
                "runs",
                "warmup",
            ]
        )

        for spec in KERNELS:
            for size in spec.sizes:
                tag = f"{spec.label}_{size}"
                print(f"[scaling] {tag}", file=sys.stderr)

                generated_veri = run_dir / "generated" / f"{tag}.veri"
                generated_veri.write_text(render_veritas_variant(spec, size), encoding="utf-8")

                flags = c_define_flags(spec, size)

                plain_probe = run_dir / "bin" / f"{tag}_probe"
                compile_c(spec.c_source, plain_probe, ["-O3", *flags])
                expected = observe_checksum(plain_probe)

                binaries = {
                    "veritas": run_dir / "bin" / f"{tag}_veritas",
                    "gcc_o2": run_dir / "bin" / f"{tag}_gcc_o2",
                    "gcc_o3": run_dir / "bin" / f"{tag}_gcc_o3",
                    "checked_c_o2": run_dir / "bin" / f"{tag}_checked_c_o2",
                    "checked_c_o3": run_dir / "bin" / f"{tag}_checked_c_o3",
                }

                compile_veritas(generated_veri, binaries["veritas"])
                ensure_matching_checksum("veritas", expected, observe_checksum(binaries["veritas"]))

                compile_c(spec.c_source, binaries["gcc_o2"], ["-O2", *flags])
                ensure_matching_checksum("gcc_o2", expected, observe_checksum(binaries["gcc_o2"]))

                compile_c(spec.c_source, binaries["gcc_o3"], ["-O3", *flags])
                ensure_matching_checksum("gcc_o3", expected, observe_checksum(binaries["gcc_o3"]))

                compile_c(spec.checked_c_source, binaries["checked_c_o2"], ["-O2", *flags])
                ensure_matching_checksum("checked_c_o2", expected, observe_checksum(binaries["checked_c_o2"]))

                compile_c(spec.checked_c_source, binaries["checked_c_o3"], ["-O3", *flags])
                ensure_matching_checksum("checked_c_o3", expected, observe_checksum(binaries["checked_c_o3"]))

                for impl_key, impl_label, _ in IMPLEMENTATIONS:
                    export_json = run_dir / "raw" / "hyperfine" / f"{tag}_{impl_key}.json"
                    stats = time_binary(binaries[impl_key], export_json)
                    writer.writerow(
                        [
                            spec.label,
                            size,
                            impl_key,
                            impl_label,
                            expected,
                            f"{stats['mean_s']:.12f}",
                            f"{stats['stddev_s']:.12f}",
                            f"{stats['min_s']:.12f}",
                            f"{stats['max_s']:.12f}",
                            f"{stats['user_s']:.12f}",
                            f"{stats['system_s']:.12f}",
                            RUNS,
                            WARMUP,
                        ]
                    )
                    fh.flush()

    print(run_dir)


if __name__ == "__main__":
    main()
