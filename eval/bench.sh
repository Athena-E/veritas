#!/usr/bin/env bash
# Veritas Evaluation Benchmark Harness
#
# This harness is manifest-driven and writes outputs into a reproducible run
# directory under eval/runs/.
#
# Usage:
#   ./eval/bench.sh [--all | --compile | --timing | --verify | --errors |
#                    --binary-size | --runtime | --loc]
#
# Environment:
#   BENCH_LABEL=<label>      label for the run directory
#   BENCH_RUN_DIR=<path>     existing run directory to reuse
#   BENCH_RUNS=<n>           hyperfine run count / repeated run count
#   BENCH_WARMUP=<n>         hyperfine warmup count

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$REPO_ROOT"

VERITAS="target/release/veritas"
SUITE_RESOLVER="$SCRIPT_DIR/resolve_suite.sh"
CREATE_RUN_DIR="$SCRIPT_DIR/create_run_dir.sh"
TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

RUNS="${BENCH_RUNS:-10}"
WARMUP="${BENCH_WARMUP:-3}"
MODE="${1:---all}"
RUN_LABEL="${BENCH_LABEL:-${MODE#--}}"

if [ -n "${BENCH_RUN_DIR:-}" ]; then
    RUN_DIR="$BENCH_RUN_DIR"
else
    RUN_DIR="$("$CREATE_RUN_DIR" "$RUN_LABEL")"
fi

RAW_DIR="$RUN_DIR/raw"
RESULTS_DIR="$RUN_DIR/derived"
LOGS_DIR="$RUN_DIR/logs"

mkdir -p "$RAW_DIR" "$RESULTS_DIR" "$LOGS_DIR"

if [ -f "$RUN_DIR/metadata.env" ]; then
    {
        printf 'BENCH_MODE=%q\n' "$MODE"
        printf 'BENCH_RUNS=%q\n' "$RUNS"
        printf 'BENCH_WARMUP=%q\n' "$WARMUP"
    } >> "$RUN_DIR/metadata.env"
fi

command_exists() {
    command -v "$1" >/dev/null 2>&1
}

log() {
    printf '%s\n' "$*" >&2
}

jfield() {
    python3 -c "import json,sys; d=json.load(sys.stdin); print(d.get('$1', 'N/A'))"
}

json_field() {
    local file="$1"
    local expr="$2"
    python3 - "$file" "$expr" <<'PY'
import json
import sys

path = sys.argv[1]
expr = sys.argv[2]

with open(path, "r", encoding="utf-8") as fh:
    data = json.load(fh)

result = data
for part in expr.split("."):
    if part.isdigit():
        result = result[int(part)]
    else:
        result = result[part]
print(result)
PY
}

run_and_capture() {
    local bin="$1"
    local stdout_file="$2"
    local stderr_file="$3"

    python3 - "$bin" "$stdout_file" "$stderr_file" <<'PY'
import subprocess
import sys

bin_path, stdout_path, stderr_path = sys.argv[1:4]
with open(stdout_path, "wb") as stdout_fh, open(stderr_path, "wb") as stderr_fh:
    result = subprocess.run([bin_path], stdout=stdout_fh, stderr=stderr_fh, check=False)
print(result.returncode)
PY
}

write_invocation_log() {
    {
        echo ""
        echo "# bench.sh invocation"
        printf 'BENCH_MODE=%q BENCH_RUNS=%q BENCH_WARMUP=%q ' "$MODE" "$RUNS" "$WARMUP"
        printf '%q ' "$0" "$@"
        echo ""
    } >> "$RUN_DIR/commands.sh"
}

resolve_suite() {
    local manifest="$1"
    local output_name="$2"
    local output_path="$RAW_DIR/$output_name"
    bash "$SUITE_RESOLVER" "$manifest" | tee "$output_path"
}

suite_stem() {
    local manifest="$1"
    basename "$manifest" .txt
}

sanitize_csv_field() {
    printf '%s' "$1" | tr ',' ';' | tr '"' "'"
}

jfield_file() {
    local file="$1"
    local field="$2"
    jfield "$field" < "$file"
}

extract_diagnostic_summary() {
    local diagnostic_file="$1"
    local source_path="${2:-}"

    python3 - "$diagnostic_file" "$source_path" <<'PY'
import re
import sys

diagnostic_path = sys.argv[1]
source_path = sys.argv[2]

ansi = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")

priority = []
fallback = []

with open(diagnostic_path, "r", encoding="utf-8", errors="replace") as fh:
    for raw_line in fh:
        line = ansi.sub("", raw_line).strip()
        if not line:
            continue
        if source_path and line == source_path:
            continue
        if line in {"Source code:"}:
            continue
        if set(line) <= {"=", "-"}:
            continue
        if line.startswith("//"):
            continue
        if line.startswith("-->"):
            continue
        if "│" in line or "╭" in line or "╰" in line or "─" in line:
            continue
        if "<unknown>:" in line:
            continue
        if "Error:" in line or "FAILED" in line or "failed" in line:
            priority.append(line)
            continue
        fallback.append(line)

summary = priority[0] if priority else (fallback[0] if fallback else "")
summary = summary.replace(",", ";").replace('"', "'")
print(summary[:160])
PY
}

lookup_feature_metadata() {
    local source_path="$1"
    awk -F '\t' -v key="$source_path" '
        NR > 1 && $1 == key {
            print $2 "\t" $3
            found = 1
            exit
        }
        END {
            if (!found) {
                print "unclassified\tunclassified"
            }
        }
    ' eval/feature_tags.tsv
}

lookup_error_metadata() {
    local source_path="$1"
    awk -F '\t' -v key="$source_path" '
        NR > 1 && $1 == key {
            print $2 "\t" $3
            found = 1
            exit
        }
        END {
            if (!found) {
                print "unclassified\tunclassified"
            }
        }
    ' eval/error_classes.tsv
}

write_feature_verifier_summary() {
    local matrix_csv="$1"

    {
        echo "feature_tag,total,frontend_ok,verifier_ok"
        awk -F ',' '
        BEGIN {
            OFS = ","
        }
        NR == 1 { next }
        {
            split($4, tags, ";")
            seen_count = 0
            delete seen
            for (i in tags) {
                tag = tags[i]
                gsub(/^ +| +$/, "", tag)
                if (tag == "") {
                    continue
                }
                if (!(tag in seen)) {
                    seen[tag] = 1
                    ordered[++seen_count] = tag
                }
            }
            if (seen_count == 0) {
                ordered[++seen_count] = "unclassified"
            }
            for (i = 1; i <= seen_count; i++) {
                tag = ordered[i]
                total[tag]++
                if ($7 == "yes") {
                    frontend_ok[tag]++
                }
                if ($8 == "yes") {
                    verifier_ok[tag]++
                }
                delete seen[tag]
                delete ordered[i]
            }
        }
        END {
            for (tag in total) {
                print tag, total[tag], frontend_ok[tag] + 0, verifier_ok[tag] + 0
            }
        }
    ' "$matrix_csv" | sort
    }
}

write_error_class_summary() {
    local matrix_csv="$1"

    {
        echo "error_class,total,rejected,accepted"
        awk -F ',' '
        BEGIN {
            OFS = ","
        }
        NR == 1 { next }
        {
            total[$4]++
            if ($6 == "yes") {
                rejected[$4]++
            } else {
                accepted[$4]++
            }
        }
        END {
            for (klass in total) {
                print klass, total[klass], rejected[klass] + 0, accepted[klass] + 0
            }
        }
    ' "$matrix_csv" | sort
    }
}

lookup_runtime_case() {
    local source_path="$1"
    awk -F '\t' -v key="$source_path" '
        NR > 1 && $2 == key {
            print $1 "\t" $3 "\t" $4 "\t" $5
            found = 1
            exit
        }
        END {
            if (!found) {
                print "\t\t\t"
            }
        }
    ' eval/runtime_cases.tsv
}

lookup_runtime_baseline() {
    local source_path="$1"
    awk -F '\t' -v key="$source_path" '
        NR > 1 && $1 == key {
            print $2 "\t" $3 "\t" $4
            found = 1
            exit
        }
        END {
            if (!found) {
                print "\t\t"
            }
        }
    ' eval/runtime_baselines.tsv
}

observe_runtime_result() {
    local bin="$1"
    local mode="$2"
    local stdout_file="$3"
    local stderr_file="$4"
    local run_exit_code observed first_line

    run_exit_code="$(run_and_capture "$bin" "$stdout_file" "$stderr_file")"

    case "$mode" in
        exit)
            observed="exit=$run_exit_code"
            ;;
        checksum)
            if [ -s "$stderr_file" ]; then
                first_line="$(sed -n '1p' "$stderr_file")"
            else
                first_line="$(sed -n '1p' "$stdout_file")"
            fi
            case "$first_line" in
                checksum=*)
                    observed="$first_line"
                    ;;
                *)
                    observed="checksum=$first_line"
                    ;;
            esac
            ;;
        *)
            observed="unsupported_mode:$mode"
            ;;
    esac

    printf '%s\t%s\n' "$run_exit_code" "$observed"
}

write_feature_compilation_summary() {
    local csv="$1"
    local summary_csv="$2"
    local hotspots_csv="$3"

    python3 - "$csv" "$summary_csv" "$hotspots_csv" <<'PY'
import csv
import math
import sys

in_csv, summary_csv, hotspots_csv = sys.argv[1:4]

def read_rows(path):
    with open(path, newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))

rows = read_rows(in_csv)
numeric_rows = []
for row in rows:
    try:
        compile_ms = float(row["compile_ms"])
        fe_queries = float(row["frontend_smt_queries"])
        fe_ms = float(row["frontend_smt_ms"])
        binary_bytes = float(row["binary_bytes"])
    except ValueError:
        continue
    verify_ok = row["verify_status"] == "pass"
    verify_ms = float(row["verify_ms"]) if verify_ok else None
    ver_queries = float(row["verifier_smt_queries"]) if verify_ok else None
    ver_ms = float(row["verifier_smt_ms"]) if verify_ok else None
    numeric_rows.append(
        {
            **row,
            "compile_ms_num": compile_ms,
            "frontend_smt_queries_num": fe_queries,
            "frontend_smt_ms_num": fe_ms,
            "binary_bytes_num": binary_bytes,
            "verify_ms_num": verify_ms,
            "verifier_smt_queries_num": ver_queries,
            "verifier_smt_ms_num": ver_ms,
        }
    )

def avg(values):
    return sum(values) / len(values) if values else ""

compile_vals = [r["compile_ms_num"] for r in numeric_rows]
fe_query_vals = [r["frontend_smt_queries_num"] for r in numeric_rows]
fe_ms_vals = [r["frontend_smt_ms_num"] for r in numeric_rows]
bin_vals = [r["binary_bytes_num"] for r in numeric_rows]
verify_rows = [r for r in numeric_rows if r["verify_status"] == "pass"]
verify_vals = [r["verify_ms_num"] for r in verify_rows]
ver_query_vals = [r["verifier_smt_queries_num"] for r in verify_rows]
ver_ms_vals = [r["verifier_smt_ms_num"] for r in verify_rows]

with open(summary_csv, "w", newline="", encoding="utf-8") as fh:
    writer = csv.writer(fh)
    writer.writerow(
        [
            "suite",
            "program_count",
            "verify_pass_count",
            "mean_compile_ms",
            "mean_verify_ms",
            "mean_frontend_smt_queries",
            "mean_verifier_smt_queries",
            "mean_frontend_smt_ms",
            "mean_verifier_smt_ms",
            "mean_binary_bytes",
            "max_compile_program",
            "max_compile_ms",
            "max_verify_program",
            "max_verify_ms",
        ]
    )
    if numeric_rows:
        max_compile = max(numeric_rows, key=lambda r: r["compile_ms_num"])
        max_verify = max(verify_rows, key=lambda r: r["verify_ms_num"]) if verify_rows else None
        writer.writerow(
            [
                numeric_rows[0]["suite"],
                len(numeric_rows),
                len(verify_rows),
                f"{avg(compile_vals):.3f}",
                f"{avg(verify_vals):.3f}" if verify_vals else "",
                f"{avg(fe_query_vals):.1f}",
                f"{avg(ver_query_vals):.1f}" if ver_query_vals else "",
                f"{avg(fe_ms_vals):.3f}",
                f"{avg(ver_ms_vals):.3f}" if ver_ms_vals else "",
                f"{avg(bin_vals):.1f}",
                max_compile["file"],
                f"{max_compile['compile_ms_num']:.3f}",
                max_verify["file"] if max_verify else "",
                f"{max_verify['verify_ms_num']:.3f}" if max_verify else "",
            ]
        )

def classify_driver(row):
    fe_ms = row["frontend_smt_ms_num"]
    ver_ms = row["verifier_smt_ms_num"] or 0.0
    name = row["file"]
    if "quantifier" in name:
        return "quantifiers"
    if "bubble_sort" in name or "binary_search" in name or "sortedness" in name:
        return "invariants_and_sortedness"
    if "array" in name:
        return "array_proof_obligations"
    if ver_ms > fe_ms:
        return "dtal_verification"
    if fe_ms > 0:
        return "frontend_smt"
    return "code_generation"

hotspots = sorted(
    verify_rows,
    key=lambda r: (r["verify_ms_num"] if r["verify_ms_num"] is not None else -1.0),
    reverse=True,
)

with open(hotspots_csv, "w", newline="", encoding="utf-8") as fh:
    writer = csv.writer(fh)
    writer.writerow(
        [
            "suite",
            "file",
            "compile_ms",
            "verify_ms",
            "frontend_smt_queries",
            "frontend_smt_ms",
            "verifier_smt_queries",
            "verifier_smt_ms",
            "dominant_cost_driver",
        ]
    )
    for row in hotspots:
        writer.writerow(
            [
                row["suite"],
                row["file"],
                f"{row['compile_ms_num']:.3f}",
                f"{row['verify_ms_num']:.3f}",
                int(row["frontend_smt_queries_num"]),
                f"{row['frontend_smt_ms_num']:.3f}",
                int(row["verifier_smt_queries_num"]),
                f"{row['verifier_smt_ms_num']:.3f}",
                classify_driver(row),
            ]
        )
PY
}

write_timing_summary() {
    local csv="$1"
    local summary_csv="$2"

    python3 - "$csv" "$summary_csv" <<'PY'
import csv
import sys

in_csv, out_csv = sys.argv[1:3]
rows = list(csv.DictReader(open(in_csv, newline="", encoding="utf-8")))
by_key = {}
for row in rows:
    by_key.setdefault((row["suite"], row["file"]), {})[row["variant"]] = row

with open(out_csv, "w", newline="", encoding="utf-8") as fh:
    writer = csv.writer(fh)
    writer.writerow(
        [
            "suite",
            "file",
            "compile_only_status",
            "compile_only_mean_s",
            "compile_only_stddev_s",
            "compile_plus_verify_status",
            "compile_plus_verify_mean_s",
            "compile_plus_verify_stddev_s",
            "verify_overhead_ratio",
        ]
    )
    for (suite, file_name) in sorted(by_key):
        compile_row = by_key[(suite, file_name)].get("compile_only", {})
        verify_row = by_key[(suite, file_name)].get("compile_plus_verify", {})
        ratio = ""
        try:
            if compile_row.get("status") == "pass" and verify_row.get("status") == "pass":
                compile_mean = float(compile_row["mean_s"])
                verify_mean = float(verify_row["mean_s"])
                ratio = f"{verify_mean / compile_mean:.3f}" if compile_mean > 0 else ""
        except (KeyError, ValueError, ZeroDivisionError):
            ratio = ""
        writer.writerow(
            [
                suite,
                file_name,
                compile_row.get("status", ""),
                compile_row.get("mean_s", ""),
                compile_row.get("stddev_s", ""),
                verify_row.get("status", ""),
                verify_row.get("mean_s", ""),
                verify_row.get("stddev_s", ""),
                ratio,
            ]
        )
PY
}

write_runtime_timing_summary() {
    local csv="$1"
    local summary_csv="$2"

    python3 - "$csv" "$summary_csv" <<'PY'
import csv
import sys

in_csv, out_csv = sys.argv[1:3]
rows = list(csv.DictReader(open(in_csv, newline="", encoding="utf-8")))
by_benchmark = {}
for row in rows:
    by_benchmark.setdefault(row["benchmark"], {})[row["implementation"]] = row

with open(out_csv, "w", newline="", encoding="utf-8") as fh:
    writer = csv.writer(fh)
    writer.writerow(
        [
            "benchmark",
            "expected",
            "veritas_status",
            "veritas_mean_s",
            "gcc_o2_status",
            "gcc_o2_mean_s",
            "gcc_o3_status",
            "gcc_o3_mean_s",
            "veritas_vs_gcc_o2",
            "veritas_vs_gcc_o3",
        ]
    )
    for benchmark in sorted(by_benchmark):
        group = by_benchmark[benchmark]
        veritas = group.get("veritas", {})
        gcc_o2 = group.get("gcc_o2", {})
        gcc_o3 = group.get("gcc_o3", {})

        def ratio(lhs, rhs):
            try:
                lhs_v = float(lhs)
                rhs_v = float(rhs)
                return f"{lhs_v / rhs_v:.3f}" if rhs_v > 0 else ""
            except (TypeError, ValueError):
                return ""

        writer.writerow(
            [
                benchmark,
                veritas.get("expected", gcc_o2.get("expected", gcc_o3.get("expected", ""))),
                veritas.get("status", ""),
                veritas.get("mean_s", ""),
                gcc_o2.get("status", ""),
                gcc_o2.get("mean_s", ""),
                gcc_o3.get("status", ""),
                gcc_o3.get("mean_s", ""),
                ratio(veritas.get("mean_s"), gcc_o2.get("mean_s")),
                ratio(veritas.get("mean_s"), gcc_o3.get("mean_s")),
            ]
        )
PY
}

write_binary_size_summary() {
    local csv="$1"
    local text_csv="$2"
    local summary_csv="$3"

    python3 - "$csv" "$text_csv" "$summary_csv" <<'PY'
import csv
import sys

bin_csv, text_csv, out_csv = sys.argv[1:4]
bin_rows = {row["program"]: row for row in csv.DictReader(open(bin_csv, newline="", encoding="utf-8"))}
text_rows = {row["program"]: row for row in csv.DictReader(open(text_csv, newline="", encoding="utf-8"))}

with open(out_csv, "w", newline="", encoding="utf-8") as fh:
    writer = csv.writer(fh)
    writer.writerow(
        [
            "program",
            "veritas_elf_bytes",
            "gcc_o0_elf_bytes",
            "gcc_o2_elf_bytes",
            "veritas_text_bytes",
            "gcc_o0_text_bytes",
            "gcc_o2_text_bytes",
            "elf_vs_gcc_o2_ratio",
            "text_vs_gcc_o2_ratio",
            "fairness_note",
        ]
    )
    for program in sorted(bin_rows):
        bin_row = bin_rows[program]
        text_row = text_rows.get(program, {})

        def ratio(lhs_key, rhs_key, row_a, row_b):
            try:
                lhs = float(row_a[lhs_key])
                rhs = float(row_b[rhs_key])
                return f"{lhs / rhs:.3f}" if rhs > 0 else ""
            except (KeyError, ValueError, ZeroDivisionError):
                return ""

        writer.writerow(
            [
                program,
                bin_row.get("veritas_bytes", ""),
                bin_row.get("gcc_O0_bytes", ""),
                bin_row.get("gcc_O2_bytes", ""),
                text_row.get("veritas_text_bytes", ""),
                text_row.get("gcc_O0_text_bytes", ""),
                text_row.get("gcc_O2_text_bytes", ""),
                ratio("veritas_bytes", "gcc_O2_bytes", bin_row, bin_row),
                ratio("veritas_text_bytes", "gcc_O2_text_bytes", text_row, text_row),
                "ELF totals are not perfectly comparable because Veritas emits freestanding binaries while GCC links hosted startup/runtime code.",
            ]
        )
PY
}

write_tcb_summary() {
    local csv="$1"
    local summary_csv="$2"

    python3 - "$csv" "$summary_csv" <<'PY'
import csv
import sys

in_csv, out_csv = sys.argv[1:3]
rows = list(csv.DictReader(open(in_csv, newline="", encoding="utf-8")))
by_component = {row["component"]: row for row in rows}
total = float(by_component["total"]["lines"])
trusted = float(by_component["verifier_tcb"]["lines"])
untrusted = total - trusted

with open(out_csv, "w", newline="", encoding="utf-8") as fh:
    writer = csv.writer(fh)
    writer.writerow(
        [
            "trusted_component",
            "trusted_files",
            "trusted_lines",
            "total_lines",
            "trusted_ratio",
            "untrusted_lines",
            "untrusted_ratio",
        ]
    )
    writer.writerow(
        [
            "verifier_tcb",
            by_component["verifier_tcb"]["files"],
            by_component["verifier_tcb"]["lines"],
            by_component["total"]["lines"],
            f"{trusted / total:.3f}",
            f"{untrusted:.0f}",
            f"{untrusted / total:.3f}",
        ]
    )
PY
}

append_hyperfine_summary() {
    local export_json="$1"
    local suite="$2"
    local file="$3"
    local variant="$4"
    local status="$5"
    local csv="$6"

    local mean stddev min max user system
    mean="$(json_field "$export_json" "results.0.mean")"
    stddev="$(json_field "$export_json" "results.0.stddev")"
    min="$(json_field "$export_json" "results.0.min")"
    max="$(json_field "$export_json" "results.0.max")"
    user="$(json_field "$export_json" "results.0.user")"
    system="$(json_field "$export_json" "results.0.system")"

    printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
        "$suite" "$file" "$variant" "$status" "$mean" "$stddev" "$min" "$max" "$user" "$system" >> "$csv"
}

ensure_release_build() {
    log "Building veritas (release)..."
    cargo build --release -q
}

run_compile_bench() {
    log "=== Compilation Benchmarks ==="

    local suite_manifest="eval/suites/feature_suite.txt"
    local suite_name
    suite_name="$(suite_stem "$suite_manifest")"
    local suite_file="$TMPDIR/${suite_name}.txt"
    resolve_suite "$suite_manifest" "resolved_${suite_name}.txt" > "$suite_file"

    local raw_jsonl="$RAW_DIR/compilation_${suite_name}.jsonl"
    local csv="$RESULTS_DIR/compilation.csv"
    local summary_csv="$RESULTS_DIR/feature_compilation_summary.csv"
    local hotspots_csv="$RESULTS_DIR/feature_cost_hotspots.csv"
    : > "$raw_jsonl"
    echo "suite,file,compile_ms,verify_ms,frontend_smt_queries,frontend_smt_ms,verifier_smt_queries,verifier_smt_ms,binary_bytes,verify_status" > "$csv"

    while IFS= read -r f; do
        local name
        name="$(basename "$f" .veri)"
        log "  $name..."

        local json json_v compile_ms verify_ms fe_queries fe_ms ver_queries ver_ms binary verify_status
        json="$("$VERITAS" "$f" -o "$TMPDIR/out" --bench 2>/dev/null || echo '{"error":true}')"
        printf '%s\n' "$json" >> "$raw_jsonl"
        if echo "$json" | grep -q '"error"'; then
            log "    SKIP (compile error)"
            continue
        fi

        compile_ms="$(echo "$json" | jfield compile_ms)"
        fe_queries="$(echo "$json" | jfield frontend_smt_queries)"
        fe_ms="$(echo "$json" | jfield frontend_smt_ms)"
        binary="$(echo "$json" | jfield binary_bytes)"

        json_v="$("$VERITAS" "$f" --verify -o "$TMPDIR/out_v" --bench 2>/dev/null || echo '{"verify_failed":true}')"
        printf '%s\n' "$json_v" >> "$raw_jsonl"

        if echo "$json_v" | grep -q '"verify_failed"'; then
            verify_ms="FAIL"
            ver_queries="FAIL"
            ver_ms="FAIL"
            verify_status="fail"
        else
            verify_ms="$(echo "$json_v" | jfield verify_ms)"
            ver_queries="$(echo "$json_v" | jfield verifier_smt_queries)"
            ver_ms="$(echo "$json_v" | jfield verifier_smt_ms)"
            verify_status="pass"
        fi

        printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
            "$suite_name" "$name" "$compile_ms" "$verify_ms" "$fe_queries" "$fe_ms" \
            "$ver_queries" "$ver_ms" "$binary" "$verify_status" >> "$csv"
    done < "$suite_file"

    write_feature_compilation_summary "$csv" "$summary_csv" "$hotspots_csv"

    log "  -> $csv"
    log "  -> $summary_csv"
    log "  -> $hotspots_csv"
}

run_timing_bench() {
    log "=== Hyperfine Timing Benchmarks ==="

    if ! command_exists hyperfine; then
        log "hyperfine is required for --timing"
        exit 1
    fi

    local suite_manifest="eval/suites/verification_stress_suite.txt"
    local suite_name
    suite_name="$(suite_stem "$suite_manifest")"
    local suite_file="$TMPDIR/${suite_name}.txt"
    resolve_suite "$suite_manifest" "resolved_${suite_name}.txt" > "$suite_file"

    local csv="$RESULTS_DIR/timing.csv"
    local summary_csv="$RESULTS_DIR/verification_stress_timing_summary.csv"
    local raw_dir="$RAW_DIR/hyperfine"
    mkdir -p "$raw_dir"
    echo "suite,file,variant,status,mean_s,stddev_s,min_s,max_s,user_s,system_s" > "$csv"

    while IFS= read -r f; do
        local name compile_json verify_json cmd
        name="$(basename "$f" .veri)"
        log "  $name (compile)..."

        compile_json="$raw_dir/${name}_compile.json"
        cmd="$VERITAS $f -o $TMPDIR/timing_out --bench"
        if ! $VERITAS "$f" -o "$TMPDIR/timing_probe" --bench >/dev/null 2>&1; then
            log "    compile probe failed; recording failure"
            printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
                "$suite_name" "$name" "compile" "fail" "" "" "" "" "" "" >> "$csv"
            continue
        fi
        hyperfine --warmup "$WARMUP" --runs "$RUNS" \
            --export-json "$compile_json" \
            "$cmd" > "$LOGS_DIR/hyperfine_${name}_compile.log"
        append_hyperfine_summary "$compile_json" "$suite_name" "$name" "compile_only" "pass" "$csv"

        log "  $name (compile+verify)..."
        verify_json="$raw_dir/${name}_verify.json"
        cmd="$VERITAS $f --verify -o $TMPDIR/timing_out_v --bench"
        if ! $VERITAS "$f" --verify -o "$TMPDIR/timing_probe_v" --bench >/dev/null 2>&1; then
            log "    verify probe failed; recording failure"
            printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
                "$suite_name" "$name" "compile_plus_verify" "fail" "" "" "" "" "" "" >> "$csv"
            continue
        fi
        hyperfine --warmup "$WARMUP" --runs "$RUNS" \
            --export-json "$verify_json" \
            "$cmd" > "$LOGS_DIR/hyperfine_${name}_verify.log"
        append_hyperfine_summary "$verify_json" "$suite_name" "$name" "compile_plus_verify" "pass" "$csv"
    done < "$suite_file"

    write_timing_summary "$csv" "$summary_csv"

    log "  -> $csv"
    log "  -> $summary_csv"
}

run_error_bench() {
    log "=== Error Rejection Tests ==="

    local suite_manifest="eval/suites/negative_suite.txt"
    local suite_name
    suite_name="$(suite_stem "$suite_manifest")"
    local suite_file="$TMPDIR/${suite_name}.txt"
    resolve_suite "$suite_manifest" "resolved_${suite_name}.txt" > "$suite_file"

    local csv="$RESULTS_DIR/error_rejection.csv"
    local matrix_csv="$RESULTS_DIR/error_rejection_matrix.csv"
    local summary_csv="$RESULTS_DIR/error_rejection_summary_by_class.csv"
    echo "suite,file,expected_error,rejected,error_message" > "$csv"
    echo "suite,file,source,error_class,error_tags,rejected,error_message" > "$matrix_csv"

    while IFS= read -r f; do
        local name rejected first_line error_class error_tags error_log
        name="$(basename "$f" .veri)"
        error_log="$LOGS_DIR/${name}.frontend_error.log"
        "$VERITAS" "$f" -o "$TMPDIR/err_out" > "$error_log" 2>&1 || true
        IFS=$'\t' read -r error_class error_tags <<< "$(lookup_error_metadata "$f")"
        error_tags="$(sanitize_csv_field "${error_tags//,/;}")"

        if [ -f "$TMPDIR/err_out" ] && [ -x "$TMPDIR/err_out" ]; then
            rejected="no"
        else
            rejected="yes"
        fi
        rm -f "$TMPDIR/err_out"

        first_line="$(extract_diagnostic_summary "$error_log" "$f")"
        printf '%s,%s,%s,%s,%s\n' \
            "$suite_name" "$name" "type_error" "$rejected" "$first_line" >> "$csv"
        printf '%s,%s,%s,%s,%s,%s,%s\n' \
            "$suite_name" "$name" "$f" "$error_class" "$error_tags" "$rejected" "$first_line" >> "$matrix_csv"
    done < "$suite_file"

    write_error_class_summary "$matrix_csv" "$summary_csv" > "$summary_csv"

    log "  -> $csv"
    log "  -> $matrix_csv"
    log "  -> $summary_csv"
}

run_binary_size_bench() {
    log "=== Binary Size Comparison ==="

    local csv="$RESULTS_DIR/binary_size.csv"
    local text_csv="$RESULTS_DIR/text_size.csv"
    local summary_csv="$RESULTS_DIR/binary_size_summary.csv"
    echo "program,veritas_bytes,gcc_O0_bytes,gcc_O2_bytes" > "$csv"
    echo "program,veritas_text_bytes,gcc_O0_text_bytes,gcc_O2_text_bytes" > "$text_csv"

    declare -A c_map
    c_map[01_simple]="simple_arithmetic"
    c_map[07_function_calls]="function_calls"
    c_map[14_for_loops]="for_loops"
    c_map[22_bubble_sort]="bubble_sort_feature"

    for veri_name in "${!c_map[@]}"; do
        local c_name veri_file c_file veri_size gcc_O0_size gcc_O2_size veri_text gcc_O0_text gcc_O2_text
        c_name="${c_map[$veri_name]}"
        veri_file="src/examples/${veri_name}.veri"
        c_file="eval/c_equivalents/${c_name}.c"

        [ -f "$veri_file" ] || continue
        [ -f "$c_file" ] || continue

        log "  $veri_name..."

        "$VERITAS" "$veri_file" -o "$TMPDIR/veri_bin" --bench >/dev/null 2>&1 || continue
        veri_size="$(stat -c%s "$TMPDIR/veri_bin" 2>/dev/null || echo "ERR")"

        gcc -O0 -o "$TMPDIR/gcc_O0" "$c_file" 2>/dev/null || continue
        gcc_O0_size="$(stat -c%s "$TMPDIR/gcc_O0")"

        gcc -O2 -o "$TMPDIR/gcc_O2" "$c_file" 2>/dev/null || continue
        gcc_O2_size="$(stat -c%s "$TMPDIR/gcc_O2")"

        printf '%s,%s,%s,%s\n' \
            "$veri_name" "$veri_size" "$gcc_O0_size" "$gcc_O2_size" >> "$csv"

        veri_text="$veri_size"
        gcc_O0_text="$(objdump -h "$TMPDIR/gcc_O0" | awk '/.text/{print strtonum("0x"$3)}')"
        gcc_O2_text="$(objdump -h "$TMPDIR/gcc_O2" | awk '/.text/{print strtonum("0x"$3)}')"
        printf '%s,%s,%s,%s\n' \
            "$veri_name" "$veri_text" "$gcc_O0_text" "$gcc_O2_text" >> "$text_csv"
    done

    write_binary_size_summary "$csv" "$text_csv" "$summary_csv"

    log "  -> $csv"
    log "  -> $text_csv"
    log "  -> $summary_csv"
}

run_runtime_bench() {
    log "=== Runtime Correctness Comparison ==="

    local cases_file="eval/runtime_cases.tsv"
    local csv="$RESULTS_DIR/runtime_correctness.csv"
    local timing_csv="$RESULTS_DIR/runtime_timing.csv"
    local timing_summary_csv="$RESULTS_DIR/algorithm_runtime_summary.csv"
    local raw_dir="$RAW_DIR/runtime"
    local raw_hyperfine_dir="$RAW_DIR/runtime_hyperfine"
    mkdir -p "$raw_dir"
    mkdir -p "$raw_hyperfine_dir"
    echo "case_id,source,mode,expected,observed,compile_status,run_exit_code,status,stdout_file,stderr_file,note" > "$csv"

    while IFS=$'\t' read -r case_id source mode expected note; do
        local bin stdout_file stderr_file compile_status run_exit_code observed status

        [ -n "$case_id" ] || continue
        case "$case_id" in
            \#*) continue ;;
        esac

        log "  $case_id..."

        if [ ! -f "$source" ]; then
            printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
                "$case_id" "$source" "$mode" "$expected" "missing_source" "missing" "N/A" "FAIL" "" "" "$note" >> "$csv"
            continue
        fi

        bin="$TMPDIR/${case_id}_veri"
        stdout_file="$raw_dir/${case_id}.stdout"
        stderr_file="$raw_dir/${case_id}.stderr"

        if "$VERITAS" "$source" -o "$bin" >/dev/null 2>&1; then
            compile_status="ok"
        else
            compile_status="compile_fail"
            printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
                "$case_id" "$source" "$mode" "$expected" "compile_failed" "$compile_status" "N/A" "FAIL" "" "" "$note" >> "$csv"
            continue
        fi

        IFS=$'\t' read -r run_exit_code observed <<< "$(observe_runtime_result "$bin" "$mode" "$stdout_file" "$stderr_file")"

        status="FAIL"
        case "$mode" in
            exit)
                if [ "$observed" = "$expected" ]; then
                    status="PASS"
                fi
                ;;
            checksum)
                if [ "$observed" = "$expected" ] && [ "$run_exit_code" -ge 0 ]; then
                    status="PASS"
                fi
                ;;
        esac

        printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
            "$case_id" "$source" "$mode" "$expected" "$observed" "$compile_status" "$run_exit_code" "$status" "$stdout_file" "$stderr_file" "$note" >> "$csv"
    done < "$cases_file"

    log "  -> $csv"

    if ! command_exists hyperfine; then
        return
    fi

    echo "benchmark,source,implementation,compiler,mode,expected,correctness_status,status,mean_s,stddev_s,min_s,max_s,user_s,system_s" > "$timing_csv"

    local suite_manifest="eval/suites/algorithm_suite.txt"
    local suite_name
    suite_name="$(suite_stem "$suite_manifest")"
    local suite_file="$TMPDIR/${suite_name}.txt"
    resolve_suite "$suite_manifest" "resolved_${suite_name}.txt" > "$suite_file"

    while IFS= read -r f; do
        local case_id mode expected note benchmark_label c_source c_flags
        local veritas_bin baseline_o2 baseline_o3
        local stdout_file stderr_file run_exit_code observed correctness_status
        local hf_json

        IFS=$'\t' read -r case_id mode expected note <<< "$(lookup_runtime_case "$f")"
        IFS=$'\t' read -r benchmark_label c_source c_flags <<< "$(lookup_runtime_baseline "$f")"
        [ -n "$case_id" ] || continue
        [ -n "$benchmark_label" ] || continue

        log "  ${benchmark_label} (runtime timing)..."

        veritas_bin="$TMPDIR/${benchmark_label}_veritas"
        if ! "$VERITAS" "$f" -o "$veritas_bin" >/dev/null 2>&1; then
            printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
                "$benchmark_label" "$f" "veritas" "veritas_default" "$mode" "$expected" "compile_fail" "fail" "" "" "" "" "" "" >> "$timing_csv"
            continue
        fi

        stdout_file="$raw_dir/${benchmark_label}_veritas.stdout"
        stderr_file="$raw_dir/${benchmark_label}_veritas.stderr"
        IFS=$'\t' read -r run_exit_code observed <<< "$(observe_runtime_result "$veritas_bin" "$mode" "$stdout_file" "$stderr_file")"
        correctness_status="mismatch"
        if [ "$observed" = "$expected" ] && { [ "$mode" = "exit" ] || [ "$run_exit_code" -ge 0 ]; }; then
            correctness_status="pass"
        fi
        if [ "$correctness_status" = "pass" ]; then
            hf_json="$raw_hyperfine_dir/${benchmark_label}_veritas.json"
            hyperfine --shell=none --ignore-failure --warmup "$WARMUP" --runs "$RUNS" \
                --export-json "$hf_json" \
                "$veritas_bin" > "$LOGS_DIR/hyperfine_${benchmark_label}_veritas.log"
            append_hyperfine_summary "$hf_json" "$benchmark_label" "$f" "veritas" "pass" "$TMPDIR/runtime_hf.tmp"
            tail -n 1 "$TMPDIR/runtime_hf.tmp" | awk -F ',' -v mode="$mode" -v expected="$expected" -v correctness="$correctness_status" \
                'BEGIN { OFS="," } { print $1,$2,"veritas","veritas_default",mode,expected,correctness,$4,$5,$6,$7,$8,$9,$10 }' >> "$timing_csv"
            : > "$TMPDIR/runtime_hf.tmp"
        else
            printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
                "$benchmark_label" "$f" "veritas" "veritas_default" "$mode" "$expected" "$correctness_status" "fail" "" "" "" "" "" "" >> "$timing_csv"
        fi

        if [ ! -f "$c_source" ]; then
            continue
        fi

        baseline_o2="$TMPDIR/${benchmark_label}_gcc_o2"
        baseline_o3="$TMPDIR/${benchmark_label}_gcc_o3"

        if gcc -O2 $c_flags -o "$baseline_o2" "$c_source" >/dev/null 2>&1; then
            stdout_file="$raw_dir/${benchmark_label}_gcc_o2.stdout"
            stderr_file="$raw_dir/${benchmark_label}_gcc_o2.stderr"
            IFS=$'\t' read -r run_exit_code observed <<< "$(observe_runtime_result "$baseline_o2" "$mode" "$stdout_file" "$stderr_file")"
            correctness_status="mismatch"
            if [ "$observed" = "$expected" ] && { [ "$mode" = "exit" ] || [ "$run_exit_code" -ge 0 ]; }; then
                correctness_status="pass"
            fi
            if [ "$correctness_status" = "pass" ]; then
                hf_json="$raw_hyperfine_dir/${benchmark_label}_gcc_o2.json"
                hyperfine --shell=none --ignore-failure --warmup "$WARMUP" --runs "$RUNS" \
                    --export-json "$hf_json" \
                    "$baseline_o2" > "$LOGS_DIR/hyperfine_${benchmark_label}_gcc_o2.log"
                append_hyperfine_summary "$hf_json" "$benchmark_label" "$f" "gcc_o2" "pass" "$TMPDIR/runtime_hf.tmp"
                tail -n 1 "$TMPDIR/runtime_hf.tmp" | awk -F ',' -v mode="$mode" -v expected="$expected" -v correctness="$correctness_status" \
                    'BEGIN { OFS="," } { print $1,$2,"gcc_o2","gcc -O2",mode,expected,correctness,$4,$5,$6,$7,$8,$9,$10 }' >> "$timing_csv"
                : > "$TMPDIR/runtime_hf.tmp"
            else
                printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
                    "$benchmark_label" "$f" "gcc_o2" "gcc -O2" "$mode" "$expected" "$correctness_status" "fail" "" "" "" "" "" "" >> "$timing_csv"
            fi
        fi

        if gcc -O3 $c_flags -o "$baseline_o3" "$c_source" >/dev/null 2>&1; then
            stdout_file="$raw_dir/${benchmark_label}_gcc_o3.stdout"
            stderr_file="$raw_dir/${benchmark_label}_gcc_o3.stderr"
            IFS=$'\t' read -r run_exit_code observed <<< "$(observe_runtime_result "$baseline_o3" "$mode" "$stdout_file" "$stderr_file")"
            correctness_status="mismatch"
            if [ "$observed" = "$expected" ] && { [ "$mode" = "exit" ] || [ "$run_exit_code" -ge 0 ]; }; then
                correctness_status="pass"
            fi
            if [ "$correctness_status" = "pass" ]; then
                hf_json="$raw_hyperfine_dir/${benchmark_label}_gcc_o3.json"
                hyperfine --shell=none --ignore-failure --warmup "$WARMUP" --runs "$RUNS" \
                    --export-json "$hf_json" \
                    "$baseline_o3" > "$LOGS_DIR/hyperfine_${benchmark_label}_gcc_o3.log"
                append_hyperfine_summary "$hf_json" "$benchmark_label" "$f" "gcc_o3" "pass" "$TMPDIR/runtime_hf.tmp"
                tail -n 1 "$TMPDIR/runtime_hf.tmp" | awk -F ',' -v mode="$mode" -v expected="$expected" -v correctness="$correctness_status" \
                    'BEGIN { OFS="," } { print $1,$2,"gcc_o3","gcc -O3",mode,expected,correctness,$4,$5,$6,$7,$8,$9,$10 }' >> "$timing_csv"
                : > "$TMPDIR/runtime_hf.tmp"
            else
                printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
                    "$benchmark_label" "$f" "gcc_o3" "gcc -O3" "$mode" "$expected" "$correctness_status" "fail" "" "" "" "" "" "" >> "$timing_csv"
            fi
        fi
    done < "$suite_file"

    write_runtime_timing_summary "$timing_csv" "$timing_summary_csv"
    log "  -> $timing_csv"
    log "  -> $timing_summary_csv"
}

run_verifier_matrix() {
    log "=== Verifier Acceptance Matrix ==="

    local suite_manifest="eval/suites/feature_suite.txt"
    local suite_name
    suite_name="$(suite_stem "$suite_manifest")"
    local suite_file="$TMPDIR/${suite_name}.txt"
    resolve_suite "$suite_manifest" "resolved_${suite_name}.txt" > "$suite_file"

    local csv="$RESULTS_DIR/verifier_matrix.csv"
    local matrix_csv="$RESULTS_DIR/feature_verifier_matrix.csv"
    local summary_csv="$RESULTS_DIR/feature_verifier_summary_by_tag.csv"
    echo "suite,file,frontend_ok,verifier_ok,frontend_smt_queries,verifier_smt_queries" > "$csv"
    echo "suite,file,source,feature_tags,narrative_role,agreement_status,frontend_ok,verifier_ok,frontend_smt_queries,verifier_smt_queries,frontend_diagnostic,verifier_diagnostic" > "$matrix_csv"

    while IFS= read -r f; do
        local name fe_ok fe_queries ver_ok ver_queries feature_tags narrative_role
        local fe_json fe_stderr ver_json ver_stderr fe_diag ver_diag agreement_status
        name="$(basename "$f" .veri)"
        log "  $name..."
        IFS=$'\t' read -r feature_tags narrative_role <<< "$(lookup_feature_metadata "$f")"
        feature_tags="$(sanitize_csv_field "${feature_tags//,/;}")"
        narrative_role="$(sanitize_csv_field "$narrative_role")"
        fe_json="$TMPDIR/${name}.frontend.json"
        fe_stderr="$LOGS_DIR/${name}.frontend_verify.log"
        ver_json="$TMPDIR/${name}.verifier.json"
        ver_stderr="$LOGS_DIR/${name}.physical_verify.log"

        if "$VERITAS" "$f" -o "$TMPDIR/vm_out" --bench > "$fe_json" 2> "$fe_stderr"; then
            fe_ok="yes"
            fe_queries="$(jfield_file "$fe_json" frontend_smt_queries)"
            fe_diag=""
        else
            fe_ok="no"
            fe_queries="N/A"
            fe_diag="$(extract_diagnostic_summary "$fe_stderr" "$f")"
        fi

        if "$VERITAS" "$f" --verify -o "$TMPDIR/vm_out_v" --bench > "$ver_json" 2> "$ver_stderr"; then
            ver_ok="yes"
            ver_queries="$(jfield_file "$ver_json" verifier_smt_queries)"
            ver_diag=""
        else
            ver_ok="no"
            ver_queries="N/A"
            ver_diag="$(extract_diagnostic_summary "$ver_stderr" "$f")"
        fi

        if [ "$fe_ok" = "yes" ] && [ "$ver_ok" = "yes" ]; then
            agreement_status="both_pass"
        elif [ "$fe_ok" = "no" ] && [ "$ver_ok" = "no" ]; then
            agreement_status="both_reject"
        elif [ "$fe_ok" = "no" ]; then
            agreement_status="frontend_reject"
        else
            agreement_status="verifier_reject"
        fi

        printf '%s,%s,%s,%s,%s,%s\n' \
            "$suite_name" "$name" "$fe_ok" "$ver_ok" "$fe_queries" "$ver_queries" >> "$csv"
        printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
            "$suite_name" "$name" "$f" "$feature_tags" "$narrative_role" "$agreement_status" "$fe_ok" "$ver_ok" "$fe_queries" "$ver_queries" "$fe_diag" "$ver_diag" >> "$matrix_csv"
    done < "$suite_file"

    write_feature_verifier_summary "$matrix_csv" "$summary_csv" > "$summary_csv"

    log "  -> $csv"
    log "  -> $matrix_csv"
    log "  -> $summary_csv"
}

run_loc_analysis() {
    log "=== Lines of Code / TCB Analysis ==="

    {
        echo "component,files,lines"

        fe_lines="$(find src/frontend -name '*.rs' -exec cat {} + | wc -l)"
        fe_files="$(find src/frontend -name '*.rs' | wc -l)"
        echo "frontend,$fe_files,$fe_lines"

        be_lines="$(find src/backend -name '*.rs' -exec cat {} + | wc -l)"
        be_files="$(find src/backend -name '*.rs' | wc -l)"
        echo "backend,$be_files,$be_lines"

        ver_lines="$(find src/verifier -name '*.rs' -exec cat {} + | wc -l)"
        ver_files="$(find src/verifier -name '*.rs' | wc -l)"
        echo "verifier_tcb,$ver_files,$ver_lines"

        com_lines="$(find src/common -name '*.rs' -exec cat {} + | wc -l)"
        com_files="$(find src/common -name '*.rs' | wc -l)"
        echo "common,$com_files,$com_lines"

        pipe_lines="$(cat src/pipeline.rs src/main.rs src/lib.rs 2>/dev/null | wc -l)"
        echo "pipeline,3,$pipe_lines"

        total_lines="$(find src -name '*.rs' -exec cat {} + | wc -l)"
        total_files="$(find src -name '*.rs' | wc -l)"
        echo "total,$total_files,$total_lines"
    } > "$RESULTS_DIR/loc.csv"

    write_tcb_summary "$RESULTS_DIR/loc.csv" "$RESULTS_DIR/tcb_summary.csv"

    log "  -> $RESULTS_DIR/loc.csv"
    log "  -> $RESULTS_DIR/tcb_summary.csv"
}

main() {
    write_invocation_log "$@"
    ensure_release_build

    case "$MODE" in
        --all)
            run_loc_analysis
            run_compile_bench
            run_timing_bench
            run_verifier_matrix
            run_error_bench
            run_binary_size_bench
            run_runtime_bench
            ;;
        --compile)      run_compile_bench ;;
        --timing)       run_timing_bench ;;
        --verify)       run_verifier_matrix ;;
        --errors)       run_error_bench ;;
        --binary-size)  run_binary_size_bench ;;
        --runtime)      run_runtime_bench ;;
        --loc)          run_loc_analysis ;;
        *)
            log "Usage: $0 [--all|--compile|--timing|--verify|--errors|--binary-size|--runtime|--loc]"
            exit 1
            ;;
    esac

    log ""
    log "Done. Results in $RUN_DIR/"
}

main "$@"
