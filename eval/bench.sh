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

    log "  -> $csv"
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
        append_hyperfine_summary "$compile_json" "$suite_name" "$name" "compile" "pass" "$csv"

        log "  $name (compile+verify)..."
        verify_json="$raw_dir/${name}_verify.json"
        cmd="$VERITAS $f --verify -o $TMPDIR/timing_out_v --bench"
        if ! $VERITAS "$f" --verify -o "$TMPDIR/timing_probe_v" --bench >/dev/null 2>&1; then
            log "    verify probe failed; recording failure"
            printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
                "$suite_name" "$name" "verify" "fail" "" "" "" "" "" "" >> "$csv"
            continue
        fi
        hyperfine --warmup "$WARMUP" --runs "$RUNS" \
            --export-json "$verify_json" \
            "$cmd" > "$LOGS_DIR/hyperfine_${name}_verify.log"
        append_hyperfine_summary "$verify_json" "$suite_name" "$name" "verify" "pass" "$csv"
    done < "$suite_file"

    log "  -> $csv"
}

run_error_bench() {
    log "=== Error Rejection Tests ==="

    local suite_manifest="eval/suites/negative_suite.txt"
    local suite_name
    suite_name="$(suite_stem "$suite_manifest")"
    local suite_file="$TMPDIR/${suite_name}.txt"
    resolve_suite "$suite_manifest" "resolved_${suite_name}.txt" > "$suite_file"

    local csv="$RESULTS_DIR/error_rejection.csv"
    echo "suite,file,expected_error,rejected,error_message" > "$csv"

    while IFS= read -r f; do
        local name error_output rejected first_line
        name="$(basename "$f" .veri)"
        error_output="$("$VERITAS" "$f" -o "$TMPDIR/err_out" 2>&1 || true)"

        if [ -f "$TMPDIR/err_out" ] && [ -x "$TMPDIR/err_out" ]; then
            rejected="no"
        else
            rejected="yes"
        fi
        rm -f "$TMPDIR/err_out"

        first_line="$(echo "$error_output" | grep -i -m1 'error\|FAILED' | head -c 160 | tr ',' ';' | tr '"' "'")"
        printf '%s,%s,%s,%s,%s\n' \
            "$suite_name" "$name" "type_error" "$rejected" "$first_line" >> "$csv"
    done < "$suite_file"

    log "  -> $csv"
}

run_binary_size_bench() {
    log "=== Binary Size Comparison ==="

    local csv="$RESULTS_DIR/binary_size.csv"
    local text_csv="$RESULTS_DIR/text_size.csv"
    echo "program,veritas_bytes,gcc_O0_bytes,gcc_O2_bytes" > "$csv"
    echo "program,veritas_text_bytes,gcc_O0_text_bytes,gcc_O2_text_bytes" > "$text_csv"

    declare -A c_map
    c_map[01_simple]="simple_arithmetic"
    c_map[07_function_calls]="function_calls"
    c_map[14_for_loops]="for_loops"
    c_map[bubble_sort]="bubble_sort"

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

    log "  -> $csv"
    log "  -> $text_csv"
}

run_runtime_bench() {
    log "=== Runtime Correctness Comparison ==="

    local cases_file="eval/runtime_cases.tsv"
    local csv="$RESULTS_DIR/runtime_correctness.csv"
    local raw_dir="$RAW_DIR/runtime"
    mkdir -p "$raw_dir"
    echo "case_id,source,mode,expected,observed,compile_status,run_exit_code,status,stdout_file,stderr_file,note" > "$csv"

    while IFS=$'\t' read -r case_id source mode expected note; do
        local bin stdout_file stderr_file compile_status run_exit_code observed status first_line

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

        status="FAIL"
        case "$mode" in
            exit)
                if [ "$observed" = "$expected" ]; then
                    status="PASS"
                fi
                ;;
            checksum)
                if [ "$observed" = "$expected" ] && [ "$run_exit_code" -ge 0 ] && [ "$run_exit_code" -lt 128 ]; then
                    status="PASS"
                fi
                ;;
        esac

        printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
            "$case_id" "$source" "$mode" "$expected" "$observed" "$compile_status" "$run_exit_code" "$status" "$stdout_file" "$stderr_file" "$note" >> "$csv"
    done < "$cases_file"

    log "  -> $csv"
}

run_verifier_matrix() {
    log "=== Verifier Acceptance Matrix ==="

    local suite_manifest="eval/suites/feature_suite.txt"
    local suite_name
    suite_name="$(suite_stem "$suite_manifest")"
    local suite_file="$TMPDIR/${suite_name}.txt"
    resolve_suite "$suite_manifest" "resolved_${suite_name}.txt" > "$suite_file"

    local csv="$RESULTS_DIR/verifier_matrix.csv"
    echo "suite,file,frontend_ok,verifier_ok,frontend_smt_queries,verifier_smt_queries" > "$csv"

    while IFS= read -r f; do
        local name fe_ok fe_queries ver_ok ver_queries json json_v
        name="$(basename "$f" .veri)"
        log "  $name..."

        if json="$("$VERITAS" "$f" -o "$TMPDIR/vm_out" --bench 2>/dev/null)"; then
            fe_ok="yes"
            fe_queries="$(echo "$json" | jfield frontend_smt_queries)"
        else
            fe_ok="no"
            fe_queries="N/A"
        fi

        if json_v="$("$VERITAS" "$f" --verify -o "$TMPDIR/vm_out_v" --bench 2>/dev/null)"; then
            ver_ok="yes"
            ver_queries="$(echo "$json_v" | jfield verifier_smt_queries)"
        else
            ver_ok="no"
            ver_queries="N/A"
        fi

        printf '%s,%s,%s,%s,%s,%s\n' \
            "$suite_name" "$name" "$fe_ok" "$ver_ok" "$fe_queries" "$ver_queries" >> "$csv"
    done < "$suite_file"

    log "  -> $csv"
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

    log "  -> $RESULTS_DIR/loc.csv"
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
