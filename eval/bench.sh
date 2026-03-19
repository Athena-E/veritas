#!/usr/bin/env bash
# Veritas Evaluation Benchmark Harness
# Produces CSV output for compilation time, verification, code quality, and SMT stats
#
# Usage: ./eval/bench.sh [--all | --compile | --verify | --codegen | --errors | --binary-size]
# Default: --all

set -euo pipefail
cd "$(dirname "$0")/.."

VERITAS="target/release/veritas"
RESULTS_DIR="eval/results"
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

RUNS=${BENCH_RUNS:-5}  # number of timing runs for averages

mkdir -p "$RESULTS_DIR"

# JSON field extractor using python
jfield() {
    python3 -c "import json,sys; d=json.load(sys.stdin); print(d.get('$1','N/A'))"
}

# Ensure release build is up to date
echo "Building veritas (release)..." >&2
cargo build --release -q 2>&1 >&2

# ============================================================
# Helper functions
# ============================================================

median() {
    # Read numbers from stdin, output median
    sort -n | awk '{a[NR]=$1} END {print a[int((NR+1)/2)]}'
}

bench_compile() {
    local file="$1"
    local flags="$2"
    local out="$3"

    local times=()
    for _ in $(seq 1 "$RUNS"); do
        local t
        t=$( { /usr/bin/time -f "%e" "$VERITAS" "$file" $flags -o "$TMPDIR/out" --bench 2>&1 1>"$TMPDIR/json"; } 2>&1 )
        times+=("$t")
    done

    # Use the last JSON output for SMT stats
    cat "$TMPDIR/json"
}

# ============================================================
# 1. Compilation benchmarks (all examples, with/without verify)
# ============================================================

run_compile_bench() {
    echo "=== Compilation Benchmarks ===" >&2
    echo "file,compile_ms,verify_ms,frontend_smt_queries,frontend_smt_ms,verifier_smt_queries,verifier_smt_ms,binary_bytes" > "$RESULTS_DIR/compilation.csv"

    for f in src/examples/*.veri; do
        name=$(basename "$f" .veri)
        echo "  $name..." >&2

        # Without verification
        json=$("$VERITAS" "$f" -o "$TMPDIR/out" --bench 2>/dev/null || echo '{"error":true}')
        if echo "$json" | grep -q '"error"'; then
            echo "    SKIP (compile error)" >&2
            continue
        fi

        compile_ms=$(echo "$json" | jfield compile_ms)
        fe_queries=$(echo "$json" | jfield frontend_smt_queries)
        fe_ms=$(echo "$json" | jfield frontend_smt_ms)
        binary=$(echo "$json" | jfield binary_bytes)

        # With verification (may fail)
        json_v=$("$VERITAS" "$f" --verify -o "$TMPDIR/out_v" --bench 2>/dev/null || echo '{"verify_failed":true}')
        if echo "$json_v" | grep -q '"verify_failed"'; then
            verify_ms="FAIL"
            ver_queries="FAIL"
            ver_ms="FAIL"
        else
            verify_ms=$(echo "$json_v" | jfield verify_ms)
            ver_queries=$(echo "$json_v" | jfield verifier_smt_queries)
            ver_ms=$(echo "$json_v" | jfield verifier_smt_ms)
        fi

        echo "$name,$compile_ms,$verify_ms,$fe_queries,$fe_ms,$ver_queries,$ver_ms,$binary" >> "$RESULTS_DIR/compilation.csv"
    done

    echo "  -> $RESULTS_DIR/compilation.csv" >&2
}

# ============================================================
# 2. Multi-run timing benchmarks (statistical)
# ============================================================

run_timing_bench() {
    echo "=== Timing Benchmarks ($RUNS runs each) ===" >&2
    echo "file,run,compile_ms,verify_ms" > "$RESULTS_DIR/timing.csv"

    # Select representative programs of varying complexity
    local programs=(
        src/examples/01_simple.veri
        src/examples/07_function_calls.veri
        src/examples/14_for_loops.veri
        src/examples/17_preconditions.veri
        src/examples/19_loop_invariant.veri
        src/examples/22_bubble_sort.veri
        src/examples/bubble_sort.veri
    )

    for f in "${programs[@]}"; do
        [ -f "$f" ] || continue
        name=$(basename "$f" .veri)
        echo "  $name ($RUNS runs)..." >&2

        for run in $(seq 1 "$RUNS"); do
            json=$("$VERITAS" "$f" -o "$TMPDIR/out" --bench 2>/dev/null || echo '{}')
            compile_ms=$(echo "$json" | jfield compile_ms 2>/dev/null || echo "ERR")
            echo "$name,$run,$compile_ms," >> "$RESULTS_DIR/timing.csv"
        done

        # Also time with verification
        for run in $(seq 1 "$RUNS"); do
            json=$("$VERITAS" "$f" --verify -o "$TMPDIR/out" --bench 2>/dev/null || echo '{}')
            compile_ms=$(echo "$json" | jfield compile_ms 2>/dev/null || echo "ERR")
            verify_ms=$(echo "$json" | jfield verify_ms 2>/dev/null || echo "FAIL")
            echo "${name}_verified,$run,$compile_ms,$verify_ms" >> "$RESULTS_DIR/timing.csv"
        done
    done

    echo "  -> $RESULTS_DIR/timing.csv" >&2
}

# ============================================================
# 3. Error rejection tests (negative cases)
# ============================================================

run_error_bench() {
    echo "=== Error Rejection Tests ===" >&2
    echo "file,expected_error,rejected,error_message" > "$RESULTS_DIR/error_rejection.csv"

    for f in src/examples/errors/*.veri; do
        name=$(basename "$f" .veri)
        error_output=$("$VERITAS" "$f" -o "$TMPDIR/err_out" 2>&1 || true)
        # Check if binary was actually produced
        if [ -f "$TMPDIR/err_out" ] && [ -x "$TMPDIR/err_out" ]; then
            rejected="no"
        else
            rejected="yes"
        fi
        rm -f "$TMPDIR/err_out"

        # Extract first line of error
        first_line=$(echo "$error_output" | grep -i -m1 "error\|FAILED" | head -c 100 | tr ',' ';' | tr '"' "'")
        echo "$name,type_error,$rejected,$first_line" >> "$RESULTS_DIR/error_rejection.csv"
    done

    echo "  -> $RESULTS_DIR/error_rejection.csv" >&2
}

# ============================================================
# 4. Binary size comparison with GCC
# ============================================================

run_binary_size_bench() {
    echo "=== Binary Size Comparison ===" >&2
    echo "program,veritas_bytes,gcc_O0_bytes,gcc_O2_bytes" > "$RESULTS_DIR/binary_size.csv"

    declare -A c_map
    c_map[01_simple]="simple_arithmetic"
    c_map[07_function_calls]="function_calls"
    c_map[14_for_loops]="for_loops"
    c_map[bubble_sort]="bubble_sort"

    for veri_name in "${!c_map[@]}"; do
        c_name="${c_map[$veri_name]}"
        veri_file="src/examples/${veri_name}.veri"
        c_file="eval/c_equivalents/${c_name}.c"

        [ -f "$veri_file" ] || continue
        [ -f "$c_file" ] || continue

        echo "  $veri_name..." >&2

        # Veritas binary
        "$VERITAS" "$veri_file" -o "$TMPDIR/veri_bin" --bench >/dev/null 2>&1 || continue
        veri_size=$(stat -c%s "$TMPDIR/veri_bin" 2>/dev/null || echo "ERR")

        # GCC -O0
        gcc -O0 -o "$TMPDIR/gcc_O0" "$c_file" 2>/dev/null || continue
        gcc_O0_size=$(stat -c%s "$TMPDIR/gcc_O0")

        # GCC -O2
        gcc -O2 -o "$TMPDIR/gcc_O2" "$c_file" 2>/dev/null
        gcc_O2_size=$(stat -c%s "$TMPDIR/gcc_O2")

        echo "$veri_name,$veri_size,$gcc_O0_size,$gcc_O2_size" >> "$RESULTS_DIR/binary_size.csv"
    done

    # Also compare stripped sizes (just the .text section)
    echo "" >&2
    echo "program,veritas_text_bytes,gcc_O0_text_bytes,gcc_O2_text_bytes" > "$RESULTS_DIR/text_size.csv"

    for veri_name in "${!c_map[@]}"; do
        c_name="${c_map[$veri_name]}"
        veri_file="src/examples/${veri_name}.veri"
        c_file="eval/c_equivalents/${c_name}.c"

        [ -f "$veri_file" ] || continue
        [ -f "$c_file" ] || continue

        "$VERITAS" "$veri_file" -o "$TMPDIR/veri_bin" --bench >/dev/null 2>&1 || continue
        veri_text=$(stat -c%s "$TMPDIR/veri_bin")  # Veritas ELF is essentially all .text

        gcc -O0 -o "$TMPDIR/gcc_O0" "$c_file" 2>/dev/null || continue
        gcc_O0_text=$(objdump -h "$TMPDIR/gcc_O0" | awk '/.text/{print strtonum("0x"$3)}')

        gcc -O2 -o "$TMPDIR/gcc_O2" "$c_file" 2>/dev/null
        gcc_O2_text=$(objdump -h "$TMPDIR/gcc_O2" | awk '/.text/{print strtonum("0x"$3)}')

        echo "$veri_name,$veri_text,$gcc_O0_text,$gcc_O2_text" >> "$RESULTS_DIR/text_size.csv"
    done

    echo "  -> $RESULTS_DIR/binary_size.csv" >&2
    echo "  -> $RESULTS_DIR/text_size.csv" >&2
}

# ============================================================
# 5. Runtime performance comparison
# ============================================================

run_runtime_bench() {
    echo "=== Runtime Performance Comparison ===" >&2
    echo "program,veritas_exit,gcc_O0_exit,gcc_O2_exit,note" > "$RESULTS_DIR/runtime_correctness.csv"

    declare -A c_map
    c_map[01_simple]="simple_arithmetic"
    c_map[07_function_calls]="function_calls"
    c_map[14_for_loops]="for_loops"
    c_map[bubble_sort]="bubble_sort"

    for veri_name in "${!c_map[@]}"; do
        c_name="${c_map[$veri_name]}"
        veri_file="src/examples/${veri_name}.veri"
        c_file="eval/c_equivalents/${c_name}.c"

        [ -f "$veri_file" ] || continue
        [ -f "$c_file" ] || continue

        echo "  $veri_name..." >&2

        # Compile and run Veritas
        "$VERITAS" "$veri_file" -o "$TMPDIR/veri_run" --bench >/dev/null 2>&1 || continue
        veri_exit=$("$TMPDIR/veri_run" >/dev/null 2>&1 && echo $? || echo $?)

        # Compile and run GCC -O0
        gcc -O0 -o "$TMPDIR/gcc_O0_run" "$c_file" 2>/dev/null
        gcc_O0_exit=$("$TMPDIR/gcc_O0_run" >/dev/null 2>&1 && echo $? || echo $?)

        # Compile and run GCC -O2
        gcc -O2 -o "$TMPDIR/gcc_O2_run" "$c_file" 2>/dev/null
        gcc_O2_exit=$("$TMPDIR/gcc_O2_run" >/dev/null 2>&1 && echo $? || echo $?)

        match="MATCH"
        if [ "$veri_exit" != "$gcc_O0_exit" ]; then
            match="MISMATCH"
        fi

        echo "$veri_name,$veri_exit,$gcc_O0_exit,$gcc_O2_exit,$match" >> "$RESULTS_DIR/runtime_correctness.csv"
    done

    echo "  -> $RESULTS_DIR/runtime_correctness.csv" >&2
}

# ============================================================
# 6. Verifier acceptance matrix
# ============================================================

run_verifier_matrix() {
    echo "=== Verifier Acceptance Matrix ===" >&2
    echo "file,frontend_ok,verifier_ok,exit_code,frontend_smt_queries,verifier_smt_queries" > "$RESULTS_DIR/verifier_matrix.csv"

    for f in src/examples/*.veri; do
        name=$(basename "$f" .veri)
        echo "  $name..." >&2

        # Frontend (compile without verify)
        if json=$("$VERITAS" "$f" -o "$TMPDIR/vm_out" --bench 2>/dev/null); then
            fe_ok="yes"
            fe_queries=$(echo "$json" | jfield frontend_smt_queries)

            # Run the binary
            "$TMPDIR/vm_out" >/dev/null 2>&1 || true
            exit_code=$?
        else
            fe_ok="no"
            fe_queries="N/A"
            exit_code="N/A"
        fi

        # Verifier (compile with verify)
        if json_v=$("$VERITAS" "$f" --verify -o "$TMPDIR/vm_out_v" --bench 2>/dev/null); then
            ver_ok="yes"
            ver_queries=$(echo "$json_v" | jfield verifier_smt_queries)
        else
            ver_ok="no"
            ver_queries="N/A"
        fi

        echo "$name,$fe_ok,$ver_ok,$exit_code,$fe_queries,$ver_queries" >> "$RESULTS_DIR/verifier_matrix.csv"
    done

    echo "  -> $RESULTS_DIR/verifier_matrix.csv" >&2
}

# ============================================================
# 7. LOC / TCB analysis
# ============================================================

run_loc_analysis() {
    echo "=== Lines of Code / TCB Analysis ===" >&2
    {
        echo "component,files,lines"

        # Frontend
        fe_lines=$(find src/frontend -name "*.rs" -exec cat {} + | wc -l)
        fe_files=$(find src/frontend -name "*.rs" | wc -l)
        echo "frontend,$fe_files,$fe_lines"

        # Backend
        be_lines=$(find src/backend -name "*.rs" -exec cat {} + | wc -l)
        be_files=$(find src/backend -name "*.rs" | wc -l)
        echo "backend,$be_files,$be_lines"

        # Verifier (TCB)
        ver_lines=$(find src/verifier -name "*.rs" -exec cat {} + | wc -l)
        ver_files=$(find src/verifier -name "*.rs" | wc -l)
        echo "verifier_tcb,$ver_files,$ver_lines"

        # Common
        com_lines=$(find src/common -name "*.rs" -exec cat {} + | wc -l)
        com_files=$(find src/common -name "*.rs" | wc -l)
        echo "common,$com_files,$com_lines"

        # Pipeline + main
        pipe_lines=$(cat src/pipeline.rs src/main.rs src/lib.rs 2>/dev/null | wc -l)
        echo "pipeline,3,$pipe_lines"

        # Total
        total_lines=$(find src -name "*.rs" -exec cat {} + | wc -l)
        total_files=$(find src -name "*.rs" | wc -l)
        echo "total,$total_files,$total_lines"

    } > "$RESULTS_DIR/loc.csv"

    echo "  -> $RESULTS_DIR/loc.csv" >&2
}

# ============================================================
# Main dispatch
# ============================================================

mode="${1:---all}"

case "$mode" in
    --all)
        run_loc_analysis
        run_compile_bench
        run_timing_bench
        run_verifier_matrix
        run_error_bench
        run_binary_size_bench
        run_runtime_bench
        ;;
    --compile)    run_compile_bench ;;
    --timing)     run_timing_bench ;;
    --verify)     run_verifier_matrix ;;
    --errors)     run_error_bench ;;
    --binary-size) run_binary_size_bench ;;
    --runtime)    run_runtime_bench ;;
    --loc)        run_loc_analysis ;;
    *)
        echo "Usage: $0 [--all|--compile|--timing|--verify|--errors|--binary-size|--runtime|--loc]"
        exit 1
        ;;
esac

echo "" >&2
echo "Done. Results in $RESULTS_DIR/" >&2
