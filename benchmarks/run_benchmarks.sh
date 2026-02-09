#!/bin/bash
# Veritas Compiler Benchmark Suite
# Measures: Compilation speed, Verification overhead, Generated code performance

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$SCRIPT_DIR/results"
ITERATIONS=${1:-10}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

mkdir -p "$OUTPUT_DIR"

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║         Veritas Compiler Benchmark Suite                   ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo "Project directory: $PROJECT_DIR"
echo "Iterations per benchmark: $ITERATIONS"
echo "Results directory: $OUTPUT_DIR"
echo ""

# Build release version first
echo -e "${YELLOW}Building release version...${NC}"
cd "$PROJECT_DIR"
cargo build --release 2>/dev/null
COMPILER="$PROJECT_DIR/target/release/veritas"
echo -e "${GREEN}Build complete.${NC}"
echo ""

# Timestamp for this run
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_FILE="$OUTPUT_DIR/benchmark_$TIMESTAMP.txt"

# Function to measure time in milliseconds
measure_time() {
    local start=$(date +%s%N)
    "$@" > /dev/null 2>&1
    local exit_code=$?
    local end=$(date +%s%N)
    echo $(( (end - start) / 1000000 ))
    return $exit_code
}

# Function to measure execution time only
measure_exec() {
    local start=$(date +%s%N)
    "$1" 2>/dev/null
    local exit_code=$?
    local end=$(date +%s%N)
    local time_ms=$(( (end - start) / 1000000 ))
    echo "$time_ms $exit_code"
}

# Function to run multiple iterations and compute stats
run_benchmark() {
    local name="$1"
    shift
    local times=()

    for i in $(seq 1 $ITERATIONS); do
        local t=$(measure_time "$@")
        times+=($t)
    done

    # Compute min, max, avg
    local sum=0
    local min=${times[0]}
    local max=${times[0]}
    for t in "${times[@]}"; do
        sum=$((sum + t))
        ((t < min)) && min=$t
        ((t > max)) && max=$t
    done
    local avg=$((sum / ITERATIONS))

    echo "$avg $min $max"
}

# ============================================================================
# SECTION 1: Compilation Speed (without verification)
# ============================================================================
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}SECTION 1: Compilation Speed (no verification)${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

{
    echo "=== COMPILATION SPEED (no verification) ==="
    echo "Timestamp: $(date)"
    echo "Iterations: $ITERATIONS"
    echo ""
    printf "%-30s %10s %10s %10s\n" "Benchmark" "Avg(ms)" "Min(ms)" "Max(ms)"
    printf "%-30s %10s %10s %10s\n" "─────────" "──────" "──────" "──────"
} >> "$RESULTS_FILE"

for bench in "$SCRIPT_DIR"/*.veri; do
    name=$(basename "$bench" .veri)
    result=$(run_benchmark "$name" "$COMPILER" "$bench" -o /tmp/bench_out -O)
    read avg min max <<< "$result"

    printf "%-30s %10d %10d %10d\n" "$name" "$avg" "$min" "$max"
    printf "%-30s %10d %10d %10d\n" "$name" "$avg" "$min" "$max" >> "$RESULTS_FILE"
done

echo ""

# ============================================================================
# SECTION 2: Verification Overhead
# ============================================================================
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}SECTION 2: Verification Overhead${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

{
    echo ""
    echo "=== VERIFICATION OVERHEAD ==="
    echo ""
    printf "%-30s %10s %10s %10s %10s\n" "Benchmark" "NoVerify" "Verify" "Overhead" "Ratio"
    printf "%-30s %10s %10s %10s %10s\n" "─────────" "────────" "──────" "────────" "─────"
} >> "$RESULTS_FILE"

printf "%-30s %10s %10s %10s %10s\n" "Benchmark" "NoVerify" "Verify" "Overhead" "Ratio"
printf "%-30s %10s %10s %10s %10s\n" "─────────" "────────" "──────" "────────" "─────"

for bench in "$SCRIPT_DIR"/*.veri; do
    name=$(basename "$bench" .veri)

    # Without verification
    result_no=$(run_benchmark "$name" "$COMPILER" "$bench" -o /tmp/bench_out -O)
    read avg_no _ _ <<< "$result_no"

    # With verification
    result_yes=$(run_benchmark "$name" "$COMPILER" "$bench" --verify -o /tmp/bench_out -O)
    read avg_yes _ _ <<< "$result_yes"

    overhead=$((avg_yes - avg_no))
    if [ $avg_no -gt 0 ]; then
        ratio=$(echo "scale=2; $avg_yes / $avg_no" | bc)
    else
        ratio="N/A"
    fi

    printf "%-30s %10d %10d %10d %10s\n" "$name" "$avg_no" "$avg_yes" "$overhead" "${ratio}x"
    printf "%-30s %10d %10d %10d %10s\n" "$name" "$avg_no" "$avg_yes" "$overhead" "${ratio}x" >> "$RESULTS_FILE"
done

echo ""

# ============================================================================
# SECTION 3: Generated Code Performance
# ============================================================================
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}SECTION 3: Generated Code Performance${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

{
    echo ""
    echo "=== GENERATED CODE PERFORMANCE ==="
    echo ""
    printf "%-30s %10s %10s %10s %12s\n" "Benchmark" "Avg(ms)" "Min(ms)" "Max(ms)" "Exit Code"
    printf "%-30s %10s %10s %10s %12s\n" "─────────" "──────" "──────" "──────" "─────────"
} >> "$RESULTS_FILE"

printf "%-30s %10s %10s %10s %12s\n" "Benchmark" "Avg(ms)" "Min(ms)" "Max(ms)" "Exit Code"
printf "%-30s %10s %10s %10s %12s\n" "─────────" "──────" "──────" "──────" "─────────"

for bench in "$SCRIPT_DIR"/*.veri; do
    name=$(basename "$bench" .veri)
    exe="/tmp/bench_${name}"

    # Compile (suppress all output)
    "$COMPILER" "$bench" -o "$exe" -O > /dev/null 2>&1

    # Run multiple times and collect stats
    times=()
    exit_code=0
    for i in $(seq 1 $ITERATIONS); do
        result=$(measure_exec "$exe")
        read t ec <<< "$result"
        times+=($t)
        exit_code=$ec
    done

    # Compute min, max, avg
    sum=0
    min=${times[0]}
    max=${times[0]}
    for t in "${times[@]}"; do
        sum=$((sum + t))
        ((t < min)) && min=$t
        ((t > max)) && max=$t
    done
    avg=$((sum / ITERATIONS))

    printf "%-30s %10d %10d %10d %12d\n" "$name" "$avg" "$min" "$max" "$exit_code"
    printf "%-30s %10d %10d %10d %12d\n" "$name" "$avg" "$min" "$max" "$exit_code" >> "$RESULTS_FILE"
done

echo ""

# NOTE: Fibonacci scaling section removed for brevity

# ============================================================================
# SECTION 5: Binary Size Analysis
# ============================================================================
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}SECTION 5: Binary Size Analysis${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

{
    echo ""
    echo "=== BINARY SIZE ANALYSIS ==="
    echo ""
    printf "%-30s %15s\n" "Benchmark" "Size (bytes)"
    printf "%-30s %15s\n" "─────────" "────────────"
} >> "$RESULTS_FILE"

printf "%-30s %15s\n" "Benchmark" "Size (bytes)"
printf "%-30s %15s\n" "─────────" "────────────"

for bench in "$SCRIPT_DIR"/*.veri; do
    name=$(basename "$bench" .veri)
    exe="/tmp/bench_${name}"

    "$COMPILER" "$bench" -o "$exe" -O > /dev/null 2>&1
    size=$(stat -c%s "$exe" 2>/dev/null || stat -f%z "$exe" 2>/dev/null)

    printf "%-30s %15d\n" "$name" "$size"
    printf "%-30s %15d\n" "$name" "$size" >> "$RESULTS_FILE"
done

echo ""

# ============================================================================
# Summary
# ============================================================================
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}Benchmark Complete!${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Results saved to: $RESULTS_FILE"
echo ""

# Cleanup
rm -f /tmp/bench_* /tmp/fib_test* 2>/dev/null || true
