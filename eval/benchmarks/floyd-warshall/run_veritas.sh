#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

REPS=${REPS:-5}
N=64
ROOT_DIR=$(cd ../../.. && pwd)
VERITAS_BIN=${VERITAS_BIN:-"$ROOT_DIR/target/release/veritas"}
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

build() {
    if [ ! -x "$VERITAS_BIN" ]; then
        (cd "$ROOT_DIR" && cargo build --release >/dev/null)
    fi

    gcc -O2 -DN="$N" -o "$TMP_DIR/fw_c_o2" floyd_warshall.c
    gcc -O3 -DN="$N" -o "$TMP_DIR/fw_c_o3" floyd_warshall.c

    sed "s/^const N: usize = 1024;/const N: usize = $N;/" \
        floyd_warshall.rs > "$TMP_DIR/floyd_warshall.rs"
    rustc -C opt-level=3 -o "$TMP_DIR/fw_rs" "$TMP_DIR/floyd_warshall.rs"

    sed "s/^    static final int N = 1024;/    static final int N = $N;/" \
        FloydWarshall.java > "$TMP_DIR/FloydWarshall.java"
    javac -d "$TMP_DIR" "$TMP_DIR/FloydWarshall.java"

    "$VERITAS_BIN" floyd_warshall.veri -O -o "$TMP_DIR/fw_veri" >/dev/null
}

measure_ms() {
    local cmd="$1"
    local best_ns=0
    for _ in $(seq 1 "$REPS"); do
        local start_ns end_ns elapsed_ns
        start_ns=$(date +%s%N)
        bash -lc "$cmd" >/dev/null 2>/dev/null
        end_ns=$(date +%s%N)
        elapsed_ns=$((end_ns - start_ns))
        if [ "$best_ns" -eq 0 ] || [ "$elapsed_ns" -lt "$best_ns" ]; then
            best_ns=$elapsed_ns
        fi
    done

    python3 - "$best_ns" <<'PY'
import sys
ns = int(sys.argv[1])
print(f"{ns / 1_000_000:.3f}")
PY
}

checksum_line() {
    local cmd="$1"
    local out_file="$TMP_DIR/out.txt"
    local err_file="$TMP_DIR/err.txt"
    bash -lc "$cmd" >"$out_file" 2>"$err_file"
    local line
    if [ -s "$err_file" ]; then
        line=$(sed -n '1p' "$err_file")
    else
        line=$(sed -n '1p' "$out_file")
    fi

    case "$line" in
        checksum=*)
            printf "%s\n" "$line"
            ;;
        *)
            printf "checksum=%s\n" "$line"
            ;;
    esac
}

build

echo "Floyd-Warshall benchmark at N=$N"
echo "Best of $REPS end-to-end runs (lower is better)"
printf "%-18s %12s %s\n" "Implementation" "Best(ms)" "Checksum"

printf "%-18s %12s %s\n" \
    "C -O2" \
    "$(measure_ms "$TMP_DIR/fw_c_o2")" \
    "$(checksum_line "$TMP_DIR/fw_c_o2")"

printf "%-18s %12s %s\n" \
    "C -O3" \
    "$(measure_ms "$TMP_DIR/fw_c_o3")" \
    "$(checksum_line "$TMP_DIR/fw_c_o3")"

printf "%-18s %12s %s\n" \
    "Rust -O3" \
    "$(measure_ms "$TMP_DIR/fw_rs")" \
    "$(checksum_line "$TMP_DIR/fw_rs")"

printf "%-18s %12s %s\n" \
    "Java" \
    "$(measure_ms "java -cp $TMP_DIR FloydWarshall")" \
    "$(checksum_line "java -cp $TMP_DIR FloydWarshall")"

printf "%-18s %12s %s\n" \
    "Veritas -O" \
    "$(measure_ms "$TMP_DIR/fw_veri")" \
    "$(checksum_line "$TMP_DIR/fw_veri")"
