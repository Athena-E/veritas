#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

REPS=${REPS:-5}

build() {
    gcc -O2 -o fw_c floyd_warshall.c
    gcc -O3 -o fw_c_o3 floyd_warshall.c
    rustc -C opt-level=3 -o fw_rs floyd_warshall.rs
    go build -o fw_go floyd_warshall.go
    javac FloydWarshall.java
}

bench() {
    local name="$1"; shift
    local best="999"
    for _ in $(seq 1 "$REPS"); do
        t=$("$@" 2>/dev/null)
        awk -v a="$t" -v b="$best" 'BEGIN{exit !(a<b)}' && best="$t"
    done
    printf "%-18s %s s\n" "$name" "$best"
}

build
echo "Best of $REPS runs (lower is better):"
bench "C -O2"       ./fw_c
bench "C -O3"       ./fw_c_o3
bench "Rust -O3"    ./fw_rs
bench "Go"          ./fw_go
bench "Java"        java FloydWarshall
