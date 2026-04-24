#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

TMP_DIR="$(mktemp -d /tmp/veritas-region-stage1-XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

TARGET_DIR="${CARGO_TARGET_DIR:-$ROOT_DIR/target}"

run_ok() {
    local label="$1"
    shift
    echo "[ok] $label"
    CARGO_TARGET_DIR="$TARGET_DIR" cargo run --quiet -- "$@"
}

run_expect_fail() {
    local label="$1"
    local needle="$2"
    local src="$3"
    local log="$TMP_DIR/$label.log"

    echo "[fail] $label"
    set +e
    CARGO_TARGET_DIR="$TARGET_DIR" cargo run --quiet -- "$src" >"$log" 2>&1
    local status=$?
    set -e

    if [ "$status" -eq 0 ]; then
        echo "expected failure for $label, but compilation succeeded" >&2
        cat "$log" >&2
        exit 1
    fi

    if ! rg -q "$needle" "$log"; then
        echo "expected diagnostic for $label to contain: $needle" >&2
        cat "$log" >&2
        exit 1
    fi
}

cat >"$TMP_DIR/hosted_return_array.veri" <<'EOF'
fn main() -> [int; 4] {
    [0; 4]
}
EOF

cat >"$TMP_DIR/hosted_array_call_escape.veri" <<'EOF'
fn consume(arr: [int; 4]) -> int {
    arr[0]
}

fn main() -> int {
    let arr: [int; 4] = [0; 4];
    consume(arr)
}
EOF

cat >"$TMP_DIR/nested_region_ok.veri" <<'EOF'
fn main() -> int {
    let mut sum: int = 0;
    region {
        let arr: [int; 4] = [7; 4];
        sum = arr[0];
    }
    sum
}
EOF

cat >"$TMP_DIR/nested_region_bad_return.veri" <<'EOF'
fn main() -> int {
    region {
        return 1;
    }
    0
}
EOF

cat >"$TMP_DIR/nested_region_bad_assign.veri" <<'EOF'
fn main() -> int {
    let mut arr: [int; 4] = [0; 4];
    let arr2: [int; 4] = [5; 4];
    region {
        arr = arr2;
    }
    arr[0]
}
EOF

cat >"$TMP_DIR/nested_region_bad_escape_assign.veri" <<'EOF'
fn main() -> int {
    let mut arr: [int; 4] = [0; 4];
    region {
        let tmp: [int; 4] = [1; 4];
        arr = tmp;
    }
    arr[0]
}
EOF

run_ok \
  "hosted-array-example-verifies" \
  src/examples/20_array_loop_invariant.veri \
  --verify \
  -o "$TMP_DIR/array_loop_invariant"

run_expect_fail \
  "hosted-return-array-rejected" \
  "returning arrays on the hosted target" \
  "$TMP_DIR/hosted_return_array.veri"

run_expect_fail \
  "hosted-array-call-rejected" \
  "passing arrays to function calls on the hosted target" \
  "$TMP_DIR/hosted_array_call_escape.veri"

run_ok \
  "bare-metal-array-call-allowed" \
  "$TMP_DIR/hosted_array_call_escape.veri" \
  --target-bare-metal \
  -o "$TMP_DIR/bare_metal_array_call"

run_ok \
  "hosted-nested-region-verifies" \
  "$TMP_DIR/nested_region_ok.veri" \
  --verify \
  -o "$TMP_DIR/nested_region_ok"

run_expect_fail \
  "hosted-region-return-rejected" \
  "returning from inside a hosted region block" \
  "$TMP_DIR/nested_region_bad_return.veri"

run_expect_fail \
  "hosted-region-array-assign-rejected" \
  "assigning whole arrays inside a hosted region block" \
  "$TMP_DIR/nested_region_bad_escape_assign.veri"

run_ok \
  "hosted-region-outer-array-assign-allowed" \
  "$TMP_DIR/nested_region_bad_assign.veri" \
  --verify \
  -o "$TMP_DIR/nested_region_outer_assign_ok"

echo "Function-local and nested region validation passed."
