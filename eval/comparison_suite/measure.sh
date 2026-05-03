#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
INDEX_FILE="${SCRIPT_DIR}/index.tsv"
RESULTS_DIR="${SCRIPT_DIR}/results"

mkdir -p "${RESULTS_DIR}"

mode="all"
if [[ $# -gt 0 ]]; then
    mode="$1"
fi

count_loc_metrics() {
    local rel_path="$1"
    local abs_path="${SCRIPT_DIR}/${rel_path}"
    local ext="${rel_path##*.}"

    awk -v ext="${ext}" '
        function is_blank(line) {
            return line ~ /^[[:space:]]*$/
        }

        function is_comment(line) {
            if (ext == "veri" || ext == "dfy" || ext == "rs") {
                return line ~ /^[[:space:]]*\/\//
            }
            if (ext == "hs") {
                return line ~ /^[[:space:]]*--/
            }
            return 0
        }

        function is_annotation(line) {
            if (ext == "veri") {
                return line ~ /requires / || line ~ /invariant / || line ~ /forall / || line ~ /\{v:[^}]+\}/
            }
            if (ext == "dfy") {
                return line ~ /^[[:space:]]*predicate / || line ~ /requires / || line ~ /ensures / || line ~ /invariant / || line ~ /^[[:space:]]*reads / || line ~ /forall /
            }
            if (ext == "hs") {
                return line ~ /\{\-@/
            }
            if (ext == "rs") {
                return line ~ /^[[:space:]]*spec fn / || line ~ /requires / || line ~ /ensures / || line ~ /invariant/ || line ~ /forall\|/
            }
            return 0
        }

        {
            if (is_blank($0) || is_comment($0)) {
                next
            }
            total += 1
            if (is_annotation($0)) {
                annotation += 1
            }
        }

        END {
            executable = total - annotation
            if (total == 0) {
                ratio = "0.000"
            } else if (executable == 0) {
                ratio = "inf"
            } else {
                ratio = sprintf("%.3f", annotation / executable)
            }
            printf "%d,%d,%d,%s\n", total, annotation, executable, ratio
        }
    ' "${abs_path}"
}

emit_loc_csv() {
    local output="${RESULTS_DIR}/loc_metrics.csv"
    printf "task_id,task_slug,system,file,total_loc,annotation_loc,executable_loc,annotation_to_code_ratio\n" > "${output}"

    tail -n +2 "${INDEX_FILE}" | while IFS=$'\t' read -r task_id task_slug veritas_file dafny_file liquid_haskell_file verus_file main_property; do
        for pair in \
            "veritas:${veritas_file}" \
            "dafny:${dafny_file}" \
            "liquid_haskell:${liquid_haskell_file}" \
            "verus:${verus_file}"
        do
            local system="${pair%%:*}"
            local rel_path="${pair#*:}"
            local counts
            counts="$(count_loc_metrics "${rel_path}")"
            IFS=, read -r total annotation executable ratio <<< "${counts}"
            printf "%s,%s,%s,%s,%s,%s,%s,%s\n" \
                "${task_id}" "${task_slug}" "${system}" "${rel_path}" \
                "${total}" "${annotation}" "${executable}" "${ratio}" >> "${output}"
        done
    done
}

extract_json_field() {
    local json="$1"
    local key="$2"
    sed -n "s/.*\"${key}\":\\([^,}]*\\).*/\\1/p" <<< "${json}" | head -n 1
}

emit_veritas_csv() {
    local output="${RESULTS_DIR}/veritas_metrics.csv"
    printf "task_id,task_slug,file,compile_ms,verify_ms,binary_bytes,frontend_smt_queries,frontend_smt_ms,verifier_smt_queries,verifier_smt_ms,verification_outcome\n" > "${output}"

    tail -n +2 "${INDEX_FILE}" | while IFS=$'\t' read -r task_id task_slug veritas_file dafny_file liquid_haskell_file verus_file main_property; do
        local tmp_out
        tmp_out="$(mktemp /tmp/veritas-comparison-XXXXXX)"

        local json
        if json="$("${ROOT_DIR}/target/release/veritas" "${SCRIPT_DIR}/${veritas_file}" --verify --bench -o "${tmp_out}" 2>&1)"; then
            local last_line
            last_line="$(tail -n 1 <<< "${json}")"
            printf "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,pass\n" \
                "${task_id}" \
                "${task_slug}" \
                "${veritas_file}" \
                "$(extract_json_field "${last_line}" "compile_ms")" \
                "$(extract_json_field "${last_line}" "verify_ms")" \
                "$(extract_json_field "${last_line}" "binary_bytes")" \
                "$(extract_json_field "${last_line}" "frontend_smt_queries")" \
                "$(extract_json_field "${last_line}" "frontend_smt_ms")" \
                "$(extract_json_field "${last_line}" "verifier_smt_queries")" \
                "$(extract_json_field "${last_line}" "verifier_smt_ms")" >> "${output}"
        else
            printf "%s,%s,%s,,,,,,,,fail\n" "${task_id}" "${task_slug}" "${veritas_file}" >> "${output}"
        fi

        rm -f "${tmp_out}"
    done
}

emit_placeholder_csv() {
    local output="${RESULTS_DIR}/external_tool_status.csv"
    printf "system,tool_validated_locally,notes\n" > "${output}"
    printf "dafny,no,Programs authored but Dafny not available in this environment.\n" >> "${output}"
    printf "liquid_haskell,no,Programs authored but Liquid Haskell not available in this environment.\n" >> "${output}"
    printf "verus,no,Programs authored but Verus not available in this environment.\n" >> "${output}"
}

case "${mode}" in
    --loc)
        emit_loc_csv
        ;;
    --veritas)
        emit_veritas_csv
        ;;
    --external-status)
        emit_placeholder_csv
        ;;
    --all|all)
        emit_loc_csv
        emit_veritas_csv
        emit_placeholder_csv
        ;;
    *)
        echo "Usage: $0 [--loc|--veritas|--external-status|--all]" >&2
        exit 1
        ;;
esac
