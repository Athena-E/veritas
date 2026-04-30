#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 1 ]; then
    echo "usage: $0 <suite-manifest>" >&2
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
MANIFEST="$1"

if [[ "$MANIFEST" != /* ]]; then
    MANIFEST="$REPO_ROOT/$MANIFEST"
fi

if [ ! -f "$MANIFEST" ]; then
    echo "suite manifest not found: $MANIFEST" >&2
    exit 1
fi

cd "$REPO_ROOT"

tmpfile="$(mktemp)"
trap 'rm -f "$tmpfile"' EXIT

while IFS= read -r line || [ -n "$line" ]; do
    case "$line" in
        "" | \#*)
            continue
            ;;
        path:*)
            path="${line#path:}"
            if [ ! -e "$path" ]; then
                echo "missing path in suite manifest: $path" >&2
                exit 1
            fi
            printf '%s\n' "$path" >> "$tmpfile"
            ;;
        glob:*)
            pattern="${line#glob:}"
            matches=()
            while IFS= read -r match; do
                matches+=("$match")
            done < <(compgen -G "$pattern" || true)
            if [ "${#matches[@]}" -eq 0 ]; then
                echo "glob matched nothing in suite manifest: $pattern" >&2
                exit 1
            fi
            printf '%s\n' "${matches[@]}" >> "$tmpfile"
            ;;
        dir:*)
            dir="${line#dir:}"
            if [ ! -d "$dir" ]; then
                echo "missing directory in suite manifest: $dir" >&2
                exit 1
            fi
            find "$dir" -type f | sort >> "$tmpfile"
            ;;
        *)
            echo "unsupported suite manifest entry: $line" >&2
            exit 1
            ;;
    esac
done < "$MANIFEST"

sort -u "$tmpfile"
