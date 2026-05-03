# Comparison Results

This directory stores derived measurement artifacts for the cross-system
comparison suite.

Current generated files:

- `loc_metrics.csv`
  heuristic line-count metrics for all comparison files
- `veritas_metrics.csv`
  benchmark and solver metrics collected from the Veritas side
- `external_tool_status.csv`
  records whether non-Veritas toolchains were available locally when the
  measurements were produced

Generate or refresh these files with:

```bash
./eval/comparison_suite/measure.sh --all
```

Notes:

- the LOC counts are heuristic by design; the counting rule is defined in
  `measure.sh` and is meant to be explicit and repeatable rather than perfect
- only the Veritas side is executable in the current environment
- Dafny, Liquid Haskell, and Verus measurements remain placeholders until
  those toolchains are installed and validated locally
