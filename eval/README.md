# Evaluation Harness Bootstrap

This directory contains the evaluation harness, benchmark inputs, and
supporting assets for dissertation-grade evaluation.

## New baseline workflow

1. Resolve a named suite manifest:

   ```bash
   ./eval/resolve_suite.sh eval/suites/feature_suite.txt
   ```

2. Create a reproducible run directory:

   ```bash
   ./eval/create_run_dir.sh <label>
   ```

   This creates `eval/runs/<timestamp>_<label>/` and captures:

   - git commit and worktree status
   - tool versions
   - machine metadata
   - frozen copies of the suite manifests

3. Store benchmark outputs under that run directory:

   - `raw/` for raw `--bench` JSON and Hyperfine exports
   - `derived/` for CSV summaries and processed tables
   - `logs/` for human-readable logs

## Current suite manifests

- `eval/suites/feature_suite.txt`
- `eval/suites/negative_suite.txt`
- `eval/suites/verification_stress_suite.txt`
- `eval/suites/algorithm_suite.txt`

## Current status

The legacy script `eval/bench.sh` still writes flat results to `eval/results/`.
The next harness step should be to move published runs onto the manifest-driven
`eval/runs/` layout and separate raw outputs from derived summaries.
