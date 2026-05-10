# Curated Feature Suite

This directory holds the dissertation-facing curated valid-program suite.

Purpose:

- keep the feature-suite population separate from the wider `src/examples/`
  sandbox
- make the published valid-program set easy to inspect without mixing in
  rejected examples, experiments, and superseded variants
- preserve the original example filenames while freezing a deliberate curated
  subset

Layout:

- `programs/` contains the curated published copies of the valid-program suite
- `index.tsv` gives each curated entry a stable dissertation-facing index
- `eval/suites/feature_suite.txt` is the executable manifest used by the
  harness

The curation rule is breadth over raw count: each entry should cover a distinct
source-language feature, proof obligation, or trust-architecture story.

The suite programs are intentionally cleaner and more presentation-ready than
the broader `src/examples/` sandbox. They should avoid dead code, commented-out
experiments, redundant helper functions, and examples whose only purpose is
parser smoke testing.
