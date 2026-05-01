# Evaluation Suite Manifests

This directory defines the named evaluation suites referenced by the
dissertation plan.

Each manifest is a plain-text file with one entry per line. Blank lines and
lines starting with `#` are ignored.

Supported entry forms:

- `path:<relative-path>`
- `glob:<relative-glob>`
- `dir:<relative-directory>`

All paths are resolved relative to the repository root.

Current suite intent:

- `feature_suite.txt`
  representative supported-language programs for correctness and
  expressiveness claims
- `negative_suite.txt`
  all intentionally invalid programs used for safety rejection
- `verification_stress_suite.txt`
  programs chosen to stress SMT and verifier cost
- `algorithm_suite.txt`
  larger kernels used for runtime and practicality comparisons

These manifests are intended to be stable inputs to the evaluation harness.
When a program is added or removed, that should be a deliberate, reviewable
change to the relevant suite file.

Related metadata files:

- `eval/feature_tags.tsv`
  feature and narrative-role labels for the feature suite
- `eval/error_classes.tsv`
  error-class labels for the negative suite

Canonicalisation rules:

- Prefer the richer featureful local version when two programs represent the
  same conceptual algorithm.
- Keep binary-search and bubble-sort examples in the feature and
  verification-stress suites only when they add proof-structure coverage.
- Keep the algorithm suite focused on runtime-practicality cases that also
  have explicit semantic checks in `eval/runtime_cases.tsv`.
- Treat committed translated external kernels as the only publishable external
  benchmarks until additional translations and manifests are added deliberately.
