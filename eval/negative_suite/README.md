# Curated Negative Suite

This directory holds the dissertation-facing curated invalid-program suite.

Purpose:

- keep the published rejection corpus separate from the broader `src/examples/`
  sandbox
- freeze the exact invalid-program population used for safety-rejection claims
- give the suite stable numbering even where the original source files used
  descriptive non-numeric names

Layout:

- `programs/` contains the curated published copies of the invalid-program
  suite
- `index.tsv` gives each curated entry a stable dissertation-facing index and
  records the original source path
- `eval/suites/negative_suite.txt` is the executable manifest used by the
  harness

Unlike the feature suite, the negative suite is intentionally exhaustive over
the committed invalid examples selected for evaluation rather than narrowly
representative: the dissertation claim is stronger when the full curated
population is included.
