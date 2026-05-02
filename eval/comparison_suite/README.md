# Cross-System Comparison Suite

This directory contains small matched programs used to compare Veritas against
other verification systems on verification cost and annotation burden.

Current scope:

- primary empirical counterpart: `Dafny`
- focus: contracts, arrays, and loop invariants
- excluded from this first suite: borrow/ownership features that are not
  directly comparable to Dafny

Layout:

- `index.tsv` defines the comparison tasks
- `veritas/` contains the Veritas versions
- `dafny/` contains the Dafny versions

This suite is intentionally small. It is meant to provide a defensible,
controlled comparison set rather than a broad language shootout.

Current validation status:

- the Veritas programs have been checked locally against the current compiler
  and verifier
- the Dafny programs have been authored and curated for matching intent, but
  have not yet been tool-validated in this environment
