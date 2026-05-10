# DTAL Tampering Corpus

This directory defines the dissertation-facing corpus used to demonstrate that
standalone DTAL verification rejects low-level programs that have been
incorrectly modified after compilation.

Purpose:

- support the trust-architecture claim that the compiler need not be fully
  trusted
- show that standalone `--verify-dtal` rejects malformed but syntactically
  valid DTAL programs
- keep these cases separate from the source-language feature and negative
  suites, because they target backend corruption rather than frontend errors

Method:

- start from a valid curated feature-suite program
- compile it to DTAL
- apply a small text-level mutation that preserves DTAL syntax
- run standalone verification on the tampered DTAL text
- record the expected verifier failure class

The executable check for this corpus lives in
`tests/dtal_tampering.rs` and can be run with:

```bash
cargo test --test dtal_tampering -- --ignored
```

This corpus is intentionally mutation-based rather than file-based. The test
derives tampered DTAL from the compiler's current output so that the trust test
tracks backend evolution instead of depending on hand-maintained stale `.dtal`
snapshots.

Concrete tampered `.dtal` artifacts can be regenerated with:

```bash
cargo run --bin generate_dtal_tampering
```

This writes the current corpus to `eval/dtal_tampering/generated/`, so each
case can be checked manually with `--verify-dtal`.

Current coverage:

- 14 mutation cases
- arithmetic overflow rejection
- precondition and assertion failures
- positive and negative out-of-bounds memory accesses
- use-after-drop / consumed-register rejection
- ownership duplication and live-borrow violations
- mutable aliasing violations
- entry-state and signature structural mismatches
