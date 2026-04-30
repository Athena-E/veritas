# Evaluation Run

- label: `bootstrap`
- timestamp (UTC): `20260429_235925`
- git commit: `6daa957`

This directory is the root for one reproducible evaluation run.

Subdirectories:

- `raw/` raw benchmark outputs such as JSON and Hyperfine exports
- `derived/` CSV summaries and processed tables
- `logs/` human-readable logs
- `manifests/` frozen suite manifests copied at run creation time

Metadata is stored in `metadata.env`.
Git worktree state is stored in `git_status.txt`.
