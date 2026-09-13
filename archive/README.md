# Legacy research archive

This directory preserves earlier research scripts and duplicate root-level outputs that were present before the publication-oriented repository refactor.

## Purpose

Legacy material is retained because exploratory code, abandoned approaches, and intermediate workflows can be important for reconstructing how analytical decisions evolved. At the same time, presenting those files at the repository root makes it difficult for external reviewers to distinguish current entry points from historical work.

The archive therefore serves two goals:

1. **Preserve provenance** — historical files remain accessible in the current tree and in Git history.
2. **Clarify the public interface** — canonical work lives in `analysis/`, `scripts/`, `figures/`, and the documented data directories.

## Contents

- `root-scripts/` — earlier root-level R scripts, including exploratory `Untitled-*` files and prior cleaning/manipulation workflows.
- `root-figures/` — duplicate figure files formerly stored at the repository root.
- `scripts-readme-set07.md` — historical modular-script README retained verbatim.

## Use

Do not assume archived files are mutually consistent, current, or intended to execute in sequence. Use them for provenance and implementation history. Start new reproduction work from the root `README.md`, `analysis/README.md`, and `scripts/README.md`.

See `../PROVENANCE.md` for the exact pre-refactor → post-refactor path mapping.
