# Changelog

All notable repository-structure and publication-infrastructure changes are documented here. Analytical findings remain preliminary unless associated with a tagged research release.

## Unreleased

### Repository publication refactor

- Reorganized the repository around canonical `analysis/`, modular `scripts/`, canonical `figures/`, and `archive/legacy/` paths.
- Preserved relocated analytical and legacy files using their existing Git blob content.
- Added a publication-facing repository banner and revised README.
- Added `CITATION.cff` for scholarly citation metadata.
- Added `DATA.md`, `REPRODUCIBILITY.md`, `PROVENANCE.md`, and `RIGHTS.md`.
- Added local raw-data staging documentation and ignore rules.
- Added a machine-readable R package-name manifest without fabricating historical package versions.
- Added lightweight CI to parse publication-facing R scripts for syntax regressions.
- Clarified that the targeting dashboard depends on upstream objects and is not yet a standalone pipeline.
- Made the preliminary/non-frozen status of empirical outputs explicit.

### Not changed by this refactor

- Statistical estimators and model specifications.
- Event-classification rules.
- Spatial transformation logic.
- Population-weighting logic.
- Existing model objects.
- Existing data dictionaries and diagnostic artifacts.
- Existing figure content.

See `PROVENANCE.md` for exact path relocations and the pre-refactor commit anchor.
