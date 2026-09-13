# Provenance and structural refactor log

This document records the publication-oriented repository refactor performed from the pre-refactor `main` state at commit:

```text
bee48e3cc9cc6e9d765dd2bec55cdc0484663b58
```

The refactor is designed to preserve analytical work and historical traceability while separating canonical research entry points from legacy/scratch material.

## Preservation principle

Where an existing file is relocated, the refactor reuses the **same Git blob SHA** rather than rewriting its content. In other words, relocation changes repository structure, not the underlying file bytes. Full prior paths remain recoverable through Git history.

No statistical model, transformation, estimator, classification rule, or figure-generating logic is modified by the structural move itself.

## Canonical analytical relocations

| Pre-refactor path | Publication-oriented path | Treatment |
|---|---|---|
| `03) Heatmap - Ethnicity Population Proportion vs Attack Frequency by Month.R` | `analysis/01_population_weighted_heatmap.R` | Relocated verbatim; canonical analysis |
| `04) Targetting Composition Plot.R` | `analysis/02_targeting_composition_dashboard.R` | Relocated verbatim; spelling normalized in path only; canonical but upstream-dependent |
| `figs/` | `figures/` | Directory relocated verbatim; canonical figures |
| `gtd_manual_review.csv` | `data/review/gtd_manual_review.csv` | Relocated verbatim |
| `gtd_probable_inspired.csv` | `data/review/gtd_probable_inspired.csv` | Relocated verbatim |

## Legacy root scripts

The following pre-refactor root-level scripts are retained verbatim under `archive/legacy/root-scripts/`:

```text
Attempt.R
GTD Cleaning.R
GTD Manipulation.R
Iraq Ethnicity Cleaning.R
Layer 1.R
Untitled-1.R
Untitled-2.R
Untitled-4.R
Untitled-5.R
Untitled-6.R
Untitled-8.R
Untitled-9.R
```

These files remain part of the project record but are no longer presented as canonical entry points.

## Duplicate root-level figures

Pre-refactor root-level figure copies are retained under `archive/legacy/root-figures/`:

```text
fig1.svg
fig3.svg
fig4.svg
monthly_attacks.svg
population_weighted_heatmap.svg
```

The publication-facing figure directory is `figures/`.

## Scripts documentation

The previous `scripts/00) README.md` is retained in the archive as historical documentation. A new `scripts/README.md` describes the current modular scripts using their actual filenames.

## New publication infrastructure

The refactor adds:

- `CITATION.cff`
- `DATA.md`
- `REPRODUCIBILITY.md`
- `RIGHTS.md`
- `CHANGELOG.md`
- `.gitignore`
- `.github/workflows/r-syntax.yml`
- `analysis/README.md`
- `archive/README.md`
- `data/raw/README.md`
- `environment/dependencies.R`
- `assets/repository-banner.svg`

## What remains unresolved

The refactor intentionally does not claim to solve issues for which the necessary evidence is not yet available:

- exact historical R/package versions;
- end-to-end validation from a clean environment;
- redistribution status of every currently tracked derived data product;
- a finalized open-source/content licensing strategy; and
- a frozen empirical result set.

Those items should be resolved before a citable tagged release.
