# Canonical analyses

This directory contains the publication-facing high-level analytical entry points. Lower-level data import, cleaning, validation, and helper functions remain in `../scripts/`.

## `01_population_weighted_heatmap.R`

Canonical population-weighted district-month workflow.

The script currently:

- loads shared project setup/helpers;
- validates required GTD, Iraq ethnicity/spatial, and LandScan inputs;
- records raw-file provenance;
- performs event and spatial quality checks;
- constructs area-weighted ethnicity shares as a diagnostic baseline;
- constructs LandScan population-weighted district ethnicity shares;
- exports schemas and quality-control tables; and
- proceeds into district-month analytical and figure-generation work.

Run from the **repository root** so relative paths resolve correctly:

```r
source("analysis/01_population_weighted_heatmap.R")
```

## `02_targeting_composition_dashboard.R`

Publication-oriented targeting dashboard that combines:

1. observed civilian vs state/security targeting composition;
2. model-predicted probability of civilian targeting; and
3. model-predicted probability of religious targeting conditional on civilian targeting.

This script is currently **upstream-dependent**, not standalone. It checks for the following objects before execution:

```text
target_composition_wide
two_stage_predictions
demographic_order
```

That dependency should remain explicit until the upstream observed-composition and model-fitting sequence is consolidated into canonical scripts.

## Canonical vs legacy

Files under `../archive/legacy/` are preserved research history. They may contain useful implementation detail or earlier approaches, but they are not the recommended starting point for reproducing the current analysis.

See `../PROVENANCE.md` for the path mapping used in the publication refactor.
