# Reproducibility

This repository is an active research workspace. It contains reproducibility controls, but it is **not yet a frozen computational capsule**. The goal of this document is to distinguish what is already reproducible from what still requires a known-good environment snapshot before a tagged release.

## Current controls

The codebase currently includes:

- an explicit project seed: `1501211`;
- package-availability checks in `scripts/00) Project Setup.R`;
- source-file existence checks in the population-weighted workflow;
- raw-file indexing;
- schema inventories and dictionaries;
- duplicate, missingness, date/time, categorical, numeric, and spatial diagnostics;
- intermediate analytical objects and saved model objects; and
- archived legacy scripts retained for provenance.

## Environment

The core setup script currently checks for:

```text
tidyverse
janitor
mice
sf
broom
broom.mixed
skimr
lubridate
patchwork
scales
terra
raster
exactextractr
```

Some downstream model-fitting work may require additional packages. Saved model names indicate that `brms` has been used in at least part of the modeling workflow, but the publication refactor does not invent an exact package version history that is not recorded in the repository.

A machine-readable package manifest is available at `environment/dependencies.R`.

## Why there is no fabricated `renv.lock`

A lockfile is only useful if it reflects an environment known to execute the canonical workflow successfully. Creating a lockfile from arbitrary current package versions would give false confidence. Before the first tagged research release:

1. run the canonical workflow successfully in the intended R environment;
2. initialize `renv` in that environment;
3. call `renv::snapshot()`;
4. commit `renv.lock`; and
5. validate restoration in a clean environment with `renv::restore()`.

## Suggested execution sequence

Run from the repository root.

### 1. Inspect source-data requirements

Read `DATA.md` and place locally obtained source files under `data/raw/` using the expected paths.

### 2. Load project setup

```r
source("scripts/00) Project Setup.R")
```

### 3. Run the general initialization pipeline if appropriate

```r
source("scripts/25) Data Initialization Pipeline.R")
```

This pipeline is intentionally broad. Review source paths and expected file types before execution.

### 4. Run the canonical population-weighted analysis

```r
source("analysis/01_population_weighted_heatmap.R")
```

The script performs explicit file checks before beginning downstream work.

### 5. Run the targeting dashboard only after upstream objects exist

```r
source("analysis/02_targeting_composition_dashboard.R")
```

This script expects at least the following objects to exist in the active session:

```text
target_composition_wide
two_stage_predictions
demographic_order
```

The dependency is currently explicit rather than hidden behind an incomplete wrapper.

## Validation before publication

A publication/tagged-release checklist should include:

- clean clone;
- raw data obtained independently from source locations;
- environment restored from a committed lockfile;
- full canonical workflow executed from the repository root;
- deterministic outputs checked where applicable;
- figure/table outputs regenerated;
- session information recorded;
- derived-data redistribution reviewed; and
- citation metadata updated to the release version.

## Syntax CI

The repository includes a lightweight GitHub Actions workflow that parses the canonical `analysis/` and modular `scripts/` R files. This detects syntax regressions without pretending to validate statistical correctness or external-data availability.
