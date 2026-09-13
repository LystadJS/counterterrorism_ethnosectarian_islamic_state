# Modular research scripts

This directory contains reusable project setup, import, cleaning, diagnostic, transformation, and helper scripts. The publication-facing high-level analyses live in `../analysis/`.

## Recommended orientation

The core design rule is:

> Import conservatively, audit before transformation, and make analytical assumptions visible.

The script collection includes:

- project setup and package checks;
- raw-file indexing;
- CSV, delimited, DAT, shapefile, and XLSX import helpers;
- column-name and missing-value standardization;
- raw-table combination with source metadata;
- schema and duplicate audits;
- controlled type conversion;
- missingness diagnostics and visualization;
- numeric, categorical, date/time, and spatial diagnostics;
- clean-data and dictionary export helpers; and
- project-specific district-panel helpers.

## Key scripts

| Script | Role |
|---|---|
| `00) Project Setup.R` | Package checks, directory setup, missing-value tokens, deterministic seed |
| `01) Raw File Index.R` | Raw-file provenance/indexing |
| `02) Import Data (CSV).R` | Character-first CSV import |
| `05) Import Data (SHP).R` | Spatial-file import |
| `08) Clean Missing Values.R` | Common missing-token handling |
| `11) Schema Inventory and Column Audit.R` | Schema audit |
| `12) Duplicate Audit.R` | Duplicate checks |
| `13) Data Type Guessing.R` | Type review before coercion |
| `14) Data Type Conversion.R` | Controlled type conversion |
| `15) Missingness Summary Tables.R` | Missingness summaries |
| `22) Initial Diagnostics (SHP).R` | Spatial diagnostics |
| `24) Export Cleaned Data.R` | Clean-data/dictionary export helpers |
| `25) Data Initialization Pipeline.R` | Broad initialization pipeline tying modular scripts together |
| `25) Load Helper Scripts.R` | Loads project helper functions |
| `26) ISIS District Panel Helpers.R` | Project-specific district-panel/spatial helpers |

## Execution

Run scripts from the repository root. Relative paths are written with that assumption.

For the general initialization workflow:

```r
source("scripts/00) Project Setup.R")
source("scripts/25) Data Initialization Pipeline.R")
```

For publication-facing analyses, see `../analysis/README.md`.

## Historical note

The pre-refactor `scripts/00) README.md` used example filenames that no longer matched the actual repository. It is preserved in `../archive/legacy/scripts-readme-set07.md`; this file is the current documentation.
