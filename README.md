<div align="center">

# Counterterrorism, Ethnosectarian Context & Islamic State Attack Patterns

**R workflows for event-data cleaning, spatial-demographic integration, panel construction, diagnostics, and visualization**

[Portfolio](https://lystadjs.github.io/) · [Research](https://lystadjs.github.io/research.html) · [Code & Development](https://lystadjs.github.io/code.html) · [GitHub Profile](https://github.com/LystadJS)

</div>

---

**Repository status:** `ACTIVE RESEARCH WORKSPACE` · methods and file organization are still evolving

## Overview

This repository contains analytical workflows for studying terrorism and Islamic State-linked attack patterns alongside local ethnosectarian context in Iraq. The codebase combines event-level terrorism data, district boundaries, ethnicity information, and population weighting to produce analysis-ready geographic and temporal structures.

The repository should be treated as a **research workspace rather than a finalized publication artifact**. It contains both modularized workflows and retained working/legacy scripts. No empirical result in this repository should be treated as a final claim unless it is separately documented as such.

## Analytical scope

The current codebase includes workflows that:

1. ingest and clean Global Terrorism Database (GTD) event records;
2. identify formal Islamic State affiliates and candidate incidents using group labels and narrative information;
3. transform Iraqi district and ethnicity spatial data;
4. combine ethnicity fragments with LandScan population information to estimate district-level population-weighted ethnic composition;
5. calculate district-level composition measures, including ethnic-group shares and fractionalization;
6. construct district-month analytical panels; and
7. generate diagnostic and publication-oriented visualizations relating attack patterns to demographic context.

## Repository map

| Path | Role |
|---|---|
| `scripts/` | Modular import, cleaning, validation, transformation, and helper scripts |
| `03) Heatmap - Ethnicity Population Proportion vs Attack Frequency by Month.R` | Population-weighted district-month panel and heatmap workflow |
| `04) Targetting Composition Plot.R` | Target-composition visualization workflow |
| `GTD Cleaning.R` | GTD ingestion and Islamic State identification/filtering logic |
| `GTD Manipulation.R` | Event-data cleaning, typing, transformation, and derived variables |
| `Iraq Ethnicity Cleaning.R` | ESOC/LandScan district-level ethnicity transformation |
| `data/dictionary/` | Data schemas and variable dictionaries |
| `data/intermediate/` | Intermediate analytical objects |
| `data/miscellaneous/` | Supporting documentation, including GTD reference material |

Several top-level `Untitled-*.R` and other working scripts are retained for provenance and experimentation. They should not be assumed to be canonical entry points.

## Reproducibility

The analytical code is written primarily in **R** and uses tidyverse-style pipelines. Spatial work uses `sf`; individual workflows may require additional spatial/data packages depending on the entry point.

A typical reproducibility path is:

```text
raw source data
    ↓
import + validation
    ↓
clean event and spatial data
    ↓
population-weighted demographic transformation
    ↓
spatial/event joins
    ↓
district-month analytical panel
    ↓
diagnostics + figures
```

The heatmap workflow explicitly validates expected source files, creates reproducibility indexes, exports data dictionaries, and performs data-quality checks before downstream analysis.

## Data provenance and use

The code references external data sources including:

- **Global Terrorism Database (GTD)** event data;
- Iraqi district and ethnicity spatial data associated with **ESOC** materials; and
- **LandScan 2008** population raster data.

These source datasets may be governed by their own licenses, EULAs, citation requirements, or redistribution restrictions. Review the original source terms and the supporting documentation in this repository before redistributing source data or derived products.

## Research integrity

This repository emphasizes auditability over polished appearance. Intermediate files, validation outputs, dictionaries, and working scripts are intentionally visible where useful for reconstructing analytical decisions.

Current limitations include an evolving directory structure and the coexistence of refactored and legacy scripts. Future cleanup should preserve provenance while consolidating canonical entry points.

## Citation

If code from this repository materially contributes to published work, cite the specific repository version or commit used and separately cite the underlying source datasets according to their respective requirements.

---

**John S. Lystad**  
[Website](https://lystadjs.github.io/) · [Research](https://lystadjs.github.io/research.html) · [Code & Development](https://lystadjs.github.io/code.html) · [GitHub](https://github.com/LystadJS)
