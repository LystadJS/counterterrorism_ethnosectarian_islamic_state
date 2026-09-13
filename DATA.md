# Data provenance and governance

This project combines several classes of research data. The repository distinguishes **source data**, **derived analytical data**, **review artifacts**, and **documentation** so that provenance and redistribution decisions remain explicit.

## External source families

The current workflows reference data from sources including:

- **Global Terrorism Database (GTD)** event records;
- Iraqi district and ethnicity spatial materials associated with **ESOC** resources; and
- **LandScan 2008** population data.

Each source can have its own licensing, EULA, citation, registration, attribution, or redistribution requirements. Those terms remain controlling even when code in this repository reads or transforms the data.

## Repository policy

### `data/raw/`

Raw third-party source data should be placed here locally and are ignored by the publication-facing repository configuration. A placeholder README documents expected locations.

### `data/intermediate/`

Intermediate objects are retained where they materially improve auditability or allow reconstruction of analytical decisions. Their presence does not imply that they are authoritative final products.

### `data/processed/`

Derived analytical datasets currently used by the project. Before any tagged release, each file should be checked for whether its contents may be redistributed under the source-data terms.

### `data/review/`

Derived review files used in classification or manual-inspection workflows. These can contain source-derived text or fields and therefore require the same redistribution review as other derived products.

### `data/dictionary/`

Schemas, dictionaries, and field documentation used to make transformations inspectable.

### `data/miscellaneous/`

Supporting source documentation retained for provenance. Third-party documents remain subject to their original rights and terms.

## Redistribution audit required before release

Before creating a citable/tagged research release:

1. identify every tracked file derived from restricted or registered source data;
2. verify whether that derived file may be redistributed publicly;
3. remove or replace restricted material with generation instructions where necessary;
4. document source versions, access dates, geographic/temporal coverage, and exclusions;
5. document any manual labeling/classification procedures; and
6. separately cite each underlying dataset.

This refactor does **not** make a legal determination about the redistribution status of existing derived files. It makes that unresolved question visible rather than concealing it.

## Expected local raw-data layout

The population-weighted workflow currently expects inputs such as:

```text
data/raw/csv/GTD_A.csv
data/raw/Iraq Ethnicity.zip
data/raw/LandScan_2008.tif
```

Additional modular import scripts also support `data/raw/dat/`, `data/raw/shp/`, and `data/raw/xlsx/`.

See `REPRODUCIBILITY.md` for execution guidance and `RIGHTS.md` for the repository's current rights position.
