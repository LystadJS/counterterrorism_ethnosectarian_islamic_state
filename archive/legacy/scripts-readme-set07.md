# Data Import, Cleaning, and Initial EDA Scripts — Set 07

This bundle contains modular R scripts for the first stage of applied data work.

The guiding rule is:

> Import first as character. Audit second. Convert types third.

This prevents R from silently guessing bad column types when raw files contain mixed encodings, missingness codes, strange IDs, leading zeroes, or inconsistent date formats.

## Main tasks covered

- Project folder setup
- Raw file indexing
- Multiple CSV import with all columns as character
- Delimited `.dat`, `.txt`, and `.tsv` import with all columns as character
- Fixed-width `.dat` import with codebook-driven widths
- Shapefile import with `sf`
- Optional Excel import as character
- Column-name standardization
- Common missingness-token cleanup
- String trimming and whitespace cleaning
- Combining raw tables with source metadata
- Schema inventory
- Duplicate row and duplicate key checks
- Type guessing reports
- Controlled type conversion
- Missingness summary tables
- ggplot missingness map
- ggplot recreation of `mice::md.pattern()` logic
- Pairwise missingness co-occurrence
- Numeric distribution plots
- Categorical attribute plots
- Date/time diagnostics
- Shapefile diagnostics
- Optional MICE diagnostics
- Clean-data and log export
- Master initial pipeline template

## Recommended workflow

1. Put raw CSV files in `data/raw/csv/`.
2. Put raw `.dat`, `.txt`, or `.tsv` files in `data/raw/dat/`.
3. Put shapefiles in `data/raw/shp/`.
4. Run `00) Project Setup.R`.
5. Run the individual import script for your file type.
6. Run missingness cleanup.
7. Run schema and missingness diagnostics.
8. Review the type guessing report.
9. Convert types only after review.
10. Export cleaned data.

## Important scripts

- `02_import_csvs_as_character.R`
- `08_clean_common_missing_values.R`
- `13_type_guessing_report.R`
- `14_controlled_type_conversion.R`
- `16_ggplot_missingness_map.R`
- `17_ggplot_md_pattern_replica.R`
- `25_master_initial_data_pipeline_template.R`

## Important note

The `17_ggplot_md_pattern_replica.R` script intentionally converts each variable to observed/missing flags before pivoting. This avoids the common `pivot_longer()` error caused by trying to combine numeric, character, date, and factor columns into one value column.
