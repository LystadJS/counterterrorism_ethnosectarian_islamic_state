# ========================================================================
# 25) Load Helper Scripts.R
# Purpose: Source helper scripts without running the analysis pipeline.
# ========================================================================

helper_scripts <-
  c(
    "01) Raw File Index.R",
    "02) Import Data (CSV).R",
    "03) Import Data (Delimited).R",
    "04) Import Data (DAT).R",
    "05) Import Data (SHP).R",
    "06) Import Data (XLSX).R",
    "07) Standardize Column Names.R",
    "08) Clean Missing Values.R",
    "09) Clean Strings.R",
    "10) Combine Raw Tables.R",
    "11) Schema Inventory and Column Audit.R",
    "12) Duplicate Audit.R",
    "13) Data Type Guessing.R",
    "14) Data Type Conversion.R",
    "15) Missingness Summary Tables.R",
    "16) Plot Missingness Map.R",
    "17) Plot Missingness Heatmap.R",
    "18) Pairwise Missingness Co-occurrence.R",
    "19) Initial Diagnostics (Numeric).R",
    "20) Initial Diagnostics (Categorical).R",
    "21) Initial Diagnostics (Date-Time).R",
    "22) Initial Diagnostics (SHP).R",
    "23) Initial Diagnostics (MICE).R",
    "24) Export Cleaned Data.R",
    "26) ISIS District Panel Helpers.R"
  )

purrr::walk(
  file.path("scripts", helper_scripts),
  source
)
