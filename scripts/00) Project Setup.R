# ========================================================================
# 00) Project Setup.R
# Purpose: Load packages, create folders, and define global project objects.
# ========================================================================

# 01) Required packages ----------------------------------------------------

required_packages <- c(
  "tidyverse",
  "janitor",
  "mice",
  "sf",
  "broom",
  "broom.mixed",
  "skimr",
  "lubridate",
  "patchwork",
  "scales",
  "terra",
  "raster",
  "exactextractr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install these packages before running the project: ",
    paste(missing_packages, collapse = ", ")
  )
}

purrr::walk(
  required_packages,
  ~ suppressPackageStartupMessages(library(.x, character.only = TRUE))
)

# 02) Project folders ------------------------------------------------------

project_directories <- c(
  "data",
  "data/raw",
  "data/raw/csv",
  "data/raw/dat",
  "data/raw/shp",
  "data/raw/xlsx",
  "data/intermediate",
  "data/clean",
  "data/dictionary",
  "outputs",
  "outputs/tables",
  "outputs/plots",
  "logs",
  "scripts"
)

purrr::walk(
  project_directories,
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# 03) Common missing-value tokens -----------------------------------------

common_missing_tokens <- c(
  "",
  " ",
  ".",
  "..",
  "...",
  "NA",
  "N/A",
  "Na",
  "na",
  "n/a",
  "NULL",
  "Null",
  "null",
  "NONE",
  "None",
  "none",
  "MISSING",
  "Missing",
  "missing",
  "UNKNOWN",
  "Unknown",
  "unknown",
  "UNK",
  "unk",
  "NOT AVAILABLE",
  "Not Available",
  "not available",
  "NOT APPLICABLE",
  "Not Applicable",
  "not applicable",
  "REFUSED",
  "Refused",
  "refused",
  "DON'T KNOW",
  "Don't know",
  "dont know",
  "DK",
  "dk",
  "-99",
  "-98",
  "-97",
  "-999",
  "-9999",
  "999",
  "9999",
  "99999"
)
saveRDS(common_missing_tokens, "data/intermediate/common_missing_tokens.rds")

# 04) Reproducibility options ---------------------------------------------

set.seed(1501211)

options(
  mc.cores = parallel::detectCores()
)
