# ========================================================================
# 04) Import Data (.DAT).R
# Purpose: Import fixed-width .dat files using an explicit schema.
# ========================================================================

read_fwf_all_character <- function(file_path, schema, clean_names = TRUE) {
  fwf_positions <- readr::fwf_widths(
    widths = schema$width,
    col_names = schema$column_name
  )

  imported_data <- readr::read_fwf(
    file = file_path,
    col_positions = fwf_positions,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(),
    progress = FALSE
  )

  if (isTRUE(clean_names)) {
    imported_data <- janitor::clean_names(imported_data)
  }

  imported_data |>
    dplyr::mutate(
      source_file = basename(file_path),
      source_path = file_path,
      .before = 1
    )
}
