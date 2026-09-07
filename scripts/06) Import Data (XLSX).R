# ========================================================================
# 06) Import Data (XLSX).R
# Purpose: Import Excel worksheets as all-character data.
# ========================================================================

read_excel_sheet_all_character <- function(
  file_path,
  sheet = 1,
  clean_names = TRUE
) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install the readxl package before importing Excel files.")
  }

  imported_data <- readxl::read_excel(
    path = file_path,
    sheet = sheet,
    col_types = "text"
  )

  if (isTRUE(clean_names)) {
    imported_data <- janitor::clean_names(imported_data)
  }

  imported_data |>
    dplyr::mutate(
      source_file = basename(file_path),
      source_path = file_path,
      source_sheet = as.character(sheet),
      .before = 1
    )
}

read_excel_workbook_all_character <- function(file_path, clean_names = TRUE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install the readxl package before importing Excel files.")
  }

  sheet_names <- readxl::excel_sheets(file_path)

  sheet_list <- purrr::map(
    sheet_names,
    ~ read_excel_sheet_all_character(
      file_path = file_path,
      sheet = .x,
      clean_names = clean_names
    )
  )

  names(sheet_list) <- sheet_names
  sheet_list
}
