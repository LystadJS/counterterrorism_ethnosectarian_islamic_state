# ========================================================================
# 02) Import Data (CSV).R
# Purpose: Import CSV files with all columns forced to character.
# ========================================================================

read_csv_all_character <- function(file_path, clean_names = TRUE) {
  imported_data <-
    readr::read_csv(
      file = file_path,
      col_types = readr::cols(.default = readr::col_character()),
      na = character(),
      show_col_types = FALSE,
      progress = FALSE
    )
  if (isTRUE(clean_names)) {
    imported_data <-
      janitor::clean_names(imported_data)
  }
  imported_data |>
    dplyr::mutate(
      source_file = basename(file_path),
      source_path = file_path,
      .before = 1
    )
}

read_many_csvs_all_character <- function(
  folder = "data/raw/csv",
  recursive = TRUE,
  clean_names = TRUE
) {
  csv_files <-
    list.files(
      path = folder,
      pattern = "\\.csv$",
      recursive = recursive,
      full.names = TRUE,
      ignore.case = TRUE
    )
  if (length(csv_files) == 0) {
    stop("No CSV files found in: ", folder)
  }

  csv_list <-
    purrr::map(
      csv_files,
      ~ read_csv_all_character(.x, clean_names = clean_names)
    )
  names(csv_list) <-
    basename(csv_files)
  csv_list
}
