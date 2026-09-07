# ========================================================================
# 07) Standardize Column Names.R
# Purpose: Standardize column names and create name crosswalks.
# ========================================================================

standardize_column_names <- function(data) {
  janitor::clean_names(data)
}

standardize_column_names_in_list <- function(data_list) {
  purrr::map(data_list, standardize_column_names)
}

make_column_name_crosswalk <- function(data) {
  tibble::tibble(
    original_name = names(data),
    clean_name = janitor::make_clean_names(names(data))
  )
}
