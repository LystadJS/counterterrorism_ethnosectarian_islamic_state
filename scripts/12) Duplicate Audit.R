# ========================================================================
# 12) Duplicate Audit.R
# Purpose: Audit row, single-key, and composite-key duplicates.
# ========================================================================

audit_duplicate_rows <- function(data) {
  tibble::tibble(
    rows = nrow(data),
    duplicate_rows = sum(duplicated(data)),
    duplicate_row_rate = mean(duplicated(data))
  )
}

audit_key_column <- function(data, key_column) {
  key_column <- rlang::ensym(key_column)

  key_data <- data |>
    dplyr::count(!!key_column, name = "key_count")

  tibble::tibble(
    key_column = rlang::as_label(key_column),
    rows = nrow(data),
    unique_keys = dplyr::n_distinct(dplyr::pull(data, !!key_column), na.rm = TRUE),
    missing_keys = sum(is.na(dplyr::pull(data, !!key_column))),
    duplicated_keys = sum(key_data$key_count > 1, na.rm = TRUE)
  )
}

audit_composite_key <- function(data, key_columns) {
  key_data <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(key_columns)), name = "key_count")

  tibble::tibble(
    key_columns = paste(key_columns, collapse = " + "),
    rows = nrow(data),
    unique_keys = nrow(key_data),
    duplicated_keys = sum(key_data$key_count > 1, na.rm = TRUE),
    maximum_key_count = max(key_data$key_count, na.rm = TRUE)
  )
}

show_duplicate_keys <- function(data, key_columns) {
  data |>
    dplyr::count(dplyr::across(dplyr::all_of(key_columns)), name = "key_count") |>
    dplyr::filter(key_count > 1) |>
    dplyr::arrange(dplyr::desc(key_count))
}
