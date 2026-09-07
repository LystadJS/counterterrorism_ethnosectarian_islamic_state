# ========================================================================
# 11) Schema Inventory and Column Audit.R
# Purpose: Produce lightweight schema and dataset audits.
# ========================================================================

schema_inventory <- function(data) {
  tibble::tibble(
    column_name = names(data),
    column_position = seq_along(data),
    column_class = purrr::map_chr(data, ~ paste(class(.x), collapse = " | ")),
    missing_count = purrr::map_int(data, ~ sum(is.na(.x))),
    missing_rate = purrr::map_dbl(data, ~ mean(is.na(.x))),
    unique_count = purrr::map_int(data, ~ dplyr::n_distinct(.x, na.rm = TRUE))
  )
}

character_length_audit <- function(data) {
  data |>
    dplyr::select(dplyr::where(is.character)) |>
    purrr::imap_dfr(
      ~ tibble::tibble(
        column_name = .y,
        min_length = min(nchar(.x), na.rm = TRUE),
        median_length = stats::median(nchar(.x), na.rm = TRUE),
        max_length = max(nchar(.x), na.rm = TRUE)
      )
    )
}

dataset_inventory <- function(data) {
  tibble::tibble(
    rows = nrow(data),
    columns = ncol(data),
    duplicate_rows = sum(duplicated(data)),
    complete_rows = sum(stats::complete.cases(data)),
    complete_row_rate = mean(stats::complete.cases(data))
  )
}
