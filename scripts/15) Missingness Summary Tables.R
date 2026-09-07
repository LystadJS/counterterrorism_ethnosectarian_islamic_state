# ========================================================================
# 15) Missingness Summary Tables.R
# Purpose: Summarize missingness by variable and by row.
# ========================================================================

missingness_by_variable <- function(data) {
  tibble::tibble(
    variable = names(data),
    missing_count = purrr::map_int(data, ~ sum(is.na(.x))),
    missing_rate = purrr::map_dbl(data, ~ mean(is.na(.x))),
    observed_count = nrow(data) - missing_count
  ) |>
    dplyr::arrange(dplyr::desc(missing_rate), variable)
}

missingness_by_row <- function(data) {
  tibble::tibble(
    row_number = seq_len(nrow(data)),
    missing_count = rowSums(is.na(data)),
    missing_rate = missing_count / ncol(data)
  ) |>
    dplyr::arrange(dplyr::desc(missing_count))
}

complete_case_summary <- function(data) {
  complete_rows <- stats::complete.cases(data)

  tibble::tibble(
    rows = nrow(data),
    columns = ncol(data),
    complete_rows = sum(complete_rows),
    incomplete_rows = sum(!complete_rows),
    complete_row_rate = mean(complete_rows),
    incomplete_row_rate = mean(!complete_rows)
  )
}
