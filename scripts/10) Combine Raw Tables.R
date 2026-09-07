# ========================================================================
# 10) Combine Raw Tables.R
# Purpose: Compare and bind raw imported tables.
# ========================================================================

bind_raw_tables <- function(data_list, id_column = "source_table") {
  dplyr::bind_rows(data_list, .id = id_column)
}

compare_table_schemas <- function(data_list) {
  purrr::imap_dfr(
    data_list,
    ~ tibble::tibble(
      table_name = .y,
      column_name = names(.x),
      column_type = purrr::map_chr(.x, ~ paste(class(.x), collapse = " | "))
    )
  ) |>
    dplyr::arrange(column_name, table_name)
}

make_column_presence_matrix <- function(data_list) {
  schema_table <- compare_table_schemas(data_list)

  schema_table |>
    dplyr::mutate(present = TRUE) |>
    dplyr::select(table_name, column_name, present) |>
    tidyr::pivot_wider(
      names_from = table_name,
      values_from = present,
      values_fill = FALSE
    ) |>
    dplyr::arrange(column_name)
}
