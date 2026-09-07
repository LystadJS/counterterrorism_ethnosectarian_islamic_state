# ========================================================================
# 24) Export Cleaned Data.R
# Purpose: Export cleaned data, dictionaries, tables, and plots.
# ========================================================================

ensure_parent_directory <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

export_cleaned_data <- function(data, base_name = "cleaned_data") {
  csv_path <-
    file.path("data/clean", paste0(base_name, ".csv"))
  rds_path <-
    file.path("data/clean", paste0(base_name, ".rds"))

  ensure_parent_directory(csv_path)
  ensure_parent_directory(rds_path)

  readr::write_csv(data, csv_path, na = "")
  saveRDS(data, rds_path)

  tibble::tibble(
    csv_path = csv_path,
    rds_path = rds_path,
    rows = nrow(data),
    columns = ncol(data)
  )
}

export_data_dictionary <- function(
  schema_table,
  base_name = "data_dictionary"
) {
  output_path <-
    file.path("data/dictionary", paste0(base_name, ".csv"))
  ensure_parent_directory(output_path)

  readr::write_csv(schema_table, output_path)

  tibble::tibble(output_path = output_path)
}

export_table <- function(data, file_name) {
  output_path <-
    file.path("outputs/tables", file_name)
  ensure_parent_directory(output_path)

  readr::write_csv(data, output_path)

  tibble::tibble(
    output_path = output_path,
    rows = nrow(data),
    columns = ncol(data)
  )
}

export_plot <- function(
  plot_object,
  file_name,
  width = 10,
  height = 7,
  dpi = 300
) {
  output_path <-
    file.path("outputs/plots", file_name)
  ensure_parent_directory(output_path)

  ggplot2::ggsave(
    filename = output_path,
    plot = plot_object,
    width = width,
    height = height,
    dpi = dpi
  )

  tibble::tibble(output_path = output_path)
}
