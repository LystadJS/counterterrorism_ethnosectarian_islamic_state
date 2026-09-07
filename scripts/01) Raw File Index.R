# ========================================================================
# 01) Raw File Index.R
# Purpose: Build a file inventory for raw data folders.
# ========================================================================

make_raw_file_index <- function(raw_data_root = "data/raw") {
  raw_files <- list.files(
    path = raw_data_root,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )

  tibble::tibble(file_path = raw_files) |>
    dplyr::mutate(
      file_name = basename(file_path),
      file_extension = stringr::str_to_lower(tools::file_ext(file_path)),
      file_size_bytes = file.info(file_path)$size,
      modified_time = file.info(file_path)$mtime,
      parent_folder = dirname(file_path)
    ) |>
    dplyr::arrange(file_extension, file_name)
}

write_raw_file_index <- function(
  raw_data_root = "data/raw",
  csv_path = "logs/raw_file_index.csv",
  rds_path = "data/intermediate/raw_file_index.rds"
) {
  raw_file_index <- make_raw_file_index(raw_data_root)

  readr::write_csv(raw_file_index, csv_path)
  saveRDS(raw_file_index, rds_path)

  raw_file_index
}
