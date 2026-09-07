# ========================================================================
# 03) Import Data (Delimited).R
# Purpose: Import delimited text files as all-character data.
# ========================================================================

read_delimited_all_character <- function(
  file_path,
  delim = NULL,
  clean_names = TRUE
) {
  if (is.null(delim)) {
    delim <- dplyr::if_else(
      stringr::str_detect(stringr::str_to_lower(file_path), "\\.tsv$"),
      "\t",
      ","
    )
  }

  imported_data <- readr::read_delim(
    file = file_path,
    delim = delim,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(),
    trim_ws = FALSE,
    show_col_types = FALSE,
    progress = FALSE
  )

  if (isTRUE(clean_names)) {
    imported_data <- janitor::clean_names(imported_data)
  }

  imported_data |>
    dplyr::mutate(
      source_file = basename(file_path),
      source_path = file_path,
      source_delimiter = delim,
      .before = 1
    )
}

read_many_delimited_all_character <- function(
  folder = "data/raw/dat",
  pattern = "\\.(dat|txt|tsv)$",
  delim = NULL
) {
  files <- list.files(
    path = folder,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    stop("No delimited .dat/.txt/.tsv files found in: ", folder)
  }

  data_list <- purrr::map(files, ~ read_delimited_all_character(.x, delim = delim))
  names(data_list) <- basename(files)

  data_list
}
