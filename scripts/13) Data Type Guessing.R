# ========================================================================
# 13) Data Type Guessing.R
# Purpose: Heuristically identify likely variable types before conversion.
# ========================================================================

looks_numeric <- function(x) {
  x <- stats::na.omit(as.character(x))
  if (length(x) == 0) return(FALSE)
  mean(!is.na(readr::parse_double(x, na = character()))) > 0.95
}

looks_integer <- function(x) {
  x <- stats::na.omit(as.character(x))
  if (length(x) == 0) return(FALSE)
  parsed <- readr::parse_double(x, na = character())
  mean(!is.na(parsed) & parsed == floor(parsed)) > 0.95
}

looks_binary <- function(x) {
  values <- unique(stats::na.omit(as.character(x)))
  allowed <- c("0", "1", "TRUE", "FALSE", "true", "false", "Yes", "No", "yes", "no")
  length(values) > 0 && all(values %in% allowed)
}

looks_date <- function(x) {
  x <- stats::na.omit(as.character(x))
  if (length(x) == 0) return(FALSE)
  parsed <- suppressWarnings(lubridate::ymd(x))
  mean(!is.na(parsed)) > 0.80
}

guess_column_type <- function(x) {
  dplyr::case_when(
    looks_binary(x) ~ "binary",
    looks_integer(x) ~ "integer",
    looks_numeric(x) ~ "double",
    looks_date(x) ~ "date",
    TRUE ~ "character"
  )
}

type_guessing_report <- function(data) {
  tibble::tibble(
    column_name = names(data),
    current_class = purrr::map_chr(data, ~ paste(class(.x), collapse = " | ")),
    guessed_type = purrr::map_chr(data, guess_column_type),
    missing_count = purrr::map_int(data, ~ sum(is.na(.x))),
    unique_count = purrr::map_int(data, ~ dplyr::n_distinct(.x, na.rm = TRUE))
  )
}
