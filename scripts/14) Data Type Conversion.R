# ========================================================================
# 14) Data Type Conversion.R
# Purpose: Convert selected columns according to an explicit plan.
# ========================================================================

parse_double_safe <- function(x) {
  readr::parse_double(as.character(x), na = c("", "NA", "N/A"))
}

parse_integer_safe <- function(x) {
  as.integer(readr::parse_double(as.character(x), na = c("", "NA", "N/A")))
}

parse_binary_safe <- function(x) {
  x_clean <- stringr::str_to_lower(stringr::str_squish(as.character(x)))

  dplyr::case_when(
    x_clean %in% c("1", "true", "yes", "y") ~ 1L,
    x_clean %in% c("0", "false", "no", "n") ~ 0L,
    TRUE ~ NA_integer_
  )
}

parse_date_safe <- function(x) {
  suppressWarnings(lubridate::ymd(x))
}

convert_columns_by_plan <- function(data, type_plan) {
  converted_data <- data

  for (row_number in seq_len(nrow(type_plan))) {
    column_name <- type_plan$column_name[[row_number]]
    target_type <- type_plan$target_type[[row_number]]

    if (!column_name %in% names(converted_data)) {
      warning("Skipping missing column in type plan: ", column_name)
      next
    }

    converted_data[[column_name]] <- switch(
      target_type,
      character = as.character(converted_data[[column_name]]),
      double = parse_double_safe(converted_data[[column_name]]),
      numeric = parse_double_safe(converted_data[[column_name]]),
      integer = parse_integer_safe(converted_data[[column_name]]),
      binary = parse_binary_safe(converted_data[[column_name]]),
      date = parse_date_safe(converted_data[[column_name]]),
      converted_data[[column_name]]
    )
  }

  converted_data
}
