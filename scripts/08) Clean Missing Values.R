# ========================================================================
# 08) Clean Missing Values.R
# Purpose: Convert explicit missing-value tokens to true NA values.
# ========================================================================

clean_missing_value <- function(x, missing_tokens = common_missing_tokens) {
  if (!is.character(x)) {
    return(x)
  }

  x_trimmed <- stringr::str_squish(x)
  x_trimmed[x_trimmed %in% missing_tokens] <- NA_character_
  x_trimmed
}

clean_common_missing_values <- function(
  data,
  missing_tokens = common_missing_tokens
) {
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ clean_missing_value(.x, missing_tokens = missing_tokens)
      )
    )
}

clean_common_missing_values_in_list <- function(
  data_list,
  missing_tokens = common_missing_tokens
) {
  purrr::map(
    data_list,
    ~ clean_common_missing_values(.x, missing_tokens = missing_tokens)
  )
}
