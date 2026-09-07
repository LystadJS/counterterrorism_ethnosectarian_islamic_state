# ========================================================================
# 09) Clean Strings.R
# Purpose: Light character cleaning without changing substantive labels.
# ========================================================================

clean_character_strings <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ .x |>
          stringr::str_replace_all("\\u00A0", " ") |>
          stringr::str_squish()
      )
    )
}

lowercase_selected_columns <- function(data, columns) {
  data |>
    dplyr::mutate(
      dplyr::across(
        {{ columns }},
        stringr::str_to_lower
      )
    )
}

empty_strings_to_na <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ dplyr::na_if(.x, "")
      )
    )
}
