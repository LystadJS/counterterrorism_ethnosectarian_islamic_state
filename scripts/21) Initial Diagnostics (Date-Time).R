# ========================================================================
# 21) Initial Diagnostics (Date-Time).R
# Purpose: Summarize and visualize date variables.
# ========================================================================

date_summary_table <- function(data) {
  date_data <- data |>
    dplyr::select(dplyr::where(~ inherits(.x, "Date") || inherits(.x, "POSIXct")))

  tibble::tibble(
    variable = names(date_data),
    missing_count = purrr::map_int(date_data, ~ sum(is.na(.x))),
    minimum_date = purrr::map_chr(date_data, ~ as.character(min(.x, na.rm = TRUE))),
    maximum_date = purrr::map_chr(date_data, ~ as.character(max(.x, na.rm = TRUE))),
    unique_count = purrr::map_int(date_data, ~ dplyr::n_distinct(.x, na.rm = TRUE))
  )
}

plot_date_counts <- function(data, date_variable, date_unit = "month") {
  date_variable <- rlang::ensym(date_variable)

  plot_data <- data |>
    dplyr::mutate(date_floor = lubridate::floor_date(!!date_variable, unit = date_unit)) |>
    dplyr::count(date_floor, name = "n")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = date_floor, y = n)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1) +
    ggplot2::labs(
      title = paste("Record Counts by", stringr::str_to_title(date_unit)),
      x = "Date",
      y = "Count"
    ) +
    ggplot2::theme_classic()
}

date_gap_diagnostic <- function(data, date_variable) {
  date_variable <- rlang::ensym(date_variable)

  data |>
    dplyr::distinct(!!date_variable) |>
    dplyr::arrange(!!date_variable) |>
    dplyr::mutate(
      previous_date = dplyr::lag(!!date_variable),
      gap_days = as.numeric(!!date_variable - previous_date)
    ) |>
    dplyr::filter(!is.na(gap_days)) |>
    dplyr::arrange(dplyr::desc(gap_days))
}
