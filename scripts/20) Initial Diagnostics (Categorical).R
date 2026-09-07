# ========================================================================
# 20) Initial Diagnostics (Categorical).R
# Purpose: Summarize and visualize categorical variables.
# ========================================================================

categorical_summary_table <- function(data) {
  data |>
    dplyr::select(dplyr::where(~ is.character(.x) || is.factor(.x))) |>
    purrr::imap_dfr(
      ~ tibble::tibble(
        variable = .y,
        missing_count = sum(is.na(.x)),
        missing_rate = mean(is.na(.x)),
        unique_count = dplyr::n_distinct(.x, na.rm = TRUE)
      )
    )
}

top_levels_table <- function(data, variable, n = 20) {
  variable <- rlang::ensym(variable)

  data |>
    dplyr::count(!!variable, sort = TRUE, name = "n") |>
    dplyr::mutate(percent = n / sum(n)) |>
    dplyr::slice_head(n = n)
}

plot_top_levels <- function(data, variable, n = 20) {
  variable <- rlang::ensym(variable)

  plot_data <- data |>
    dplyr::count(!!variable, sort = TRUE, name = "n") |>
    dplyr::slice_head(n = n)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = forcats::fct_reorder(as.factor(!!variable), n), y = n)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top", n, "Levels of", rlang::as_label(variable)),
      x = NULL,
      y = "Count"
    ) +
    ggplot2::theme_classic()
}
