# ========================================================================
# 19) Initial Diagnostics (Numeric).R
# Purpose: Summarize and visualize numeric variables.
# ========================================================================

numeric_summary_table <- function(data) {
  data |>
    dplyr::select(dplyr::where(is.numeric)) |>
    purrr::imap_dfr(
      ~ tibble::tibble(
        variable = .y,
        missing_count = sum(is.na(.x)),
        mean = mean(.x, na.rm = TRUE),
        sd = stats::sd(.x, na.rm = TRUE),
        min = min(.x, na.rm = TRUE),
        median = stats::median(.x, na.rm = TRUE),
        max = max(.x, na.rm = TRUE)
      )
    )
}

plot_numeric_histograms <- function(data) {
  plot_data <- data |>
    dplyr::select(dplyr::where(is.numeric)) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "value"
    )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free") +
    ggplot2::labs(title = "Numeric Variable Distributions") +
    ggplot2::theme_classic()
}

plot_numeric_boxplots <- function(data) {
  plot_data <- data |>
    dplyr::select(dplyr::where(is.numeric)) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "value"
    )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = variable, y = value)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = "Numeric Variable Boxplots", x = NULL, y = "Value") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
