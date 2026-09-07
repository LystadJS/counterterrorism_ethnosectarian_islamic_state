# ========================================================================
# 17) Plot Missingness Heatmap.R
# Purpose: Recreate a mice-style missingness-pattern heatmap in ggplot.
# ========================================================================

make_md_pattern_table <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(is.na(.x), 0L, 1L)
      )
    ) |>
    dplyr::count(dplyr::across(dplyr::everything()), name = "pattern_count") |>
    dplyr::arrange(dplyr::desc(pattern_count)) |>
    dplyr::mutate(pattern_id = dplyr::row_number(), .before = 1)
}

make_md_pattern_plot_data <- function(data) {
  pattern_table <- make_md_pattern_table(data)

  pattern_table |>
    tidyr::pivot_longer(
      cols = -c(pattern_id, pattern_count),
      names_to = "variable",
      values_to = "observed_flag"
    ) |>
    dplyr::mutate(
      observed_status = dplyr::if_else(observed_flag == 1L, "Observed", "Missing"),
      pattern_label = paste0("Pattern ", pattern_id, " (n=", pattern_count, ")")
    )
}

plot_md_pattern_ggplot <- function(data) {
  plot_data <- make_md_pattern_plot_data(data)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = variable, y = pattern_label, fill = observed_status)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::labs(
      title = "Missingness Pattern Heatmap",
      x = "Variable",
      y = "Missingness Pattern",
      fill = "Cell Status"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

summarize_md_patterns <- function(data) {
  make_md_pattern_table(data)
}
