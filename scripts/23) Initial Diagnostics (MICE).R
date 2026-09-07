# ========================================================================
# 23) Initial Diagnostics (MICE).R
# Purpose: Create mice missingness pattern tables and ggplot alternatives.
# ========================================================================

run_mice_md_pattern_table <- function(data) {
  mice::md.pattern(data, plot = FALSE)
}

make_mice_md_pattern_ggplot_data <- function(data) {
  md_matrix <- mice::md.pattern(data, plot = FALSE)

  md_df <- as.data.frame(md_matrix) |>
    tibble::rownames_to_column("pattern_row") |>
    dplyr::mutate(pattern_id = dplyr::row_number())

  variable_columns <- names(data)
  variable_columns <- variable_columns[variable_columns %in% names(md_df)]

  md_df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variable_columns),
      names_to = "variable",
      values_to = "observed_flag"
    ) |>
    dplyr::mutate(
      observed_status = dplyr::if_else(observed_flag == 1, "Observed", "Missing"),
      pattern_label = paste0("Pattern ", pattern_id)
    )
}

plot_mice_md_pattern_ggplot <- function(data) {
  plot_data <- make_mice_md_pattern_ggplot_data(data)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = variable, y = pattern_label, fill = observed_status)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::labs(
      title = "mice::md.pattern() Recreated in ggplot",
      x = "Variable",
      y = "Pattern",
      fill = "Cell status"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
