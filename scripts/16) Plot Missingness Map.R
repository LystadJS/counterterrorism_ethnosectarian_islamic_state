# ========================================================================
# 16) Plot Missingness Map.R
# Purpose: Plot cell-level missingness as a row-by-variable map.
# ========================================================================

make_missingness_map_data <- function(data) {
  data |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = -row_id,
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(missing_status = dplyr::if_else(is.na(value), "Missing", "Observed"))
}

plot_missingness_map <- function(data) {
  plot_data <- make_missingness_map_data(data)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = variable, y = row_id, fill = missing_status)
  ) +
    ggplot2::geom_tile() +
    ggplot2::labs(
      title = "Missingness Map",
      x = "Variable",
      y = "Row",
      fill = "Status"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
