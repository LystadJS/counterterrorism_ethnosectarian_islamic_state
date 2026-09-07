# ========================================================================
# 18) Pairwise Missingness Co-occurrence.R
# Purpose: Summarize pairwise missingness overlap between variables.
# ========================================================================

pairwise_missingness_table <- function(data) {
  missing_matrix <- is.na(data)
  co_missing_matrix <- t(missing_matrix) %*% missing_matrix

  as.data.frame(as.table(co_missing_matrix)) |>
    tibble::as_tibble() |>
    dplyr::rename(
      variable_1 = Var1,
      variable_2 = Var2,
      co_missing_count = Freq
    ) |>
    dplyr::mutate(co_missing_rate = co_missing_count / nrow(data)) |>
    dplyr::arrange(dplyr::desc(co_missing_count))
}

plot_pairwise_missingness <- function(data) {
  plot_data <- pairwise_missingness_table(data)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = variable_1, y = variable_2, fill = co_missing_rate)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient(low = "white", high = "black") +
    ggplot2::labs(
      title = "Pairwise Missingness Co-occurrence",
      x = NULL,
      y = NULL,
      fill = "Co-missing rate"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
