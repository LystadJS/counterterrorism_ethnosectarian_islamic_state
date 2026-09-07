# ========================================================================
# 22) Initial Diagnostics (SHP).R
# Purpose: Summarize and visualize sf shapefile objects.
# ========================================================================

sf_diagnostic_summary <- function(sf_data) {
  tibble::tibble(
    rows = nrow(sf_data),
    columns = ncol(sf_data),
    geometry_column = attr(sf_data, "sf_column"),
    geometry_types = paste(unique(as.character(sf::st_geometry_type(sf_data))), collapse = " | "),
    crs_input = sf::st_crs(sf_data)$input,
    crs_epsg = sf::st_crs(sf_data)$epsg,
    invalid_geometry_count = sum(!sf::st_is_valid(sf_data)),
    empty_geometry_count = sum(sf::st_is_empty(sf_data)),
    bbox_xmin = sf::st_bbox(sf_data)[["xmin"]],
    bbox_ymin = sf::st_bbox(sf_data)[["ymin"]],
    bbox_xmax = sf::st_bbox(sf_data)[["xmax"]],
    bbox_ymax = sf::st_bbox(sf_data)[["ymax"]]
  )
}

sf_attribute_missingness <- function(sf_data) {
  attribute_data <- sf_data |>
    sf::st_drop_geometry()

  tibble::tibble(
    variable = names(attribute_data),
    missing_count = purrr::map_int(attribute_data, ~ sum(is.na(.x))),
    missing_rate = purrr::map_dbl(attribute_data, ~ mean(is.na(.x))),
    unique_count = purrr::map_int(attribute_data, ~ dplyr::n_distinct(.x, na.rm = TRUE))
  ) |>
    dplyr::arrange(dplyr::desc(missing_rate), variable)
}

plot_sf_geometry <- function(sf_data, fill_variable = NULL) {
  if (is.null(fill_variable)) {
    return(
      ggplot2::ggplot(sf_data) +
        ggplot2::geom_sf() +
        ggplot2::labs(title = "Initial Shapefile Geometry Plot") +
        ggplot2::theme_classic()
    )
  }

  fill_variable <- rlang::ensym(fill_variable)

  ggplot2::ggplot(sf_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = !!fill_variable)) +
    ggplot2::labs(
      title = paste("Initial Shapefile Map:", rlang::as_label(fill_variable)),
      fill = rlang::as_label(fill_variable)
    ) +
    ggplot2::theme_classic()
}
