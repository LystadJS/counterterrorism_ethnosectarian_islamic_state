# ========================================================================
# 05) Import Data (SHP).R
# Purpose: Import shapefiles while preserving geometry.
# ========================================================================

read_shapefile_clean <- function(
  file_path,
  target_crs = NULL,
  clean_names = TRUE
) {
  spatial_data <- sf::st_read(file_path, quiet = TRUE)

  if (isTRUE(clean_names)) {
    spatial_data <- janitor::clean_names(spatial_data)
  }

  if (!is.null(target_crs)) {
    spatial_data <- sf::st_transform(spatial_data, crs = target_crs)
  }

  spatial_data |>
    dplyr::mutate(
      source_file = basename(file_path),
      source_path = file_path,
      .before = 1
    )
}

read_many_shapefiles <- function(folder = "data/raw/shp", target_crs = NULL) {
  shp_files <- list.files(
    path = folder,
    pattern = "\\.shp$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(shp_files) == 0) {
    stop("No .shp files found in: ", folder)
  }

  shp_list <- purrr::map(
    shp_files,
    ~ read_shapefile_clean(.x, target_crs = target_crs)
  )

  names(shp_list) <- basename(shp_files)
  shp_list
}

audit_sf_object <- function(sf_data) {
  tibble::tibble(
    rows = nrow(sf_data),
    columns = ncol(sf_data),
    geometry_type = paste(unique(as.character(sf::st_geometry_type(sf_data))), collapse = " | "),
    crs_input = sf::st_crs(sf_data)$input,
    crs_epsg = sf::st_crs(sf_data)$epsg,
    invalid_geometry_count = sum(!sf::st_is_valid(sf_data)),
    empty_geometry_count = sum(sf::st_is_empty(sf_data))
  )
}
