# ========================================================================
# 26) ISIS District Panel Helpers.R
# Purpose: Project-specific helpers for GTD, Iraq districts, LandScan, and plots.
# ========================================================================

assert_file_exists <- function(path, path_label = "file") {
  if (is.na(path) || length(path) == 0 || !file.exists(path)) {
    stop("Missing ", path_label, ": ", path)
  }

  invisible(path)
}

find_required_file <- function(root, pattern, file_label) {
  matching_files <- list.files(
    path = root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(matching_files) == 0) {
    stop("Could not find ", file_label, " using pattern: ", pattern)
  }

  matching_files[[1]]
}

make_gtd_type_plan <- function() {
  tibble::tibble(
    column_name = c(
      "eventid",
      "iyear",
      "imonth",
      "iday",
      "latitude",
      "longitude",
      "success",
      "suicide",
      "nkill",
      "nwound"
    ),
    target_type = c(
      "character",
      "integer",
      "integer",
      "integer",
      "double",
      "double",
      "binary",
      "binary",
      "double",
      "double"
    )
  )
}

import_clean_gtd_iraq <- function(gtd_path) {
  gtd_missing_tokens <- c(
    "",
    " ",
    "NA",
    "N/A",
    "na",
    "n/a",
    "NULL",
    "null"
  )

  read_csv_all_character(file_path = gtd_path, clean_names = TRUE) |>
    clean_common_missing_values(missing_tokens = gtd_missing_tokens) |>
    clean_character_strings() |>
    convert_columns_by_plan(type_plan = make_gtd_type_plan()) |>
    dplyr::filter(country_txt == "Iraq") |>
    dplyr::mutate(
      event_year = iyear,
      event_month_number = dplyr::if_else(
        imonth >= 1 & imonth <= 12,
        imonth,
        NA_integer_
      ),
      event_day_clean = dplyr::if_else(
        is.na(iday) | iday < 1 | iday > 31,
        1L,
        iday
      ),
      event_date = lubridate::make_date(
        year = event_year,
        month = event_month_number,
        day = event_day_clean
      ),
      event_month = lubridate::floor_date(event_date, unit = "month"),
      nkill = dplyr::if_else(nkill < 0, NA_real_, nkill),
      nwound = dplyr::if_else(nwound < 0, NA_real_, nwound)
    )
}

summarize_gtd_iraq_quality <- function(gtd_iraq) {
  gtd_iraq |>
    dplyr::summarize(
      rows = dplyr::n(),
      first_year = min(event_year, na.rm = TRUE),
      last_year = max(event_year, na.rm = TRUE),
      missing_month = sum(is.na(event_month)),
      missing_latitude = sum(is.na(latitude)),
      missing_longitude = sum(is.na(longitude)),
      missing_coordinates = sum(is.na(latitude) | is.na(longitude)),
      .groups = "drop"
    )
}

clean_district_boundaries <- function(districts_raw_sf) {
  districts_raw_sf |>
    sf::st_make_valid() |>
    dplyr::transmute(
      district_id = adm3code,
      district_name = adm3name,
      governorate = adm2name,
      governorate_id = adm2code,
      district_area_km2 = as.numeric(area_km2),
      district_area_km2_geometry = as.numeric(sf::st_area(geometry)) / 1000000,
      population_total = as.numeric(total_pop),
      district_label = stringr::str_c(adm3name, " [", adm2name, "]"),
      geometry = geometry
    )
}

build_area_weighted_ethnicity <- function(ethnicity_raw_sf) {
  ethnicity_components <- ethnicity_raw_sf |>
    sf::st_make_valid() |>
    dplyr::mutate(
      district_id = adm3code,
      district_name = adm3name,
      governorate = adm2name,
      intersect_area_km2 = as.numeric(sf::st_area(geometry)) / 1000000,
      kurdish_area = dplyr::if_else(ethnicity == 1, intersect_area_km2, 0),
      shia_area = dplyr::if_else(shia == 1, intersect_area_km2, 0),
      sunni_area = dplyr::if_else(sunni == 1, intersect_area_km2, 0),
      christian_area = dplyr::if_else(christians == 1, intersect_area_km2, 0),
      turcoman_area = dplyr::if_else(turcomans == 1, intersect_area_km2, 0),
      mixed_shia_sunni_area = dplyr::if_else(
        mixed_sh_su == 1,
        intersect_area_km2,
        0
      ),
      shia_area_adjusted = shia_area + 0.50 * mixed_shia_sunni_area,
      sunni_area_adjusted = sunni_area + 0.50 * mixed_shia_sunni_area
    ) |>
    sf::st_drop_geometry()

  ethnicity_components |>
    dplyr::group_by(district_id, district_name, governorate) |>
    dplyr::summarize(
      intersect_area_total_km2 = sum(intersect_area_km2, na.rm = TRUE),
      kurdish_area = sum(kurdish_area, na.rm = TRUE),
      shia_area = sum(shia_area_adjusted, na.rm = TRUE),
      sunni_area = sum(sunni_area_adjusted, na.rm = TRUE),
      christian_area = sum(christian_area, na.rm = TRUE),
      turcoman_area = sum(turcoman_area, na.rm = TRUE),
      mixed_shia_sunni_area = sum(mixed_shia_sunni_area, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      kurdish_prop = kurdish_area / intersect_area_total_km2,
      shia_prop = shia_area / intersect_area_total_km2,
      sunni_prop = sunni_area / intersect_area_total_km2,
      christian_prop = christian_area / intersect_area_total_km2,
      turcoman_prop = turcoman_area / intersect_area_total_km2,
      mixed_shia_sunni_prop = mixed_shia_sunni_area / intersect_area_total_km2,
      other_prop = pmax(
        0,
        1 -
          kurdish_prop -
          shia_prop -
          sunni_prop -
          christian_prop -
          turcoman_prop
      )
    ) |>
    add_dominant_group_variables()
}

add_dominant_group_variables <- function(data) {
  data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dominant_group = c(
        "Kurdish",
        "Shia",
        "Sunni",
        "Christian",
        "Turcoman",
        "Other"
      )[which.max(c(
        kurdish_prop,
        shia_prop,
        sunni_prop,
        christian_prop,
        turcoman_prop,
        other_prop
      ))],
      dominant_group_prop = max(
        c(
          kurdish_prop,
          shia_prop,
          sunni_prop,
          christian_prop,
          turcoman_prop,
          other_prop
        ),
        na.rm = TRUE
      ),
      ethnic_fractionalization = 1 -
        (kurdish_prop^2 +
          shia_prop^2 +
          sunni_prop^2 +
          christian_prop^2 +
          turcoman_prop^2 +
          other_prop^2)
    ) |>
    dplyr::ungroup()
}

build_landscan_weighted_ethnicity <- function(ethnicity_raw_sf, landscan_path) {
  assert_file_exists(landscan_path, "LandScan raster")

  landscan_raster <- raster::raster(landscan_path)

  landscan_crs <- sf::st_crs(raster::crs(landscan_raster))

  ethnicity_population_sf <- ethnicity_raw_sf |>
    sf::st_make_valid() |>
    sf::st_transform(crs = landscan_crs) |>
    dplyr::mutate(
      district_id = adm3code,
      district_name = adm3name,
      governorate = adm2name,
      kurdish_indicator = dplyr::if_else(ethnicity == 1, 1L, 0L),
      shia_indicator = dplyr::if_else(shia == 1, 1L, 0L),
      sunni_indicator = dplyr::if_else(sunni == 1, 1L, 0L),
      christian_indicator = dplyr::if_else(christians == 1, 1L, 0L),
      turcoman_indicator = dplyr::if_else(turcomans == 1, 1L, 0L),
      mixed_shia_sunni_indicator = dplyr::if_else(mixed_sh_su == 1, 1L, 0L)
    )

  ethnicity_population_sf$landscan_population <- exactextractr::exact_extract(
    x = landscan_raster,
    y = ethnicity_population_sf,
    fun = "sum",
    progress = TRUE
  )

  ethnicity_population_components <- ethnicity_population_sf |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      landscan_population = tidyr::replace_na(landscan_population, 0),
      kurdish_population = dplyr::if_else(
        kurdish_indicator == 1,
        landscan_population,
        0
      ),
      shia_population = dplyr::if_else(
        shia_indicator == 1,
        landscan_population,
        0
      ),
      sunni_population = dplyr::if_else(
        sunni_indicator == 1,
        landscan_population,
        0
      ),
      christian_population = dplyr::if_else(
        christian_indicator == 1,
        landscan_population,
        0
      ),
      turcoman_population = dplyr::if_else(
        turcoman_indicator == 1,
        landscan_population,
        0
      ),
      mixed_shia_sunni_population = dplyr::if_else(
        mixed_shia_sunni_indicator == 1,
        landscan_population,
        0
      ),
      shia_population_adjusted = shia_population +
        0.50 * mixed_shia_sunni_population,
      sunni_population_adjusted = sunni_population +
        0.50 * mixed_shia_sunni_population
    )

  ethnicity_population_components |>
    dplyr::group_by(district_id, district_name, governorate) |>
    dplyr::summarize(
      landscan_population_total = sum(landscan_population, na.rm = TRUE),
      kurdish_population = sum(kurdish_population, na.rm = TRUE),
      shia_population = sum(shia_population_adjusted, na.rm = TRUE),
      sunni_population = sum(sunni_population_adjusted, na.rm = TRUE),
      christian_population = sum(christian_population, na.rm = TRUE),
      turcoman_population = sum(turcoman_population, na.rm = TRUE),
      mixed_shia_sunni_population = sum(
        mixed_shia_sunni_population,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      kurdish_prop = dplyr::if_else(
        landscan_population_total > 0,
        kurdish_population / landscan_population_total,
        NA_real_
      ),
      shia_prop = dplyr::if_else(
        landscan_population_total > 0,
        shia_population / landscan_population_total,
        NA_real_
      ),
      sunni_prop = dplyr::if_else(
        landscan_population_total > 0,
        sunni_population / landscan_population_total,
        NA_real_
      ),
      christian_prop = dplyr::if_else(
        landscan_population_total > 0,
        christian_population / landscan_population_total,
        NA_real_
      ),
      turcoman_prop = dplyr::if_else(
        landscan_population_total > 0,
        turcoman_population / landscan_population_total,
        NA_real_
      ),
      mixed_shia_sunni_prop = dplyr::if_else(
        landscan_population_total > 0,
        mixed_shia_sunni_population / landscan_population_total,
        NA_real_
      ),
      other_prop = pmax(
        0,
        1 -
          kurdish_prop -
          shia_prop -
          sunni_prop -
          christian_prop -
          turcoman_prop
      )
    ) |>
    add_dominant_group_variables()
}

check_ethnicity_proportions <- function(ethnicity_data) {
  ethnicity_data |>
    dplyr::mutate(
      prop_sum = kurdish_prop +
        shia_prop +
        sunni_prop +
        christian_prop +
        turcoman_prop +
        other_prop
    ) |>
    dplyr::summarize(
      districts = dplyr::n(),
      minimum_prop_sum = min(prop_sum, na.rm = TRUE),
      maximum_prop_sum = max(prop_sum, na.rm = TRUE),
      mean_prop_sum = mean(prop_sum, na.rm = TRUE),
      .groups = "drop"
    )
}

compare_area_and_population_weighting <- function(
  area_weighted_ethnicity,
  population_weighted_ethnicity
) {
  area_weighted_ethnicity |>
    dplyr::select(
      district_id,
      district_name,
      governorate,
      area_sunni_prop = sunni_prop,
      area_shia_prop = shia_prop,
      area_kurdish_prop = kurdish_prop,
      area_dominant_group = dominant_group
    ) |>
    dplyr::left_join(
      population_weighted_ethnicity |>
        dplyr::select(
          district_id,
          population_sunni_prop = sunni_prop,
          population_shia_prop = shia_prop,
          population_kurdish_prop = kurdish_prop,
          population_dominant_group = dominant_group,
          landscan_population_total
        ),
      by = "district_id"
    ) |>
    dplyr::mutate(
      sunni_difference = population_sunni_prop - area_sunni_prop,
      shia_difference = population_shia_prop - area_shia_prop,
      kurdish_difference = population_kurdish_prop - area_kurdish_prop,
      dominant_group_changed = area_dominant_group != population_dominant_group
    )
}

spatial_join_attacks_to_districts <- function(gtd_iraq, districts_clean_sf) {
  attacks_points_sf <- gtd_iraq |>
    dplyr::filter(!is.na(latitude), !is.na(longitude)) |>
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    ) |>
    sf::st_transform(crs = sf::st_crs(districts_clean_sf))

  districts_join_sf <- districts_clean_sf |>
    dplyr::select(
      district_id,
      district_name,
      governorate,
      district_label,
      district_area_km2,
      district_area_km2_geometry,
      population_total,
      geometry
    )

  attacks_points_sf |>
    sf::st_join(districts_join_sf, join = sf::st_within, left = TRUE) |>
    sf::st_drop_geometry()
}

summarize_spatial_join_quality <- function(attacks_with_districts) {
  attacks_with_districts |>
    dplyr::summarize(
      attacks_with_coordinates = dplyr::n(),
      attacks_matched_to_district = sum(!is.na(district_id)),
      attacks_unmatched_to_district = sum(is.na(district_id)),
      match_rate = attacks_matched_to_district / attacks_with_coordinates,
      .groups = "drop"
    )
}

build_district_month_panel <- function(
  attacks_with_districts,
  districts_clean_table,
  ethnicity_shares
) {
  attacks_monthly <- attacks_with_districts |>
    dplyr::filter(!is.na(district_id), !is.na(event_month)) |>
    dplyr::count(
      district_id,
      district_name,
      governorate,
      district_label,
      event_month,
      name = "attack_count"
    )

  month_sequence <- seq(
    from = min(attacks_monthly$event_month, na.rm = TRUE),
    to = max(attacks_monthly$event_month, na.rm = TRUE),
    by = "month"
  )

  districts_clean_table |>
    tidyr::crossing(event_month = month_sequence) |>
    dplyr::left_join(
      attacks_monthly,
      by = c(
        "district_id",
        "district_name",
        "governorate",
        "district_label",
        "event_month"
      )
    ) |>
    dplyr::mutate(attack_count = tidyr::replace_na(attack_count, 0L)) |>
    dplyr::left_join(
      ethnicity_shares,
      by = c("district_id", "district_name", "governorate")
    ) |>
    dplyr::mutate(
      population_qc_flag = dplyr::case_when(
        is.na(population_total) ~ "missing_population",
        population_total < 10000 ~ "implausibly_low_population",
        TRUE ~ "population_available"
      ),
      attack_rate_100k = dplyr::if_else(
        population_qc_flag == "population_available" & population_total > 0,
        attack_count / population_total * 100000,
        NA_real_
      ),
      attack_rate_1000km2 = dplyr::if_else(
        !is.na(district_area_km2_geometry) & district_area_km2_geometry > 0,
        attack_count / district_area_km2_geometry * 1000,
        NA_real_
      )
    )
}

make_ethnic_palette <- function() {
  c(
    "Kurdish" = "#006A4E",
    "Shia" = "#842829",
    "Sunni" = "#005B8F",
    "Christian" = "#2E2D5B",
    "Turcoman" = "#8F567A",
    "Other" = "#525252"
  )
}

make_district_plot_levels <- function(district_month_panel, top_n = NULL) {
  order_table <- district_month_panel |>
    dplyr::group_by(district_id, district_label, dominant_group) |>
    dplyr::summarize(
      total_attacks = sum(attack_count, na.rm = TRUE),
      dominant_group_prop = dplyr::first(dominant_group_prop),
      .groups = "drop"
    )

  if (!is.null(top_n)) {
    order_table <- order_table |>
      dplyr::arrange(dplyr::desc(total_attacks)) |>
      dplyr::slice_head(n = top_n)
  }

  order_table |>
    dplyr::arrange(
      dominant_group,
      dplyr::desc(dominant_group_prop),
      dplyr::desc(total_attacks),
      district_label
    ) |>
    dplyr::pull(district_label) |>
    rev()
}

prepare_ethnicity_long_for_plot <- function(
  ethnicity_shares,
  district_plot_levels,
  ethnic_palette
) {
  ethnicity_shares |>
    dplyr::mutate(
      district_label = stringr::str_c(district_name, " [", governorate, "]"),
      district_label = factor(district_label, levels = district_plot_levels)
    ) |>
    dplyr::filter(!is.na(district_label)) |>
    dplyr::select(
      district_label,
      kurdish_prop,
      shia_prop,
      sunni_prop,
      christian_prop,
      turcoman_prop,
      other_prop
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::ends_with("_prop"),
      names_to = "group",
      values_to = "proportion"
    ) |>
    dplyr::mutate(
      group = dplyr::case_when(
        group == "kurdish_prop" ~ "Kurdish",
        group == "shia_prop" ~ "Shia",
        group == "sunni_prop" ~ "Sunni",
        group == "christian_prop" ~ "Christian",
        group == "turcoman_prop" ~ "Turcoman",
        group == "other_prop" ~ "Other",
        TRUE ~ group
      ),
      group = factor(group, levels = names(ethnic_palette))
    )
}

plot_population_composition_panel <- function(
  ethnicity_long,
  ethnic_palette,
  y_text_size = 4.5
) {
  ggplot2::ggplot(
    ethnicity_long,
    ggplot2::aes(x = proportion, y = district_label, fill = group)
  ) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = c(0, 0.5, 1),
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::scale_fill_manual(values = ethnic_palette, drop = FALSE) +
    ggplot2::labs(
      title = "District Population Composition",
      subtitle = "Population-Weighted Group Shares",
      x = "Population share",
      y = NULL,
      fill = "Group"
    ) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 10),
      plot.subtitle = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_text(size = y_text_size),
      axis.text.x = ggplot2::element_text(size = 7),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 7),
      legend.key.size = grid::unit(0.35, "cm")
    )
}

plot_attack_heatmap_panel <- function(
  heatmap_data,
  heatmap_metric = "attack_count",
  subtitle = "Monthly District-Level Attack Frequency"
) {
  ggplot2::ggplot(
    heatmap_data,
    ggplot2::aes(
      x = event_month,
      y = district_label,
      fill = .data[[heatmap_metric]]
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.03) +
    ggplot2::scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_fill_gradient(
      low = "grey96",
      high = "#842829",
      trans = "sqrt",
      name = "Monthly\nattacks"
    ) +
    ggplot2::labs(
      title = "IS Attack Frequency",
      subtitle = subtitle,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 10),
      plot.subtitle = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 8),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 7)
    )
}

build_population_weighted_heatmap <- function(
  district_month_panel,
  ethnicity_shares,
  top_n = NULL,
  figure_title,
  figure_subtitle,
  figure_height = 11,
  y_text_size = 4.5
) {
  ethnic_palette <- make_ethnic_palette()
  district_plot_levels <- make_district_plot_levels(
    district_month_panel,
    top_n = top_n
  )

  heatmap_data <- district_month_panel |>
    dplyr::filter(district_label %in% district_plot_levels) |>
    dplyr::mutate(
      district_label = factor(district_label, levels = district_plot_levels),
      event_month = as.Date(event_month)
    )

  ethnicity_long <- prepare_ethnicity_long_for_plot(
    ethnicity_shares = ethnicity_shares,
    district_plot_levels = district_plot_levels,
    ethnic_palette = ethnic_palette
  )

  composition_panel <- plot_population_composition_panel(
    ethnicity_long = ethnicity_long,
    ethnic_palette = ethnic_palette,
    y_text_size = y_text_size
  )

  attack_panel <- plot_attack_heatmap_panel(
    heatmap_data = heatmap_data,
    subtitle = if (is.null(top_n)) {
      "Monthly district-level attack frequency"
    } else {
      paste0("Top ", top_n, " districts by total IS attack frequency")
    }
  )

  composition_panel +
    attack_panel +
    patchwork::plot_layout(widths = c(1.3, 3.7), guides = "collect") +
    patchwork::plot_annotation(
      title = figure_title,
      subtitle = figure_subtitle,
    ) &
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0),
      legend.position = "bottom"
    )
}
