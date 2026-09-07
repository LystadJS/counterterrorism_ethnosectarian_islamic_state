# ========================================================================
# 01) Heatmap - Ethnic Population Proportion vs Attack Frequency.R
# Purpose: Build a LandScan population-weighted district-month ISIS panel
#          and produce publication-ready heatmaps.
# ========================================================================

# 00) SETUP

source("scripts/00) Project Setup.R")
source("scripts/25) Load Helper Scripts.R")

# 01) INPUT PATHS

# Keep these paths as written because the original project paths are accurate.
gtd_path <- "data/raw/csv/GTD_A.csv"
ethnicity_zip_path <- "data/raw/Iraq Ethnicity.zip"
ethnicity_unzip_dir <- "data/raw/iraq_ethnicity_unzipped"
landscan_path <- "data/raw/LandScan_2008.tif"

# 02) VALIDATE FILE LOCATIONS

# Unzip the Iraq ethnicity bundle only when the unzipped folder is absent.
if (!dir.exists(ethnicity_unzip_dir)) {
  unzip(
    zipfile = ethnicity_zip_path,
    exdir = ethnicity_unzip_dir
  )
}

# Locate the Iraqi district boundary shapefile inside the unzipped bundle.
district_path <- find_required_file(
  root = ethnicity_unzip_dir,
  pattern = "iraq_districts_06162010\\.shp$",
  file_label = "Iraq district boundary shapefile"
)

# Locate the ethnicity-by-district intersection shapefile.
ethnicity_intersect_path <- find_required_file(
  root = ethnicity_unzip_dir,
  pattern = "Iraq_ethnic_Complete_Intersect\\.shp$",
  file_label = "Iraq ethnicity intersection shapefile"
)

# Fail early if any required files are missing.
assert_file_exists(gtd_path, "GTD CSV")
assert_file_exists(ethnicity_zip_path, "Iraq ethnicity ZIP")
assert_file_exists(district_path, "Iraq district boundary shapefile")
assert_file_exists(
  ethnicity_intersect_path,
  "Iraq ethnicity intersection shapefile"
)
assert_file_exists(landscan_path, "LandScan 2008 raster")

# Save a raw file index for reproducibility.
raw_file_index <- write_raw_file_index(raw_data_root = "data/raw")

# 03) Import and clean GTD Iraq events ------------------------------------

# Import GTD as all-character data, preserve substantive "Unknown" labels,
# convert only needed fields, and construct event-level date variables.
gtd_iraq <- import_clean_gtd_iraq(gtd_path = gtd_path)

# Summarize GTD date and coordinate quality before spatial joining.
gtd_iraq_qc <- summarize_gtd_iraq_quality(gtd_iraq)
print(gtd_iraq_qc)

# Export GTD diagnostic products.
gtd_schema <- schema_inventory(gtd_iraq)
gtd_missingness <- missingness_by_variable(gtd_iraq)
gtd_complete_case_summary <- complete_case_summary(gtd_iraq)
gtd_eventid_audit <- audit_key_column(gtd_iraq, eventid)

export_cleaned_data(gtd_iraq, base_name = "gtd_iraq_cleaned")
export_data_dictionary(gtd_schema, base_name = "gtd_iraq_schema")
export_table(gtd_missingness, file_name = "gtd_iraq_missingness.csv")
export_table(
  gtd_complete_case_summary,
  file_name = "gtd_iraq_complete_cases.csv"
)
export_table(gtd_eventid_audit, file_name = "gtd_iraq_eventid_audit.csv")

# 04) Import and clean spatial files --------------------------------------

# Import Iraqi district boundaries and ethnicity/district intersections.
districts_raw_sf <- read_shapefile_clean(
  file_path = district_path,
  target_crs = NULL,
  clean_names = TRUE
)

ethnicity_raw_sf <- read_shapefile_clean(
  file_path = ethnicity_intersect_path,
  target_crs = NULL,
  clean_names = TRUE
)

# Run shapefile diagnostics before transforming the data.
districts_sf_diagnostic <- sf_diagnostic_summary(districts_raw_sf)
ethnicity_sf_diagnostic <- sf_diagnostic_summary(ethnicity_raw_sf)
districts_attribute_missingness <- sf_attribute_missingness(districts_raw_sf)
ethnicity_attribute_missingness <- sf_attribute_missingness(ethnicity_raw_sf)

print(districts_sf_diagnostic)
print(ethnicity_sf_diagnostic)

export_table(districts_sf_diagnostic, file_name = "districts_sf_diagnostic.csv")
export_table(ethnicity_sf_diagnostic, file_name = "ethnicity_sf_diagnostic.csv")
export_table(
  districts_attribute_missingness,
  file_name = "districts_attribute_missingness.csv"
)
export_table(
  ethnicity_attribute_missingness,
  file_name = "ethnicity_attribute_missingness.csv"
)

# Create a clean district boundary object and a non-spatial district table.
districts_clean_sf <- clean_district_boundaries(districts_raw_sf)
districts_clean_table <- districts_clean_sf |>
  sf::st_drop_geometry()

# Audit district identifiers before using them as panel keys.
district_id_audit <- audit_key_column(districts_clean_table, district_id)
print(district_id_audit)

export_cleaned_data(
  districts_clean_table,
  base_name = "iraq_districts_cleaned_attributes"
)

export_data_dictionary(
  schema_inventory(districts_clean_table),
  base_name = "iraq_districts_schema"
)

export_table(
  district_id_audit,
  file_name = "iraq_district_id_audit.csv"
)

# 05) Build area-weighted ethnicity table as a diagnostic baseline ---------

# This is not the final composition table. It is kept as a comparison against
# the stronger LandScan population-weighted composition table below.
ethnicity_area_shares <- build_area_weighted_ethnicity(ethnicity_raw_sf)
ethnicity_area_prop_check <- check_ethnicity_proportions(ethnicity_area_shares)

print(ethnicity_area_prop_check)

export_cleaned_data(
  ethnicity_area_shares,
  base_name = "iraq_district_ethnic_composition_area_weighted"
)

export_table(
  ethnicity_area_prop_check,
  file_name = "ethnicity_area_weighted_prop_check.csv"
)

# 06) Build LandScan population-weighted ethnicity table -------------------

# This is the final composition table used in the heatmap and panel data.
ethnicity_population_shares <- build_landscan_weighted_ethnicity(
  ethnicity_raw_sf = ethnicity_raw_sf,
  landscan_path = landscan_path
)

population_prop_check <- check_ethnicity_proportions(
  ethnicity_population_shares
)
print(population_prop_check)

# Compare area-weighted and population-weighted shares for documentation.
ethnicity_weighting_comparison <- compare_area_and_population_weighting(
  area_weighted_ethnicity = ethnicity_area_shares,
  population_weighted_ethnicity = ethnicity_population_shares
)

# Set the active ethnicity table to the population-weighted version.
ethnicity_shares <- ethnicity_population_shares

export_cleaned_data(
  ethnicity_shares,
  base_name = "iraq_district_ethnic_composition_landscan_weighted"
)

export_data_dictionary(
  schema_inventory(ethnicity_shares),
  base_name = "iraq_district_ethnic_composition_landscan_weighted_schema"
)

export_table(
  population_prop_check,
  file_name = "ethnicity_population_weighted_prop_check.csv"
)

export_table(
  ethnicity_weighting_comparison,
  file_name = "area_vs_population_weighting_comparison.csv"
)

# 07) Spatially join GTD attacks to Iraqi districts ------------------------

# Assign each GTD event with coordinates to an Iraqi district polygon.
gtd_attacks_district <- spatial_join_attacks_to_districts(
  gtd_iraq = gtd_iraq,
  districts_clean_sf = districts_clean_sf
)

# Check whether event points were successfully assigned to districts.
spatial_join_qc <- summarize_spatial_join_quality(gtd_attacks_district)
print(spatial_join_qc)

export_cleaned_data(
  gtd_attacks_district,
  base_name = "gtd_iraq_attacks_with_districts"
)

export_data_dictionary(
  schema_inventory(gtd_attacks_district),
  base_name = "gtd_iraq_attacks_with_districts_schema"
)

export_table(
  spatial_join_qc,
  file_name = "gtd_district_spatial_join_qc.csv"
)

# 08) Build final district-month analysis panel ----------------------------

# Create one row per district-month, including zero-attack months.
district_month_panel <- build_district_month_panel(
  attacks_with_districts = gtd_attacks_district,
  districts_clean_table = districts_clean_table,
  ethnicity_shares = ethnicity_shares
)

# Confirm that district-month rows are unique.
district_month_key_audit <- audit_composite_key(
  data = district_month_panel,
  key_columns = c("district_id", "event_month")
)

# Summarize the final panel for a quick project-level sanity check.
district_month_summary <- district_month_panel |>
  dplyr::summarize(
    rows = dplyr::n(),
    districts = dplyr::n_distinct(district_id),
    months = dplyr::n_distinct(event_month),
    total_attacks = sum(attack_count, na.rm = TRUE),
    missing_ethnicity_rows = sum(is.na(dominant_group)),
    low_population_rows = sum(
      population_qc_flag == "implausibly_low_population"
    ),
    .groups = "drop"
  )

print(district_month_key_audit)
print(district_month_summary)

export_cleaned_data(
  district_month_panel,
  base_name = "district_month_isis_ethnicity_panel_landscan_weighted"
)

export_data_dictionary(
  schema_inventory(district_month_panel),
  base_name = "district_month_isis_ethnicity_panel_landscan_weighted_schema"
)

export_table(
  missingness_by_variable(district_month_panel),
  file_name = "district_month_panel_missingness.csv"
)

export_table(
  complete_case_summary(district_month_panel),
  file_name = "district_month_panel_complete_cases.csv"
)

export_table(
  district_month_key_audit,
  file_name = "district_month_key_audit.csv"
)

export_table(
  district_month_summary,
  file_name = "district_month_summary.csv"
)

# 09) Build setup diagnostic plots ----------------------------------------

# Plot total ISIS attack frequency by month.
plot_monthly_attacks <- district_month_panel |>
  dplyr::group_by(event_month) |>
  dplyr::summarize(
    monthly_attacks = sum(attack_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = event_month, y = monthly_attacks)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.2) +
  ggplot2::labs(
    title = "Monthly ISIS Attack Frequency in Iraq",
    subtitle = "Aggregated from the LandScan-weighted district-month panel",
    x = NULL,
    y = "Monthly attack count"
  ) +
  ggplot2::theme_classic()

# Plot the area-vs-population weighting comparison for Sunni share.
plot_sunni_area_vs_population <- ethnicity_weighting_comparison |>
  ggplot2::ggplot(
    ggplot2::aes(x = area_sunni_prop, y = population_sunni_prop)
  ) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::geom_point(alpha = 0.75, size = 2) +
  ggplot2::scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  ggplot2::labs(
    title = "Area-Weighted vs. LandScan Population-Weighted Sunni Share",
    subtitle = "Each point represents one Iraqi district",
    x = "Area-weighted Sunni share",
    y = "Population-weighted Sunni share"
  ) +
  ggplot2::theme_classic()

print(plot_monthly_attacks)
print(plot_sunni_area_vs_population)

export_plot(
  plot_monthly_attacks,
  file_name = "monthly_isis_attack_frequency.png",
  width = 10,
  height = 6
)

export_plot(
  plot_sunni_area_vs_population,
  file_name = "area_vs_landscan_population_weighted_sunni_share.png",
  width = 8,
  height = 7
)

# 10) Build final publication heatmaps ------------------------------------

# Full district heatmap for appendix or exploratory documentation.
final_population_weighted_heatmap <- build_population_weighted_heatmap(
  district_month_panel = district_month_panel,
  ethnicity_shares = ethnicity_shares,
  top_n = NULL,
  figure_title = "ISIS Attack Frequency Across Iraqi Districts by Ethnic/Sectarian Population Composition",
  figure_subtitle = "Districts are ordered by LandScan-weighted dominant group, population concentration, and total ISIS attack frequency",
  figure_height = 11,
  y_text_size = 4.5
)

# Top-40 heatmap for the main paper or presentation figure.
final_top40_population_weighted_heatmap <- build_population_weighted_heatmap(
  district_month_panel = district_month_panel,
  ethnicity_shares = ethnicity_shares,
  top_n = 40,
  figure_title = "ISIS Attack Frequency Across High-Activity Iraqi Districts by Ethnic/Sectarian Population Composition",
  figure_subtitle = "Top 40 districts by total attack frequency; districts ordered by LandScan-weighted dominant group and group concentration",
  figure_height = 9,
  y_text_size = 7
)

print(final_population_weighted_heatmap)
print(final_top40_population_weighted_heatmap)

export_plot(
  final_population_weighted_heatmap,
  file_name = "final_population_weighted_isis_ethnicity_district_month_heatmap.png",
  width = 15,
  height = 11
)

export_plot(
  final_top40_population_weighted_heatmap,
  file_name = "final_top40_population_weighted_isis_ethnicity_district_month_heatmap.png",
  width = 15,
  height = 9
)

# 11) End of pipeline ------------------------------------------------------

message(
  "Pipeline complete. Review data/clean, data/dictionary, outputs/tables, and outputs/plots."
)
