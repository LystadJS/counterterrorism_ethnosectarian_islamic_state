# ========================================================================
# ATTACK FREQUENCY VS. AVERAGE CASUALTY SEVERITY
# COLOR GRADIENT = LANDSCAN-WEIGHTED SUNNI POPULATION SHARE
#
# PURPOSE:
#   Build a district-month ISIS panel for Iraq, then create:
#
#   1. A labeled district-level scatterplot:
#        x     = total ISIS attacks
#        y     = known casualties per attack
#        color = LandScan-weighted Sunni population share
#        size  = LandScan district population
#
#   2. A national monthly line plot:
#        monthly attacks
#        monthly known casualties
#
# IMPORTANT:
#   - No heatmap is created.
#   - 0% Sunni is dark red.
#   - 100% Sunni is black.
#   - Average casualties per attack is unstable in districts with very
#     few attacks, so the scatterplot filters to districts with at least
#     5 total attacks by default.
# ========================================================================

# ========================================================================
# 00) PACKAGES
# ========================================================================

library(tidyverse)
library(janitor)
library(lubridate)
library(sf)
library(raster)
library(exactextractr)
library(scales)
library(ggrepel)


# ========================================================================
# 01) FILE PATHS
# ========================================================================

gtd_path <- "data/raw/csv/GTD_A.csv"

ethnicity_zip_path <- "data/raw/Iraq Ethnicity.zip"

ethnicity_unzip_dir <- "data/raw/iraq_ethnicity_unzipped"

landscan_path <- "data/raw/LandScan_2008.tif"

clean_dir <- "data/clean"

table_dir <- "outputs/tables"

plot_dir <- "outputs/plots"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)


# ========================================================================
# 02) LOCATE SHAPEFILES
# ========================================================================

if (!dir.exists(ethnicity_unzip_dir)) {
  unzip(
    zipfile = ethnicity_zip_path,
    exdir = ethnicity_unzip_dir
  )
}

district_path <- list.files(
  path = ethnicity_unzip_dir,
  pattern = "iraq_districts_06162010\\.shp$",
  recursive = TRUE,
  full.names = TRUE
) |>
  first()

ethnicity_intersect_path <- list.files(
  path = ethnicity_unzip_dir,
  pattern = "Iraq_ethnic_Complete_Intersect\\.shp$",
  recursive = TRUE,
  full.names = TRUE
) |>
  first()

stopifnot(file.exists(gtd_path))

stopifnot(file.exists(landscan_path))

stopifnot(file.exists(district_path))

stopifnot(file.exists(ethnicity_intersect_path))


# ========================================================================
# 03) IMPORT AND CLEAN GTD DATA
# ========================================================================

gtd_raw <- readr::read_csv(
  file = gtd_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  janitor::clean_names()

gtd_iraq <- gtd_raw |>
  filter(
    country_txt == "Iraq"
  ) |>
  mutate(
    eventid = as.character(eventid),
    iyear = as.integer(iyear),
    imonth = as.integer(imonth),
    iday = as.integer(iday),
    latitude = readr::parse_number(latitude),
    longitude = readr::parse_number(longitude),
    nkill = readr::parse_number(nkill),
    nwound = readr::parse_number(nwound)
  ) |>
  mutate(
    event_month_number = if_else(
      imonth >= 1 & imonth <= 12,
      imonth,
      NA_integer_
    ),
    event_date = lubridate::make_date(
      year = iyear,
      month = event_month_number,
      day = 1L
    ),
    event_month = lubridate::floor_date(
      event_date,
      unit = "month"
    )
  ) |>
  mutate(
    nkill_missing = is.na(nkill),
    nwound_missing = is.na(nwound),
    known_fatalities_event = replace_na(nkill, 0),
    known_wounded_event = replace_na(nwound, 0),
    known_casualties_event = known_fatalities_event + known_wounded_event,
    fatal_attack_event = known_fatalities_event > 0,
    casualty_attack_event = known_casualties_event > 0
  )


# ========================================================================
# 04) IMPORT DISTRICT AND ETHNICITY SHAPEFILES
# ========================================================================

districts_raw_sf <- sf::st_read(
  dsn = district_path,
  quiet = TRUE
) |>
  janitor::clean_names() |>
  sf::st_make_valid()

ethnicity_raw_sf <- sf::st_read(
  dsn = ethnicity_intersect_path,
  quiet = TRUE
) |>
  janitor::clean_names() |>
  sf::st_make_valid()

districts_clean_sf <- districts_raw_sf |>
  transmute(
    district_id = adm3code,
    district_name = adm3name,
    governorate = adm2name,
    governorate_id = adm2code,
    population_total_esoc = suppressWarnings(as.numeric(total_pop)),
    district_area_km2 = suppressWarnings(as.numeric(area_km2)),
    district_label = str_c(adm3name, " [", adm2name, "]"),
    geometry = geometry
  )

districts_clean_table <- districts_clean_sf |>
  sf::st_drop_geometry()


# ========================================================================
# 05) BUILD LANDSCAN-WEIGHTED ETHNIC / SECTARIAN SHARES
# ========================================================================

landscan_raster <- raster::raster(landscan_path)

landscan_crs <- sf::st_crs(
  raster::projection(landscan_raster)
)

ethnicity_population_sf <- ethnicity_raw_sf |>
  sf::st_transform(
    crs = landscan_crs
  ) |>
  mutate(
    district_id = adm3code,
    district_name = adm3name,
    governorate = adm2name,
    kurdish_indicator = as.integer(
      replace_na(
        suppressWarnings(as.numeric(ethnicity)) == 1,
        FALSE
      )
    ),
    shia_indicator = as.integer(
      replace_na(
        suppressWarnings(as.numeric(shia)) == 1,
        FALSE
      )
    ),
    sunni_indicator = as.integer(
      replace_na(
        suppressWarnings(as.numeric(sunni)) == 1,
        FALSE
      )
    ),
    christian_indicator = as.integer(
      replace_na(
        suppressWarnings(as.numeric(christians)) == 1,
        FALSE
      )
    ),
    turcoman_indicator = as.integer(
      replace_na(
        suppressWarnings(as.numeric(turcomans)) == 1,
        FALSE
      )
    ),
    mixed_shia_sunni_indicator = as.integer(
      replace_na(
        suppressWarnings(as.numeric(mixed_sh_su)) == 1,
        FALSE
      )
    )
  )

ethnicity_population_sf$landscan_population <- exactextractr::exact_extract(
  x = landscan_raster,
  y = ethnicity_population_sf,
  fun = "sum",
  progress = TRUE
)

ethnicity_population_components <- ethnicity_population_sf |>
  sf::st_drop_geometry() |>
  mutate(
    landscan_population = replace_na(landscan_population, 0),
    kurdish_population = if_else(
      kurdish_indicator == 1,
      landscan_population,
      0
    ),
    shia_population = if_else(
      shia_indicator == 1,
      landscan_population,
      0
    ),
    sunni_population = if_else(
      sunni_indicator == 1,
      landscan_population,
      0
    ),
    christian_population = if_else(
      christian_indicator == 1,
      landscan_population,
      0
    ),
    turcoman_population = if_else(
      turcoman_indicator == 1,
      landscan_population,
      0
    ),
    mixed_shia_sunni_population = if_else(
      mixed_shia_sunni_indicator == 1,
      landscan_population,
      0
    )
  ) |>
  mutate(
    shia_population_adjusted = shia_population +
      0.50 * mixed_shia_sunni_population,
    sunni_population_adjusted = sunni_population +
      0.50 * mixed_shia_sunni_population
  )

ethnicity_shares <- ethnicity_population_components |>
  group_by(
    district_id,
    district_name,
    governorate
  ) |>
  summarize(
    landscan_population_total = sum(landscan_population, na.rm = TRUE),
    kurdish_population = sum(kurdish_population, na.rm = TRUE),
    shia_population = sum(shia_population_adjusted, na.rm = TRUE),
    sunni_population = sum(sunni_population_adjusted, na.rm = TRUE),
    christian_population = sum(christian_population, na.rm = TRUE),
    turcoman_population = sum(turcoman_population, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    district_label = str_c(district_name, " [", governorate, "]"),
    kurdish_prop = if_else(
      landscan_population_total > 0,
      kurdish_population / landscan_population_total,
      NA_real_
    ),
    shia_prop = if_else(
      landscan_population_total > 0,
      shia_population / landscan_population_total,
      NA_real_
    ),
    sunni_prop = if_else(
      landscan_population_total > 0,
      sunni_population / landscan_population_total,
      NA_real_
    ),
    christian_prop = if_else(
      landscan_population_total > 0,
      christian_population / landscan_population_total,
      NA_real_
    ),
    turcoman_prop = if_else(
      landscan_population_total > 0,
      turcoman_population / landscan_population_total,
      NA_real_
    ),
    other_prop = pmax(
      0,
      1 -
        (kurdish_prop +
          shia_prop +
          sunni_prop +
          christian_prop +
          turcoman_prop)
    )
  ) |>
  rowwise() |>
  mutate(
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
  ungroup()

ethnicity_prop_check <- ethnicity_shares |>
  mutate(
    prop_sum = kurdish_prop +
      shia_prop +
      sunni_prop +
      christian_prop +
      turcoman_prop +
      other_prop
  ) |>
  summarize(
    districts = n(),
    minimum_prop_sum = min(prop_sum, na.rm = TRUE),
    maximum_prop_sum = max(prop_sum, na.rm = TRUE),
    mean_prop_sum = mean(prop_sum, na.rm = TRUE),
    zero_landscan_population_districts = sum(
      landscan_population_total == 0,
      na.rm = TRUE
    )
  )

print(ethnicity_prop_check)


# ========================================================================
# 06) SPATIAL JOIN GTD EVENTS TO DISTRICTS
# ========================================================================

gtd_iraq_coordinates <- gtd_iraq |>
  filter(
    !is.na(latitude),
    !is.na(longitude)
  )

gtd_iraq_points_sf <- gtd_iraq_coordinates |>
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  sf::st_transform(
    crs = sf::st_crs(districts_clean_sf)
  )

districts_join_sf <- districts_clean_sf |>
  dplyr::select(
    district_id,
    district_name,
    governorate,
    district_label,
    population_total_esoc,
    geometry
  )

gtd_attacks_district_sf <- gtd_iraq_points_sf |>
  sf::st_join(
    districts_join_sf,
    join = sf::st_within,
    left = TRUE
  )

gtd_attacks_district <- gtd_attacks_district_sf |>
  sf::st_drop_geometry()

spatial_join_check <- gtd_attacks_district |>
  summarize(
    attacks_with_coordinates = n(),
    attacks_matched_to_district = sum(!is.na(district_id)),
    attacks_unmatched_to_district = sum(is.na(district_id)),
    match_rate = attacks_matched_to_district / attacks_with_coordinates
  )

print(spatial_join_check)


# ========================================================================
# 07) BUILD DISTRICT-MONTH PANEL
# ========================================================================

monthly_attack_casualty <- gtd_attacks_district |>
  filter(
    !is.na(district_id),
    !is.na(event_month)
  ) |>
  group_by(
    district_id,
    district_name,
    governorate,
    district_label,
    event_month
  ) |>
  summarize(
    attack_count = n(),
    known_fatalities = sum(known_fatalities_event, na.rm = TRUE),
    known_wounded = sum(known_wounded_event, na.rm = TRUE),
    known_casualties = sum(known_casualties_event, na.rm = TRUE),
    fatal_attack_count = sum(fatal_attack_event, na.rm = TRUE),
    casualty_attack_count = sum(casualty_attack_event, na.rm = TRUE),
    events_missing_nkill = sum(nkill_missing, na.rm = TRUE),
    events_missing_nwound = sum(nwound_missing, na.rm = TRUE),
    events_missing_any_casualty_field = sum(
      nkill_missing | nwound_missing,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

month_sequence <- seq(
  from = min(monthly_attack_casualty$event_month, na.rm = TRUE),
  to = max(monthly_attack_casualty$event_month, na.rm = TRUE),
  by = "month"
)

district_month_panel <- districts_clean_table |>
  crossing(
    event_month = month_sequence
  ) |>
  left_join(
    monthly_attack_casualty,
    by = c(
      "district_id",
      "district_name",
      "governorate",
      "district_label",
      "event_month"
    )
  ) |>
  mutate(
    attack_count = replace_na(attack_count, 0L),
    known_fatalities = replace_na(known_fatalities, 0),
    known_wounded = replace_na(known_wounded, 0),
    known_casualties = replace_na(known_casualties, 0),
    fatal_attack_count = replace_na(fatal_attack_count, 0L),
    casualty_attack_count = replace_na(casualty_attack_count, 0L),
    events_missing_nkill = replace_na(events_missing_nkill, 0L),
    events_missing_nwound = replace_na(events_missing_nwound, 0L),
    events_missing_any_casualty_field = replace_na(
      events_missing_any_casualty_field,
      0L
    )
  ) |>
  left_join(
    ethnicity_shares,
    by = c(
      "district_id",
      "district_name",
      "governorate",
      "district_label"
    )
  ) |>
  mutate(
    attack_rate_100k = if_else(
      !is.na(landscan_population_total) & landscan_population_total > 0,
      attack_count / landscan_population_total * 100000,
      NA_real_
    ),
    known_casualty_rate_100k = if_else(
      !is.na(landscan_population_total) & landscan_population_total > 0,
      known_casualties / landscan_population_total * 100000,
      NA_real_
    )
  )

district_month_panel_check <- district_month_panel |>
  summarize(
    rows = n(),
    districts = n_distinct(district_id),
    months = n_distinct(event_month),
    total_attacks = sum(attack_count, na.rm = TRUE),
    total_known_casualties = sum(known_casualties, na.rm = TRUE),
    missing_dominant_group_rows = sum(is.na(dominant_group)),
    missing_population_rows = sum(is.na(landscan_population_total))
  )

print(district_month_panel_check)


# ========================================================================
# 08) EXPORT CLEANED DATA
# ========================================================================

readr::write_csv(
  district_month_panel,
  file.path(clean_dir, "district_month_attack_casualty_panel.csv"),
  na = ""
)

readr::write_csv(
  ethnicity_shares,
  file.path(clean_dir, "iraq_district_ethnicity_landscan_weighted.csv"),
  na = ""
)

readr::write_csv(
  gtd_attacks_district,
  file.path(clean_dir, "gtd_iraq_attacks_with_districts.csv"),
  na = ""
)


# ========================================================================
# 09) BUILD DISTRICT-LEVEL SUMMARY
# ========================================================================

district_attack_casualty_summary <- district_month_panel |>
  group_by(
    district_id,
    district_label,
    district_name,
    governorate,
    dominant_group
  ) |>
  summarize(
    total_attacks = sum(attack_count, na.rm = TRUE),
    total_known_fatalities = sum(known_fatalities, na.rm = TRUE),
    total_known_wounded = sum(known_wounded, na.rm = TRUE),
    total_known_casualties = sum(known_casualties, na.rm = TRUE),
    fatal_attack_count = sum(fatal_attack_count, na.rm = TRUE),
    casualty_attack_count = sum(casualty_attack_count, na.rm = TRUE),
    landscan_population_total = first(landscan_population_total),
    dominant_group_prop = first(dominant_group_prop),
    ethnic_fractionalization = first(ethnic_fractionalization),
    kurdish_prop = first(kurdish_prop),
    shia_prop = first(shia_prop),
    sunni_prop = first(sunni_prop),
    christian_prop = first(christian_prop),
    turcoman_prop = first(turcoman_prop),
    other_prop = first(other_prop),
    .groups = "drop"
  ) |>
  mutate(
    known_casualties_per_attack = if_else(
      total_attacks > 0,
      total_known_casualties / total_attacks,
      NA_real_
    ),
    known_fatalities_per_attack = if_else(
      total_attacks > 0,
      total_known_fatalities / total_attacks,
      NA_real_
    ),
    known_wounded_per_attack = if_else(
      total_attacks > 0,
      total_known_wounded / total_attacks,
      NA_real_
    ),
    attack_rate_total_100k = if_else(
      !is.na(landscan_population_total) & landscan_population_total > 0,
      total_attacks / landscan_population_total * 100000,
      NA_real_
    ),
    casualty_rate_total_100k = if_else(
      !is.na(landscan_population_total) & landscan_population_total > 0,
      total_known_casualties / landscan_population_total * 100000,
      NA_real_
    )
  ) |>
  arrange(
    desc(total_attacks)
  )

readr::write_csv(
  district_attack_casualty_summary,
  file.path(table_dir, "district_attack_casualty_summary.csv"),
  na = ""
)


# ========================================================================
# 10) SCATTERPLOT SETTINGS
# ========================================================================

scatter_min_attacks <- 5

scatter_data <- district_attack_casualty_summary |>
  filter(
    total_attacks >= scatter_min_attacks,
    !is.na(known_casualties_per_attack),
    !is.na(sunni_prop)
  )

scatter_data_check <- scatter_data |>
  summarize(
    plotted_districts = n(),
    minimum_attacks = min(total_attacks, na.rm = TRUE),
    maximum_attacks = max(total_attacks, na.rm = TRUE),
    minimum_sunni_share = min(sunni_prop, na.rm = TRUE),
    maximum_sunni_share = max(sunni_prop, na.rm = TRUE),
    maximum_casualties_per_attack = max(
      known_casualties_per_attack,
      na.rm = TRUE
    )
  )

print(scatter_data_check)


# ========================================================================
# 11) CREATE SCATTERPLOT:
#     TOTAL ATTACKS VS. AVERAGE CASUALTIES PER ATTACK
#     COLOR = SUNNI POPULATION SHARE
# ========================================================================

plot_attack_vs_average_casualty_scatter <- scatter_data |>
  ggplot(
    aes(
      x = total_attacks,
      y = known_casualties_per_attack
    )
  ) +
  geom_point(
    aes(
      color = sunni_prop,
      size = landscan_population_total
    ),
    alpha = 0.88
  ) +
  ggrepel::geom_text_repel(
    aes(
      label = district_label
    ),
    color = "grey15",
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.alpha = 0.45,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    labels = comma_format()
  ) +
  scale_y_continuous(
    labels = comma_format(accuracy = 0.1)
  ) +
  scale_size_continuous(
    labels = comma_format(),
    name = "LandScan\npopulation"
  ) +
  scale_color_gradient(
    low = "#842829",
    high = "#000000",
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    name = "Sunni population\nshare"
  ) +
  labs(
    title = "District-Level ISIS Attack Frequency vs. Average Casualty Severity",
    subtitle = str_c(
      "Each point is one Iraqi district with at least ",
      scatter_min_attacks,
      " attacks. Color shows LandScan-weighted Sunni population share."
    ),
    x = "Total ISIS attack count",
    y = "Known casualties per attack"
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    legend.position = "bottom"
  )

print(plot_attack_vs_average_casualty_scatter)

ggsave(
  filename = file.path(
    plot_dir,
    "district_attack_frequency_vs_average_casualties_per_attack_sunni_gradient.png"
  ),
  plot = plot_attack_vs_average_casualty_scatter,
  width = 12,
  height = 9,
  dpi = 300
)


# ========================================================================
# 12) OPTIONAL LOG-X SCATTERPLOT
# ========================================================================
# This is often easier to read because a few districts may dominate the
# total attack-count scale.

plot_attack_vs_average_casualty_scatter_log <- scatter_data |>
  ggplot(
    aes(
      x = total_attacks,
      y = known_casualties_per_attack
    )
  ) +
  geom_point(
    aes(
      color = sunni_prop,
      size = landscan_population_total
    ),
    alpha = 0.88
  ) +
  ggrepel::geom_text_repel(
    aes(
      label = district_label
    ),
    color = "grey15",
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.alpha = 0.45,
    show.legend = FALSE
  ) +
  scale_x_log10(
    labels = comma_format()
  ) +
  scale_y_continuous(
    labels = comma_format(accuracy = 0.1)
  ) +
  scale_size_continuous(
    labels = comma_format(),
    name = "LandScan\npopulation"
  ) +
  scale_color_gradient(
    low = "#842829",
    high = "#000000",
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    name = "Sunni population\nshare"
  ) +
  labs(
    title = "District-Level ISIS Attack Frequency vs. Average Casualty Severity",
    subtitle = str_c(
      "Log-scaled x-axis; districts with at least ",
      scatter_min_attacks,
      " attacks. Color shows LandScan-weighted Sunni population share."
    ),
    x = "Total ISIS attack count, log scale",
    y = "Known casualties per attack"
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    legend.position = "bottom"
  )

print(plot_attack_vs_average_casualty_scatter_log)

ggsave(
  filename = file.path(
    plot_dir,
    "district_attack_frequency_vs_average_casualties_per_attack_sunni_gradient_logx.png"
  ),
  plot = plot_attack_vs_average_casualty_scatter_log,
  width = 12,
  height = 9,
  dpi = 300
)


# ========================================================================
# 13) BUILD MONTHLY NATIONAL SUMMARY
# ========================================================================

monthly_attack_casualty_summary <- district_month_panel |>
  group_by(
    event_month
  ) |>
  summarize(
    monthly_attacks = sum(attack_count, na.rm = TRUE),
    monthly_known_fatalities = sum(known_fatalities, na.rm = TRUE),
    monthly_known_wounded = sum(known_wounded, na.rm = TRUE),
    monthly_known_casualties = sum(known_casualties, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  monthly_attack_casualty_summary,
  file.path(table_dir, "monthly_attack_casualty_summary.csv"),
  na = ""
)


# ========================================================================
# 14) CREATE MONTHLY LINE PLOT
# ========================================================================

monthly_attack_casualty_long <- monthly_attack_casualty_summary |>
  dplyr::select(
    event_month,
    monthly_attacks,
    monthly_known_casualties
  ) |>
  pivot_longer(
    cols = c(
      monthly_attacks,
      monthly_known_casualties
    ),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = case_when(
      series == "monthly_attacks" ~ "Monthly attacks",
      series == "monthly_known_casualties" ~ "Monthly known casualties",
      TRUE ~ series
    ),
    series = factor(
      series,
      levels = c(
        "Monthly attacks",
        "Monthly known casualties"
      )
    )
  )

plot_monthly_attack_casualty_lines <- monthly_attack_casualty_long |>
  ggplot(
    aes(
      x = event_month,
      y = value
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  facet_wrap(
    vars(series),
    scales = "free_y",
    ncol = 1
  ) +
  scale_y_continuous(
    labels = comma_format()
  ) +
  labs(
    title = "Monthly ISIS Attack Frequency and Known Casualties in Iraq",
    subtitle = "National monthly totals aggregated from the district-month panel",
    x = NULL,
    y = NULL
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    strip.text = element_text(
      face = "bold"
    )
  )

print(plot_monthly_attack_casualty_lines)

ggsave(
  filename = file.path(
    plot_dir,
    "monthly_attack_frequency_vs_known_casualties_lines.png"
  ),
  plot = plot_monthly_attack_casualty_lines,
  width = 11,
  height = 8,
  dpi = 300
)


# ========================================================================
# 15) EXPORT SCATTERPLOT DIAGNOSTIC TABLE
# ========================================================================

scatter_diagnostic_table <- scatter_data |>
  dplyr::select(
    district_label,
    governorate,
    dominant_group,
    sunni_prop,
    dominant_group_prop,
    total_attacks,
    total_known_casualties,
    known_casualties_per_attack,
    landscan_population_total,
    ethnic_fractionalization
  ) |>
  arrange(
    desc(known_casualties_per_attack)
  )

readr::write_csv(
  scatter_diagnostic_table,
  file.path(
    table_dir,
    "scatter_average_casualties_per_attack_sunni_gradient_diagnostics.csv"
  ),
  na = ""
)

print(scatter_diagnostic_table, n = 30)
