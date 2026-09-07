# ========================================================================
# FRACTIONALIZATION VS. ISIS ATTACK RATE
# FROM SCRATCH
#
# PURPOSE:
#   Build a district-month ISIS attack panel for Iraq, then visualize
#   whether attack rates are higher in districts with greater ethnic /
#   sectarian fractionalization.
#
# MAIN VISUALIZATION:
#   x     = ethnic_fractionalization
#   y     = mean monthly ISIS attack rate per 100,000 people
#   color = dominant ethnic / sectarian group
#   size  = LandScan district population
#   facet = conflict phase
#
# INPUT FILES:
#   data/raw/GTD_A.csv
#   data/raw/Iraq Ethnicity.zip
#   data/raw/LandScan_2008.tif
#
# OUTPUTS:
#   outputs/plots/fractionalization_vs_attack_rate_by_phase.png
#   outputs/plots/fractionalization_vs_attack_rate_labeled.png
#   outputs/plots/fractionalization_binned_attack_rate.png
#   outputs/tables/fractionalization_attack_rate_phase_data.csv
# ========================================================================

# ========================================================================
# 00) PACKAGES
# ========================================================================

# LOAD CORE DATA WRANGLING TOOLS
library(tidyverse)

# LOAD COLUMN-NAME CLEANING TOOLS
library(janitor)

# LOAD DATE-HANDLING TOOLS
library(lubridate)

# LOAD SPATIAL VECTOR TOOLS
library(sf)

# LOAD RASTER TOOLS
library(raster)

# LOAD EXACT RASTER EXTRACTION TOOLS
library(exactextractr)

# LOAD AXIS-LABEL FORMATTING TOOLS
library(scales)

# LOAD TEXT-LABEL REPELLING TOOLS
library(ggrepel)

# LOAD GAM SMOOTHER SUPPORT
library(mgcv)


# ========================================================================
# 01) FILE PATHS
# ========================================================================

# DEFINE GTD FILE PATH
gtd_path <- "data/raw/csv/GTD_A.csv"

# DEFINE IRAQ ETHNICITY ZIP FILE PATH
ethnicity_zip_path <- "data/raw/Iraq Ethnicity.zip"

# DEFINE ETHNICITY UNZIP DIRECTORY
ethnicity_unzip_dir <- "data/raw/iraq_ethnicity_unzipped"

# DEFINE LANDSCAN RASTER FILE PATH
landscan_path <- "data/raw/LandScan_2008.tif"

# DEFINE CLEAN DATA OUTPUT DIRECTORY
clean_dir <- "data/clean"

# DEFINE TABLE OUTPUT DIRECTORY
table_dir <- "outputs/tables"

# DEFINE PLOT OUTPUT DIRECTORY
plot_dir <- "outputs/plots"

# CREATE CLEAN DATA DIRECTORY
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

# CREATE TABLE OUTPUT DIRECTORY
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# CREATE PLOT OUTPUT DIRECTORY
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)


# ========================================================================
# 02) UNZIP ETHNICITY DATA AND LOCATE SHAPEFILES
# ========================================================================

# UNZIP ETHNICITY ZIP FILE ONLY IF NEEDED
if (!dir.exists(ethnicity_unzip_dir)) {
  unzip(
    zipfile = ethnicity_zip_path,
    exdir = ethnicity_unzip_dir
  )
}

# FIND IRAQI DISTRICT BOUNDARY SHAPEFILE
district_path <- list.files(
  path = ethnicity_unzip_dir,
  pattern = "iraq_districts_06162010\\.shp$",
  recursive = TRUE,
  full.names = TRUE
) |>
  first()

# FIND ETHNICITY-DISTRICT INTERSECTION SHAPEFILE
ethnicity_intersect_path <- list.files(
  path = ethnicity_unzip_dir,
  pattern = "Iraq_ethnic_Complete_Intersect\\.shp$",
  recursive = TRUE,
  full.names = TRUE
) |>
  first()

# CHECK THAT GTD FILE EXISTS
stopifnot(file.exists(gtd_path))

# CHECK THAT LANDSCAN FILE EXISTS
stopifnot(file.exists(landscan_path))

# CHECK THAT DISTRICT SHAPEFILE EXISTS
stopifnot(file.exists(district_path))

# CHECK THAT ETHNICITY INTERSECTION SHAPEFILE EXISTS
stopifnot(file.exists(ethnicity_intersect_path))


# ========================================================================
# 03) IMPORT AND CLEAN GTD DATA
# ========================================================================

# READ GTD AS CHARACTER-FIRST DATA FOR SAFE TYPE CONVERSION
gtd_raw <- readr::read_csv(
  file = gtd_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  janitor::clean_names()

# FILTER TO IRAQ AND CONVERT REQUIRED VARIABLES
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
    known_casualties_event = known_fatalities_event + known_wounded_event
  )


# ========================================================================
# 04) IMPORT DISTRICT AND ETHNICITY SHAPEFILES
# ========================================================================

# READ IRAQI DISTRICT BOUNDARIES
districts_raw_sf <- sf::st_read(
  dsn = district_path,
  quiet = TRUE
) |>
  janitor::clean_names() |>
  sf::st_make_valid()

# READ ETHNICITY-DISTRICT INTERSECTION POLYGONS
ethnicity_raw_sf <- sf::st_read(
  dsn = ethnicity_intersect_path,
  quiet = TRUE
) |>
  janitor::clean_names() |>
  sf::st_make_valid()

# CLEAN DISTRICT BOUNDARY ATTRIBUTES
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

# CREATE NON-SPATIAL DISTRICT TABLE
districts_clean_table <- districts_clean_sf |>
  sf::st_drop_geometry()


# ========================================================================
# 05) BUILD LANDSCAN-WEIGHTED ETHNIC / SECTARIAN COMPOSITION
# ========================================================================

# READ LANDSCAN RASTER
landscan_raster <- raster::raster(landscan_path)

# EXTRACT LANDSCAN CRS AS AN SF CRS OBJECT
landscan_crs <- sf::st_crs(
  raster::projection(landscan_raster)
)

# TRANSFORM ETHNICITY POLYGONS TO LANDSCAN CRS AND CREATE GROUP INDICATORS
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

# EXTRACT LANDSCAN POPULATION INSIDE EACH ETHNICITY-DISTRICT POLYGON
ethnicity_population_sf$landscan_population <- exactextractr::exact_extract(
  x = landscan_raster,
  y = ethnicity_population_sf,
  fun = "sum",
  progress = TRUE
)

# CREATE GROUP-SPECIFIC POPULATION COMPONENTS
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

# AGGREGATE GROUP POPULATIONS TO DISTRICT LEVEL
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

# CHECK THAT POPULATION SHARES SUM TO APPROXIMATELY ONE
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

# PRINT ETHNICITY CHECK
print(ethnicity_prop_check)


# ========================================================================
# 06) SPATIALLY JOIN GTD EVENTS TO DISTRICTS
# ========================================================================

# KEEP ONLY GTD EVENTS WITH VALID COORDINATES
gtd_iraq_coordinates <- gtd_iraq |>
  filter(
    !is.na(latitude),
    !is.na(longitude)
  )

# CONVERT GTD EVENTS TO SPATIAL POINTS
gtd_iraq_points_sf <- gtd_iraq_coordinates |>
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  sf::st_transform(
    crs = sf::st_crs(districts_clean_sf)
  )

# KEEP ONLY DISTRICT JOIN VARIABLES
districts_join_sf <- districts_clean_sf |>
  dplyr::select(
    district_id,
    district_name,
    governorate,
    district_label,
    population_total_esoc,
    geometry
  )

# SPATIALLY JOIN EVENTS TO DISTRICTS
gtd_attacks_district_sf <- gtd_iraq_points_sf |>
  sf::st_join(
    districts_join_sf,
    join = sf::st_within,
    left = TRUE
  )

# DROP GEOMETRY AFTER JOIN
gtd_attacks_district <- gtd_attacks_district_sf |>
  sf::st_drop_geometry()

# CHECK SPATIAL JOIN SUCCESS
spatial_join_check <- gtd_attacks_district |>
  summarize(
    attacks_with_coordinates = n(),
    attacks_matched_to_district = sum(!is.na(district_id)),
    attacks_unmatched_to_district = sum(is.na(district_id)),
    match_rate = attacks_matched_to_district / attacks_with_coordinates
  )

# PRINT SPATIAL JOIN CHECK
print(spatial_join_check)


# ========================================================================
# 07) BUILD COMPLETE DISTRICT-MONTH PANEL
# ========================================================================

# AGGREGATE ATTACKS TO DISTRICT-MONTH LEVEL
monthly_attacks <- gtd_attacks_district |>
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
    .groups = "drop"
  )

# CREATE COMPLETE MONTH SEQUENCE
month_sequence <- seq(
  from = min(monthly_attacks$event_month, na.rm = TRUE),
  to = max(monthly_attacks$event_month, na.rm = TRUE),
  by = "month"
)

# BUILD COMPLETE DISTRICT-MONTH PANEL
district_month_panel <- districts_clean_table |>
  crossing(
    event_month = month_sequence
  ) |>
  left_join(
    monthly_attacks,
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
    known_casualties = replace_na(known_casualties, 0)
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

# CHECK DISTRICT-MONTH PANEL
district_month_panel_check <- district_month_panel |>
  summarize(
    rows = n(),
    districts = n_distinct(district_id),
    months = n_distinct(event_month),
    total_attacks = sum(attack_count, na.rm = TRUE),
    missing_ethnicity_rows = sum(is.na(ethnic_fractionalization)),
    missing_population_rows = sum(is.na(landscan_population_total))
  )

# PRINT PANEL CHECK
print(district_month_panel_check)

# EXPORT COMPLETE DISTRICT-MONTH PANEL
readr::write_csv(
  district_month_panel,
  file.path(clean_dir, "district_month_fractionalization_attack_panel.csv"),
  na = ""
)


# ========================================================================
# 08) DEFINE CONFLICT PHASES
# ========================================================================

# ADD CONFLICT PHASE LABELS TO DISTRICT-MONTH PANEL
district_month_panel_phase <- district_month_panel |>
  mutate(
    event_month = as.Date(event_month),
    conflict_phase = case_when(
      event_month < ymd("2014-06-01") ~ "Pre-Caliphate Expansion",
      event_month >= ymd("2014-06-01") &
        event_month < ymd("2017-12-09") ~ "Territorial Caliphate",
      event_month >= ymd("2017-12-09") ~ "Post-Territorial Insurgency",
      TRUE ~ NA_character_
    ),
    conflict_phase = factor(
      conflict_phase,
      levels = c(
        "Pre-Caliphate Expansion",
        "Territorial Caliphate",
        "Post-Territorial Insurgency"
      )
    )
  )


# ========================================================================
# 09) BUILD DISTRICT-PHASE ANALYSIS TABLE
# ========================================================================

# AGGREGATE TO DISTRICT-PHASE LEVEL
fractionalization_phase_data <- district_month_panel_phase |>
  filter(
    !is.na(conflict_phase),
    !is.na(ethnic_fractionalization),
    !is.na(landscan_population_total),
    landscan_population_total > 0
  ) |>
  group_by(
    district_id,
    district_label,
    district_name,
    governorate,
    conflict_phase,
    dominant_group
  ) |>
  summarize(
    total_attacks = sum(attack_count, na.rm = TRUE),
    months_observed = n_distinct(event_month),
    landscan_population_total = first(landscan_population_total),
    ethnic_fractionalization = first(ethnic_fractionalization),
    dominant_group_prop = first(dominant_group_prop),
    sunni_prop = first(sunni_prop),
    shia_prop = first(shia_prop),
    kurdish_prop = first(kurdish_prop),
    .groups = "drop"
  ) |>
  mutate(
    total_attack_rate_100k = total_attacks / landscan_population_total * 100000,
    mean_monthly_attack_rate_100k = total_attack_rate_100k / months_observed,
    any_attacks = total_attacks > 0
  )

# EXPORT DISTRICT-PHASE ANALYSIS TABLE
readr::write_csv(
  fractionalization_phase_data,
  file.path(table_dir, "fractionalization_attack_rate_phase_data.csv"),
  na = ""
)

# CHECK DISTRICT-PHASE TABLE
fractionalization_phase_check <- fractionalization_phase_data |>
  summarize(
    rows = n(),
    districts = n_distinct(district_id),
    phases = n_distinct(conflict_phase),
    total_attacks = sum(total_attacks, na.rm = TRUE),
    max_mean_monthly_attack_rate_100k = max(
      mean_monthly_attack_rate_100k,
      na.rm = TRUE
    )
  )

# PRINT DISTRICT-PHASE CHECK
print(fractionalization_phase_check)


# ========================================================================
# 10) COLOR PALETTE
# ========================================================================

# DEFINE DOMINANT-GROUP COLOR PALETTE
dominant_group_palette <- c(
  "Kurdish" = "#006A4E",
  "Shia" = "#842829",
  "Sunni" = "#005B8F",
  "Christian" = "#2E2D5B",
  "Turcoman" = "#8F567A",
  "Other" = "#525252"
)


# ========================================================================
# 11) MAIN PLOT:
#     FRACTIONALIZATION VS. ATTACK RATE BY CONFLICT PHASE
# ========================================================================

# BUILD MAIN FRACTIONALIZATION SCATTERPLOT
plot_fractionalization_attack_rate <- fractionalization_phase_data |>
  ggplot(
    aes(
      x = ethnic_fractionalization,
      y = mean_monthly_attack_rate_100k
    )
  ) +
  geom_point(
    aes(
      color = dominant_group,
      size = landscan_population_total
    ),
    alpha = 0.75
  ) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    se = TRUE,
    color = "black",
    linewidth = 0.8
  ) +
  facet_wrap(
    vars(conflict_phase),
    scales = "free_y"
  ) +
  scale_x_continuous(
    labels = number_format(accuracy = 0.01),
    limits = c(0, NA)
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    trans = "sqrt"
  ) +
  scale_color_manual(
    values = dominant_group_palette,
    drop = FALSE,
    name = "Dominant group"
  ) +
  scale_size_continuous(
    labels = comma_format(),
    name = "LandScan\npopulation"
  ) +
  labs(
    title = "Ethnic/Sectarian Fractionalization and ISIS Attack Rates",
    subtitle = "Each point represents one Iraqi district during one conflict phase",
    x = "Ethnic/sectarian fractionalization",
    y = "Mean monthly ISIS attacks per 100,000 people"
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
    ),
    legend.position = "bottom"
  )

# PRINT MAIN PLOT
print(plot_fractionalization_attack_rate)

# EXPORT MAIN PLOT
ggsave(
  filename = file.path(
    plot_dir,
    "fractionalization_vs_attack_rate_by_phase.png"
  ),
  plot = plot_fractionalization_attack_rate,
  width = 12,
  height = 8,
  dpi = 300
)


# ========================================================================
# 12) LABELED VERSION:
#     LABEL ONLY HIGH-ATTACK-RATE DISTRICT-PHASE OBSERVATIONS
# ========================================================================

# DEFINE LABELING THRESHOLD USING THE TOP 15 PERCENT OF ATTACK RATES
label_threshold <- quantile(
  fractionalization_phase_data$mean_monthly_attack_rate_100k,
  probs = 0.85,
  na.rm = TRUE
)

# CREATE LABELED DATA
fractionalization_label_data <- fractionalization_phase_data |>
  mutate(
    label_text = if_else(
      mean_monthly_attack_rate_100k >= label_threshold,
      district_label,
      NA_character_
    )
  )

# BUILD LABELED FRACTIONALIZATION SCATTERPLOT
plot_fractionalization_attack_rate_labeled <- fractionalization_label_data |>
  ggplot(
    aes(
      x = ethnic_fractionalization,
      y = mean_monthly_attack_rate_100k
    )
  ) +
  geom_point(
    aes(
      color = dominant_group,
      size = landscan_population_total
    ),
    alpha = 0.75
  ) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    se = TRUE,
    color = "black",
    linewidth = 0.8
  ) +
  ggrepel::geom_text_repel(
    aes(
      label = label_text
    ),
    size = 3,
    color = "grey15",
    max.overlaps = Inf,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.alpha = 0.45,
    na.rm = TRUE,
    show.legend = FALSE
  ) +
  facet_wrap(
    vars(conflict_phase),
    scales = "free_y"
  ) +
  scale_x_continuous(
    labels = number_format(accuracy = 0.01),
    limits = c(0, NA)
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    trans = "sqrt"
  ) +
  scale_color_manual(
    values = dominant_group_palette,
    drop = FALSE,
    name = "Dominant group"
  ) +
  scale_size_continuous(
    labels = comma_format(),
    name = "LandScan\npopulation"
  ) +
  labs(
    title = "Ethnic/Sectarian Fractionalization and ISIS Attack Rates",
    subtitle = "Labels identify district-phase observations in the top 15% of attack rates",
    x = "Ethnic/sectarian fractionalization",
    y = "Mean monthly ISIS attacks per 100,000 people"
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
    ),
    legend.position = "bottom"
  )

# PRINT LABELED PLOT
print(plot_fractionalization_attack_rate_labeled)

# EXPORT LABELED PLOT
ggsave(
  filename = file.path(
    plot_dir,
    "fractionalization_vs_attack_rate_labeled.png"
  ),
  plot = plot_fractionalization_attack_rate_labeled,
  width = 12,
  height = 8,
  dpi = 300
)


# ========================================================================
# 13) BINNED SUMMARY PLOT:
#     CLEANER VERSION FOR A PAPER
# ========================================================================

# CREATE FRACTIONALIZATION BINS WITHIN EACH CONFLICT PHASE
fractionalization_binned_data <- fractionalization_phase_data |>
  filter(
    !is.na(ethnic_fractionalization),
    !is.na(mean_monthly_attack_rate_100k)
  ) |>
  group_by(
    conflict_phase
  ) |>
  mutate(
    fractionalization_bin = ntile(
      ethnic_fractionalization,
      5
    )
  ) |>
  ungroup() |>
  group_by(
    conflict_phase,
    fractionalization_bin
  ) |>
  summarize(
    mean_fractionalization = mean(
      ethnic_fractionalization,
      na.rm = TRUE
    ),
    mean_attack_rate_100k = mean(
      mean_monthly_attack_rate_100k,
      na.rm = TRUE
    ),
    median_attack_rate_100k = median(
      mean_monthly_attack_rate_100k,
      na.rm = TRUE
    ),
    districts = n(),
    .groups = "drop"
  )

# EXPORT BINNED TABLE
readr::write_csv(
  fractionalization_binned_data,
  file.path(table_dir, "fractionalization_binned_attack_rate_data.csv"),
  na = ""
)

# BUILD BINNED SUMMARY PLOT
plot_fractionalization_binned <- fractionalization_binned_data |>
  ggplot(
    aes(
      x = mean_fractionalization,
      y = mean_attack_rate_100k,
      group = conflict_phase
    )
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    aes(
      size = districts
    ),
    alpha = 0.85
  ) +
  facet_wrap(
    vars(conflict_phase),
    scales = "free_y"
  ) +
  scale_x_continuous(
    labels = number_format(accuracy = 0.01)
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    trans = "sqrt"
  ) +
  scale_size_continuous(
    name = "Districts\nin bin"
  ) +
  labs(
    title = "Binned Relationship Between Fractionalization and ISIS Attack Rates",
    subtitle = "Districts are divided into five fractionalization bins within each conflict phase",
    x = "Mean ethnic/sectarian fractionalization in bin",
    y = "Mean monthly ISIS attacks per 100,000 people"
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
    ),
    legend.position = "bottom"
  )

# PRINT BINNED PLOT
print(plot_fractionalization_binned)

# EXPORT BINNED PLOT
ggsave(
  filename = file.path(
    plot_dir,
    "fractionalization_binned_attack_rate.png"
  ),
  plot = plot_fractionalization_binned,
  width = 11,
  height = 7,
  dpi = 300
)


# ========================================================================
# 14) SIMPLE DIAGNOSTIC CORRELATION TABLE
# ========================================================================

# CREATE PHASE-SPECIFIC CORRELATION SUMMARY
fractionalization_correlation_summary <- fractionalization_phase_data |>
  group_by(
    conflict_phase
  ) |>
  summarize(
    districts = n(),
    correlation_fractionalization_attack_rate = cor(
      ethnic_fractionalization,
      mean_monthly_attack_rate_100k,
      use = "complete.obs"
    ),
    correlation_fractionalization_total_attacks = cor(
      ethnic_fractionalization,
      total_attacks,
      use = "complete.obs"
    ),
    .groups = "drop"
  )

# EXPORT CORRELATION SUMMARY
readr::write_csv(
  fractionalization_correlation_summary,
  file.path(table_dir, "fractionalization_attack_rate_correlation_summary.csv"),
  na = ""
)

# PRINT CORRELATION SUMMARY
print(fractionalization_correlation_summary)
