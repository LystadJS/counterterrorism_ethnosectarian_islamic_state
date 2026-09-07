# ========================================================================
# SPATIAL RESIDUAL MAP FROM ISIS DISTRICT-LEVEL ATTACK MODEL
# ESOC + LANDSCAN VERSION
# FROM SCRATCH
#
# PURPOSE:
#   Build a district-year ISIS attack model from only:
#     1. GTD_A.csv
#     2. Iraq Ethnicity.zip
#     3. LandScan_2008.tif
#
#   Then map where observed ISIS attacks are higher or lower than expected.
#
# CORE CONTRAST:
#   1. Observed ISIS attack counts by district
#   2. Predicted ISIS attack counts by district
#   3. Residuals:
#      observed attacks - predicted attacks
#
# MODEL LOGIC:
#   Negative binomial count model with:
#     - ISIS attack count as outcome
#     - log population as offset
#     - LandScan-weighted demographic proportions
#     - LandScan-derived urban proxy
#     - district area
#     - year fixed effects
#
# IMPORTANT LIMITATION:
#   This script cannot truly include road_density or built_up_area because
#   those data are not present in the stated workspace.
#
#   Instead:
#     - urban_proxy is built from LandScan population density
#     - road_density is created as unavailable
#     - built_up_area is created as unavailable
#     - the model automatically omits unavailable infrastructure variables
#
# UNIT OF RAW DATA:
#   One GTD attack event
#
# UNIT OF MODEL DATA:
#   District-year
#
# UNIT OF MAP DATA:
#   Iraq district
#
# KEY OUTPUTS:
#   outputs/plots/isis_spatial_residual_map_nb_model.png
#   outputs/plots/isis_observed_vs_predicted_district_attacks.png
#   outputs/plots/isis_high_low_residual_districts.png
#   outputs/tables/isis_district_year_model_data.csv
#   outputs/tables/isis_district_residuals.csv
#   outputs/tables/isis_high_residual_districts.csv
#   outputs/tables/isis_low_residual_districts.csv
#   outputs/tables/isis_negative_binomial_model_summary.csv
#   outputs/models/isis_district_nb_model.rds
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
library(MASS)
library(broom)
library(scales)
library(forcats)
library(viridis)


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

model_dir <- "outputs/models"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(gtd_path))

stopifnot(file.exists(ethnicity_zip_path))

stopifnot(file.exists(landscan_path))


# ========================================================================
# 02) USER-EDITABLE MODEL SETTINGS
# ========================================================================

analysis_start_year <- 2004

analysis_end_year <- 2022

minimum_population_for_model <- 1

top_residual_districts_to_save <- 25

use_year_fixed_effects <- TRUE

dominant_group_cutoff <- 0.50

minority_heavy_cutoff <- 0.25

mixed_shia_sunni_split <- 0.50

use_road_density_if_available <- TRUE

use_built_up_area_if_available <- TRUE


# ========================================================================
# 03) UNZIP ESOC ETHNICITY DATA AND LOCATE SHAPEFILES
# ========================================================================

if (!dir.exists(ethnicity_unzip_dir)) {
  dir.create(
    ethnicity_unzip_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

if (length(list.files(ethnicity_unzip_dir, recursive = TRUE)) == 0) {
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

stopifnot(file.exists(district_path))

stopifnot(file.exists(ethnicity_intersect_path))


# ========================================================================
# 04) IMPORT AND CLEAN GTD DATA
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
    nwound = readr::parse_number(nwound),
    group_name_raw = str_squish(as.character(gname)),
    attack_type_raw = str_squish(as.character(attacktype1_txt)),
    target_type_raw = str_squish(as.character(targtype1_txt))
  ) |>
  mutate(
    event_month_number = if_else(
      imonth >= 1 & imonth <= 12,
      imonth,
      NA_integer_
    ),
    event_day_number = if_else(
      iday >= 1 & iday <= 31,
      iday,
      1L
    ),
    event_date = lubridate::make_date(
      year = iyear,
      month = event_month_number,
      day = event_day_number
    ),
    event_month = lubridate::floor_date(
      event_date,
      unit = "month"
    ),
    event_year = lubridate::year(event_month),
    known_fatalities_event = replace_na(nkill, 0),
    known_wounded_event = replace_na(nwound, 0),
    known_casualties_event = known_fatalities_event + known_wounded_event
  ) |>
  filter(
    !is.na(event_year)
  ) |>
  filter(
    event_year >= analysis_start_year,
    event_year <= analysis_end_year
  )


# ========================================================================
# 05) FILTER TO ISIS / ISLAMIC STATE ATTACKS
# ========================================================================

isis_group_pattern <- paste(
  "islamic state",
  "isis",
  "isil",
  "daesh",
  "al-dawla al-islamiya",
  "islamic state of iraq",
  "islamic state of iraq and syria",
  "islamic state of iraq and the levant",
  sep = "|"
)

gtd_isis_iraq <- gtd_iraq |>
  mutate(
    isis_related_attack = str_detect(
      string = str_to_lower(group_name_raw),
      pattern = isis_group_pattern
    )
  ) |>
  filter(
    isis_related_attack
  )

isis_filter_check <- gtd_isis_iraq |>
  count(
    group_name_raw,
    sort = TRUE
  )

print(isis_filter_check, n = 50)

readr::write_csv(
  isis_filter_check,
  file.path(table_dir, "isis_group_name_filter_check.csv"),
  na = ""
)


# ========================================================================
# 06) IMPORT ESOC DISTRICT AND ETHNICITY SHAPEFILES
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
    district_id = as.character(adm3code),
    district_name = as.character(adm3name),
    governorate = as.character(adm2name),
    governorate_id = as.character(adm2code),
    population_total_esoc = suppressWarnings(as.numeric(total_pop)),
    district_area_km2_esoc = suppressWarnings(as.numeric(area_km2)),
    district_label = str_c(adm3name, " [", adm2name, "]"),
    geometry = geometry
  ) |>
  sf::st_make_valid()

districts_projected_sf <- districts_clean_sf |>
  sf::st_transform(
    crs = 3857
  ) |>
  mutate(
    district_area_km2_geometry = as.numeric(
      sf::st_area(geometry)
    ) /
      1000000
  ) |>
  sf::st_transform(
    crs = sf::st_crs(districts_clean_sf)
  )

districts_clean_sf <- districts_clean_sf |>
  left_join(
    districts_projected_sf |>
      sf::st_drop_geometry() |>
      dplyr::select(
        district_id,
        district_area_km2_geometry
      ),
    by = "district_id"
  ) |>
  mutate(
    district_area_km2 = case_when(
      !is.na(district_area_km2_esoc) &
        district_area_km2_esoc > 0 ~ district_area_km2_esoc,
      !is.na(district_area_km2_geometry) &
        district_area_km2_geometry > 0 ~ district_area_km2_geometry,
      TRUE ~ NA_real_
    )
  )

districts_clean_table <- districts_clean_sf |>
  sf::st_drop_geometry()

readr::write_csv(
  districts_clean_table,
  file.path(clean_dir, "iraq_district_boundaries_clean_table.csv"),
  na = ""
)


# ========================================================================
# 07) IMPORT LANDSCAN RASTER
# ========================================================================

landscan_raster <- raster::raster(
  landscan_path
)

landscan_crs <- sf::st_crs(
  raster::projection(landscan_raster)
)

if (is.na(landscan_crs)) {
  stop(
    "LandScan raster CRS is missing. Define the raster CRS before continuing."
  )
}


# ========================================================================
# 08) BUILD LANDSCAN-WEIGHTED DISTRICT DEMOGRAPHICS
# ========================================================================

ethnicity_population_sf <- ethnicity_raw_sf |>
  sf::st_transform(
    crs = landscan_crs
  ) |>
  mutate(
    district_id = as.character(adm3code),
    district_name = as.character(adm3name),
    governorate = as.character(adm2name),
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
    ),
    shia_population_adjusted = shia_population +
      mixed_shia_sunni_split * mixed_shia_sunni_population,
    sunni_population_adjusted = sunni_population +
      mixed_shia_sunni_split * mixed_shia_sunni_population
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
    minority_prop = replace_na(christian_prop, 0) +
      replace_na(turcoman_prop, 0),
    other_prop = pmax(
      0,
      1 -
        (replace_na(kurdish_prop, 0) +
          replace_na(shia_prop, 0) +
          replace_na(sunni_prop, 0) +
          replace_na(christian_prop, 0) +
          replace_na(turcoman_prop, 0))
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
      replace_na(kurdish_prop, 0),
      replace_na(shia_prop, 0),
      replace_na(sunni_prop, 0),
      replace_na(christian_prop, 0),
      replace_na(turcoman_prop, 0),
      replace_na(other_prop, 0)
    ))],
    dominant_group_prop = max(
      c(
        replace_na(kurdish_prop, 0),
        replace_na(shia_prop, 0),
        replace_na(sunni_prop, 0),
        replace_na(christian_prop, 0),
        replace_na(turcoman_prop, 0),
        replace_na(other_prop, 0)
      ),
      na.rm = TRUE
    ),
    ethnic_fractionalization = 1 -
      (replace_na(kurdish_prop, 0)^2 +
        replace_na(shia_prop, 0)^2 +
        replace_na(sunni_prop, 0)^2 +
        replace_na(christian_prop, 0)^2 +
        replace_na(turcoman_prop, 0)^2 +
        replace_na(other_prop, 0)^2),
    demographic_context = case_when(
      replace_na(minority_prop, 0) >= minority_heavy_cutoff ~ "Minority-heavy",
      replace_na(sunni_prop, 0) >= dominant_group_cutoff &
        dominant_group == "Sunni" ~ "Sunni-dominant",
      replace_na(shia_prop, 0) >= dominant_group_cutoff &
        dominant_group == "Shia" ~ "Shia-dominant",
      replace_na(kurdish_prop, 0) >= dominant_group_cutoff &
        dominant_group == "Kurdish" ~ "Kurdish-dominant",
      TRUE ~ "Mixed"
    )
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

readr::write_csv(
  ethnicity_prop_check,
  file.path(table_dir, "ethnicity_landscan_prop_check.csv"),
  na = ""
)

readr::write_csv(
  ethnicity_shares,
  file.path(clean_dir, "iraq_district_ethnicity_landscan_weighted.csv"),
  na = ""
)


# ========================================================================
# 09) BUILD DISTRICT MODEL COVARIATES FROM AVAILABLE DATA ONLY
# ========================================================================

district_covariates <- districts_clean_sf |>
  sf::st_drop_geometry() |>
  dplyr::select(
    district_id,
    district_name,
    governorate,
    governorate_id,
    district_label,
    district_area_km2,
    population_total_esoc
  ) |>
  left_join(
    ethnicity_shares |>
      dplyr::select(
        district_id,
        landscan_population_total,
        sunni_prop,
        shia_prop,
        kurdish_prop,
        christian_prop,
        turcoman_prop,
        minority_prop,
        other_prop,
        dominant_group,
        dominant_group_prop,
        ethnic_fractionalization,
        demographic_context
      ),
    by = "district_id"
  ) |>
  mutate(
    total_population = landscan_population_total,
    urban_proxy = if_else(
      !is.na(total_population) &
        total_population > 0 &
        !is.na(district_area_km2) &
        district_area_km2 > 0,
      total_population / district_area_km2,
      NA_real_
    ),
    road_density = NA_real_,
    built_up_area = NA_real_,
    covariate_data_status = case_when(
      is.na(total_population) ~ "Missing LandScan population",
      total_population <= 0 ~ "Zero or invalid LandScan population",
      is.na(urban_proxy) ~ "Missing urban proxy",
      TRUE ~ "Usable for base model"
    )
  )

district_covariate_check <- district_covariates |>
  summarize(
    districts = n(),
    districts_with_valid_landscan_population = sum(
      total_population > 0,
      na.rm = TRUE
    ),
    missing_sunni_prop = sum(is.na(sunni_prop)),
    missing_shia_prop = sum(is.na(shia_prop)),
    missing_kurdish_prop = sum(is.na(kurdish_prop)),
    missing_minority_prop = sum(is.na(minority_prop)),
    missing_urban_proxy = sum(is.na(urban_proxy)),
    road_density_available = sum(!is.na(road_density)),
    built_up_area_available = sum(!is.na(built_up_area))
  )

print(district_covariate_check)

readr::write_csv(
  district_covariates,
  file.path(
    clean_dir,
    "district_model_covariates_generated_from_esoc_landscan.csv"
  ),
  na = ""
)

readr::write_csv(
  district_covariate_check,
  file.path(table_dir, "district_covariate_check.csv"),
  na = ""
)


# ========================================================================
# 10) SPATIALLY JOIN ISIS EVENTS TO DISTRICTS
# ========================================================================

gtd_isis_coordinates <- gtd_isis_iraq |>
  filter(
    !is.na(latitude),
    !is.na(longitude)
  ) |>
  filter(
    between(latitude, -90, 90),
    between(longitude, -180, 180)
  )

gtd_isis_points_sf <- gtd_isis_coordinates |>
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
    governorate_id,
    district_label,
    geometry
  )

gtd_isis_district_sf <- gtd_isis_points_sf |>
  sf::st_join(
    districts_join_sf,
    join = sf::st_within,
    left = TRUE
  )

gtd_isis_district <- gtd_isis_district_sf |>
  sf::st_drop_geometry()

spatial_join_check <- gtd_isis_district |>
  summarize(
    isis_attacks_with_coordinates = n(),
    isis_attacks_matched_to_district = sum(!is.na(district_id)),
    isis_attacks_unmatched_to_district = sum(is.na(district_id)),
    match_rate = isis_attacks_matched_to_district /
      isis_attacks_with_coordinates
  )

print(spatial_join_check)

readr::write_csv(
  spatial_join_check,
  file.path(table_dir, "isis_spatial_join_check.csv"),
  na = ""
)

readr::write_csv(
  gtd_isis_district,
  file.path(clean_dir, "gtd_isis_iraq_attacks_with_districts.csv"),
  na = ""
)


# ========================================================================
# 11) COUNT ISIS ATTACKS BY DISTRICT-PHASE
# ========================================================================

conflict_phase_table <- tibble::tribble(
  ~conflict_phase               , ~phase_start_year , ~phase_end_year ,
  "Pre-caliphate"               ,              2004 ,            2013 ,
  "Territorial caliphate"       ,              2014 ,            2017 ,
  "Post-territorial insurgency" ,              2018 ,            2022
) |>
  mutate(
    conflict_phase = factor(
      conflict_phase,
      levels = c(
        "Pre-caliphate",
        "Territorial caliphate",
        "Post-territorial insurgency"
      )
    ),
    phase_years = phase_end_year - phase_start_year + 1
  )

gtd_isis_district_phase <- gtd_isis_district |>
  mutate(
    conflict_phase = case_when(
      event_year >= 2004 & event_year <= 2013 ~ "Pre-caliphate",
      event_year >= 2014 & event_year <= 2017 ~ "Territorial caliphate",
      event_year >= 2018 & event_year <= 2022 ~ "Post-territorial insurgency",
      TRUE ~ NA_character_
    ),
    conflict_phase = factor(
      conflict_phase,
      levels = levels(conflict_phase_table$conflict_phase)
    )
  )

district_phase_sequence <- expand_grid(
  district_id = districts_clean_table$district_id,
  conflict_phase = conflict_phase_table$conflict_phase
) |>
  left_join(
    conflict_phase_table,
    by = "conflict_phase"
  )

district_phase_attack_counts <- gtd_isis_district_phase |>
  filter(
    !is.na(district_id),
    !is.na(conflict_phase)
  ) |>
  count(
    district_id,
    conflict_phase,
    name = "isis_attack_count"
  ) |>
  right_join(
    district_phase_sequence,
    by = c(
      "district_id",
      "conflict_phase"
    )
  ) |>
  mutate(
    isis_attack_count = replace_na(
      isis_attack_count,
      0L
    )
  )

district_phase_attack_count_check <- district_phase_attack_counts |>
  summarize(
    district_phase_rows = n(),
    total_isis_attacks_in_model_frame = sum(
      isis_attack_count,
      na.rm = TRUE
    ),
    district_phases_with_attacks = sum(
      isis_attack_count > 0,
      na.rm = TRUE
    ),
    zero_attack_rows = sum(
      isis_attack_count == 0,
      na.rm = TRUE
    )
  )

print(district_phase_attack_count_check)

readr::write_csv(
  district_phase_attack_count_check,
  file.path(table_dir, "district_phase_attack_count_check.csv"),
  na = ""
)


# ========================================================================
# 12) BUILD DISTRICT-PHASE MODEL DATA
# ========================================================================

district_phase_model_data <- district_phase_attack_counts |>
  left_join(
    district_covariates |>
      dplyr::select(
        district_id,
        district_name,
        governorate,
        governorate_id,
        district_label,
        total_population,
        sunni_prop,
        shia_prop,
        kurdish_prop,
        minority_prop,
        other_prop,
        dominant_group,
        dominant_group_prop,
        ethnic_fractionalization,
        demographic_context,
        urban_proxy,
        road_density,
        built_up_area,
        district_area_km2
      ),
    by = "district_id"
  ) |>
  filter(
    !is.na(total_population),
    total_population >= minimum_population_for_model
  ) |>
  filter(
    !is.na(sunni_prop),
    !is.na(kurdish_prop),
    !is.na(minority_prop),
    !is.na(ethnic_fractionalization),
    !is.na(urban_proxy),
    !is.na(district_area_km2),
    !is.na(phase_years)
  ) |>
  mutate(
    exposure_person_years = total_population * phase_years,
    log_exposure_offset = log(
      pmax(
        exposure_person_years,
        minimum_population_for_model
      )
    ),
    sunni_prop_z = as.numeric(scale(sunni_prop)),
    kurdish_prop_z = as.numeric(scale(kurdish_prop)),
    minority_prop_z = as.numeric(scale(minority_prop)),
    ethnic_fractionalization_z = as.numeric(scale(ethnic_fractionalization)),
    urban_proxy_z = as.numeric(scale(log1p(urban_proxy))),
    district_area_km2_z = as.numeric(scale(log1p(district_area_km2)))
  ) |>
  filter(
    is.finite(log_exposure_offset),
    is.finite(sunni_prop_z),
    is.finite(kurdish_prop_z),
    is.finite(minority_prop_z),
    is.finite(ethnic_fractionalization_z),
    is.finite(urban_proxy_z),
    is.finite(district_area_km2_z)
  )

model_data_check <- district_phase_model_data |>
  summarize(
    model_rows = n(),
    model_districts = n_distinct(district_id),
    model_phases = n_distinct(conflict_phase),
    total_observed_attacks = sum(isis_attack_count, na.rm = TRUE),
    zero_attack_rows = sum(isis_attack_count == 0, na.rm = TRUE),
    nonzero_attack_rows = sum(isis_attack_count > 0, na.rm = TRUE),
    mean_attack_count = mean(isis_attack_count, na.rm = TRUE),
    variance_attack_count = var(isis_attack_count, na.rm = TRUE),
    overdispersion_ratio = variance_attack_count / mean_attack_count
  )

print(model_data_check)

readr::write_csv(
  model_data_check,
  file.path(table_dir, "isis_district_phase_model_data_check.csv"),
  na = ""
)

readr::write_csv(
  district_phase_model_data,
  file.path(table_dir, "isis_district_phase_model_data.csv"),
  na = ""
)

readr::write_csv(
  district_phase_model_data,
  file.path(clean_dir, "isis_district_phase_model_data.csv"),
  na = ""
)


# ========================================================================
# 13) FIT NEGATIVE BINOMIAL MODEL SAFELY
# ========================================================================

nb_formula_phase <- as.formula(
  isis_attack_count ~
    sunni_prop_z +
    kurdish_prop_z +
    minority_prop_z +
    ethnic_fractionalization_z +
    urban_proxy_z +
    district_area_km2_z +
    conflict_phase +
    offset(log_exposure_offset)
)

nb_formula_baseline <- as.formula(
  isis_attack_count ~
    sunni_prop_z +
    kurdish_prop_z +
    minority_prop_z +
    ethnic_fractionalization_z +
    urban_proxy_z +
    district_area_km2_z +
    offset(log_exposure_offset)
)

isis_nb_model <- tryCatch(
  {
    MASS::glm.nb(
      formula = nb_formula_phase,
      data = district_phase_model_data,
      init.theta = 1,
      control = glm.control(
        maxit = 200,
        epsilon = 1e-8
      )
    )
  },
  warning = function(w_phase) {
    message(
      "Phase negative binomial model produced a warning. Trying simpler baseline negative binomial model."
    )

    MASS::glm.nb(
      formula = nb_formula_baseline,
      data = district_phase_model_data,
      init.theta = 1,
      control = glm.control(
        maxit = 200,
        epsilon = 1e-8
      )
    )
  },
  error = function(e_phase) {
    message(
      "Phase negative binomial model failed. Trying simpler baseline negative binomial model."
    )

    tryCatch(
      {
        MASS::glm.nb(
          formula = nb_formula_baseline,
          data = district_phase_model_data,
          init.theta = 1,
          control = glm.control(
            maxit = 200,
            epsilon = 1e-8
          )
        )
      },
      error = function(e_baseline) {
        message(
          "Negative binomial failed. Falling back to quasipoisson so residual map can still be produced."
        )

        glm(
          formula = nb_formula_baseline,
          family = quasipoisson(link = "log"),
          data = district_phase_model_data,
          control = glm.control(
            maxit = 200,
            epsilon = 1e-8
          )
        )
      }
    )
  }
)

saveRDS(
  isis_nb_model,
  file.path(model_dir, "isis_district_attack_model.rds")
)

model_summary_table <- broom::tidy(
  isis_nb_model,
  conf.int = TRUE,
  exponentiate = TRUE
) |>
  mutate(
    term = str_replace_all(
      term,
      "conflict_phase",
      "phase_"
    )
  )

model_glance_table <- broom::glance(
  isis_nb_model
)

model_formula_used <- tibble(
  model_class = class(isis_nb_model)[1],
  model_formula_used = as.character(formula(isis_nb_model))[3]
)

print(model_summary_table, n = 100)

print(model_glance_table)

print(model_formula_used)

readr::write_csv(
  model_summary_table,
  file.path(table_dir, "isis_attack_model_summary.csv"),
  na = ""
)

readr::write_csv(
  model_glance_table,
  file.path(table_dir, "isis_attack_model_glance.csv"),
  na = ""
)

readr::write_csv(
  model_formula_used,
  file.path(table_dir, "isis_attack_model_formula_used.csv"),
  na = ""
)


# ========================================================================
# 14) GENERATE PREDICTED ATTACK COUNTS AND RESIDUALS
# ========================================================================

district_phase_predictions <- district_phase_model_data |>
  mutate(
    predicted_attack_count = predict(
      object = isis_nb_model,
      newdata = district_phase_model_data,
      type = "response"
    ),
    raw_residual = isis_attack_count - predicted_attack_count,
    response_residual = residuals(
      object = isis_nb_model,
      type = "response"
    ),
    pearson_residual = residuals(
      object = isis_nb_model,
      type = "pearson"
    )
  )

readr::write_csv(
  district_phase_predictions,
  file.path(table_dir, "isis_district_phase_predictions.csv"),
  na = ""
)


# ========================================================================
# 15) AGGREGATE RESIDUALS BACK TO DISTRICTS
# ========================================================================

district_residuals <- district_phase_predictions |>
  group_by(
    district_id,
    district_name,
    governorate,
    governorate_id,
    district_label
  ) |>
  summarize(
    total_population = first(total_population),
    sunni_prop = first(sunni_prop),
    shia_prop = first(shia_prop),
    kurdish_prop = first(kurdish_prop),
    minority_prop = first(minority_prop),
    ethnic_fractionalization = first(ethnic_fractionalization),
    demographic_context = first(demographic_context),
    dominant_group = first(dominant_group),
    urban_proxy = first(urban_proxy),
    road_density = first(road_density),
    built_up_area = first(built_up_area),
    district_area_km2 = first(district_area_km2),
    observed_isis_attacks = sum(
      isis_attack_count,
      na.rm = TRUE
    ),
    predicted_isis_attacks = sum(
      predicted_attack_count,
      na.rm = TRUE
    ),
    raw_residual = observed_isis_attacks - predicted_isis_attacks,
    absolute_residual = abs(raw_residual),
    residual_ratio = if_else(
      predicted_isis_attacks > 0,
      observed_isis_attacks / predicted_isis_attacks,
      NA_real_
    ),
    mean_pearson_residual = mean(
      pearson_residual,
      na.rm = TRUE
    ),
    max_pearson_residual = max(
      pearson_residual,
      na.rm = TRUE
    ),
    min_pearson_residual = min(
      pearson_residual,
      na.rm = TRUE
    ),
    active_phases_observed = sum(
      isis_attack_count > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    residual_direction = case_when(
      raw_residual > 0 ~ "Higher than expected",
      raw_residual < 0 ~ "Lower than expected",
      raw_residual == 0 ~ "Exactly expected",
      TRUE ~ NA_character_
    ),
    residual_rank_high = min_rank(
      desc(raw_residual)
    ),
    residual_rank_low = min_rank(
      raw_residual
    ),
    residual_rank_absolute = min_rank(
      desc(absolute_residual)
    )
  ) |>
  arrange(
    desc(raw_residual)
  )

district_residual_check <- district_residuals |>
  summarize(
    districts = n(),
    total_observed_attacks = sum(
      observed_isis_attacks,
      na.rm = TRUE
    ),
    total_predicted_attacks = sum(
      predicted_isis_attacks,
      na.rm = TRUE
    ),
    minimum_raw_residual = min(
      raw_residual,
      na.rm = TRUE
    ),
    median_raw_residual = median(
      raw_residual,
      na.rm = TRUE
    ),
    maximum_raw_residual = max(
      raw_residual,
      na.rm = TRUE
    )
  )

print(district_residual_check)

readr::write_csv(
  district_residual_check,
  file.path(table_dir, "isis_district_residual_check.csv"),
  na = ""
)

readr::write_csv(
  district_residuals,
  file.path(table_dir, "isis_district_residuals.csv"),
  na = ""
)

readr::write_csv(
  district_residuals,
  file.path(clean_dir, "isis_district_residuals.csv"),
  na = ""
)


# ========================================================================
# 16) IDENTIFY HIGH-RESIDUAL AND LOW-RESIDUAL DISTRICTS
# ========================================================================

high_residual_districts <- district_residuals |>
  slice_max(
    order_by = raw_residual,
    n = top_residual_districts_to_save,
    with_ties = FALSE
  ) |>
  dplyr::select(
    district_name,
    governorate,
    district_label,
    observed_isis_attacks,
    predicted_isis_attacks,
    raw_residual,
    residual_ratio,
    total_population,
    sunni_prop,
    shia_prop,
    kurdish_prop,
    minority_prop,
    ethnic_fractionalization,
    demographic_context,
    dominant_group,
    urban_proxy,
    district_area_km2
  )

low_residual_districts <- district_residuals |>
  slice_min(
    order_by = raw_residual,
    n = top_residual_districts_to_save,
    with_ties = FALSE
  ) |>
  dplyr::select(
    district_name,
    governorate,
    district_label,
    observed_isis_attacks,
    predicted_isis_attacks,
    raw_residual,
    residual_ratio,
    total_population,
    sunni_prop,
    shia_prop,
    kurdish_prop,
    minority_prop,
    ethnic_fractionalization,
    demographic_context,
    dominant_group,
    urban_proxy,
    district_area_km2
  )

print(high_residual_districts, n = top_residual_districts_to_save)

print(low_residual_districts, n = top_residual_districts_to_save)

readr::write_csv(
  high_residual_districts,
  file.path(table_dir, "isis_high_residual_districts.csv"),
  na = ""
)

readr::write_csv(
  low_residual_districts,
  file.path(table_dir, "isis_low_residual_districts.csv"),
  na = ""
)


# ========================================================================
# 17) JOIN RESIDUALS BACK TO DISTRICT POLYGONS
# ========================================================================

district_residuals_sf <- districts_clean_sf |>
  left_join(
    district_residuals,
    by = c(
      "district_id",
      "district_name",
      "governorate",
      "governorate_id",
      "district_label"
    )
  )

readr::write_csv(
  district_residuals_sf |>
    sf::st_drop_geometry(),
  file.path(table_dir, "isis_district_residuals_for_mapping.csv"),
  na = ""
)


# ========================================================================
# 18) MAP SPATIAL RESIDUALS
# ========================================================================

residual_limit <- max(
  abs(district_residuals_sf$raw_residual),
  na.rm = TRUE
)

plot_spatial_residual_map <- district_residuals_sf |>
  ggplot() +
  geom_sf(
    aes(
      fill = raw_residual
    ),
    color = "grey35",
    linewidth = 0.10
  ) +
  scale_fill_gradient2(
    low = "#2E2D5B",
    mid = "grey95",
    high = "#842829",
    midpoint = 0,
    limits = c(
      -residual_limit,
      residual_limit
    ),
    oob = scales::squish,
    labels = number_format(
      accuracy = 1
    ),
    na.value = "grey80",
    name = "Observed − predicted\nISIS attacks"
  ) +
  labs(
    title = "Spatial Residuals from ISIS District Attack Model",
    subtitle = "Positive residuals indicate more ISIS attacks than expected after population, demography, urban proxy, district area, and year controls",
    caption = "Model: negative binomial district-year count model with log LandScan population offset. Road density and built-up area are not included because those files are not available.",
    x = NULL,
    y = NULL
  ) +
  theme_void(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    plot.subtitle = element_text(
      margin = margin(
        b = 8
      )
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      margin = margin(
        t = 8
      )
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 9
    ),
    legend.text = element_text(
      size = 8
    )
  )

print(plot_spatial_residual_map)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_spatial_residual_map_nb_model.png"
  ),
  plot = plot_spatial_residual_map,
  width = 11,
  height = 8,
  dpi = 300
)


# ========================================================================
# 19) PLOT OBSERVED VS PREDICTED DISTRICT ATTACKS
# ========================================================================

plot_observed_vs_predicted <- district_residuals |>
  ggplot(
    aes(
      x = predicted_isis_attacks,
      y = observed_isis_attacks
    )
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.50,
    color = "grey35"
  ) +
  geom_point(
    aes(
      size = total_population,
      color = raw_residual
    ),
    alpha = 0.80
  ) +
  scale_x_continuous(
    labels = number_format(
      accuracy = 1
    )
  ) +
  scale_y_continuous(
    labels = number_format(
      accuracy = 1
    )
  ) +
  scale_size_continuous(
    labels = comma_format(),
    name = "LandScan population"
  ) +
  scale_color_gradient2(
    low = "#2E2D5B",
    mid = "grey70",
    high = "#842829",
    midpoint = 0,
    name = "Observed − predicted"
  ) +
  labs(
    title = "Observed vs. Predicted ISIS Attacks by District",
    subtitle = "Points above the dashed line had more attacks than expected; points below had fewer",
    caption = "Predictions are summed from district-year negative binomial fitted values.",
    x = "Predicted ISIS attacks",
    y = "Observed ISIS attacks"
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    plot.subtitle = element_text(
      margin = margin(
        b = 8
      )
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      margin = margin(
        t = 8
      )
    ),
    legend.position = "right"
  )

print(plot_observed_vs_predicted)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_observed_vs_predicted_district_attacks.png"
  ),
  plot = plot_observed_vs_predicted,
  width = 10,
  height = 7,
  dpi = 300
)


# ========================================================================
# 20) PLOT HIGHEST AND LOWEST RESIDUAL DISTRICTS
# ========================================================================

high_low_residual_districts <- bind_rows(
  high_residual_districts |>
    mutate(
      residual_group = "Higher than expected"
    ),
  low_residual_districts |>
    mutate(
      residual_group = "Lower than expected"
    )
) |>
  mutate(
    district_label_plot = str_c(
      district_name,
      " [",
      governorate,
      "]"
    ),
    district_label_plot = fct_reorder(
      district_label_plot,
      raw_residual
    )
  )

plot_high_low_residual_districts <- high_low_residual_districts |>
  ggplot(
    aes(
      x = raw_residual,
      y = district_label_plot,
      fill = residual_group
    )
  ) +
  geom_col(
    width = 0.72
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.45,
    color = "grey30"
  ) +
  scale_x_continuous(
    labels = number_format(
      accuracy = 1
    )
  ) +
  scale_fill_manual(
    values = c(
      "Higher than expected" = "#842829",
      "Lower than expected" = "#2E2D5B"
    ),
    name = NULL
  ) +
  labs(
    title = "Districts with the Largest ISIS Attack Model Residuals",
    subtitle = "Residual = observed ISIS attacks minus predicted ISIS attacks",
    caption = "Large positive residuals may indicate unmodeled strategic value, local network strength, battlefield dynamics, access corridors, reporting differences, or omitted covariates.",
    x = "Observed − predicted ISIS attacks",
    y = NULL
  ) +
  theme_classic(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    plot.subtitle = element_text(
      margin = margin(
        b = 8
      )
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      margin = margin(
        t = 8
      )
    ),
    legend.position = "bottom"
  )

print(plot_high_low_residual_districts)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_high_low_residual_districts.png"
  ),
  plot = plot_high_low_residual_districts,
  width = 11,
  height = 9,
  dpi = 300
)


# ========================================================================
# 21) OPTIONAL MAP: OBSERVED / PREDICTED RATIO
# ========================================================================

plot_residual_ratio_map <- district_residuals_sf |>
  mutate(
    residual_ratio_capped = pmin(
      residual_ratio,
      5
    )
  ) |>
  ggplot() +
  geom_sf(
    aes(
      fill = residual_ratio_capped
    ),
    color = "grey35",
    linewidth = 0.10
  ) +
  scale_fill_viridis_c(
    option = "magma",
    labels = number_format(
      accuracy = 0.1
    ),
    na.value = "grey80",
    name = "Observed / predicted\ncapped at 5"
  ) +
  labs(
    title = "Observed-to-Predicted ISIS Attack Ratio by District",
    subtitle = "Values above 1 indicate more attacks than expected; values below 1 indicate fewer than expected",
    caption = "Ratio is capped at 5 for map readability. Use the residual table for exact values.",
    x = NULL,
    y = NULL
  ) +
  theme_void(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    plot.subtitle = element_text(
      margin = margin(
        b = 8
      )
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      margin = margin(
        t = 8
      )
    ),
    legend.position = "right"
  )

print(plot_residual_ratio_map)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_observed_predicted_ratio_map_nb_model.png"
  ),
  plot = plot_residual_ratio_map,
  width = 11,
  height = 8,
  dpi = 300
)


# ========================================================================
# 22) FINAL DIAGNOSTIC SUMMARY
# ========================================================================

final_diagnostic_summary <- tibble(
  diagnostic = c(
    "Raw Iraq GTD events",
    "ISIS / Islamic State Iraq events",
    "ISIS events with valid coordinates",
    "ISIS events matched to districts",
    "Districts in boundary file",
    "Districts in model data",
    "District-year rows in model",
    "Total observed ISIS attacks in model",
    "Total predicted ISIS attacks in model",
    "Road density included in model",
    "Built-up area included in model",
    "Negative binomial theta",
    "Model AIC",
    "Largest positive raw residual",
    "Largest negative raw residual",
    "District with largest positive residual",
    "District with largest negative residual"
  ),
  value = c(
    nrow(gtd_iraq),
    nrow(gtd_isis_iraq),
    nrow(gtd_isis_coordinates),
    spatial_join_check$isis_attacks_matched_to_district,
    nrow(districts_clean_table),
    n_distinct(district_year_model_data$district_id),
    nrow(district_year_model_data),
    sum(district_year_predictions$isis_attack_count, na.rm = TRUE),
    sum(district_year_predictions$predicted_attack_count, na.rm = TRUE),
    "No; no road file available in workspace",
    "No; no built-up-area file available in workspace",
    isis_nb_model$theta,
    AIC(isis_nb_model),
    max(district_residuals$raw_residual, na.rm = TRUE),
    min(district_residuals$raw_residual, na.rm = TRUE),
    district_residuals |>
      slice_max(raw_residual, n = 1, with_ties = FALSE) |>
      pull(district_label),
    district_residuals |>
      slice_min(raw_residual, n = 1, with_ties = FALSE) |>
      pull(district_label)
  )
)

print(final_diagnostic_summary)

readr::write_csv(
  final_diagnostic_summary,
  file.path(table_dir, "isis_spatial_residual_model_diagnostics.csv"),
  na = ""
)
