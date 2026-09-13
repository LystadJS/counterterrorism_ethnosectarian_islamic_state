# ========================================================================
# MODEL-BASED TARGETING MECHANISM VISUALIZATION
# CIVILIAN + RELIGIOUS + INFRASTRUCTURE COMBINED
# FROM SCRATCH
#
# PURPOSE:
#   Build an event-level ISIS targeting dataset for Iraq and estimate
#   whether district demographic composition predicts target-selection
#   mechanism.
#
# CORE CONTRAST:
#   1. State / security targets
#   2. Civilian / religious / infrastructure targets
#
# UNIT OF RAW DATA:
#   One GTD attack event
#
# UNIT OF MODELING DATA:
#   District × conflict phase grouped target counts
#
# KEY PREDICTORS:
#   - LandScan-weighted Sunni population share
#   - Ethnic / sectarian fractionalization
#   - Conflict phase
#
# KEY OUTPUTS:
#   outputs/plots/predicted_civ_rel_infra_targeting_by_sunni_share.png
#   outputs/plots/predicted_civ_rel_infra_targeting_by_fractionalization.png
#   outputs/plots/observed_targeting_mechanism_by_phase_and_group.png
#   outputs/tables/targeting_mechanism_binary_model_data.csv
#   outputs/tables/targeting_mechanism_binary_model_results.csv
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
library(broom)


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
# 02) UNZIP ETHNICITY DATA AND LOCATE SHAPEFILES
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
    nwound = readr::parse_number(nwound),
    target_type_raw = str_squish(as.character(targtype1_txt)),
    attack_type_raw = str_squish(as.character(attacktype1_txt)),
    weapon_type_raw = str_squish(as.character(weaptype1_txt))
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
    ),
    event_year = lubridate::year(event_month),
    known_fatalities_event = replace_na(nkill, 0),
    known_wounded_event = replace_na(nwound, 0),
    known_casualties_event = known_fatalities_event + known_wounded_event
  )


# ========================================================================
# 04) RECODE TARGET TYPES INTO BINARY TARGETING MECHANISM
# ========================================================================

gtd_iraq <- gtd_iraq |>
  mutate(
    target_mechanism = case_when(
      target_type_raw %in%
        c(
          "Military",
          "Police",
          "Government (General)",
          "Government (Diplomatic)"
        ) ~ "State / security",

      target_type_raw %in%
        c(
          "Private Citizens & Property",
          "Educational Institution",
          "Journalists & Media",
          "NGO",
          "Tourists",
          "Food or Water Supply",
          "Religious Figures/Institutions",
          "Business",
          "Transportation",
          "Utilities",
          "Telecommunication",
          "Airports & Aircraft",
          "Maritime"
        ) ~ "Civilian / religious / infrastructure",

      target_type_raw %in%
        c(
          "Violent Political Party",
          "Terrorists/Non-State Militia",
          "Other",
          "Unknown"
        ) ~ "Other / excluded",

      is.na(target_type_raw) | target_type_raw == "" ~ "Other / excluded",

      TRUE ~ "Other / excluded"
    ),
    target_mechanism = factor(
      target_mechanism,
      levels = c(
        "State / security",
        "Civilian / religious / infrastructure",
        "Other / excluded"
      )
    ),
    state_security_target = target_mechanism == "State / security",
    civilian_religious_infrastructure_target = target_mechanism ==
      "Civilian / religious / infrastructure",
    classified_target = state_security_target |
      civilian_religious_infrastructure_target
  )

target_recode_check <- gtd_iraq |>
  count(
    target_type_raw,
    target_mechanism,
    sort = TRUE
  )

readr::write_csv(
  target_recode_check,
  file.path(table_dir, "target_type_recode_check.csv"),
  na = ""
)

print(target_recode_check, n = 50)


# ========================================================================
# 05) IMPORT DISTRICT AND ETHNICITY SHAPEFILES
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
# 06) BUILD LANDSCAN-WEIGHTED DISTRICT DEMOGRAPHICS
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
    ),
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

readr::write_csv(
  ethnicity_shares,
  file.path(clean_dir, "iraq_district_ethnicity_landscan_weighted.csv"),
  na = ""
)


# ========================================================================
# 07) SPATIALLY JOIN GTD EVENTS TO DISTRICTS
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

readr::write_csv(
  gtd_attacks_district,
  file.path(clean_dir, "gtd_iraq_attacks_with_districts.csv"),
  na = ""
)


# ========================================================================
# 08) JOIN EVENT TARGETS TO DISTRICT DEMOGRAPHICS
# ========================================================================

isis_event_target_demographics <- gtd_attacks_district |>
  filter(
    !is.na(district_id)
  ) |>
  left_join(
    ethnicity_shares |>
      dplyr::select(
        district_id,
        district_label,
        landscan_population_total,
        kurdish_prop,
        shia_prop,
        sunni_prop,
        christian_prop,
        turcoman_prop,
        other_prop,
        dominant_group,
        dominant_group_prop,
        ethnic_fractionalization
      ),
    by = c(
      "district_id",
      "district_label"
    )
  ) |>
  mutate(
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

event_target_check <- isis_event_target_demographics |>
  summarize(
    events = n(),
    classified_events = sum(classified_target, na.rm = TRUE),
    excluded_events = sum(!classified_target, na.rm = TRUE),
    classified_share = classified_events / events,
    districts = n_distinct(district_id),
    missing_conflict_phase = sum(is.na(conflict_phase)),
    missing_sunni_prop = sum(is.na(sunni_prop)),
    missing_fractionalization = sum(is.na(ethnic_fractionalization))
  )

print(event_target_check)

readr::write_csv(
  isis_event_target_demographics,
  file.path(table_dir, "isis_event_target_demographics_binary_mechanism.csv"),
  na = ""
)


# ========================================================================
# 09) BUILD DISTRICT-PHASE TARGETING MECHANISM COUNTS
# ========================================================================

minimum_classified_events_per_district_phase <- 5

targeting_mechanism_district_phase <- isis_event_target_demographics |>
  filter(
    classified_target,
    !is.na(conflict_phase),
    !is.na(sunni_prop),
    !is.na(ethnic_fractionalization),
    !is.na(dominant_group),
    !is.na(governorate)
  ) |>
  group_by(
    district_id,
    district_label,
    governorate,
    conflict_phase,
    dominant_group
  ) |>
  summarize(
    classified_events = n(),
    state_security_events = sum(state_security_target, na.rm = TRUE),
    civilian_religious_infrastructure_events = sum(
      civilian_religious_infrastructure_target,
      na.rm = TRUE
    ),
    sunni_prop = first(sunni_prop),
    shia_prop = first(shia_prop),
    kurdish_prop = first(kurdish_prop),
    dominant_group_prop = first(dominant_group_prop),
    ethnic_fractionalization = first(ethnic_fractionalization),
    landscan_population_total = first(landscan_population_total),
    .groups = "drop"
  ) |>
  filter(
    classified_events >= minimum_classified_events_per_district_phase
  )

scaling_values <- targeting_mechanism_district_phase |>
  summarize(
    sunni_mean = mean(sunni_prop, na.rm = TRUE),
    sunni_sd = sd(sunni_prop, na.rm = TRUE),
    fractionalization_mean = mean(ethnic_fractionalization, na.rm = TRUE),
    fractionalization_sd = sd(ethnic_fractionalization, na.rm = TRUE)
  )

targeting_mechanism_district_phase <- targeting_mechanism_district_phase |>
  mutate(
    sunni_prop_scaled = (sunni_prop - scaling_values$sunni_mean) /
      scaling_values$sunni_sd,
    ethnic_fractionalization_scaled = (ethnic_fractionalization -
      scaling_values$fractionalization_mean) /
      scaling_values$fractionalization_sd,
    civilian_religious_infrastructure_share = civilian_religious_infrastructure_events /
      classified_events,
    state_security_share = state_security_events / classified_events
  )

targeting_model_data_check <- targeting_mechanism_district_phase |>
  summarize(
    rows = n(),
    districts = n_distinct(district_id),
    phases = n_distinct(conflict_phase),
    classified_events = sum(classified_events, na.rm = TRUE),
    state_security_events = sum(state_security_events, na.rm = TRUE),
    civilian_religious_infrastructure_events = sum(
      civilian_religious_infrastructure_events,
      na.rm = TRUE
    )
  )

print(targeting_model_data_check)

readr::write_csv(
  targeting_mechanism_district_phase,
  file.path(table_dir, "targeting_mechanism_binary_model_data.csv"),
  na = ""
)


# ========================================================================
# 10) FIT GROUPED QUASIBINOMIAL MODEL
# ========================================================================

targeting_mechanism_model <- glm(
  cbind(
    civilian_religious_infrastructure_events,
    state_security_events
  ) ~
    sunni_prop_scaled *
    conflict_phase +
    ethnic_fractionalization_scaled * conflict_phase,
  data = targeting_mechanism_district_phase,
  family = quasibinomial()
)

targeting_mechanism_model_results <- broom::tidy(
  targeting_mechanism_model,
  conf.int = TRUE
)

print(targeting_mechanism_model_results)

readr::write_csv(
  targeting_mechanism_model_results,
  file.path(table_dir, "targeting_mechanism_binary_model_results.csv"),
  na = ""
)


# ========================================================================
# 11) OPTIONAL GOVERNORATE-ADJUSTED ROBUSTNESS MODEL
# ========================================================================

targeting_mechanism_model_governorate_adjusted <- glm(
  cbind(
    civilian_religious_infrastructure_events,
    state_security_events
  ) ~
    sunni_prop_scaled *
    conflict_phase +
    ethnic_fractionalization_scaled * conflict_phase +
    governorate,
  data = targeting_mechanism_district_phase,
  family = quasibinomial()
)

targeting_mechanism_model_governorate_adjusted_results <- broom::tidy(
  targeting_mechanism_model_governorate_adjusted,
  conf.int = TRUE
)

readr::write_csv(
  targeting_mechanism_model_governorate_adjusted_results,
  file.path(
    table_dir,
    "targeting_mechanism_binary_model_results_governorate_adjusted.csv"
  ),
  na = ""
)


# ========================================================================
# 12) PREDICTION HELPER
# ========================================================================

make_prediction <- function(model_object, prediction_grid) {
  link_prediction <- predict(
    model_object,
    newdata = prediction_grid,
    type = "link",
    se.fit = TRUE
  )

  prediction_grid |>
    mutate(
      estimate = plogis(link_prediction$fit),
      conf_low = plogis(link_prediction$fit - 1.96 * link_prediction$se.fit),
      conf_high = plogis(link_prediction$fit + 1.96 * link_prediction$se.fit)
    )
}

phase_levels <- levels(
  targeting_mechanism_district_phase$conflict_phase
)


# ========================================================================
# 13) PREDICTED MECHANISM PROBABILITY BY SUNNI SHARE
# ========================================================================

prediction_grid_sunni <- tidyr::crossing(
  sunni_prop = seq(
    from = 0,
    to = 1,
    length.out = 101
  ),
  ethnic_fractionalization = median(
    targeting_mechanism_district_phase$ethnic_fractionalization,
    na.rm = TRUE
  ),
  conflict_phase = factor(
    phase_levels,
    levels = phase_levels
  )
) |>
  mutate(
    sunni_prop_scaled = (sunni_prop - scaling_values$sunni_mean) /
      scaling_values$sunni_sd,
    ethnic_fractionalization_scaled = (ethnic_fractionalization -
      scaling_values$fractionalization_mean) /
      scaling_values$fractionalization_sd
  )

predicted_by_sunni <- make_prediction(
  targeting_mechanism_model,
  prediction_grid_sunni
)

readr::write_csv(
  predicted_by_sunni,
  file.path(
    table_dir,
    "predicted_civ_rel_infra_targeting_by_sunni_share.csv"
  ),
  na = ""
)

plot_predicted_by_sunni <- predicted_by_sunni |>
  ggplot(
    aes(
      x = sunni_prop,
      y = estimate,
      color = conflict_phase,
      fill = conflict_phase
    )
  ) +
  geom_ribbon(
    aes(
      ymin = conf_low,
      ymax = conf_high
    ),
    alpha = 0.18,
    color = NA
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Predicted ISIS Civilian/Religious/Infrastructure Targeting by Sunni Share",
    subtitle = "Grouped quasibinomial model; comparison category is state/security targeting",
    x = "LandScan-weighted Sunni population share",
    y = "Predicted probability of civilian/religious/infrastructure targeting",
    color = "Conflict phase",
    fill = "Conflict phase"
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

print(plot_predicted_by_sunni)

ggsave(
  filename = file.path(
    plot_dir,
    "predicted_civ_rel_infra_targeting_by_sunni_share.png"
  ),
  plot = plot_predicted_by_sunni,
  width = 11,
  height = 7,
  dpi = 300
)


# ========================================================================
# 14) PREDICTED MECHANISM PROBABILITY BY FRACTIONALIZATION
# ========================================================================

prediction_grid_fractionalization <- tidyr::crossing(
  ethnic_fractionalization = seq(
    from = min(
      targeting_mechanism_district_phase$ethnic_fractionalization,
      na.rm = TRUE
    ),
    to = max(
      targeting_mechanism_district_phase$ethnic_fractionalization,
      na.rm = TRUE
    ),
    length.out = 101
  ),
  sunni_prop = median(
    targeting_mechanism_district_phase$sunni_prop,
    na.rm = TRUE
  ),
  conflict_phase = factor(
    phase_levels,
    levels = phase_levels
  )
) |>
  mutate(
    sunni_prop_scaled = (sunni_prop - scaling_values$sunni_mean) /
      scaling_values$sunni_sd,
    ethnic_fractionalization_scaled = (ethnic_fractionalization -
      scaling_values$fractionalization_mean) /
      scaling_values$fractionalization_sd
  )

predicted_by_fractionalization <- make_prediction(
  targeting_mechanism_model,
  prediction_grid_fractionalization
)

readr::write_csv(
  predicted_by_fractionalization,
  file.path(
    table_dir,
    "predicted_civ_rel_infra_targeting_by_fractionalization.csv"
  ),
  na = ""
)

plot_predicted_by_fractionalization <- predicted_by_fractionalization |>
  ggplot(
    aes(
      x = ethnic_fractionalization,
      y = estimate,
      color = conflict_phase,
      fill = conflict_phase
    )
  ) +
  geom_ribbon(
    aes(
      ymin = conf_low,
      ymax = conf_high
    ),
    alpha = 0.18,
    color = NA
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  scale_x_continuous(
    labels = number_format(accuracy = 0.01)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Predicted ISIS Civilian/Religious/Infrastructure Targeting by Fractionalization",
    subtitle = "Grouped quasibinomial model; comparison category is state/security targeting",
    x = "Ethnic / sectarian fractionalization",
    y = "Predicted probability of civilian/religious/infrastructure targeting",
    color = "Conflict phase",
    fill = "Conflict phase"
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

print(plot_predicted_by_fractionalization)

ggsave(
  filename = file.path(
    plot_dir,
    "predicted_civ_rel_infra_targeting_by_fractionalization.png"
  ),
  plot = plot_predicted_by_fractionalization,
  width = 11,
  height = 7,
  dpi = 300
)


# ========================================================================
# 15) OBSERVED TARGETING MECHANISM BY PHASE AND DOMINANT GROUP
# ========================================================================

observed_targeting_mechanism <- targeting_mechanism_district_phase |>
  group_by(
    conflict_phase,
    dominant_group
  ) |>
  summarize(
    classified_events = sum(classified_events, na.rm = TRUE),
    state_security_events = sum(state_security_events, na.rm = TRUE),
    civilian_religious_infrastructure_events = sum(
      civilian_religious_infrastructure_events,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    state_security_share = state_security_events / classified_events,
    civilian_religious_infrastructure_share = civilian_religious_infrastructure_events /
      classified_events
  ) |>
  pivot_longer(
    cols = c(
      state_security_share,
      civilian_religious_infrastructure_share
    ),
    names_to = "mechanism",
    values_to = "share"
  ) |>
  mutate(
    mechanism = case_when(
      mechanism == "state_security_share" ~ "State / security",
      mechanism == "civilian_religious_infrastructure_share" ~
        "Civilian / religious / infrastructure",
      TRUE ~ mechanism
    ),
    mechanism = factor(
      mechanism,
      levels = c(
        "State / security",
        "Civilian / religious / infrastructure"
      )
    )
  )

readr::write_csv(
  observed_targeting_mechanism,
  file.path(table_dir, "observed_targeting_mechanism_by_phase_and_group.csv"),
  na = ""
)

plot_observed_targeting_mechanism <- observed_targeting_mechanism |>
  ggplot(
    aes(
      x = dominant_group,
      y = share,
      fill = mechanism
    )
  ) +
  geom_col(
    position = "fill",
    width = 0.75
  ) +
  facet_wrap(
    vars(conflict_phase),
    nrow = 1
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Observed ISIS Targeting Mechanism by Conflict Phase and District Type",
    subtitle = "Bars show classified target shares within each phase × dominant-group category",
    x = "Dominant district group",
    y = "Share of classified attacks",
    fill = "Targeting mechanism"
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    legend.position = "bottom"
  )

print(plot_observed_targeting_mechanism)

ggsave(
  filename = file.path(
    plot_dir,
    "observed_targeting_mechanism_by_phase_and_group.png"
  ),
  plot = plot_observed_targeting_mechanism,
  width = 13,
  height = 7,
  dpi = 300
)


# ========================================================================
# 16) OBSERVED TARGETING MECHANISM BY SUNNI-SHARE BIN
# ========================================================================

observed_targeting_by_sunni_bin <- isis_event_target_demographics |>
  filter(
    classified_target,
    !is.na(sunni_prop),
    !is.na(conflict_phase)
  ) |>
  mutate(
    sunni_share_bin = case_when(
      sunni_prop < 0.25 ~ "0–24% Sunni",
      sunni_prop < 0.50 ~ "25–49% Sunni",
      sunni_prop < 0.75 ~ "50–74% Sunni",
      sunni_prop >= 0.75 ~ "75–100% Sunni",
      TRUE ~ NA_character_
    ),
    sunni_share_bin = factor(
      sunni_share_bin,
      levels = c(
        "0–24% Sunni",
        "25–49% Sunni",
        "50–74% Sunni",
        "75–100% Sunni"
      )
    )
  ) |>
  count(
    conflict_phase,
    sunni_share_bin,
    target_mechanism,
    name = "events"
  ) |>
  group_by(
    conflict_phase,
    sunni_share_bin
  ) |>
  mutate(
    bin_total_events = sum(events, na.rm = TRUE),
    target_share = events / bin_total_events
  ) |>
  ungroup()

readr::write_csv(
  observed_targeting_by_sunni_bin,
  file.path(table_dir, "observed_targeting_mechanism_by_sunni_bin.csv"),
  na = ""
)

plot_observed_targeting_by_sunni_bin <- observed_targeting_by_sunni_bin |>
  filter(
    target_mechanism %in%
      c(
        "State / security",
        "Civilian / religious / infrastructure"
      )
  ) |>
  ggplot(
    aes(
      x = sunni_share_bin,
      y = target_share,
      fill = target_mechanism
    )
  ) +
  geom_col(
    position = "fill",
    width = 0.75
  ) +
  facet_wrap(
    vars(conflict_phase),
    nrow = 1
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Observed ISIS Targeting Mechanism by Sunni Population-Share Bin",
    subtitle = "Bars show classified target shares within each phase × Sunni-share bin",
    x = "LandScan-weighted Sunni population-share bin",
    y = "Share of classified attacks",
    fill = "Targeting mechanism"
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    legend.position = "bottom"
  )

print(plot_observed_targeting_by_sunni_bin)

ggsave(
  filename = file.path(
    plot_dir,
    "observed_targeting_mechanism_by_sunni_bin.png"
  ),
  plot = plot_observed_targeting_by_sunni_bin,
  width = 13,
  height = 7,
  dpi = 300
)
