# ========================================================================
# POPULATION-ADJUSTED ISIS ATTACK-RATE MAP
# ESOC + LANDSCAN DENOMINATOR
# FROM SCRATCH
#
# PURPOSE:
#   Build a district-level ISIS attack-rate dataset for Iraq and map
#   whether ISIS attacks were concentrated relative to district population.
#
# CORE CONTRAST:
#   1. Raw ISIS attack counts by district
#   2. ISIS attacks per 100,000 people by district
#
# UNIT OF RAW DATA:
#   One GTD attack event
#
# UNIT OF ANALYTIC DATA:
#   Iraq district
#
# KEY PREDICTORS / CONTEXT VARIABLES:
#   - LandScan-weighted Sunni population share
#   - LandScan-weighted Shia population share
#   - LandScan-weighted Kurdish population share
#   - Ethnic / sectarian fractionalization
#   - Dominant district group
#
# KEY OUTCOMES:
#   - ISIS attack count
#   - ISIS attacks per 100,000 people
#
# KEY OUTPUTS:
#   outputs/plots/isis_attack_rate_per_100k_by_district.png
#   outputs/plots/isis_raw_attack_count_by_district.png
#   outputs/tables/district_isis_attack_rate_landscan.csv
#   data/clean/district_isis_attack_rate_landscan.csv
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
    group_name_raw = str_squish(as.character(gname)),
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
# 04) FILTER TO ISIS / ISLAMIC STATE ATTACKS
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

readr::write_csv(
  isis_filter_check,
  file.path(table_dir, "isis_group_name_filter_check.csv"),
  na = ""
)

print(isis_filter_check, n = 50)


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

readr::write_csv(
  districts_clean_table,
  file.path(clean_dir, "iraq_district_boundaries_clean_table.csv"),
  na = ""
)


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
# 07) SPATIALLY JOIN ISIS EVENTS TO DISTRICTS
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
    district_label,
    population_total_esoc,
    district_area_km2,
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
  gtd_isis_district,
  file.path(clean_dir, "gtd_isis_iraq_attacks_with_districts.csv"),
  na = ""
)


# ========================================================================
# 08) COUNT ISIS ATTACKS BY DISTRICT
# ========================================================================

district_isis_counts <- gtd_isis_district |>
  filter(
    !is.na(district_id)
  ) |>
  group_by(
    district_id,
    district_label,
    district_name,
    governorate
  ) |>
  summarize(
    isis_attack_count = n(),
    isis_fatalities_known = sum(known_fatalities_event, na.rm = TRUE),
    isis_wounded_known = sum(known_wounded_event, na.rm = TRUE),
    isis_casualties_known = sum(known_casualties_event, na.rm = TRUE),
    first_isis_attack_month = min(event_month, na.rm = TRUE),
    last_isis_attack_month = max(event_month, na.rm = TRUE),
    .groups = "drop"
  )

district_isis_count_check <- district_isis_counts |>
  summarize(
    attacked_districts = n(),
    total_isis_attacks_joined = sum(isis_attack_count, na.rm = TRUE),
    total_known_fatalities = sum(isis_fatalities_known, na.rm = TRUE),
    total_known_wounded = sum(isis_wounded_known, na.rm = TRUE),
    total_known_casualties = sum(isis_casualties_known, na.rm = TRUE)
  )

print(district_isis_count_check)

readr::write_csv(
  district_isis_counts,
  file.path(table_dir, "district_isis_raw_attack_counts.csv"),
  na = ""
)


# ========================================================================
# 09) BUILD DISTRICT-LEVEL POPULATION-ADJUSTED ATTACK-RATE DATA
# ========================================================================

district_isis_rate_sf <- districts_clean_sf |>
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
  left_join(
    district_isis_counts |>
      dplyr::select(
        district_id,
        district_label,
        isis_attack_count,
        isis_fatalities_known,
        isis_wounded_known,
        isis_casualties_known,
        first_isis_attack_month,
        last_isis_attack_month
      ),
    by = c(
      "district_id",
      "district_label"
    )
  ) |>
  mutate(
    isis_attack_count = replace_na(isis_attack_count, 0L),
    isis_fatalities_known = replace_na(isis_fatalities_known, 0),
    isis_wounded_known = replace_na(isis_wounded_known, 0),
    isis_casualties_known = replace_na(isis_casualties_known, 0),
    isis_attacks_per_100k = if_else(
      landscan_population_total > 0,
      100000 * isis_attack_count / landscan_population_total,
      NA_real_
    ),
    isis_attacks_per_10k = if_else(
      landscan_population_total > 0,
      10000 * isis_attack_count / landscan_population_total,
      NA_real_
    ),
    isis_fatalities_per_100k = if_else(
      landscan_population_total > 0,
      100000 * isis_fatalities_known / landscan_population_total,
      NA_real_
    ),
    population_data_status = case_when(
      is.na(landscan_population_total) ~ "Missing LandScan population",
      landscan_population_total <= 0 ~ "Zero or invalid LandScan population",
      TRUE ~ "Valid LandScan population"
    ),
    attack_status = case_when(
      isis_attack_count > 0 ~ "At least one ISIS attack",
      isis_attack_count == 0 ~ "No matched ISIS attacks",
      TRUE ~ NA_character_
    )
  )

district_isis_rate_table <- district_isis_rate_sf |>
  sf::st_drop_geometry() |>
  dplyr::select(
    district_id,
    district_name,
    governorate,
    governorate_id,
    district_label,
    population_total_esoc,
    landscan_population_total,
    district_area_km2,
    kurdish_prop,
    shia_prop,
    sunni_prop,
    christian_prop,
    turcoman_prop,
    other_prop,
    dominant_group,
    dominant_group_prop,
    ethnic_fractionalization,
    isis_attack_count,
    isis_attacks_per_100k,
    isis_attacks_per_10k,
    isis_fatalities_known,
    isis_wounded_known,
    isis_casualties_known,
    isis_fatalities_per_100k,
    first_isis_attack_month,
    last_isis_attack_month,
    population_data_status,
    attack_status
  ) |>
  arrange(
    desc(isis_attacks_per_100k)
  )

district_rate_check <- district_isis_rate_table |>
  summarize(
    districts = n(),
    districts_with_valid_landscan_population = sum(
      landscan_population_total > 0,
      na.rm = TRUE
    ),
    districts_with_isis_attacks = sum(
      isis_attack_count > 0,
      na.rm = TRUE
    ),
    total_isis_attacks = sum(
      isis_attack_count,
      na.rm = TRUE
    ),
    minimum_attack_rate = min(
      isis_attacks_per_100k,
      na.rm = TRUE
    ),
    median_attack_rate = median(
      isis_attacks_per_100k,
      na.rm = TRUE
    ),
    maximum_attack_rate = max(
      isis_attacks_per_100k,
      na.rm = TRUE
    )
  )

print(district_rate_check)

readr::write_csv(
  district_isis_rate_table,
  file.path(table_dir, "district_isis_attack_rate_landscan.csv"),
  na = ""
)

readr::write_csv(
  district_isis_rate_table,
  file.path(clean_dir, "district_isis_attack_rate_landscan.csv"),
  na = ""
)


# ========================================================================
# 10) DIAGNOSTIC TABLE: TOP DISTRICTS BY POPULATION-ADJUSTED RATE
# ========================================================================

top_districts_by_rate <- district_isis_rate_table |>
  filter(
    !is.na(isis_attacks_per_100k)
  ) |>
  slice_max(
    order_by = isis_attacks_per_100k,
    n = 25,
    with_ties = FALSE
  ) |>
  dplyr::select(
    district_name,
    governorate,
    district_label,
    dominant_group,
    dominant_group_prop,
    landscan_population_total,
    isis_attack_count,
    isis_attacks_per_100k,
    isis_fatalities_known,
    isis_fatalities_per_100k,
    sunni_prop,
    shia_prop,
    kurdish_prop,
    ethnic_fractionalization
  )

print(top_districts_by_rate, n = 25)

readr::write_csv(
  top_districts_by_rate,
  file.path(table_dir, "top_districts_by_isis_attack_rate_landscan.csv"),
  na = ""
)


# ========================================================================
# 11) MAP: POPULATION-ADJUSTED ISIS ATTACK RATE
# ========================================================================

plot_isis_attack_rate <- district_isis_rate_sf |>
  ggplot() +
  geom_sf(
    aes(
      fill = isis_attacks_per_100k
    ),
    color = "grey35",
    linewidth = 0.10
  ) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "sqrt",
    labels = number_format(
      accuracy = 0.1
    ),
    na.value = "grey85",
    name = "ISIS attacks\nper 100,000 people"
  ) +
  labs(
    title = "Population-Adjusted ISIS Attack Rate by District",
    subtitle = "GTD ISIS attacks joined to Iraq districts; denominator is LandScan 2008 population",
    caption = "Rate = ISIS attack count / LandScan district population × 100,000. Color scale uses square-root transformation to reduce high-outlier dominance."
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

print(plot_isis_attack_rate)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_attack_rate_per_100k_by_district.png"
  ),
  plot = plot_isis_attack_rate,
  width = 11,
  height = 8,
  dpi = 300
)


# ========================================================================
# 12) MAP: RAW ISIS ATTACK COUNTS
# ========================================================================

plot_isis_raw_counts <- district_isis_rate_sf |>
  ggplot() +
  geom_sf(
    aes(
      fill = isis_attack_count
    ),
    color = "grey35",
    linewidth = 0.10
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "sqrt",
    labels = number_format(
      accuracy = 1
    ),
    name = "Raw ISIS\nattack count"
  ) +
  labs(
    title = "Raw ISIS Attack Counts by District",
    subtitle = "Comparison map before population adjustment",
    caption = "Raw counts show event concentration. Population-adjusted rates show attack exposure relative to district population size."
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

print(plot_isis_raw_counts)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_raw_attack_count_by_district.png"
  ),
  plot = plot_isis_raw_counts,
  width = 11,
  height = 8,
  dpi = 300
)


# ========================================================================
# 13) OPTIONAL MAP: KNOWN ISIS FATALITIES PER 100,000 PEOPLE
# ========================================================================

plot_isis_fatality_rate <- district_isis_rate_sf |>
  ggplot() +
  geom_sf(
    aes(
      fill = isis_fatalities_per_100k
    ),
    color = "grey35",
    linewidth = 0.10
  ) +
  scale_fill_viridis_c(
    option = "inferno",
    trans = "sqrt",
    labels = number_format(
      accuracy = 0.1
    ),
    na.value = "grey85",
    name = "Known ISIS fatalities\nper 100,000 people"
  ) +
  labs(
    title = "Population-Adjusted Known ISIS Fatality Rate by District",
    subtitle = "Fatality denominator is LandScan 2008 district population",
    caption = "Fatality rate = known ISIS-attributed fatalities / LandScan district population × 100,000. Missing or unknown fatalities are treated as zero only after GTD parsing."
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

print(plot_isis_fatality_rate)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_fatality_rate_per_100k_by_district.png"
  ),
  plot = plot_isis_fatality_rate,
  width = 11,
  height = 8,
  dpi = 300
)


# ========================================================================
# 14) OBSERVED ATTACK RATE BY DOMINANT DISTRICT GROUP
# ========================================================================

observed_attack_rate_by_group <- district_isis_rate_table |>
  filter(
    !is.na(dominant_group),
    !is.na(isis_attacks_per_100k),
    landscan_population_total > 0
  ) |>
  group_by(
    dominant_group
  ) |>
  summarize(
    districts = n(),
    total_population = sum(landscan_population_total, na.rm = TRUE),
    total_isis_attacks = sum(isis_attack_count, na.rm = TRUE),
    mean_district_attack_rate = mean(isis_attacks_per_100k, na.rm = TRUE),
    median_district_attack_rate = median(isis_attacks_per_100k, na.rm = TRUE),
    population_weighted_attack_rate = 100000 *
      total_isis_attacks /
      total_population,
    .groups = "drop"
  ) |>
  arrange(
    desc(population_weighted_attack_rate)
  )

print(observed_attack_rate_by_group)

readr::write_csv(
  observed_attack_rate_by_group,
  file.path(table_dir, "observed_isis_attack_rate_by_dominant_group.csv"),
  na = ""
)

plot_attack_rate_by_group <- observed_attack_rate_by_group |>
  ggplot(
    aes(
      x = reorder(
        dominant_group,
        population_weighted_attack_rate
      ),
      y = population_weighted_attack_rate
    )
  ) +
  geom_col(
    width = 0.72
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = number_format(
      accuracy = 0.1
    )
  ) +
  labs(
    title = "Population-Weighted ISIS Attack Rate by Dominant District Group",
    subtitle = "Group-level rate uses total ISIS attacks divided by total LandScan population within each dominant-group category",
    x = "Dominant district group",
    y = "ISIS attacks per 100,000 people"
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

print(plot_attack_rate_by_group)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_attack_rate_by_dominant_group.png"
  ),
  plot = plot_attack_rate_by_group,
  width = 10,
  height = 6,
  dpi = 300
)


# ========================================================================
# 15) FINAL DIAGNOSTIC SUMMARY
# ========================================================================

final_diagnostic_summary <- tibble(
  diagnostic = c(
    "Raw Iraq GTD events",
    "ISIS / Islamic State Iraq events",
    "ISIS events with valid coordinates",
    "ISIS events matched to districts",
    "Districts in boundary file",
    "Districts with valid LandScan population",
    "Districts with at least one ISIS attack",
    "Total matched ISIS attacks",
    "Maximum district attack rate per 100k"
  ),
  value = c(
    nrow(gtd_iraq),
    nrow(gtd_isis_iraq),
    nrow(gtd_isis_coordinates),
    spatial_join_check$isis_attacks_matched_to_district,
    nrow(districts_clean_table),
    sum(
      district_isis_rate_table$landscan_population_total > 0,
      na.rm = TRUE
    ),
    sum(
      district_isis_rate_table$isis_attack_count > 0,
      na.rm = TRUE
    ),
    sum(
      district_isis_rate_table$isis_attack_count,
      na.rm = TRUE
    ),
    max(
      district_isis_rate_table$isis_attacks_per_100k,
      na.rm = TRUE
    )
  )
)

print(final_diagnostic_summary)

readr::write_csv(
  final_diagnostic_summary,
  file.path(table_dir, "population_adjusted_isis_attack_rate_diagnostics.csv"),
  na = ""
)
