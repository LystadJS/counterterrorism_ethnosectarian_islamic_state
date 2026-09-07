# ========================================================================
# DEMOGRAPHY-BY-TARGET-TYPE HEATMAP FOR ISIS ATTACKS
# ESOC + LANDSCAN DEMOGRAPHIC CONTEXT
# FROM SCRATCH
#
# PURPOSE:
#   Build a district-level and attack-level dataset for Iraq that shows
#   whether ISIS target selection differs across demographic contexts.
#
# CORE CONTRAST:
#   Target-category composition of ISIS attacks across:
#     1. Sunni-dominant districts
#     2. Shia-dominant districts
#     3. Kurdish-dominant districts
#     4. Mixed districts
#     5. Minority-heavy districts
#
# UNIT OF RAW DATA:
#   One GTD attack event
#
# UNIT OF ANALYTIC DATA:
#   Demographic-context-by-target-category cell
#
# KEY PREDICTORS / CONTEXT VARIABLES:
#   - LandScan-weighted Sunni population share
#   - LandScan-weighted Shia population share
#   - LandScan-weighted Kurdish population share
#   - LandScan-weighted Christian population share
#   - LandScan-weighted Turcoman population share
#   - Ethnic / sectarian fractionalization
#   - Demographic context
#
# KEY OUTCOMES:
#   - ISIS attack count by demographic context and target category
#   - Within-context percentage of ISIS attacks by target category
#
# KEY OUTPUTS:
#   outputs/plots/isis_demography_target_type_heatmap.png
#   outputs/tables/isis_demography_target_type_summary.csv
#   outputs/tables/isis_target_type_filter_check.csv
#   outputs/tables/isis_group_name_filter_check.csv
#   data/clean/gtd_isis_iraq_attacks_with_districts_and_targets.csv
#   data/clean/iraq_district_ethnicity_landscan_weighted.csv
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
# 02) USER-EDITABLE CLASSIFICATION SETTINGS
# ========================================================================

dominant_group_cutoff <- 0.50

minority_heavy_cutoff <- 0.25

mixed_shia_sunni_split <- 0.50

heatmap_label_type <- "both"


# ========================================================================
# 03) UNZIP ETHNICITY DATA AND LOCATE SHAPEFILES
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
    target_type_raw = str_squish(as.character(targtype1_txt)),
    target_subtype_raw = str_squish(as.character(targsubtype1_txt)),
    attack_type_raw = str_squish(as.character(attacktype1_txt)),
    weapon_type_raw = str_squish(as.character(weaptype1_txt))
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

readr::write_csv(
  isis_filter_check,
  file.path(table_dir, "isis_group_name_filter_check.csv"),
  na = ""
)

print(isis_filter_check, n = 50)


# ========================================================================
# 06) IMPORT DISTRICT AND ETHNICITY SHAPEFILES
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
# 07) BUILD LANDSCAN-WEIGHTED DISTRICT DEMOGRAPHICS
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
  ungroup() |>
  mutate(
    demographic_context = factor(
      demographic_context,
      levels = c(
        "Sunni-dominant",
        "Shia-dominant",
        "Kurdish-dominant",
        "Mixed",
        "Minority-heavy"
      )
    )
  )

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

demographic_context_check <- ethnicity_shares |>
  count(
    demographic_context,
    sort = TRUE
  )

print(ethnicity_prop_check)

print(demographic_context_check)

readr::write_csv(
  ethnicity_shares,
  file.path(clean_dir, "iraq_district_ethnicity_landscan_weighted.csv"),
  na = ""
)

readr::write_csv(
  demographic_context_check,
  file.path(table_dir, "demographic_context_district_count_check.csv"),
  na = ""
)


# ========================================================================
# 08) SPATIALLY JOIN ISIS EVENTS TO DISTRICTS
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
        minority_prop,
        other_prop,
        dominant_group,
        dominant_group_prop,
        ethnic_fractionalization,
        demographic_context
      ),
    by = c(
      "district_id",
      "district_label"
    )
  ) |>
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
    minority_prop,
    other_prop,
    dominant_group,
    dominant_group_prop,
    ethnic_fractionalization,
    demographic_context,
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


# ========================================================================
# 09) COLLAPSE GTD TARGET TYPES INTO ANALYTIC TARGET CATEGORIES
# ========================================================================

target_category_levels <- c(
  "civilian/infrastructure",
  "police/security",
  "military",
  "government",
  "religious",
  "business/economic",
  "transportation/utilities",
  "other"
)

gtd_isis_district_targets <- gtd_isis_district |>
  mutate(
    target_text_combined = str_c(
      target_type_raw,
      target_subtype_raw,
      sep = " "
    ),
    target_text_combined = str_to_lower(
      str_squish(target_text_combined)
    ),
    target_category = case_when(
      str_detect(
        target_text_combined,
        "police|security|intelligence"
      ) ~ "police/security",
      str_detect(
        target_text_combined,
        "military|army|soldier|troop|barracks|checkpoint|patrol"
      ) ~ "military",
      str_detect(
        target_text_combined,
        "government|political|politician|official|election|diplomatic|embassy"
      ) ~ "government",
      str_detect(
        target_text_combined,
        "religious|mosque|church|shrine|cleric|imam|priest|pilgrim|place of worship"
      ) ~ "religious",
      str_detect(
        target_text_combined,
        "business|market|bank|hotel|restaurant|shop|commercial|oil|gas|industrial"
      ) ~ "business/economic",
      str_detect(
        target_text_combined,
        "transportation|airport|aircraft|bus|rail|train|road|bridge|pipeline|utility|utilities|electric|telecom|water"
      ) ~ "transportation/utilities",
      str_detect(
        target_text_combined,
        "private citizens|property|civilian|educational|school|university|media|journalist|ngo|food|infrastructure"
      ) ~ "civilian/infrastructure",
      TRUE ~ "other"
    ),
    target_category = factor(
      target_category,
      levels = target_category_levels
    )
  )

target_type_filter_check <- gtd_isis_district_targets |>
  count(
    target_type_raw,
    target_subtype_raw,
    target_category,
    sort = TRUE
  )

target_category_check <- gtd_isis_district_targets |>
  count(
    target_category,
    sort = TRUE
  )

print(target_category_check, n = 50)

readr::write_csv(
  target_type_filter_check,
  file.path(table_dir, "isis_target_type_filter_check.csv"),
  na = ""
)

readr::write_csv(
  target_category_check,
  file.path(table_dir, "isis_target_category_count_check.csv"),
  na = ""
)

readr::write_csv(
  gtd_isis_district_targets,
  file.path(clean_dir, "gtd_isis_iraq_attacks_with_districts_and_targets.csv"),
  na = ""
)


# ========================================================================
# 10) COUNT ISIS ATTACKS BY DEMOGRAPHIC CONTEXT AND TARGET CATEGORY
# ========================================================================

isis_demography_target_summary <- gtd_isis_district_targets |>
  filter(
    !is.na(district_id),
    !is.na(demographic_context),
    !is.na(target_category)
  ) |>
  mutate(
    demographic_context = factor(
      demographic_context,
      levels = c(
        "Sunni-dominant",
        "Shia-dominant",
        "Kurdish-dominant",
        "Mixed",
        "Minority-heavy"
      )
    ),
    target_category = factor(
      target_category,
      levels = target_category_levels
    )
  ) |>
  count(
    demographic_context,
    target_category,
    name = "isis_attack_count"
  ) |>
  complete(
    demographic_context = factor(
      c(
        "Sunni-dominant",
        "Shia-dominant",
        "Kurdish-dominant",
        "Mixed",
        "Minority-heavy"
      ),
      levels = c(
        "Sunni-dominant",
        "Shia-dominant",
        "Kurdish-dominant",
        "Mixed",
        "Minority-heavy"
      )
    ),
    target_category = factor(
      target_category_levels,
      levels = target_category_levels
    ),
    fill = list(
      isis_attack_count = 0
    )
  ) |>
  group_by(
    demographic_context
  ) |>
  mutate(
    context_attack_total = sum(
      isis_attack_count,
      na.rm = TRUE
    ),
    within_context_percent = if_else(
      context_attack_total > 0,
      isis_attack_count / context_attack_total,
      NA_real_
    ),
    percent_label = scales::percent(
      within_context_percent,
      accuracy = 1
    ),
    count_label = str_c(
      "n = ",
      isis_attack_count
    ),
    heatmap_label = case_when(
      heatmap_label_type == "percent" ~ percent_label,
      heatmap_label_type == "count" ~ count_label,
      TRUE ~ str_c(
        percent_label,
        "\n",
        count_label
      )
    )
  ) |>
  ungroup()

print(isis_demography_target_summary, n = 50)

readr::write_csv(
  isis_demography_target_summary,
  file.path(table_dir, "isis_demography_target_type_summary.csv"),
  na = ""
)


# ========================================================================
# 11) BUILD WIDE TABLE FOR EASY INSPECTION
# ========================================================================

isis_demography_target_wide <- isis_demography_target_summary |>
  mutate(
    within_context_percent = round(
      within_context_percent,
      4
    )
  ) |>
  dplyr::select(
    demographic_context,
    target_category,
    isis_attack_count,
    within_context_percent
  ) |>
  pivot_wider(
    names_from = target_category,
    values_from = c(
      isis_attack_count,
      within_context_percent
    )
  )

print(isis_demography_target_wide)

readr::write_csv(
  isis_demography_target_wide,
  file.path(table_dir, "isis_demography_target_type_summary_wide.csv"),
  na = ""
)


# ========================================================================
# 12) OPTIONAL DIAGNOSTIC: TARGET MIX BY DOMINANT GROUP
# ========================================================================

isis_dominant_group_target_summary <- gtd_isis_district_targets |>
  filter(
    !is.na(district_id),
    !is.na(dominant_group),
    !is.na(target_category)
  ) |>
  mutate(
    target_category = factor(
      target_category,
      levels = target_category_levels
    )
  ) |>
  count(
    dominant_group,
    target_category,
    name = "isis_attack_count"
  ) |>
  group_by(
    dominant_group
  ) |>
  mutate(
    dominant_group_attack_total = sum(
      isis_attack_count,
      na.rm = TRUE
    ),
    within_dominant_group_percent = if_else(
      dominant_group_attack_total > 0,
      isis_attack_count / dominant_group_attack_total,
      NA_real_
    )
  ) |>
  ungroup() |>
  arrange(
    dominant_group,
    desc(within_dominant_group_percent)
  )

print(isis_dominant_group_target_summary, n = 100)

readr::write_csv(
  isis_dominant_group_target_summary,
  file.path(table_dir, "isis_target_type_by_dominant_group.csv"),
  na = ""
)


# ========================================================================
# 13) HEATMAP: DEMOGRAPHIC CONTEXT BY TARGET CATEGORY
# ========================================================================

plot_demography_target_heatmap <- isis_demography_target_summary |>
  ggplot(
    aes(
      x = target_category,
      y = demographic_context,
      fill = within_context_percent
    )
  ) +
  geom_tile(
    color = "grey95",
    linewidth = 0.75
  ) +
  geom_text(
    aes(
      label = heatmap_label
    ),
    size = 3.3,
    lineheight = 0.90
  ) +
  scale_fill_viridis_c(
    option = "magma",
    labels = percent_format(
      accuracy = 1
    ),
    na.value = "grey85",
    name = "Percent of ISIS attacks\nwithin context"
  ) +
  scale_x_discrete(
    position = "top"
  ) +
  labs(
    title = "ISIS Target Selection by District Demographic Context",
    subtitle = "GTD ISIS attacks joined to ESOC districts; demographic context is built from LandScan-weighted ESOC ethnic composition",
    caption = "Cell values show target-category share within each demographic context. Mixed Sunni-Shia ESOC fragments are split 50/50 between Sunni and Shia population.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 35,
      hjust = 0,
      vjust = 0,
      face = "bold"
    ),
    axis.text.y = element_text(
      face = "bold"
    ),
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

print(plot_demography_target_heatmap)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_demography_target_type_heatmap.png"
  ),
  plot = plot_demography_target_heatmap,
  width = 12,
  height = 7,
  dpi = 300
)


# ========================================================================
# 14) OPTIONAL HEATMAP: RAW COUNTS INSTEAD OF PERCENTAGES
# ========================================================================

plot_demography_target_count_heatmap <- isis_demography_target_summary |>
  ggplot(
    aes(
      x = target_category,
      y = demographic_context,
      fill = isis_attack_count
    )
  ) +
  geom_tile(
    color = "grey95",
    linewidth = 0.75
  ) +
  geom_text(
    aes(
      label = isis_attack_count
    ),
    size = 3.4
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "sqrt",
    labels = number_format(
      accuracy = 1
    ),
    name = "Raw ISIS\nattack count"
  ) +
  scale_x_discrete(
    position = "top"
  ) +
  labs(
    title = "Raw ISIS Attack Counts by Demographic Context and Target Category",
    subtitle = "Raw counts should be interpreted alongside the percentage heatmap because contexts have different total attack volumes",
    caption = "Data: GTD ISIS attacks spatially joined to ESOC Iraq districts.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 35,
      hjust = 0,
      vjust = 0,
      face = "bold"
    ),
    axis.text.y = element_text(
      face = "bold"
    ),
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

print(plot_demography_target_count_heatmap)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_demography_target_type_raw_count_heatmap.png"
  ),
  plot = plot_demography_target_count_heatmap,
  width = 12,
  height = 7,
  dpi = 300
)


# ========================================================================
# 15) OPTIONAL BAR CHART: TARGET COMPOSITION BY DEMOGRAPHIC CONTEXT
# ========================================================================

plot_demography_target_bars <- isis_demography_target_summary |>
  filter(
    context_attack_total > 0
  ) |>
  ggplot(
    aes(
      x = within_context_percent,
      y = demographic_context,
      fill = target_category
    )
  ) +
  geom_col(
    width = 0.75,
    color = "white",
    linewidth = 0.15
  ) +
  scale_x_continuous(
    labels = percent_format(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  scale_fill_viridis_d(
    option = "turbo",
    name = "Target category"
  ) +
  labs(
    title = "ISIS Target-Type Composition by District Demographic Context",
    subtitle = "Each bar sums to 100% within demographic context",
    x = "Percent of ISIS attacks within demographic context",
    y = NULL
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

print(plot_demography_target_bars)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_demography_target_type_stacked_bar.png"
  ),
  plot = plot_demography_target_bars,
  width = 12,
  height = 7,
  dpi = 300
)


# ========================================================================
# 16) STATISTICAL DIAGNOSTIC: CHI-SQUARE TEST OF ASSOCIATION
# ========================================================================

target_context_contingency <- gtd_isis_district_targets |>
  filter(
    !is.na(district_id),
    !is.na(demographic_context),
    !is.na(target_category)
  ) |>
  count(
    demographic_context,
    target_category
  ) |>
  pivot_wider(
    names_from = target_category,
    values_from = n,
    values_fill = 0
  ) |>
  column_to_rownames(
    var = "demographic_context"
  ) |>
  as.matrix()

chi_square_target_context <- chisq.test(
  target_context_contingency
)

chi_square_summary <- tibble(
  statistic = unname(chi_square_target_context$statistic),
  degrees_freedom = unname(chi_square_target_context$parameter),
  p_value = unname(chi_square_target_context$p.value),
  method = chi_square_target_context$method
)

print(chi_square_summary)

readr::write_csv(
  chi_square_summary,
  file.path(table_dir, "chi_square_target_context_summary.csv"),
  na = ""
)

chi_square_expected_counts <- chi_square_target_context$expected |>
  as_tibble(
    rownames = "demographic_context"
  )

readr::write_csv(
  chi_square_expected_counts,
  file.path(table_dir, "chi_square_target_context_expected_counts.csv"),
  na = ""
)


# ========================================================================
# 17) STANDARDIZED RESIDUALS FROM CHI-SQUARE TEST
# ========================================================================

chi_square_residuals <- chi_square_target_context$stdres |>
  as_tibble(
    rownames = "demographic_context"
  ) |>
  pivot_longer(
    cols = -demographic_context,
    names_to = "target_category",
    values_to = "standardized_residual"
  ) |>
  mutate(
    target_category = factor(
      target_category,
      levels = target_category_levels
    ),
    demographic_context = factor(
      demographic_context,
      levels = c(
        "Sunni-dominant",
        "Shia-dominant",
        "Kurdish-dominant",
        "Mixed",
        "Minority-heavy"
      )
    )
  )

print(chi_square_residuals, n = 50)

readr::write_csv(
  chi_square_residuals,
  file.path(table_dir, "chi_square_target_context_standardized_residuals.csv"),
  na = ""
)

plot_chi_square_residual_heatmap <- chi_square_residuals |>
  ggplot(
    aes(
      x = target_category,
      y = demographic_context,
      fill = standardized_residual
    )
  ) +
  geom_tile(
    color = "grey95",
    linewidth = 0.75
  ) +
  geom_text(
    aes(
      label = number(
        standardized_residual,
        accuracy = 0.1
      )
    ),
    size = 3.3
  ) +
  scale_fill_gradient2(
    low = "#2E2D5B",
    mid = "grey95",
    high = "#842829",
    midpoint = 0,
    name = "Standardized\nresidual"
  ) +
  scale_x_discrete(
    position = "top"
  ) +
  labs(
    title = "Where ISIS Target Selection Deviates from Independence",
    subtitle = "Positive residuals mean more attacks than expected under demographic-context / target-category independence",
    caption = "Standardized residuals come from a chi-square test. Interpret cautiously when expected cell counts are small.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 35,
      hjust = 0,
      vjust = 0,
      face = "bold"
    ),
    axis.text.y = element_text(
      face = "bold"
    ),
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

print(plot_chi_square_residual_heatmap)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_target_context_chi_square_residual_heatmap.png"
  ),
  plot = plot_chi_square_residual_heatmap,
  width = 12,
  height = 7,
  dpi = 300
)


# ========================================================================
# 18) FINAL DIAGNOSTIC SUMMARY
# ========================================================================

final_diagnostic_summary <- tibble(
  diagnostic = c(
    "Raw Iraq GTD events",
    "ISIS / Islamic State Iraq events",
    "ISIS events with valid coordinates",
    "ISIS events matched to districts",
    "ISIS events used in target heatmap",
    "Districts in boundary file",
    "Districts with valid LandScan population",
    "Districts classified as Sunni-dominant",
    "Districts classified as Shia-dominant",
    "Districts classified as Kurdish-dominant",
    "Districts classified as Mixed",
    "Districts classified as Minority-heavy",
    "Chi-square statistic",
    "Chi-square degrees of freedom",
    "Chi-square p-value"
  ),
  value = c(
    nrow(gtd_iraq),
    nrow(gtd_isis_iraq),
    nrow(gtd_isis_coordinates),
    spatial_join_check$isis_attacks_matched_to_district,
    sum(isis_demography_target_summary$isis_attack_count, na.rm = TRUE),
    nrow(districts_clean_table),
    sum(
      ethnicity_shares$landscan_population_total > 0,
      na.rm = TRUE
    ),
    sum(
      ethnicity_shares$demographic_context == "Sunni-dominant",
      na.rm = TRUE
    ),
    sum(
      ethnicity_shares$demographic_context == "Shia-dominant",
      na.rm = TRUE
    ),
    sum(
      ethnicity_shares$demographic_context == "Kurdish-dominant",
      na.rm = TRUE
    ),
    sum(
      ethnicity_shares$demographic_context == "Mixed",
      na.rm = TRUE
    ),
    sum(
      ethnicity_shares$demographic_context == "Minority-heavy",
      na.rm = TRUE
    ),
    unname(chi_square_target_context$statistic),
    unname(chi_square_target_context$parameter),
    unname(chi_square_target_context$p.value)
  )
)

print(final_diagnostic_summary)

readr::write_csv(
  final_diagnostic_summary,
  file.path(table_dir, "isis_demography_target_type_diagnostics.csv"),
  na = ""
)
