source("scripts/00) Project Setup.R")
source("scripts/25) Data Initialization Pipeline.R")

GTD_path <-
  "data/raw/csv/GTD_A.csv"

ethnicity_zip_path <-
  "data/raw/Iraq Ethnicity.zip"

ethnicity_unzip_dir <-
  "data/raw/iraq_ethnicity_unzipped"

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

file.exists(GTD_path)

file.exists(district_path)

file.exists(ethnicity_intersect_path)

# DEFINE A NARROW GTD-SPECIFIC MISSING TOKEN LIST
GTD_missing_tokens <- c(
  "",
  " ",
  "NA",
  "N/A",
  "na",
  "n/a",
  "NULL",
  "null"
)

# IMPORT GTD WITH ALL COLUMNS AS CHARACTER
GTD_raw <- read_csv_all_character(
  file_path = GTD_path,
  clean_names = TRUE
)

# CLEAN BASIC MISSING VALUES WITHOUT DESTROYING "UNKNOWN" CATEGORIES
gtd_clean_strings <- GTD_raw |>
  clean_common_missing_values(
    missing_tokens = GTD_missing_tokens
  ) |>
  clean_character_strings()

# DEFINE GTD TYPE CONVERSION PLAN
gtd_type_plan <- tibble(
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

# CONVERT SELECTED GTD COLUMNS USING YOUR HELPER
gtd_typed <- gtd_clean_strings |>
  convert_columns_by_plan(
    type_plan = gtd_type_plan
  )

# FILTER TO IRAQ EVENTS
gtd_iraq <- gtd_typed |>
  filter(
    country_txt == "Iraq"
  )

# CLEAN DATE FIELDS AND CREATE MONTH VARIABLE
gtd_iraq <- gtd_iraq |>
  mutate(
    event_year = iyear,
    event_month_number = if_else(
      imonth >= 1 & imonth <= 12,
      imonth,
      NA_integer_
    ),
    event_day_clean = if_else(
      is.na(iday) | iday < 1 | iday > 31,
      1L,
      iday
    ),
    event_date = lubridate::make_date(
      year = event_year,
      month = event_month_number,
      day = event_day_clean
    ),
    event_month = lubridate::floor_date(
      event_date,
      unit = "month"
    )
  )

# REMOVE IMPOSSIBLE CASUALTY VALUES IF THEY APPEAR
gtd_iraq <- gtd_iraq |>
  mutate(
    nkill = if_else(
      nkill < 0,
      NA_real_,
      nkill
    ),
    nwound = if_else(
      nwound < 0,
      NA_real_,
      nwound
    )
  )

# CHECK IRAQ GTD STRUCTURE
gtd_iraq_qc <- gtd_iraq |>
  summarize(
    rows = n(),
    first_year = min(event_year, na.rm = TRUE),
    last_year = max(event_year, na.rm = TRUE),
    missing_month = sum(is.na(event_month)),
    missing_latitude = sum(is.na(latitude)),
    missing_longitude = sum(is.na(longitude)),
    missing_coordinates = sum(is.na(latitude) | is.na(longitude))
  )

# PRINT GTD QUALITY CHECK
gtd_iraq_qc

# CREATE GTD SCHEMA TABLE
gtd_schema <- schema_inventory(gtd_iraq)

# CREATE GTD MISSINGNESS TABLE
gtd_missingness <- missingness_by_variable(gtd_iraq)

# CREATE GTD COMPLETE-CASE SUMMARY
gtd_complete_case_summary <- complete_case_summary(gtd_iraq)

# AUDIT GTD EVENT ID UNIQUENESS
gtd_eventid_audit <- audit_key_column(
  data = gtd_iraq,
  key_column = eventid
)

# EXPORT GTD SCHEMA
export_data_dictionary(
  schema_table = gtd_schema,
  base_name = "gtd_iraq_schema"
)

# EXPORT CLEANED IRAQ GTD
export_cleaned_data(
  data = gtd_iraq,
  base_name = "gtd_iraq_cleaned"
)


# IMPORT IRAQI DISTRICT BOUNDARIES
districts_raw_sf <- read_shapefile_clean(
  file_path = district_path,
  target_crs = NULL,
  clean_names = TRUE
)

# IMPORT ETHNICITY-DISTRICT INTERSECTION SHAPEFILE
ethnicity_raw_sf <- read_shapefile_clean(
  file_path = ethnicity_intersect_path,
  target_crs = NULL,
  clean_names = TRUE
)

# MAKE DISTRICT GEOMETRIES VALID
districts_raw_sf <- districts_raw_sf |>
  sf::st_make_valid()

# MAKE ETHNICITY INTERSECTION GEOMETRIES VALID
ethnicity_raw_sf <- ethnicity_raw_sf |>
  sf::st_make_valid()

# DIAGNOSE DISTRICT SHAPEFILE GEOMETRY
districts_sf_diagnostic <- sf_diagnostic_summary(
  sf_data = districts_raw_sf
)

# DIAGNOSE ETHNICITY SHAPEFILE GEOMETRY
ethnicity_sf_diagnostic <- sf_diagnostic_summary(
  sf_data = ethnicity_raw_sf
)

# DIAGNOSE DISTRICT ATTRIBUTE MISSINGNESS
districts_attribute_missingness <- sf_attribute_missingness(
  sf_data = districts_raw_sf
)

# DIAGNOSE ETHNICITY ATTRIBUTE MISSINGNESS
ethnicity_attribute_missingness <- sf_attribute_missingness(
  sf_data = ethnicity_raw_sf
)

# PRINT DISTRICT SHAPEFILE DIAGNOSTIC
districts_sf_diagnostic

# PRINT ETHNICITY SHAPEFILE DIAGNOSTIC
ethnicity_sf_diagnostic

# CLEAN DISTRICT BOUNDARY ATTRIBUTES
districts_clean_sf <- districts_raw_sf |>
  transmute(
    district_id = adm3code,
    district_name = adm3name,
    governorate = adm2name,
    governorate_id = adm2code,
    district_area_km2 = as.numeric(area_km2),
    district_area_km2_geometry = as.numeric(sf::st_area(geometry)) / 1000000,
    population_total = as.numeric(total_pop),
    district_label = str_c(adm3name, " [", adm2name, "]"),
    geometry = geometry
  )

# CREATE NON-SPATIAL DISTRICT TABLE
districts_clean_table <- districts_clean_sf |>
  sf::st_drop_geometry()

# AUDIT DISTRICT ID UNIQUENESS
district_id_audit <- audit_key_column(
  data = districts_clean_table,
  key_column = district_id
)

# PRINT DISTRICT ID AUDIT
district_id_audit

# EXPORT DISTRICT ATTRIBUTE TABLE
export_cleaned_data(
  data = districts_clean_table,
  base_name = "iraq_districts_cleaned_attributes"
)

# EXPORT DISTRICT DATA DICTIONARY
export_data_dictionary(
  schema_table = schema_inventory(districts_clean_table),
  base_name = "iraq_districts_schema"
)


# BUILD AREA-BASED ETHNIC COMPONENTS FROM INTERSECTION GEOMETRY
ethnicity_components <- ethnicity_raw_sf |>
  mutate(
    district_id = adm3code,
    district_name = adm3name,
    governorate = adm2name,
    intersect_area_km2 = as.numeric(sf::st_area(geometry)) / 1000000,
    kurdish_area = if_else(
      ethnicity == 1,
      intersect_area_km2,
      0
    ),
    shia_area = if_else(
      shia == 1,
      intersect_area_km2,
      0
    ),
    sunni_area = if_else(
      sunni == 1,
      intersect_area_km2,
      0
    ),
    christian_area = if_else(
      christians == 1,
      intersect_area_km2,
      0
    ),
    turcoman_area = if_else(
      turcomans == 1,
      intersect_area_km2,
      0
    ),
    mixed_shia_sunni_area = if_else(
      mixed_sh_su == 1,
      intersect_area_km2,
      0
    )
  ) |>
  sf::st_drop_geometry()

# SPLIT MIXED SHIA-SUNNI AREAS EVENLY BETWEEN SHIA AND SUNNI
ethnicity_components <- ethnicity_components |>
  mutate(
    shia_area_adjusted = shia_area + 0.50 * mixed_shia_sunni_area,
    sunni_area_adjusted = sunni_area + 0.50 * mixed_shia_sunni_area
  )

# AGGREGATE ETHNIC AREA COMPONENTS TO DISTRICT LEVEL
ethnicity_shares <- ethnicity_components |>
  group_by(
    district_id,
    district_name,
    governorate
  ) |>
  summarize(
    intersect_area_total_km2 = sum(intersect_area_km2, na.rm = TRUE),
    kurdish_area = sum(kurdish_area, na.rm = TRUE),
    shia_area = sum(shia_area_adjusted, na.rm = TRUE),
    sunni_area = sum(sunni_area_adjusted, na.rm = TRUE),
    christian_area = sum(christian_area, na.rm = TRUE),
    turcoman_area = sum(turcoman_area, na.rm = TRUE),
    mixed_shia_sunni_area = sum(mixed_shia_sunni_area, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    kurdish_prop = kurdish_area / intersect_area_total_km2,
    shia_prop = shia_area / intersect_area_total_km2,
    sunni_prop = sunni_area / intersect_area_total_km2,
    christian_prop = christian_area / intersect_area_total_km2,
    turcoman_prop = turcoman_area / intersect_area_total_km2,
    mixed_shia_sunni_prop = mixed_shia_sunni_area / intersect_area_total_km2
  )

# CREATE RESIDUAL OTHER PROPORTION
ethnicity_shares <- ethnicity_shares |>
  mutate(
    other_prop = pmax(
      0,
      1 -
        (kurdish_prop +
          shia_prop +
          sunni_prop +
          christian_prop +
          turcoman_prop)
    )
  )

# CREATE DOMINANT GROUP AND FRACTIONALIZATION INDEX
ethnicity_shares <- ethnicity_shares |>
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

# CHECK THAT PROPORTIONS SUM TO APPROXIMATELY ONE
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
    mean_prop_sum = mean(prop_sum, na.rm = TRUE)
  )

# PRINT ETHNIC PROPORTION CHECK
ethnicity_prop_check

# EXPORT ETHNICITY TABLE
export_cleaned_data(
  data = ethnicity_shares,
  base_name = "iraq_district_ethnic_composition"
)

# EXPORT ETHNICITY DATA DICTIONARY
export_data_dictionary(
  schema_table = schema_inventory(ethnicity_shares),
  base_name = "iraq_district_ethnic_composition_schema"
)


# KEEP ONLY GTD ROWS WITH VALID COORDINATES
gtd_iraq_with_coordinates <- gtd_iraq |>
  filter(
    !is.na(latitude),
    !is.na(longitude)
  )

# CONVERT GTD POINTS TO AN SF OBJECT
gtd_iraq_points_sf <- gtd_iraq_with_coordinates |>
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# TRANSFORM GTD POINTS TO THE DISTRICT CRS
gtd_iraq_points_sf <- gtd_iraq_points_sf |>
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
    district_area_km2,
    district_area_km2_geometry,
    population_total,
    geometry
  )

# SPATIALLY JOIN GTD POINTS TO IRAQI DISTRICT POLYGONS
gtd_attacks_district_sf <- gtd_iraq_points_sf |>
  sf::st_join(
    districts_join_sf,
    join = sf::st_within,
    left = TRUE
  )

# DROP GEOMETRY AFTER THE SPATIAL JOIN
gtd_attacks_district <- gtd_attacks_district_sf |>
  sf::st_drop_geometry()

# CHECK SPATIAL JOIN SUCCESS
spatial_join_qc <- gtd_attacks_district |>
  summarize(
    attacks_with_coordinates = n(),
    attacks_matched_to_district = sum(!is.na(district_id)),
    attacks_unmatched_to_district = sum(is.na(district_id)),
    match_rate = attacks_matched_to_district / attacks_with_coordinates
  )

# PRINT SPATIAL JOIN QUALITY CHECK
spatial_join_qc

# EXPORT ATTACKS WITH DISTRICT ASSIGNMENT
export_cleaned_data(
  data = gtd_attacks_district,
  base_name = "gtd_iraq_attacks_with_districts"
)

# EXPORT ATTACK-DISTRICT DATA DICTIONARY
export_data_dictionary(
  schema_table = schema_inventory(gtd_attacks_district),
  base_name = "gtd_iraq_attacks_with_districts_schema"
)


# COUNT ATTACKS BY DISTRICT-MONTH
attacks_monthly <- gtd_attacks_district |>
  filter(
    !is.na(district_id),
    !is.na(event_month)
  ) |>
  count(
    district_id,
    district_name,
    governorate,
    district_label,
    event_month,
    name = "attack_count"
  )

# CREATE FULL MONTH SEQUENCE
month_sequence <- seq(
  from = min(attacks_monthly$event_month, na.rm = TRUE),
  to = max(attacks_monthly$event_month, na.rm = TRUE),
  by = "month"
)

# CREATE COMPLETE DISTRICT-MONTH PANEL
district_month_panel <- districts_clean_table |>
  crossing(
    event_month = month_sequence
  ) |>
  left_join(
    attacks_monthly,
    by = c(
      "district_id",
      "district_name",
      "governorate",
      "district_label",
      "event_month"
    )
  ) |>
  mutate(
    attack_count = replace_na(attack_count, 0L)
  )

# ADD ETHNIC COMPOSITION TO DISTRICT-MONTH PANEL
district_month_panel <- district_month_panel |>
  left_join(
    ethnicity_shares,
    by = c(
      "district_id",
      "district_name",
      "governorate"
    )
  )


# ADD RATE VARIABLES AND POPULATION QUALITY FLAGS
district_month_panel <- district_month_panel |>
  mutate(
    population_qc_flag = case_when(
      is.na(population_total) ~ "missing_population",
      population_total < 10000 ~ "implausibly_low_population",
      TRUE ~ "population_available"
    ),
    attack_rate_100k = if_else(
      population_qc_flag == "population_available" & population_total > 0,
      attack_count / population_total * 100000,
      NA_real_
    ),
    attack_rate_1000km2 = if_else(
      !is.na(district_area_km2_geometry) & district_area_km2_geometry > 0,
      attack_count / district_area_km2_geometry * 1000,
      NA_real_
    )
  )

# AUDIT DISTRICT-MONTH COMPOSITE KEY
district_month_key_audit <- audit_composite_key(
  data = district_month_panel,
  key_columns = c(
    "district_id",
    "event_month"
  )
)

# PRINT DISTRICT-MONTH KEY AUDIT
district_month_key_audit

# CHECK FINAL PANEL SUMMARY
district_month_summary <- district_month_panel |>
  summarize(
    rows = n(),
    districts = n_distinct(district_id),
    months = n_distinct(event_month),
    total_attacks = sum(attack_count, na.rm = TRUE),
    missing_ethnicity_rows = sum(is.na(dominant_group)),
    low_population_rows = sum(
      population_qc_flag == "implausibly_low_population"
    )
  )

# PRINT FINAL PANEL SUMMARY
district_month_summary


# EXPORT FINAL DISTRICT-MONTH PANEL
export_cleaned_data(
  data = district_month_panel,
  base_name = "district_month_isis_ethnicity_panel"
)

# EXPORT FINAL PANEL DATA DICTIONARY
export_data_dictionary(
  schema_table = schema_inventory(district_month_panel),
  base_name = "district_month_isis_ethnicity_panel_schema"
)

# EXPORT FINAL PANEL MISSINGNESS TABLE
readr::write_csv(
  missingness_by_variable(district_month_panel),
  "outputs/tables/district_month_panel_missingness.csv"
)

# EXPORT FINAL PANEL COMPLETE-CASE SUMMARY
readr::write_csv(
  complete_case_summary(district_month_panel),
  "outputs/tables/district_month_panel_complete_case_summary.csv"
)


# BUILD MONTHLY ATTACK FREQUENCY PLOT
plot_monthly_attacks <- district_month_panel |>
  group_by(
    event_month
  ) |>
  summarize(
    monthly_attacks = sum(attack_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(
    aes(
      x = event_month,
      y = monthly_attacks
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.2
  ) +
  labs(
    title = "Monthly ISIS Attack Frequency in Iraq",
    subtitle = "Aggregated from district-month panel",
    x = NULL,
    y = "Monthly attack count"
  ) +
  theme_classic()

# PRINT MONTHLY ATTACK PLOT
plot_monthly_attacks

# EXPORT MONTHLY ATTACK PLOT
export_plot(
  plot_object = plot_monthly_attacks,
  file_name = "monthly_isis_attack_frequency.png",
  width = 10,
  height = 6
)


# BUILD DISTRICT-LEVEL ETHNICITY LONG TABLE
ethnicity_long <- ethnicity_shares |>
  mutate(
    district_label = str_c(district_name, " [", governorate, "]")
  ) |>
  dplyr::select(
    district_label,
    kurdish_prop,
    shia_prop,
    sunni_prop,
    christian_prop,
    turcoman_prop,
    other_prop
  ) |>
  pivot_longer(
    cols = ends_with("_prop"),
    names_to = "group",
    values_to = "proportion"
  ) |>
  mutate(
    group = case_when(
      group == "kurdish_prop" ~ "Kurdish",
      group == "shia_prop" ~ "Shia",
      group == "sunni_prop" ~ "Sunni",
      group == "christian_prop" ~ "Christian",
      group == "turcoman_prop" ~ "Turcoman",
      group == "other_prop" ~ "Other",
      TRUE ~ group
    )
  )

# CREATE DISTRICT ORDER FOR ETHNICITY PLOT
ethnicity_district_order <- ethnicity_shares |>
  mutate(
    district_label = str_c(district_name, " [", governorate, "]")
  ) |>
  arrange(
    dominant_group,
    desc(dominant_group_prop)
  ) |>
  pull(district_label)

# APPLY DISTRICT ORDER TO ETHNICITY LONG TABLE
ethnicity_long <- ethnicity_long |>
  mutate(
    district_label = factor(
      district_label,
      levels = ethnicity_district_order
    )
  )

# DEFINE ETHNICITY COLOR PALETTE
ethnic_palette <- c(
  "Kurdish" = "#006A4E",
  "Shia" = "#842829",
  "Sunni" = "#005B8F",
  "Christian" = "#2E2D5B",
  "Turcoman" = "#8F567A",
  "Other" = "#525252"
)

# BUILD ETHNICITY COMPOSITION PLOT
plot_ethnicity_composition <- ggplot(
  ethnicity_long,
  aes(
    x = proportion,
    y = district_label,
    fill = group
  )
) +
  geom_col(
    width = 0.9
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_fill_manual(
    values = ethnic_palette
  ) +
  labs(
    title = "District-Level Ethnic/Sectarian Composition",
    subtitle = "Districts ordered by dominant group and group concentration",
    x = "Area-weighted district share",
    y = NULL,
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 5),
    legend.position = "bottom"
  )

# PRINT ETHNICITY COMPOSITION PLOT
plot_ethnicity_composition

# EXPORT ETHNICITY COMPOSITION PLOT
export_plot(
  plot_object = plot_ethnicity_composition,
  file_name = "district_ethnic_composition_setup_plot.png",
  width = 10,
  height = 12
)


# CHECK LOW-POPULATION DISTRICTS
district_month_panel |>
  distinct(
    district_id,
    district_name,
    governorate,
    population_total,
    population_qc_flag
  ) |>
  arrange(
    population_total
  ) |>
  print(n = 30)

# CHECK MOST ATTACKED DISTRICTS
district_month_panel |>
  group_by(
    district_name,
    governorate,
    dominant_group
  ) |>
  summarize(
    total_attacks = sum(attack_count, na.rm = TRUE),
    mean_monthly_attacks = mean(attack_count, na.rm = TRUE),
    max_monthly_attacks = max(attack_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    desc(total_attacks)
  ) |>
  print(n = 30)

# CHECK ETHNICALLY MIXED DISTRICTS
ethnicity_shares |>
  arrange(
    desc(ethnic_fractionalization)
  ) |>
  dplyr::select(
    district_name,
    governorate,
    kurdish_prop,
    shia_prop,
    sunni_prop,
    christian_prop,
    turcoman_prop,
    other_prop,
    dominant_group,
    ethnic_fractionalization
  ) |>
  print(n = 30)

# CHECK SPATIAL JOIN FAILURES
gtd_attacks_district |>
  filter(
    is.na(district_id)
  ) |>
  dplyr::select(
    eventid,
    event_date,
    provstate,
    city,
    latitude,
    longitude,
    gname,
    attacktype1_txt,
    targtype1_txt
  ) |>
  print(n = 30)


# ========================================================================
# FINAL HEATMAP: ISIS ATTACK FREQUENCY BY DISTRICT, MONTH, AND ETHNICITY
# ========================================================================

# 00) PACKAGES ------------------------------------------------------------

# LOAD CORE DATA WRANGLING TOOLS
library(tidyverse)

# LOAD DATE TOOLS
library(lubridate)

# LOAD PLOT AXIS LABEL TOOLS
library(scales)

# LOAD PLOT COMPOSITION TOOLS
library(patchwork)


# 01) CHECK REQUIRED OBJECTS ----------------------------------------------

# CHECK THAT THE DISTRICT-MONTH PANEL EXISTS
if (!exists("district_month_panel")) {
  stop(
    "district_month_panel does not exist. Re-run the district-month panel construction script."
  )
}

# CHECK THAT THE ETHNICITY TABLE EXISTS
if (!exists("ethnicity_shares")) {
  stop(
    "ethnicity_shares does not exist. Re-run the ethnicity aggregation script."
  )
}

# CHECK THAT THE EXPORT HELPER EXISTS
if (!exists("export_plot")) {
  stop(
    "export_plot() does not exist. Source your 24) Export Cleaned Data.R helper script."
  )
}


# 02) DEFINE PLOT SETTINGS -------------------------------------------------

# DEFINE THE ATTACK VARIABLE TO USE IN THE HEATMAP
heatmap_metric <- "attack_count"

# DEFINE THE HUMAN-READABLE HEATMAP LEGEND LABEL
heatmap_legend_label <- "Monthly\nattacks"

# DEFINE THE MAIN FIGURE TITLE
main_title <- "ISIS Attack Frequency Across Iraqi Districts by Ethnic/Sectarian Composition"

# DEFINE THE MAIN FIGURE SUBTITLE
main_subtitle <- "Districts are ordered by dominant ethnic/sectarian group, group concentration, and total ISIS attack frequency"

# DEFINE THE MAIN FIGURE CAPTION
main_caption <- "Note: Attack frequency is measured as monthly event counts from GTD-derived ISIS attack records. Ethnic/sectarian composition is area-weighted from district-intersected ethnicity polygons."

# DEFINE THE ETHNICITY PALETTE
ethnic_palette <- c(
  "Kurdish" = "#006A4E",
  "Shia" = "#842829",
  "Sunni" = "#005B8F",
  "Christian" = "#2E2D5B",
  "Turcoman" = "#8F567A",
  "Other" = "#525252"
)

# DEFINE THE LOW COLOR FOR ATTACK INTENSITY
attack_low_color <- "grey96"

# DEFINE THE HIGH COLOR FOR ATTACK INTENSITY
attack_high_color <- "#842829"


# 03) CHECK REQUIRED COLUMNS ----------------------------------------------

# DEFINE REQUIRED PANEL COLUMNS
required_panel_columns <- c(
  "district_id",
  "district_name",
  "governorate",
  "district_label",
  "event_month",
  "attack_count",
  "dominant_group",
  "dominant_group_prop"
)

# FIND MISSING PANEL COLUMNS
missing_panel_columns <- setdiff(
  required_panel_columns,
  names(district_month_panel)
)

# STOP IF REQUIRED PANEL COLUMNS ARE MISSING
if (length(missing_panel_columns) > 0) {
  stop(
    paste(
      "district_month_panel is missing these columns:",
      paste(missing_panel_columns, collapse = ", ")
    )
  )
}

# DEFINE REQUIRED ETHNICITY COLUMNS
required_ethnicity_columns <- c(
  "district_id",
  "district_name",
  "governorate",
  "kurdish_prop",
  "shia_prop",
  "sunni_prop",
  "christian_prop",
  "turcoman_prop",
  "other_prop"
)

# FIND MISSING ETHNICITY COLUMNS
missing_ethnicity_columns <- setdiff(
  required_ethnicity_columns,
  names(ethnicity_shares)
)

# STOP IF REQUIRED ETHNICITY COLUMNS ARE MISSING
if (length(missing_ethnicity_columns) > 0) {
  stop(
    paste(
      "ethnicity_shares is missing these columns:",
      paste(missing_ethnicity_columns, collapse = ", ")
    )
  )
}


# 04) CREATE DISTRICT ORDER -----------------------------------------------

# BUILD DISTRICT-LEVEL ORDERING TABLE
district_order_table <- district_month_panel |>
  group_by(
    district_id,
    district_label,
    dominant_group
  ) |>
  summarize(
    total_attacks = sum(attack_count, na.rm = TRUE),
    mean_monthly_attacks = mean(attack_count, na.rm = TRUE),
    max_monthly_attacks = max(attack_count, na.rm = TRUE),
    dominant_group_prop = first(dominant_group_prop),
    .groups = "drop"
  ) |>
  arrange(
    dominant_group,
    desc(dominant_group_prop),
    desc(total_attacks),
    district_label
  )

# CREATE DISTRICT ORDER VECTOR
district_order <- district_order_table |>
  pull(district_label)

# REVERSE DISTRICT ORDER SO THE FIRST SORTED DISTRICT APPEARS AT THE TOP
district_plot_levels <- rev(district_order)


# 05) PREPARE HEATMAP DATA -------------------------------------------------

# PREPARE DISTRICT-MONTH PANEL FOR PLOTTING
heatmap_data <- district_month_panel |>
  mutate(
    district_label = factor(
      district_label,
      levels = district_plot_levels
    ),
    event_month = as.Date(event_month)
  )

# CREATE A SIMPLE HEATMAP RANGE CHECK
heatmap_range_check <- heatmap_data |>
  summarize(
    rows = n(),
    districts = n_distinct(district_label),
    months = n_distinct(event_month),
    total_attacks = sum(attack_count, na.rm = TRUE),
    max_monthly_district_attacks = max(attack_count, na.rm = TRUE)
  )

# PRINT HEATMAP RANGE CHECK
heatmap_range_check


# 06) PREPARE ETHNICITY SIDE PANEL DATA -----------------------------------

# PREPARE ETHNICITY DATA IN LONG FORMAT
ethnicity_long <- ethnicity_shares |>
  mutate(
    district_label = str_c(
      district_name,
      " [",
      governorate,
      "]"
    )
  ) |>
  mutate(
    district_label = factor(
      district_label,
      levels = district_plot_levels
    )
  ) |>
  dplyr::select(
    district_label,
    kurdish_prop,
    shia_prop,
    sunni_prop,
    christian_prop,
    turcoman_prop,
    other_prop
  ) |>
  pivot_longer(
    cols = ends_with("_prop"),
    names_to = "group",
    values_to = "proportion"
  ) |>
  mutate(
    group = case_when(
      group == "kurdish_prop" ~ "Kurdish",
      group == "shia_prop" ~ "Shia",
      group == "sunni_prop" ~ "Sunni",
      group == "christian_prop" ~ "Christian",
      group == "turcoman_prop" ~ "Turcoman",
      group == "other_prop" ~ "Other",
      TRUE ~ group
    )
  ) |>
  mutate(
    group = factor(
      group,
      levels = names(ethnic_palette)
    )
  )


# 07) BUILD ETHNICITY SIDE PANEL ------------------------------------------

# BUILD THE LEFT ETHNIC-COMPOSITION PANEL
plot_ethnicity_side_panel <- ggplot(
  ethnicity_long,
  aes(
    x = proportion,
    y = district_label,
    fill = group
  )
) +
  geom_col(
    width = 0.9
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0, 0.5, 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_fill_manual(
    values = ethnic_palette,
    drop = FALSE
  ) +
  labs(
    title = "District Composition",
    x = "Area share",
    y = NULL,
    fill = "Group"
  ) +
  theme_classic(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10
    ),
    axis.text.y = element_text(
      size = 4.5
    ),
    axis.text.x = element_text(
      size = 7
    ),
    axis.title.x = element_text(
      size = 8
    ),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(
      size = 8
    ),
    legend.text = element_text(
      size = 7
    ),
    legend.key.size = unit(
      0.35,
      "cm"
    ),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )


# 08) BUILD ATTACK HEATMAP PANEL ------------------------------------------

# BUILD THE RIGHT ATTACK HEATMAP PANEL
plot_attack_heatmap <- ggplot(
  heatmap_data,
  aes(
    x = event_month,
    y = district_label,
    fill = .data[[heatmap_metric]]
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.03
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_gradient(
    low = attack_low_color,
    high = attack_high_color,
    trans = "sqrt",
    name = heatmap_legend_label
  ) +
  labs(
    title = "ISIS Attack Activity Over Time",
    subtitle = "Monthly district-level attack frequency",
    x = NULL,
    y = NULL
  ) +
  theme_classic(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10
    ),
    plot.subtitle = element_text(
      size = 8
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      size = 8
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 8
    ),
    legend.text = element_text(
      size = 7
    ),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )


# 09) COMBINE PANELS INTO FINAL FIGURE ------------------------------------

# COMBINE THE ETHNICITY PANEL AND THE ATTACK HEATMAP
final_isis_ethnicity_heatmap <- plot_ethnicity_side_panel +
  plot_attack_heatmap +
  plot_layout(
    widths = c(1.25, 3.75),
    guides = "collect"
  ) +
  plot_annotation(
    title = main_title,
    subtitle = main_subtitle,
    caption = main_caption
  ) &
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 10
    ),
    plot.caption = element_text(
      size = 8,
      hjust = 0
    ),
    legend.position = "bottom"
  )

# PRINT THE FINAL HEATMAP
final_isis_ethnicity_heatmap


# 10) EXPORT FINAL FIGURE --------------------------------------------------

# EXPORT THE FINAL HEATMAP USING YOUR HELPER FUNCTION
export_plot(
  plot_object = final_isis_ethnicity_heatmap,
  file_name = "final_isis_ethnicity_district_month_heatmap.png",
  width = 15,
  height = 11
)


# LOAD RASTER DATA TOOLS
library(terra)

# LOAD LEGACY RASTER CLASS SUPPORT FOR EXACTEXTRACTR
library(raster)

# LOAD EXACT POLYGON-Raster EXTRACTION TOOLS
library(exactextractr)

# DEFINE LANDSCAN RASTER PATH
landscan_path <- "data/raw/LandScan_2008.tif"

# CHECK THAT LANDSCAN FILE EXISTS
file.exists(landscan_path)

# READ LANDSCAN RASTER AS A TERRA OBJECT
landscan_terra <- terra::rast(
  landscan_path
)

# PRINT LANDSCAN RASTER SUMMARY
landscan_terra

# CONVERT LANDSCAN RASTER TO RASTER PACKAGE OBJECT FOR EXACTEXTRACTR
landscan_raster <- raster::raster(
  landscan_path
)

# CHECK THAT ETHNICITY SHAPEFILE EXISTS IN MEMORY
if (!exists("ethnicity_raw_sf")) {
  stop("ethnicity_raw_sf does not exist. Re-run the shapefile import step.")
}

# CHECK THAT DISTRICT SHAPEFILE EXISTS IN MEMORY
if (!exists("districts_clean_sf")) {
  stop("districts_clean_sf does not exist. Re-run the district cleaning step.")
}

# MAKE ETHNICITY POLYGONS VALID
ethnicity_population_sf <- ethnicity_raw_sf |>
  sf::st_make_valid()

# TRANSFORM ETHNICITY POLYGONS TO LANDSCAN CRS
ethnicity_population_sf <- ethnicity_population_sf |>
  sf::st_transform(
    crs = sf::st_crs(landscan_raster)
  )

# CREATE CLEAN DISTRICT AND GROUP INDICATOR COLUMNS
ethnicity_population_sf <- ethnicity_population_sf |>
  mutate(
    district_id = adm3code,
    district_name = adm3name,
    governorate = adm2name,
    kurdish_indicator = if_else(ethnicity == 1, 1L, 0L),
    shia_indicator = if_else(shia == 1, 1L, 0L),
    sunni_indicator = if_else(sunni == 1, 1L, 0L),
    christian_indicator = if_else(christians == 1, 1L, 0L),
    turcoman_indicator = if_else(turcomans == 1, 1L, 0L),
    mixed_shia_sunni_indicator = if_else(mixed_sh_su == 1, 1L, 0L)
  )


# EXTRACT LANDSCAN POPULATION SUM INSIDE EACH ETHNICITY-DISTRICT POLYGON
ethnicity_population_sf$landscan_population <- exactextractr::exact_extract(
  x = landscan_raster,
  y = ethnicity_population_sf,
  fun = "sum",
  progress = TRUE
)

# DROP GEOMETRY AFTER RASTER EXTRACTION
ethnicity_population_components <- ethnicity_population_sf |>
  sf::st_drop_geometry()

# REPLACE MISSING LANDSCAN EXTRACTIONS WITH ZERO
ethnicity_population_components <- ethnicity_population_components |>
  mutate(
    landscan_population = replace_na(
      landscan_population,
      0
    )
  )


# BUILD POPULATION-WEIGHTED ETHNIC/SECTARIAN COMPONENTS
ethnicity_population_components <- ethnicity_population_components |>
  mutate(
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
  )


# SPLIT MIXED SHIA-SUNNI POPULATION EVENLY BETWEEN SHIA AND SUNNI
ethnicity_population_components <- ethnicity_population_components |>
  mutate(
    shia_population_adjusted = shia_population +
      0.50 * mixed_shia_sunni_population,
    sunni_population_adjusted = sunni_population +
      0.50 * mixed_shia_sunni_population
  )


# AGGREGATE LANDSCAN-WEIGHTED GROUP POPULATIONS TO DISTRICT LEVEL
ethnicity_population_shares <- ethnicity_population_components |>
  group_by(
    district_id,
    district_name,
    governorate
  ) |>
  summarize(
    landscan_population_total = sum(
      landscan_population,
      na.rm = TRUE
    ),
    kurdish_population = sum(
      kurdish_population,
      na.rm = TRUE
    ),
    shia_population = sum(
      shia_population_adjusted,
      na.rm = TRUE
    ),
    sunni_population = sum(
      sunni_population_adjusted,
      na.rm = TRUE
    ),
    christian_population = sum(
      christian_population,
      na.rm = TRUE
    ),
    turcoman_population = sum(
      turcoman_population,
      na.rm = TRUE
    ),
    mixed_shia_sunni_population = sum(
      mixed_shia_sunni_population,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# CREATE POPULATION-WEIGHTED ETHNIC/SECTARIAN PROPORTIONS
ethnicity_population_shares <- ethnicity_population_shares |>
  mutate(
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
    mixed_shia_sunni_prop = if_else(
      landscan_population_total > 0,
      mixed_shia_sunni_population / landscan_population_total,
      NA_real_
    )
  )

# CREATE RESIDUAL OTHER POPULATION SHARE
ethnicity_population_shares <- ethnicity_population_shares |>
  mutate(
    other_prop = pmax(
      0,
      1 -
        (kurdish_prop +
          shia_prop +
          sunni_prop +
          christian_prop +
          turcoman_prop)
    )
  )


# CREATE DOMINANT GROUP AND ETHNIC FRACTIONALIZATION INDEX
ethnicity_population_shares <- ethnicity_population_shares |>
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

# CHECK POPULATION-WEIGHTED PROPORTION SUMS
population_prop_check <- ethnicity_population_shares |>
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
    districts_with_zero_landscan_population = sum(
      landscan_population_total == 0,
      na.rm = TRUE
    )
  )

# PRINT POPULATION PROPORTION CHECK
population_prop_check


# COMPARE AREA-WEIGHTED AND POPULATION-WEIGHTED SHARES IF OLD TABLE EXISTS
if (exists("ethnicity_shares")) {
  ethnicity_weighting_comparison <- ethnicity_shares |>
    dplyr::select(
      district_id,
      district_name,
      governorate,
      area_sunni_prop = sunni_prop,
      area_shia_prop = shia_prop,
      area_kurdish_prop = kurdish_prop,
      area_dominant_group = dominant_group
    ) |>
    left_join(
      ethnicity_population_shares |>
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
    mutate(
      sunni_difference = population_sunni_prop - area_sunni_prop,
      shia_difference = population_shia_prop - area_shia_prop,
      kurdish_difference = population_kurdish_prop - area_kurdish_prop,
      dominant_group_changed = area_dominant_group != population_dominant_group
    )

  # PRINT LARGEST SUNNI DIFFERENCES
  ethnicity_weighting_comparison |>
    arrange(
      desc(abs(sunni_difference))
    ) |>
    print(n = 25)
}


# REPLACE OLD AREA-WEIGHTED ETHNICITY TABLE WITH POPULATION-WEIGHTED TABLE
ethnicity_shares <- ethnicity_population_shares


# REMOVE OLD ETHNICITY VARIABLES FROM DISTRICT-MONTH PANEL
district_month_panel_population_weighted <- district_month_panel |>
  dplyr::select(
    -any_of(c(
      "intersect_area_total_km2",
      "kurdish_area",
      "shia_area",
      "sunni_area",
      "christian_area",
      "turcoman_area",
      "mixed_shia_sunni_area",
      "kurdish_prop",
      "shia_prop",
      "sunni_prop",
      "christian_prop",
      "turcoman_prop",
      "mixed_shia_sunni_prop",
      "other_prop",
      "dominant_group",
      "dominant_group_prop",
      "ethnic_fractionalization"
    ))
  ) |>
  left_join(
    ethnicity_population_shares,
    by = c(
      "district_id",
      "district_name",
      "governorate"
    )
  )

# REPLACE ACTIVE PANEL OBJECT WITH POPULATION-WEIGHTED VERSION
district_month_panel <- district_month_panel_population_weighted


# ========================================================================
# FINAL HEATMAP: ISIS ATTACK FREQUENCY BY DISTRICT, MONTH, AND ETHNICITY
# ========================================================================

# 00) PACKAGES ------------------------------------------------------------

# LOAD CORE DATA WRANGLING TOOLS
library(tidyverse)

# LOAD DATE TOOLS
library(lubridate)

# LOAD PLOT AXIS LABEL TOOLS
library(scales)

# LOAD PLOT COMPOSITION TOOLS
library(patchwork)


# 01) CHECK REQUIRED OBJECTS ----------------------------------------------

# CHECK THAT THE DISTRICT-MONTH PANEL EXISTS
if (!exists("district_month_panel")) {
  stop(
    "district_month_panel does not exist. Re-run the district-month panel construction script."
  )
}

# CHECK THAT THE ETHNICITY TABLE EXISTS
if (!exists("ethnicity_shares")) {
  stop(
    "ethnicity_shares does not exist. Re-run the ethnicity aggregation script."
  )
}

# CHECK THAT THE EXPORT HELPER EXISTS
if (!exists("export_plot")) {
  stop(
    "export_plot() does not exist. Source your 24) Export Cleaned Data.R helper script."
  )
}


# 02) DEFINE PLOT SETTINGS -------------------------------------------------

# DEFINE THE ATTACK VARIABLE TO USE IN THE HEATMAP
heatmap_metric <- "attack_count"

# DEFINE THE HUMAN-READABLE HEATMAP LEGEND LABEL
heatmap_legend_label <- "Monthly\nattacks"

# DEFINE THE MAIN FIGURE TITLE
main_title <- "ISIS Attack Frequency Across Iraqi Districts by Ethnic/Sectarian Composition"

# DEFINE THE MAIN FIGURE SUBTITLE
main_subtitle <- "Districts are ordered by dominant ethnic/sectarian group, group concentration, and total ISIS attack frequency"

# DEFINE THE MAIN FIGURE CAPTION
main_caption <- "Note: Attack frequency is measured as monthly event counts from GTD-derived ISIS attack records. Ethnic/sectarian composition is area-weighted from district-intersected ethnicity polygons."

# DEFINE THE ETHNICITY PALETTE
ethnic_palette <- c(
  "Kurdish" = "#006A4E",
  "Shia" = "#842829",
  "Sunni" = "#005B8F",
  "Christian" = "#2E2D5B",
  "Turcoman" = "#8F567A",
  "Other" = "#525252"
)

# DEFINE THE LOW COLOR FOR ATTACK INTENSITY
attack_low_color <- "grey96"

# DEFINE THE HIGH COLOR FOR ATTACK INTENSITY
attack_high_color <- "#842829"


# 03) CHECK REQUIRED COLUMNS ----------------------------------------------

# DEFINE REQUIRED PANEL COLUMNS
required_panel_columns <- c(
  "district_id",
  "district_name",
  "governorate",
  "district_label",
  "event_month",
  "attack_count",
  "dominant_group",
  "dominant_group_prop"
)

# FIND MISSING PANEL COLUMNS
missing_panel_columns <- setdiff(
  required_panel_columns,
  names(district_month_panel)
)

# STOP IF REQUIRED PANEL COLUMNS ARE MISSING
if (length(missing_panel_columns) > 0) {
  stop(
    paste(
      "district_month_panel is missing these columns:",
      paste(missing_panel_columns, collapse = ", ")
    )
  )
}

# DEFINE REQUIRED ETHNICITY COLUMNS
required_ethnicity_columns <- c(
  "district_id",
  "district_name",
  "governorate",
  "kurdish_prop",
  "shia_prop",
  "sunni_prop",
  "christian_prop",
  "turcoman_prop",
  "other_prop"
)

# FIND MISSING ETHNICITY COLUMNS
missing_ethnicity_columns <- setdiff(
  required_ethnicity_columns,
  names(ethnicity_shares)
)

# STOP IF REQUIRED ETHNICITY COLUMNS ARE MISSING
if (length(missing_ethnicity_columns) > 0) {
  stop(
    paste(
      "ethnicity_shares is missing these columns:",
      paste(missing_ethnicity_columns, collapse = ", ")
    )
  )
}


# 04) CREATE DISTRICT ORDER -----------------------------------------------

# BUILD DISTRICT-LEVEL ORDERING TABLE
district_order_table <- district_month_panel |>
  group_by(
    district_id,
    district_label,
    dominant_group
  ) |>
  summarize(
    total_attacks = sum(attack_count, na.rm = TRUE),
    mean_monthly_attacks = mean(attack_count, na.rm = TRUE),
    max_monthly_attacks = max(attack_count, na.rm = TRUE),
    dominant_group_prop = first(dominant_group_prop),
    .groups = "drop"
  ) |>
  arrange(
    dominant_group,
    desc(dominant_group_prop),
    desc(total_attacks),
    district_label
  )

# CREATE DISTRICT ORDER VECTOR
district_order <- district_order_table |>
  pull(district_label)

# REVERSE DISTRICT ORDER SO THE FIRST SORTED DISTRICT APPEARS AT THE TOP
district_plot_levels <- rev(district_order)


# 05) PREPARE HEATMAP DATA -------------------------------------------------

# PREPARE DISTRICT-MONTH PANEL FOR PLOTTING
heatmap_data <- district_month_panel |>
  mutate(
    district_label = factor(
      district_label,
      levels = district_plot_levels
    ),
    event_month = as.Date(event_month)
  )

# CREATE A SIMPLE HEATMAP RANGE CHECK
heatmap_range_check <- heatmap_data |>
  summarize(
    rows = n(),
    districts = n_distinct(district_label),
    months = n_distinct(event_month),
    total_attacks = sum(attack_count, na.rm = TRUE),
    max_monthly_district_attacks = max(attack_count, na.rm = TRUE)
  )

# PRINT HEATMAP RANGE CHECK
heatmap_range_check


# 06) PREPARE ETHNICITY SIDE PANEL DATA -----------------------------------

# PREPARE ETHNICITY DATA IN LONG FORMAT
ethnicity_long <- ethnicity_shares |>
  mutate(
    district_label = str_c(
      district_name,
      " [",
      governorate,
      "]"
    )
  ) |>
  mutate(
    district_label = factor(
      district_label,
      levels = district_plot_levels
    )
  ) |>
  dplyr::select(
    district_label,
    kurdish_prop,
    shia_prop,
    sunni_prop,
    christian_prop,
    turcoman_prop,
    other_prop
  ) |>
  pivot_longer(
    cols = ends_with("_prop"),
    names_to = "group",
    values_to = "proportion"
  ) |>
  mutate(
    group = case_when(
      group == "kurdish_prop" ~ "Kurdish",
      group == "shia_prop" ~ "Shia",
      group == "sunni_prop" ~ "Sunni",
      group == "christian_prop" ~ "Christian",
      group == "turcoman_prop" ~ "Turcoman",
      group == "other_prop" ~ "Other",
      TRUE ~ group
    )
  ) |>
  mutate(
    group = factor(
      group,
      levels = names(ethnic_palette)
    )
  )


# 07) BUILD ETHNICITY SIDE PANEL ------------------------------------------

# BUILD THE LEFT ETHNIC-COMPOSITION PANEL
plot_ethnicity_side_panel <- ggplot(
  ethnicity_long,
  aes(
    x = proportion,
    y = district_label,
    fill = group
  )
) +
  geom_col(
    width = 0.9
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0, 0.5, 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_fill_manual(
    values = ethnic_palette,
    drop = FALSE
  ) +
  labs(
    title = "District Composition",
    x = "Area share",
    y = NULL,
    fill = "Group"
  ) +
  theme_classic(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10
    ),
    axis.text.y = element_text(
      size = 4.5
    ),
    axis.text.x = element_text(
      size = 7
    ),
    axis.title.x = element_text(
      size = 8
    ),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(
      size = 8
    ),
    legend.text = element_text(
      size = 7
    ),
    legend.key.size = unit(
      0.35,
      "cm"
    ),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )


# 08) BUILD ATTACK HEATMAP PANEL ------------------------------------------

# BUILD THE RIGHT ATTACK HEATMAP PANEL
plot_attack_heatmap <- ggplot(
  heatmap_data,
  aes(
    x = event_month,
    y = district_label,
    fill = .data[[heatmap_metric]]
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.03
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_gradient(
    low = attack_low_color,
    high = attack_high_color,
    trans = "sqrt",
    name = heatmap_legend_label
  ) +
  labs(
    title = "ISIS Attack Activity Over Time",
    subtitle = "Monthly district-level attack frequency",
    x = NULL,
    y = NULL
  ) +
  theme_classic(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10
    ),
    plot.subtitle = element_text(
      size = 8
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      size = 8
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 8
    ),
    legend.text = element_text(
      size = 7
    ),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )


# 09) COMBINE PANELS INTO FINAL FIGURE ------------------------------------

# COMBINE THE ETHNICITY PANEL AND THE ATTACK HEATMAP
final_isis_ethnicity_heatmap <- plot_ethnicity_side_panel +
  plot_attack_heatmap +
  plot_layout(
    widths = c(1.25, 3.75),
    guides = "collect"
  ) +
  plot_annotation(
    title = main_title,
    subtitle = main_subtitle,
    caption = main_caption
  ) &
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 10
    ),
    plot.caption = element_text(
      size = 8,
      hjust = 0
    ),
    legend.position = "bottom"
  )

# PRINT THE FINAL HEATMAP
final_isis_ethnicity_heatmap


# 10) EXPORT FINAL FIGURE --------------------------------------------------

# EXPORT THE FINAL HEATMAP USING YOUR HELPER FUNCTION
export_plot(
  plot_object = final_isis_ethnicity_heatmap,
  file_name = "final_isis_ethnicity_district_month_heatmap.png",
  width = 15,
  height = 11
)


# BUILD AREA VS POPULATION COMPARISON PLOT FOR SUNNI SHARE
plot_sunni_area_vs_population <- ethnicity_weighting_comparison |>
  ggplot(
    aes(
      x = area_sunni_prop,
      y = population_sunni_prop
    )
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    alpha = 0.75,
    size = 2
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Area-Weighted vs. LandScan Population-Weighted Sunni Share",
    subtitle = "Each point represents one Iraqi district",
    x = "Area-weighted Sunni share",
    y = "Population-weighted Sunni share"
  ) +
  theme_classic()

# PRINT COMPARISON PLOT
plot_sunni_area_vs_population

# EXPORT COMPARISON PLOT
export_plot(
  plot_object = plot_sunni_area_vs_population,
  file_name = "area_vs_landscan_population_weighted_sunni_share.png",
  width = 8,
  height = 7
)


# ========================================================================
# FINAL HEATMAP: ISIS ATTACK FREQUENCY BY DISTRICT, MONTH, AND
# LANDSCAN-WEIGHTED ETHNIC / SECTARIAN POPULATION COMPOSITION
# ========================================================================

# 00) PACKAGES ------------------------------------------------------------

# LOAD CORE DATA WRANGLING TOOLS
library(tidyverse)

# LOAD DATE-HANDLING TOOLS
library(lubridate)

# LOAD AXIS LABEL TOOLS
library(scales)

# LOAD PLOT COMPOSITION TOOLS
library(patchwork)


# 01) CHECK REQUIRED OBJECTS ----------------------------------------------

# STOP IF THE DISTRICT-MONTH PANEL DOES NOT EXIST
if (!exists("district_month_panel")) {
  stop(
    "district_month_panel does not exist. Re-run the district-month panel construction script."
  )
}

# STOP IF THE POPULATION-WEIGHTED ETHNICITY TABLE DOES NOT EXIST
if (!exists("ethnicity_shares")) {
  stop(
    "ethnicity_shares does not exist. Re-run the LandScan population-weighted ethnicity script."
  )
}

# STOP IF THE EXPORT HELPER DOES NOT EXIST
if (!exists("export_plot")) {
  stop(
    "export_plot() does not exist. Source your 24) Export Cleaned Data.R helper script."
  )
}


# 02) DEFINE FIGURE SETTINGS ----------------------------------------------

# DEFINE THE HEATMAP VARIABLE
heatmap_metric <- "attack_count"

# DEFINE THE HEATMAP LEGEND LABEL
heatmap_legend_label <- "Monthly\nattacks"

# DEFINE THE MAIN FIGURE TITLE
main_title <- "ISIS Attack Frequency Across Iraqi Districts by Ethnic/Sectarian Population Composition"

# DEFINE THE MAIN FIGURE SUBTITLE
main_subtitle <- "Districts are ordered by LandScan-weighted dominant group, population concentration, and total ISIS attack frequency"

# DEFINE THE MAIN FIGURE CAPTION
main_caption <- paste(
  "Note: Attack frequency is measured as monthly event counts from GTD-derived ISIS attack records.",
  "Ethnic/sectarian composition is population-weighted using LandScan raster population estimates",
  "intersected with district-level ethnicity polygons.",
  "The square-root fill scale reduces domination by extreme attack months."
)

# DEFINE ETHNIC / SECTARIAN COLOR PALETTE
ethnic_palette <- c(
  "Kurdish" = "#006A4E",
  "Shia" = "#842829",
  "Sunni" = "#005B8F",
  "Christian" = "#2E2D5B",
  "Turcoman" = "#8F567A",
  "Other" = "#525252"
)

# DEFINE LOW ATTACK-INTENSITY COLOR
attack_low_color <- "grey96"

# DEFINE HIGH ATTACK-INTENSITY COLOR
attack_high_color <- "#842829"


# 03) CHECK REQUIRED COLUMNS ----------------------------------------------

# DEFINE REQUIRED DISTRICT-MONTH PANEL COLUMNS
required_panel_columns <- c(
  "district_id",
  "district_name",
  "governorate",
  "district_label",
  "event_month",
  "attack_count",
  "dominant_group",
  "dominant_group_prop",
  "kurdish_prop",
  "shia_prop",
  "sunni_prop",
  "christian_prop",
  "turcoman_prop",
  "other_prop"
)

# FIND MISSING DISTRICT-MONTH PANEL COLUMNS
missing_panel_columns <- setdiff(
  required_panel_columns,
  names(district_month_panel)
)

# STOP IF REQUIRED PANEL COLUMNS ARE MISSING
if (length(missing_panel_columns) > 0) {
  stop(
    paste(
      "district_month_panel is missing these columns:",
      paste(missing_panel_columns, collapse = ", ")
    )
  )
}

# DEFINE REQUIRED ETHNICITY TABLE COLUMNS
required_ethnicity_columns <- c(
  "district_id",
  "district_name",
  "governorate",
  "landscan_population_total",
  "kurdish_prop",
  "shia_prop",
  "sunni_prop",
  "christian_prop",
  "turcoman_prop",
  "other_prop",
  "dominant_group",
  "dominant_group_prop"
)

# FIND MISSING ETHNICITY TABLE COLUMNS
missing_ethnicity_columns <- setdiff(
  required_ethnicity_columns,
  names(ethnicity_shares)
)

# STOP IF REQUIRED ETHNICITY COLUMNS ARE MISSING
if (length(missing_ethnicity_columns) > 0) {
  stop(
    paste(
      "ethnicity_shares is missing these columns:",
      paste(missing_ethnicity_columns, collapse = ", ")
    )
  )
}


# 04) CREATE DISTRICT ORDER -----------------------------------------------

# CREATE DISTRICT-LEVEL ORDERING TABLE
district_order_table <- district_month_panel |>
  group_by(
    district_id,
    district_label,
    dominant_group
  ) |>
  summarize(
    total_attacks = sum(attack_count, na.rm = TRUE),
    mean_monthly_attacks = mean(attack_count, na.rm = TRUE),
    max_monthly_attacks = max(attack_count, na.rm = TRUE),
    dominant_group_prop = first(dominant_group_prop),
    .groups = "drop"
  ) |>
  arrange(
    dominant_group,
    desc(dominant_group_prop),
    desc(total_attacks),
    district_label
  )

# CREATE DISTRICT ORDER VECTOR
district_order <- district_order_table |>
  pull(district_label)

# REVERSE DISTRICT ORDER SO FIRST SORTED DISTRICT APPEARS AT TOP
district_plot_levels <- rev(district_order)


# 05) PREPARE HEATMAP DATA -------------------------------------------------

# PREPARE DISTRICT-MONTH DATA FOR PLOTTING
heatmap_data <- district_month_panel |>
  mutate(
    district_label = factor(
      district_label,
      levels = district_plot_levels
    ),
    event_month = as.Date(event_month)
  )

# CREATE HEATMAP RANGE CHECK
heatmap_range_check <- heatmap_data |>
  summarize(
    rows = n(),
    districts = n_distinct(district_label),
    months = n_distinct(event_month),
    total_attacks = sum(attack_count, na.rm = TRUE),
    max_monthly_district_attacks = max(attack_count, na.rm = TRUE),
    missing_dominant_group = sum(is.na(dominant_group)),
    missing_population_weighted_props = sum(
      is.na(kurdish_prop) |
        is.na(shia_prop) |
        is.na(sunni_prop) |
        is.na(christian_prop) |
        is.na(turcoman_prop) |
        is.na(other_prop)
    )
  )

# PRINT HEATMAP RANGE CHECK
heatmap_range_check


# 06) PREPARE POPULATION-WEIGHTED ETHNICITY SIDE PANEL ---------------------

# PREPARE POPULATION-WEIGHTED ETHNICITY DATA IN LONG FORMAT
ethnicity_long <- ethnicity_shares |>
  mutate(
    district_label = str_c(
      district_name,
      " [",
      governorate,
      "]"
    )
  ) |>
  mutate(
    district_label = factor(
      district_label,
      levels = district_plot_levels
    )
  ) |>
  dplyr::select(
    district_label,
    kurdish_prop,
    shia_prop,
    sunni_prop,
    christian_prop,
    turcoman_prop,
    other_prop
  ) |>
  pivot_longer(
    cols = ends_with("_prop"),
    names_to = "group",
    values_to = "proportion"
  ) |>
  mutate(
    group = case_when(
      group == "kurdish_prop" ~ "Kurdish",
      group == "shia_prop" ~ "Shia",
      group == "sunni_prop" ~ "Sunni",
      group == "christian_prop" ~ "Christian",
      group == "turcoman_prop" ~ "Turcoman",
      group == "other_prop" ~ "Other",
      TRUE ~ group
    )
  ) |>
  mutate(
    group = factor(
      group,
      levels = names(ethnic_palette)
    )
  )


# 07) BUILD POPULATION-WEIGHTED ETHNICITY SIDE PANEL -----------------------

# BUILD LEFT PANEL SHOWING LANDSCAN-WEIGHTED POPULATION COMPOSITION
plot_population_composition_side_panel <- ggplot(
  ethnicity_long,
  aes(
    x = proportion,
    y = district_label,
    fill = group
  )
) +
  geom_col(
    width = 0.9
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0, 0.5, 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_fill_manual(
    values = ethnic_palette,
    drop = FALSE
  ) +
  labs(
    title = "District Population Composition",
    subtitle = "LandScan-weighted group shares",
    x = "Population share",
    y = NULL,
    fill = "Group"
  ) +
  theme_classic(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10
    ),
    plot.subtitle = element_text(
      size = 8
    ),
    axis.text.y = element_text(
      size = 4.5
    ),
    axis.text.x = element_text(
      size = 7
    ),
    axis.title.x = element_text(
      size = 8
    ),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(
      size = 8
    ),
    legend.text = element_text(
      size = 7
    ),
    legend.key.size = unit(
      0.35,
      "cm"
    ),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )


# 08) BUILD ATTACK HEATMAP PANEL ------------------------------------------

# BUILD RIGHT PANEL SHOWING MONTHLY ISIS ATTACK FREQUENCY
plot_attack_heatmap <- ggplot(
  heatmap_data,
  aes(
    x = event_month,
    y = district_label,
    fill = .data[[heatmap_metric]]
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.03
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_gradient(
    low = attack_low_color,
    high = attack_high_color,
    trans = "sqrt",
    name = heatmap_legend_label
  ) +
  labs(
    title = "ISIS Attack Activity Over Time",
    subtitle = "Monthly district-level attack frequency",
    x = NULL,
    y = NULL
  ) +
  theme_classic(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10
    ),
    plot.subtitle = element_text(
      size = 8
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      size = 8
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 8
    ),
    legend.text = element_text(
      size = 7
    ),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )


# 09) COMBINE PANELS INTO FINAL POPULATION-CENTERED FIGURE -----------------

# COMBINE POPULATION COMPOSITION PANEL AND ATTACK HEATMAP
final_population_weighted_heatmap <- plot_population_composition_side_panel +
  plot_attack_heatmap +
  plot_layout(
    widths = c(1.3, 3.7),
    guides = "collect"
  ) +
  plot_annotation(
    title = main_title,
    subtitle = main_subtitle,
    caption = main_caption
  ) &
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 10
    ),
    plot.caption = element_text(
      size = 8,
      hjust = 0
    ),
    legend.position = "bottom"
  )

# PRINT FINAL POPULATION-WEIGHTED HEATMAP
final_population_weighted_heatmap


# 10) EXPORT FINAL POPULATION-WEIGHTED FIGURE ------------------------------

# EXPORT FINAL FIGURE USING YOUR EXPORT HELPER
export_plot(
  plot_object = final_population_weighted_heatmap,
  file_name = "final_population_weighted_isis_ethnicity_district_month_heatmap.png",
  width = 15,
  height = 11
)


# ========================================================================
# FINAL TOP-40 HEATMAP: POPULATION-WEIGHTED ETHNIC COMPOSITION
# ========================================================================

# 01) SELECT TOP 40 DISTRICTS ---------------------------------------------

# DEFINE NUMBER OF DISTRICTS TO DISPLAY
top_n_districts <- 40

# IDENTIFY TOP DISTRICTS BY TOTAL ISIS ATTACK FREQUENCY
top_districts <- district_month_panel |>
  group_by(
    district_id,
    district_label,
    dominant_group
  ) |>
  summarize(
    total_attacks = sum(attack_count, na.rm = TRUE),
    dominant_group_prop = first(dominant_group_prop),
    .groups = "drop"
  ) |>
  arrange(
    desc(total_attacks)
  ) |>
  slice_head(
    n = top_n_districts
  )

# CREATE ORDER FOR TOP DISTRICTS
top_district_order <- top_districts |>
  arrange(
    dominant_group,
    desc(dominant_group_prop),
    desc(total_attacks),
    district_label
  ) |>
  pull(district_label)

# REVERSE ORDER FOR TOP-TO-BOTTOM DISPLAY
top_district_plot_levels <- rev(top_district_order)


# 02) PREPARE TOP-40 HEATMAP DATA -----------------------------------------

# FILTER DISTRICT-MONTH PANEL TO TOP DISTRICTS
top_heatmap_data <- district_month_panel |>
  filter(
    district_label %in% top_district_order
  ) |>
  mutate(
    district_label = factor(
      district_label,
      levels = top_district_plot_levels
    ),
    event_month = as.Date(event_month)
  )

# PREPARE TOP-DISTRICT POPULATION COMPOSITION DATA
top_ethnicity_long <- ethnicity_shares |>
  mutate(
    district_label = str_c(
      district_name,
      " [",
      governorate,
      "]"
    )
  ) |>
  filter(
    district_label %in% top_district_order
  ) |>
  mutate(
    district_label = factor(
      district_label,
      levels = top_district_plot_levels
    )
  ) |>
  dplyr::select(
    district_label,
    kurdish_prop,
    shia_prop,
    sunni_prop,
    christian_prop,
    turcoman_prop,
    other_prop
  ) |>
  pivot_longer(
    cols = ends_with("_prop"),
    names_to = "group",
    values_to = "proportion"
  ) |>
  mutate(
    group = case_when(
      group == "kurdish_prop" ~ "Kurdish",
      group == "shia_prop" ~ "Shia",
      group == "sunni_prop" ~ "Sunni",
      group == "christian_prop" ~ "Christian",
      group == "turcoman_prop" ~ "Turcoman",
      group == "other_prop" ~ "Other",
      TRUE ~ group
    )
  ) |>
  mutate(
    group = factor(
      group,
      levels = names(ethnic_palette)
    )
  )


# 03) BUILD TOP-40 POPULATION COMPOSITION PANEL ----------------------------

# BUILD LEFT PANEL FOR TOP-40 DISTRICT POPULATION COMPOSITION
plot_top40_population_composition <- ggplot(
  top_ethnicity_long,
  aes(
    x = proportion,
    y = district_label,
    fill = group
  )
) +
  geom_col(
    width = 0.9
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0, 0.5, 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_fill_manual(
    values = ethnic_palette,
    drop = FALSE
  ) +
  labs(
    title = "District Population Composition",
    subtitle = "LandScan-weighted group shares",
    x = "Population share",
    y = NULL,
    fill = "Group"
  ) +
  theme_classic(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10
    ),
    plot.subtitle = element_text(
      size = 8
    ),
    axis.text.y = element_text(
      size = 7
    ),
    axis.text.x = element_text(
      size = 7
    ),
    axis.title.x = element_text(
      size = 8
    ),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(
      size = 8
    ),
    legend.text = element_text(
      size = 7
    ),
    legend.key.size = unit(
      0.35,
      "cm"
    )
  )


# 04) BUILD TOP-40 ATTACK HEATMAP PANEL -----------------------------------

# BUILD RIGHT PANEL FOR TOP-40 MONTHLY ATTACK FREQUENCY
plot_top40_attack_heatmap <- ggplot(
  top_heatmap_data,
  aes(
    x = event_month,
    y = district_label,
    fill = attack_count
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.06
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_gradient(
    low = attack_low_color,
    high = attack_high_color,
    trans = "sqrt",
    name = "Monthly\nattacks"
  ) +
  labs(
    title = "ISIS Attack Activity Over Time",
    subtitle = "Top 40 districts by total ISIS attack frequency",
    x = NULL,
    y = NULL
  ) +
  theme_classic(
    base_size = 10
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10
    ),
    plot.subtitle = element_text(
      size = 8
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      size = 8
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 8
    ),
    legend.text = element_text(
      size = 7
    )
  )


# 05) COMBINE TOP-40 FINAL FIGURE -----------------------------------------

# COMBINE TOP-40 POPULATION COMPOSITION PANEL AND HEATMAP
final_top40_population_weighted_heatmap <- plot_top40_population_composition +
  plot_top40_attack_heatmap +
  plot_layout(
    widths = c(1.35, 3.65),
    guides = "collect"
  ) +
  plot_annotation(
    title = "ISIS Attack Frequency Across High-Activity Iraqi Districts by Ethnic/Sectarian Population Composition",
    subtitle = "Top 40 districts by total attack frequency; districts ordered by LandScan-weighted dominant group and group concentration",
    caption = paste(
      "Note: Attack frequency is measured as monthly event counts from GTD-derived ISIS attack records.",
      "Ethnic/sectarian composition is population-weighted using LandScan raster population estimates.",
      "The square-root fill scale reduces domination by extreme attack months."
    )
  ) &
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 10
    ),
    plot.caption = element_text(
      size = 8,
      hjust = 0
    ),
    legend.position = "bottom"
  )

# PRINT TOP-40 POPULATION-WEIGHTED HEATMAP
final_top40_population_weighted_heatmap

# EXPORT TOP-40 POPULATION-WEIGHTED HEATMAP
export_plot(
  plot_object = final_top40_population_weighted_heatmap,
  file_name = "final_top40_population_weighted_isis_ethnicity_district_month_heatmap.png",
  width = 15,
  height = 9
)
