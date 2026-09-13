# ========================================================================
# TEMPORAL PHASE VISUALIZATION OF ISIS ATTACK PATTERNS
# GTD MONTHLY ATTACK FREQUENCY + TARGET SELECTION
# FROM SCRATCH
#
# PURPOSE:
#   Build monthly ISIS attack-pattern visualizations showing how attack
#   frequency and target selection changed across conflict phases.
#
# CORE CONTRAST:
#   1. Monthly ISIS attack frequency over time
#   2. Monthly ISIS target-category composition over time
#   3. Phase-specific shifts in attack volume and target selection
#   4. Optional comparison across Iraq, Syria, and other countries
#
# UNIT OF RAW DATA:
#   One GTD attack event
#
# UNIT OF ANALYTIC DATA:
#   Month-by-target-category-by-theater cell
#
# KEY TIME VARIABLES:
#   - Event year
#   - Event month
#   - Monthly event date
#   - Editable conflict phase
#
# KEY OUTCOMES:
#   - ISIS attack count by month
#   - ISIS attack count by month and target category
#   - Known fatalities by month
#   - Known wounded by month
#   - Known casualties by month
#   - Claimed attack count by month
#
# KEY OUTPUTS:
#   outputs/plots/isis_monthly_attack_frequency_by_phase.png
#   outputs/plots/isis_monthly_target_stacked_area_by_phase.png
#   outputs/plots/isis_monthly_target_stacked_area_by_theater.png
#   outputs/plots/isis_monthly_attack_frequency_by_theater.png
#   outputs/tables/isis_monthly_attack_summary.csv
#   outputs/tables/isis_monthly_target_summary.csv
#   outputs/tables/isis_phase_summary.csv
#   data/clean/gtd_isis_temporal_phase_events.csv
# ========================================================================

# ========================================================================
# 00) PACKAGES
# ========================================================================

library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(forcats)
library(viridis)


# ========================================================================
# 01) FILE PATHS
# ========================================================================

gtd_path <- "data/raw/csv/GTD_A.csv"

clean_dir <- "data/clean"

table_dir <- "outputs/tables"

plot_dir <- "outputs/plots"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(gtd_path))


# ========================================================================
# 02) USER-EDITABLE SETTINGS
# ========================================================================

analysis_start_date <- lubridate::ymd("2004-01-01")

analysis_end_date <- lubridate::ymd("2022-12-31")

include_only_isis_related_attacks <- TRUE

make_theater_panels <- TRUE

smooth_monthly_line <- FALSE

minimum_monthly_count_for_label <- 1


# ========================================================================
# 03) USER-EDITABLE CONFLICT PHASE BOUNDARIES
# ========================================================================

conflict_phase_boundaries <- tibble::tribble(
  ~conflict_phase               , ~phase_start , ~phase_end   ,
  "Pre-caliphate insurgency"    , "2004-01-01" , "2014-06-28" ,
  "Territorial caliphate"       , "2014-06-29" , "2017-12-31" ,
  "Post-territorial insurgency" , "2018-01-01" , "2022-12-31"
) |>
  mutate(
    phase_start = lubridate::ymd(phase_start),
    phase_end = lubridate::ymd(phase_end),
    phase_midpoint = phase_start + ((phase_end - phase_start) / 2),
    conflict_phase = factor(
      conflict_phase,
      levels = conflict_phase
    )
  )


# ========================================================================
# 04) USER-EDITABLE TARGET CATEGORY ORDER
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


# ========================================================================
# 05) HELPER FUNCTION: ASSIGN CONFLICT PHASE
# ========================================================================

assign_conflict_phase <- function(date_vector, phase_table) {
  purrr::map_chr(
    .x = date_vector,
    .f = function(one_date) {
      if (is.na(one_date)) {
        return(NA_character_)
      }

      phase_match <- phase_table |>
        filter(
          one_date >= phase_start,
          one_date <= phase_end
        )

      if (nrow(phase_match) == 0) {
        return(NA_character_)
      }

      return(
        as.character(phase_match$conflict_phase[[1]])
      )
    }
  )
}


# ========================================================================
# 06) IMPORT AND CLEAN GTD DATA
# ========================================================================

gtd_raw <- readr::read_csv(
  file = gtd_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  janitor::clean_names()

gtd_clean <- gtd_raw |>
  mutate(
    eventid = as.character(eventid),
    iyear = as.integer(iyear),
    imonth = as.integer(imonth),
    iday = as.integer(iday),
    latitude = readr::parse_number(latitude),
    longitude = readr::parse_number(longitude),
    nkill = readr::parse_number(nkill),
    nwound = readr::parse_number(nwound),
    claimed = readr::parse_number(claimed),
    group_name_raw = str_squish(as.character(gname)),
    country_name = str_squish(as.character(country_txt)),
    province_name = str_squish(as.character(provstate)),
    city_name = str_squish(as.character(city)),
    attack_type_raw = str_squish(as.character(attacktype1_txt)),
    target_type_raw = str_squish(as.character(targtype1_txt)),
    target_subtype_raw = str_squish(as.character(targsubtype1_txt)),
    claim_mode_raw = str_squish(as.character(claimmode_txt))
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
    known_casualties_event = known_fatalities_event + known_wounded_event,
    claimed_flag = case_when(
      claimed == 1 ~ 1,
      claimed == 0 ~ 0,
      TRUE ~ NA_real_
    )
  ) |>
  filter(
    !is.na(event_month)
  ) |>
  filter(
    event_month >= analysis_start_date,
    event_month <= analysis_end_date
  )


# ========================================================================
# 07) FILTER TO ISIS / ISLAMIC STATE ATTACKS
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

gtd_isis <- gtd_clean |>
  mutate(
    isis_related_attack = str_detect(
      string = str_to_lower(group_name_raw),
      pattern = isis_group_pattern
    )
  ) |>
  filter(
    isis_related_attack
  )

isis_filter_check <- gtd_isis |>
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
# 08) CREATE THEATER VARIABLE
# ========================================================================

gtd_isis_theater <- gtd_isis |>
  mutate(
    theater = case_when(
      country_name == "Iraq" ~ "Iraq",
      country_name == "Syria" ~ "Syria",
      TRUE ~ "Other countries"
    ),
    theater = factor(
      theater,
      levels = c(
        "Iraq",
        "Syria",
        "Other countries"
      )
    )
  )

theater_check <- gtd_isis_theater |>
  count(
    theater,
    country_name,
    sort = TRUE
  )

readr::write_csv(
  theater_check,
  file.path(table_dir, "isis_theater_country_check.csv"),
  na = ""
)

print(theater_check, n = 100)


# ========================================================================
# 09) COLLAPSE TARGET TYPES INTO ANALYTIC CATEGORIES
# ========================================================================

gtd_isis_targets <- gtd_isis_theater |>
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

target_type_filter_check <- gtd_isis_targets |>
  count(
    target_type_raw,
    target_subtype_raw,
    target_category,
    sort = TRUE
  )

target_category_check <- gtd_isis_targets |>
  count(
    target_category,
    sort = TRUE
  )

print(target_category_check, n = 50)

readr::write_csv(
  target_type_filter_check,
  file.path(table_dir, "isis_temporal_target_type_filter_check.csv"),
  na = ""
)

readr::write_csv(
  target_category_check,
  file.path(table_dir, "isis_temporal_target_category_count_check.csv"),
  na = ""
)


# ========================================================================
# 10) ADD CONFLICT PHASE LABELS
# ========================================================================

gtd_isis_phase_events <- gtd_isis_targets |>
  mutate(
    conflict_phase = assign_conflict_phase(
      date_vector = event_month,
      phase_table = conflict_phase_boundaries
    ),
    conflict_phase = factor(
      conflict_phase,
      levels = levels(conflict_phase_boundaries$conflict_phase)
    )
  )

phase_event_check <- gtd_isis_phase_events |>
  count(
    conflict_phase,
    sort = FALSE
  )

print(phase_event_check)

readr::write_csv(
  phase_event_check,
  file.path(table_dir, "isis_conflict_phase_event_check.csv"),
  na = ""
)

readr::write_csv(
  gtd_isis_phase_events,
  file.path(clean_dir, "gtd_isis_temporal_phase_events.csv"),
  na = ""
)


# ========================================================================
# 11) BUILD MONTHLY ATTACK SUMMARY
# ========================================================================

monthly_sequence <- tibble(
  event_month = seq.Date(
    from = lubridate::floor_date(analysis_start_date, unit = "month"),
    to = lubridate::floor_date(analysis_end_date, unit = "month"),
    by = "month"
  )
)

isis_monthly_attack_summary <- gtd_isis_phase_events |>
  count(
    event_month,
    name = "isis_attack_count"
  ) |>
  right_join(
    monthly_sequence,
    by = "event_month"
  ) |>
  mutate(
    isis_attack_count = replace_na(isis_attack_count, 0L),
    conflict_phase = assign_conflict_phase(
      date_vector = event_month,
      phase_table = conflict_phase_boundaries
    ),
    conflict_phase = factor(
      conflict_phase,
      levels = levels(conflict_phase_boundaries$conflict_phase)
    )
  ) |>
  arrange(
    event_month
  )

readr::write_csv(
  isis_monthly_attack_summary,
  file.path(table_dir, "isis_monthly_attack_summary.csv"),
  na = ""
)

print(isis_monthly_attack_summary, n = 24)


# ========================================================================
# 12) BUILD MONTHLY TARGET-CATEGORY SUMMARY
# ========================================================================

isis_monthly_target_summary <- gtd_isis_phase_events |>
  count(
    event_month,
    target_category,
    name = "isis_attack_count"
  ) |>
  complete(
    event_month = monthly_sequence$event_month,
    target_category = factor(
      target_category_levels,
      levels = target_category_levels
    ),
    fill = list(
      isis_attack_count = 0
    )
  ) |>
  group_by(
    event_month
  ) |>
  mutate(
    monthly_attack_total = sum(
      isis_attack_count,
      na.rm = TRUE
    ),
    target_share_in_month = if_else(
      monthly_attack_total > 0,
      isis_attack_count / monthly_attack_total,
      NA_real_
    ),
    conflict_phase = assign_conflict_phase(
      date_vector = event_month,
      phase_table = conflict_phase_boundaries
    ),
    conflict_phase = factor(
      conflict_phase,
      levels = levels(conflict_phase_boundaries$conflict_phase)
    )
  ) |>
  ungroup() |>
  arrange(
    event_month,
    target_category
  )

readr::write_csv(
  isis_monthly_target_summary,
  file.path(table_dir, "isis_monthly_target_summary.csv"),
  na = ""
)

print(isis_monthly_target_summary, n = 40)


# ========================================================================
# 13) BUILD MONTHLY THEATER SUMMARY
# ========================================================================

isis_monthly_theater_summary <- gtd_isis_phase_events |>
  count(
    event_month,
    theater,
    name = "isis_attack_count"
  ) |>
  complete(
    event_month = monthly_sequence$event_month,
    theater = factor(
      c(
        "Iraq",
        "Syria",
        "Other countries"
      ),
      levels = c(
        "Iraq",
        "Syria",
        "Other countries"
      )
    ),
    fill = list(
      isis_attack_count = 0
    )
  ) |>
  mutate(
    conflict_phase = assign_conflict_phase(
      date_vector = event_month,
      phase_table = conflict_phase_boundaries
    ),
    conflict_phase = factor(
      conflict_phase,
      levels = levels(conflict_phase_boundaries$conflict_phase)
    )
  ) |>
  arrange(
    event_month,
    theater
  )

readr::write_csv(
  isis_monthly_theater_summary,
  file.path(table_dir, "isis_monthly_theater_summary.csv"),
  na = ""
)


# ========================================================================
# 14) BUILD MONTHLY TARGET SUMMARY BY THEATER
# ========================================================================

isis_monthly_target_theater_summary <- gtd_isis_phase_events |>
  count(
    event_month,
    theater,
    target_category,
    name = "isis_attack_count"
  ) |>
  complete(
    event_month = monthly_sequence$event_month,
    theater = factor(
      c(
        "Iraq",
        "Syria",
        "Other countries"
      ),
      levels = c(
        "Iraq",
        "Syria",
        "Other countries"
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
    event_month,
    theater
  ) |>
  mutate(
    monthly_theater_attack_total = sum(
      isis_attack_count,
      na.rm = TRUE
    ),
    target_share_in_theater_month = if_else(
      monthly_theater_attack_total > 0,
      isis_attack_count / monthly_theater_attack_total,
      NA_real_
    ),
    conflict_phase = assign_conflict_phase(
      date_vector = event_month,
      phase_table = conflict_phase_boundaries
    ),
    conflict_phase = factor(
      conflict_phase,
      levels = levels(conflict_phase_boundaries$conflict_phase)
    )
  ) |>
  ungroup() |>
  arrange(
    event_month,
    theater,
    target_category
  )

readr::write_csv(
  isis_monthly_target_theater_summary,
  file.path(table_dir, "isis_monthly_target_theater_summary.csv"),
  na = ""
)


# ========================================================================
# 15) BUILD PHASE-LEVEL SUMMARY
# ========================================================================

isis_phase_summary <- gtd_isis_phase_events |>
  filter(
    !is.na(conflict_phase)
  ) |>
  group_by(
    conflict_phase
  ) |>
  summarize(
    isis_attack_count = n(),
    known_fatalities_total = sum(
      known_fatalities_event,
      na.rm = TRUE
    ),
    known_wounded_total = sum(
      known_wounded_event,
      na.rm = TRUE
    ),
    known_casualties_total = sum(
      known_casualties_event,
      na.rm = TRUE
    ),
    claimed_attack_count = sum(
      claimed_flag == 1,
      na.rm = TRUE
    ),
    claim_known_denominator = sum(
      !is.na(claimed_flag)
    ),
    percent_claimed = if_else(
      claim_known_denominator > 0,
      claimed_attack_count / claim_known_denominator,
      NA_real_
    ),
    first_month = min(
      event_month,
      na.rm = TRUE
    ),
    last_month = max(
      event_month,
      na.rm = TRUE
    ),
    active_months = n_distinct(
      event_month
    ),
    mean_monthly_attacks = isis_attack_count / active_months,
    .groups = "drop"
  )

print(isis_phase_summary)

readr::write_csv(
  isis_phase_summary,
  file.path(table_dir, "isis_phase_summary.csv"),
  na = ""
)


# ========================================================================
# 16) PHASE TARGET-CATEGORY SUMMARY
# ========================================================================

isis_phase_target_summary <- gtd_isis_phase_events |>
  filter(
    !is.na(conflict_phase)
  ) |>
  count(
    conflict_phase,
    target_category,
    name = "isis_attack_count"
  ) |>
  complete(
    conflict_phase = factor(
      levels(conflict_phase_boundaries$conflict_phase),
      levels = levels(conflict_phase_boundaries$conflict_phase)
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
    conflict_phase
  ) |>
  mutate(
    phase_attack_total = sum(
      isis_attack_count,
      na.rm = TRUE
    ),
    target_share_in_phase = if_else(
      phase_attack_total > 0,
      isis_attack_count / phase_attack_total,
      NA_real_
    )
  ) |>
  ungroup()

print(isis_phase_target_summary, n = 50)

readr::write_csv(
  isis_phase_target_summary,
  file.path(table_dir, "isis_phase_target_summary.csv"),
  na = ""
)


# ========================================================================
# 17) PLOT 1: MONTHLY ISIS ATTACK FREQUENCY WITH PHASE SHADING
# ========================================================================

plot_monthly_attack_frequency <- isis_monthly_attack_summary |>
  ggplot(
    aes(
      x = event_month,
      y = isis_attack_count
    )
  ) +
  geom_rect(
    data = conflict_phase_boundaries,
    aes(
      xmin = phase_start,
      xmax = phase_end,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey85",
    alpha = 0.35
  ) +
  geom_vline(
    data = conflict_phase_boundaries,
    aes(
      xintercept = phase_start
    ),
    linewidth = 0.35,
    linetype = "dashed",
    color = "grey35"
  ) +
  geom_line(
    linewidth = 0.75,
    color = "black"
  ) +
  geom_point(
    size = 0.9,
    color = "black"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  scale_y_continuous(
    labels = number_format(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  labs(
    title = "Monthly ISIS Attack Frequency Over Time",
    subtitle = "Monthly GTD attack counts with editable conflict-phase boundaries",
    caption = "Vertical dashed lines indicate phase start dates. Shaded regions show conflict-phase intervals.",
    x = NULL,
    y = "ISIS attacks per month"
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
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(plot_monthly_attack_frequency)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_monthly_attack_frequency_by_phase.png"
  ),
  plot = plot_monthly_attack_frequency,
  width = 12,
  height = 6,
  dpi = 300
)


# ========================================================================
# 18) PLOT 2: STACKED AREA CHART BY TARGET CATEGORY
# ========================================================================

plot_monthly_target_area <- isis_monthly_target_summary |>
  ggplot(
    aes(
      x = event_month,
      y = isis_attack_count,
      fill = target_category
    )
  ) +
  geom_rect(
    data = conflict_phase_boundaries,
    aes(
      xmin = phase_start,
      xmax = phase_end,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey90",
    alpha = 0.25
  ) +
  geom_area(
    alpha = 0.92,
    linewidth = 0.10,
    color = "grey20"
  ) +
  geom_vline(
    data = conflict_phase_boundaries,
    aes(
      xintercept = phase_start
    ),
    inherit.aes = FALSE,
    linewidth = 0.35,
    linetype = "dashed",
    color = "grey25"
  ) +
  scale_fill_viridis_d(
    option = "turbo",
    name = "Target category"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  scale_y_continuous(
    labels = number_format(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "ISIS Target Selection Over Time",
    subtitle = "Monthly ISIS attack counts stacked by collapsed GTD target category",
    caption = "Target categories are analytically collapsed from GTD targtype1_txt and targsubtype1_txt. Dashed lines indicate conflict-phase starts.",
    x = NULL,
    y = "ISIS attacks per month"
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
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom"
  )

print(plot_monthly_target_area)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_monthly_target_stacked_area_by_phase.png"
  ),
  plot = plot_monthly_target_area,
  width = 13,
  height = 7,
  dpi = 300
)


# ========================================================================
# 19) PLOT 3: MONTHLY ATTACK FREQUENCY BY THEATER
# ========================================================================

plot_monthly_attack_frequency_by_theater <- isis_monthly_theater_summary |>
  ggplot(
    aes(
      x = event_month,
      y = isis_attack_count
    )
  ) +
  geom_rect(
    data = conflict_phase_boundaries,
    aes(
      xmin = phase_start,
      xmax = phase_end,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey88",
    alpha = 0.30
  ) +
  geom_vline(
    data = conflict_phase_boundaries,
    aes(
      xintercept = phase_start
    ),
    inherit.aes = FALSE,
    linewidth = 0.30,
    linetype = "dashed",
    color = "grey35"
  ) +
  geom_line(
    linewidth = 0.70,
    color = "black"
  ) +
  facet_wrap(
    facets = vars(theater),
    ncol = 1,
    scales = "free_y"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  scale_y_continuous(
    labels = number_format(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  labs(
    title = "Monthly ISIS Attack Frequency by Theater",
    subtitle = "Separate panels for Iraq, Syria, and other countries",
    caption = "Panels use free y-axis scales so lower-volume theaters remain visible.",
    x = NULL,
    y = "ISIS attacks per month"
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    strip.text = element_text(
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
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(plot_monthly_attack_frequency_by_theater)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_monthly_attack_frequency_by_theater.png"
  ),
  plot = plot_monthly_attack_frequency_by_theater,
  width = 12,
  height = 9,
  dpi = 300
)


# ========================================================================
# 20) PLOT 4: STACKED TARGET AREA BY THEATER
# ========================================================================

plot_monthly_target_area_by_theater <- isis_monthly_target_theater_summary |>
  ggplot(
    aes(
      x = event_month,
      y = isis_attack_count,
      fill = target_category
    )
  ) +
  geom_rect(
    data = conflict_phase_boundaries,
    aes(
      xmin = phase_start,
      xmax = phase_end,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey90",
    alpha = 0.20
  ) +
  geom_area(
    alpha = 0.92,
    linewidth = 0.08,
    color = "grey20"
  ) +
  geom_vline(
    data = conflict_phase_boundaries,
    aes(
      xintercept = phase_start
    ),
    inherit.aes = FALSE,
    linewidth = 0.30,
    linetype = "dashed",
    color = "grey35"
  ) +
  facet_wrap(
    facets = vars(theater),
    ncol = 1,
    scales = "free_y"
  ) +
  scale_fill_viridis_d(
    option = "turbo",
    name = "Target category"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  scale_y_continuous(
    labels = number_format(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "ISIS Target Selection Over Time by Theater",
    subtitle = "Monthly ISIS attack counts stacked by target category; panels compare Iraq, Syria, and other countries",
    caption = "Panels use free y-axis scales. Target categories are collapsed from GTD target type and subtype fields.",
    x = NULL,
    y = "ISIS attacks per month"
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    strip.text = element_text(
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
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom"
  )

print(plot_monthly_target_area_by_theater)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_monthly_target_stacked_area_by_theater.png"
  ),
  plot = plot_monthly_target_area_by_theater,
  width = 13,
  height = 10,
  dpi = 300
)


# ========================================================================
# 21) PLOT 5: PHASE-LEVEL TARGET COMPOSITION
# ========================================================================

plot_phase_target_composition <- isis_phase_target_summary |>
  ggplot(
    aes(
      x = target_share_in_phase,
      y = conflict_phase,
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
    title = "ISIS Target Composition by Conflict Phase",
    subtitle = "Each bar sums to 100% within phase",
    caption = "This plot helps distinguish volume changes from target-selection changes.",
    x = "Percent of ISIS attacks within phase",
    y = NULL
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
    legend.position = "bottom"
  )

print(plot_phase_target_composition)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_phase_target_composition.png"
  ),
  plot = plot_phase_target_composition,
  width = 12,
  height = 6,
  dpi = 300
)


# ========================================================================
# 22) OPTIONAL PLOT: CLAIMED ATTACKS BY MONTH
# ========================================================================

isis_monthly_claim_summary <- gtd_isis_phase_events |>
  group_by(
    event_month
  ) |>
  summarize(
    isis_attack_count = n(),
    claimed_attack_count = sum(
      claimed_flag == 1,
      na.rm = TRUE
    ),
    claim_known_denominator = sum(
      !is.na(claimed_flag)
    ),
    percent_claimed = if_else(
      claim_known_denominator > 0,
      claimed_attack_count / claim_known_denominator,
      NA_real_
    ),
    .groups = "drop"
  ) |>
  right_join(
    monthly_sequence,
    by = "event_month"
  ) |>
  mutate(
    isis_attack_count = replace_na(isis_attack_count, 0L),
    claimed_attack_count = replace_na(claimed_attack_count, 0),
    claim_known_denominator = replace_na(claim_known_denominator, 0),
    conflict_phase = assign_conflict_phase(
      date_vector = event_month,
      phase_table = conflict_phase_boundaries
    ),
    conflict_phase = factor(
      conflict_phase,
      levels = levels(conflict_phase_boundaries$conflict_phase)
    )
  )

readr::write_csv(
  isis_monthly_claim_summary,
  file.path(table_dir, "isis_monthly_claim_summary.csv"),
  na = ""
)

plot_monthly_claimed_attacks <- isis_monthly_claim_summary |>
  ggplot(
    aes(
      x = event_month,
      y = claimed_attack_count
    )
  ) +
  geom_rect(
    data = conflict_phase_boundaries,
    aes(
      xmin = phase_start,
      xmax = phase_end,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey88",
    alpha = 0.30
  ) +
  geom_vline(
    data = conflict_phase_boundaries,
    aes(
      xintercept = phase_start
    ),
    inherit.aes = FALSE,
    linewidth = 0.30,
    linetype = "dashed",
    color = "grey35"
  ) +
  geom_line(
    linewidth = 0.75,
    color = "black"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  scale_y_continuous(
    labels = number_format(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  labs(
    title = "Monthly ISIS Claimed Attacks Over Time",
    subtitle = "Uses GTD claimed field where available",
    caption = "Claiming behavior may reflect propaganda strategy, reporting quality, and data availability, not only operational behavior.",
    x = NULL,
    y = "Claimed ISIS attacks per month"
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
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(plot_monthly_claimed_attacks)

ggsave(
  filename = file.path(
    plot_dir,
    "isis_monthly_claimed_attacks_by_phase.png"
  ),
  plot = plot_monthly_claimed_attacks,
  width = 12,
  height = 6,
  dpi = 300
)


# ========================================================================
# 23) FINAL DIAGNOSTIC SUMMARY
# ========================================================================

final_diagnostic_summary <- tibble(
  diagnostic = c(
    "Raw GTD events imported",
    "Events inside analysis date window",
    "ISIS / Islamic State events",
    "ISIS events with valid event month",
    "ISIS events in Iraq",
    "ISIS events in Syria",
    "ISIS events in other countries",
    "First ISIS event month in analysis data",
    "Last ISIS event month in analysis data",
    "Total known fatalities",
    "Total known wounded",
    "Total known casualties",
    "Claimed attacks where claimed == 1",
    "Events with known claimed coding",
    "Target categories observed",
    "Conflict phases defined"
  ),
  value = c(
    nrow(gtd_raw),
    nrow(gtd_clean),
    nrow(gtd_isis_phase_events),
    sum(!is.na(gtd_isis_phase_events$event_month)),
    sum(gtd_isis_phase_events$theater == "Iraq", na.rm = TRUE),
    sum(gtd_isis_phase_events$theater == "Syria", na.rm = TRUE),
    sum(gtd_isis_phase_events$theater == "Other countries", na.rm = TRUE),
    as.character(min(gtd_isis_phase_events$event_month, na.rm = TRUE)),
    as.character(max(gtd_isis_phase_events$event_month, na.rm = TRUE)),
    sum(gtd_isis_phase_events$known_fatalities_event, na.rm = TRUE),
    sum(gtd_isis_phase_events$known_wounded_event, na.rm = TRUE),
    sum(gtd_isis_phase_events$known_casualties_event, na.rm = TRUE),
    sum(gtd_isis_phase_events$claimed_flag == 1, na.rm = TRUE),
    sum(!is.na(gtd_isis_phase_events$claimed_flag)),
    n_distinct(gtd_isis_phase_events$target_category, na.rm = TRUE),
    nrow(conflict_phase_boundaries)
  )
)

print(final_diagnostic_summary)

readr::write_csv(
  final_diagnostic_summary,
  file.path(table_dir, "isis_temporal_phase_diagnostics.csv"),
  na = ""
)
