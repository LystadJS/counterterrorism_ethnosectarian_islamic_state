# LIBRARIES:
library(tidyverse)

# FUNCTIONS:
CleanNames <- function(x) {                          # General Name Cleaning Function
  x |>                                                 # Data Object
    stringr::str_trim() |>                               # Remove whitespace
    stringr::str_to_lower() |>                           # Convert to lower-case
    stringr::str_replace_all("[^a-z0-9]+", "_") |>       # Replace non alpha-numeric lower-case characters to underscore
    stringr::str_replace_all("^_+|_+$", "") |>           # Removes trailing and leading underscores
    make.unique(sep = "_")                               # Adds an underscore separator
}
ParseBinary <- function(x) {                                     # Binary parsing Function
  x_std <-                                                         # Standardized data object
    x |>                                                             # Data Object
      stringr::str_trim() |>                                           # Remove whitespace
      stringr::str_to_lower()                                          # Convert to lower-case

  dplyr::case_when(                                                # Create new vector with:
    x_std %in% c("1", "y", "yes", "true", "t")  ~ 1L,                # Binary Indicator - 1
    x_std %in% c("0", "n", "no", "false", "f")  ~ 0L,                # Binary Indicator - 0
    is.na(x_std)                                ~ NA_integer_,       # Binary Indicator - N/A
    TRUE                                        ~ NA_integer_        # Else - N/A
  )
}
WM <- function(x, w) {                                                   # Weighted Mean Function
  if (all(is.na(x)) || all(is.na(w)) || sum(w, na.rm = TRUE) == 0) {       # Check for impossible values
    return(NA_real_)                                                         
  }
  stats::weighted.mean(                                                    # Compute weighted mean
    x     = x,                                                               # x = values to be averaged
    w     = w,                                                               # w = weights for values
    na.rm = TRUE)                                                            # Ignore missing values
}
MinMax <- function(x) {                               # Min-Max Rescaling Function
  rng <- range(x, na.rm = TRUE)                         # Find the minimum and maximum value of x
  if (!is.finite(rng[1]) || !is.finite(rng[2]))         # Check for impossible values
    return(rep(NA_real_, length(x)))                     
  if (rng[1] == rng[2])                                 # Check if non-missing values are identical
    return(rep(1, length(x)))       
  (x - rng[1]) / (rng[2] - rng[1])                      # Perform min-max scaling
}

# DATA:
data_dir <- # Raw data directory
  "data/raw/"
file_index <- # Data file index
  tibble(
    file_path = list.files(
      path       = data_dir,
      pattern    = "\\.csv$",
      full.names = TRUE,
      recursive  = TRUE
    )
  )
raw_list <- 
  file_index |>
  mutate(
    data = purrr::map(
      file_path,
      \(fp) readr::read_csv(
        file           = fp,
        col_types      = readr::cols(.default = readr::col_character()),
        na             = na_tokens,
        id             = "source_file",
        show_col_types = FALSE,
        progress       = FALSE,
        name_repair    = "unique"
      ) |>
        rename_with(CleanNames)
    )
  )
raw_tbl <- 
  raw_list |>
  pull(data) |>
  list_rbind()

# DATA MANIPULATION:
bin_cols <- # Binary columns
  c(
    "success", 
    "suicide", 
    "multiple",
    "claimed", 
    "claim2", 
    "claim3"
  )
date_cols <- # Date columns
  character(0)
datetime_cols <- # Date-time columns
  character(0)
dbl_cols <- # Double columns
  c(
    "latitude", 
    "longitude", 
    "propvalue"
  )
int_cols <- # Integer columns
  c(
    "iyear", 
    "imonth", 
    "iday",
    "nkill", 
    "nwound",
    "attacktype1", 
    "attacktype2", 
    "attacktype3",
    "targtype1", 
    "targtype2", 
    "targtype3",
    "weaptype1", 
    "weaptype2", 
    "weaptype3", 
    "weaptype4",
    "weapsubtype1", 
    "weapsubtype2", 
    "weapsubtype3", 
    "weapsubtype4"
  )
num_text_cols <- # Numeric columns
  character(0)

na_tokens <- # Missing value token values
  c(
    "",
    "NA", 
    "N/A", 
    "NULL", 
    "UNKNOWN", 
    "UNK", 
    "-99", 
    "-999"
  )

clean_tbl <- 
  raw_tbl |>
  mutate(
    across(
      where(is.character),
      \(x) x |>
        stringr::str_squish() |>
        dplyr::na_if("")
    )
  ) |>
  mutate(
    across(
      any_of(int_cols), 
      \(x) readr::parse_integer(x, na = na_tokens)
    ),
    across(
      any_of(dbl_cols),
      \(x) readr::parse_double(x, na = na_tokens)
    ),
    across(
      any_of(num_text_cols), 
      \(x) readr::parse_number(x, na = na_tokens)
    ),
    across(
      any_of(date_cols), 
      \(x) readr::parse_date(x, na = na_tokens)
    ),
    across(
      any_of(datetime_cols), 
      \(x) readr::parse_datetime(x, na = na_tokens)
    ),
    across(
      any_of(bin_cols), 
      ParseBinary
    )
  ) |>
  mutate( # Transform use columns 
    year       = iyear,
    month      = imonth,
    day        = iday,
    fatalities = nkill,
    injuries   = nwound,
    country    = country_txt,
    region     = region_txt,
    admin1     = provstate,
    city_name  = city,
    perp_text = stringr::str_c( # Perpetrator ID
      coalesce(gname, ""), " || ",
      coalesce(gname2, ""), " || ",
      coalesce(gname3, ""), " || ",
      coalesce(gsubname, ""), " || ",
      coalesce(gsubname2, ""), " || ",
      coalesce(gsubname3, "")
    ),
    narrative_text = stringr::str_c( # Narrative text 
      coalesce(summary, ""), " || ",
      coalesce(motive, ""), " || ",
      coalesce(addnotes, "")
    ),
    event_date = dplyr::case_when( # Event date
      !is.na(year) & !is.na(month) & !is.na(day) &
        month >= 1 & month <= 12 &
        day >= 1 & day <= 31 ~
        as.Date(sprintf("%04d-%02d-%02d", year, month, day)),
      TRUE ~ as.Date(NA)
    )
  ) |>
  distinct()

# ISIS FORMAL AFFILIATE START YEARS -----------------------------------------
# Tight version for the main affiliate-year project.
# Pre-ISIS predecessors are excluded from the main formal sample here.

is_formal_start_year <- 
  c(
    "Islamic State of Iraq and the Levant (ISIL)" = 2013,
    "Boko Haram" = 2015,
    "Khorasan Chapter of the Islamic State" = 2015,
    "Abu Sayyaf Group (ASG)" = 2014,
    "Sinai Province of the Islamic State" = 2014,
    "Bangsamoro Islamic Freedom Movement (BIFM)" = 2015,
    "Allied Democratic Forces (ADF)" = 2019,
    "Tripoli Province of the Islamic State" = 2014,
    "Barqa Province of the Islamic State" = 2014,
    "Islamic State in the Greater Sahara (ISGS)" = 2015,
    "Central Africa Province of the Islamic State" = 2019,
    "Ansar al-Sunna (Mozambique)" = 2019,
    "Ansar Bayt al-Maqdis (Ansar Jerusalem)" = 2014,
    "Maute Group" = 2015,
    "Adan-Abyan Province of the Islamic State" = 2014,
    "Islamic State in Bangladesh" = 2015,
    "Fezzan Province of the Islamic State" = 2014,
    "Mujahidin Indonesia Timur (MIT)" = 2014,
    "Sanaa Province of the Islamic State" = 2014,
    "Jamaah Ansharut Daulah" = 2015,
    "Caucasus Province of the Islamic State" = 2015,
    "Islamic State in Egypt" = 2015,
    "Jund al-Khilafah (Tunisia)" = 2015,
    "Hadramawt Province of the Islamic State" = 2014,
    "Tehrik-e-Khilafat" = 2014,
    "East Asia Division of the Islamic State" = 2017,
    "Najd Province of the Islamic State" = 2014,
    "Algeria Province of the Islamic State" = 2014,
    "Supporters of the Islamic State in Jerusalem" = 2015,
    "National Thowheeth Jama'ath" = 2019,
    "Ansar Al-Khilafa (Philippines)" = 2014,
    "Hind Province of the Islamic State" = 2019,
    "Hijaz Province of the Islamic State" = 2014,
    "Pakistan Province of the Islamic State" = 2021,
    "Al Bayda Province of the Islamic State" = 2014,
    "Lahij Province of the Islamic State" = 2014,
    "Islamic Youth Shura Council" = 2014,
    "Lions of Khilafah in the Maldives" = 2015,
    "Bahrain Province of the Islamic State" = 2015,
    "Jaish-e-Khorasan (JeK)" = 2015,
    "Jundul Khilafah (Philippines)" = 2015,
    "Jund al-Khilafa" = 2014,
    "Shabwah Province of the Islamic State" = 2014,
    "Supporters of the Islamic State in the Land of the Two Holy Mosques" = 2015
  )

formal_names <- 
  names(is_formal_start_year)

# ANALYTIC TABLE

incident_tbl <-
  clean_tbl |>
  mutate(
    gname_start     = unname(is_formal_start_year[gname]),
    gname2_start    = unname(is_formal_start_year[gname2]),
    gname3_start    = unname(is_formal_start_year[gname3]),
    gsubname_start  = unname(is_formal_start_year[gsubname]),
    gsubname2_start = unname(is_formal_start_year[gsubname2]),
    gsubname3_start = unname(is_formal_start_year[gsubname3])
  ) |>
  rowwise() |>
  mutate(
    formal_start_year = {
      yrs <- 
        c(
          gname_start, 
          gname2_start, 
          gname3_start,
          gsubname_start, 
          gsubname2_start, 
          gsubname3_start
        )
      yrs <- 
        yrs[!is.na(yrs)]
      if (length(yrs) == 0) NA_integer_ else min(yrs)
    },
    affiliate_name = dplyr::coalesce(
      if_else(gname %in% formal_names, gname, NA_character_),
      if_else(gname2 %in% formal_names, gname2, NA_character_),
      if_else(gname3 %in% formal_names, gname3, NA_character_),
      if_else(gsubname %in% formal_names, gsubname, NA_character_),
      if_else(gsubname2 %in% formal_names, gsubname2, NA_character_),
      if_else(gsubname3 %in% formal_names, gsubname3, NA_character_)
    ),
    formal_affiliate_hit = !is.na(formal_start_year) & !is.na(year) & year >= formal_start_year,

    # -----------------------
    # INCIDENT-LEVEL DVs
    # target families
    # -----------------------
    target_security_state   = as.integer(any(c(targtype1, targtype2, targtype3) %in% c(2, 3, 4, 7), na.rm = TRUE)),
    target_civilian_soft    = as.integer(any(c(targtype1, targtype2, targtype3) %in% c(1, 8, 10, 12, 14, 15, 18), na.rm = TRUE)),
    target_religious        = as.integer(any(c(targtype1, targtype2, targtype3) %in% c(15), na.rm = TRUE)),
    target_infrastructure   = as.integer(any(c(targtype1, targtype2, targtype3) %in% c(6, 9, 11, 16, 19, 21), na.rm = TRUE)),
    target_private_citizens = as.integer(any(c(targtype1, targtype2, targtype3) %in% c(14), na.rm = TRUE)),
    target_police           = as.integer(any(c(targtype1, targtype2, targtype3) %in% c(3), na.rm = TRUE)),
    target_military         = as.integer(any(c(targtype1, targtype2, targtype3) %in% c(4), na.rm = TRUE)),
    target_government       = as.integer(any(c(targtype1, targtype2, targtype3) %in% c(2, 7), na.rm = TRUE)),

    # -----------------------
    # INCIDENT-LEVEL DVs
    # tactic families
    # -----------------------
    tactic_suicide         = as.integer(suicide == 1),
    tactic_bombing         = as.integer(any(c(attacktype1, attacktype2, attacktype3) %in% c(3), na.rm = TRUE)),
    tactic_armed_assault   = as.integer(any(c(attacktype1, attacktype2, attacktype3) %in% c(2), na.rm = TRUE)),
    tactic_unarmed_assault = as.integer(any(c(attacktype1, attacktype2, attacktype3) %in% c(8), na.rm = TRUE)),
    tactic_facility_attack = as.integer(any(c(attacktype1, attacktype2, attacktype3) %in% c(7), na.rm = TRUE)),
    tactic_hostage_kidnap  = as.integer(any(c(attacktype1, attacktype2, attacktype3) %in% c(5, 6), na.rm = TRUE)),
    tactic_hijack          = as.integer(any(c(attacktype1, attacktype2, attacktype3) %in% c(4), na.rm = TRUE)),
    tactic_explosives      = as.integer(
      any(c(weaptype1, weaptype2, weaptype3, weaptype4) %in% c(6), na.rm = TRUE) |
        any(
          (c(weaptype1, weaptype2, weaptype3, weaptype4) %in% c(2)) &
            (c(weapsubtype1, weapsubtype2, weapsubtype3, weapsubtype4) %in% c(30)),
          na.rm = TRUE
        )
    ),
    tactic_firearms          = as.integer(any(c(weaptype1, weaptype2, weaptype3, weaptype4) %in% c(5), na.rm = TRUE)),
    tactic_melee             = as.integer(any(c(weaptype1, weaptype2, weaptype3, weaptype4) %in% c(9), na.rm = TRUE)),
    tactic_incendiary        = as.integer(any(c(weaptype1, weaptype2, weaptype3, weaptype4) %in% c(8), na.rm = TRUE)),
    non_missing_attack_types = sum(!is.na(c(attacktype1, attacktype2, attacktype3)) &
                                     c(attacktype1, attacktype2, attacktype3) != 9),
    tactic_complex           = as.integer(non_missing_attack_types > 1 | multiple == 1),

    claim_any                = as.integer(any(c(claimed, claim2, claim3) == 1, na.rm = TRUE)),
    attack_count = 1L
  ) |>
  ungroup() |>
  filter(
    formal_affiliate_hit,
    !is.na(affiliate_name),
    !is.na(year),
    !is.na(country)
  ) |>
  mutate(
    target_hard   = as.integer(target_security_state == 1 | target_infrastructure == 1),
    target_soft   = as.integer(target_civilian_soft == 1),
    lethality_any = as.integer(coalesce(fatalities, 0L) > 0)
  )

# HOME COUNTRY LOOKUP -------------------------------------------------------

home_country_lookup <- 
  incident_tbl |>
  count(affiliate_name, country, sort = TRUE) |>
  group_by(affiliate_name) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    affiliate_name,
    home_country = country
  )

# COMPETITION MEASURE -------------------------------------------------------
# country-year competition from the full GTD, not just the ISIS sample

generic_or_low_info_groups <- 
  c(
    "Unknown",
    "Unaffiliated Individual(s)",
    "Jihadi-inspired extremists",
    "Islamist extremists",
    "Muslim extremists"
  )

country_year_competition <- 
  clean_tbl |>
  mutate(
    gname_clean = na_if(gname, "")
  ) |>
  filter(!is.na(year), !is.na(country)) |>
  mutate(
    known_org         = !is.na(gname_clean) & !(gname_clean %in% generic_or_low_info_groups),
    formal_is_primary = gname_clean %in% formal_names
  ) |>
  group_by(country, year) |>
  summarise(
    active_groups_all   = n_distinct(gname_clean[known_org]),
    active_groups_nonis = n_distinct(gname_clean[known_org & !formal_is_primary]),
    attacks_all         = dplyr::n(),
    attacks_nonis       = sum(!formal_is_primary, na.rm = TRUE),
    .groups             = "drop"
  )

# AFFILIATE-COUNTRY-YEAR PANEL ---------------------------------------------

affiliate_country_year_tbl <- 
  incident_tbl |>
  group_by(affiliate_name, year, country, region) |>
  summarise(
    attacks = dplyr::n(),
    fatalities_total = sum(fatalities, na.rm = TRUE),
    injuries_total   = sum(injuries, na.rm = TRUE),
    fatalities_mean  = mean(fatalities, na.rm = TRUE),
    injuries_mean    = mean(injuries, na.rm = TRUE),

    # main DVs: target shares
    civilian_soft_share    = mean(target_civilian_soft, na.rm = TRUE),
    security_state_share   = mean(target_security_state, na.rm = TRUE),
    religious_share        = mean(target_religious, na.rm = TRUE),
    infrastructure_share   = mean(target_infrastructure, na.rm = TRUE),
    private_citizens_share = mean(target_private_citizens, na.rm = TRUE),
    police_share           = mean(target_police, na.rm = TRUE),
    military_share         = mean(target_military, na.rm = TRUE),
    government_share       = mean(target_government, na.rm = TRUE),
    hard_target_share      = mean(target_hard, na.rm = TRUE),
    soft_target_share      = mean(target_soft, na.rm = TRUE),

    # tactic shares
    suicide_share         = mean(tactic_suicide, na.rm = TRUE),
    bombing_share         = mean(tactic_bombing, na.rm = TRUE),
    armed_assault_share   = mean(tactic_armed_assault, na.rm = TRUE),
    unarmed_assault_share = mean(tactic_unarmed_assault, na.rm = TRUE),
    facility_attack_share = mean(tactic_facility_attack, na.rm = TRUE),
    hostage_kidnap_share  = mean(tactic_hostage_kidnap, na.rm = TRUE),
    hijack_share          = mean(tactic_hijack, na.rm = TRUE),
    explosives_share      = mean(tactic_explosives, na.rm = TRUE),
    firearms_share        = mean(tactic_firearms, na.rm = TRUE),
    melee_share           = mean(tactic_melee, na.rm = TRUE),
    incendiary_share      = mean(tactic_incendiary, na.rm = TRUE),
    complex_share         = mean(tactic_complex, na.rm = TRUE),

    # controls
    success_share       = mean(success == 1, na.rm = TRUE),
    claim_share         = mean(claim_any == 1, na.rm = TRUE),
    lethality_any_share = mean(lethality_any == 1, na.rm = TRUE),
    .groups             = "drop"
  ) |>
  left_join(home_country_lookup, by      = "affiliate_name") |>
  left_join(country_year_competition, by = c("country", "year")) |>
  group_by(affiliate_name) |>
  mutate(
    home_country_ind        = as.integer(country == home_country),
    first_year_in_country   = min(year, na.rm = TRUE),
    years_active_in_country = year - first_year_in_country + 1
  ) |>
  ungroup() |>
  group_by(affiliate_name, year) |>
  mutate(
    country_share_within_affiliate_year = attacks / sum(attacks, na.rm = TRUE),
    country_hhi_component               = country_share_within_affiliate_year^2
  ) |>
  ungroup()

# AFFILIATE-YEAR PANEL ------------------------------------------------------

affiliate_year_tbl <- affiliate_country_year_tbl |>
  group_by(affiliate_name, year) |>
  summarise(
    attacks        = sum(attacks, na.rm = TRUE),
    country_spread = n_distinct(country),
    region_spread  = n_distinct(region),

    # DVs
    civilian_soft_share    = WM(civilian_soft_share, attacks),
    security_state_share   = WM(security_state_share, attacks),
    religious_share        = WM(religious_share, attacks),
    infrastructure_share   = WM(infrastructure_share, attacks),
    private_citizens_share = WM(private_citizens_share, attacks),
    police_share           = WM(police_share, attacks),
    military_share         = WM(military_share, attacks),
    government_share       = WM(government_share, attacks),
    hard_target_share      = WM(hard_target_share, attacks),
    soft_target_share      = WM(soft_target_share, attacks),

    suicide_share          = WM(suicide_share, attacks),
    bombing_share          = WM(bombing_share, attacks),
    armed_assault_share    = WM(armed_assault_share, attacks),
    unarmed_assault_share  = WM(unarmed_assault_share, attacks),
    facility_attack_share  = WM(facility_attack_share, attacks),
    hostage_kidnap_share   = WM(hostage_kidnap_share, attacks),
    hijack_share           = WM(hijack_share, attacks),
    explosives_share       = WM(explosives_share, attacks),
    firearms_share         = WM(firearms_share, attacks),
    melee_share            = WM(melee_share, attacks),
    incendiary_share       = WM(incendiary_share, attacks),
    complex_share          = WM(complex_share, attacks),

    fatalities_total       = sum(fatalities_total, na.rm = TRUE),
    injuries_total         = sum(injuries_total, na.rm = TRUE),
    fatalities_mean        = WM(fatalities_mean, attacks),
    injuries_mean          = WM(injuries_mean, attacks),

    # CONTROLS
    success_share          = WM(success_share, attacks),
    claim_share            = WM(claim_share, attacks),
    lethality_any_share    = WM(lethality_any_share, attacks),

    # MODERATOR PIECES - LOCAL EMBEDEDNESS
    home_country_attack_share    = sum(attacks[home_country_ind == 1], na.rm = TRUE) / sum(attacks, na.rm = TRUE),
    country_concentration_hhi    = sum(country_hhi_component, na.rm = TRUE),
    years_active_in_country_mean = WM(years_active_in_country, attacks),

    # COMPETITION
    competition_groups_nonis  = WM(active_groups_nonis, attacks),
    competition_attacks_nonis = WM(attacks_nonis, attacks),
    competition_groups_all    = WM(active_groups_all, attacks),
    competition_attacks_all   = WM(attacks_all, attacks),

    .groups                   = "drop"
  ) |>
  group_by(affiliate_name) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    years_since_first_observed = year - min(year, na.rm = TRUE) + 1,

    # moderator: local embeddedness index
    home_country_attack_share_01    = MinMax(home_country_attack_share),
    country_concentration_hhi_01    = MinMax(country_concentration_hhi),
    years_active_in_country_mean_01 = MinMax(years_active_in_country_mean),
    local_embeddedness_index        =
      (home_country_attack_share_01 +
         country_concentration_hhi_01 +
         years_active_in_country_mean_01) / 3,

    # LAGGED CONTROLS / MODERATORS
    lag_attacks                      = lag(attacks),
    lag_civilian_soft_share          = lag(civilian_soft_share),
    lag_security_state_share         = lag(security_state_share),
    lag_suicide_share                = lag(suicide_share),
    lag_explosives_share             = lag(explosives_share),
    lag_competition_groups_nonis     = lag(competition_groups_nonis),
    lag_competition_attacks_nonis    = lag(competition_attacks_nonis),
    lag_home_country_attack_share    = lag(home_country_attack_share),
    lag_country_concentration_hhi    = lag(country_concentration_hhi),
    lag_years_active_in_country_mean = lag(years_active_in_country_mean),
    lag_local_embeddedness_index     = lag(local_embeddedness_index)
  ) |>
  ungroup()




# OPTIONAL: MERGE AFFILIATION SCORES (MAIN IVs) -----------------------------

if (file.exists(affiliation_scores_path)) {

  affiliation_scores_tbl <- readr::read_csv(
    affiliation_scores_path,
    col_types = readr::cols(.default = readr::col_character()),
    na = na_tokens,
    show_col_types = FALSE
  ) |>
    rename_with(CleanNames) |>
    mutate(
      affiliate_name = dplyr::coalesce(affiliate_name, group_name),
      year = readr::parse_integer(as.character(year), na = na_tokens),
      scai = readr::parse_double(as.character(scai), na = na_tokens),
      formal_status_score = readr::parse_double(as.character(formal_status_score), na = na_tokens),
      admin_control_score = readr::parse_double(as.character(admin_control_score), na = na_tokens),
      media_integration_score = readr::parse_double(as.character(media_integration_score), na = na_tokens),
      finance_integration_score = readr::parse_double(as.character(finance_integration_score), na = na_tokens),
      personnel_integration_score = readr::parse_double(as.character(personnel_integration_score), na = na_tokens),
      operational_integration_score = readr::parse_double(as.character(operational_integration_score), na = na_tokens),
      governance_replication_score = readr::parse_double(as.character(governance_replication_score), na = na_tokens)
    ) |>
    select(
      affiliate_name, year, scai,
      formal_status_score, admin_control_score, media_integration_score,
      finance_integration_score, personnel_integration_score,
      operational_integration_score, governance_replication_score
    )

  affiliate_year_tbl <- affiliate_year_tbl |>
    left_join(affiliation_scores_tbl, by = c("affiliate_name", "year")) |>
    group_by(affiliate_name) |>
    arrange(year, .by_group = TRUE) |>
    mutate(
      scai_lag = lag(scai),
      formal_status_score_lag = lag(formal_status_score),
      admin_control_score_lag = lag(admin_control_score),
      media_integration_score_lag = lag(media_integration_score),
      finance_integration_score_lag = lag(finance_integration_score),
      personnel_integration_score_lag = lag(personnel_integration_score),
      operational_integration_score_lag = lag(operational_integration_score),
      governance_replication_score_lag = lag(governance_replication_score)
    ) |>
    ungroup()
}

# OPTIONAL: LONG WEAPON FILE -------------------------------------------------
# Useful for descriptive work, not required for the main panel.

weapon_long_tbl <- clean_tbl |>
  pivot_longer(
    cols = any_of(c("weaptype1_txt", "weaptype2_txt", "weaptype3_txt", "weaptype4_txt")),
    names_to = "weapon_slot",
    values_to = "weapon_name",
    values_drop_na = TRUE
  )

# QUALITY REPORT ------------------------------------------------------------

quality_report <- affiliate_year_tbl |>
  summarise(
    n_rows = n(),
    n_cols = ncol(affiliate_year_tbl),
    n_affiliates = n_distinct(affiliate_name),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    across(
      everything(),
      \(x) mean(is.na(x)),
      .names = "pct_na__{.col}"
    )
  )

# WRITE OUTPUTS -------------------------------------------------------------

readr::write_csv(clean_tbl, "data/processed/gtd_clean_tbl.csv")
readr::write_csv(incident_tbl, "data/processed/gtd_formal_affiliate_incidents.csv")
readr::write_csv(affiliate_country_year_tbl, "data/processed/affiliate_country_year_tbl.csv")
readr::write_csv(affiliate_year_tbl, "data/processed/affiliate_year_tbl.csv")
readr::write_csv(weapon_long_tbl, "data/processed/weapon_long_tbl.csv")
readr::write_csv(quality_report, "data/processed/quality_report_affiliate_year.csv")