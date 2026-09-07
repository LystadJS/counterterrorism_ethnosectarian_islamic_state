# ============================= #
# 0. SETUP
# ============================= #  

# LIBRARIES
library(tidyverse)
library(stringr)

# ============================= #
# 1. LOAD + MERGE GTD           #
# ============================= #
# ----------------------------- #
# 
# - Reads GTD columns as character
# - Keep formal affiliate cases
# - Keep strong probability inspired cases
# - Sends a narrow set of generic jihadist cases to review
# - Adds post-2013 gate for review cases
# 
# ----------------------------- #

GTD <- list.files( 
  path = "data/",
  pattern = "\\.csv$",
  full.names = TRUE
) |>
  set_names() |>
  map(
    read_csv,
    .progress = TRUE,
    col_types = cols(.default = col_character()) # Sets all values to characters
  ) |>
  bind_rows(.id = "source_filename") |>
  distinct(eventid, .keep_all = TRUE) # Drop duplicate events

# ============================= #
# 2. DICTIONARIES               #
# ============================= #
# ----------------------------- #
# 
# - Sets dictionary for formal IS affiliates: ISNames_Formal
# - Sets dictionary for generic jihadist perpetrators: GenericJihadiLabels
# - Sets dictionary for low information incidents for further review: LowInfoLabels
# - 
# - 
# 
# ----------------------------- #

ISNames_Formal <- c(
  "Islamic State of Iraq and the Levant (ISIL)",
  "Islamic State of Iraq (ISI)",
  "Al-Qaida in Iraq",
  "Tawhid and Jihad",
  "Khorasan Chapter of the Islamic State",
  "Sinai Province of the Islamic State",
  "Islamic State in the Greater Sahara (ISGS)",
  "Central Africa Province of the Islamic State",
  "Tripoli Province of the Islamic State",
  "Barqa Province of the Islamic State",
  "Fezzan Province of the Islamic State",
  "Caucasus Province of the Islamic State",
  "Pakistan Province of the Islamic State",
  "Hind Province of the Islamic State",
  "Adan-Abyan Province of the Islamic State",
  "Hadramawt Province of the Islamic State",
  "Lahij Province of the Islamic State",
  "Shabwah Province of the Islamic State",
  "Sanaa Province of the Islamic State",
  "Najd Province of the Islamic State",
  "Hijaz Province of the Islamic State",
  "Bahrain Province of the Islamic State",
  "Al Bayda Province of the Islamic State",
  "East Asia Division of the Islamic State",
  "Islamic State in Egypt",
  "Islamic State in Bangladesh",
  "Supporters of the Islamic State in Jerusalem",
  "Supporters of the Islamic State in the Land of the Two Holy Mosques",
  "Boko Haram",
  "Ansar Bayt al-Maqdis (Ansar Jerusalem)",
  "Allied Democratic Forces (ADF)",
  "Abu Sayyaf Group (ASG)",
  "Maute Group",
  "Bangsamoro Islamic Freedom Movement (BIFM)",
  "Jamaah Ansharut Daulah",
  "Mujahidin Indonesia Timur (MIT)",
  "Ansar al-Sunna (Mozambique)",
  "National Thowheeth Jama'ath",
  "Jamaat al-Tawhid al-Watania",
  "Tehrik-e-Khilafat",
  "Ansar Al-Khilafa (Philippines)",
  "Jundul Khilafah (Philippines)",
  "Soldiers of the Caliphate",
  "Vanguards of the Caliphate",
  "Lions of Khilafah in the Maldives",
  "Jund al-Khilafa",
  "Jund al-Khilafah (Tunisia)",
  "Jaish-e-Khorasan (JeK)",
  "Islamic Youth Shura Council"
)

GenericJihadiLabels <- c(
  "Jihadi-inspired extremists",
  "Islamist extremists",
  "Muslim extremists"
)

LowInfoLabels <- c(
  "Unknown",
  "Unaffiliated Individual(s)"
)

# -----------------------------
# 3. POSITIVE TEXT PATTERNS
# -----------------------------

# Strong explicit ISIS language
ISregex_Core <- regex(
  paste(
    c(
      "islamic state",
      "islamic state group",
      "islamic state in iraq and the levant",
      "islamic state in iraq and syria",
      "isis",
      "isil",
      "daesh"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

# Moderate / allegiance-style ISIS language
ISregex_Allegiance <- regex(
  paste(
    c(
      "abu bakr al-baghdadi",
      "al-baghdadi",
      "pledg(ed|e)? allegiance",
      "oath of allegiance",
      "baya",
      "bay`a",
      "bayah",
      "caliphate",
      "khilafah",
      "khalifa",
      "wilayat",
      "wilayah",
      "province of the islamic state",
      "supporters of the islamic state",
      "supporters of the caliphate",
      "soldiers of the caliphate",
      "loyal to the islamic state",
      "pro-isis",
      "isis-inspired",
      "isil-inspired",
      "inspired by islamic state",
      "inspired by isis",
      "isis supporter",
      "isis sympathizer",
      "isis follower",
      "islamic state"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

# Non-ISIS Islamic / jihadist groups that can create false positives
ISregex_Rejection <- regex(
  paste(
    c(
      "palestinian islamic jihad",
      "egyptian islamic jihad",
      "harakat ul jihad",
      "harkat-ul-jihad",
      "islamic movement of uzbekistan",
      "armed islamic group",
      "salafist group for preaching and combat",
      "muslim brotherhood"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

# -----------------------------
# 4. COLLAPSE TEXT FIELDS
# -----------------------------

GTD <- GTD |>
  mutate(
    perp_text = str_c(
      replace_na(gname, ""), " || ",
      replace_na(gname2, ""), " || ",
      replace_na(gname3, ""), " || ",
      replace_na(gsubname, ""), " || ",
      replace_na(gsubname2, ""), " || ",
      replace_na(gsubname3, "")
    ),
    narrative_text = str_c(
      replace_na(summary, ""), " || ",
      replace_na(motive, ""), " || ",
      replace_na(addnotes, "")
    )
  )

# -----------------------------
# 5. BUILD COMPONENT FLAGS
# -----------------------------

GTD <- GTD |>
  mutate(
    # Parse year only when needed
    iyear_num = suppressWarnings(parse_integer(iyear)),

    # Formal name hit
    formal_name_hit =
      gname %in% ISNames_Formal |
      gname2 %in% ISNames_Formal |
      gname3 %in% ISNames_Formal |
      gsubname %in% ISNames_Formal |
      gsubname2 %in% ISNames_Formal |
      gsubname3 %in% ISNames_Formal,

    # Generic jihadist label hit
    generic_jihadi_hit =
      gname %in% GenericJihadiLabels |
      gname2 %in% GenericJihadiLabels |
      gname3 %in% GenericJihadiLabels,

    # Low-information label hit
    low_info_label_hit =
      gname %in% LowInfoLabels |
      gname2 %in% LowInfoLabels |
      gname3 %in% LowInfoLabels,

    # Strong explicit ISIS text
    core_text_hit =
      str_detect(perp_text, ISregex_Core) |
      str_detect(narrative_text, ISregex_Core),

    # Moderate / allegiance-style ISIS text
    allegiance_text_hit =
      str_detect(perp_text, ISregex_Allegiance) |
      str_detect(narrative_text, ISregex_Allegiance),

    # Rejection / likely false-positive text
    exclusion_only_hit =
      str_detect(perp_text, ISregex_Rejection) |
      str_detect(narrative_text, ISregex_Rejection),

    # Claim indicators
    claim_hit =
      replace_na(claimed, "") %in% c("1", "Yes", "yes", "TRUE", "True") |
      replace_na(claim2, "") %in% c("1", "Yes", "yes", "TRUE", "True") |
      replace_na(claim3, "") %in% c("1", "Yes", "yes", "TRUE", "True")
  ) |>
  mutate(
    # Weak ISIS evidence for low-information cases
    weak_is_signal = core_text_hit | allegiance_text_hit | claim_hit
  )

# -----------------------------
# 6. SCORE
# -----------------------------

GTD <- GTD |>
  mutate(
    is_inspired_score =
      0L +
      if_else(formal_name_hit, 5L, 0L) +
      if_else(core_text_hit, 3L, 0L) +
      if_else(allegiance_text_hit, 2L, 0L) +
      if_else(generic_jihadi_hit & core_text_hit, 2L, 0L) +
      if_else(claim_hit & core_text_hit, 2L, 0L) +
      if_else(low_info_label_hit & weak_is_signal, 1L, 0L) -
      if_else(exclusion_only_hit & !core_text_hit & !formal_name_hit, 3L, 0L)
  )

# -----------------------------
# 7. REVIEW GATES
# -----------------------------

GTD <- GTD |>
  mutate(
    # Post-2013 gate for generic jihadist labels
    generic_jihadi_review_hit =
      generic_jihadi_hit &
      !is.na(iyear_num) &
      iyear_num >= 2013,

    # Low-information labels only go to review if there is some ISIS signal
    low_info_review_hit =
      low_info_label_hit &
      weak_is_signal,

    # Final review candidate logic
    review_candidate =
      generic_jihadi_review_hit |
      low_info_review_hit |
      is_inspired_score >= 3L
  )

# -----------------------------
# 8. CLASSIFY
# -----------------------------

GTD <- GTD |>
  mutate(
    is_inspired_class = case_when(
      formal_name_hit ~ "formal_or_affiliate",
      is_inspired_score >= 5L ~ "probable_inspired",
      review_candidate ~ "manual_review",
      TRUE ~ "not_is_inspired"
    ),
    inclusion_reason = case_when(
      formal_name_hit ~ "formal_name_dictionary",
      is_inspired_score >= 5L ~ "high_score_text_claim_signal",
      generic_jihadi_review_hit ~ "generic_jihadi_label_post2013",
      low_info_review_hit ~ "low_info_label_plus_is_signal",
      is_inspired_score >= 3L ~ "medium_score_review",
      TRUE ~ "not_selected"
    )
  )

# -----------------------------
# 9. OUTPUT DATASETS
# -----------------------------

GTD_IS_Formal_Affiliate <- GTD |>
  filter(is_inspired_class == "formal_or_affiliate")

GTD_IS_Inspired <- GTD |>
  filter(is_inspired_class %in% "probable_inspired")

GTD_Review <- GTD |>
  filter(is_inspired_class == "manual_review")

write_csv(GTD_IS_Formal_Affiliate, "gtd_formal_or_affiliate.csv")
write_csv(GTD_IS_Inspired, "gtd_probable_inspired.csv")
write_csv(GTD_Review, "gtd_manual_review.csv")

# -----------------------------
# 10. OPTIONAL DIAGNOSTICS
# -----------------------------

GTD |>
  count(is_inspired_class)

GTD_Review |>
  count(gname, sort = TRUE)

GTD |>
  filter(eventid == "201410230047") |>
  select(
    eventid, iyear, country_txt, city, gname, summary, motive,
    core_text_hit, allegiance_text_hit, claim_hit,
    is_inspired_score, is_inspired_class, inclusion_reason
  )

library(tidyverse)
library(stringr)

# =========================================================
# ANALYTIC DATA CLEANING FOR ISIS-AFFILIATE PROJECT
# Assumes:
#   1) GTD already exists from your import step
#   2) is_inspired_class already exists from your filtering step
#   3) ISFormal_StartYear already exists from your formal-name script
#
# Main output:
#   - GTD_Formal_Incidents_Clean
#   - Affiliate_Country_Year
#   - Affiliate_Year
#   - GTD_Inspired_Unassigned
#
# Optional merge:
#   - affiliation_scores.csv with yearly SCAI / block scores
# =========================================================

# -----------------------------
# 0. HELPERS
# -----------------------------

parse_int_chr <- function(x) {
  suppressWarnings(
    readr::parse_integer(
      x,
      na = c("", "NA", "N/A", "Unknown", "-9", "-99")
    )
  )
}

parse_dbl_chr <- function(x) {
  suppressWarnings(
    readr::parse_double(
      x,
      na = c("", "NA", "N/A", "Unknown", "-9", "-99")
    )
  )
}

parse_01_chr <- function(x) {
  case_when(
    x %in% c("1", "Yes", "yes", "TRUE", "True") ~ 1L,
    x %in% c("0", "No", "no", "FALSE", "False") ~ 0L,
    TRUE ~ NA_integer_
  )
}

# safe weighted mean
wm <- function(x, w) {
  if (all(is.na(x)) || all(is.na(w)) || sum(w, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  weighted.mean(x = x, w = w, na.rm = TRUE)
}

# rescale 0-1 within a vector
minmax01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2])) return(rep(NA_real_, length(x)))
  if (rng[1] == rng[2]) return(rep(1, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

FormalNames <- names(ISFormal_StartYear)

# -----------------------------
# 1. MAIN ANALYTIC SAMPLE
#    Use formal affiliates only
# -----------------------------

GTD_Formal_Incidents_Clean <- GTD |>
  filter(is_inspired_class == "formal_or_affiliate") |>
  mutate(
    # canonical affiliate assignment:
    # first formal-name hit across perpetrator fields
    affiliate_name = coalesce(
      if_else(gname %in% FormalNames, gname, NA_character_),
      if_else(gname2 %in% FormalNames, gname2, NA_character_),
      if_else(gname3 %in% FormalNames, gname3, NA_character_),
      if_else(gsubname %in% FormalNames, gsubname, NA_character_),
      if_else(gsubname2 %in% FormalNames, gsubname2, NA_character_),
      if_else(gsubname3 %in% FormalNames, gsubname3, NA_character_)
    ),

    # time
    year = parse_int_chr(iyear),
    month = parse_int_chr(imonth),
    day = parse_int_chr(iday),

    # location
    country = na_if(country_txt, ""),
    region = na_if(region_txt, ""),
    province = na_if(provstate, ""),
    city_name = na_if(city, ""),

    # claims / success / tactics
    success_num = parse_01_chr(success),
    suicide_num = parse_01_chr(suicide),
    multiple_num = parse_01_chr(multiple),
    claimed_num = parse_01_chr(claimed),
    claim2_num = parse_01_chr(claim2),
    claim3_num = parse_01_chr(claim3),

    # casualties
    nkill_num = parse_dbl_chr(nkill),
    nwound_num = parse_dbl_chr(nwound),

    # target types
    targ1 = parse_int_chr(targtype1),
    targ2 = parse_int_chr(targtype2),
    targ3 = parse_int_chr(targtype3),

    # attack types
    atk1 = parse_int_chr(attacktype1),
    atk2 = parse_int_chr(attacktype2),
    atk3 = parse_int_chr(attacktype3),

    # weapon types / subtypes
    weap1 = parse_int_chr(weaptype1),
    weap2 = parse_int_chr(weaptype2),
    weap3 = parse_int_chr(weaptype3),
    weap4 = parse_int_chr(weaptype4),

    weapsub1 = parse_int_chr(weapsubtype1),
    weapsub2 = parse_int_chr(weapsubtype2),
    weapsub3 = parse_int_chr(weapsubtype3),
    weapsub4 = parse_int_chr(weapsubtype4)
  ) |>
  filter(
    !is.na(affiliate_name),
    !is.na(year),
    !is.na(country)
  ) |>
  rowwise() |>
  mutate(
    # -------------------------
    # DVs: target families
    # -------------------------
    # security / state targets
    target_security_state = as.integer(any(c(targ1, targ2, targ3) %in% c(2, 3, 4, 7), na.rm = TRUE)),

    # civilian / soft targets
    # business, education, media, NGO, private citizens/property,
    # religious institutions, tourists
    target_civilian_soft = as.integer(any(c(targ1, targ2, targ3) %in% c(1, 8, 10, 12, 14, 15, 18), na.rm = TRUE)),

    # religious targets
    target_religious = as.integer(any(c(targ1, targ2, targ3) %in% c(15), na.rm = TRUE)),

    # infrastructure / transport / utilities / airports / maritime / telecom / food-water
    target_infrastructure = as.integer(any(c(targ1, targ2, targ3) %in% c(6, 9, 11, 16, 19, 21), na.rm = TRUE)),

    # civilian private citizens/property only
    target_private_citizens = as.integer(any(c(targ1, targ2, targ3) %in% c(14), na.rm = TRUE)),

    # police target
    target_police = as.integer(any(c(targ1, targ2, targ3) %in% c(3), na.rm = TRUE)),

    # military target
    target_military = as.integer(any(c(targ1, targ2, targ3) %in% c(4), na.rm = TRUE)),

    # government / diplomatic target
    target_government = as.integer(any(c(targ1, targ2, targ3) %in% c(2, 7), na.rm = TRUE)),

    # -------------------------
    # DVs: tactic families
    # -------------------------
    tactic_suicide = as.integer(suicide_num == 1),

    tactic_bombing = as.integer(any(c(atk1, atk2, atk3) %in% c(3), na.rm = TRUE)),
    tactic_armed_assault = as.integer(any(c(atk1, atk2, atk3) %in% c(2), na.rm = TRUE)),
    tactic_unarmed_assault = as.integer(any(c(atk1, atk2, atk3) %in% c(8), na.rm = TRUE)),
    tactic_facility_attack = as.integer(any(c(atk1, atk2, atk3) %in% c(7), na.rm = TRUE)),
    tactic_hostage_kidnap = as.integer(any(c(atk1, atk2, atk3) %in% c(5, 6), na.rm = TRUE)),
    tactic_hijack = as.integer(any(c(atk1, atk2, atk3) %in% c(4), na.rm = TRUE)),

    # explosives:
    # GTD note: chemical weapons delivered via explosive are coded as chemical + explosive subtype
    tactic_explosives = as.integer(
      any(c(weap1, weap2, weap3, weap4) %in% c(6), na.rm = TRUE) |
        any(
          (c(weap1, weap2, weap3, weap4) %in% c(2)) &
            (c(weapsub1, weapsub2, weapsub3, weapsub4) %in% c(30)),
          na.rm = TRUE
        )
    ),

    tactic_firearms = as.integer(any(c(weap1, weap2, weap3, weap4) %in% c(5), na.rm = TRUE)),
    tactic_melee = as.integer(any(c(weap1, weap2, weap3, weap4) %in% c(9), na.rm = TRUE)),
    tactic_incendiary = as.integer(any(c(weap1, weap2, weap3, weap4) %in% c(8), na.rm = TRUE)),

    # complex attack = multiple attack types OR marked multiple incident
    non_missing_attack_types = sum(!is.na(c(atk1, atk2, atk3)) & c(atk1, atk2, atk3) != 9),
    tactic_complex = as.integer(non_missing_attack_types > 1 | multiple_num == 1),

    # claims
    claim_any = as.integer(any(c(claimed_num, claim2_num, claim3_num) == 1, na.rm = TRUE)),

    # casualties
    fatalities = coalesce(nkill_num, 0),
    injuries = coalesce(nwound_num, 0),

    # useful flags
    attack_count = 1L
  ) |>
  ungroup() |>
  mutate(
    # optional outcome families
    target_hard = as.integer(target_security_state == 1 | target_infrastructure == 1),
    target_soft = as.integer(target_civilian_soft == 1),
    lethality_any = as.integer(fatalities > 0)
  )

# -----------------------------
# 2. KEEP PROBABLE / REVIEW CASES SEPARATE
#    Do not force these into affiliate-year panels
# -----------------------------

GTD_Inspired_Unassigned <- GTD |>
  filter(is_inspired_class %in% c("probable_inspired", "manual_review")) |>
  mutate(
    year = parse_int_chr(iyear),
    country = na_if(country_txt, ""),
    region = na_if(region_txt, "")
  )

# -----------------------------
# 3. HOME COUNTRY LOOKUP
# -----------------------------

HomeCountryLookup <- GTD_Formal_Incidents_Clean |>
  count(affiliate_name, country, sort = TRUE) |>
  group_by(affiliate_name) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    affiliate_name,
    home_country = country
  )

# -----------------------------
# 4. COMPETITION MEASURE
#    Use full GTD, not only ISIS sample
# -----------------------------
# This is a broad organizational competition proxy:
# number of OTHER known non-ISIS groups active in the same country-year,
# and number of OTHER non-ISIS attacks in the same country-year.

GenericOrLowInfoGroups <- c(
  "Unknown",
  "Unaffiliated Individual(s)",
  "Jihadi-inspired extremists",
  "Islamist extremists",
  "Muslim extremists"
)

CountryYearCompetition <- GTD |>
  mutate(
    year = parse_int_chr(iyear),
    country = na_if(country_txt, ""),
    gname_clean = na_if(gname, "")
  ) |>
  filter(!is.na(year), !is.na(country)) |>
  mutate(
    known_org = !is.na(gname_clean) & !(gname_clean %in% GenericOrLowInfoGroups),
    formal_is_primary = gname_clean %in% FormalNames
  ) |>
  group_by(country, year) |>
  summarise(
    active_groups_all = n_distinct(gname_clean[known_org]),
    active_groups_nonis = n_distinct(gname_clean[known_org & !formal_is_primary]),
    attacks_all = n(),
    attacks_nonis = sum(!formal_is_primary, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 5. AFFILIATE-COUNTRY-YEAR PANEL
# -----------------------------

Affiliate_Country_Year <- GTD_Formal_Incidents_Clean |>
  group_by(affiliate_name, year, country, region) |>
  summarise(
    attacks = n(),
    fatalities_total = sum(fatalities, na.rm = TRUE),
    injuries_total = sum(injuries, na.rm = TRUE),
    fatalities_mean = mean(fatalities, na.rm = TRUE),
    injuries_mean = mean(injuries, na.rm = TRUE),

    # DVs: target shares
    civilian_soft_share = mean(target_civilian_soft, na.rm = TRUE),
    security_state_share = mean(target_security_state, na.rm = TRUE),
    religious_share = mean(target_religious, na.rm = TRUE),
    infrastructure_share = mean(target_infrastructure, na.rm = TRUE),
    private_citizens_share = mean(target_private_citizens, na.rm = TRUE),
    police_share = mean(target_police, na.rm = TRUE),
    military_share = mean(target_military, na.rm = TRUE),
    government_share = mean(target_government, na.rm = TRUE),
    hard_target_share = mean(target_hard, na.rm = TRUE),
    soft_target_share = mean(target_soft, na.rm = TRUE),

    # DVs: tactic shares
    suicide_share = mean(tactic_suicide, na.rm = TRUE),
    bombing_share = mean(tactic_bombing, na.rm = TRUE),
    armed_assault_share = mean(tactic_armed_assault, na.rm = TRUE),
    unarmed_assault_share = mean(tactic_unarmed_assault, na.rm = TRUE),
    facility_attack_share = mean(tactic_facility_attack, na.rm = TRUE),
    hostage_kidnap_share = mean(tactic_hostage_kidnap, na.rm = TRUE),
    hijack_share = mean(tactic_hijack, na.rm = TRUE),
    explosives_share = mean(tactic_explosives, na.rm = TRUE),
    firearms_share = mean(tactic_firearms, na.rm = TRUE),
    melee_share = mean(tactic_melee, na.rm = TRUE),
    incendiary_share = mean(tactic_incendiary, na.rm = TRUE),
    complex_share = mean(tactic_complex, na.rm = TRUE),

    # controls
    success_share = mean(success_num == 1, na.rm = TRUE),
    claim_share = mean(claim_any == 1, na.rm = TRUE),
    lethality_any_share = mean(lethality_any == 1, na.rm = TRUE),

    .groups = "drop"
  ) |>
  left_join(HomeCountryLookup, by = "affiliate_name") |>
  left_join(CountryYearCompetition, by = c("country", "year")) |>
  group_by(affiliate_name) |>
  mutate(
    # moderator pieces: local embeddedness components
    home_country_ind = as.integer(country == home_country),
    first_year_in_country = min(year, na.rm = TRUE),
    years_active_in_country = year - first_year_in_country + 1
  ) |>
  ungroup() |>
  group_by(affiliate_name, year) |>
  mutate(
    country_share_within_affiliate_year = attacks / sum(attacks, na.rm = TRUE),
    country_hhi_component = country_share_within_affiliate_year^2
  ) |>
  ungroup()

# -----------------------------
# 6. AFFILIATE-YEAR PANEL
#    This is the main paper dataset
# -----------------------------

Affiliate_Year <- Affiliate_Country_Year |>
  group_by(affiliate_name, year) |>
  summarise(
    # size / scope controls
    attacks = sum(attacks, na.rm = TRUE),
    country_spread = n_distinct(country),
    region_spread = n_distinct(region),

    # main DVs: target shares (weighted by attacks in each country)
    civilian_soft_share = wm(civilian_soft_share, attacks),
    security_state_share = wm(security_state_share, attacks),
    religious_share = wm(religious_share, attacks),
    infrastructure_share = wm(infrastructure_share, attacks),
    private_citizens_share = wm(private_citizens_share, attacks),
    police_share = wm(police_share, attacks),
    military_share = wm(military_share, attacks),
    government_share = wm(government_share, attacks),
    hard_target_share = wm(hard_target_share, attacks),
    soft_target_share = wm(soft_target_share, attacks),

    # tactic shares
    suicide_share = wm(suicide_share, attacks),
    bombing_share = wm(bombing_share, attacks),
    armed_assault_share = wm(armed_assault_share, attacks),
    unarmed_assault_share = wm(unarmed_assault_share, attacks),
    facility_attack_share = wm(facility_attack_share, attacks),
    hostage_kidnap_share = wm(hostage_kidnap_share, attacks),
    hijack_share = wm(hijack_share, attacks),
    explosives_share = wm(explosives_share, attacks),
    firearms_share = wm(firearms_share, attacks),
    melee_share = wm(melee_share, attacks),
    incendiary_share = wm(incendiary_share, attacks),
    complex_share = wm(complex_share, attacks),

    # casualties
    fatalities_total = sum(fatalities_total, na.rm = TRUE),
    injuries_total = sum(injuries_total, na.rm = TRUE),
    fatalities_mean = wm(fatalities_mean, attacks),
    injuries_mean = wm(injuries_mean, attacks),

    # controls
    success_share = wm(success_share, attacks),
    claim_share = wm(claim_share, attacks),
    lethality_any_share = wm(lethality_any_share, attacks),

    # moderator pieces: local embeddedness
    home_country_attack_share = sum(attacks[home_country_ind == 1], na.rm = TRUE) / sum(attacks, na.rm = TRUE),
    country_concentration_hhi = sum(country_hhi_component, na.rm = TRUE),
    years_active_in_country_mean = wm(years_active_in_country, attacks),

    # competition: weighted local competition exposure
    competition_groups_nonis = wm(active_groups_nonis, attacks),
    competition_attacks_nonis = wm(attacks_nonis, attacks),
    competition_groups_all = wm(active_groups_all, attacks),
    competition_attacks_all = wm(attacks_all, attacks),

    .groups = "drop"
  ) |>
  group_by(affiliate_name) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    years_since_first_observed = year - min(year, na.rm = TRUE) + 1,

    # lagged controls / IV-ready fields
    lag_attacks = lag(attacks),
    lag_civilian_soft_share = lag(civilian_soft_share),
    lag_security_state_share = lag(security_state_share),
    lag_suicide_share = lag(suicide_share),
    lag_explosives_share = lag(explosives_share),
    lag_competition_groups_nonis = lag(competition_groups_nonis),
    lag_competition_attacks_nonis = lag(competition_attacks_nonis),
    lag_home_country_attack_share = lag(home_country_attack_share),
    lag_country_concentration_hhi = lag(country_concentration_hhi),
    lag_years_active_in_country_mean = lag(years_active_in_country_mean)
  ) |>
  ungroup()

# -----------------------------
# 7. OPTIONAL: LOCAL EMBEDDEDNESS INDEX
#    I would keep the components too.
# -----------------------------

Affiliate_Year <- Affiliate_Year |>
  group_by(affiliate_name) |>
  mutate(
    home_country_attack_share_01 = minmax01(home_country_attack_share),
    country_concentration_hhi_01 = minmax01(country_concentration_hhi),
    years_active_in_country_mean_01 = minmax01(years_active_in_country_mean),
    local_embeddedness_index =
      (home_country_attack_share_01 +
         country_concentration_hhi_01 +
         years_active_in_country_mean_01) / 3
  ) |>
  ungroup() |>
  group_by(affiliate_name) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    lag_local_embeddedness_index = lag(local_embeddedness_index)
  ) |>
  ungroup()

# -----------------------------
# 8. OPTIONAL: MERGE YEARLY AFFILIATION SCORES
#    Replace filename/column names as needed
# -----------------------------
# Expected minimum columns:
#   affiliate_name
#   year
#   scai
#
# Optional block columns:
#   formal_status_score
#   admin_control_score
#   media_integration_score
#   finance_integration_score
#   personnel_integration_score
#   operational_integration_score
#   governance_replication_score

# affiliation_scores <- read_csv(
#   "affiliation_scores.csv",
#   col_types = cols(.default = col_character())
# ) |>
#   mutate(
#     affiliate_name = coalesce(affiliate_name, group_name),
#     year = parse_int_chr(coalesce(as.character(year), as.character(snapshot_year))),
#     scai = parse_dbl_chr(scai),
#     formal_status_score = parse_dbl_chr(formal_status_score),
#     admin_control_score = parse_dbl_chr(admin_control_score),
#     media_integration_score = parse_dbl_chr(media_integration_score),
#     finance_integration_score = parse_dbl_chr(finance_integration_score),
#     personnel_integration_score = parse_dbl_chr(personnel_integration_score),
#     operational_integration_score = parse_dbl_chr(operational_integration_score),
#     governance_replication_score = parse_dbl_chr(governance_replication_score)
#   ) |>
#   select(
#     affiliate_name, year, scai,
#     formal_status_score, admin_control_score, media_integration_score,
#     finance_integration_score, personnel_integration_score,
#     operational_integration_score, governance_replication_score
#   )
#
# Affiliate_Year <- Affiliate_Year |>
#   left_join(affiliation_scores, by = c("affiliate_name", "year")) |>
#   group_by(affiliate_name) |>
#   arrange(year, .by_group = TRUE) |>
#   mutate(
#     scai_lag = lag(scai),
#     media_integration_lag = lag(media_integration_score),
#     finance_integration_lag = lag(finance_integration_score),
#     admin_control_lag = lag(admin_control_score)
#   ) |>
#   ungroup()

# -----------------------------
# 9. SAVE OUTPUTS
# -----------------------------

write_csv(GTD_Formal_Incidents_Clean, "gtd_formal_incidents_clean.csv")
write_csv(Affiliate_Country_Year, "affiliate_country_year_panel.csv")
write_csv(Affiliate_Year, "affiliate_year_panel.csv")
write_csv(GTD_Inspired_Unassigned, "gtd_inspired_unassigned.csv")

# -----------------------------
# 10. QUICK CHECKS
# -----------------------------

Affiliate_Year |>
  count(affiliate_name, sort = TRUE)

Affiliate_Year |>
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_affiliates = n_distinct(affiliate_name),
    n_rows = n()
  )

Affiliate_Year |>
  select(
    affiliate_name, year, attacks,
    civilian_soft_share, security_state_share,
    suicide_share, explosives_share,
    home_country_attack_share, country_concentration_hhi,
    competition_groups_nonis, competition_attacks_nonis
  ) |>
  print(n = 50)
