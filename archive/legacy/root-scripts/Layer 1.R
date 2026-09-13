# ============================================================
# ISIS ATTACK CONCENTRATION VS POPULATION
# FIGURES:
# 1. LORENZ-STYLE CONCENTRATION CURVE
# 2. OBSERVED VS EXPECTED DISTRICT BURDEN RATIO
# 3. DISTRICT OVERBURDEN SKYLINE PLOT
# ============================================================

# ============================================================
# 0. LIBRARIES
# ============================================================

library(tidyverse) # Load tidyverse for data wrangling and ggplot2.
library(sf) # Load sf for vector spatial data.
library(terra) # Load terra for raster population extraction.
library(janitor) # Load janitor for clean column names.
library(scales) # Load scales for axis labels.
library(glue) # Load glue for readable labels.
library(ggrepel) # Load ggrepel for cleaner text labels.


# ============================================================
# 1. FILE PATHS
# ============================================================

gtd_path <- "data/raw/csv/GTD_A.csv" # Set the GTD CSV path.

ethnicity_zip_path <- "data/raw/Iraq Ethnicity.zip" # Set the ESOC ethnicity ZIP path.

ethnicity_unzip_dir <- "data/raw/iraq_ethnicity_unzipped" # Set the ESOC unzip folder.

landscan_path <- "data/raw/LandScan_2008.tif" # Set the LandScan raster path.

figure_dir <- "outputs/figures" # Set the output figure folder.

table_dir <- "outputs/tables" # Set the output table folder.

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE) # Create the figure folder if needed.

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE) # Create the table folder if needed.


# ============================================================
# 2. BASIC SAFETY CHECKS
# ============================================================

stopifnot(file.exists(gtd_path)) # Stop if the GTD CSV is missing.

stopifnot(file.exists(ethnicity_zip_path)) # Stop if the ESOC ZIP file is missing.

stopifnot(file.exists(landscan_path)) # Stop if the LandScan raster is missing.


# ============================================================
# 3. UNZIP ESOC ETHNICITY DATA IF NEEDED
# ============================================================

dir.create(ethnicity_unzip_dir, recursive = TRUE, showWarnings = FALSE) # Create the unzip directory.

existing_unzipped_files <- list.files(ethnicity_unzip_dir, recursive = TRUE) # Check existing unzipped files.

if (length(existing_unzipped_files) == 0) {
  # Only unzip if the folder appears empty.

  unzip(ethnicity_zip_path, exdir = ethnicity_unzip_dir) # Unzip the ESOC ethnicity files.
}


# ============================================================
# 4. FIND THE ESOC SHAPEFILE
# ============================================================

ethnicity_shp_candidates <- list.files(
  # Find all shapefiles inside the ESOC folder.
  path = ethnicity_unzip_dir, # Search inside the unzipped ethnicity folder.
  pattern = "\\.shp$", # Keep only shapefile paths.
  recursive = TRUE, # Search subfolders too.
  full.names = TRUE # Return full file paths.
)

stopifnot(length(ethnicity_shp_candidates) > 0) # Stop if no shapefile exists.

ethnicity_shp_path <- ethnicity_shp_candidates[1] # Use the first shapefile found.

message(glue("Using ESOC shapefile: {ethnicity_shp_path}")) # Print the shapefile being used.


# ============================================================
# 5. IMPORT ESOC DISTRICT POLYGONS
# ============================================================

districts_raw <- st_read(
  # Read the ESOC district polygons.
  ethnicity_shp_path, # Use the detected shapefile path.
  quiet = TRUE # Suppress verbose read messages.
) |>
  clean_names() # Clean all column names.


districts_raw <- st_make_valid(districts_raw) # Repair invalid geometries if present.


# ============================================================
# 6. BUILD DISTRICT IDENTIFIERS
# ============================================================

district_columns <- names(districts_raw) # Store the district column names.

districts <- districts_raw |> # Start district processing.
  mutate(
    # Create standardized district fields.

    province_name = coalesce(
      # Build a province field from likely column names.
      !!!syms(intersect(
        c(
          "province",
          "prov",
          "prov_name",
          "governorat",
          "governorate",
          "admin1",
          "adm1_name",
          "name_1"
        ),
        district_columns
      )),
      NA_character_
    ),

    district_name = coalesce(
      # Build a district field from likely column names.
      !!!syms(intersect(
        c(
          "district",
          "dist",
          "dist_name",
          "qadha",
          "qadaa",
          "admin2",
          "adm2_name",
          "name_2",
          "name"
        ),
        district_columns
      )),
      NA_character_
    ),

    district_id = row_number() # Create a fallback unique district ID.
  ) |>
  mutate(
    # Clean the district labels.

    province_name = if_else(
      # Replace missing province names.
      is.na(province_name) | province_name == "", # Check for missing or blank province values.
      "Unknown province", # Use a fallback province label.
      as.character(province_name) # Otherwise keep the province label.
    ),

    district_name = if_else(
      # Replace missing district names.
      is.na(district_name) | district_name == "", # Check for missing or blank district values.
      paste0("District ", district_id), # Use fallback district label.
      as.character(district_name) # Otherwise keep the district label.
    ),

    district_label = paste(province_name, district_name, sep = " / ") # Create readable district label.
  )


# ============================================================
# 7. DETECT OPTIONAL SUNNI / SHIA / KURDISH COLUMNS
# ============================================================

non_geometry_names <- names(st_drop_geometry(districts)) # Store non-geometry column names.

sunni_col <- non_geometry_names[str_detect(non_geometry_names, "sunni|sun")] |>
  first() # Detect likely Sunni column.

shia_col <- non_geometry_names[str_detect(
  non_geometry_names,
  "shia|shiite|shii"
)] |>
  first() # Detect likely Shia column.

kurd_col <- non_geometry_names[str_detect(non_geometry_names, "kurd")] |>
  first() # Detect likely Kurdish column.


districts <- districts |> # Add optional demographic fields.
  mutate(
    # Create standardized demographic variables.

    pct_sunni_raw = if (!is.na(sunni_col)) {
      as.numeric(.data[[sunni_col]])
    } else {
      NA_real_
    }, # Extract Sunni value.

    pct_shia_raw = if (!is.na(shia_col)) {
      as.numeric(.data[[shia_col]])
    } else {
      NA_real_
    }, # Extract Shia value.

    pct_kurd_raw = if (!is.na(kurd_col)) {
      as.numeric(.data[[kurd_col]])
    } else {
      NA_real_
    } # Extract Kurdish value.
  ) |>
  mutate(
    # Standardize demographic percentages.

    pct_sunni = case_when(
      # Convert Sunni values to 0-100 scale.
      is.na(pct_sunni_raw) ~ NA_real_, # Keep missing as missing.
      pct_sunni_raw <= 1 ~ pct_sunni_raw * 100, # Convert proportion to percent.
      TRUE ~ pct_sunni_raw # Keep already-percent values.
    ),

    pct_shia = case_when(
      # Convert Shia values to 0-100 scale.
      is.na(pct_shia_raw) ~ NA_real_, # Keep missing as missing.
      pct_shia_raw <= 1 ~ pct_shia_raw * 100, # Convert proportion to percent.
      TRUE ~ pct_shia_raw # Keep already-percent values.
    ),

    pct_kurd = case_when(
      # Convert Kurdish values to 0-100 scale.
      is.na(pct_kurd_raw) ~ NA_real_, # Keep missing as missing.
      pct_kurd_raw <= 1 ~ pct_kurd_raw * 100, # Convert proportion to percent.
      TRUE ~ pct_kurd_raw # Keep already-percent values.
    )
  ) |>
  mutate(
    # Create a dominant demographic category.

    dominant_group = case_when(
      # Assign dominant group using available columns.
      !is.na(pct_sunni) &
        pct_sunni >= pct_shia &
        pct_sunni >= pct_kurd ~ "Sunni plurality",
      !is.na(pct_shia) &
        pct_shia >= pct_sunni &
        pct_shia >= pct_kurd ~ "Shia plurality",
      !is.na(pct_kurd) &
        pct_kurd >= pct_sunni &
        pct_kurd >= pct_shia ~ "Kurdish plurality",
      TRUE ~ "Unknown / unavailable"
    )
  )


# ============================================================
# 8. IMPORT LANDSCAN POPULATION RASTER
# ============================================================

landscan <- rast(landscan_path) # Read the LandScan raster.

districts_for_raster <- st_transform(
  # Transform districts to raster CRS.
  districts, # Use ESOC district polygons.
  crs(landscan) # Match LandScan CRS.
)

districts_vect <- vect(districts_for_raster) # Convert sf polygons to terra vector format.


# ============================================================
# 9. EXTRACT DISTRICT POPULATION FROM LANDSCAN
# ============================================================

district_population_raw <- terra::extract(
  # Extract raster population by district.
  landscan, # Use the LandScan population raster.
  districts_vect, # Use district polygons.
  fun = sum, # Sum population cells inside each district.
  na.rm = TRUE # Ignore missing raster cells.
)

district_population <- district_population_raw |> # Start cleaning extracted population.
  as_tibble() |> # Convert extraction result to tibble.
  clean_names() |> # Clean column names.
  rename(district_id = id) |> # Rename terra extraction ID.
  rename(population = 2) |> # Rename the population sum column.
  mutate(population = as.numeric(population)) # Ensure population is numeric.


districts_pop <- districts |> # Attach district population to polygons.
  left_join(district_population, by = "district_id") |> # Join population by district ID.
  mutate(
    # Clean population values.

    population = replace_na(population, 0), # Replace missing population with zero.

    population = if_else(population < 0, 0, population) # Replace impossible negative values with zero.
  )


# ============================================================
# 10. IMPORT AND CLEAN GTD DATA
# ============================================================

gtd_raw <- readr::read_csv(
  # Read the GTD CSV.
  gtd_path, # Use the GTD file path.
  show_col_types = FALSE # Suppress column type messages.
) |>
  clean_names() # Clean GTD column names.


gtd <- gtd_raw |> # Start GTD cleaning.
  mutate(
    # Standardize key fields.

    eventid = as.character(eventid), # Ensure event ID is character.

    iyear = as.integer(iyear), # Ensure year is integer.

    imonth = as.integer(imonth), # Ensure month is integer.

    iday = as.integer(iday), # Ensure day is integer.

    latitude = as.numeric(latitude), # Ensure latitude is numeric.

    longitude = as.numeric(longitude), # Ensure longitude is numeric.

    gname = as.character(gname), # Ensure perpetrator name is character.

    nkill = as.numeric(nkill), # Ensure deaths are numeric.

    nwound = as.numeric(nwound) # Ensure wounds are numeric.
  ) |>
  mutate(
    # Create safer casualty variables.

    nkill = replace_na(nkill, 0), # Replace missing deaths with zero.

    nwound = replace_na(nwound, 0), # Replace missing wounds with zero.

    casualties = nkill + nwound # Create combined casualty count.
  )


# ============================================================
# 11. FILTER TO ISIS / ISLAMIC STATE ATTACKS
# ============================================================

isis_attacks <- gtd |> # Start ISIS filtering.
  filter(
    # Keep Islamic State-related perpetrator records.

    str_detect(
      # Search perpetrator names.
      str_to_lower(gname), # Convert perpetrator name to lowercase.
      "islamic state|isis|isil|daesh|islamic state of iraq|islamic state of iraq and the levant"
    )
  ) |>
  filter(!is.na(latitude), !is.na(longitude)) |> # Remove attacks without coordinates.
  filter(!is.na(eventid)) # Remove attacks without event IDs.


# ============================================================
# 12. CONVERT ISIS ATTACKS TO SPATIAL POINTS
# ============================================================

isis_points <- isis_attacks |> # Start spatial point conversion.
  st_as_sf(
    # Convert GTD rows to sf points.
    coords = c("longitude", "latitude"), # Use longitude and latitude columns.
    crs = 4326, # GTD coordinates are usually WGS84.
    remove = FALSE # Keep longitude and latitude columns.
  )


isis_points <- st_transform(isis_points, st_crs(districts_pop)) # Transform attacks to ESOC district CRS.


# ============================================================
# 13. SPATIALLY JOIN ISIS ATTACKS TO DISTRICTS
# ============================================================

isis_joined <- st_join(
  # Join attack points to district polygons.
  isis_points, # Use ISIS attack points.
  districts_pop |> dplyr::select(district_id, district_label), # Keep only district ID and label from polygons.
  join = st_within, # Assign point to containing polygon.
  left = FALSE # Drop points that do not fall in any district.
)


district_attack_counts <- isis_joined |> # Start district attack count table.
  st_drop_geometry() |> # Remove point geometry.
  count(district_id, name = "observed_attacks") # Count attacks by district.


# ============================================================
# 14. BUILD DISTRICT-LEVEL OBSERVED / EXPECTED TABLE
# ============================================================

district_burden <- districts_pop |> # Start district burden table.
  st_drop_geometry() |> # Remove polygon geometry.
  dplyr::select(
    # Keep useful variables.
    district_id, # Keep district ID.
    province_name, # Keep province name.
    district_name, # Keep district name.
    district_label, # Keep district label.
    population, # Keep LandScan population.
    pct_sunni, # Keep Sunni percentage if available.
    pct_shia, # Keep Shia percentage if available.
    pct_kurd, # Keep Kurdish percentage if available.
    dominant_group # Keep dominant group label.
  ) |>
  left_join(district_attack_counts, by = "district_id") |> # Attach observed attack counts.
  mutate(
    # Fill and calculate burden measures.

    observed_attacks = replace_na(observed_attacks, 0), # Replace missing attack counts with zero.

    total_population = sum(population, na.rm = TRUE), # Calculate total population across districts.

    total_attacks = sum(observed_attacks, na.rm = TRUE), # Calculate total observed ISIS attacks.

    population_share = population / total_population, # Calculate each district's population share.

    attack_share = observed_attacks / total_attacks, # Calculate each district's attack share.

    expected_attacks = total_attacks * population_share, # Calculate expected attacks under population-proportional null.

    burden_ratio = observed_attacks / expected_attacks, # Calculate observed divided by expected attacks.

    burden_ratio = if_else(
      # Handle zero expected attack edge cases.
      expected_attacks <= 0 & observed_attacks <= 0, # Check zero-population and zero-attack districts.
      NA_real_, # Set undefined zero-over-zero cases to missing.
      burden_ratio # Otherwise keep burden ratio.
    ),

    burden_ratio = if_else(
      # Handle positive attacks in zero-population districts.
      expected_attacks <= 0 & observed_attacks > 0, # Check impossible positive-over-zero cases.
      Inf, # Set to infinite burden.
      burden_ratio # Otherwise keep burden ratio.
    ),

    excess_attacks = observed_attacks - expected_attacks, # Calculate observed minus expected attacks.

    attack_rate_per_100k = observed_attacks / population * 100000, # Calculate population-adjusted attack rate.

    attack_rate_per_100k = if_else(
      # Handle zero-population attack rate edge cases.
      population <= 0, # Check zero population.
      NA_real_, # Set rate to missing.
      attack_rate_per_100k # Otherwise keep rate.
    )
  ) |>
  arrange(desc(burden_ratio)) # Sort by burden ratio.


write_csv(
  # Save the district burden table.
  district_burden, # Use the burden table.
  file.path(table_dir, "district_attack_population_burden.csv") # Save to outputs.
)


# ============================================================
# 15. CREATE THE LORENZ-STYLE CONCENTRATION DATA
# ============================================================
# ============================================================
# CREATE ORIGIN ROW FOR THE LORENZ CURVE
# ============================================================

lorenz_origin <- tibble(
  district_id = NA_integer_, # No district ID for the origin point.
  district_label = "Origin", # Label the origin point.
  population = 0, # Zero population at origin.
  observed_attacks = 0, # Zero attacks at origin.
  attack_rate_per_100k = NA_real_, # No attack rate at origin.
  cumulative_population_share = 0, # Start x-axis at zero.
  cumulative_attack_share = 0 # Start y-axis at zero.
)


# ============================================================
# CREATE LORENZ-STYLE CONCENTRATION DATA
# ============================================================

lorenz_data <- district_burden |> # Start with district-level burden data.
  filter(population > 0) |> # Keep only districts with positive population.
  filter(!is.na(observed_attacks)) |> # Keep only rows with valid observed attack counts.
  arrange(desc(attack_rate_per_100k)) |> # Sort from highest to lowest attack rate.
  mutate(
    cumulative_population = cumsum(population), # Calculate cumulative population.

    cumulative_attacks = cumsum(observed_attacks), # Calculate cumulative ISIS attacks.

    cumulative_population_share = cumulative_population / sum(population), # Convert population to cumulative share.

    cumulative_attack_share = cumulative_attacks / sum(observed_attacks) # Convert attacks to cumulative share.
  ) |>
  dplyr::select(
    district_id, # Keep district ID.
    district_label, # Keep readable district label.
    population, # Keep district population.
    observed_attacks, # Keep observed ISIS attacks.
    attack_rate_per_100k, # Keep ISIS attack rate per 100,000 residents.
    cumulative_population_share, # Keep cumulative population share.
    cumulative_attack_share # Keep cumulative attack share.
  ) |>
  bind_rows(lorenz_origin) |> # Add the origin row without using the magrittr dot.
  arrange(cumulative_population_share) # Put the origin at the beginning of the curve.

top_lorenz_label <- lorenz_data |> # Create annotation data for concentration claim.
  filter(cumulative_population_share >= 0.20) |> # Find point where cumulative population reaches 20%.
  slice(1) # Keep first row after 20% population.


# ============================================================
# 16. FIGURE 1: LORENZ-STYLE CONCENTRATION CURVE
# ============================================================

p_lorenz <- ggplot(
  lorenz_data,
  aes(x = cumulative_population_share, y = cumulative_attack_share)
) + # Start Lorenz plot.

  geom_abline(
    # Add proportional-distribution reference line.
    intercept = 0, # Set intercept to zero.
    slope = 1, # Set slope to one.
    linewidth = 0.7, # Use modest line width.
    linetype = "dashed", # Use dashed line.
    color = "gray45" # Use neutral gray reference line.
  ) +

  geom_area(
    # Add soft concentration area.
    alpha = 0.18, # Use transparent fill.
    fill = "gray30" # Use neutral dark gray fill.
  ) +

  geom_line(
    # Draw observed concentration curve.
    linewidth = 1.2, # Make observed line prominent.
    color = "black" # Use black line.
  ) +

  geom_point(
    # Add point at 20% population threshold.
    data = top_lorenz_label, # Use the annotation point.
    aes(x = cumulative_population_share, y = cumulative_attack_share), # Set annotation coordinates.
    size = 3, # Use visible point size.
    color = "black" # Use black point.
  ) +

  geom_label_repel(
    # Add readable annotation label.
    data = top_lorenz_label, # Use the annotation point.
    aes(
      # Set label aesthetics.
      x = cumulative_population_share, # Set x-position.
      y = cumulative_attack_share, # Set y-position.
      label = glue(
        # Create concentration label.
        "Top 20% of population-ranked exposure\naccounts for {percent(cumulative_attack_share, accuracy = 1)} of ISIS attacks"
      )
    ),
    size = 3.6, # Set label size.
    label.size = 0.25, # Set label border.
    fill = "white", # Use white label fill.
    min.segment.length = 0 # Always draw connector if useful.
  ) +

  scale_x_continuous(
    # Format x-axis.
    labels = percent_format(accuracy = 1), # Show population share as percent.
    limits = c(0, 1), # Force full 0-100% range.
    expand = expansion(mult = c(0, 0.01)) # Reduce extra whitespace.
  ) +

  scale_y_continuous(
    # Format y-axis.
    labels = percent_format(accuracy = 1), # Show attack share as percent.
    limits = c(0, 1), # Force full 0-100% range.
    expand = expansion(mult = c(0, 0.01)) # Reduce extra whitespace.
  ) +

  labs(
    # Add plot labels.
    title = "ISIS Attacks Were More Concentrated Than Population",
    subtitle = "Districts are ordered from highest to lowest ISIS attack rate per 100,000 residents",
    x = "Cumulative share of population",
    y = "Cumulative share of ISIS attacks",
    caption = "Dashed line shows the population-proportional null expectation. Curve above the line indicates concentration of attacks among a smaller population share."
  ) +

  theme_classic(base_size = 13) + # Use clean publication-style theme.

  theme(
    # Customize theme.
    plot.title = element_text(face = "bold", size = 17), # Bold title.
    plot.subtitle = element_text(size = 11, color = "gray30"), # Smaller subtitle.
    plot.caption = element_text(size = 9, color = "gray35"), # Smaller caption.
    axis.title = element_text(face = "bold"), # Bold axis titles.
    panel.grid.major.y = element_line(color = "gray90"), # Add light horizontal gridlines.
    panel.grid.major.x = element_line(color = "gray92") # Add light vertical gridlines.
  )


ggsave(
  # Save Lorenz plot.
  filename = file.path(figure_dir, "fig1_lorenz_attack_concentration.png"), # Set output path.
  plot = p_lorenz, # Save Lorenz plot object.
  width = 9, # Set figure width.
  height = 7, # Set figure height.
  dpi = 320 # Set high resolution.
)


# ============================================================
# 17. PREPARE DATA FOR OBSERVED VS EXPECTED BURDEN RATIO
# ============================================================

burden_plot_data <- district_burden |> # Start burden ratio plotting data.
  filter(population > 0) |> # Keep districts with population.
  filter(expected_attacks > 0) |> # Keep districts with positive expected attacks.
  filter(observed_attacks > 0) |> # Keep districts with at least one observed attack.
  filter(is.finite(burden_ratio)) |> # Remove infinite burden values.
  arrange(desc(burden_ratio)) |> # Sort by burden ratio.
  slice_head(n = 30) |> # Keep top 30 overburdened districts.
  mutate(
    # Prepare plotting labels.

    district_label = fct_reorder(district_label, burden_ratio), # Reorder districts by burden ratio.

    burden_direction = case_when(
      # Create burden category.
      burden_ratio >= 2 ~ "At least 2x expected",
      burden_ratio >= 1 ~ "Above expected",
      burden_ratio < 1 ~ "Below expected",
      TRUE ~ "Unknown"
    )
  )


# ============================================================
# 18. FIGURE 2: OBSERVED VS EXPECTED DISTRICT BURDEN RATIO
# ============================================================

p_burden <- ggplot(
  burden_plot_data,
  aes(x = burden_ratio, y = district_label)
) + # Start burden plot.

  geom_vline(
    # Add population-proportional reference line.
    xintercept = 1, # Set reference at observed = expected.
    linewidth = 0.8, # Make reference line visible.
    linetype = "dashed", # Use dashed line.
    color = "gray35" # Use neutral gray.
  ) +

  geom_segment(
    # Add lollipop stems.
    aes(
      # Set segment aesthetics.
      x = 1, # Start each segment at expected line.
      xend = burden_ratio, # End at observed/expected ratio.
      y = district_label, # Start district position.
      yend = district_label # End district position.
    ),
    linewidth = 0.8, # Use visible stem width.
    color = "gray65" # Use neutral gray stems.
  ) +

  geom_point(
    # Add burden ratio points.
    aes(
      # Set point aesthetics.
      size = observed_attacks, # Scale point size by observed attacks.
      fill = pct_sunni # Fill points by percent Sunni if available.
    ),
    shape = 21, # Use filled circle with border.
    color = "black", # Use black point border.
    alpha = 0.9 # Slight transparency.
  ) +

  scale_x_continuous(
    # Format x-axis.
    trans = "log10", # Use log scale for burden ratio.
    breaks = c(1, 2, 5, 10, 20, 50, 100), # Use interpretable breaks.
    labels = c("1x", "2x", "5x", "10x", "20x", "50x", "100x") # Label as multiples.
  ) +

  scale_size_continuous(
    # Format point size scale.
    range = c(2.5, 9), # Set point size range.
    labels = comma_format(), # Use comma labels.
    name = "Observed\nattacks" # Set size legend title.
  ) +

  scale_fill_gradient(
    # Format Sunni percentage fill.
    low = "white", # Use white for lower Sunni percentage.
    high = "black", # Use black for higher Sunni percentage.
    na.value = "gray70", # Use gray when Sunni data unavailable.
    limits = c(0, 100), # Use 0-100 percent scale.
    labels = label_percent(scale = 1), # Label as percent.
    name = "Sunni\npopulation"
  ) +

  labs(
    # Add plot labels.
    title = "Districts Where ISIS Attacks Exceeded Population-Based Expectations",
    subtitle = "Observed attacks divided by expected attacks under a population-proportional null model",
    x = "Observed / expected ISIS attacks",
    y = NULL,
    caption = "The dashed line marks observed = expected. Values greater than 1 indicate more ISIS attacks than expected based only on population share."
  ) +

  theme_classic(base_size = 13) + # Use clean publication theme.

  theme(
    # Customize theme.
    plot.title = element_text(face = "bold", size = 17), # Bold title.
    plot.subtitle = element_text(size = 11, color = "gray30"), # Smaller subtitle.
    plot.caption = element_text(size = 9, color = "gray35"), # Smaller caption.
    axis.title.x = element_text(face = "bold"), # Bold x-axis title.
    axis.text.y = element_text(size = 9), # Smaller district labels.
    legend.position = "right", # Put legend on right.
    panel.grid.major.x = element_line(color = "gray90") # Add light vertical gridlines.
  )


ggsave(
  # Save burden ratio plot.
  filename = file.path(figure_dir, "fig2_observed_expected_burden_ratio.png"), # Set output path.
  plot = p_burden, # Save burden plot object.
  width = 11, # Set figure width.
  height = 8.5, # Set figure height.
  dpi = 320 # Set high resolution.
)


# ============================================================
# 19. PREPARE DATA FOR DISTRICT OVERBURDEN SKYLINE PLOT
# ============================================================

skyline_data <- district_burden |> # Start skyline plotting data.
  filter(population > 0) |> # Keep districts with positive population.
  filter(expected_attacks > 0) |> # Keep districts with positive expected attacks.
  filter(is.finite(burden_ratio)) |> # Remove infinite ratios.
  mutate(
    # Handle zero-attack districts for plotting.

    burden_ratio_plot = if_else(
      # Create plotting version of burden ratio.
      observed_attacks == 0, # Check zero observed attacks.
      0, # Let zero-attack districts sit at zero.
      burden_ratio # Otherwise use actual burden ratio.
    )
  ) |>
  arrange(desc(burden_ratio_plot)) |> # Order districts by overburden.
  mutate(
    # Create population-width rectangle positions.

    population_width = population / sum(population), # Convert population to total-share width.

    xmin = lag(cumsum(population_width), default = 0), # Start rectangle at previous cumulative population.

    xmax = cumsum(population_width), # End rectangle at current cumulative population.

    xmid = (xmin + xmax) / 2, # Calculate rectangle midpoint.

    skyline_label = if_else(
      # Label only major overburdened districts.
      row_number() <= 8 & burden_ratio_plot > 1, # Keep labels for top 8 meaningful districts.
      district_label, # Use district label.
      NA_character_ # Otherwise no label.
    )
  )


skyline_label_data <- skyline_data |> # Create label data for skyline plot.
  filter(!is.na(skyline_label)) # Keep labeled districts only.


skyline_y_limit <- skyline_data |> # Create reasonable y-axis upper limit.
  filter(burden_ratio_plot > 0) |> # Keep positive burden ratios.
  summarize(ymax = quantile(burden_ratio_plot, 0.98, na.rm = TRUE)) |> # Use 98th percentile to avoid extreme compression.
  pull(ymax) # Pull value.


skyline_y_limit <- max(skyline_y_limit, 2, na.rm = TRUE) # Ensure y-limit is at least 2.


# ============================================================
# 20. FIGURE 3: DISTRICT OVERBURDEN SKYLINE PLOT
# ============================================================

p_skyline <- ggplot(skyline_data) + # Start skyline plot.

  geom_hline(
    # Add observed = expected reference line.
    yintercept = 1, # Set reference at 1.
    linewidth = 0.8, # Use visible width.
    linetype = "dashed", # Use dashed line.
    color = "gray30" # Use neutral gray.
  ) +

  geom_rect(
    # Draw population-width district rectangles.
    aes(
      # Set rectangle aesthetics.
      xmin = xmin, # Set rectangle left boundary.
      xmax = xmax, # Set rectangle right boundary.
      ymin = 0, # Start every rectangle at zero.
      ymax = pmin(burden_ratio_plot, skyline_y_limit), # Cap extreme heights for readability.
      fill = pct_sunni # Fill by percent Sunni if available.
    ),
    color = "gray25", # Use dark rectangle borders.
    linewidth = 0.08, # Use thin borders.
    alpha = 0.95 # Use mostly opaque bars.
  ) +

  geom_text_repel(
    # Add labels for top overburdened districts.
    data = skyline_label_data, # Use label data.
    aes(
      # Set label aesthetics.
      x = xmid, # Put labels at rectangle midpoint.
      y = pmin(burden_ratio_plot, skyline_y_limit), # Put labels at capped rectangle height.
      label = skyline_label # Use district label.
    ),
    size = 3.1, # Set label size.
    min.segment.length = 0, # Always draw segment if needed.
    max.overlaps = Inf, # Allow all selected labels.
    box.padding = 0.35, # Add label padding.
    segment.color = "gray40" # Use gray connector lines.
  ) +

  annotate(
    # Add reference-line annotation.
    geom = "label", # Use label annotation.
    x = 0.82, # Place near right side.
    y = 1.08, # Place just above reference line.
    label = "Observed = expected by population", # Explain dashed line.
    size = 3.4, # Set annotation size.
    fill = "white", # Use white background.
    label.size = 0.2 # Use thin label border.
  ) +

  scale_x_continuous(
    # Format x-axis.
    labels = percent_format(accuracy = 1), # Show cumulative population share.
    expand = expansion(mult = c(0, 0)) # Remove x padding.
  ) +

  scale_y_continuous(
    # Format y-axis.
    breaks = pretty_breaks(n = 8), # Use readable y breaks.
    labels = function(x) paste0(x, "x"), # Label burden as multiples.
    limits = c(0, skyline_y_limit * 1.12), # Add top room for labels.
    expand = expansion(mult = c(0, 0.03)) # Add slight top padding.
  ) +

  scale_fill_gradient(
    # Format Sunni fill gradient.
    low = "white", # Use white for low Sunni percentage.
    high = "black", # Use black for high Sunni percentage.
    na.value = "gray70", # Use gray if missing.
    limits = c(0, 100), # Set 0-100 scale.
    labels = label_percent(scale = 1), # Label as percent.
    name = "Sunni\npopulation"
  ) +

  labs(
    # Add plot labels.
    title = "District Overburden Skyline: Population Width vs ISIS Attack Burden",
    subtitle = "Each district's width equals its population share; height equals observed / expected ISIS attacks",
    x = "Cumulative population share across districts ordered by attack overburden",
    y = "Observed / expected ISIS attacks",
    caption = "Bars above 1x received more ISIS attacks than expected under a population-proportional null model. Extreme values are visually capped at the 98th percentile for readability."
  ) +

  theme_classic(base_size = 13) + # Use clean publication theme.

  theme(
    # Customize theme.
    plot.title = element_text(face = "bold", size = 17), # Bold title.
    plot.subtitle = element_text(size = 11, color = "gray30"), # Smaller subtitle.
    plot.caption = element_text(size = 9, color = "gray35"), # Smaller caption.
    axis.title = element_text(face = "bold"), # Bold axis titles.
    axis.text.x = element_text(size = 10), # Set x-axis text size.
    axis.text.y = element_text(size = 10), # Set y-axis text size.
    legend.position = "right", # Put legend on right.
    panel.grid.major.y = element_line(color = "gray90") # Add light horizontal gridlines.
  )


ggsave(
  # Save skyline plot.
  filename = file.path(figure_dir, "fig3_district_overburden_skyline.png"), # Set output path.
  plot = p_skyline, # Save skyline plot object.
  width = 12, # Set figure width.
  height = 7.5, # Set figure height.
  dpi = 320 # Set high resolution.
)


# ============================================================
# 21. PRINT SUMMARY DIAGNOSTICS
# ============================================================

message("============================================================") # Print divider.

message("DISTRICT BURDEN TABLE CREATED") # Print status.

message(glue("Total districts: {nrow(district_burden)}")) # Print district count.

message(glue(
  "Total population: {comma(sum(district_burden$population, na.rm = TRUE))}"
)) # Print total population.

message(glue(
  "Total ISIS attacks joined to districts: {comma(sum(district_burden$observed_attacks, na.rm = TRUE))}"
)) # Print total attacks.

message(glue("Figures saved to: {figure_dir}")) # Print figure folder.

message(glue(
  "Burden table saved to: {file.path(table_dir, 'district_attack_population_burden.csv')}"
)) # Print table path.

message("============================================================") # Print divider.


# ============================================================
# 22. DISPLAY PLOTS IN RSTUDIO VIEWER / PLOTS PANE
# ============================================================

p_lorenz # Display Figure 1.

p_burden # Display Figure 2.

p_skyline # Display Figure 3.
