# ============================================================
# MINIMAL ESOC IRAQ ETHNICITY TRANSFORMATION SCRIPT
# ============================================================


library(tidyverse)                                                     # Load data wrangling tools.
library(sf)                                                            # Load shapefile tools.


esoc_root <- 
  "data/raw/Iraq Ethnicity/"                        # Set path to the ESOC folder.


population_zip <- 
  file.path(esoc_root, "Iraq Ethnicity Population.zip")                 # Set path to the nested population shapefile zip.


population_folder <- 
  file.path(esoc_root, "Iraq Ethnicity Population")                     # Set path to extracted nested population folder.


intersect_shp <- 
  file.path(population_folder, "Iraq_ethnic_Complete_Intersect.shp")    # Set path to the intersect shapefile.


if (!file.exists(intersect_shp)) {                                      # Check whether the shapefile is already extracted.
  
  dir.create(population_folder, recursive = TRUE, showWarnings = FALSE) # Create extraction folder.
  
  unzip(population_zip, exdir = population_folder)                      # Extract shapefile zip.
  
}                                                                       # End unzip check.


ethnic_fragments <- 
  st_read(intersect_shp, quiet = TRUE) |>                               # Read district-ethnicity fragments.
  st_drop_geometry()                                                    # Drop geometry.


read_esoc_arcinfo_population <- function(path) {                        # Define decoder for ESOC ArcInfo population table.
  
  con <- file(path, "rb")                                               # Open binary connection.
  
  on.exit(close(con))                                                   # Close connection on exit.
  
  rows <- list()                                                        # Create empty row list.
  
  i <- 1                                                                # Start row counter.
  
  repeat {                                                              # Start reading loop.
    
    fid <- readBin(con, "integer", 1, size = 4, endian = "big")         # Read fragment ID.
    
    if (length(fid) == 0) break                                         # Stop at end of file.
    
    count <- readBin(con, "integer", 1, size = 4, endian = "big")       # Read raster-cell count.
    
    area <- readBin(con, "numeric", 1, size = 4, endian = "big")        # Read raster-area value.
    
    pop_sum <- readBin(con, "numeric", 1, size = 4, endian = "big")     # Read LandScan population sum.
    
    rows[[i]] <- tibble(FID_IRAQ_E = fid, COUNT = count, AREA = area, SUM = pop_sum) # Store row.
    
    i <- i + 1                                                          # Advance row counter.
    
  }                                                                     # End reading loop.
  
  bind_rows(rows)                                                       # Return decoded table.
  
}                                                                       # End decoder function.


landscan_population <- 
  read_esoc_arcinfo_population(
    file.path(esoc_root, "Iraq Ethnicity Complete", "info", "arc0000.dat")
  )                                                                     # Decode ESOC LandScan population table.


district_ethnic_proportions <- 
  ethnic_fragments |> 
  left_join(
    landscan_population,
    by = c("FID_Iraq_e" = "FID_IRAQ_E")
  ) |>                                                                  # Attach LandScan population to fragments.
  mutate(
    ethnic_group = case_when(
      Ethnicity == 1 ~ "kurdish_inferred",
      Shia == 1 ~ "shia",
      Sunni == 1 ~ "sunni",
      Christians == 1 ~ "christian",
      Turcomans == 1 ~ "turcoman",
      Mixed_ShSu == 1 ~ "mixed_shia_sunni",
      TRUE ~ "unclassified"
    )
  ) |>                                                                  # Label each fragment by ethnicity.
  group_by(
    ADM3CODE,
    ADM3NAME,
    ADM2CODE,
    ADM2NAME,
    ethnic_group
  ) |>                                                                  # Group by district and ethnic group.
  summarise(
    ethnic_population = sum(SUM, na.rm = TRUE),
    .groups = "drop"
  ) |>                                                                  # Sum fragment population by district and ethnic group.
  pivot_wider(
    names_from = ethnic_group,
    values_from = ethnic_population,
    values_fill = 0,
    names_glue = "{ethnic_group}_pop"
  ) |>                                                                  # Create one population column per ethnic group.
  mutate(
    total_ethnic_pop = 
      kurdish_inferred_pop +
      shia_pop +
      sunni_pop +
      christian_pop +
      turcoman_pop +
      mixed_shia_sunni_pop,
    kurdish_inferred_share = kurdish_inferred_pop / total_ethnic_pop,
    shia_share = shia_pop / total_ethnic_pop,
    sunni_share = sunni_pop / total_ethnic_pop,
    christian_share = christian_pop / total_ethnic_pop,
    turcoman_share = turcoman_pop / total_ethnic_pop,
    mixed_shia_sunni_share = mixed_shia_sunni_pop / total_ethnic_pop,
    ethnic_fractionalization =
      1 -
      (
        kurdish_inferred_share^2 +
        shia_share^2 +
        sunni_share^2 +
        christian_share^2 +
        turcoman_share^2 +
        mixed_shia_sunni_share^2
      )
  )                                                                     # Create district totals, shares, and fractionalization.


readr::write_csv(
  district_ethnic_proportions,
  "data/esoc_iraq_district_ethnic_proportions.csv"
)                                                                       # Save final regression-ready dataset.
