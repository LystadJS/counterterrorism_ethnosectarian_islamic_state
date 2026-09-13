# ========================================================================
# TARGETING DASHBOARD: OBSERVED COMPOSITION + TWO-STAGE MODEL
# Purpose:
#   Build one integrated, publication-quality dashboard showing:
#   1) observed civilian vs state/security targeting composition,
#   2) model-predicted probability of civilian targeting,
#   3) model-predicted probability of religious targeting among civilian attacks.
#
# Assumptions:
#   - target_composition_wide already exists from the observed composition script.
#   - two_stage_predictions already exists from the two-stage model script.
#   - demographic_order already exists from the prior workflow.
#   - export_table() and export_plot() helper functions already exist in the project.
#
# Key design decisions:
#   - Civilian targeting is solid black.
#   - State/security targeting starts at the civilian/state boundary and turns red quickly.
#   - Religious targeting is shown as a white outlined box, starting at zero.
#   - Diagonal white hatch marks are drawn inside the religious box.
#   - Pr(Civilian) model is black.
#   - Pr(Religious | Civilian) model is red.
#
# Last updated:
#   2026-05-10
# ========================================================================

# 00) PACKAGE CHECKS ------------------------------------------------------

required_dashboard_packages <- c(
  # Define required packages.
  "dplyr", # Data wrangling.
  "tidyr", # Data reshaping.
  "purrr", # Functional helpers.
  "ggplot2", # Plotting.
  "scales", # Percent labels and formatting.
  "tibble", # Tibble helpers.
  "grid" # Unit spacing helpers.
)

missing_dashboard_packages <- required_dashboard_packages[
  # Find missing packages.
  !purrr::map_lgl(required_dashboard_packages, requireNamespace, quietly = TRUE) # Check availability.
]

if (length(missing_dashboard_packages) > 0) {
  # Stop if packages are missing.
  stop(
    # Return useful installation message.
    paste0(
      "Install these packages before running this script: ",
      paste(missing_dashboard_packages, collapse = ", ")
    )
  )
}


# 01) VALIDATE REQUIRED OBJECTS -------------------------------------------

required_dashboard_objects <- c(
  # Define required objects.
  "target_composition_wide", # Observed targeting summary.
  "two_stage_predictions", # Two-stage model predictions.
  "demographic_order" # Existing demographic order.
)

missing_dashboard_objects <- required_dashboard_objects[
  # Find missing objects.
  !purrr::map_lgl(required_dashboard_objects, exists) # Check whether objects exist.
]

if (length(missing_dashboard_objects) > 0) {
  # Stop if anything is missing.
  stop(
    # Return helpful message.
    paste0(
      "Run the observed target-composition script and the two-stage model script first. Missing objects: ",
      paste(missing_dashboard_objects, collapse = ", ")
    )
  )
}


# 02) DEFINE PANEL ORDER AND PERIOD ORDER ---------------------------------

dashboard_panel_levels <- c(
  # Define dashboard column order.
  "Observed composition", # Raw observed composition.
  "Model: Pr(Civilian)", # Stage 1 model.
  "Model: Pr(Religious | Civilian)" # Stage 2 model.
)

dashboard_period_levels <- c(
  # Define ISIS phase order.
  "Expansion / seizure period", # Early phase.
  "Territorial war period", # Territorial war phase.
  "Post-territorial insurgency" # Later insurgent phase.
)


# 03) BUILD STORY-ORIENTED DEMOGRAPHIC ORDER ------------------------------

dashboard_story_order <- target_composition_wide |> # Start from observed composition.
  mutate(
    # Standardize fields used for ranking.
    period = factor(as.character(period), levels = dashboard_period_levels), # Lock period order.
    civilian_share = pmin(pmax(civilian_share, 0), 1), # Bound civilian share.
    government_share = pmin(pmax(government_share, 0), 1), # Bound state share.
    religious_share = pmin(pmax(religious_share, 0), civilian_share), # Keep religious share inside civilian share.
    religious_share_within_civilian = if_else(
      # Calculate religious share conditional on civilian targeting.
      civilian_share > 0, # Require positive civilian share.
      religious_share / civilian_share, # Conditional religious share.
      0 # Otherwise zero.
    )
  ) |>
  filter(
    # Keep usable rows.
    !is.na(period) # Require valid period.
  ) |>
  group_by(
    # Group by demographic type.
    demographic_type # Demographic type.
  ) |>
  summarize(
    # Summarize for storytelling score.
    total_attacks_all_periods = sum(total_attacks, na.rm = TRUE), # Total attacks across phases.
    civilian_share_overall = weighted.mean(
      civilian_share,
      total_attacks,
      na.rm = TRUE
    ), # Overall civilian share.
    religious_within_civilian_overall = weighted.mean(
      religious_share_within_civilian,
      total_attacks,
      na.rm = TRUE
    ), # Overall religious share within civilian.
    post_period_civilian_share = weighted.mean(
      # Calculate post-territorial civilian share.
      civilian_share[period == "Post-territorial insurgency"], # Post-territorial civilian share.
      total_attacks[period == "Post-territorial insurgency"], # Weight by attacks.
      na.rm = TRUE # Remove missing values.
    ),
    .groups = "drop" # Drop grouping.
  ) |>
  mutate(
    # Clean and score groups.
    post_period_civilian_share = if_else(
      # Replace unstable NaN values.
      is.nan(post_period_civilian_share), # Check for NaN.
      civilian_share_overall, # Fall back to overall civilian share.
      post_period_civilian_share # Otherwise keep observed value.
    ),
    story_score = 0.45 *
      civilian_share_overall + # Main story dimension.
      0.40 * religious_within_civilian_overall + # Mechanism dimension.
      0.15 * post_period_civilian_share # Late-phase shift dimension.
  ) |>
  arrange(story_score) |> # Order from more state-focused to more civilian/religious-focused.
  pull(demographic_type) # Extract ordered vector.

dashboard_story_order <- dashboard_story_order[
  # Remove missing labels.
  !is.na(dashboard_story_order) # Keep non-missing values.
]

if (length(dashboard_story_order) == 0) {
  # Fall back if no valid story order exists.
  dashboard_story_order <- demographic_order # Use old order.
}


# 04) HELPER: BUILD OBSERVED BAR RECTANGLES -------------------------------

make_observed_abrupt_gradient <- function(
  data,
  bins = 180,
  transition_power = 0.10
) {
  # Build civilian-black/state-red bars.

  state_palette <- grDevices::colorRampPalette(
    # Build abrupt state-side palette.
    c(
      "#1F1F1F", # Black at the boundary.
      "#5A1616", # Dark red immediately after.
      "#842829" # Project red toward the right edge.
    )
  )(bins) # Create color vector.

  cleaned_data <- data |> # Start from observed data.
    mutate(
      # Create plotting variables.
      period = factor(as.character(period), levels = dashboard_period_levels), # Lock phase order.
      demographic_type_ordered = factor(
        demographic_type,
        levels = dashboard_story_order
      ), # Apply story order.
      row_y = as.numeric(demographic_type_ordered), # Numeric y position.
      civilian_share = pmin(pmax(civilian_share, 0), 1), # Bound civilian share.
      government_share = pmin(pmax(government_share, 0), 1), # Bound state share.
      civilian_xmin = 0, # Civilian region starts at zero.
      civilian_xmax = civilian_share, # Civilian region ends at the civilian share.
      government_xmin = civilian_share, # State region begins at the border.
      government_xmax = 1 # State region ends at 100%.
    ) |>
    filter(
      # Keep valid rows.
      !is.na(period), # Require valid period.
      !is.na(row_y) # Require y position.
    )

  civilian_rectangles <- cleaned_data |> # Build civilian rectangles.
    transmute(
      # Keep plotting fields.
      period = period, # Phase.
      demographic_type = demographic_type, # Demographic type.
      demographic_type_ordered = demographic_type_ordered, # Ordered demographic type.
      row_y = row_y, # Y position.
      xmin = civilian_xmin, # Left edge.
      xmax = civilian_xmax, # Right edge.
      fill_color = "#1F1F1F", # Civilian section is solid black.
      panel = factor("Observed composition", levels = dashboard_panel_levels) # Assign panel.
    )

  government_rectangles <- purrr::map_dfr(
    # Build state-side rectangles row-by-row.
    seq_len(nrow(cleaned_data)), # Iterate over rows.
    function(i) {
      # Function for one row.

      row_data <- cleaned_data[i, ] # Extract one row.

      if (is.na(row_data$government_share) || row_data$government_share <= 0) {
        # Skip rows with no state share.
        return(tibble::tibble()) # Return empty tibble.
      }

      tibble::tibble(
        # Create bins for state side.
        bin = seq_len(bins) # Bin index.
      ) |>
        mutate(
          # Compute positions and colors.
          period = row_data$period, # Phase.
          demographic_type = row_data$demographic_type, # Demographic type.
          demographic_type_ordered = row_data$demographic_type_ordered, # Ordered demographic type.
          row_y = row_data$row_y, # Y position.
          relative_pos = bin / bins, # Relative location within state region.
          transformed_pos = relative_pos^transition_power, # Make transition abrupt.
          color_index = pmax(1, pmin(bins, ceiling(transformed_pos * bins))), # Bound palette index.
          xmin = row_data$government_xmin +
            ((bin - 1) / bins) * row_data$government_share, # Rectangle left edge.
          xmax = row_data$government_xmin +
            (bin / bins) * row_data$government_share, # Rectangle right edge.
          fill_color = state_palette[color_index], # Assign fill color.
          panel = factor(
            "Observed composition",
            levels = dashboard_panel_levels
          ) # Assign panel.
        ) |>
        dplyr::select(
          # Keep final fields.
          period, # Phase.
          demographic_type, # Demographic type.
          demographic_type_ordered, # Ordered demographic type.
          row_y, # Y position.
          xmin, # Left edge.
          xmax, # Right edge.
          fill_color, # Fill color.
          panel # Panel.
        )
    }
  )

  bind_rows(
    # Combine civilian and state rectangles.
    civilian_rectangles, # Civilian rectangles.
    government_rectangles # State rectangles.
  )
}


# 05) PREPARE OBSERVED BAR DATA -------------------------------------------

dashboard_observed_gradient_data <- make_observed_abrupt_gradient(
  # Create observed bar rectangles.
  data = target_composition_wide, # Use observed composition.
  bins = 180, # Use many bins for smooth rendering.
  transition_power = 0.10 # Make red transition abrupt.
)


# 06) PREPARE OBSERVED MARKER AND LABEL DATA ------------------------------

dashboard_observed_marker_data <- target_composition_wide |> # Start from observed summary.
  mutate(
    # Build plotting fields.
    period = factor(as.character(period), levels = dashboard_period_levels), # Lock period order.
    panel = factor("Observed composition", levels = dashboard_panel_levels), # Assign panel.
    demographic_type_ordered = factor(
      demographic_type,
      levels = dashboard_story_order
    ), # Apply story order.
    row_y = as.numeric(demographic_type_ordered), # Numeric y position.
    civilian_share = pmin(pmax(civilian_share, 0), 1), # Bound civilian share.
    government_share = pmin(pmax(government_share, 0), 1), # Bound state share.
    religious_share = pmin(pmax(religious_share, 0), civilian_share), # Bound religious share inside civilian share.
    border_x = civilian_share, # Civilian/state boundary.
    religious_box_xmin = 0, # Religious box begins at zero.
    religious_box_xmax = religious_share, # Religious box ends at religious share.
    religious_label_x = religious_share / 2, # Center religious label above religious box.
    civilian_label_x = civilian_share / 2, # Center civilian label above civilian section.
    government_label_x = civilian_share + (government_share / 2), # Center state label above state section.
    religious_label = if_else(
      # Religious label text.
      religious_share >= 0.03, # Only label visible shares.
      scales::percent(religious_share, accuracy = 1), # Format as percent.
      NA_character_ # Otherwise blank.
    ),
    civilian_label = if_else(
      # Civilian label text.
      civilian_share >= 0.05, # Only label visible shares.
      scales::percent(civilian_share, accuracy = 1), # Format as percent.
      NA_character_ # Otherwise blank.
    ),
    government_label = if_else(
      # State label text.
      government_share >= 0.05, # Only label visible shares.
      scales::percent(government_share, accuracy = 1), # Format as percent.
      NA_character_ # Otherwise blank.
    )
  ) |>
  filter(
    # Keep valid rows.
    !is.na(period), # Require period.
    !is.na(row_y) # Require y position.
  )


# 07) HELPER: BUILD DIAGONAL HATCH LINES INSIDE RELIGIOUS BOX -------------

build_religious_hatch_data <- function(
  data,
  box_half_height = 0.36,
  dx = 0.08,
  spacing = 0.035
) {
  # Build hatch segments.

  hatch_data <- data |> # Start from observed marker data.
    filter(religious_share > 0) |> # Keep only rows with religious box width.
    mutate(
      # Define vertical extent of the box.
      ymin = row_y - box_half_height, # Lower box edge.
      ymax = row_y + box_half_height # Upper box edge.
    )

  purrr::map_dfr(
    # Build hatch lines row-by-row.
    seq_len(nrow(hatch_data)), # Iterate rows.
    function(i) {
      # Function for one row.

      row_i <- hatch_data[i, ] # Extract row.

      xmin <- row_i$religious_box_xmin # Extract left edge.
      xmax <- row_i$religious_box_xmax # Extract right edge.
      ymin <- row_i$ymin # Extract lower edge.
      ymax <- row_i$ymax # Extract upper edge.
      height <- ymax - ymin # Calculate box height.

      base_positions <- seq(xmin - dx, xmax, by = spacing) # Starting positions for diagonals.

      purrr::map_dfr(
        # Build one diagonal per base position.
        base_positions, # Use base positions.
        function(b) {
          # Function for one diagonal.

          x_start <- max(b, xmin) # Clip line to left edge.
          x_end <- min(b + dx, xmax) # Clip line to right edge.

          if (x_end <= x_start) {
            # Skip invalid lines.
            return(tibble::tibble()) # Return empty tibble.
          }

          y_start <- ymin + ((x_start - b) / dx) * height # Interpolate start y.
          y_end <- ymin + ((x_end - b) / dx) * height # Interpolate end y.

          tibble::tibble(
            # Return one hatch segment.
            period = row_i$period, # Phase.
            panel = row_i$panel, # Panel.
            demographic_type = row_i$demographic_type, # Demographic type.
            demographic_type_ordered = row_i$demographic_type_ordered, # Ordered type.
            row_y = row_i$row_y, # Row position.
            x = x_start, # Segment start x.
            xend = x_end, # Segment end x.
            y = y_start, # Segment start y.
            yend = y_end # Segment end y.
          )
        }
      )
    }
  )
}

dashboard_religious_hatch_data <- build_religious_hatch_data(
  # Build hatch-line data.
  data = dashboard_observed_marker_data, # Use observed marker data.
  box_half_height = 0.36, # Match bar height.
  dx = 0.08, # Width of each diagonal.
  spacing = 0.035 # Spacing between diagonals.
)


# 08) PREPARE MODEL DATA ---------------------------------------------------

dashboard_model_data <- two_stage_predictions |> # Start from two-stage model predictions.
  mutate(
    # Build plotting fields.
    period = factor(as.character(period), levels = dashboard_period_levels), # Lock phase order.
    demographic_type_ordered = factor(
      as.character(demographic_type),
      levels = dashboard_story_order
    ), # Apply story order.
    row_y = as.numeric(demographic_type_ordered), # Numeric y position.
    panel = case_when(
      # Map model outcomes to dashboard panels.
      as.character(outcome_label) ==
        "Pr(Civilian target)" ~ "Model: Pr(Civilian)", # Stage 1.
      as.character(outcome_label) ==
        "Pr(Religious target | Civilian target)" ~ "Model: Pr(Religious | Civilian)", # Stage 2.
      TRUE ~ NA_character_ # Fallback.
    ),
    panel = factor(panel, levels = dashboard_panel_levels), # Lock panel order.
    model_color = case_when(
      # Swap colors as requested.
      panel == "Model: Pr(Civilian)" ~ "#1F1F1F", # Black for civilian model.
      panel == "Model: Pr(Religious | Civilian)" ~ "#842829", # Red for religious-given-civilian model.
      TRUE ~ "#525252" # Fallback.
    ),
    label_x = pmin(conf_high + 0.025, 1.045), # Put labels just right of the CI.
    probability_label = scales::percent(predicted_probability, accuracy = 1) # Format label text.
  ) |>
  filter(
    # Keep valid rows.
    !is.na(period), # Require phase.
    !is.na(panel), # Require panel.
    !is.na(row_y) # Require y position.
  ) |>
  dplyr::select(
    # Keep needed plotting fields.
    period, # Phase.
    demographic_type, # Demographic type.
    demographic_type_ordered, # Ordered demographic type.
    row_y, # Y position.
    panel, # Panel.
    predicted_probability, # Point estimate.
    conf_low, # Lower CI.
    conf_high, # Upper CI.
    label_x, # Label x.
    probability_label, # Label text.
    model_color # Plot color.
  )


# 09) EXPORT SUPPORTING TABLES --------------------------------------------

export_table(
  # Export story order.
  tibble::tibble(
    row_order = seq_along(dashboard_story_order), # Row number.
    demographic_type = dashboard_story_order # Ordered demographic type.
  ),
  file_name = "targeting_dashboard_story_order.csv" # Output filename.
)

export_table(
  # Export observed marker data.
  dashboard_observed_marker_data, # Use observed labels and markers.
  file_name = "targeting_dashboard_observed_marker_data.csv" # Output filename.
)

export_table(
  # Export observed gradient data.
  dashboard_observed_gradient_data, # Use observed rectangle data.
  file_name = "targeting_dashboard_observed_gradient_data.csv" # Output filename.
)

export_table(
  # Export hatch-line data.
  dashboard_religious_hatch_data, # Use hatch-line data.
  file_name = "targeting_dashboard_religious_hatch_data.csv" # Output filename.
)

export_table(
  # Export model plotting data.
  dashboard_model_data, # Use model data.
  file_name = "targeting_dashboard_two_stage_model_data.csv" # Output filename.
)


# 10) BUILD DASHBOARD PLOT -------------------------------------------------

plot_targeting_dashboard_final <- ggplot2::ggplot() + # Start plot.

  # OBSERVED BAR RECTANGLES ------------------------------------------------
  ggplot2::geom_rect(
    # Draw observed composition bars.
    data = dashboard_observed_gradient_data, # Use observed rectangle data.
    ggplot2::aes(
      xmin = xmin, # Rectangle left edge.
      xmax = xmax, # Rectangle right edge.
      ymin = row_y - 0.36, # Rectangle lower edge.
      ymax = row_y + 0.36, # Rectangle upper edge.
      fill = fill_color # Use precomputed fill color.
    ),
    color = NA # No outline on individual small rectangles.
  ) +

  # OBSERVED CIVILIAN / STATE BOUNDARY ------------------------------------
  ggplot2::geom_segment(
    # Draw exact civilian/state boundary.
    data = dashboard_observed_marker_data, # Use observed marker data.
    ggplot2::aes(
      x = border_x, # Boundary x.
      xend = border_x, # Same x.
      y = row_y - 0.43, # Lower extent.
      yend = row_y + 0.43 # Upper extent.
    ),
    inherit.aes = FALSE, # Use explicit mapping.
    color = "#000000", # Black line.
    linewidth = 1.05, # Make line easy to see.
    lineend = "round" # Rounded ends.
  ) +

  # RELIGIOUS WHITE BOX ----------------------------------------------------
  ggplot2::geom_rect(
    # Draw religious box starting at zero.
    data = dashboard_observed_marker_data |> filter(religious_share > 0), # Keep rows with religious share.
    ggplot2::aes(
      xmin = religious_box_xmin, # Left edge of religious box.
      xmax = religious_box_xmax, # Right edge of religious box.
      ymin = row_y - 0.36, # Align with full bar height.
      ymax = row_y + 0.36 # Align with full bar height.
    ),
    inherit.aes = FALSE, # Use explicit mapping.
    fill = NA, # No fill.
    color = "white", # White outline.
    linewidth = 1.1 # Thick outline.
  ) +

  # RELIGIOUS DIAGONAL HATCH LINES ----------------------------------------
  ggplot2::geom_segment(
    # Draw hatch lines inside religious box.
    data = dashboard_religious_hatch_data, # Use hatch-line data.
    ggplot2::aes(
      x = x, # Segment start x.
      xend = xend, # Segment end x.
      y = y, # Segment start y.
      yend = yend # Segment end y.
    ),
    inherit.aes = FALSE, # Use explicit mapping.
    color = "white", # White hatch lines.
    linewidth = 0.65, # Hatch-line thickness.
    alpha = 0.95, # High visibility.
    lineend = "round" # Rounded ends.
  ) +

  # OBSERVED RELIGIOUS LABELS ---------------------------------------------
  ggplot2::geom_text(
    # Add religious share labels above box.
    data = dashboard_observed_marker_data |> filter(!is.na(religious_label)), # Keep visible labels.
    ggplot2::aes(
      x = religious_label_x, # Center over religious box.
      y = row_y + 0.72, # Above bar.
      label = religious_label # Religious percent.
    ),
    inherit.aes = FALSE, # Use explicit mapping.
    size = 2.85, # Text size.
    fontface = "bold", # Bold text.
    color = "#1F1F1F" # Dark text.
  ) +

  # OBSERVED CIVILIAN LABELS ----------------------------------------------
  ggplot2::geom_text(
    # Add civilian share labels.
    data = dashboard_observed_marker_data |> filter(!is.na(civilian_label)), # Keep visible labels.
    ggplot2::aes(
      x = civilian_label_x, # Center over civilian section.
      y = row_y + 0.50, # Above bar.
      label = civilian_label # Civilian percent.
    ),
    inherit.aes = FALSE, # Use explicit mapping.
    size = 3.8, # Text size.
    fontface = "bold", # Bold text.
    color = "#1F1F1F" # Dark text.
  ) +

  # OBSERVED STATE LABELS --------------------------------------------------
  ggplot2::geom_text(
    # Add state share labels.
    data = dashboard_observed_marker_data |> filter(!is.na(government_label)), # Keep visible labels.
    ggplot2::aes(
      x = government_label_x, # Center over state section.
      y = row_y + 0.50, # Above bar.
      label = government_label # State percent.
    ),
    inherit.aes = FALSE, # Use explicit mapping.
    size = 3.8, # Text size.
    fontface = "bold", # Bold text.
    color = "#1F1F1F" # Dark text.
  ) +

  # MODEL CONFIDENCE INTERVALS --------------------------------------------
  ggplot2::geom_errorbarh(
    # Draw model confidence intervals.
    data = dashboard_model_data, # Use model plotting data.
    ggplot2::aes(
      xmin = conf_low, # Lower CI.
      xmax = conf_high, # Upper CI.
      y = row_y, # Row position.
      color = model_color # Model color.
    ),
    inherit.aes = FALSE, # Use explicit mapping.
    height = 0.20, # CI cap height.
    linewidth = 0.85, # CI line width.
    alpha = 0.75 # Slight transparency.
  ) +

  # MODEL POINT ESTIMATES --------------------------------------------------
  ggplot2::geom_point(
    # Draw point estimates.
    data = dashboard_model_data, # Use model plotting data.
    ggplot2::aes(
      x = predicted_probability, # Predicted probability.
      y = row_y, # Row position.
      fill = model_color # Fill color.
    ),
    inherit.aes = FALSE, # Use explicit mapping.
    shape = 21, # Filled circle.
    size = 3.15, # Point size.
    stroke = 0.50, # White outline thickness.
    color = "white" # White outline.
  ) +

  # MODEL LABELS -----------------------------------------------------------
  ggplot2::geom_text(
    # Add percent labels right of CI.
    data = dashboard_model_data, # Use model plotting data.
    ggplot2::aes(
      x = label_x, # Right of CI.
      y = row_y, # Row position.
      label = probability_label, # Label text.
      color = model_color # Match model color.
    ),
    inherit.aes = FALSE, # Use explicit mapping.
    hjust = 0, # Left-align label text.
    size = 3.80, # Larger font.
    fontface = "bold" # Bold labels.
  ) +

  # FACETS -----------------------------------------------------------------
  ggplot2::facet_grid(
    # Build dashboard facets.
    rows = ggplot2::vars(period), # Rows = ISIS phases.
    cols = ggplot2::vars(panel), # Columns = observed + model panels.
    scales = "fixed", # Shared scale across panels.
    space = "fixed" # Fixed panel sizes.
  ) +

  # SCALES -----------------------------------------------------------------
  ggplot2::scale_fill_identity() + # Use supplied fill colors directly.
  ggplot2::scale_color_identity() + # Use supplied line/text colors directly.
  ggplot2::scale_y_continuous(
    # Manual y-axis settings.
    breaks = seq_along(dashboard_story_order), # One break per demographic type.
    labels = dashboard_story_order, # Ordered demographic labels.
    expand = ggplot2::expansion(mult = c(0.08, 0.12)) # Add vertical breathing room.
  ) +
  ggplot2::scale_x_continuous(
    # Shared x-axis settings.
    labels = scales::percent_format(accuracy = 1), # Format as percent.
    limits = c(0, 1.07), # Reduce wasted whitespace past 100%.
    breaks = seq(0, 1, by = 0.25), # Major breaks.
    expand = ggplot2::expansion(mult = c(0.00, 0.005)) # Very small outer expansion.
  ) +

  # LABELS -----------------------------------------------------------------
  ggplot2::labs(
    # Add plot titles and caption.
    title = "ISIS Targeting Mechanisms by District Demography and Phase",
    subtitle = "Observed civilian/state targeting is paired with two-stage model estimates of civilian targeting and religious targeting",
    x = "Share / predicted probability",
    y = NULL,
    caption = paste0(
      "Rows are ordered from more state-focused to more civilian/religious-focused targeting profiles. ",
      "Observed composition: civilian targeting is solid black; state/security targeting begins at the black boundary line and turns red quickly. ",
      "White outlined and hatched boxes show religious targets as a share of all categorized attacks, starting at zero within the civilian portion. ",
      "Model panels show predicted probabilities with 95% confidence intervals from two logistic regressions: ",
      "Stage 1 models civilian vs state/security targeting; Stage 2 models religious targeting among civilian attacks. ",
      "Pr(Civilian) is shown in black; Pr(Religious | Civilian) is shown in red."
    )
  ) +

  # THEME ------------------------------------------------------------------
  ggplot2::theme_classic() + # Use clean classic theme.
  ggplot2::theme(
    # Refine appearance.
    strip.background = ggplot2::element_rect(fill = "#F2F2F2", color = NA), # Light strip background.
    strip.text.x = ggplot2::element_text(face = "bold", size = 9.8), # Column strip text.
    strip.text.y = ggplot2::element_text(
      face = "bold",
      size = 9.4,
      angle = -45,
      hjust = 0.5,
      vjust = 0.50
    ),
    axis.text.y = ggplot2::element_text(
      face = "bold",
      size = 8.4,
      angle = -45,
      hjust = 1,
      vjust = 0.95
    ), # Y-axis labels.
    axis.text.x = ggplot2::element_text(size = 8.0), # X-axis labels.
    axis.title.x = ggplot2::element_text(size = 9.4), # X-axis title.
    plot.title = ggplot2::element_text(face = "bold", size = 15), # Main title.
    plot.subtitle = ggplot2::element_text(size = 10.4), # Subtitle.
    plot.caption = ggplot2::element_text(hjust = 0, size = 8.0), # Left-aligned caption.
    panel.grid.major.x = ggplot2::element_line(
      color = "#E6E6E6",
      linewidth = 0.22
    ), # Light x-grid.
    panel.grid.major.y = ggplot2::element_blank(), # Remove y-grid.
    panel.grid.minor = ggplot2::element_blank(), # Remove minor grid.
    panel.spacing.x = grid::unit(0.85, "lines"), # Horizontal panel spacing.
    panel.spacing.y = grid::unit(1.10, "lines"), # Vertical panel spacing.
    legend.position = "none" # No legend needed.
  )


# 11) PRINT DASHBOARD ------------------------------------------------------

print(plot_targeting_dashboard_final) # Print final dashboard plot.


# 12) EXPORT DASHBOARD -----------------------------------------------------

export_plot(
  # Export final dashboard plot.
  plot_targeting_dashboard_final, # Use final dashboard.
  file_name = "targeting_dashboard_observed_plus_two_stage_model_final.png", # Output filename.
  width = 16.2, # Plot width.
  height = 13.8 # Plot height.
)
