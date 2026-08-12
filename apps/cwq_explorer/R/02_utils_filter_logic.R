# cwq_explorer Shiny App utilities for filtering logic
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(dplyr)

# Define choices for an input applying order_vec as their display order
active_ordered_choices <- function(order_vec, active_vals) {
  intersect(order_vec, as.character(unique(active_vals)))
}

# Define selection for an input, preserving current selection if available, otherwise use fallback
# Fallback defaults to all valid choices
preserve_selection <- function(
  current_sel,
  valid_choices,
  fallback = valid_choices
) {
  valid_sel <- intersect(as.character(current_sel), valid_choices)
  if (length(valid_sel) > 0) valid_sel else fallback
}

# Calculate updated adj_wy choices and selections from df_filt, preserving current selection
# if available
compute_wy_updates <- function(current_wy, df_filt) {
  wy_choices <- sort(unique(df_filt$wy_adj))
  selected_wy <- preserve_selection(
    current_sel = current_wy,
    valid_choices = wy_choices,
    fallback = max(wy_choices)
  )

  list(choices = wy_choices, selected = selected_wy)
}

# Calculate updated strata choices and selections from df_filt, keeping valid previous selections
compute_strata_updates <- function(current_strata, df_filt, strata_order) {
  strata_choices <- active_ordered_choices(
    order_vec = strata_order,
    active_vals = df_filt$stratum
  )

  selected_strata <- preserve_selection(
    current_sel = current_strata,
    valid_choices = strata_choices
  )

  list(choices = strata_choices, selected = selected_strata)
}

# Calculate updated station choices and selections from df_filt, keeping valid previous selections,
# auto-adding stations for newly selected strata, and selecting all stations within selected
# strata when parameter changes
compute_station_updates <- function(
  current_strata,
  prev_strata,
  current_stations,
  current_param,
  prev_param,
  df_filt,
  station_order
) {
  station_choices <- active_ordered_choices(
    order_vec = station_order,
    active_vals = df_filt$station_abbr
  )

  # Detect parameter change
  param_changed <- !is.null(prev_param) && (current_param != prev_param)

  if (param_changed) {
    # If parameter changed, select all available stations across active strata
    selected_stations <- station_choices
  } else {
    # Otherwise, preserve existing selections and auto-add stations for new strata
    added_strata <- setdiff(
      as.character(current_strata),
      as.character(prev_strata)
    )

    new_stations <- df_filt |>
      dplyr::filter(as.character(stratum) %in% added_strata) |>
      dplyr::pull(station_abbr) |>
      as.character() |>
      unique()

    valid_existing_stations <- intersect(
      as.character(current_stations),
      station_choices
    )

    combined_stations <- intersect(
      station_order,
      union(valid_existing_stations, new_stations)
    )

    selected_stations <- preserve_selection(
      current_sel = combined_stations,
      valid_choices = station_choices,
      fallback = station_choices
    )
  }

  list(choices = station_choices, selected = selected_stations)
}

# Compute all available strata and stations in df_filt and apply custom ordering
compute_select_all <- function(df_filt, strata_order, station_order) {
  all_strata <- active_ordered_choices(
    order_vec = strata_order,
    active_vals = df_filt$stratum
  )
  all_stations <- active_ordered_choices(
    order_vec = station_order,
    active_vals = df_filt$station_abbr
  )

  list(strata = all_strata, stations = all_stations)
}
