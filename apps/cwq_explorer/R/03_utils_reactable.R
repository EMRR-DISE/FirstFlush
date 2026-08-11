# cwq_explorer Shiny App utilities for reactable
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(reactable)
library(htmltools)

# Custom select dropdown builder for reactable columns
select_filter <- function(values, name) {
  # Get unique non-NA choices
  unique_vals <- sort(unique(values[!is.na(values)]))

  tags$select(
    # Triggers reactable's built-in filter event on change
    onchange = sprintf(
      "Reactable.setFilter('station_table', '%s', event.target.value)",
      name
    ),
    tags$option(value = "", "All"),
    lapply(unique_vals, function(val) {
      tags$option(value = val, val)
    }),
    style = "width: 100%; height: 28px; font-size: 12px; border-radius: 4px; border: 1px solid #ccc; padding: 2px 4px;"
  )
}
