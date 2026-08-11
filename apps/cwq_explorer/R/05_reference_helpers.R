# cwq_explorer Shiny App helpers for reference tab
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(leaflet)
library(reactable)
library(dplyr)

# Build Leaflet Map for Station Reference Page
build_station_map <- function(sf_strata, sf_stations) {
  strata_pal <- colorFactor(palette = "Set2", domain = sf_strata$Stratum)

  leaflet() |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(
      data = sf_strata,
      group = "Strata",
      fillColor = ~ strata_pal(Stratum),
      fillOpacity = 0.4,
      color = "#333333",
      weight = 1,
      opacity = 0.7,
      label = ~Stratum,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#000000",
        fillOpacity = 0.6,
        bringToFront = FALSE
      )
    ) |>
    addCircleMarkers(
      data = sf_stations,
      group = "Stations",
      radius = 5,
      fillColor = "#5B67B6",
      fillOpacity = 0.85,
      color = "#1A1A1A",
      weight = 1.2,
      label = ~station_abbr,
      popup = ~ paste0(
        "<strong>Survey:</strong> ",
        survey,
        "<br><strong>Station:</strong> ",
        station_abbr,
        "<br><strong>Name:</strong> ",
        station_name,
        "<br><strong>Stratum:</strong> ",
        stratum,
        "<br><strong>Parameters:</strong> ",
        parameters
      )
    )
}

# Build Reactable Table for Station Reference Page
build_station_table <- function(df_stations) {
  df_stations |>
    arrange(stratum, station_abbr) |>
    reactable(
      searchable = TRUE,
      filterable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      pagination = FALSE,
      columns = list(
        station_abbr = colDef(
          name = "Abbreviation",
          width = 130,
          align = "center",
          filterInput = function(values, name) select_filter(values, name)
        ),
        station_name = colDef(name = "Full Station Name", minWidth = 200),
        stratum = colDef(
          name = "Stratum",
          minWidth = 220,
          filterInput = function(values, name) select_filter(values, name)
        )
      )
    )
}
