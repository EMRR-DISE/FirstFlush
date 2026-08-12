# Shiny App used to explore continuous WQ data for the First Flush synthesis -
# source data is the daily values dataset
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(tibble)
library(leaflet)
library(reactable)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Build UI
ui <- page_navbar(
  title = "First Flush CWQ Data Explorer",
  theme = bs_theme(version = 5),
  ui_tab_explorer(dowy_adj_rng),
  ui_tab_reference()
)

# Build server
server <- function(input, output, session) {
  # Initialize reactive value to store the previously selected strata and parameter
  prev_strata <- reactiveVal(character(0))
  prev_param <- reactiveVal(NULL)

  # Add debounce to dowy_rng slider
  # Debounced slider value: waits 400ms after the user stops dragging
  dowy_rng_d <- reactive({
    input$dowy_rng
  }) |>
    debounce(400)

  # Filter cwq data to parameter
  cwq_data_param <- reactive({
    cwq_data_c |> filter(parameter == input$param)
  })

  # Reset adj_wy choices when parameter is changed, preserving current selection if available
  observe({
    wy_update <- compute_wy_updates(
      current_wy = isolate(input$adj_wy),
      df_filt = cwq_data_param()
    )

    updateSelectInput(
      session,
      "adj_wy",
      choices = wy_update$choices,
      selected = wy_update$selected
    )
  })

  # Reset the Day of Adjusted WY slider to display all values when adj_wy is changed or
  # explicit button click
  observe({
    updateSliderInput(session, "dowy_rng", value = dowy_adj_rng)
  }) |>
    bindEvent(input$adj_wy, input$reset_slider)

  # Filter cwq and storms data by selected WY and DOWY range
  ls_data_filt_wy <- reactive({
    cwq_data_param <- cwq_data_param()
    rng <- dowy_rng_d()
    map(
      lst(cwq_data_param, df_storms),
      \(x) {
        filter(x, wy_adj == input$adj_wy, between(dowy_adj, rng[1], rng[2]))
      }
    )
  })

  # Populate strata choices, keeping valid previous selections
  observe({
    strata_update <- compute_strata_updates(
      current_strata = isolate(input$strata),
      df_filt = ls_data_filt_wy()$cwq_data_param,
      strata_order = strata_order
    )

    updateSelectizeInput(
      session,
      "strata",
      choices = strata_update$choices,
      selected = strata_update$selected
    )
  })

  # Filter cwq further by selected strata
  cwq_data_strata <- reactive({
    ls_data_filt_wy()$cwq_data_param |> filter(stratum %in% input$strata)
  })

  # Populate station choices, keeping valid previous selections, auto-adding stations for
  # newly selected strata, and resetting selections on parameter change
  observe({
    req(input$strata)
    station_update <- compute_station_updates(
      current_strata = input$strata,
      prev_strata = isolate(prev_strata()),
      current_stations = isolate(input$stations),
      current_param = input$param,
      prev_param = isolate(prev_param()),
      df_filt = cwq_data_strata(),
      station_order = station_order
    )

    # Update the tracking reactive value for the next event
    prev_strata(as.character(input$strata))
    prev_param(input$param)

    freezeReactiveValue(input, "stations")
    updateSelectizeInput(
      session,
      "stations",
      choices = station_update$choices,
      selected = station_update$selected
    )
  })

  # Handle "Select All" button click
  observe({
    select_all_update <- compute_select_all(
      df_filt = ls_data_filt_wy()$cwq_data_param,
      strata_order = strata_order,
      station_order = station_order
    )

    updateSelectizeInput(
      session,
      "strata",
      selected = select_all_update$strata
    )
    freezeReactiveValue(input, "stations")
    updateSelectizeInput(
      session,
      "stations",
      selected = select_all_update$stations
    )
  }) |>
    bindEvent(input$select_all)

  # Handle "Remove All" button click
  observe({
    updateSelectizeInput(session, "strata", selected = character(0))
    freezeReactiveValue(input, "stations")
    updateSelectizeInput(session, "stations", selected = character(0))
  }) |>
    bindEvent(input$remove_all)

  # Finish filtering cwq by stations
  cwq_data_filt <- reactive({
    # Halt reactivity if either selection is empty
    req(input$strata, input$stations)
    cwq_data_strata() |> filter(station_abbr %in% input$stations)
  })

  # Dedicated reactive for just the storm data needed by the plot
  df_storms_filt <- reactive({
    ls_data_filt_wy()$df_storms
  })

  # Render plot output
  output$plot <- renderPlot(
    {
      df_cwq <- cwq_data_filt()

      # Quietly halt plotting until df_cwq exists and has at least 1 row
      req(df_cwq, nrow(df_cwq) > 0)

      plot_cwq_heatmap(df_cwq = df_cwq, df_storms = df_storms_filt())
    },
    res = 96
  ) |>
    bindCache(
      input$param,
      input$adj_wy,
      dowy_rng_d(),
      input$strata,
      input$stations
    )

  # Station Reference page outputs
  # Observers for station map zoom buttons
  observe({
    leafletProxy("station_map") |>
      fitBounds(
        lng1 = as.numeric(bbox_strata["xmin"]),
        lat1 = as.numeric(bbox_strata["ymin"]),
        lng2 = as.numeric(bbox_strata["xmax"]),
        lat2 = as.numeric(bbox_strata["ymax"])
      )
  }) |>
    bindEvent(input$zoom_strata)

  observe({
    leafletProxy("station_map") |>
      fitBounds(
        lng1 = as.numeric(bbox_stations["xmin"]),
        lat1 = as.numeric(bbox_stations["ymin"]),
        lng2 = as.numeric(bbox_stations["xmax"]),
        lat2 = as.numeric(bbox_stations["ymax"])
      )
  }) |>
    bindEvent(input$zoom_stations)

  # Render station map
  output$station_map <- renderLeaflet({
    build_station_map(sf_strata, sf_stations)
  })

  # Render station reactable
  output$station_table <- renderReactable({
    build_station_table(cwq_stations_c)
  })
}

# Run app
shinyApp(ui, server)
