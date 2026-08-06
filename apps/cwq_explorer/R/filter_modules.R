# cwq_explorer Shiny App filter modules
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(shiny)
library(bslib)

# UI module for plot filter controls
filter_controls_ui <- function(id, dowy_adj_rng) {
  ns <- NS(id)
  sidebar(
    title = "Plot Controls",
    width = 335,
    selectInput(
      ns("param"),
      "Parameter",
      choices = c(
        "Turbidity" = "turbidity",
        "Specific Conductance" = "sp_cond",
        "Water Temperature" = "water_temp",
        "Discharge (TF)" = "discharge",
        "Velocity (TF)" = "velocity_tf"
      )
    ),
    selectInput(ns("adj_wy"), "Adjusted Water Year", choices = character(0)),
    sliderInput(
      ns("dowy_rng"),
      "Day of Adjusted WY - Display Range",
      value = dowy_adj_rng,
      min = dowy_adj_rng[1],
      max = dowy_adj_rng[2]
    ),
    actionButton(
      ns("reset_slider"),
      "Reset Slider Range",
      class = "btn-sm btn-outline-secondary w-100 mb-1"
    ),
    hr(class = "my-2"),
    div(
      class = "mb-1",
      span(
        "STRATA & STATIONS",
        class = "fw-bold text-secondary text-uppercase small d-block mb-0"
      ),
      div(
        class = "d-flex align-items-center mb-1",
        actionLink(
          ns("select_all"),
          "Select all",
          class = "small text-decoration-none me-2"
        ),
        span("|", class = "text-muted small me-2"),
        actionLink(
          ns("remove_all"),
          "Clear all",
          class = "small text-danger text-decoration-none"
        )
      )
    ),
    selectizeInput(
      ns("strata"),
      "Strata",
      choices = character(0),
      multiple = TRUE,
      options = list(
        plugins = list('remove_button'),
        placeholder = 'Select Strata'
      )
    ),
    selectizeInput(
      ns("stations"),
      "Stations",
      choices = character(0),
      multiple = TRUE,
      options = list(
        plugins = list('remove_button'),
        placeholder = 'Select Stations'
      )
    )
  )
}

# Server Module for plot filter controls and interaction with UI
filter_controls_server <- function(
  id,
  cwq_data,
  df_storms,
  strata_order,
  station_order,
  dowy_adj_rng
) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive value to store the previously selected strata
    prev_strata <- reactiveVal(character(0))

    # Add debounce to dowy_rng slider
    # Debounced slider value: waits 400ms after the user stops dragging
    dowy_rng_d <- reactive({
      input$dowy_rng
    }) |>
      debounce(400)

    # Filter cwq data to parameter
    cwq_data_param <- reactive({
      cwq_data |> filter(parameter == input$param)
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

    # Populate station choices, keeping valid previous selections, and auto-adding stations for
    # newly selected strata
    observe({
      req(input$strata)
      station_update <- compute_station_updates(
        current_strata = input$strata,
        prev_strata = isolate(prev_strata()),
        current_stations = isolate(input$stations),
        df_filt = cwq_data_strata(),
        station_order = station_order
      )

      # Update the tracking reactive value for the next event
      prev_strata(as.character(input$strata))

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

    # Return objects for server
    return(list(
      cwq_data_filt = cwq_data_filt,
      df_storms = df_storms_filt,
      cache_keys = reactive({
        list(
          input$param,
          input$adj_wy,
          dowy_rng_d(),
          input$strata,
          input$stations
        )
      })
    ))
  })
}
