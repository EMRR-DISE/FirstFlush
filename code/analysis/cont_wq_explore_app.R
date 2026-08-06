# Shiny App used to explore continuous WQ data for the First Flush synthesis -
# source data is the daily values dataset
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)
library(shiny)
library(bslib)
library(qs2)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Define functions
# Assign adjusted calendar year from a date. An adjusted calendar year is defined as
# September-August, with Sept-Dec of the previous calendar year included with the following year
assign_wy_adj <- function(d) {
  if_else(month(d) >= 9, year(d) + 1, year(d))
}

# Function to calculate day of adjusted water year from a date
assign_wy_adj_day <- function(d) {
  start_date <- as_date(
    if_else(
      month(d) >= 9,
      paste0(year(d), "-09-01"),
      paste0(year(d) - 1, "-09-01")
    )
  )
  as.integer(d - start_date + 1)
}

# Import data
fp_processed_data <- here("data/processed")
cwq_data <- qd_read(file.path(fp_processed_data, "wq/cwq_data_dv_all.qdata"))
cwq_stations <- readRDS(
  file.path(fp_processed_data, "wq/cwq_station_metadata.rds")
)
load(file.path(fp_processed_data, "storms/StormData.RData"))

# Define plot order for stations and strata
strata_order <- c(
  "Sacramento River Mainstem",
  "Sacramento River Deep Water Ship Channel",
  "Cache Slough and Liberty Island",
  "North and South Forks Mokelumne River",
  "Out of Bounds",
  "South Delta",
  "Confluence",
  "Suisun and Honker Bays",
  "Suisun Marsh and Montezuma Slough",
  "San Pablo Bay and Carquinez Strait"
)

station_order <- c(
  "FPT",
  "SRH",
  "GES",
  "MIR",
  "SXS",
  "C62",
  "DWS",
  "RYI-RYF",
  "UCS",
  "LIB",
  "NMR",
  "SMR",
  "LPS",
  "MOK",
  "SJR",
  "MSD",
  "TWA",
  "VCU",
  "ORB",
  "MDM",
  "OBI",
  "HLT",
  "ORQ",
  "SJG",
  "PPT",
  "RVB",
  "SJJ",
  "ANH",
  "MAL",
  "HON",
  "RYC",
  "GZL",
  "NSL",
  "BDL",
  "FMB",
  "GOD",
  "SBR",
  "MRZ",
  "CRQ"
)

# Prepare continuous WQ data for plots
cwq_data_c <- cwq_data |>
  # Consolidate discharge and discharge_tf
  mutate(discharge = coalesce(discharge_tf, discharge)) |>
  select(-c(discharge_tf, ssc)) |>
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value",
    values_drop_na = TRUE
  ) |>
  group_by(station_abbr, parameter) |>
  complete(date = seq(min(date), max(date), by = "1 day")) |>
  ungroup() |>
  left_join(
    distinct(cwq_stations, station_abbr, stratum),
    by = join_by(station_abbr)
  ) |>
  mutate(
    stratum = if_else(
      station_abbr == "RYI-RYF",
      "Sacramento River Deep Water Ship Channel",
      stratum
    ),
    station_abbr = factor(station_abbr, levels = station_order),
    stratum = factor(stratum, levels = strata_order),
    wy_adj = assign_wy_adj(date),
    dowy_adj = assign_wy_adj_day(date)
  ) |>
  filter(
    between(wy_adj, min(Sacflow_wstorms$WY), max(Sacflow_wstorms$WY)),
    !month(date) %in% 6:8
  )

# Create data frame of first days of all storms
df_storms <- StormStartEnd |>
  mutate(
    wy_adj = assign_wy_adj(FirstDay),
    dowy_adj = assign_wy_adj_day(FirstDay),
    .keep = "none"
  ) |>
  filter(between(wy_adj, min(cwq_data_c$wy_adj), max(cwq_data_c$wy_adj)))

# Define range of dowy_adj for slider defaults
dowy_adj_rng <- range(cwq_data_c$dowy_adj)

# Build UI
ui <- page_sidebar(
  title = "First Flush WQ Data Explorer",
  sidebar = sidebar(
    title = "Plot Controls",
    width = 335,
    selectInput(
      "param",
      "Parameter",
      choices = c(
        "Discharge (TF)" = "discharge",
        "Specific Conductance" = "sp_cond",
        "Turbidity" = "turbidity",
        "Velocity (TF)" = "velocity_tf",
        "Water Temperature" = "water_temp"
      )
    ),
    selectInput(
      "adj_wy",
      "Adjusted Water Year",
      choices = character(0)
    ),
    sliderInput(
      "dowy_rng",
      "Day of Adjusted WY - Display Range",
      value = dowy_adj_rng,
      min = dowy_adj_rng[1],
      max = dowy_adj_rng[2]
    ),
    actionButton(
      "reset_slider",
      "Reset Slider Range",
      class = "btn-sm btn-outline-secondary w-100 mb-1"
    ),
    hr(class = "my-2"),
    div(
      class = "mb-1",
      span(
        "Strata & Stations",
        class = "fw-bold text-secondary text-uppercase small d-block mb-1"
      ),
      div(
        class = "d-flex align-items-center mb-1",
        actionLink(
          "select_all",
          "Select all",
          class = "small text-decoration-none me-2"
        ),
        span("|", class = "text-muted small me-2"),
        actionLink(
          "remove_all",
          "Clear all",
          class = "small text-danger text-decoration-none"
        )
      )
    ),
    selectizeInput(
      "strata",
      "Strata",
      choices = character(0),
      multiple = TRUE,
      options = list(
        plugins = list('remove_button'),
        placeholder = 'Select Strata'
      )
    ),
    selectizeInput(
      "stations",
      "Stations",
      choices = character(0),
      multiple = TRUE,
      options = list(
        plugins = list('remove_button'),
        placeholder = 'Select Stations'
      )
    )
  ),
  card(
    full_screen = TRUE,
    min_height = "600px",
    plotOutput("plot", height = "100%", fill = TRUE)
  )
)

# Build server
server <- function(input, output, session) {
  # Reactive value to store the previously selected strata
  prev_strata <- reactiveVal(character(0))

  cwq_data_param <- reactive({
    cwq_data_c |> filter(parameter == input$param)
  })

  # Reset adj_wy choices when parameter is changed, preserving current selection if available
  observe({
    adj_wy_choices <- sort(unique(cwq_data_param()$wy_adj))

    # Check if current selection exists in the new choices
    selected_wy <- if (isolate(input$adj_wy) %in% adj_wy_choices) {
      isolate(input$adj_wy)
    } else {
      max(adj_wy_choices)
    }

    updateSelectInput(
      session,
      "adj_wy",
      choices = adj_wy_choices,
      selected = selected_wy
    )
  })

  # Reset the Day of Adjusted WY slider to display all values when adj_wy is changed or
  # explicit button click
  observe({
    updateSliderInput(session, "dowy_rng", value = dowy_adj_rng)
  }) |>
    bindEvent(input$adj_wy, input$reset_slider)

  # Reactive expression for slider input
  dowy_rng_raw <- reactive({
    input$dowy_rng
  })

  # Debounced slider value: waits 400ms after the user stops dragging
  dowy_rng_d <- debounce(dowy_rng_raw, 400)

  # Filter cwq and storms data by selected WY and DOWY range
  ls_data_filt_wy <- reactive({
    cwq_data_param <- cwq_data_param()
    rng <- dowy_rng_d()

    map(
      lst(cwq_data_param, df_storms),
      \(x) {
        filter(
          x,
          wy_adj == input$adj_wy,
          between(dowy_adj, rng[1], rng[2])
        )
      }
    )
  })

  # Populate strata choices, keeping valid previous selections
  observe({
    active_strata <- as.character(unique(
      ls_data_filt_wy()$cwq_data_param$stratum
    ))
    # Preserve strata_order by intersecting defined order with active values
    strata_choices <- intersect(strata_order, active_strata)
    current_strata <- as.character(isolate(input$strata))

    # Retain selected strata that still exist in the new dataset
    valid_selected_strata <- intersect(current_strata, strata_choices)

    selected_strata <- if (length(valid_selected_strata) > 0) {
      valid_selected_strata
    } else {
      strata_choices
    }

    updateSelectizeInput(
      session,
      "strata",
      choices = strata_choices,
      selected = selected_strata
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

    df_current_strata <- cwq_data_strata()
    active_stations <- as.character(unique(df_current_strata$station_abbr))
    # Preserve station_order by intersecting defined order with active values
    station_choices <- intersect(station_order, active_stations)

    current_stations <- as.character(isolate(input$stations))
    old_strata <- as.character(isolate(prev_strata()))
    new_strata <- as.character(input$strata)

    # Identify strata that were newly added in this cycle
    added_strata <- setdiff(new_strata, old_strata)

    # Get all stations belonging to the newly added strata
    new_stations_for_added_strata <- df_current_strata |>
      filter(stratum %in% added_strata) |>
      pull(station_abbr) |>
      as.character() |>
      unique()

    # Retain existing valid stations and append stations from newly added strata
    valid_selected_stations <- intersect(current_stations, station_choices)

    n_existing <- length(valid_selected_stations)
    n_added <- length(new_stations_for_added_strata)

    selected_stations <- if (n_existing > 0 || n_added > 0) {
      # Maintain factor order for the combined selections
      intersect(
        station_order,
        union(valid_selected_stations, new_stations_for_added_strata)
      )
    } else {
      station_choices
    }

    # Update the tracking reactive value for the next event
    prev_strata(new_strata)

    freezeReactiveValue(input, "stations")
    updateSelectizeInput(
      session,
      "stations",
      choices = station_choices,
      selected = selected_stations
    )
  })

  # Handle "Select All" button click
  observe({
    active_strata <- as.character(unique(
      ls_data_filt_wy()$cwq_data_param$stratum
    ))
    active_stations <- as.character(unique(
      ls_data_filt_wy()$cwq_data_param$station_abbr
    ))

    all_strata <- intersect(strata_order, active_strata)
    all_stations <- intersect(station_order, active_stations)

    updateSelectizeInput(session, "strata", selected = all_strata)
    freezeReactiveValue(input, "stations")
    updateSelectizeInput(session, "stations", selected = all_stations)
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
    cwq_data_strata() |> filter(station_abbr %in% input$stations)
  })

  output$plot <- renderPlot(
    {
      req(input$strata, input$stations)
      cwq_data_filt() |>
        ggplot(aes(x = dowy_adj, y = fct_rev(station_abbr), fill = value)) +
        geom_tile() +
        geom_vline(
          data = ls_data_filt_wy()$df_storms,
          aes(xintercept = dowy_adj),
          color = "grey95",
          linewidth = 0.7,
          linetype = 2
        ) +
        scale_fill_viridis_c(option = "plasma", labels = label_comma()) +
        scale_x_continuous(
          name = "Day of adjusted WY",
          breaks = pretty_breaks(10),
          expand = expansion()
        ) +
        scale_y_discrete(name = "Station", expand = expansion()) +
        facet_wrap(
          vars(stratum),
          ncol = 1,
          scales = "free_y",
          space = "free_y"
        ) +
        theme_bw(base_size = 12)
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
}

# Run app
shinyApp(ui, server)
