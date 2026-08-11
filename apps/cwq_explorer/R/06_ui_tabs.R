# cwq_explorer Shiny App functions for UI tabs
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(reactable)

ui_tab_explorer <- function(dowy_adj_rng) {
  nav_panel(
    title = "Data Explorer",
    icon = bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Plot Controls",
        width = 335,
        selectInput(
          "param",
          "Parameter",
          choices = c(
            "Turbidity" = "turbidity",
            "Specific Conductance" = "sp_cond",
            "Water Temperature" = "water_temp",
            "Discharge (TF)" = "discharge",
            "Velocity (TF)" = "velocity_tf"
          )
        ),
        selectInput("adj_wy", "Adjusted Water Year", choices = character(0)),
        tags$label(
          "Day of Adjusted WY - Display Range",
          `for` = "dowy_rng",
          class = "control-label mb-0"
        ),
        div(
          style = "margin-top: -20px; margin-bottom: -6px;",
          actionLink(
            "reset_slider",
            "Reset range",
            class = "small text-decoration-none"
          )
        ),
        div(
          style = "margin-top: -10px; margin-bottom: -20px;",
          sliderInput(
            "dowy_rng",
            label = NULL,
            value = dowy_adj_rng,
            min = dowy_adj_rng[1],
            max = dowy_adj_rng[2]
          )
        ),
        hr(class = "my-1"),
        div(
          class = "mb-1",
          span(
            "STRATA & STATIONS",
            class = "fw-bold text-secondary text-uppercase small d-block mb-0"
          ),
          div(
            class = "d-flex align-items-center mb-0",
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
        div(
          style = "margin-top: -8px; margin-bottom: -8px;",
          selectizeInput(
            "strata",
            "Strata",
            choices = character(0),
            multiple = TRUE,
            options = list(
              plugins = list('remove_button'),
              placeholder = 'Select Strata'
            )
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
        plotOutput("plot", height = "100%", fill = TRUE),
        card_footer(
          class = "text-muted small",
          markdown(
            "
              **How to read this plot:** Heatmap tile values display continuous daily averages for the selected parameter.
              Vertical dashed lines indicate the first day of storm events within the adjusted water year (Sept–Aug).
            "
          )
        )
      )
    )
  )
}

ui_tab_reference <- function() {
  nav_panel(
    title = "Station Reference",
    icon = bs_icon("map"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center py-2",
          "Station Locations",
          div(
            actionButton(
              "zoom_strata",
              "Zoom Strata",
              class = "btn-sm btn-outline-secondary me-1",
              style = "padding: 0.15rem 0.4rem; font-size: 0.75rem; line-height: 1;"
            ),
            actionButton(
              "zoom_stations",
              "Zoom Stations",
              class = "btn-sm btn-outline-secondary",
              style = "padding: 0.15rem 0.4rem; font-size: 0.75rem; line-height: 1;"
            )
          )
        ),
        leafletOutput("station_map", height = "100%")
      ),
      card(
        full_screen = TRUE,
        card_header("Station Metadata Lookup"),
        reactableOutput("station_table")
      )
    )
  )
}
