# Shiny App used to explore continuous WQ data for the First Flush synthesis -
# source data is the daily values dataset
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(shiny)
library(bslib)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Define functions
# Function to calculate water year from a date
add_wy <- function(x) {
  if_else(month(x) >= 10, year(x) + 1, year(x))
}

# Function to calculate day of water year from a date
add_wy_day <- function(x) {
  start_date <- as_date(
    if_else(
      month(x) >= 10,
      paste0(year(x), "-10-01"),
      paste0(year(x) - 1, "-10-01")
    )
  )
  as.integer(x - start_date + 1)
}

# Import data
df_cont_wq <- read_rds(here("data/processed/wq/turbidity_dv.rds"))

df_cont_wq_station_info <- read_rds(
  here("data/processed/wq/cont_wq_station_info_analysis.rds")
)

load(here("data/processed/storms/StormData.RData"))

# Define plot order for stations
df_cont_wq_station_info_c <- df_cont_wq_station_info |>
  filter(station_abbr %in% unique(df_cont_wq$station_abbr)) |>
  mutate(
    plt_order = case_match(
      station_abbr,
      "FPT" ~ 1,
      "GLC" ~ 11,
      "HLT" ~ 9,
      "MDM" ~ 10,
      "OBI" ~ 7,
      "ORB" ~ 8,
      "ORQ" ~ 6,
      "PPT" ~ 4,
      "RVB" ~ 2,
      "SJJ" ~ 5,
      "SJR_DWR" ~ 3,
      "TWA" ~ 12
    ),
    station_full_name = fct_reorder(station_full_name, plt_order)
  ) |>
  select(station_abbr, station_full_name)

# Prepare continuous WQ data for plots in app
df_cont_wq_c <- df_cont_wq |>
  mutate(
    wy = add_wy(date),
    dowy = add_wy_day(date)
  ) |>
  left_join(df_cont_wq_station_info_c, by = join_by(station_abbr)) |>
  filter(
    wy <= max(Sacflow_wstorms$WY),
    !month(date) %in% 6:9
  )

# Create data frame of first days of all storms
df_storms <- StormStartEnd |>
  mutate(
    wy = add_wy(FirstDay),
    dowy = add_wy_day(FirstDay),
    .keep = "none"
  ) |>
  filter(between(wy, min(df_cont_wq_c$wy), max(df_cont_wq_c$wy)))

# Define options for y-axis variables for plots
plot_y_vars <- df_cont_wq_c |>
  select(where(is.numeric) & !all_of(c("wy", "dowy"))) |>
  names()

# Define water years available for plots
plot_wy <- sort(unique(df_cont_wq_c$wy))

# Combine data for plots into a list for more efficient filtering
ls_plt_data <- lst(df_cont_wq_c, df_storms)

# Build UI
ui <- page_sidebar(
  title = "First Flush WQ Data Explorer",
  sidebar = sidebar(
    title = "Plot Controls",
    selectInput("ts_y_var", "Y variable", choices = plot_y_vars),
    selectInput("ts_wy", "Water Year", choices = plot_wy),
    radioButtons(
      "ts_facet_opt",
      "Facet y-axis Range",
      choices = c("Fixed", "Free"),
      inline = TRUE
    )
  ),
  card(plotOutput("plot_ts"))
)

# Build server
server <- function(input, output, session) {
  # Filter cwq and storms data by selected WY
  ls_plt_data_filt <- reactive({
    map(ls_plt_data, \(x) filter(x, wy == input$ts_wy))
  })

  output$plot_ts <- renderPlot(
    {
      # Create base plot
      ts_plt_base <- ls_plt_data_filt()$df_cont_wq_c |>
        ggplot(aes(x = dowy, y = .data[[input$ts_y_var]])) +
        geom_line() +
        geom_vline(
          data = ls_plt_data_filt()$df_storms,
          aes(xintercept = dowy),
          color = "red",
          linetype = 2
        ) +
        theme_bw() +
        xlab("Day of Water Year")

      # Render final plot based on facet y-axis scale options
      switch(
        input$ts_facet_opt,
        Fixed = ts_plt_base +
          facet_wrap(vars(station_full_name), labeller = label_wrap_gen()),
        Free = ts_plt_base +
          facet_wrap(
            vars(station_full_name),
            scales = "free_y",
            labeller = label_wrap_gen()
          )
      )
    },
    res = 96
  )
}

# Run app
shinyApp(ui, server)
