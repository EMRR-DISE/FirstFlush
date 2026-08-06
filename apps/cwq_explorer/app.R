# Shiny App used to explore continuous WQ data for the First Flush synthesis -
# source data is the daily values dataset
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
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
  dplyr::if_else(
    lubridate::month(d) >= 9,
    lubridate::year(d) + 1,
    lubridate::year(d)
  )
}

# Function to calculate day of adjusted water year from a date
assign_wy_adj_day <- function(d) {
  start_date <- lubridate::as_date(
    dplyr::if_else(
      lubridate::month(d) >= 9,
      paste0(lubridate::year(d), "-09-01"),
      paste0(lubridate::year(d) - 1, "-09-01")
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
  title = "First Flush CWQ Data Explorer",
  sidebar = filter_controls_ui("filters", dowy_adj_rng),
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

# Build server
server <- function(input, output, session) {
  filt <- filter_controls_server(
    "filters",
    cwq_data_c,
    df_storms,
    strata_order,
    station_order,
    dowy_adj_rng
  )

  output$plot <- renderPlot(
    {
      df_cwq <- filt$cwq_data_filt()

      # Quietly halt plotting until df_cwq exists and has at least 1 row
      req(df_cwq, nrow(df_cwq) > 0)

      plot_cwq_heatmap(
        df_cwq = filt$cwq_data_filt(),
        df_storms = filt$df_storms()
      )
    },
    res = 96
  ) |>
    bindCache(filt$cache_keys())
}

# Run app
shinyApp(ui, server)
