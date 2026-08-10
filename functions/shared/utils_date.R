# Utility functions for date operations used throughout this repo
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

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
