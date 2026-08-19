# Utility functions for converting CWQ stations and strata to factors with custom orders used
# throughout this repo
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Convert station abbreviations to factor using custom station order
convert_fct_station_abbr <- function(vec) {
  # Define custom station order
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

  # Convert vector to factor
  factor(vec, levels = station_order)
}

# Convert stratum to factor using custom strata order
convert_fct_stratum <- function(vec) {
  # Define strata order
  strata_order <- c(
    "Sacramento River Mainstem",
    "Sacramento River Deep Water Ship Channel",
    "Cache Slough and Liberty Island",
    "North and South Forks Mokelumne River",
    "San Joaquin River upstream of Delta",
    "South Delta",
    "Confluence",
    "Suisun and Honker Bays",
    "Suisun Marsh and Montezuma Slough",
    "San Pablo Bay and Carquinez Strait"
  )

  # Convert vector to factor
  factor(vec, levels = strata_order)
}
