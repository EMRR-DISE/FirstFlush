# Functions used in the targets workflow for processing continuous water quality data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Read the stations metadata YAML and convert it into a flat tibble for branching
read_cwq_stations_meta <- function(yaml_file) {
  yaml_data <- yaml::read_yaml(yaml_file)

  # Bind the list elements into rows of a data frame
  yaml_data$stations |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      parameters = purrr::imap(parameters, \(x, idx) {
        tibble::tibble(parameter_name = idx, parameter_code = as.character(x))
      })
    ) |>
    tidyr::unnest(cols = parameters, keep_empty = TRUE)
}
