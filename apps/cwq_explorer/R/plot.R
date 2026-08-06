# cwq_explorer Shiny App plot code
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(ggplot2)

# CWQ Heatmap
plot_cwq_heatmap <- function(df_cwq, df_storms) {
  df_cwq |>
    ggplot(aes(
      x = dowy_adj,
      y = forcats::fct_rev(station_abbr),
      fill = value
    )) +
    geom_tile() +
    geom_vline(
      data = df_storms,
      aes(xintercept = dowy_adj),
      color = "grey95",
      linewidth = 0.7,
      linetype = 2
    ) +
    scale_fill_viridis_c(option = "plasma", labels = scales::label_comma()) +
    scale_x_continuous(
      name = "Day of adjusted WY",
      breaks = scales::pretty_breaks(10),
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
}
