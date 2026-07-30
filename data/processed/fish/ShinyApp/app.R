
library(shiny)
library(dplyr)
library(ggplot2)

## ---- Load data ---- ####
accumcurve     <- readRDS("accumcurve.rds")
storm_timeline <- readRDS("storm_timeline.rds")
allcovariates  <- readRDS("allcovariates.rds")

## ---- Precompute phase_bands once, outside the app ---- ####
plot_max <- max(accumcurve$LFSDOWY_adj, na.rm = TRUE)

phase_bands <- storm_timeline %>%
  filter(!is.na(FFDOWY_adj), !is.na(PeakDOWY_adj)) %>%
  transmute(
    WY_adj,
    NextStormDOWY_adj_capped = pmin(NextStormDOWY_adj, plot_max),
    FFDOWY_adj, PeakDOWY_adj
  ) %>%
  { bind_rows(
      transmute(., WY_adj, StormPhase = "Pre FF",
                xmin = 0, xmax = FFDOWY_adj),
      transmute(., WY_adj, StormPhase = "During FF",
                xmin = FFDOWY_adj, xmax = PeakDOWY_adj),
      transmute(., WY_adj, StormPhase = "Post FF Pre Other Storms",
                xmin = PeakDOWY_adj, xmax = NextStormDOWY_adj_capped),
      transmute(., WY_adj, StormPhase = "During and After Post Storms",
                xmin = NextStormDOWY_adj_capped, xmax = plot_max)
  ) } %>%
  filter(xmax > xmin) %>%
  mutate(StormPhase = factor(
    StormPhase,
    levels = c("Pre FF", "During FF", "Post FF Pre Other Storms", "During and After Post Storms")
  ))

## ---- Precompute storm event lines (one row per storm) ---- ####
storm_lines <- allcovariates %>%
  distinct(WY_adj, StormDate, StormDOWY_adj, AfterFirstFlush) %>%
  filter(!is.na(StormDOWY_adj))

## ---- UI ---- ####
ui <- fluidPage(
  titlePanel("Storm Phase Timing vs. Longfin Catch"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "source", "Source (Survey):",
        choices = sort(unique(accumcurve$Source)),
        selected = sort(unique(accumcurve$Source))[1]
      ),
      selectInput(
        "years", "Water Year(s):",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        "stratum", "Stratum:",
        choices = sort(unique(accumcurve$Stratum)),
        selected = sort(unique(accumcurve$Stratum))[1]
      ),
      helpText("Points show fish catch, colored by assigned phase.",
               "Background bands show the actual FF / Peak / Next-storm timing.",
               "Solid black line = First Flush. Dashed black lines = subsequent storms.")
    ),
    mainPanel(
      plotOutput("phasePlot", height = "600px")
    )
  )
)

## ---- Server ---- ####
server <- function(input, output, session) {

  observeEvent(input$source, {
    years_for_source <- accumcurve %>%
      filter(Source == input$source) %>%
      pull(WY_adj) %>%
      unique() %>%
      sort()

    updateSelectInput(
      session, "years",
      choices = years_for_source,
      selected = head(years_for_source, 4)
    )
  })

  output$phasePlot <- renderPlot({
    req(input$years, input$stratum, input$source)

    bands_sub <- phase_bands %>% filter(WY_adj %in% input$years)
    storms_sub <- storm_lines %>% filter(WY_adj %in% input$years)
    points_sub <- accumcurve %>%
      filter(
        WY_adj %in% input$years,
        Stratum == input$stratum,
        Source == input$source,
        !is.na(StormPhase)
      )

    ggplot() +
      geom_rect(
        data = bands_sub,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = StormPhase),
        alpha = 0.2
      ) +
      geom_vline(
        data = storms_sub,
        aes(xintercept = StormDOWY_adj, linetype = AfterFirstFlush),
        color = "black", linewidth = 0.7
      ) +
      scale_linetype_manual(
        values = c(`FALSE` = "solid", `TRUE` = "dashed"),
        labels = c(`FALSE` = "First Flush", `TRUE` = "Subsequent Storm"),
        name = "Storm Event"
      ) +
      geom_point(
        data = points_sub,
        aes(x = LFSDOWY_adj, y = ProportionCaught, color = StormPhase),
        size = 2
      ) +
      facet_wrap(~ WY_adj, scales = "free_x") +
      labs(
        title = paste0("Storm phase timing vs. longfin catch — ", input$stratum, " (", input$source, ")"),
        x = "Day of Adjusted Water Year", y = "Proportion Caught (cumulative)",
        fill = "Phase (background)", color = "Phase (fish catch)"
      ) +
      theme_minimal(base_size = 26)
  })
}

## ---- Launch ---- ####
shinyApp(ui, server)

