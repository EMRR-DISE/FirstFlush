library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)

# --- Sample Data Generation ---
# Simulating Sacramento River flow (cfs)
set.seed(123)
dates <- seq(as.Date("2023-10-01"), as.Date("2024-04-01"), by="day")
n <- length(dates)
flow <- 8000 + cumsum(rnorm(n, mean=50, sd=500))
flow <- pmax(flow, 4000) # Ensure no negative flows
df <- data.frame(Date = dates, Discharge = flow)

# --- UI ---
ui <- fluidPage(
  titlePanel("River Discharge Dynamics: Rising vs. Descending"),

  sidebarLayout(
    sidebarPanel(
      helpText("Define criteria for flow categorization."),

      sliderInput("threshold", "Rate of Change (%)",
                  min = 1, max = 50, value = 10, post = "%"),

      sliderInput("duration", "Duration (Days)",
                  min = 1, max = 7, value = 3),

      hr(),
      p("Red = Rising Flow"),
      p("Blue = Descending Flow"),
      p("Grey = Stable/Baseline")
    ),

    mainPanel(
      plotOutput("dischargePlot")
    )
  )
)

# --- Server ---
server <- function(input, output) {

  processed_data <- reactive({
    df %>%
      mutate(
        # Calculate % change over the user-defined duration
        lag_flow = lag(Discharge, input$duration),
        pct_change = ((Discharge - lag_flow) / lag_flow) * 100,

        # Categorize based on sliders
        status = case_when(
          pct_change >= input$threshold ~ "Rising",
          pct_change <= -input$threshold ~ "Descending",
          TRUE ~ "Stable"
        )
      ) %>%
      na.omit()
  })

  output$dischargePlot <- renderPlot({
    ggplot(processed_data(), aes(x = Date, y = Discharge, group = 1)) +
      geom_line(color = "grey80", size = 1) +
      geom_point(aes(color = status), size = 2) +
      scale_color_manual(values = c("Rising" = "red",
                                    "Descending" = "blue",
                                    "Stable" = "grey50")) +
      theme_minimal() +
      labs(y = "Discharge (cfs)", x = "Date", color = "Flow Trend") +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)
