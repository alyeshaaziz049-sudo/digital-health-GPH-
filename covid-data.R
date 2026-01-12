library(shiny)
library(ggplot2)
library(readr)

# Load data
covid <- read_csv("COVID19_state.csv", show_col_types = FALSE)

ui <- fluidPage(
  
  titlePanel("State-Level COVID-19 Impact Dashboard"),
  
  # ---------- TOP FILTER BAR ----------
  fluidRow(
    column(
      4,
      selectInput(
        "metric",
        "Select COVID Metric:",
        choices = c("Tested", "Infected", "Deaths")
      )
    ),
    column(
      4,
      sliderInput(
        "density",
        "Population Density:",
        min = min(covid$`Pop Density`),
        max = max(covid$`Pop Density`),
        value = c(min(covid$`Pop Density`), max(covid$`Pop Density`))
      )
    ),
    column(
      4,
      sliderInput(
        "smoking",
        "Smoking Rate:",
        min = min(covid$`Smoking Rate`),
        max = max(covid$`Smoking Rate`),
        value = c(min(covid$`Smoking Rate`), max(covid$`Smoking Rate`))
      )
    )
  ),
  
  hr(),
  
  # ---------- STORY ----------
  fluidRow(
    column(
      12,
      strong("Analytical Story:"),
      textOutput("story")
    )
  ),
  hr(),
  
  # ---------- FIRST ROW: PIE + HEATMAP ----------
  fluidRow(
    column(
      6,
      h4("Top 10 States Contribution"),
      plotOutput("piechart", height = "400px")
    ),
    column(
      6,
      h4("Heat Map: COVID Impact Across States"),
      plotOutput("heatmap", height = "400px")
    )
  ),
  
  hr(),
  
  # ---------- SECOND ROW: HISTOGRAM ----------
  fluidRow(
    column(
      12,
      h4("Distribution of Selected Metric"),
      plotOutput("histrogram", height = "400px")
    )
  ),
  
  hr(),
)

server <- function(input, output) {
  
  # ---------- FILTERED DATA ----------
  filtered_data <- reactive({
    covid[
      covid$`Pop Density` >= input$density[1] &
        covid$`Pop Density` <= input$density[2] &
        covid$`Smoking Rate` >= input$smoking[1] &
        covid$`Smoking Rate` <= input$smoking[2],
    ]
  })
  
  # ---------- PIE CHART ----------
  output$piechart <- renderPlot({
    
    top_states <- filtered_data()
    top_states <- top_states[order(-top_states[[input$metric]]), ]
    top_states <- head(top_states, 10)
    
    ggplot(top_states,
           aes(x = "", y = .data[[input$metric]], fill = State)) +
      geom_col(width = 1) +
      coord_polar("y") +
      theme_void()
  })
  
  # ---------- HEAT MAP ----------
  output$heatmap <- renderPlot({
    
    ggplot(filtered_data(),
           aes(x = State, y = input$metric, fill = .data[[input$metric]])) +
      geom_tile() +
      coord_flip() +
      labs(
        x = "State",
        y = "",
        fill = input$metric
      ) +
      theme_minimal()
  })
  
  # ---------- HISTOGRAM ----------
  output$histogram <- renderPlot({
    
    ggplot(filtered_data(),
           aes(x = .data[[input$metric]])) +
      geom_histogram(bins = 15) +
      labs(
        x = input$metric,
        y = "Number of States"
      ) +
      theme_minimal()
  })
  
  # ---------- STORY ----------
  output$story <- renderText({
    
    paste(
      "After applying filters for population density and smoking rate,",
      "the dashboard reveals substantial variation in", input$metric,
      "across U.S. states.",
      "The heat map highlights geographic concentration,",
      "the pie chart shows that a few states dominate the overall burden,",
      "and the histogram confirms a skewed distribution rather than uniform impact."
    )
  })
}

shinyApp(ui = ui, server = server)
