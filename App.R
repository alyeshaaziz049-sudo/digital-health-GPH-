#-------INSTALL PACKAGES------

pacman::p_load(
  shiny,
  tidyverse,
  readr,
  ggplot2,
  dplyr,
  janitor,
  plotly
)

#------LOAD DATA---------

covid_raw <- read_csv("COVID19_state.csv")

#-------CLEAN DATA-------

covid_clean <- covid_raw %>%
  clean_names() %>%
  mutate(
    infection_rate = infected / population * 100000,
    death_rate = deaths / population * 100000
  ) %>%
  select(
    state, tested, infected, deaths, population,
    infection_rate, death_rate, income, gdp,
    unemployment, icu_beds, pollution, urban
  ) %>%
  drop_na()

#-------SHINY USER INTERFACE---------

ui <- navbarPage(
  title = "COVID-19 Interactive Dashboard",
  
  # ---- TAB 1: Infection vs Death ----
  tabPanel(
    "Infection vs Death",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "income_filter",
          "Income Range:",
          min = min(covid_clean$income),
          max = max(covid_clean$income),
          value = range(covid_clean$income)
        ),
        sliderInput(
          "population_filter",
          "Population Range:",
          min = min(covid_clean$population),
          max = max(covid_clean$population),
          value = range(covid_clean$population)
        )
      ),
      mainPanel(
        plotlyOutput("scatter_plot")
      )
    )
  ),
  
  # ---- TAB 2: ICU Capacity Bar Chart ----
  tabPanel(
    "ICU Capacity",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "state_filter",
          "Select States:",
          choices = unique(covid_clean$state),
          multiple = TRUE,
          selected = unique(covid_clean$state)[1:5]
        ),
        sliderInput(
          "icu_filter",
          "ICU Beds Range:",
          min = min(covid_clean$icu_beds),
          max = max(covid_clean$icu_beds),
          value = range(covid_clean$icu_beds)
        )
      ),
      mainPanel(
        plotlyOutput("icu_bar")
      )
    )
  ),
  
  # ---- TAB 3: Pollution Pie Chart ----
  tabPanel(
    "Pollution Pie Chart",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "state_filter_pie",
          "Select States:",
          choices = unique(covid_clean$state),
          multiple = TRUE,
          selected = unique(covid_clean$state)[1:5]
        ),
        sliderInput(
          "pollution_filter",
          "Pollution Range:",
          min = min(covid_clean$pollution),
          max = max(covid_clean$pollution),
          value = range(covid_clean$pollution)
        )
      ),
      mainPanel(
        plotlyOutput("pollution_pie")
      )
    )
  )
)

#-------SHINY SERVER-------

server <- function(input, output) {
  
  # TAB 1: Scatter plot Infection vs Death
  output$scatter_plot <- renderPlotly({
    data_scatter <- covid_clean %>%
      filter(
        income >= input$income_filter[1],
        income <= input$income_filter[2],
        population >= input$population_filter[1],
        population <= input$population_filter[2]
      )
    
    p <- ggplot(data_scatter, aes(x = infection_rate, y = death_rate)) +
      geom_point(color = "firebrick", size = 3) +
      labs(
        title = "Infection Rate vs Death Rate",
        x = "Infection Rate (per 100,000)",
        y = "Death Rate (per 100,000)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # TAB 2: ICU Capacity Bar Chart
  output$icu_bar <- renderPlotly({
    data_icu <- covid_clean %>%
      filter(
        state %in% input$state_filter,
        icu_beds >= input$icu_filter[1],
        icu_beds <= input$icu_filter[2]
      )
    
    p <- ggplot(data_icu, aes(x = state, y = icu_beds)) +
      geom_col(fill = "darkgreen") +
      labs(
        title = "ICU Beds by State",
        x = "State",
        y = "Number of ICU Beds"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # TAB 3: Pollution Pie Chart
  output$pollution_pie <- renderPlotly({
    
    data_pie <- covid_clean %>%
      filter(
        state %in% input$state_filter_pie,
        pollution >= input$pollution_filter[1],
        pollution <= input$pollution_filter[2]
      ) %>%
      group_by(state) %>%
      summarise(
        total_pollution = sum(pollution, na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(
      data = data_pie,
      labels = ~state,
      values = ~total_pollution,
      type = "pie",
      textinfo = "percent",     
      hoverinfo = "label+value+percent"
    ) %>%
      layout(
        title = "Pollution Distribution by State",
        showlegend = TRUE
      )
  })
}

# 6. RUN THE SHINY APP

shinyApp(ui = ui, server = server)
