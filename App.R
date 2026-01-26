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

covid_raw <- read_csv("COVID19_state.csv")     #file containing covid-19 state level data

#-------CLEAN DATA-------

covid_clean <- covid_raw %>%       #cleaning the file containing original data.
  
  clean_names() %>%              #to convert column names to lowercase and remove spaces
  
  mutate(                        #to create new variables
    
    infection_rate = infected / population * 100000,  #infected cases per 100,000 population
    
    death_rate = deaths / population * 100000         # deaths per 100,00 population
  ) %>%
  select(                         
    state, tested, infected, deaths, population,
    infection_rate, death_rate, income, gdp,
    unemployment, icu_beds, pollution, urban
  ) %>%                                      #to keep values needed for visualization
  
  drop_na()                   #to remove rows with missing values

#-------SHINY USER INTERFACE---------

ui <- navbarPage(             #to create multiple tabs
  
  title = "COVID-19 Interactive Dashboard",
  
  # ---- TAB 1: Infection vs Death ----
  tabPanel(
    "Infection vs Death",
    sidebarLayout(             
      sidebarPanel(           
        sliderInput(          #filter data based on income range
          
          "income_filter",
          "Income Range:",
          min = min(covid_clean$income),    #minimum income value
          
          max = max(covid_clean$income),    #maximum income value
          
          value = range(covid_clean$income)  
        ),
        sliderInput(          #filter states by population size
          
          "population_filter",
          "Population Range:",
          min = min(covid_clean$population), 
          max = max(covid_clean$population),
          value = range(covid_clean$population)
        )
      ),
      mainPanel(
        plotlyOutput("scatter_plot")   #the output plot
      )
    )
  ),
  
  # ---- TAB 2: ICU Capacity Bar Chart ----
  tabPanel(
    "ICU Capacity",
    sidebarLayout(
      sidebarPanel(
        selectInput(            #to choose various states
          
          "state_filter",
          "Select States:",
          choices = unique(covid_clean$state),   #any state which is available
          multiple = TRUE,
          selected = unique(covid_clean$state)[1:5]    #choosing 5 for default use
        ),
        sliderInput(              #to filter ICU beds
          
          "icu_filter",
          "ICU Beds Range:",
          min = min(covid_clean$icu_beds),
          max = max(covid_clean$icu_beds),
          value = range(covid_clean$icu_beds)
        )
      ),
      mainPanel(
        plotlyOutput("icu_bar")    #output chart
      )
    )
  ),
  
  # ---- TAB 3: Pollution Pie Chart ----
  tabPanel(
    "Pollution Pie Chart",
    sidebarLayout(
      sidebarPanel(
        selectInput(            #to select states
          
          "state_filter_pie",
          "Select States:",
          choices = unique(covid_clean$state),
          multiple = TRUE,
          selected = unique(covid_clean$state)[1:5]     # 5 states as default
        ),
        sliderInput(            #to filter pollution values
          
          "pollution_filter",
          "Pollution Range:",
          min = min(covid_clean$pollution),
          max = max(covid_clean$pollution),
          value = range(covid_clean$pollution)
        )
      ),
      mainPanel(
        plotlyOutput("pollution_pie")     #output chart
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
    
    p <- ggplot(                   #to create scatter plot using ggplot2
      
      data_scatter, aes(x = infection_rate, y = death_rate)) +
      geom_point(color = "firebrick", size = 3) +
      labs(
        title = "Infection Rate vs Death Rate",
        x = "Infection Rate (per 100,000)",
        y = "Death Rate (per 100,000)"
      ) +
      theme_minimal()             #for clean visuals
    
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
    
    p <- ggplot(                 #create bar graph
      
      data_icu, aes(x = state, y = icu_beds)) +
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
      group_by(state) %>%       #same states in one group   
      summarise(
        total_pollution = sum(pollution, na.rm = TRUE),  #na.rm to remove any missing values
        .groups = "drop"        #to dismiss grouping 
      )
    
    plot_ly(          #to create pie chart using plotly
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

shinyApp(ui = ui, server = server)    #to launch the app
