## COVID-19 State level Data Visualization:
-------------------------------------------
This repository contains a simple **R Shiny Application** which can be used to explore COVID-19 data at the state level. It shows COVID-19 infections, deaths, ICU beds, and pollution using different plots with incorporated filters.

The Shiny App will show:

* COVID-19 infection and death rates with changing population and income levels

* ICU beds availability in different states

* View pollution distribution across selected states

sliders and filters are used to explore the data

All charts are interactive, so users can hover over them to see values.

------------------------------

## Dataset

CSV file called `COVID19_state.csv`. It includes:

* State name
* Number of COVID-19 tests, infections, and deaths
* Population size
* Income 
* ICU beds
* Pollution levels
* other indicators

new values including infection rate and death rate per 100,000 people are calculated in the code.

-----------------------------------

## Packages

The project is made in **R** using following packages:

* pacman __ for package management
* shiny __ for building the interactive web application
* tidyverse __ for data manipulation and visualization
* readr __ for reading CSV files
* dplyrfor __ data transformation
* ggplot2 __ for creating plots
* janitor __ for cleaning variable names
* plotly __ for interactive visualizations
 
--------------------------------------

## How to Run the App

1. Download this repository.
2. Open the project in **RStudio**.
3. install all the required packages.
4. Make sure `COVID19_state.csv` is in the same folder as the app file.
5. Run the R script.
6. The Shiny app will open in the browser.

---------------------------------------------

## Files in This Repository

```
COVID19_state.csv    # Dataset
app.R                # Shiny app code
README.md            # Project description
```

------------------------------------------

I have created this project for my examination.

