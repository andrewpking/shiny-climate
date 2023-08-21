library(shiny)
library(plotly)

graph_page <- tabPanel(
  "Visualizing CO2",
  h1("Visualizing CO2 Emissions"),
  h2("Cumulative CO2 Emissions Growth by Continent"),
  p("cumulative CO2 growth by continent broken down by individual countries,
    along with average temperatures for each country in the given time frame.
    The countries shown and time frame may be adjusted. The goal of this
    visualization is to show which regions are producing the most CO2 and
    how climate change is affecting them."),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "country_year",
        label = "Select Year",
        min = 1856,
        max = 2013,
        value = c(1856, 2013),
        step = 1,
        sep = ""
      ),
      selectInput(
        inputId = "country_name",
        label = "Select Regions",
        multiple = TRUE,
        choices = NULL,
        selected = 1
      )
    ),
    mainPanel(
      div(
        class = "chart",
        tabsetPanel(
          tabPanel("Plot", plotlyOutput(outputId = "co2_plotly")),
          tabPanel("Table", tableOutput("co2Table"))
        )
      )
    )
  )
)

summary_page <- tabPanel(
  "Summary",
  h1("Summary"),
  h2("Introduction"),
  p("Creating visualizations comparing climate change data on carbon emissions
    and global temperature changes over time using data that has already been 
    collected. Goal of establishing relationship between CO2 output over time 
    with climate change, along with understanding which regions are most 
    responsible for and impacted by climate change."),
  h2("Dataset"),
  p("Two datasets were used, both of which can be found on Kaggle. One is CO2 
    Emissions over time by country, the other is Temperature data over time 
    from Berkeley Earth. They can be found here:"),
  div(
    class = "list",
    tags$ul(
      tags$li(
        a(
          "2022 Complete CO2 Emissions", 
          href = paste0(
            "https://www.kaggle.com/datasets/dustinober/",
            "2022-complete-co2-emissions?select=owid-co2-data.csv")
        )
      ),
      tags$li(
        a(
          "Climate Change: Earth Surface Temperature Data", 
          href = paste0(
            "https://www.kaggle.com/datasets/",
            "berkeleyearth/climate-change-earth-",
            "surface-temperature-data?select=GlobalTemperatures.csv")
        )
      )
    )
  ),
  p("These country temperature data set from Berkeley Earth and the Complete 
    CO2 Emissions data set are merged together to create a more complete data 
    set that includes both CO2 growth and temperature change. This aggregated 
    data set is used by the app to create visualizations that would not be 
    possible without merging the data together."),
  h2("Results"),
  p("When visualizing CO2 output by continent with countries visible, three 
    major countries are visible on this chart. China, Germany, and The United 
    States are the three biggest global polluters. Unsurprisingly, these 
    countries also have the largest GDP's globally. Additionally most of the 
    carbon growth globally has happened between 1930 and the present, with an 
    upward trend for each year."),
  h2("Further Questions"),
  p("It is paramount to explore in further depth the relationship between GDP 
    and emission of climate change causing chemicals. It is possible that 
    existing economic activity could be a direct cause of climate change. 
    Policymakers may need to consider new measurements of economic growth that 
    consider climate change as a threat to global wellbeing and to measure 
    economies not only by quantity of economic output but also quality.")
)

ui <- fluidPage(
  includeCSS("www/styles.css"),
  navbarPage(
    "Climate Change Analysis",
    graph_page,
    summary_page
  )
)