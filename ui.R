library(shiny)
library(plotly)

graph_page <- tabPanel(
  "Visualizing CO2",
  h1("Visualizing CO2 Emissions"),
  h2("CO2 Emissions Growth by Region"),
  p("These estimations include..."),
  
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
  h2("CO2 Output is growing every year"),
  p("Lorem ipsum dolor...")
)

ui <- fluidPage(
  includeCSS("www/styles.css"),
  navbarPage(
    "Climate Change Analysis",
    graph_page,
    summary_page
  )
)