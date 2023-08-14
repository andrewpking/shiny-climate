graph_page <- tabPanel(
  "Visualizing CO2",
  h1("Visualizing CO2 Emissions"),
  h2("CO2 Emissions Growth per year by Region"),
  p("These estimations include land development and exclude traded goods."),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "country_year",
        label = "Select Year",
        min = 1851,
        max = 2020,
        value = c(1800, 2020),
        step = 1
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
      tabsetPanel(
        tabPanel("Plot", plotlyOutput(outputId = "co2_plotly")),
        tabPanel("Table", tableOutput("co2Table"))
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

ui <- navbarPage(
  "Climate Change Analysis",
  graph_page,
  summary_page
)