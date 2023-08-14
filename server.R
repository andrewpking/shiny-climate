library(tidyverse)
library(plotly)

# Read the data
co2_df <- read_csv(
  "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
)

# Function to get a list of countries from the data-set
get_countries <- function() {
  country_list <- co2_df %>%
    distinct(country)
  return(country_list)
}

# Function to get a list of years from the data-set
get_years <- function() {
  countries <- get_countries()
  
  df <- co2_df %>%
    filter(country %in% countries)
  
  year_list <- unique(df$year)
  
  # Initialize min and max value
  min_year <- max(year_list, na.rm = TRUE)
  max_year <- min(year_list, na.rm = TRUE)
  
  # Find the maximum among the minimum years
  min_year <- df %>%
    group_by(country) %>%
    summarise(min_year_country = min(year)) %>%
    pull(min_year_country) %>%
    max()
  
  # Find the minimum among the maximum years
  max_year <- df %>%
    group_by(country) %>%
    summarise(max_year_country = max(year)) %>%
    pull(max_year_country) %>%
    min()
  
  year_vec <- c(min_year, max_year)
  return(year_vec)
}



get_contitents <- function() {
  continents <- c(
    "Africa", "Antarctica", "Asia", "Europe",
    "India", "North America", "South America")
  return(continents)
}

server <- function(input, output){
  
  # Code for creating a bar chart of C02 by continent
  # Dynamically set choices based on the list of countries
  observe({
    updateSelectInput(
      inputId = "country_name",
      choices = get_countries(),
      selected = get_contitents()
    )
  })
  
  # Create a reactive that holds the selected countries
  selected_countries_reactive <- reactive({
    input$country_name
  })
  
  
  # For debugging.
  output$selected_countries <- renderPrint({
    selected_countries <- selected_countries_reactive()
    if (length(selected_countries) > 0) {
      paste(selected_countries, collapse = ", ")
    }
  })
  
  # Code for creating a map of C02 by year
  # Dynamically set slider choices based on available years
  observe({
    years <- get_years()
    updateSliderInput(
      inputId = "selected_year",
      label = "Select Year",
      min = years[1],
      max = years[2],
      value = years,  # Set an initial value to max year
      step = 1
    )
  })
  
  
  # Create a reactive that holds the selected countries
  selected_year_reactive <- reactive({
    input$country_year
  })
  
  # For debugging.
  output$selected_years <- renderPrint({
    selected_year <- selected_year_reactive()
    if (!is.null(selected_year)) {
      selected_year
    }
  })
  
  output$co2Table <- renderTable({
    countries <- selected_countries_reactive()
    years <- selected_year_reactive()
    min_year <- years[1]
    max_year <- years[2]
    selected_df <- co2_df %>%
      filter(country %in% countries) %>%
      filter(year %in% c(years[1]: years[2])) %>%
      group_by(country) %>%
      mutate(co2 = sum(co2, na.rm = TRUE)) %>%
      mutate(co2_growth_abs = sum(co2_growth_abs, na.rm = TRUE)) %>%
      filter(year == years[2]) %>%
      select(country, co2, co2_growth_abs) %>%
      arrange(desc(co2))
    selected_df
  })
  
  output$co2_plotly <- renderPlotly({
    countries <- selected_countries_reactive()
    years <- selected_year_reactive()
    min_year <- years[1]
    max_year <- years[2]
    selected_df <- co2_df %>%
      filter(country %in% countries) %>%
      filter(year %in% c(years[1]: years[2])) %>%
      group_by(country) %>%
      mutate(co2 = sum(co2, na.rm = TRUE)) %>%
      mutate(co2_growth_abs = sum(co2_growth_abs, na.rm = TRUE)) %>%
      filter(year == years[2])
    
    co2_plotl <- ggplot(selected_df) +
      geom_col(aes(
        y = co2,
        x = country,
        fill = co2_growth_abs,
        text = paste0("Region: ", country, "<br>",
                     "Year: ", min_year, "-", max_year, "<br>",
                     "CO2 Growth: ", co2_growth_abs,
                     "<br>",
                     "Absoluate CO2 Emissions: ", co2)
      )) +
      labs(title = paste("Absolute C02 Emissions and Growth from", min_year,
                         "to", max_year,"by Region"),
           fill = "C02 Growth",
           x = "Region", y = "Absolute C02 Emissions") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    co2_plotly <- ggplotly(co2_plotl, tooltip = "text")
    return(co2_plotly)
  })
  
  # # Render the second plot.
  # output$co2_plotly2 <- renderPlotly({
  #   years <- selected_year_reactive()
  #   selected_df <- abs_co2_df() %>%
  #     filter(year %in% years)
  #   
  #   co2_plot2 <- ggplot(selected_df) +
  #     geom_point(aes(
  #       y = co2_growth_abs,
  #       x = population,
  #       fill = abs_co2,
  #       text = paste0("Region: ", country, "<br>",
  #                     "Year: ", year, "<br>",
  #                     "CO2 Growth: ", co2_growth_abs, "<br>",
  #                     "Absoluate CO2 Emissions: ", abs_co2)
  #     )) +
  #     labs(title = "Annual CO2 Emissions by Region",
  #          fill = "Absolute CO2 Emmisions",
  #          x = "Population", y = "Absolute Annual CO2 Growth") # +
  #     # scale_x_continuous(limits = c(1840, 2020), breaks = seq(1840, 2020, 20))
  #   co2_plotly2 <- ggplotly(co2_plot2, tooltip = "text")
  #   return(co2_plotly2)
  # })
  # 
}