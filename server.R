library(plotly)

# Read the data
co2_df <- read_csv("www/data/Countries.csv")
cities_df <- read_csv("www/data/CityAnnualTemps.csv")

# Function to get a list of countries from the data-set
get_countries <- function() {
  country_list <- co2_df %>%
    distinct(Country) %>%
    pull(Country)
  return(country_list)
}

# Function to get a list of years from the data-set
get_years <- function() {
  countries <- get_countries()
  
  df <- co2_df %>%
    filter(Country %in% countries)
  
  year_list <- unique(df$dt)
  
  # Initialize min and max value
  min_year <- max(year_list, na.rm = TRUE)
  max_year <- min(year_list, na.rm = TRUE)
  
  # Find the maximum among the minimum years
  min_year <- df %>%
    group_by(Country) %>%
    summarise(min_year_country = min(dt)) %>%
    pull(min_year_country) %>%
    max()
  
  # Find the minimum among the maximum years
  max_year <- df %>%
    group_by(Country) %>%
    summarise(max_year_country = max(dt)) %>%
    pull(max_year_country) %>%
    min()
  
  year_vec <- c(min_year, max_year)
  return(year_vec)
}


# Return a list of all continents.
get_contitents <- function() {
  continents <- c(
    "Africa", "Antarctica", "Asia", "Europe",
    "India", "North America", "South America")
  return(continents)
}

# Function to create a table of co2 vs temperature increases
temp_vs_co2_increase <- function(countries, min_year, max_year) {
  selected_df <- co2_df %>%
    filter(Country %in% countries) %>%
    filter(dt %in% as.character(c(min_year: max_year))) %>%
    group_by(Country) %>%
    mutate(co2 = sum(co2, na.rm = TRUE)) %>%
    filter(dt %in% as.character(c(min_year, max_year))) %>%
    mutate(avg_temp_change = ifelse(
      !is.na(AverageTemperature),
      diff(AverageTemperature),
      NA
    ) 
    ) %>%
    mutate(max_temp_change = ifelse(
      !is.na(MaxAverageTemperature),
      diff(MaxAverageTemperature),
      NA
    ) 
    ) %>%
    mutate(min_temp_change = ifelse(
      !is.na(MinAverageTemperature),
      diff(MinAverageTemperature),
      NA
    ) 
    ) %>%
    filter(dt == as.character(max_year)) %>%
    select(dt, Country, 
           avg_temp_change, 
           max_temp_change, 
           min_temp_change,
           co2)
  return(selected_df)
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
    updateSliderInput(
      inputId = "selected_year",
      label = "Select Year",
      min = 1856,
      max = 2013,
      value = c(1856, 2013),
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
    selected_df <- temp_vs_co2_increase(countries, min_year, max_year) %>%
      arrange(desc(co2)) %>%
      reframe(
        `Year` = as.character(dt),
        Country,
        `Average Temperature Change` = avg_temp_change,
        `Maximum Temperature Change` = max_temp_change,
        `Minimum Temperature Change` = min_temp_change,
        `Total CO2 Emissions` = co2
      )
    return(selected_df)
  })
  
  output$co2_plotly <- renderPlotly({
    countries <- selected_countries_reactive()
    years <- selected_year_reactive()
    min_year <- years[1]
    max_year <- years[2]
    selected_df <- temp_vs_co2_increase(countries, min_year, max_year)
    
    co2_plotl <- ggplot(selected_df) +
      geom_col(aes(
        y = avg_temp_change,
        x = Country,
        fill = co2,
        text = paste0("Region: ", Country, "<br>",
                     "Year: ", min_year, "-", max_year, "<br>",
                     "Average Temperature Change: ", avg_temp_change, "<br>",
                     "Maximum Temperature Change: ", max_temp_change, "<br>",
                     "Minimum Temperature Change: ", min_temp_change, "<br>",
                     "Absoluate CO2 Emissions: ", co2)
      )) +
      labs(title = paste("Temperature Growth and CO2 Emissions from", min_year,
                         "to", max_year,"by Region"),
           fill = "Total CO2 Emissions",
           x = "Continent", y = "Average Temperature Change (°C)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "bottom") + 
      scale_fill_gradient(low = "darkkhaki", high = "darkgreen")
    co2_plotly <- ggplotly(co2_plotl, tooltip = "text")
    return(co2_plotly)
  })
  
  # # Render the second plot.
  # output$co2_plotly2 <- renderPlotly({
  #   years <- selected_year_reactive()
  #   countries <- get_countries()
  #   min_year <- years[1]
  #   max_year <- years[2]
  #   selected_df <- co2_df %>%
  #     filter(dt %in% as.character(c(min_year: max_year))) %>%
  #     filter(Country %in% countries)
  #   
  #   co2_plot2 <- ggplot(selected_df) +
  #     geom_point(aes(
  #       y = co2_growth_abs,
  #       x = population,
  #       fill = abs_co2,
  #       text = paste0("Region: ", Country, "<br>",
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