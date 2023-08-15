library(dplyr)
library(lubridate)
library(readr)

# Aggregate the huge city data csv file into a smaller csv file.
city_temp <- read_csv("www/data/GlobalLandTemperaturesByCity.csv")
country_temp <- read_csv("www/data/GlobalLandTemperaturesByCountry.csv")
country_co2 <- read_csv("www/data/country-co2-data.csv")

city_temp_helper <- function(temp_data){
  temp <- temp_data %>%
    mutate(dt = format(dt, "%Y")) %>%
    group_by(Country, City, dt) %>%
    summarise(
      City,
      Country,
      MaxAverageTemperature = max(AverageTemperature, na.rm = TRUE),
      MinAverageTemperature = min(AverageTemperature, na.rm = TRUE),
      AverageTemperature = mean(AverageTemperature, na.rm = TRUE),
      AverageTemperatureUncertainty = mean(AverageTemperatureUncertainty,
                                           na.rm = TRUE),
      Latitude,
      Longitude) %>%
    distinct(Country, City, dt, .keep_all = TRUE) %>%
    mutate(MaxAverageTemperature = ifelse(
      is.infinite(MaxAverageTemperature), NA, MaxAverageTemperature),
      MinAverageTemperature = ifelse(
        is.infinite(MinAverageTemperature), NA, MinAverageTemperature)
    )
  return(temp)
}

convert_coords <- function(coord_data) {
  coord_data %>%
    mutate(lat = as.numeric(sub("([0-9.]+)[NS]", "\\1", Latitude))) %>%
    mutate(lat = ifelse(
      substr(Latitude, nchar(Latitude), nchar(Latitude)) == "S", -lat, lat)) %>%
    select(-Latitude) %>%
    mutate(lng = as.numeric(sub("([0-9.]+)[EW]", "\\1", Longitude))) %>%
    mutate(lng = ifelse(
      substr(Longitude, nchar(Longitude), nchar(Longitude)) == "W",
      -lng, lng)) %>%
    select(-Longitude)
}

annual_city_temp <- city_temp_helper(city_temp) %>%
  convert_coords()

write_csv(annual_city_temp, file = "www/data/CityAnnualTemps.csv")

country_table_helper <- function(temp_data, co2_data) {
  country_tbl <- temp_data %>%
    mutate(dt = (floor_date(dt, 'year'))) %>%
    group_by(Country, dt) %>%
    summarise(
      MaxAverageTemperature = max(AverageTemperature, na.rm = TRUE),
      MinAverageTemperature = min(AverageTemperature, na.rm = TRUE),
      AverageTemperature = mean(AverageTemperature, na.rm = TRUE),
      AverageTemperatureUncertainty = mean(AverageTemperatureUncertainty,
                                           na.rm = TRUE)
    ) %>%
    mutate(dt = format(dt, "%Y")) %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    mutate_all(~ifelse(is.infinite(.), NA, .)) %>%
    filter(dt %in% as.character(c(1850: 2013)))
  co2_tbl <- co2_data %>%
    group_by(country, year) %>%
    mutate(dt = as.character(year)) %>%
    mutate(Country = country) %>%
    ungroup() %>%
    select(-c(country, year))
  merged_tbl <- country_tbl %>%
    left_join(co2_tbl, by = join_by(Country, dt))
  return(merged_tbl)
}

country_tbl <- country_table_helper(country_temp, country_co2)
write_csv(country_tbl, file = "www/data/Countries.csv")
