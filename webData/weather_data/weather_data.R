
## ********************************************
## Packages and function
## ********************************************

library(tidyverse)

theme_custom <- function(base_line_size = 0.25) {
  theme_minimal(base_family = "IBM Plex Sans", base_line_size = base_line_size) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = base_line_size),
      strip.background = element_rect(fill = "gray80", color = "gray80")
    ) 
}

## ********************************************
## Context data
## ********************************************

weather_stations <- read_table("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", 
           col_names = c("station", "lat", "lon")) %>% 
  distinct() %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326))

weather_stations <- weather_stations %>%       ## https://www.ncdc.noaa.gov/cag/city/data-info
  mutate(city = case_when(
    station == "USW00094728" ~ "New York",
    station == "USW00023174" ~ "Los Angeles",
    station == "USW00094846" ~ "Chicago",
    station == "USW00012918" ~ "Houston",
    station == "USW00023183" ~ "Phoenix")
    )

world_map <- spData::world %>% sf::st_transform(crs = sf::st_crs(4326))

world_map %>% 
  ggplot() + 
  geom_sf(fill = NA) + 
  coord_sf(crs = "+proj=moll") + 
  theme_custom() +
  labs(title = "World Map", caption = "Source: spData")
  
ggsave(filename = "worldmap.png",device = "png", width = 8, height = 4, dpi = "print")

## Where are these stations?

weather_stations %>% 
  ggplot() + 
  geom_sf(alpha = 0.2, size = 0.05) +
  coord_sf(crs = "+proj=moll") +
  theme_custom() +
  labs(title = "Weather Stations", caption = "Source: National Oceanic and Atmospheric Administration (NOAA)")

ggsave(filename = "all_weather_stations.png",device = "png", width = 8, height = 4, dpi = "print")

world_map %>% 
  ggplot() + 
  geom_sf(fill = NA, size = 0.25) + 
  geom_sf(data = filter(weather_stations, !is.na(city)), color = "red") +
  coord_sf(crs = "+proj=moll") + 
  theme_custom() +
  labs(
    title = "Target Stations", 
    subtitle = "New York, Los Angeles, Chicago, Houston, Phoenix",
    caption = "Source: National Oceanic and Atmospheric Administration (NOAA)"
    )

ggsave(filename = "target_stations.png",device = "png", width = 8, height = 4, dpi = "print")

## ********************************************
## Weather data
## ********************************************

# Email:	acastroaraujo@gmail.com
token <- "FUqaPBqMXbXvNJgzNoCFJXOenCJeTILC"

library(rnoaa)

farenheit <- function(x) (x * 9/5) + 32

ncdc_helper <- function(city = "New York", start, end, token = "FUqaPBqMXbXvNJgzNoCFJXOenCJeTILC") {
  cities <- c("New York" = "GHCND:USW00094728", 
              "Los Angeles" = "GHCND:USW00023174", 
              "Chicago" = "GHCND:USW00094846", 
              "Houston" = "GHCND:USW00012918", 
              "Phoenix" = "GHCND:USW00023183")
  
  stopifnot(city %in% names(cities))
  
  output <- ncdc(datasetid='GHCND', 
                 stationid = cities[[city]], 
                 datatypeid= 'TMAX', 
                 startdate = start, 
                 enddate = end, 
                 limit = 1000, token = token)
  
  out$data %>% 
    mutate(date = as.Date(date), temp = value / 10, city = city) %>% 
    select(city, date, temp, datatype, station)
}


## Usage
nyc2019 <- ncdc_helper("New York", "2019-01-01", "2019-03-31")

nyc2019 %>% 
  ggplot(aes(date, temp)) + 
  geom_point(alpha = 0.5) +
  geom_line() +
  scale_x_date(breaks = "months") 

nyc2019 %>% 
  mutate(week = lubridate::week(date),
         temp = farenheit(temp)) %>% 
  group_by(week) %>% 
  summarize(mean = mean(temp)) %>% 
  tibble::deframe() %>% 
  graphics::barplot()


## Max temp

## Min temp

## Highest variance

