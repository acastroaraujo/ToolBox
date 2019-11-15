
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
  
  output <- ncdc(datasetid = 'GHCND', 
                 stationid = cities[[city]], 
                 datatypeid = 'TMAX', 
                 startdate = start, 
                 enddate = end, 
                 token = token, 
                 limit = 1000)
  
  output$data %>% 
    mutate(date = as.Date(date), temp = value / 10, city = city) %>% 
    select(city, date, temp, datatype, station)
}


## Usage
nyc2019 <- ncdc_helper("New York", "2019-01-01", "2019-04-01")
nyc2018 <- ncdc_helper("New York", "2018-01-01", "2018-04-01")
nyc2017 <- ncdc_helper("New York", "2017-01-01", "2017-04-01")
nyc2016 <- ncdc_helper("New York", "2016-01-01", "2016-04-01")
nyc2015 <- ncdc_helper("New York", "2015-01-01", "2015-04-01")
nyc2014 <- ncdc_helper("New York", "2014-01-01", "2014-04-01")

nyc <- list(nyc2019, nyc2018, nyc2017, nyc2016, nyc2015, nyc2014) %>% 
  bind_rows() %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE) %>% fct_drop()) %>% 
  group_by(month, year) %>% 
  mutate(week = lubridate::week(date)) %>% 
  mutate(temp = farenheit(temp)) %>% 
  drop_na()


nyc %>% 
  group_by(year,month,  week) %>%
  summarize(
    sd = sd(temp),
    max = max(temp),
    min = min(temp),
    avg = mean(temp),
    n = n()
    ) %>% 
  arrange(desc(max)) %>% 
  filter(n > 1) %>% 
  ggplot(aes(week, max, group = year, color = factor(year))) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:13)

nyc %>% 
  group_by(year) %>% 
  ggplot(aes(date, temp)) + 
  geom_point(alpha = 0.5, size = 0.5) +
  geom_line() + 
  facet_wrap(~year, scales = "free_x") + 
  labs(title = "Temperature in First Quarter", y = "Fahrenheit\n", x = NULL,
       subtitle = "Central Park Weather Station") +
  theme_custom()

ggsave(filename = "nyc_weather_1st_quarter.png", device = "png", width = 8, height = 4, dpi = "print")

weekly_temp <- nyc %>% 
  group_by(year,  week) %>%
  summarize(
    sd = sd(temp),
    max = max(temp),
    min = min(temp),
    avg = mean(temp),
    n = n()
  ) %>% 
  arrange(desc(max)) %>% 
  filter(n > 1) 

weekly_temp %>% 
  ggplot() + 
  geom_point(aes(week, min, color = "Min Temp")) + 
  geom_smooth(aes(week, min, color = "Min Temp"), se = FALSE) +
  geom_point(aes(week, max, color = "Max Temp")) +
  geom_smooth(aes(week, max, color = "Max Temp"), se = FALSE) +
  facet_wrap(~year, scales = "free_x") +
  scale_x_continuous(breaks = seq(0.5, 12.5, 4),
                     labels = c("Jan", "Feb", "Mar", "Apr")) +
  geom_vline(xintercept = seq(0.5, 13.5, 4), linetype = "dashed") + 
  theme_custom() +
  scale_color_manual(values = c("tomato", "steelblue1")) +
  labs(y = "Fahrenheit\n", x = NULL,
       color = NULL, title = "Weekly Temperature") + 
  theme(legend.position = "bottom")

ggsave(filename = "nyc_weekly_temp.png", device = "png", width = 8, height = 4, dpi = "print")


## ************************
## Google Trends
## ************************

library(gtrendsR)

results <- gtrends("things to do", geo = "US-NY-501", time = "2019-01-01 2019-04-01")


results$interest_over_time %>% 
  ggplot(aes(date, hits)) + 
  geom_line() + 
  geom_point(
    data = filter(results$interest_over_time, rank(-hits) <= 5),
    color = "tomato"
  ) +
  theme_custom() +
  labs(title = "Google Trends", subtitle = '"things to do" shows lots of weekend activity ')

ggsave(filename = "nyc_gtrends_daily.png", device = "png", width = 8, height = 4, dpi = "print")


weekly_trends <- results$interest_over_time %>% 
  mutate(week = lubridate::week(date)) %>% 
  group_by(week) %>%
  summarize(
    max_hits = max(hits)
  ) 

weekly_trends %>% 
  ggplot(aes(week, max_hits)) + 
  geom_line() + 
  geom_point(
    data = filter(weekly_trends, rank(-max_hits) <= 5),
    color = "tomato"
  ) +
  theme_custom() +
  labs(title = "Google Trends", subtitle = '"things to do"') +
  scale_x_continuous(breaks = seq(1, 13, 2)) +
  labs(y = "Max. Hits")

ggsave(filename = "nyc_gtrends_weekly.png", device = "png", width = 8, height = 4, dpi = "print")


temp_google_merge <- weekly_temp %>% 
  filter(year == 2019) %>% 
  full_join(weekly_trends) %>% 
  mutate(month = case_when(
    week %in% 1:4 ~ "Jan",
    week %in% 5:8 ~ "Feb",
    week %in% 9:12 ~ "Mar",
    week > 12 ~ "Apr"
  ))
  
temp_google_merge %>% 
  filter(month != "Apr") %>% 
  ggplot(aes(max, max_hits)) + 
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = FALSE, color = "steelblue1") +
  geom_point(aes(color = month), show.legend = FALSE) +
  ggrepel::geom_label_repel(
    #data = filter(temp_google_merge, rank(-max_hits) <= 5),
    mapping = aes(label = month, color = month), alpha = 0.8,
    show.legend = FALSE
    ) + 
  theme_custom() +
  labs(title = '"Things to do" and Weekly Temperature',
       subtitle = 'January-March, 2019', caption = "Restricted to New York City",
       y = "Google Trends", x = "Max. Temperature") +
  scale_color_manual(values = wesanderson::wes_palette("Darjeeling1"))

  ggsave(filename = "nyc_gtrends_temp_weekly.png", device = "png", width = 8, height = 4, dpi = "print")






