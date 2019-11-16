


## ********************************************
## Packages and function
## ********************************************

library(tidyverse)
library(gganimate)

theme_custom <- function(base_line_size = 0.25) {
  theme_minimal(base_family = "IBM Plex Sans", base_line_size = base_line_size) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = base_line_size),
      strip.background = element_rect(fill = "gray80", color = "gray80")
    ) 
}

world_map <- spData::world %>% 
  ## Projecciones geográficas
  sf::st_transform(crs = sf::st_crs(4326)) %>% 
  sf::st_transform("+proj=moll")

weather_stations <- read_table(
  file = "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", 
  col_names = c("station", "lat", "lon", "var", "start", "end")
  ) %>% 
  filter(var == "TMAX") %>% 
  select(-var) %>% 
  ## Projecciones geográficas
  sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326)) %>% 
  sf::st_transform("+proj=moll")

## Deshacerse del "geometry"
weather_stations$lon <- sf::st_coordinates(weather_stations)[, 1]
weather_stations$lat <- sf::st_coordinates(weather_stations)[, 2]
weather_stations <- sf::st_drop_geometry(weather_stations)

weather_stations %>% 
  ggplot() + 
  geom_sf(data = world_map, color = NA) +
  geom_point(
    mapping = aes(lon, lat, color = start),
    size = 0.05, alpha = 1/3, show.legend = FALSE) + 
  theme_void() +
  labs(x = NULL, y = NULL) +
  scale_color_gradient(low = "yellow", high = "steelblue1")


weather_stations_expanded <- weather_stations %>% 
  mutate(year = map2(start, end, function(x, y) x:y)) %>% 
  #mutate(n = map_dbl(year, length)) %>% 
  unnest(cols = year)

g <- weather_stations_expanded %>% 
  ggplot() + 
  geom_sf(data = world_map, color = NA) +
  geom_point(
    mapping = aes(lon, lat),
    size = 0.1, alpha = 1/3, show.legend = FALSE) + 
  scale_color_gradient(low = "yellow", high = "steelblue1") +
  theme_custom() + 
  ## gganimate
  labs(title = "Estaciones meteorológicas",
       subtitle = "Año: {frame_time}",
       caption = "Fuente: National Oceanic and Atmospheric Administration",
       x = NULL, y = NULL) +
  transition_time(year) +
  enter_appear() + 
  exit_disappear() 

year_range <- range(weather_stations_expanded$year)
num_frames <- length(year_range[[1]]:year_range[[2]])

animate(g, start_pause = 5, end_pause = 20, nframes = num_frames)

anim_save(plot = last_animation(), "weather_stations.gif")

