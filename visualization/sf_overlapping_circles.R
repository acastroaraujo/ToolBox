
library(tidyverse)
library(sf)

# Working with overlapping features ---------------------------------------
# Source: https://r-spatial.github.io/sf/reference/geos_binary_ops.html

set.seed(1)
random_points <- runif(100, 0, 100)

input <- array(random_points, dim = c(length(random_points) / 2, 2)) %>% 
  st_multipoint() %>% 
  st_sfc() %>% 
  st_cast("POINT") %>% 
  st_buffer(dist = 5) 

input_colors <- sample(unlist(wesanderson::wes_palettes), length(input), replace = TRUE)

input %>% 
  ggplot() + 
  geom_sf(fill = input_colors, alpha = 0.5, color = NA) + ## outside of mapping = aes()
  theme_void(base_family = "Avenir Next Condensed") + 
  labs(title = "overlapping circles")

ggsave("visualization/circle-pictures/overlapping_circles.png", device = "png", dpi = "print")

overlapping_differences <- st_difference(input)  ## overlapping areas are erased from geometries sequentially:
                                                 ## s1, s2-s1, s3-s2-s1, ...
overlapping_differences %>% 
  ggplot() +
  geom_sf(fill = sample(input_colors, size = length(overlapping_differences), replace = TRUE), 
          alpha = 0.5, color = NA) + 
  theme_void(base_family = "Avenir Next Condensed") + 
  labs(title = "non-overlapping circles")

non_overlapping_differences <- st_intersection(input) 
  
non_overlapping_differences %>% 
  ggplot() + 
  geom_sf(fill = sample(input_colors, length(non_overlapping_differences), replace = TRUE), alpha = 0.5, color = NA) + 
  theme_void(base_family = "Avenir Next Condensed") + 
  labs(title = "Non-overlapping Circles")

input %>% 
  st_sf() %>% 
  st_intersection() %>%
  ggplot() + 
  geom_sf(aes(fill = n.overlaps), color = NA) + 
  scale_fill_viridis_c(option = "magma", guide = "legend") +
  theme_void(base_family = "Avenir Next Condensed") + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold", size = 22)) +
  labs(title = "number of overlaps", fill = NULL) 





