
library(ggplot2)
library(purrr)
library(sf)

if (!dir.exists("visualization/garabato-pictures/")) {
  dir.create("visualization/garabato-pictures/")
}

garabato <- function(f, trazos, ..., seed = NULL) {
  set.seed(seed)
  args <- map(list(...), ~ rep(.x, each = 2))
  args$n <- trazos * 2
  
  do.call(f, args) %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    st_linestring()
}

custom_plot <- function(sf_obj) {
  sf_obj %>% 
    ggplot() + 
    geom_sf(color = "steelblue", size = 0.5, alpha = 0.5, fill = "pink") + 
    theme_void(base_family = "Avenir Next Condensed") 
}

a <- garabato(rnorm, trazos = 200, mean = 1:200, sd = sqrt(1:200), seed = 123) %>% 
  st_cast("MULTIPOLYGON") %>% 
  custom_plot() +
  ggtitle("normal distribution with increasing\ncenter and scale")

b <- garabato(rnorm, trazos = 200, mean = 0, sd = 1, seed = 123) %>% 
  st_cast("MULTIPOLYGON") %>% 
  custom_plot() +
  ggtitle("standard normal distribution")

c <- garabato(rnorm, 200, mean = c(rep(1, 100), rep(10, 100)), sd = 2, seed = 123) %>% 
  st_cast("MULTIPOLYGON") %>% 
  custom_plot() +
  ggtitle("mixture of two normals")

d <- garabato(rbinom, 200, size = 10, prob = 0.2, seed = 123) %>% 
  st_cast("MULTIPOLYGON") %>% 
  custom_plot() +
  ggtitle("binomial distribution")


# Composition -------------------------------------------------------------

library(patchwork)

b + c + d + a &
  theme(plot.background = element_rect(fill = "antiquewhite", color = "antiquewhite"))

ggsave("visualization/garabato-pictures/pic-1.png", device = "png", dpi = "print", 
       width = 8, height = 8,
       bg = "antiquewhite")


