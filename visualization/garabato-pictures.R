
# Set up ------------------------------------------------------------------

library(ggplot2)
library(purrr)
library(sf)

if (!dir.exists("visualization/garabato-pictures/")) {
  dir.create("visualization/garabato-pictures/")
}


# Functions ---------------------------------------------------------------

rotate <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
rad <- function(degree) degree / 360 * 2 * pi

garabato <- function(f, trazos, ..., seed = NULL) {
  set.seed(seed)
  args <- map(list(...), ~ rep(.x, each = 2))
  args$n <- trazos * 2
  
  output <- do.call(f, args) %>% 
    matrix(ncol = 2, byrow = TRUE) 
  
  (output %*% rotate(rad(45))) %>% 
    st_linestring()
}

custom_plot <- function(sf_obj) {
  sf_obj %>% 
    ggplot() + 
    geom_sf(color = "steelblue", size = 0.5, alpha = 0.5, fill = "#FDD103") + 
    theme_void(base_family = "Avenir Next Condensed") +
    theme(plot.background = element_rect(fill = "antiquewhite", color = "antiquewhite"),
          plot.title = element_text(hjust = 0.5))
}


# Drawings ----------------------------------------------------------------

garabato(rnorm, trazos = 200, mean = 1:200, sd = sqrt(1:200), seed = 123) %>% 
  st_cast("MULTIPOLYGON") %>% 
  custom_plot() +
  ggtitle("normal distribution with increasing center and scale")

ggsave("visualization/garabato-pictures/pic-1.png", device = "png", dpi = "print", bg = "antiquewhite")

garabato(rnorm, trazos = 200, mean = 0, sd = 1, seed = 123) %>% 
  st_cast("MULTIPOLYGON") %>% 
  custom_plot() +
  ggtitle("standard normal distribution")

ggsave("visualization/garabato-pictures/pic-2.png", device = "png", dpi = "print", bg = "antiquewhite")

garabato(rnorm, 200, mean = c(rep(1, 100), rep(10, 100)), sd = 2, seed = 123) %>% 
  st_cast("MULTIPOLYGON") %>% 
  custom_plot() +
  ggtitle("mixture of two normals")

ggsave("visualization/garabato-pictures/pic-3.png", device = "png", dpi = "print", bg = "antiquewhite")

garabato(runif, 200, min = log(1:100), max = 1:100) %>% 
  st_cast("MULTIPOLYGON") %>% 
  custom_plot() +
  ggtitle("not a uniform distribution")

ggsave("visualization/garabato-pictures/pic-4.png", device = "png", dpi = "print", bg = "antiquewhite")



