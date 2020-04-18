
# Source of inspiration: https://infrahumano.github.io/exterior/2020/04/18/mediod%C3%ADa.html
#                        https://youtu.be/C2vbYpa-AWk

library(sf)
library(ggplot2)
library(purrr)

if (!dir.exists("visualization/spiral-pictures/")) dir.create("visualization/spiral-pictures/")


# First attempt -----------------------------------------------------------

square <- rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0))

shift <- function(x, ..., d = 0.1) x + rbind(c(0, d), c(d, 0), c(0, -d), c(-d, 0), c(0, d))

accumulate(1:100, shift, .init = square, d = 0.01) %>% 
  st_polygon() %>%
  ggplot() + 
  geom_sf(fill = NA, size = 1/6, color = "steelblue") + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "pink", color = NA))

ggsave("visualization/spiral-pictures/pic-1.png", device = "png", dpi = "print", bg = "pink")



# Second attempt ----------------------------------------------------------

rotate <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

spiral <- function(sf_obj, ..., d = 0.1) {
  
  ## The idea is to rotate, shrink, and then raise the original square
  stopifnot(class(sf_obj) == c("XY", "POLYGON", "sfg"))
  ## This function will only work with sf objects
  angle <- pi/2 - acos(d / (1-d))  ## this angle will only work with 1 x 1 squares
  
  rotated_obj <- sf_obj * rotate(angle)  
  shrunk_obj <- rotated_obj * sqrt(2*d^2 - 2*d + 1)
  shrunk_obj + c(0, d)
  
}

sf_square <- st_polygon(list(square))

output <- accumulate(1:200, spiral, .init = sf_square, d = 1/30) %>% 
  st_polygon() 

output %>% 
  ggplot() + 
  geom_sf(fill = NA, size = 0.2, color = "steelblue") + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "pink", color = NA))

ggsave("visualization/spiral-pictures/pic-2.png", device = "png", dpi = "print", bg = "pink")

output %>% 
  ggplot() + 
  geom_sf(fill = "steelblue", size = 0.2, color = NA) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "pink", color = NA))

ggsave("visualization/spiral-pictures/pic-3.png", device = "png", dpi = "print", bg = "pink")
