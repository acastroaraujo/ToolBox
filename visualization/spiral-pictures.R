
# Source of inspiration: https://infrahumano.github.io/exterior/2020/04/18/mediod%C3%ADa.html
#                        https://youtu.be/C2vbYpa-AWk

library(tidyverse)
library(sf)

if (!dir.exists("visualization/spiral-pictures/")) dir.create("visualization/spiral-pictures/")


# General functions --------------------------------------------------------

rotate <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
rad <- function(degree) degree / 360 * 2 * pi


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

rotate_shrink_push <- function(M, ..., d = 0.1) {
  
  angle <- pi/2 - acos(d / (1 - d))  ## this angle will only work with 1 x 1 squares
  
  M <- M %*% rotate(angle)  ## rotate
  M <- M * sqrt(2*d^2 - 2*d + 1) ## shrink
  M <- M + matrix(rep(c(0, d), nrow(M)), ncol = 2, byrow = TRUE)  ## push
  return(M)
}

output <- accumulate(1:200, rotate_shrink_push, .init = square, d = 1/30) %>% 
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
  theme(plot.background = element_rect(fill = "antiquewhite", color = NA))

ggsave("visualization/spiral-pictures/pic-3.png", device = "png", dpi = "print", bg = "antiquewhite")


# Third attempt -----------------------------------------------------------

triangle <- rbind(c(0, 0), c(0, 1),  c(1, 0), c(0, 0))

tri_shift <- function(x, ..., d = 0.1) x + rbind(c(0, d), c(d, 0), c(-d, 0), c(0, d))

o1 <- accumulate(1:30, tri_shift, .init = triangle, d = 1/30)

o2 <- accumulate(1:30, tri_shift, .init = triangle, d = 1/30) %>% 
  map(~ . %*% rbind(c(-1, 0), c(0, 1))) %>% 
  map(~ . + rbind(c(1, 0), c(1, 0), c(1, 0), c(1, 0)))

c(o1,  o2) %>%  
  st_multilinestring() %>% 
  st_cast("MULTIPOLYGON") %>% 
  ggplot() + 
  geom_sf(fill = "#0E75AD", color = "white", size = 1/20) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "#FFBA61", color = NA)) 

ggsave("visualization/spiral-pictures/pic-4.png", device = "png", dpi = "print", bg = "#FFBA61")


# Final attempt -----------------------------------------------------------

next_row <- function(M, ..., fraction = 0.1) {
  i <- nrow(M)
  rbind(M, M[i - 3, ] + (M[i - 2, ] - M[i - 3, ]) * fraction)
}

spiral <- function(x, N = 300, ...) {
  reduce(1:N, next_row, .init = x, ...) 
}

spiral(triangle)
  list() %>% 
  st_multilinestring() %>% 
  ggplot() + 
  geom_sf(size = 0.2, color = "black") + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "antiquewhite", color = NA))

spiral(square, N = 500, fraction = 0.05) %>% 
  list() %>% 
  st_multilinestring() %>% 
  ggplot() + 
  geom_sf(size = 0.2, color = "black") + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "antiquewhite", color = NA))

ggsave("visualization/spiral-pictures/pic-5.png", device = "png", dpi = "print", bg = "antiquewhite")


## This is the same pattern from Javier's blog:

polygons <- list(
  rbind(c(0, 0), c(0, 500), c(250, 450), c(0, 0)),
  rbind(c(0, 500), c(250, 450), c(0, 1000), c(0, 500)), 
  rbind(c(0, 0), c(250, 450), c(600, 0), c(0, 0)),
  rbind(c(250, 450), c(0, 1000), c(500, 1000), c(500, 750), c(250, 450)),
  rbind(c(600, 0), c(250, 450), c(500, 750), c(800, 350), c(600, 0)),
  rbind(c(600, 0), c(800, 350), c(1000, 250), c(1000, 0), c(600, 0)),
  rbind(c(500, 750), c(800, 350), c(1000, 250), c(1000, 1000), c(500, 750)),
  rbind(c(500, 750), c(1000, 1000), c(500, 1000), c(500, 750))
)

map(polygons, spiral, fraction = 0.08, N = 500) %>% 
  st_multilinestring() %>% 
  ggplot() + 
  geom_sf(color = "steelblue", size = 0.2) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "antiquewhite", color = NA))

ggsave("visualization/spiral-pictures/pic-6.png", device = "png", dpi = "print", bg = "antiquewhite")

map(polygons, spiral, fraction = 0.05, N = 500) %>% 
  st_multilinestring() %>% 
  st_cast("MULTIPOLYGON") %>% 
  ggplot() + 
  geom_sf(fill = "steelblue", size = NA) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "antiquewhite", color = NA))

ggsave("visualization/spiral-pictures/pic-7.png", device = "png", dpi = "print", bg = "antiquewhite")


# Composition -------------------------------------------------------------

library(patchwork)

a <- spiral(square, N = 500, fraction = 0.08) %>% 
  list() %>% 
  st_multilinestring() %>% 
  ggplot() 

b <- list(triangle, rbind(c(0, 1), c(1, 1), c(1, 0), c(0, 1))) %>% 
  map(spiral) %>% 
  st_multilinestring() %>% 
  ggplot() 

c <- map(polygons, spiral, fraction = 0.08, N = 500) %>% 
  st_multilinestring() %>% 
  ggplot() 

output <-  c(0, seq(-90, 90, 30), 180, 270) %>% 
  map(~ square %*% rotate(rad(.))) %>% 
  map(spiral, fraction = .05, N = 500) %>% 
  st_multilinestring() 

upper <- output %>% unlist() %>% max()
lower <- output %>% unlist() %>% min()

enclosing_square <- rbind(c(lower, lower), c(lower, upper), c(upper, upper), c(upper, lower), c(lower, lower))  %>% 
  list() %>% 
  st_polygon() 

d <- output %>% 
  ggplot() +
  geom_sf(data = enclosing_square, fill = NA, color = "steelblue", size = 0.1) 

composition <- a + b + c + d & 
  geom_sf(color = "steelblue", size = 0.1) &
  theme_void() &
  theme(plot.background = element_rect(fill = "antiquewhite", color = "antiquewhite"))

composition + plot_layout(design = "1234")

ggsave("visualization/spiral-pictures/pic-8.png", device = "png", dpi = "print", bg = "antiquewhite")

c(0, seq(-90, 60, 30), 180, 270) %>% 
  map(~ square %*% rotate(rad(.))) %>% 
  map(spiral, fraction = .05, N = 500) %>% 
  append(enclosing_square) %>% 
  st_multilinestring() %>% 
  st_cast("MULTIPOLYGON") %>% 
  ggplot() +
  geom_sf(data = enclosing_square, fill = NA, color = "steelblue", size = 1) +
  geom_sf(fill = "steelblue", size = 0.1, color = NA) +
  theme_void() +
  theme(plot.background = element_rect(fill = "antiquewhite", color = "antiquewhite"))

ggsave("visualization/spiral-pictures/pic-9.png", device = "png", dpi = "print", bg = "antiquewhite")

