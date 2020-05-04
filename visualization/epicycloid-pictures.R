
# https://www.johndcook.com/blog/2020/04/24/envelopes-of-epicycloids/

# Imagine two ants crawling around a circle at different speeds and draw a line
# between the two ants at regular time intervals. The position of the two ants
# at time t are (cos pt, sin pt) and (cos qt, sin qt) where p and q are
# integers, p > q > 0, and t comes from dividing the interval [0, 2π] into an
# integer number of points.

# These lines form an envelope, a set of tangent lines, around a curve in the
# middle known as an epicycloid.

library(purrr)
library(ggplot2)
library(sf)


# Functions ---------------------------------------------------------------

make_line <- function(x, p, q) {
  stopifnot(length(x) == 1)
  rbind(c(cos(p*x), sin(p*x)), 
        c(cos(q*x), sin(q*x)))
}

draw_epicycloid <- function(p, q, n = 200) {
  xs <- seq(0, 2*pi, length.out = n)
  input <- st_multilinestring(map(xs, make_line, p, q))
  
  input %>% 
    ggplot() + 
    geom_sf(color = "#424146", size = 0.2, alpha = 0.8) +
    geom_sf(data = st_convex_hull(input), fill = NA, size = 0.5, color = "#424146") + 
    theme_void(base_family = "Avenir Next Condensed") + 
    theme(plot.background = element_rect(fill = "antiquewhite", color = "antiquewhite"))
}


# Examples ----------------------------------------------------------------

draw_epicycloid(2, 4, 300)
ggsave("visualization/circle-pictures/epicycloid-1.png", device = "png", dpi = "print", bg = "antiquewhite")
draw_epicycloid(4, 10, 1e3)
ggsave("visualization/circle-pictures/epicycloid-2.png", device = "png", dpi = "print", bg = "antiquewhite")
draw_epicycloid(4, 40, n = 1e3)
ggsave("visualization/circle-pictures/epicycloid-3.png", device = "png", dpi = "print", bg = "antiquewhite")

# gif ---------------------------------------------------------------------

library(magick) 

p <- seq(1, 5, length.out = 40)
q <- seq(1, 3, length.out = 40)

plots <- map2(c(p, rev(p)), c(rev(q), q), draw_epicycloid, n = 500)

temp <- tempfile()
dir.create(temp)

paths <- file.path(temp, paste0("pic-", seq_along(plots), ".png"))
walk2(paths, plots, ggsave, device = "png", width = 3, height = 3, bg = "antiquewhite")

paths %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 5, optimize = TRUE) %>%
  image_write("visualization/circle-pictures/epicycloid.gif", quality = 40)







       