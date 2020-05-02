# https://www.youtube.com/watch?v=hkCakDslpXM
# https://www.youtube.com/watch?v=qhbuKbxJsk8

library(tidyverse)
library(sf)

circle <- st_buffer(st_point(c(0, 0)), 10)

mandala <- function(circle, i = 2, s = 1) {
  
  many_circles <- circle %>% 
    st_sfc() %>% 
    st_cast("POINT") %>%
    st_buffer(10 / s)
  
  index <- seq(1, length(many_circles), i)[-1]
  
  many_circles[index] %>% 
    map(pluck, 1) %>% 
    list() %>% 
    st_multipolygon()
  
}

draw_circle_mandala <- function(circle, i = 3, s = 1) {
  
  ggplot() + 
    geom_sf(data = mandala(circle, i, s), fill = "#424146", color = "antiquewhite", size = 0.2) +
    geom_sf(data = circle * 0.5, fill = "#424146", color = NA, alpha = 0.8) +
    theme_void() + 
    theme(panel.background = element_rect(fill = "antiquewhite", color = "antiquewhite"))
  
}


# example -----------------------------------------------------------------

draw_circle_mandala(circle, 3, 1)
ggsave("visualization/circle-pictures/circle-mandala.png", device = "png", dpi = "print", bg = "antiquewhite")

# temporary directory -----------------------------------------------------

temp <- tempfile()
dir.create(temp)

# sequence of plots -------------------------------------------------------

s_seq <- c(seq(1, 14, 0.5), seq(13.5, 1.2, -0.5))
i_seq <- c(seq(3, 30, 1), seq(29, 3.5, -1))

gg_list1 <- map(s_seq, draw_circle_mandala, circle = circle, i = 3)
gg_list2 <- map(i_seq, draw_circle_mandala, circle = circle, s = 1) 

# gif ---------------------------------------------------------------------

paths1 <- file.path(temp, paste0("pic-", seq_along(gg_list1), ".png"))
walk2(paths1, gg_list1, ggsave, device = "png", dpi = "print", bg = "antiquewhite")

library(magick)  

paths1 %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 5) %>%
  image_write("visualization/circle-pictures/circle-mandala-1.gif")

paths2 <- file.path(temp, paste0("pic-", seq_along(gg_list2), ".png"))
walk2(paths2, gg_list2, ggsave, device = "png", dpi = "print", bg = "antiquewhite")

paths2 %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 5) %>%
  image_write("visualization/circle-pictures/circle-mandala-2.gif")
