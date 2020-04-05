
# Sources of inspiration:
# https://infrahumano.github.io/exterior/2020/04/04/noche.html
# https://r-spatial.github.io/sf/articles/sf1.html

library(tidyverse)
library(sf)

coin_toss <- function() {
  sample(c(TRUE, FALSE), 1)
}

N <- 80
set.seed(12345)
output <- map(0:N, function(j) {
  v_sequence <- if (coin_toss()) seq(0, N, 2) else seq(1, N, 2)
  h_sequence <- if (coin_toss()) seq(0, N, 2) else seq(1, N, 2)
  v_lines <- purrr::map(v_sequence, function(i) matrix(c(j, j, i, i + 1), nrow = 2))  
  h_lines <- purrr::map(h_sequence, function(i) matrix(c(i, i + 1, j, j), nrow = 2))
  return(sf::st_multilinestring(append(v_lines, h_lines)))
}) %>% 
  tibble::enframe() %>% 
  sf::st_as_sf()

output %>%
  ggplot() + 
  geom_sf(color = "steelblue") +
  theme_void()

ggsave("visualization/grid-pictures.png", device = "png", dpi = "print")

