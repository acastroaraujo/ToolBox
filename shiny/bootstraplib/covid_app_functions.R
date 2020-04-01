
gg_dots <- function(data_frame, state_map, var) {
  variable <- sym(var)
  
  ggplot(data_frame) +
    geom_sf(data = state_map, color = "white", size = 0.5) +
    geom_sf(aes(size = !!variable, color = log(!!variable), alpha = log(!!variable)), show.legend = FALSE) +
    scale_color_viridis_c(option = "magma", direction = -1, na.value = "white") +
    theme_void()
}

gg_choropleth <- function(data_frame, state_map, var) {
  variable <- sym(var)
  
  data_frame %>% 
    filter(!is.na(!!variable)) %>% 
    ggplot() +
    geom_sf(data = state_map, color = "white", size = NA) + ## uniform gray background
    geom_sf(color = "white", size = 0, mapping = aes(fill = !!variable), show.legend = FALSE) +
    geom_sf(data = state_map, color = "white", size = 0.5, fill = NA) + ## white lines
    scale_fill_viridis_c(option = "magma", direction = -1, na.value = NA, trans = "log10") +
    theme_void()
}


