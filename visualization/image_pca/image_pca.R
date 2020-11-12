

# Reconstructing Images with PCA ------------------------------------------

# Idea taken from: https://kieranhealy.org/blog/archives/2019/10/27/reconstructing-images-using-pca/

library(tidyverse)
library(magick)
library(jpeg)

# Black and White ---------------------------------------------------------

pic <- readJPEG("visualization/image_pca/dubois_black_and_white.jpeg")
pic_pca <- prcomp(pic, scale = TRUE, center = TRUE)

reconstruct_matrix <- function(pca_obj, p) {
  
  stopifnot(p >= 1)
  
  n <- dim(pca_obj$x)[[1]]
  mat <- pca_obj$x[ , 1:p] %*% t(pca_obj$rotation[ , 1:p])
  recon_mat <- scale(mat,  scale = 1/pca_obj$scale, center = -1 * pca_obj$center)
  colnames(recon_mat) <- 1:ncol(recon_mat)
  
  recon_mat %>% 
    as_tibble() %>% 
    mutate(x = n():1) %>% 
    pivot_longer(-x, names_to = "y", values_to = "z") %>% 
    mutate(y = as.numeric(y))
  
}

plot_gray <- function(df) {
  
  df %>% 
    ggplot(aes(y, x, fill = z)) + 
    geom_raster(show.legend = FALSE) +  
    scale_fill_gradient(low = "black", high = "white") +
    coord_fixed() + 
    theme_void()
  
}

reconstruct_matrix(pca_obj, 100) %>% plot_gray()

# temporary directory -----------------------------------------------------

temp <- tempfile()
dir.create(temp)

# sequence of plots -------------------------------------------------------

index <- c(1:29, seq(30, 200, 10), 300, 400, 500) 

matrix_list <- map(index, reconstruct_matrix, pca_obj = pic_pca)
plot_list <- map(matrix_list, plot_gray)  

# gifs --------------------------------------------------------------------

paths <- file.path(temp, paste0("pic-", seq_along(plot_list), ".png"))
walk2(paths, plot_list, ggsave, device = "png", bg = "antiquewhite", width = 5, height = 5)

paths %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 10) %>%
  image_write("visualization/image_pca/dubois_black_and_white.gif")




