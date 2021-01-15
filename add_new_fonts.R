
install.packages("extrafont")

## Donwload fonts and add manually to Font Book
## For example:
## https://fonts.google.com/specimen/IBM+Plex+Sans

extrafont::font_import()  ## type y (this might take a while)

library(ggplot2)
ggplot(data.frame(x = runif(1000))) + 
  geom_density(aes(x)) + 
  theme_minimal(base_family = "Amiri") +
  labs(title = "The new font looks like this",
       subtitle = "lorem ipsum\n") 

