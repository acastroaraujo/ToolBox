
install.packages("extrafont")

## Donwload fonts and add manually to Font Book
## For example:
## https://fonts.google.com/specimen/IBM+Plex+Sans

extrafont::font_import()  ## type y (this might take a while)

ggplot(tibble(x = runif(100))) + 
  geom_density(aes(x)) + 
  theme_minimal(base_family = "IBM Plex Sans") +
  labs("IBM Plex Sans") 
