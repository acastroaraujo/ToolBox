
# *************************************************************************
# Packages
# *************************************************************************

library(sf)
library(tidyverse)
library(tidycensus)

# *************************************************************************
# Access Setup
# *************************************************************************

## This key is linked to andres.castroaraujo@tbwachiat.com
census_key <- "1b37027d05fc830c7304e4182eac9198a59046f8"
census_api_key(census_key)
options(tigris_use_cache = TRUE)

dictionary <- load_variables(2017, "acs5", cache = TRUE)

# *************************************************************************
# Explore variables here (they resemble what's stored in "dictionary"):
# https://www.socialexplorer.com/data/ACS2016/metadata/?ds=ACS16
# 
# In order to download the data we input the right variable names into the
# tidycensus::get_acs() function. It's easier to do this many times if we
# create a small helper function like help_get_data()
# *************************************************************************

help_get_data <- function(vars, state_abbr = NULL, geography = "county", map_info = FALSE) {
  
  output <- tidycensus::get_acs(
    variables = vars,
    state = if (!is.null(state_abbr)) paste(state_abbr) else NULL,
    geography = geography,
    geometry = map_info) %>% 
    rename_all(str_to_lower) %>% 
    select(-moe) %>%
    spread(variable, estimate) %>% 
    mutate(state = str_extract(name, ", .*") %>% str_remove(", ")) %>% 
    mutate(name = str_remove(name, " County, .*")) %>% 
    left_join(tibble(state = state.name, state_abbr = state.abb))
  
  return(output)

}


# This function discards the margin of errors in exchange for the possibility of stacking
# many similar variables into one.

# Demonstration

# First, create a named vector
pop_total <- c(pop_total = "B01003_001")

# Second, download info with map data (in case you don't have a map)
df <- help_get_data(pop_total, map_info = TRUE)

# Third, create a named vector of variables that suit your purpose.
# For example: Men above 65 years
# Source: https://www.socialexplorer.com/data/ACS2016/metadata/?ds=ACS16&table=B01001
# The _ between 1 and 0 comes from looking at the dictionary.

code <- paste0("B01001_02", 0:5)
names(code) <- paste0("men_65_plus", 0:5)

# Fourth, download
men_plus_65 <- help_get_data(code)

# Clean up
men_plus_65 <- men_plus_65 %>%
  pivot_longer(contains("men_65"), names_to = "var", values_to = "value") %>% 
  mutate(var = str_replace(var, "men_65_plus\\d", "men_65_plus")) ## regex clean


