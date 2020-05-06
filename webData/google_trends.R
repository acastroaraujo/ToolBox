
library(tidyverse)
library(gtrendsR)


# Basic Search and Visualization ------------------------------------------

results <- gtrends("things to do", geo = "US-NY-501", time = "2019-01-01 2019-04-01")

results$interest_over_time %>% 
  ggplot(aes(date, hits)) + 
  geom_line() + 
  geom_point(
    data = filter(results$interest_over_time, rank(-hits) <= 5),
    color = "tomato"
    ) +
  theme_minimal() +
  labs(title = "Google Trends", subtitle = '"things to do"')


# Example Analysis --------------------------------------------------------

# This script gathers data for a combination of categories and states in USA
# It can a take a while to download though...

# Categories and places ---------------------------------------------------

categories <- gtrendsR::categories %>% 
  filter(str_detect(tolower(name), "travel|hotel"))

states <- gtrendsR::countries %>% 
  mutate_all(as.character) %>% 
  filter(paste0(sub_code, "-", name) %in% paste0(paste0("US-", state.abb), "-", str_to_upper(state.name))) 

# Lookup tables -----------------------------------------------------------

lookup_category <- categories$name
names(lookup_category) <- categories$id

lookup_state <- state.name
names(lookup_state) <- state.abb


# Helper functions --------------------------------------------------------

decompose_series <- function(df) {
  require(tsibble); require(feasts)
  
  input <- df %>% 
    mutate(date = as.Date(date)) %>%
    as_tsibble(index = date) 
  
  out <- input %>% 
    model(STL(hits)) %>%
    components()
  
  out %>% 
    full_join(input) %>% 
    as_tibble()
}


# States ------------------------------------------------------------------

output1 <- vector("list", nrow(categories) * nrow(states))

n <- 1
for (i in seq_len(nrow(categories))) {
  for (j in seq_len(nrow(states))) {
    output1[[n]] <- gtrends(
      keyword = NA, category = categories$id[[i]], geo = states$sub_code[[j]],
      time = paste(as.Date("2020-01-01"), Sys.Date()),
      onlyInterest = TRUE
    )
    
    cat("\r", scales::percent(n / length(output)))
    n <- n + 1
    Sys.sleep(runif(1, 0, 3))
  }
}

state_output <- output1 %>% 
  map( ~ .x$interest_over_time) %>% 
  map( ~ .x %>% mutate( 
    category = lookup_category[as.character(category)], 
    state_abb = str_remove(geo, "US-"),
    state = lookup_state[state_abb]
  )) %>% 
  map(decompose_series) %>% 
  bind_rows()

# National - Since 2020 ---------------------------------------------------

output2 <- vector("list", nrow(categories))

for (i in seq_len(nrow(categories))) {
  output2[[i]] <- gtrends(
    keyword = NA, category = categories$id[[i]], geo = "US",
    time = paste(as.Date("2020-01-01"), Sys.Date()), 
    onlyInterest = TRUE
  )
  
  cat("\r", scales::percent(i / length(output)))
  Sys.sleep(runif(1, 0, 3))
}

usa_output <- output2 %>% 
  map( ~ .x$interest_over_time) %>% 
  map( ~ .x %>% mutate(category = lookup_category[as.character(category)])) %>% 
  map(decompose_series) %>% 
  bind_rows()

