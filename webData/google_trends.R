
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



# Extra functions ---------------------------------------------------------

check_and_fix_interest_over_time <- function(data_frame) {
  
  ok <- names(data_frame$interest_over_time) %in% c("date", "hits", "keyword", "geo", "time", "gprop", "category")
  if (all(!ok)) stop(call. = FALSE, "Something is wrong with the <<interest_over_time>> data frame")
  
  if (class(data_frame$interest_over_time$hits) == "character") {
    data_frame$interest_over_time <- data_frame$interest_over_time %>% 
      mutate(hits = str_replace(hits, "<1", "0") %>% as.integer())
  }
  
  return(data_frame)
  
}

custom_seasonal_layout <- function(data_frame, type = c("season_year", "remainder")) {
  
  subtitle <- switch(match.arg(type),
    "season_year" = "Seasonal Effects",
    "remainder" = "Unexplained Variance"
  )
  
  remove_year <- function(x) {
    input <- as.POSIXlt(x) %>% unclass()
    input$year <- 0
    class(input) <- c("POSIXlt", "POSIXt")
    output <- as.Date(input)
    return(output)
  }
  
  y_var <- sym(match.arg(type))
  
  df <- data_frame %>% 
    mutate(new_date = map(date, remove_year)) %>% 
    unnest(new_date) %>% 
    mutate(month = lubridate::month(date, label = TRUE) %>% as.character()) %>% 
    group_by(keyword)
  
  if (type == "season_year") {
    df <- df %>% 
      mutate(month_label = ifelse(
        test = {{y_var}} == max({{y_var}}) | {{y_var}} == min({{y_var}}), 
        yes = month, 
        no = "")
      ) %>% ungroup()
  }
  
  if (type == "remainder") {
    df <- df %>% 
      mutate(month_label = ifelse(
        test = {{y_var}} == max({{y_var}}) | {{y_var}} == min({{y_var}}), 
        yes = paste(month, year), 
        no = "")
      ) %>% ungroup()
  }
  
  df %>% 
    ggplot(aes(new_date, {{y_var}})) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_line(aes(color = year)) + 
    ggrepel::geom_label_repel(
      mapping = aes(label = month_label),
      size = 3,
      fill = NA,
      direction = "y"
    ) + 
    facet_wrap(~ str_wrap(keyword, 20)) + 
    scale_x_date(date_labels = "%b") +
    theme_light(base_family = "Avenir Next Condensed") + 
    labs(x = NULL, y = NULL, subtitle = subtitle,
         caption = "Source: Google Trends + TBWA",
         color = NULL) + 
    theme(plot.title = element_text(size = 22),
          plot.subtitle = element_text(face = "italic"),
          legend.position = "bottom")
  
}


wordcloud <- function(df, by = c("queries", "topics")) {
  require(ggwordcloud)
  
  by <- match.arg(by)
  
  plot_title <- switch (by,
    "queries" = "Related Queries",
    "topics" = "Related Topics"
  )
  
  switch(by,
    "queries" = if (is.null(df$related_queries)) return(NULL),
    "topics" = if (is.null(df$related_topics)) return(NULL)
  )
  
  data_frame <- switch(by,
    "topics" = df$related_topics %>% filter(related_topics == "top"),
    "queries" = df$related_queries %>% filter(related_queries == "top")
  ) %>% 
    mutate(subject = str_replace(subject, "<1", "0") %>% as.numeric()) 
  
  data_frame %>% 
    ggplot(aes(size = subject, label = value, color = subject)) + 
    geom_text_wordcloud(family = "Averta for TBWA", fontface = "bold", shape = "square") +
    scale_color_viridis_c(option = "viridis", begin = 0.3, end = 0.85, direction = -1) +
    labs(title = plot_title) +
    theme_minimal(base_family = "Avenir Next Condensed", base_line_size = 0) +
    theme(title = element_text(face = "bold"))
}

