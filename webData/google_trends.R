
library(tidyverse)
library(gtrendsR)

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

