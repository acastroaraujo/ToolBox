
# This app contains my approach to working with maps and ggplot2.
# It contains hover, brush, and zoom functionalities.
# It also contains a data table from the DT package
#
# Two main resources for learning Shiny:
#
# https://shiny.rstudio.com/
# https://mastering-shiny.org/
# 
# These three articles are especially important to make ggplot2 interactive:
#
# https://shiny.rstudio.com/articles/plot-interaction.html
# https://shiny.rstudio.com/articles/selecting-rows-of-data.html
# https://shiny.rstudio.com/articles/plot-interaction-advanced.html
#
# Finally, I used this app to learn more about the bootstraplib package
# https://rstudio.github.io/bootstraplib
#
# TO DO:
# - ZOOM
# - BOOTSTRAPLIB
# - DT aesthetics

# Packages

library(shiny)
library(tidyverse)
library(sf)
library(DT)

# Custom functions

source("app_functions.R")

# Data --------------------------------------------------------------------

county_map <- read_rds("data_clean/county_map.rds")
  
state_map <- county_map %>%
  group_by(state_fips, state) %>%
  summarize(miles_squared = sum(miles_squared),
            pop_total = sum(pop_total),
            geometry = sf::st_union(geometry)) %>%
  sf::st_as_sf()   #### not necessary in some versions of R (I think)

county_usafacts_agg <- read_rds("data_clean/county_usafacts.rds") %>% 
  group_by(fips, state_fips) %>%
  summarize(cases = sum(cases), deaths = sum(deaths))

county_usafacts <- county_map %>%
  full_join(county_usafacts_agg) %>%
  replace(is.na(.), 0) 

county_usafacts_grid <- county_usafacts %>% 
  mutate(coords = map(geometry, sf::st_coordinates) %>% map(as.data.frame)) %>% 
  as.data.frame() %>%
  select(-geometry) %>% 
  unnest(cols = coords)

ui <- fluidPage(
  headerPanel("COVID-19 Information by County", windowTitle = "covid-19 map"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "style", 
                   label = strong("Style"),
                   choices = c("Points" = "dots", "Choropleth" = "choropleth"),
                   selected = "dots",
                   inline = TRUE),
      
      radioButtons(inputId = "variable", 
                   label = strong("Variable"),
                   choices = c("Deaths" = "deaths", "Cases" = "cases"), 
                   selected = "deaths",
                   inline = TRUE),
      hr(),
      htmlOutput(outputId = "usafacts_map_hover_summary")
    ), 
    mainPanel(
      plotOutput(outputId = "usafacts_map", hover = "usafacts_map_hover", brush = "usafacts_map_brush")
    )  ## TO DO: ADD ZOOM, I THINK IT'S A DBL CLICK
  ),
  
  # verbatimTextOutput("console"), ## I use this to glimpse into input objects
                                   ## while I develop the app
  
  DT::dataTableOutput("usafacts_table")
  
)

server <- function(input, output, session) {
  
  # output$console <- renderPrint({   ## I use this to glimpse into input objects
  #   input$usafacts_map_brush        ## while I develop the app
  # })
  
  usafacts_map_data <- reactive({
    
    ## Read more about reactivity here: 
    ## https://shiny.rstudio.com/articles/reactivity-overview.html
    ## https://mastering-shiny.org/why-reactivity.html
    
    output <- county_usafacts
    
    if (input$style == "dots") {
      output <- county_usafacts %>% 
        mutate(geometry = sf::st_centroid(geometry))
    } 
    
    return(output)
    
  })
  
  output$usafacts_map <- renderPlot({
    
    if (input$style == "dots") {
      return(gg_dots(usafacts_map_data(), state_map, input$variable))
    }

    if (input$style == "choropleth") {
      return(gg_choropleth(usafacts_map_data(), state_map, input$variable))
    }
    
  })
  
  output$usafacts_map_hover_summary <- renderUI({
    
    if (is.null(input$usafacts_map_hover)) {
      return(HTML('<font size="1" color="grey">Move the cursor onto the map to view more information</font>'))
    }
    
    message_obj <- county_usafacts %>%
      filter(sf::st_contains(
        county_usafacts,
        sf::st_point(c(input$usafacts_map_hover$x, input$usafacts_map_hover$y)), sparse = FALSE)
      ) 
    
    if (nrow(message_obj) == 0) {
      return(HTML('<font size="1" color="grey">Move the cursor onto the map to view more information</font>'))
    }
      
    final_message <- paste0(
      em("County: "), message_obj[["name"]], br(),
      em("State: "), message_obj[["state"]], br(), br(),
      em(stringr::str_to_title(input$variable)), ": ", scales::comma(message_obj[[input$variable]]), " or ",
      round((message_obj[[input$variable]] / message_obj[["pop_total"]]) * 100000, 2),
      " per 100,000 people"
      )
        
    return(HTML(final_message))
    
  })
  
  county_usafacts_filtered <- reactive({
    
    if (is.null(input$usafacts_map_brush)) {
      return(county_usafacts %>% as.data.frame() %>% select(fips))
    } else {
      output <- county_usafacts_grid %>% 
        brushedPoints(input$usafacts_map_brush, xvar = "X", yvar = "Y") %>% 
        select(fips) %>% 
        distinct()
      return(output)
    }
    
  })
  
  output$usafacts_table <- DT::renderDataTable({
    
    ## More information about DT here: https://rstudio.github.io/DT/
    
    county_usafacts %>% 
      as.data.frame() %>% 
      inner_join(county_usafacts_filtered()) %>% 
      select(name, state, pop_total, miles_squared, cases, deaths) %>% 
      DT::datatable(rownames = FALSE)
    ## TO DO: add pretty things here
    
  })
  
}

shinyApp(ui, server)