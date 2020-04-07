
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


# Packages

library(shiny)
library(tidyverse)
library(sf)
library(DT)

# Custom functions

source("app_functions.R")

# Bootstraplib ------------------------------------------------------------
#
# Type `run_with_themer()` in the console to choose these settings:

library(bootstraplib)
bs_theme_new(bootswatch = "lumen")

bs_theme_add_variables(
  primary = "#F3DCEC",
  secondary = "#F3DCEC"
)

bs_theme_base_colors(bg = "#F3DCEC")

shinyOptions(plot.autocolors = TRUE)   ## isn't working yet??
shinyOptions(plot.autotheme = TRUE)  ### ARGH
# This is a weird fix. Remove the later calls to cowplot::ggdraw() when
# plot.autocolors works!

# library(cowplot)
# 
# theme_custom <- function() {
#   theme_void() %+replace% 
#     theme(plot.background = element_rect(fill = "#F3DCEC", color = NA))
# }



# Data --------------------------------------------------------------------

county_map <- read_rds("data_clean/county_map.rds")
  
state_map <- county_map %>%
  group_by(state_fips, state) %>%
  summarize(miles_squared = sum(miles_squared),
            pop_total = sum(pop_total),
            geometry = sf::st_union(geometry)) %>%
  sf::st_as_sf()   #### not necessary in some versions of R (I think)

county_usafacts_agg <- read_rds("data_clean/county_usafacts.rds") %>% 
  filter(fips != 0) %>% 
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
  bootstraplib::bootstrap(),
  
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
      plotOutput(outputId = "usafacts_map", #height = "400px", width = "400px",
                 hover = "usafacts_map_hover", brush = "usafacts_map_brush", dblclick = "usafacts_map_dblclick")
    )  
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
  
  ranges <- reactiveValues(x = NULL, y = NULL) ## initialize zoom
  
  output$usafacts_map <- renderPlot({
    
    if (input$style == "dots") {
      output <- gg_dots(usafacts_map_data(), state_map, input$variable)
    }

    if (input$style == "choropleth") {
      output <- gg_choropleth(usafacts_map_data(), state_map, input$variable)
    }
    
    output +                                                       ## remove this when no longer necessary
      coord_sf(xlim = ranges$x, ylim = ranges$y, expand = FALSE) + ## this controls dblclick + zoom
      theme_void()                                               ## this fixes the backround color issue
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$usafacts_map_dblclick, {
    brush <- input$usafacts_map_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
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
      arrange(desc(deaths, cases)) %>% 
      DT::datatable(rownames = FALSE, 
                    colnames = c("County", "State", "Population", "Square Miles", "Cases", "Deaths"), 
                    options = list(list(4, 'desc'), list(5, 'desc'))) %>% 
      DT::formatRound(c("pop_total", "cases", "deaths"), digits = 0) %>% 
      DT::formatRound(c("miles_squared"), digits = 2) 
    
  })
  
}

shinyApp(ui, server)


