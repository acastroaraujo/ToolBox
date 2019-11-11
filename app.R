

## ************************
## Simulación
## ************************

## Se puede usar una distribución normal y después arm::invlogit (distribución logística) 
## o stats::pnorm (probit) para simular datos entre 0 y 1.

# mu <- 0       ## corresponde a 0.5 luego de ser pasado por arm::invlogit o stats::pnorm 
# sigma <- 0.5  ## desviación estándar (más o menos)        
# N <- 10000    ## número de mesas de votación
# 
# sim <- rnorm(N, mean = mu, sd = sigma) %>% pnorm()
# summary(sim)
# ggplot(tibble(sim), aes(sim)) + geom_histogram(color = "black")

## ************************
## Aplicación
## ************************

library(shiny)
library(shinythemes)
library(tidyverse)

theme_custom <- function(base_line_size = 0.25) {
  theme_minimal(base_family = "IBM Plex Sans", base_line_size = base_line_size) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = base_line_size),
      strip.background = element_rect(fill = "gray80", color = "gray80")
    ) 
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("lumen"),
  
  titlePanel(title = strong("Cómo inventar cuentos con discontinuidades."), windowTitle = "simulación discontinuidades"),
  shiny::icon("fab fa-twitter"), a(href="https://twitter.com/acastroaraujo", "@acastroaraujo"),
  br(), br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3(strong("Instrucciones: ")),
      HTML("1. Evitar discusiones sobre incertidumbre (ej. no mostrar intervalos de confianza)"), br(), br(),
      HTML("2. Usar un modelo estadístico inadecuado"), br(), br(),
      HTML('3. No mostrar los datos reales, explicando algo así como "en esta gráfica se eliminan las observaciones, de manera que se refleje la tendencia con mayor claridad"'),
      
      h3(strong("Simulación de datos: ")),
      
      numericInput("N", label = strong("Número de observaciones: "), value = 10000),
      
      HTML("<em>Nota. Más allá de 10,000 observaciones puede ser muy lento.</em>"), 
      
      br(), br(),
      
      actionButton(inputId = "create_sample", label = "Simular datos!"),
      
      br(), br(),
      
      HTML("Descripción de la variable simulada:"),
      
      br(), br(),
      
      verbatimTextOutput("sim_summary"),
      
      HTML("Por construcción, no hay relación alguna entre ambos ejes de la gráfica"),
      
      hr(), hr(),
      sliderInput("cutoff", label = strong("Discontinuidad en: "), 
                  min = 0, max = 1, value = 0.5, step = 0.01),
      
      radioButtons("model", label = strong("Model estadístico: "), 
                   choices = c("LOESS" = "loess", "regresión polinomial" = "poly"), 
                   selected = "loess", inline = TRUE),
      
      conditionalPanel(
        condition = "input.model == 'poly'",
        sliderInput("poly_degree", "Polinomio de grado: ", 
                    min = 2, max = 8, value = 3, step = 1)
        )
    ), 
    mainPanel(
      
      tabsetPanel(
        tabPanel(title = "Gráficas", plotOutput("raw_data_plot"), plotOutput("bad_model_plot")),
        tabPanel(title = "Código", verbatimTextOutput("shiny_code"))
      )
    )
  )
  
)

server <- function(input, output, session) {
  
  
  sim <- eventReactive(eventExpr = c(input$create_sample, input$N), ignoreNULL = FALSE, {
    
    req(input$N)
    
    mu <- 0       ## corresponde a 0.5 luego de ser pasado por arm::invlogit o stats::pnorm 
    sigma <- 0.5  ## desviación estándar, corresponde más o menos a 18 puntos porcentuales.,,
    
    pnorm(rnorm(input$N, mean = mu, sd = sigma))
    
  })
  
  output$sim_summary <- renderPrint({ summary(sim(), digits = 2) })
  
  df <- reactive({
    
    tibble(y = sim(), x = seq_along(sim()) / input$N) %>% 
      mutate(group = ifelse(x > input$cutoff, "post", "pre"))
    
  })
  
  output$raw_data_plot <- renderPlot({
    
    df() %>% 
      ggplot(aes(x, y)) + 
      geom_point(aes(color = group), alpha = 0.5, show.legend = FALSE) + 
      geom_vline(xintercept = input$cutoff, linetype = "dashed") +
      geom_abline(intercept = 0.5, slope = 0, color = "blue", size = 1) +
      theme_custom() +
      labs(x = "\nx", y = "y\n", title = "Datos simulados", subtitle = "Por construcción, no existe ningún patrón en los datos")
    
  })
  
  output$bad_model_plot <- renderPlot({
    
    g <- df() %>% 
      ggplot(aes(x, y, group = group)) + 
      geom_vline(xintercept = input$cutoff, linetype = "dashed") +
      theme_custom() +
      labs(x = "\nx", y = "y\n", title = "Resultado", 
           subtitle = "Combinar un modelo no-lineal con una discontinuidad casi siempre produce salto abrupto")
    
    if (input$model == "loess") {
      return(g + geom_smooth(method = "loess", se = FALSE))
    }
    
    if (input$model == "poly") {
      return(g + geom_smooth(method = "lm", se = FALSE,
                             formula = y ~ poly(x, input$poly_degree)))
    }

    
  })
  
  output$shiny_code <- renderPrint({
    readLines("app.R") %>% 
      writeLines()
  })
  
}

shinyApp(ui, server)





