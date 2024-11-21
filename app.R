# Set the application-level cache -----------------------------------------------
shinyOptions(cache = cachem::cache_mem(max_size = 500e6))

# Load libraries ----------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)

# User Interface Object
ui <- page_sidebar(
  # App theme ----
  theme = bs_theme(bootswatch = "simplex"),
  # App title ----
  title = "PIPE",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    actionButton("total", "Indicadores Total", class = "btn-primary")
    
  )
  # textOutput("text"),
)

server <- function(input, output, session) {
  
  # bs_themer()

  observeEvent(input$total, {
    withProgress( 
      min = 1,
      max = 2,
      # message = 'Calculation in progress', 
      # detail = 'This may take a while...', 
      
      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        source(file = "indicadores_total.R")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      } 
    ) 
    
    
    # output$text <- renderText({ "Se actualizaron los Indicadores Totales" })
  })
  
}

shinyApp(ui, server)