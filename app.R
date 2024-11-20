library(shiny)
library(bslib)
library(dplyr)
library(googledrive)

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "renato.cava@upch.pe"
)

# penguins_csv <- "https://raw.githubusercontent.com/jcheng5/simplepenguins.R/main/penguins.csv"

# df <- readr::read_csv(penguins_csv)
# Find subset of columns that are suitable for scatter plot
# df_num <- df |> select(where(is.numeric), -Year)

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