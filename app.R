library(shiny)
library(bslib)
library(dplyr)

# penguins_csv <- "https://raw.githubusercontent.com/jcheng5/simplepenguins.R/main/penguins.csv"

# df <- readr::read_csv(penguins_csv)
# Find subset of columns that are suitable for scatter plot
# df_num <- df |> select(where(is.numeric), -Year)

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "simplex"),
  sidebar = sidebar(
    actionButton("total", "Indicadores Total", class = "btn-primary")
    
  ),
  textOutput("text"),
)

server <- function(input, output, session) {
  
  # bs_themer()

  observeEvent(input$total, {
    withProgress( 
      min = 1,
      max = 10,
      # message = 'Calculation in progress', 
      # detail = 'This may take a while...', 
      
      {
        setProgress(1, message = "Here we go")
        source(file = "indicadores_total.R")
        
        setProgress(10, message = "Finished!")
      } 
    ) 
    
    
    output$text <- renderText({ "Se actualizaron los Indicadores Totales" })
  })
  
}

shinyApp(ui, server)