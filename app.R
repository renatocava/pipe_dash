library(shiny)
library(bslib)
library(dplyr)

# penguins_csv <- "https://raw.githubusercontent.com/jcheng5/simplepenguins.R/main/penguins.csv"

# df <- readr::read_csv(penguins_csv)
# Find subset of columns that are suitable for scatter plot
# df_num <- df |> select(where(is.numeric), -Year)

ui <- page_sidebar(
  sidebar = sidebar(
    actionButton("total", "Indicadores Total")
    
  ),
  textOutput("text"),
)

server <- function(input, output, session) {
  observeEvent(input$total, {
    source(file = "indicadores_total.R")
    
    output$text <- renderText({ "Termino el proceso" })
  })
  
}

shinyApp(ui, server)