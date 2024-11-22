# Set the application-level cache -----------------------------------------------
shinyOptions(cache = cachem::cache_mem(max_size = 500e6))

# Load libraries ----------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)



# User Interface Object
ui <- page_sidebar(
  
  # App theme ----
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # App title ----
  title = h2("Peruvian Investigation of Pathogenic Enterobacterales"),
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    h4("Cargar"),
    actionButton("cargar_total", "Total", class = "btn-primary", disabled = T),
    actionButton("cargar_lima", "Lima", class = "btn-primary", disabled = T),
    actionButton("cargar_arequipa", "Arequipa", class = "btn-primary", disabled = T),
    actionButton("cargar_loreto", "Loreto", class = "btn-primary", disabled = T),
    actionButton("cargar_ucayali", "Ucayali", class = "btn-primary", disabled = T),
    
    br(),
    
    h4("Actualizar"),
    actionButton("actualizar_total", "Total", class = "btn-warning", disabled = T),
    actionButton("actualizar_lima", "Lima", class = "btn-warning", disabled = T),
    actionButton("actualizar_arequipa", "Arequipa", class = "btn-warning", disabled = T),
    actionButton("actualizar_loreto", "Loreto", class = "btn-warning", disabled = T),
    actionButton("actualizar_ucayali", "Ucayali", class = "btn-warning", disabled = T),
    
  ),
  
  navset_card_underline(
    title = "Indicadores",
    nav_panel("Total", tableOutput("table_total")),
    nav_panel("Lima", tableOutput("table_lima")),
    nav_panel("Arequipa", tableOutput("table_arequipa")),
    nav_panel("Ucayali", tableOutput("table_ucayali"))
  )
  
)

server <- function(input, output, session) {
  
  observeEvent(input$cargar_total, {
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
    
    output$table_total <- renderTable(indicadores_total)
    
    updateActionButton(session, "actualizar_total", disabled = F)
    
  })
  
  observeEvent(input$actualizar_total, {
    withProgress( 
      min = 1,
      max = 2,
      # message = 'Calculation in progress', 
      # detail = 'This may take a while...', 
      
      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::gs4_auth(cache = ".secrets", email = "renato.cava@upch.pe")
        googlesheets4::write_sheet(data = indicadores_total, ss = "https://docs.google.com/spreadsheets/d/1VvPH6FZ7d6-npHQeYvMGkogQ7-GGb0JkO6dI5agCDLg/edit?usp=sharing", sheet = "TEST Weekly Report IVI (DM)")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      } 
    )
    
    # output$table_total <- renderTable(indicadores_total)
    
    updateActionButton(session, "actualizar_total", disabled = T)
    
  })
  
  observeEvent(input$cargar_lima, {
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
    
  })
  
  observeEvent(input$cargar_arequipa, {
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
    
  })
  
  observeEvent(input$cargar_loreto, {
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
    
  })
  
  observeEvent(input$cargar_ucayali, {
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
    
  })
}

shinyApp(ui, server)