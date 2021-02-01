library(shiny)
library(stringr)

ui <- fluidPage(
  textInput("vstup", "Predictive keyboard"),
  textOutput("vystup")

)

server <- function(input, output){
  output$vystup <- reactive(app_hashed[[str_extract(input$vstup, regex("[[:alpha:]'<>-]+(?= *?$)"))]]$DruheSlovo)
}

shinyApp(ui = ui, server = server)

