library(shiny)
library(stringr)
library(ggplot2)

ui <- fluidPage(
  textInput("vstup", "Predictive keyboard"),
  textOutput("vystup"),
  plotOutput("plot")
)

server <- function(input, output){
  table <- reactive(app_bigramy[PrvniSlovo == str_extract(input$vstup, regex("[[:alpha:][:punct:]<>]+(?= *?$)"))])
  output$vystup <- reactive(str_c(table()$DruheSlovo, collapse = ", "))
  output$plot <- renderPlot({ggplot(table(), aes(x = reorder(DruheSlovo, -Freq), y = Freq)) + 
      geom_point() + labs(title = "Frekvence bigramů zakončených daným slovem",
                          x = "Slovo",
                          y = "Frekvence") + theme(text = element_text(size=20),
                                                   axis.text.x = element_text(angle=90, hjust=1, size = 12)) +
      ylim(0,NA)})
}

shinyApp(ui = ui, server = server)


