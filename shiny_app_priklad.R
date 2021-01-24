library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 3, min = 1, max = 10, pre = "10^"
              ),
  plotOutput("hist")
  )

server <- function(input, output){
  output$hist <- renderPlot({
   hist(rnorm(10**(input$num)), main = "Normal distribution", xlab = "value")
    
  })
  
}

shinyApp(ui = ui, server = server)

