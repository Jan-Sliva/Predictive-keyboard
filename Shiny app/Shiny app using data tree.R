library(shiny)
library(shinyjs)
library(stringr)
library(triebeard)

source("E:/honzi/Documents/Documents/R/Predictive-keyboard/Text processing/Functions.R")

predictedWords <- 10

maxNGram <- 5

ui <- fluidPage(
  useShinyjs(),
  textInput("textInput", "Predictive keyboard", placeholder = "Text for predicting", width = "800px"),
  
  lapply(1:predictedWords, function(x) {
    hidden(actionButton(paste0("textButton", x), NA))
    })
)

server <- function(input, output){
  
  words <- reactive({
      x = unlist(strsplit(tolower(input$textInput), " +"))
      if (newWord() == "") x[max(length(x) - maxNGram + 1, 1):length(x)]
      else x[max(length(x) - maxNGram, 1):(length(x)-1)]
    })
  
  newWord <- reactive(tolower(word(input$textInput, -1)))
  
  textToRender <- reactive(GetNodesByInput(data_trees, words(), newWord(), predictedWords))
  
  observeEvent(input$textInput, {
    
    renderingText <- textToRender()
    
    if (is.null(renderingText)) renderingText <- rep(NA, predictedWords)
    
    if (length(renderingText) < predictedWords)
      renderingText <- c(renderingText, rep(NA, predictedWords - length(renderingText)))
    
    lapply(1:predictedWords, 
           function(x) {
             if (is.na(renderingText[x])){
               shinyjs::hide(paste0("textButton", x), anim = TRUE, animType = "fade")
             }
             else{
               updateActionButton(inputId = paste0("textButton", x), label = renderingText[x])
               shinyjs::show(paste0("textButton", x), anim = TRUE, animType = "fade")
             }})
  })
  
  lapply(1:predictedWords, function(x) {
    observeEvent(input[[paste0("textButton", x)]], {
      if (length(textToRender()) >= x)
        updateTextInput(inputId = "textInput", 
                        value = paste0(sub("[[:alpha:][:punct:]]+$", "",input$textInput),
                                       textToRender()[x], " "))
    })
  } )
  
}

shinyApp(ui = ui, server = server)

