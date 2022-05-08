source("load.R")
library(shiny)
library(shinyjs)
library(stringr)


predictedWords <- 10

maxNGram <- 5


ui <- fluidPage(
    useShinyjs(),
    titlePanel("Predictive keyboard (english, 15 MB)"),
    textInput("textInput", NULL, placeholder = "Enter text", width = "800px"),
    
    lapply(1:predictedWords, function(x) {
        hidden(actionButton(paste0("textButton", x), NA))
    })
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    textToRender <- reactive({
        
        lowerCaseInput <- tolower(input$textInput)
        
        isNewPart <- !grepl(" +$", lowerCaseInput)
        
        index <- regexpr("(?<=[\\.\\?\\!\"])[^\\.\\?\\!\"]*$", lowerCaseInput, perl = TRUE)
        
        if(index == -1){
            string <- lowerCaseInput
        }
        else{
            string <- substr(lowerCaseInput, index, index + attr(index, "match.length") - 1)
        }
        
        out <- strsplit(sub("^ +", "", string), " +")[[1]]
        
        if(length(out) == 0) return(GetByNothing(data_tree));
        
        if(isNewPart){
            out <- out[max(1, length(out) - maxNGram + 1):length(out)]
            
            if(length(out) > 1){
                GetBySeqAndPart(data_tree, out[1:(length(out) - 1)], out[length(out)])
            }
            else{
                GetByPart(data_tree, out[1])
            }
        }
        else{
            out <- out[max(1, length(out) - maxNGram + 2):length(out)]
            
            GetBySeq(data_tree, out)
        }
        
    })
    
    
    
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
                                value = paste0(sub("[[:alpha:][:punct:]]+$", "", input$textInput),
                                               textToRender()[x], " "))
        })
    } )
    
}

shinyApp(ui = ui, server = server)
