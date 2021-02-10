# tento skript dělá aplikci, na které funguje Prediktivní klávesnice

# import knihoven
library(shiny) # knihovna pro dělání aplikace
library(stringr) # práce s textem (regex)
library(ggplot2) # grafy

# frontend

ui <- fluidPage(
  textInput("vstup", "Predictive keyboard"), # textový vstup
  textOutput("vystup"), # textový výstup
  plotOutput("plot") #grafový výstup
)

# backend

server <- function(input, output){
  
  # extrahuji záznamy z tabulky, kde První slove odpovídá poslednímu slovu ve vstupu
  
  table <- reactive(app_bigramy[PrvniSlovo == str_extract(input$vstup, regex("[[:alpha:][:punct:]<>]+(?= *?$)"))])
  
  # udělám výstup, tak, že vezmu z table všechny druhá slova a oddělím je čárkou (nemusím je seřazovat, už jsem je seřadil na konci Main.R)
  
  output$vystup <- reactive(str_c(table()$DruheSlovo, collapse = ", "))
  
  # udělám výstupový graf, že  vezmu table, na osu x dám Druhé slovo seřazené podle frekvence sestupně (jinak by se to řadilo becedně)
  # udělám graf s body, nastavím nadpis a popisek osy x a y, a potom přenastavím text na velikost 20 a text u osy x přensatvím na velikost 12 a otočím to o 90°
  # potom nastavím graf, aby ukazoval na ose y nulu
  
  output$plot <- renderPlot({ggplot(table(), aes(x = reorder(DruheSlovo, -Freq), y = Freq)) + 
      geom_point() + labs(title = "Frekvence bigramů zakončených daným slovem",
                          x = "Slovo",
                          y = "Frekvence") + theme(text = element_text(size=20),
                                                   axis.text.x = element_text(angle=90, hjust=1, size = 12)) +
      ylim(0,NA)})
}

shinyApp(ui = ui, server = server) # spouštím aplikace


