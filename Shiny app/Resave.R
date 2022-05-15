adress <- "Shiny app/Predictive_keyboard_czech_15MB"

toRewrite <- lapply(1:6, function(x){
  
  read.csv(paste0(adress, "/", x, ".csv"))
})


lapply(1:6, function(x){
  
  write.csv(toRewrite[[x]], paste0(adress, "/", x, "v2.csv"),
            fileEncoding = "UTF-8")
})
