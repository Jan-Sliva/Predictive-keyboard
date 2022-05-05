adress <- "E:/honzi/Documents/R/Predictive-keyboard/Shiny app/Predictive_keyboard_czech_15MB"

files <- CreateMeta(data_tree, 5)


lapply(1:6, function(x){
  
  write.csv(files[[x]], paste0(adress, "/", x, ".csv"))
})
