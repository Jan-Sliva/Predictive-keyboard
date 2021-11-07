adress <- "E:/honzi/Documents/Documents/R/Predictive-keyboard/Shiny app/Predictive_keyboard_1.5MB"

lapply(1:5, function(x){
  
  write.csv(data_coll[[x]], paste0(adress, "/", x, "-gram.csv"))
})
