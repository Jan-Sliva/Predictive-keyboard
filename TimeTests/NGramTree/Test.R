source("TimeTests/NGramTree/RClasses.R")

seed <- 555
count <- 100

testList <- createListNodes(count, seed)
testS3 <- createS3Nodes(count, seed)
testS4 <- CreateS4Nodes(count, seed)


results <- map_dfr(1:10, function(number){
  
  x <- benchmark(      "List-TopXSort" = {GetTop5List_TopXSort(testList)},
                       "List-Order" = {GetTop5List_Order(testList)},
                       "S3-TopXSort" = {GetTop5S3_TopXSort(testS3)},
                       "S3-Order" = {GetTop5S3_Order(testS3)},
                       "S4-TopXSort" = {GetTop5S4_TopXSort(testS4)},
                       "S4-Order" = {GetTop5S4_Order(testS4)},
                       replications = 1000,
                       columns = c("test", "elapsed")
  )
  
  x$round <- number
  
  return(x)
  
})

means <- group_by(results, test) %>%
  summarise(mean = mean(elapsed))

ggplot() + 
  geom_point(data = means, mapping = aes(x = test, y = mean, color = test), size = 3, alpha = 0.5) +
  geom_point(data = results, mapping = aes(x = test, y = elapsed, color = test), position = "jitter") +
  ylim(0, NA) + labs(x = "Použitá metoda", y = "Uplynulý čas [s]", 
                     color = "Použitá metoda",
                     title = "Vybírání 5 nějvětší prvků z listu a jejich reprezentace",
                     subtitle = "Proběhlo 10 měření, při každém měření se 1 000 krát opakovalo vybírání 5 největších z náhodného listu 100 prvků
Malými tečkami jsou jednotlivá měření, velkými tečkami je jejich průměr") 
