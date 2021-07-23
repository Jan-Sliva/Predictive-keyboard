library(purrr)
library(ggplot2)
library(rbenchmark)
library(dplyr)
library(scales)


source("TimeTests/NGramTree/Měření 2/RClasses.R")

seed <- 555
listLength <- 100
rounds <- 10
replic <- 1000
maxValue <- 1000

set.seed(seed)

listSeeds <- floor(runif(rounds, -.Machine$integer.max, .Machine$integer.max))


results <- map_dfr(1:rounds, function(number){
  
  thisSeed <- listSeeds[[number]]
  
  testList <- createListNodes(listLength, thisSeed, listLength)
  testS3 <- createS3Nodes(listLength, thisSeed, listLength)
  testS4 <- CreateS4Nodes(listLength, thisSeed, listLength)
  
  x <- benchmark(      "List-TopXSort" = {GetTop5List_TopXSort(testList)},
                       "List-Order" = {GetTop5List_Order(testList)},
                       "S3-TopXSort" = {GetTop5S3_TopXSort(testS3)},
                       "S3-Order" = {GetTop5S3_Order(testS3)},
                       "S4-TopXSort" = {GetTop5S4_TopXSort(testS4)},
                       "S4-Order" = {GetTop5S4_Order(testS4)},
                       replications = replic
  )
  
  x$round <- number
  
  return(x)
  
})

means <- group_by(results, test) %>%
  summarise(mean = mean(elapsed))

ggplot() + 
  geom_point(data = means, mapping = aes(x = test, y = mean, color = test), size = 3, alpha = 0.5) +
  geom_point(data = results, mapping = aes(x = test, y = elapsed, color = test), position = position_jitter(height = 0)) +
  labs(x = "Použitá metoda", y = "Uplynulý čas [s]", 
                     color = "Použitá metoda",
                     title = "Vybírání 5 nějvětší prvků z listu a jejich reprezentace",
                     subtitle = "Proběhlo 10 měření, při každém měření se 1 000 krát opakovalo vybírání 5 největších z náhodného listu 100 prvků
Malými tečkami jsou jednotlivá měření, velkými tečkami je jejich průměr") +
  scale_y_continuous(breaks = breaks_extended(20), limits = c(0, NA))

write.csv(results, "TimeTests/NGramTree/Měření 2/Results.csv")
write.csv(means, "TimeTests/NGramTree/Měření 2/Means.csv")
