library(purrr)
library(ggplot2)
library(rbenchmark)
library(dplyr)
library(scales)

fileName <- "Měření 9"

source(paste0("TimeTests/NGramTree/", fileName, "/RClasses.R"))

seed <- 555
listLength <- 100
rounds <- 100
replic <- 1000
maxValue <- 1000

set.seed(seed)

listSeeds <- floor(runif(rounds, -.Machine$integer.max, .Machine$integer.max))


results <- map_dfr(1:rounds, function(number){
  
  thisSeed <- listSeeds[[number]]
  
  testList <- createListNodes(listLength, thisSeed, maxValue)
  testS3 <- createS3Nodes(listLength, thisSeed, maxValue)
  testS4 <- CreateS4Nodes(listLength, thisSeed, maxValue)
  testR6 <- CreateR6Nodes(listLength, thisSeed, maxValue)
  
  x <- benchmark(      "List-Rcpp-TopXSort" = {GetTop5List_TopXSortRcpp(testList)},
                       "S3-Rcpp-TopXSort" = {GetTop5S3_TopXSortRcpp(testS3)},
                       "S4-Rcpp-TopXSort" = {GetTop5S4_TopXSortRcpp(testS4)},
                       "R6-Rcpp-TopXSort" = {GetTop5R6_TopXSortRcpp(testR6)},
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
       subtitle = "Proběhlo 100 měření, při každém měření se 1 000 krát opakovalo vybírání 5 největších z náhodného listu 100 prvků
Malými tečkami jsou jednotlivá měření, velkými tečkami je jejich průměr") +
  scale_y_continuous(breaks = breaks_extended(10), limits = c(0, NA)) +
  theme(axis.text.x = element_text(size=10))


write.csv(results, paste0("TimeTests/NGramTree/", fileName, "/Results2.csv"))
write.csv(means, paste0("TimeTests/NGramTree/", fileName, "/Means2.csv"))
