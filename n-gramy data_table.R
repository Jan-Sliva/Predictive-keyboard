# jako token budu používat "<>"
library(tidyverse)
library(quanteda)
library(data.table)

zmenitNaToken <- function(x){
  if (has.key(x, hashed_character))
    return("<>")
  else
    return(x)
}

result_tokeny <- lapply(results_test, function(x){unlist(vapply(x, zmenitNaToken, character(1)))})

x <- tokens(result_tokeny, what = "character")

bigramy <- textstat_collocations(x, min_count = 5, size = 2)

app_bigramy <- data.table(str_extract(bigramy$collocation, regex("^[[:alpha:][:punct:]<>]+(?= )")), 
                          str_extract(bigramy$collocation, regex("(?<= )[[:alpha:][:punct:]<>]+$")), bigramy$count)

setnames(app_bigramy, c("V1", "V2", "V3"), c("PrvniSlovo", "DruheSlovo", "Freq"))

app_bigramy <- app_bigramy[DruheSlovo != "<>"]

setorder(app_bigramy, -Freq)
