# jako token budu používat "<>"
library(tidyverse)
library(quanteda)
library(hash)

zmenitNaToken <- function(x){
  if (has.key(x, hashed_character))
    return("<>")
  else
    return(x)
}

result_tokeny <- lapply(results_test, function(x){unlist(vapply(x, zmenitNaToken, character(1)))})

x <- tokens(result_tokeny, what = "character")

bigramy <- textstat_collocations(x, min_count = 5, size = 2)

app_table <- data.frame(bigramy$collocation, bigramy$count)
names(app_table)[1:2] <- c("Bigram", "Freq")

app_table <-  app_table %>% filter(!endsWith(Bigram, "<>"))

app_table$PrvniSlovo <- sub(" [a-z']+", "", app_table$Bigram)
app_table$DruheSlovo <- sub("(<>)|[a-z']+ ", "", app_table$Bigram)

unique_slova <- unique(app_table$PrvniSlovo)

tables <- lapply(unique_slova, function(x){app_table %>% filter(PrvniSlovo == x) %>% select(DruheSlovo, Freq) %>% arrange(desc(Freq))})

app_hashed <- hash(unique_slova, tables)


