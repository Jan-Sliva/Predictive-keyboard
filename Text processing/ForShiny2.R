library(readr) # načítání textu ze souboru
library(stringr) # zpracování textu
library(dplyr) # zpracování tabulky
library(triebeard) # písmenkové stromy
# analýza počtu bigramů
library(quanteda) 
library(quanteda.textstats)

source("E:/honzi/Documents/Documents/R/Predictive-keyboard/NGramsTree/NGramTree.R")

# reading text file ----------------------------------------------------------------------------
raw_text_twitter <- tolower(read_lines("E:/honzi/Documents/final/en_US/test2_twitter.txt"))
raw_text_news <- tolower(read_lines("E:/honzi/Documents/final/en_US/test2_news.txt"))
raw_text_blogs <- tolower(read_lines("E:/honzi/Documents/final/en_US/test2_blogs.txt"))

raw_text <- append(append(raw_text_twitter, raw_text_news), raw_text_blogs)

# making tokens object -------------------------------------------------------------------------
raw_words <- tokens(raw_text, what = "word", remove_symbols = TRUE, 
                    remove_numbers = TRUE, remove_punct = TRUE,
                    remove_url = TRUE, padding = FALSE)

filtered_words <- tokens_remove(raw_words, "[^[:alpha:][:punct:]]", valuetype = "regex")

# counting single words -----------------------------------------------------------------------
words_dfm <- dfm(filtered_words)

words_freq <- textstat_frequency(words_dfm)

words_freq$order <- 1:length(words_freq$feature)

data_counts <- sum(ntoken(filtered_words))

data_uniqueCounts <- ncol(words_dfm)

words_freq$freqPercent <- words_freq$frequency / data_counts

data_coll <- list()

data_coll[[1]] <- data.frame(order = words_freq$order,
                             ngram = words_freq$feature,
                             freq = words_freq$frequency,
                             freqPercent = words_freq$freqPercent
)

# counting collocations -----------------------------------------------------------------------
data_counts[2:5] <- sapply(2:5, function(num){
  
  sum(sapply(ntoken(filtered_words) - num, function(x) max(0, x) ))
})

data_coll[2:5] <-  mapply(function(x, minCount){
  
  x_gram <- textstat_collocations(filtered_words, size = x, min_count = minCount)
  ret = data.frame(order = 1:nrow(x_gram), ngram = x_gram$collocation,
                   freq = x_gram$count, freqPercent = NA)
  return(ret)
  
}, x = 2:5, minCount = rep(2, 5), SIMPLIFY = FALSE)

# counting conditional frequency of n-grams ------------------------------------------------------

for (x in 1:4){
  for (index in 1:nrow(data_coll[[x]])){
    prefix = paste0(data_coll[[x]][index,]$ngram, " ")
    thisFreq = data_coll[[x]][index,]$freq
    
    data_coll[[x + 1]] <- data_coll[[x + 1]] %>% mutate(freqPercent = 
                                                        ifelse(
                                                        startsWith(ngram, prefix), 
                                                        freq / thisFreq, 
                                                        freqPercent))
  }
}

# making data tree -------------------------------------------------------------------------------
data_tree <- CreateNGramTree(data_coll, 10, "ngram", " ", "freqPercent")












