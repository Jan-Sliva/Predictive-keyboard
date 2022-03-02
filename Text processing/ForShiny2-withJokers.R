library(readr) # načítání textu ze souboru
library(stringr) # zpracování textu
library(dplyr) # zpracování tabulky
library(triebeard) # písmenkové stromy
# analýza počtu bigramů
library(quanteda) 
library(quanteda.textstats)

source("E:/honzi/Documents/R/Predictive-keyboard/NGramsTree/NGramTree.R")

freq_percent_limit <- 80
joker <- "<>"
nGramLimit <- 8

# reading text file ----------------------------------------------------------------------------
raw_text_twitter <- tolower(read_lines("E:/honzi/Documents/final/en_US/test_twitter.txt"))
raw_text_news <- tolower(read_lines("E:/honzi/Documents/final/en_US/test_news.txt"))
raw_text_blogs <- tolower(read_lines("E:/honzi/Documents/final/en_US/test_blogs.txt"))

raw_text <- append(append(raw_text_twitter, raw_text_news), raw_text_blogs)

# making tokens object -------------------------------------------------------------------------
raw_words <- tokens(raw_text, what = "word", remove_symbols = TRUE, 
                    remove_numbers = TRUE, remove_punct = TRUE,
                    remove_url = TRUE, padding = FALSE)

filtered_words_all <- tokens_remove(raw_words, "[^[:alpha:][:punct:]]", valuetype = "regex")

# counting single words -----------------------------------------------------------------------
words_dfm_all <- dfm(filtered_words_all)

data_uniqueCounts_all <- ncol(words_dfm_all)

data_counts_all <- sum(ntoken(filtered_words_all))

words_freq_all <- textstat_frequency(words_dfm_all)

# determine frequency limit
freq_freq = as.data.frame(table(words_freq_all$frequency))

names(freq_freq) <- c("freq", "freqOfFreq")

freq_freq$freq <- as.numeric(levels(freq_freq$freq))[freq_freq$freq]

freq_freq$prop = freq_freq$freqOfFreq / data_uniqueCounts_all * 100

sum_freq_freq <- freq_freq$prop[1]
lastdiff <- freq_percent_limit

for (i in 2:length(freq_freq$prop)){
  sum_freq_freq <- sum_freq_freq + freq_freq$prop[i]
  if(sum_freq_freq >= freq_percent_limit){
    if(sum_freq_freq - freq_percent_limit <= lastdiff) freq_limit <- freq_freq$freq[i]
    else freq_limit <- freq_freq$freq[i-1]
    break
  }
  lastdiff <- freq_percent_limit - sum_freq_freq
}


# determine words to remove
words_to_remove <- (words_freq_all %>% filter(frequency <= freq_limit))$feature


# replace with jokers
filtered_words <- tokens_replace(filtered_words_all, words_to_remove, rep(joker, length(words_to_remove)), "fixed")


words_dfm <- dfm(filtered_words)

words_freq <- textstat_frequency(words_dfm)

data_uniqueCounts <- ncol(words_dfm)
  
data_counts <- sum(ntoken(filtered_words))

words_freq$prop <- words_freq$frequency / data_counts

words_freq$order <- 1:length(words_freq$feature)

data_coll <- list()

data_coll[[1]] <- data.frame(order = words_freq$order,
                             ngram = words_freq$feature,
                             freq = words_freq$frequency,
                             prop = words_freq$prop
)

# counting collocations -----------------------------------------------------------------------
data_counts[2:5] <- sapply(2:5, function(num){
  
  sum(sapply(ntoken(filtered_words) - num, function(x) max(0, x) ))
})

data_coll[2:5] <-  mapply(function(x, minCount){
  
  x_gram <- textstat_collocations(filtered_words, size = x, min_count = minCount)
  if(nrow(x_gram) > 0){
    ret = data.frame(order = 1:nrow(x_gram), ngram = x_gram$collocation,
                     freq = x_gram$count, prop = NA)
  }else{
    ret = data.frame(order = c(), ngram = c(),
                     freq = c(), prop = c())
  }
  return(ret)
  
}, x = 2:5, minCount = rep(nGramLimit, 4), SIMPLIFY = FALSE)

 # counting conditional frequency of n-grams ------------------------------------------------------

for (x in 1:4){
  for (index in 1:nrow(data_coll[[x]])){
    prefix = paste0(data_coll[[x]][index,]$ngram, " ")
    thisFreq = data_coll[[x]][index,]$freq
    
    data_coll[[x + 1]] <- data_coll[[x + 1]] %>% mutate(prop = 
                                                          ifelse(
                                                            startsWith(ngram, prefix), 
                                                            freq / thisFreq, 
                                                            prop))
  }
}

# making data tree -------------------------------------------------------------------------------
data_tree <- CreateNGramTree(data_coll, 10, "ngram", " ", "prop", joker)

