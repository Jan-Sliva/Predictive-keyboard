library(readr) # načítání textu ze souboru
library(stringr) # zpracování textu
library(dplyr) # zpracování tabulky
library(quanteda) # analýza počtu bigramů
library(quanteda.textstats)
library(data.tree)
library(triebeard) # písmenkové stromy

source("Text processing/Functions.R")

raw_text_twitter <- tolower(read_lines("E:/honzi/Documents/final/en_US/test2_twitter.txt"))
raw_text_news <- tolower(read_lines("E:/honzi/Documents/final/en_US/test2_news.txt"))
raw_text_blogs <- tolower(read_lines("E:/honzi/Documents/final/en_US/test2_blogs.txt"))

raw_text <- append(append(raw_text_twitter, raw_text_news), raw_text_blogs)

raw_words <- tokens(raw_text, what = "word", remove_symbols = TRUE, 
                    remove_numbers = TRUE, remove_punct = TRUE,
                    remove_url = TRUE, padding = FALSE)

filtered_words <- tokens_remove(raw_words, "[^[:alpha:][:punct:]]", valuetype = "regex")

raw_n_grams <- textstat_collocations(filtered_words, size = c(2, 3, 4), min_count = 15)

n_grams <- data.frame(n_gram = raw_n_grams$collocation, freq = raw_n_grams$count)

first_words <- unique(word(n_grams$n_gram))

data_trees <- sapply(first_words, 
                     function(first_word, data_frame){ 
                       FromDataFrameTable(data_frame %>% filter(startsWith(n_gram, paste0(first_word, " "))),
                                                                pathName = "n_gram",
                                                                pathDelimiter = " ")},
                     data_frame = n_grams)

lapply(data_trees, function(tree){
  
  tree$Do(function(node){
    
    names <- sapply(node$children, function(node) node$name)
    node$trie <- trie(names, names)
    }, filterFun = function(node) !node$isLeaf)
})
