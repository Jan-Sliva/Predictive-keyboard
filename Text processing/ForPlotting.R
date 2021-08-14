library(readr) # načítání textu ze souboru
library(stringr) # zpracování textu
library(dplyr) # zpracování tabulky
library(quanteda) # analýza počtu bigramů
library(quanteda.textstats)
library(purrr)
library(ggplot2)

source("Text processing/Functions.R")
source("E:/honzi/Documents/Documents/R/Predictive-keyboard/Graphs/TextGraphs.R", encoding = 'UTF-8')

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

data_counts[2:7] <- sapply(2:7, function(num){
  
  sum(sapply(ntoken(filtered_words) - num, function(x) max(0, x) ))
})

data_coll[2:7] <-  mapply(function(x, minCount){
  
  x_gram <- textstat_collocations(filtered_words, size = x, min_count = minCount)
  ret = data.frame(order = 1:nrow(x_gram), ngram = x_gram$collocation,
                   freq = x_gram$count, freqPercent = x_gram$count / data_counts[x])
  return(ret)
  
}, x = 2:7, minCount = rep(2, 7), SIMPLIFY = FALSE)

# začátek prvního grafu ------------------------------------------------------

data_most_freq_ngrams <- map_dfr(1:nrow(data_coll[[1]]), function(x){
  
  thisName <- data_coll[[1]][x,2]
  startName <- data_coll[[1]][x,2]
  thisFreq <- data_coll[[1]][x,3]
  
  firstLine <- as.data.frame(list(1, thisName, data_coll[[1]][x,4]))
  names(firstLine) <- c("number", "word", "FreqPercent")
  isEmpty <- TRUE
  
  ret <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("number", "word", "FreqPercent"))
  
  for (num in 2:5) {
    filt <- data_coll[[num]] %>% filter(startsWith(ngram, paste0(thisName, " ")))
    filtInd <- which.max(filt$freq)
    ind <- filt[filtInd,]$order
    
    if (length(ind) != 0){
      y <- data_coll[[num]][ind,]
      
      toAdd <- as.data.frame(list(num, startName, y$freq / thisFreq))
      names(toAdd) <- c("number", "word", "FreqPercent")
      
      thisName <- y$ngram
      thisFreq <- y$freq
      
      ret <- rbind(ret, toAdd)
      isEmpty <- FALSE
    }
  }
  
  if (!isEmpty){
    ret <- rbind(ret, firstLine)
    return(ret)
  }
  
})



ggplot(data = data_most_freq_ngrams, aes(x = number, y = FreqPercent, line = word)) + 
  geom_point() + geom_line() + labs(title = "Nejvyšší frekvence n-gramů začínajících předešlým nejvyšším n-gramem",
                                    y = "relativní frekvence", x = "n (jak dlouhý n-gram)")
# poznámky k tomuto grafu:
# 
# vzal jsem všechna slova, na které začíná nějaký bigram a vypočítal jsem jejich relativní frekvenci (mám 1-gramy)
# pro každý 1-gram, n-gram ze začátku nastavím jako tento 1-gram:
#   našel jsem (n + 1)-gram s nejvyšší frekvencí začínající tímto n-gramem
#   vypočítal jsem relativní frekvenci posledního slova v (n + 1)-gramu za předpokladu, že už se vyskytla předešlá slova v tomto (n + 1)-gramu
#   předešlá dva kroky opakuji pro všechny n = 2:4, přičemž n-gram je (n + 1)-gram z předešlého loopu
# na grafu ukážu relativní frekvenci n-gramů a čarou spojím n-gramy korespondující s jedním slovem

# konec prvního grafu --------------------------------------------------------------------------

data_coll_counted <- lapply(data_coll, data.frame)


for (x in 1:4){
  for (index in 1:nrow(data_coll_counted[[x]])){
    prefix = paste0(data_coll_counted[[x]][index,]$ngram, " ")
    thisFreq = data_coll_counted[[x]][index,]$freq
    
    data_coll_counted[[x + 1]] <- data_coll_counted[[x + 1]] %>% mutate(freqPercent = 
                                              ifelse(
                                                startsWith(ngram, prefix), 
                                                freq / thisFreq, 
                                                freqPercent))
    
  }
}















