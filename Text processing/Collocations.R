library(quanteda.textstats)
library(quanteda)

data_coll[2:7] <-  mapply(function(x, minCount){
  
  x_gram <- textstat_collocations(filtered_words, size = x, min_count = minCount)
  ret = data.frame(order = 1:nrow(x_gram), ngram = x_gram$collocation,
                   freq = x_gram$count, freqPercent = x_gram$count / data_counts[x] * 100)
  return(ret)
  
}, x = 2:7, minCount = rep(2, 7), SIMPLIFY = FALSE)