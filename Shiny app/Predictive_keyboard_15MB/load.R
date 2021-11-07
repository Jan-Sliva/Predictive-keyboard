source("NGramTree.R")

data_meta <- lapply(1:6, function(x){
  
  return(read.csv(paste0(x, ".csv")))
})


data_tree <- LoadFromMeta(data_meta, 5)

