source("NGramTree.R")
library(readr)

data_meta <- lapply(1:6, function(x){
  return(read_csv(paste0(x, "v2.csv")))
}


data_tree <- LoadFromMeta(data_meta, 5)