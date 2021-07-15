GenerateRadialGraph <- function(data_tree, fontSize = 10){
  library(networkD3)
  library(data.tree)
  useRtreeList <- ToListExplicit(data_tree, unname = TRUE)
  radialNetwork(useRtreeList, fontSize = fontSize)
}