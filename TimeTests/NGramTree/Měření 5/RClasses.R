
createListNodes <- function(count, seed, maxValue){
  
  set.seed(seed)
  
  ret <- lapply(1:count, function(x){
    x <- list(freq = floor(runif(1, 1, maxValue + 1)))
    return(x)
  })
  
  return(ret)
}

GetTop5List_TopXSort <- function(ListNodes){
  
  TopXSort(ListNodes, function(x) x$freq, 5)
}

GetTop5List_Order <- function(listNodes){
  
  listNodes[order(sapply(listNodes, function(x) x$freq), decreasing = TRUE)][1:5]
}

createS3Nodes <- function(count, seed, maxValue){
  
  set.seed(seed)
  
  ret <- lapply(1:count, function(x){
    x <- list(freq = floor(runif(1, 1, maxValue + 1)))
    class(x) <- "S3Node"
    return(x)
  }) 
  
  return(ret)
}

GetTop5S3_TopXSort <- function(S3Nodes){
  
  TopXSort(S3Nodes, function(x) x$freq, 5)
}

GetTop5S3_Order <- function(S3Nodes){
  
  S3Nodes[order(sapply(S3Nodes, function(x) x$freq), decreasing = TRUE)][1:5]
}


CreateS4Nodes <- function(count, seed, maxValue){
  
  set.seed(seed)
  setClass("S4Node", representation(freq = "integer"))
  
  sapply(1:count, function(x){
    x <- new("S4Node", freq = as.integer(floor(runif(1, 1, maxValue + 1))))
    return(x)
  }) 
}

GetTop5S4_TopXSort <- function(S4Nodes){
  
  TopXSort(S4Nodes, function(x) x@freq, 5)
}

GetTop5S4_Order <- function(S4Nodes){
  
  S4Nodes[order(sapply(S4Nodes, function(x) x@freq), decreasing = TRUE)][1:5]
}

TopXSort <- function(toSort, parameterFunc, x = 5){
  
  topXListIndeces <- list()
  end <- 0
  
  values <- sapply(toSort, parameterFunc)
  
  for (index in 1:length(toSort)){
    
    if ( (end < x) || (values[index] > values[topXListIndeces[[x]]]))
      
      for (i in (x-1):0){
        if( (i == 0) || ( (end >= i)  && (values[index] <= values[topXListIndeces[[i]]] ) ) ){
          if (min(x, end + 1) >= (i + 2))
            for (e in min(x, end + 1):(i + 2)) {
              topXListIndeces[[e]] <-  topXListIndeces[[e-1]]
            }
          topXListIndeces[[i+1]] <-  index
          if (end < x) end = end + 1
          break
        }
      }
  }
  topXList <- sapply(topXListIndeces, function(index) toSort[index])
  return(topXList)
}

  
  
  
  
  
  
  
  
  
  







