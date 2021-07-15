
createListNodes <- function(count, seed){
  
  set.seed(seed)
  
  sapply(1:count, function(x){
    x <- list(freq = floor(runif(1, 1, 1001)))
    return(x)
  }) 
}

GetTop5List_TopXSort <- function(ListNodes){
  
  TopXSort(ListNodes, function(x) x[1])
}

GetTop5List_Order <- function(listNodes){
  
  listNodes[order(sapply(listNodes, "[[", 1))]
}

createS3Nodes <- function(count, seed){
  
  set.seed(seed)
  
  sapply(1:count, function(x){
    x <- list(freq = floor(runif(1, 1, 1001)))
    attr(x, "class") <- "Node"
    class(x) <- "Node"
    return(x)
  }) 
}

GetTop5S3_TopXSort <- function(S3Nodes){
  
  TopXSort(S3Nodes, function(x) x[1])
}

GetTop5S3_Order <- function(S3Nodes){
  
  S3Nodes[order(sapply(S3Nodes, "[[", 1))]
}


CreateS4Nodes <- function(count, seed){
  
  set.seed(seed)
  setClass("Node", representation(freq = "integer"))
  
  sapply(1:count, function(x){
    x <- new("Node", freq = as.integer(floor(runif(1, 1, 1001))))
    return(x)
  }) 
}

GetTop5S4_TopXSort <- function(S4Nodes){
  
  TopXSort(S4Nodes, function(x) x@freq)
}

GetTop5S4_Order <- function(S4Nodes){
  
  S4Nodes[order(sapply(S4Nodes, function(x) x@freq))]
}

TopXSort <- function(toSort, parameterFunc, x = 5){
  
  topXList <- list()
  
  for (i in 2:(x + 1)) topXList[i] <- NULL
  
  for (item in toSort){
    value <-  parameterFunc(item)
    
    if ( is.null(topXList[[x]]) || (value > parameterFunc(topXList[[x]])))
      
      for (i in (x-1):0){
        if( (i == 0) || ( !is.null(topXList[[i]]) && (value <= parameterFunc(topXList[[i]]) ) ) ){
          if (i < (x-1))
            for (e in x:(i + 2)) {
              if (!is.null(topXList[[e-1]])) topXList[[e]] <-  topXList[[e-1]]
            }
          topXList[[i+1]] <-  item
          break
        }
      }
  }
  return(topXList)
}
  
  
  
  
  
  
  
  
  
  
  







