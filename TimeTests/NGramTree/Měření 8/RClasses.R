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

GetTop5List_TopXSortRcpp <- function(ListNodes){
  
  TopXSortCppList(ListNodes, 5)
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

GetTop5S3_TopXSortRcpp <- function(S3Nodes){
  
  TopXSortCppS3(S3Nodes, 5)
}

GetTop5S3_Order <- function(S3Nodes){
  
  S3Nodes[order(sapply(S3Nodes, function(x) x$freq), decreasing = TRUE)][1:5]
}


CreateS4Nodes <- function(count, seed, maxValue){
  
  set.seed(seed)
  setClass("S4Node", representation(freq = "integer"))
  
  lapply(1:count, function(x){
    x <- new("S4Node", freq = as.integer(floor(runif(1, 1, maxValue + 1))))
    return(x)
  }) 
}

GetTop5S4_TopXSort <- function(S4Nodes){
  
  TopXSort(S4Nodes, function(x) x@freq, 5)
}

GetTop5S4_TopXSortRcpp <- function(S4Nodes){
  
  TopXSortCppS4(S4Nodes, 5)
}

GetTop5S4_Order <- function(S4Nodes){
  
  S4Nodes[order(sapply(S4Nodes, function(x) x@freq), decreasing = TRUE)][1:5]
}


CreateR5Nodes <- function(count, seed, maxValue){
  
  set.seed(seed)
  R5Node <- setRefClass("R5Node", fields = list(freq = "integer"))
  
  lapply(1:count, function(x){
    x <- R5Node$new(freq = as.integer(floor(runif(1, 1, maxValue + 1))))
    return(x)
  }) 
}

GetTop5R5_TopXSort <- function(R5Nodes){
  
  TopXSort(R5Nodes, function(x) x$freq, 5)
}

GetTop5R5_Order <- function(R5Nodes){
  
  R5Nodes[order(sapply(R5Nodes, function(x) x$freq), decreasing = TRUE)][1:5]
}

TopXSort <- function(toSort, parameterFunc, x = 5){
  
  topXList <- lapply(1:x, function(x) NA)
  topXListValues <- rep(-.Machine$integer.max, x)
  
  for (item in toSort){
    value <-  parameterFunc(item)
    
    if (value > topXListValues[[x]])
      
      for (i in (x-1):0){
        if( (i == 0) || (value <= topXListValues[[i]] ) ){
          if (x >= (i + 2))
            for (e in x:(i + 2)) {
              topXList[[e]] <-  topXList[[e-1]]
              topXListValues[[e]] <-  topXListValues[[e-1]]
            }
          topXList[[i+1]] <-  item
          topXListValues[[i+1]] <- value
          break
        }
      }
  }
  return(topXList)
}








