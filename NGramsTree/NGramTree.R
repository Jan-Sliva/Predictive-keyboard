
library(triebeard)

setClass("NGramBase", slots = list(children = "list", trie = "externalptr", Highest = "integer"))

setClass("NGramNode", slots = list(freq = "numeric", name = "character"), contains = "NGramBase")

setClass("NGramRoot", slots = list(maxResult = "integer"), contains = "NGramBase")


# Create(list ngramTables [1-gram, 2-gram, 3-gram, ...], string pathDelimeter, int maxResults)

CreateNGramTree <- function(ngramTables, maxResult, pathName, pathDelimeter = " ", freqPathName = "freq"){
  
  root <- CreateRoot(maxResult)
  
  for(tab in ngramTables){
    
    for (index in 1:nrow(tab)) {
      
      path <- strsplit(tab[index,][[pathName]], pathDelimeter)[[1]]
      
      root@children[[path[1]]] <- FindOrCreate(root, path, tab[index,][[freqPathName]])
    }
  }
  
  root <- SetTrieAndHighestRecursive(root, maxResult, length(ngramTables))
  
  return(root)
}

  # CreateRoot

CreateRoot <- function(maxResult){
  
  new("NGramRoot", maxResult = as.integer(maxResult), children = list(), Highest = integer(0))
}

  # CreateNodes(names, freq) -> Node

CreateNodes <-  function(node, path, freq) {
  
  if(length(path) > 1){
    
    newNode <- new("NGramNode", freq = -1, name = path[1], children = list(), Highest = integer(0))
    
    newNode@children[[path[2]]] <-  CreateNodes(newNode, path[-1], freq)
    
    return(newNode)
  }
  
  newNode <- new("NGramNode", freq = freq, name = path[1], children = list(), Highest = integer(0))
  
  return(newNode)
  
}
# setMethod("CreateNodes", signature("NGramBase", "character", "integer"), CreateNodes)


  # FindOrCreate(path, freq)

FindOrCreate <- function(node, path, freq) {
  
  nextNode <- node@children[[path[1]]]
  
  if(is.null(nextNode)){
    # Create node(s)
    return(CreateNodes(node, path, freq))
  }
  else if(length(path) == 1){
    # reset freq
    nextNode@freq <-  freq
    return(nextNode)
  }
  else{
    # continue
    nextNode@children[[path[2]]] <- FindOrCreate(nextNode, path[-1], freq)
    return(nextNode)
  }
  
  
}
# setMethod("FindOrCreate", signature("NGramBase", "character", "integer"), FindOrCreate)


SetTrieAndHighest <- function(node, maxResult){
  
  realChildren <- list()
  
  realChildrenNames <- character(0)
  
  realChildrenIndeces <- integer(0)
  
  if(length(node@children) > 0)
  for(index in 1:length(node@children)){
    
    thisChildren <- node@children[[index]]
    
    if (thisChildren@freq > 0){
      realChildren <- append(realChildren, thisChildren)
      realChildrenNames <- append(realChildrenNames, thisChildren@name)
      realChildrenIndeces <- append(realChildrenIndeces, index)
    }
  }
  
  node@Highest <- SortNGramTreeWithDict(realChildren, maxResult, realChildrenIndeces)
  
  node@trie <- trie(realChildrenNames, realChildrenIndeces)
  
  return(node)
  
}

SetTrieAndHighestRecursive <- function(node, maxResult, depth){
  
  node <- SetTrieAndHighest(node, maxResult)
  
  if(depth != 1){
    if(length(node@children) > 0)
    for (index in 1:length(node@children)) {
      node@children[[index]] <- SetTrieAndHighestRecursive(node@children[[index]], maxResult, depth - 1)
    }
  }
  
  return(node)
}

# GetBySeq

GetBySeq <- function(root, lastWords){
  
  if(length(lastWords) > root@maxResult) lastWords <- lastWords[(length(lastWords) - root@maxResult + 1):length(lastWords)]
  
  results <- unlist(lapply(1:length(lastWords), function(x){
    
    thisWords <- lastWords[x:length(lastWords)]
    
    thisNode <- root
    
    for(nextWord in thisWords){
      
      thisNode <- thisNode@children[[nextWord]]
      
      if(is.null(thisNode)){
        return(list())
      }
    }
    
    return(thisNode@children[thisNode@Highest])
  }))
  
  for (name in unique(names(results))){
    
    withThisName <- results[names(results) == name]
    
    toReset <- withThisName[[which.max(sapply(withThisName, function(x) x@freq))]]
  
    results <- results[names(results) != name]
    
    results[[name]] <- toReset
  }
  
  return(sapply(SortNGramTree(results, root@maxResult), function(x) x@name))
}

# GetBySeqAndPart(list<string> lastwords, string newPart)

GetBySeqAndPart <- function(root, lastWords, newPart){
  
  if((!is.character(newPart)) || newPart == "") return(GetBySeq(root, lastWords))
  
  if(length(lastWords) > root@maxResult) lastWords <- lastWords[(length(lastWords) - root@maxResult + 1):length(lastWords)]
  
  results <- unlist(lapply(1:length(lastWords), function(x){
    
    thisWords <- lastWords[x:length(lastWords)]
    
    thisNode <- root
    
    for(nextWord in thisWords){
      
      thisNode <- thisNode@children[[nextWord]]
      
      if(is.null(thisNode)){
        return(list())
      }
    }
    
    return(thisNode@children[prefix_match(thisNode@trie, newPart)[[1]]])
  }))
  
  for (name in unique(names(results))){
    
    withThisName <- results[names(results) == name]
    
    toReset <- withThisName[[which.max(sapply(withThisName, function(x) x@freq))]]
    
    results <- results[names(results) != name]
    
    results[[name]] <- toReset
  }
  
  return(sapply(SortNGramTree(results, root@maxResult), function(x) x@name))
}


# GetByPart(string newPart)

GetByPart <- function(root, newPart){
  
  if((!is.character(newPart)) || newPart == "") return(GetByNothing(root))
  
  toSort <- root@children[prefix_match(root@trie, newPart)[[1]]]
  
  return(sapply(SortNGramTree(toSort, root@maxResult), function(x) x@name))
}

# GetByNothing()

GetByNothing <- function(root){
  
  toSort <- root@children[root@Highest]
  
  return(sapply(SortNGramTree(toSort, root@maxResult), function(x) x@name))
}



# tse <- CreateNGramTree(list(data_coll_counted[[1]][1:100,], data_coll_counted[[2]][1:100,], data_coll_counted[[3]][1:100,], data_coll_counted[[4]][1:100,], data_coll_counted[[5]][1:100,]), 10, "ngram", " ", "freqPercent")













