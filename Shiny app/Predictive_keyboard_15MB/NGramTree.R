library(triebeard)
library(Rcpp)
Rcpp::sourceCpp("Sort.cpp")

setClass("NGramBase", slots = list(children = "list", trie = "externalptr", Highest = "integer"))

setClass("NGramNode", slots = list(freq = "numeric", name = "character"), contains = "NGramBase")

setClass("NGramRoot", slots = list(maxResult = "integer", joker = "character"), contains = "NGramBase")

noRecomendPrefix <- "\1D"

# Create(list ngramTables [1-gram, 2-gram, 3-gram, ...], string pathDelimeter, int maxResults)

CreateNGramTree <- function(ngramTables, maxResult, pathName, pathDelimeter = " ", freqPathName = "freqPercent", joker = "<>"){
  
  root <- CreateRoot(maxResult, joker)
  
  for(tab in ngramTables){
    
    for (index in 1:nrow(tab)) {
      
      path <- strsplit(tab[index,][[pathName]], pathDelimeter)[[1]]
      
      root@children[[path[1]]] <- FindOrCreate(root, path, tab[index,][[freqPathName]])
    }
  }
  
  root <- SetTrieAndHighestRecursive(root, maxResult, root@joker, length(ngramTables))
  
  return(root)
}

  # CreateRoot

CreateRoot <- function(maxResult, joker){
  
  new("NGramRoot", maxResult = as.integer(maxResult), children = list(), Highest = integer(0), joker = joker)
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


SetTrieAndHighest <- function(node, maxResult, joker){
  
  realChildren <- list()
  
  realChildrenNames <- character(0)
  
  realChildrenIndeces <- integer(0)
  
  
  if(length(node@children) > 0) {
    for(index in 1:length(node@children)){
      
      thisChildren <- node@children[[index]]
      
      if ((thisChildren@freq > 0) && (thisChildren@name != joker)){
        realChildren <- append(realChildren, thisChildren)
        realChildrenNames <- append(realChildrenNames, thisChildren@name)
        realChildrenIndeces <- append(realChildrenIndeces, index)
      } else{
        realChildrenNames <- append(realChildrenNames, paste0(noRecomendPrefix, thisChildren@name))
      }
    }
    node@trie <- trie(realChildrenNames, 1:length(node@children))
  }
  else
  {
    node@trie <- trie(character(0), integer(0))
  }
  
  node@Highest <- SortNGramTreeWithDict(realChildren, maxResult, realChildrenIndeces)
  
  
  return(node)
  
}

SetTrieAndHighestRecursive <- function(node, maxResult, joker, depth){
  
  node <- SetTrieAndHighest(node, maxResult, joker)
  
  if(depth != 1){
    if(length(node@children) > 0)
    for (index in 1:length(node@children)) {
      node@children[[index]] <- SetTrieAndHighestRecursive(node@children[[index]], maxResult, joker, depth - 1)
    }
  }
  
  return(node)
}

# GetBySeq

GetBySeq <- function(root, lastWords){
  
  if(length(lastWords) > root@maxResult) lastWords <- lastWords[(length(lastWords) - root@maxResult + 1):length(lastWords)]
  
  lastWords <- ChangeToJokers(root, lastWords)
  
  results <- unlist(lapply(1:length(lastWords), function(x){
    
    thisWords <- lastWords[x:length(lastWords)]
    
    thisNode <- root
    
    for(nextWord in thisWords){
      
      thisMatch <- longest_match(thisNode@trie, nextWord)
      
      if(is.na(thisMatch)){
        thisMatch <- longest_match(thisNode@trie, paste0(noRecomendPrefix, nextWord))
        if(is.na(thisMatch)) {return(list())}
      } 
      
      thisNode <- thisNode@children[[thisMatch]]
      
      if ((nextWord != thisNode@name) && (nextWord != paste0(noRecomendPrefix, thisNode@name))) return(list())
    }
    
    ret <- thisNode@children[thisNode@Highest]
    
    if ((length(ret) > 0) && (!is.null(ret[[1]]))) names(ret) <- sapply(ret, function(x) x@name)
    
    return(ret)
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
  
  if(length(lastWords) > root@maxResult) lastWords <- lastWords[(length(lastWords) - root@maxResult + 1):length(lastWords)]
  
  lastWords <- ChangeToJokers(root, lastWords)
  
  results <- unlist(lapply(1:length(lastWords), function(x){
    
    thisWords <- lastWords[x:length(lastWords)]
    
    thisNode <- root
    
    for(nextWord in thisWords){
      
      thisMatch <- longest_match(thisNode@trie, nextWord)
      
      if(is.na(thisMatch)){
        thisMatch <- longest_match(thisNode@trie, paste0(noRecomendPrefix, nextWord))
        if(is.na(thisMatch)) {return(list())}
      }
      
      thisNode <- thisNode@children[[thisMatch]]
      
      if ((nextWord != thisNode@name) && (nextWord != paste0(noRecomendPrefix, thisNode@name))) return(list())
    }
    
    ret <- thisNode@children[prefix_match(thisNode@trie, newPart)[[1]]]
    
    if ((length(ret) > 0) && (!is.null(ret[[1]]))) names(ret) <- sapply(ret, function(x) x@name)
    
    return(ret)
  }))
  
  for (name in unique(names(results))){
    
    withThisName <- results[names(results) == name]
    
    toReset <- withThisName[[which.max(sapply(withThisName, function(x) x@freq))]]
    
    results <- results[names(results) != name]
    
    results[[name]] <- toReset
  }
  
  ret <- sapply(SortNGramTree(results, root@maxResult), function(x) x@name)
  
  # if it is too short, extend it from roots trie
  if(length(ret) < root@maxResult){
    
    toSort <- unlist(root@children[prefix_match(root@trie, newPart)[[1]]])
    
    ext <- sapply(SortNGramTree(toSort, root@maxResult), function(x) x@name)
    
    ret <- unique(c(ret, ext))
    
    if(length(ret) > root@maxResult) ret <- ret[1:root@maxResult]
  }
  
  return(ret)
}


# GetByPart(string newPart)

GetByPart <- function(root, newPart){
  
  toSort <- unlist(root@children[prefix_match(root@trie, newPart)[[1]]])
  
  return(sapply(SortNGramTree(toSort, root@maxResult), function(x) x@name))
}

# GetByNothing()

GetByNothing <- function(root){
  
  toSort <- root@children[root@Highest]
  
  return(sapply(unname(toSort), function(x) x@name))
}

ChangeToJokers <- function(root, wordsToChange){
  
  return(sapply(wordsToChange, function(x) ifelse(is.na(prefix_match(root@trie, x)[[1]][1]), 
                                                  paste0(noRecomendPrefix , root@joker), x), USE.NAMES = FALSE))
}


CreateMeta <- function(root, levs){
  
  data_ret <- list(setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("maxRes", "joker", "start", "all", "real")))
  
  data_ret[2:levs] <- lapply(2:levs, function(x){
    
    return(setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("word", "freq", "start", "all", "real")))
  })
  
  data_ret[[levs+1]] <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("word", "freq"))
  
  pointers <- rep(1, levs + 1)
  
  maxRes <- root@maxResult
  
  maxlvl <- levs + 1
    
    
  
  data_ret[[1]][1,] <- c(root@maxResult, root@joker, 1, length(root@children), length(root@trie))
  
  pointers[1] = pointers[1] + 1
  
  mask <- logical(length(root@children))
  
  for (index in root@Highest){
    
    mask[index] <- TRUE
    
    ret <- WriteMeToMeta(root@children[index][[1]], 2, data_ret, pointers, maxRes, maxlvl)
    data_ret <- ret[[1]]
    pointers <- ret[[2]]
  }
  
  for (index in get_values(root@trie)){
    
    if (!mask[index]){
      
      mask[index] <- TRUE
      
      ret <- WriteMeToMeta(root@children[index][[1]], 2, data_ret, pointers, maxRes, maxlvl)
      data_ret <- ret[[1]]
      pointers <- ret[[2]]
    }
  }
  
  for (index in 1:length(root@children)){
    
    if (!mask[index]){
      
      ret <- WriteMeToMeta(root@children[index][[1]], 2, data_ret, pointers, maxRes, maxlvl)
      data_ret <- ret[[1]]
      pointers <- ret[[2]]
    }
    
  }
  
  return(data_ret)
  
}

WriteMeToMeta <- function(obj, lvl, data_ret, pointers, maxRes, maxlvl){
  
  if (lvl == maxlvl){
    
    data_ret[[lvl]][pointers[lvl],] <- c(obj@name, obj@freq)
    
    pointers[lvl] = pointers[lvl] + 1
    
    return(list(data_ret, pointers))
  }
  
  data_ret[[lvl]][pointers[lvl],] <- c(obj@name, obj@freq, pointers[lvl + 1], length(obj@children), length(obj@trie))
  
  pointers[lvl] = pointers[lvl] + 1
  
  mask <- logical(length(obj@children))
  
  for (index in obj@Highest){
    
    mask[index] <- TRUE
    
    ret <- WriteMeToMeta(obj@children[index][[1]], lvl+1, data_ret, pointers, maxRes, maxlvl)
    data_ret <- ret[[1]]
    pointers <- ret[[2]]
  }
  
  for (index in get_values(obj@trie)){
    
    if (!mask[index]){
      
      mask[index] <- TRUE
      
      ret <- WriteMeToMeta(obj@children[index][[1]], lvl+1, data_ret, pointers, maxRes, maxlvl)
      data_ret <- ret[[1]]
      pointers <- ret[[2]]
    }
  }
  
  if (length(obj@children) > 0)
  for (index in 1:length(obj@children)){
    
    if (!mask[index]){
      
      ret <- WriteMeToMeta(obj@children[index][[1]], lvl+1, data_ret, pointers, maxRes, maxlvl)
      data_ret <- ret[[1]]
      pointers <- ret[[2]]
    }
    
  }
  
  return(list(data_ret, pointers))
}


LoadFromMeta <- function(input, levs){
  
  maxRes <- as.integer(input[[1]]$maxRes[1])
  
  maxlvl <- levs + 1
  
  pointer <- as.integer(input[[1]]$start[1])
  
  real <- as.integer(input[[1]]$real[1])
  
  all <- as.integer(input[[1]]$all[1])
  
  if (real > 0){
    
    root <- new("NGramRoot", maxResult = maxRes, children = list(),
                Highest = 1:min(maxRes, real), joker = input[[1]]$joker[1])
    
    for (n in 1:real){
      
      loaded <- LoadMe(pointer, input, 2, maxlvl, maxRes)
      
      root@children <-  append(root@children, loaded)
      pointer = pointer + 1
    }
    
    toTrie <- sapply(1:real, function(x){
      (root@children[x][[1]])@name
    })
    
  }
  else{
    
    root <- new("NGramRoot", maxResult = maxRes, children = list(),
                Highest = integer(0), joker = input[[1]]$joker[1])
    toTrie <- character(0)
  }
  
  if ((all - real) > 0){
    
    for (n in 1:(all - real)){
      
      loaded <- LoadMe(pointer, input, 2, maxlvl, maxRes)
      
      root@children <-  append(root@children, loaded)
      pointer = pointer + 1
    }
    toTrie <- append(toTrie, sapply((real+1):all, function(x){
      paste0(noRecomendPrefix, (root@children[x][[1]])@name)}))
  }
  
  if (all > 0) root@trie <- trie(toTrie, 1:all)
  else root@trie <- trie(toTrie, integer(0))
  
  return(root)
  
}


LoadMe <- function(index, input, lvl, maxlvl, maxRes){
  
  if (lvl == maxlvl){
    
    ret <- new("NGramNode", freq = as.numeric(input[[lvl]]$freq[index]), name = input[[lvl]]$word[index],
               children = list(), Highest = integer(0), trie = trie(character(0), integer(0)))
    
    return(ret)
  }
  
  pointer <-  as.integer(input[[lvl]]$start[index])
  
  real <- as.integer(input[[lvl]]$real[index])
    
  all <- as.integer(input[[lvl]]$all[index])
  
  if (real > 0){
    
  ret <- new("NGramNode", freq = as.numeric(input[[lvl]]$freq[index]), name = input[[lvl]]$word[index],
             children = list(), Highest = 1:min(maxRes, real))
  
  for (n in 1:real){
    loaded <- LoadMe(pointer, input, lvl+1, maxlvl, maxRes)
    
    ret@children <-  append(ret@children, loaded)
    pointer = pointer + 1
  }
  
  toTrie <- sapply(1:real, function(x){
    (ret@children[x][[1]])@name
    })
  
  }
  else{
    ret <- new("NGramNode", freq = as.numeric(input[[lvl]]$freq[index]), name = input[[lvl]]$word[index],
               children = list(), Highest = integer(0))
    toTrie <- character(0)
  }
  
  if ((all - real) > 0){
    
    for (n in 1:(all - real)){
      loaded <- LoadMe(pointer, input, lvl+1, maxlvl, maxRes)
      
      ret@children <-  append(ret@children, loaded)
      pointer = pointer + 1
    }
    
    toTrie <- append(toTrie, sapply((real+1):all, function(x){
      paste0(noRecomendPrefix, (ret@children[x][[1]])@name)
    }))
  }
  
  if (all > 0) ret@trie <- trie(toTrie, 1:all)
  else ret@trie <- trie(toTrie, integer(0))
  
  return(ret)
}







