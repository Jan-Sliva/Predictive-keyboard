
NumberOfWords <- function(str){
  sapply(gregexpr("[[:alpha:]]+", str), function(x) sum(x > 0))
}

ReplaceWithToken <- function(splittedSentences, minFrequency = 2, token = "<>"){
  
  frekvence_slov <- as.data.frame(table(splittedSentences))
  
  # udělám tabulku se seřazenými výskyty slova
  
  sorted_slova <- frekvence_slov[order(frekvence_slov$Freq, decreasing = TRUE),]
  
  # Teď udělám Hešovaný list, kde budou slova, která změním za token (žolíka, který bude nahrazovat slova s malým výskytem)
  # jako token budu používat "<>"
  
  potreba_zmenit_za_token <-  dplyr::filter(sorted_slova, Freq <= hranice)$unlisted_result
  potreba_zmenit_za_token <- as.character(levels(potreba_zmenit_za_token))[potreba_zmenit_za_token]
  hashed_character <- hash(potreba_zmenit_za_token, NA) # toto jsou slova, která chci nahradit za token
  
  # tato funkce vezme list a nahradí slova, která jsou zároveň i v hashed_character za token
  
  zmenitNaToken <- function(x){
    if (has.key(x, hashed_character))
      return("<>")
    else
      return(x)
  }
  
  # aktivuji tuto funkci na všechny věty
  
  result_tokeny <- lapply(results_test, function(x){unlist(vapply(x, zmenitNaToken, character(1)))})
  
  return(result_tokeny)
  
}

GetChildrenByStartingString <- function(node, startingString){
  
  strings <- unlist(prefix_match(node$trie, startingString))
  return(sapply(strings, function(x){node$Climb(name = x)}))
}


GetNodesByInput <- function(nodeList, words, predictedPart = "", count = 5){
  
  
  if (length(words) >= 1) fromOneWord <- c(GetNodeFromOneWord(nodeList, words))
  else return(NULL)
  
  if (length(words) >= 2){
    fromMoreWords <- sapply(2:length(words), 
                            function(count) GetNodeFromNWords(nodeList, words, count))
    AllNodes <- unlist(c(fromOneWord, fromMoreWords))
  } 
  else AllNodes = fromOneWord
  
  if (is.null(AllNodes)) return(NULL)
  
  
  if ((predictedPart == "") || !is.character(predictedPart)){
    
    AllResults <- unlist(sapply(AllNodes, function(node) {node$children}))
  }
  else{
    AllResults <- unlist(sapply(AllNodes, function(node) 
      {if (!node$isLeaf) GetChildrenByStartingString(node, predictedPart)}))
  }
  
  frequencies <- unlist(sapply(AllResults, function(x){x$freq}))
  
  names(frequencies) <- unlist(sapply(AllResults, function(x){x$name}))
  
  sortedNames <- names(sort(frequencies, decreasing = TRUE))
  
  return(head(unique(sortedNames), count))
}

GetNodeFromOneWord <- function(nodeList, words){
  
  nodeList[[words[length(words)]]]
}


GetNodeFromNWords <- function(nodeList, words, n){
  
  x = nodeList[[words[length(words) - n + 1]]]
  
  if (!is.null(x)) return(x$Climb(name = words[(length(words) - n + 2):length(words)]))
  else return(NULL)
}









