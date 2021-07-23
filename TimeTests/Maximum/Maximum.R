library(rbenchmark)
library(purrr)


GetMax <- function(vec){
  
  maxNum <- -.Machine$integer.max
  
  for (num in vec){
    if (num > maxNum) maxNum <- num
  }
  
  return(maxNum)
}

GetMaxVapply <- function(splitted){
  res <- vapply(splitted, GetMax, c(0))
  return(GetMax(res))
}

GetMaxMap <- function(splitted){
  res <- map(splitted, GetMax)
  return(GetMax(res))
}