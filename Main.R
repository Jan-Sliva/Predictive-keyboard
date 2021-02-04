library(readr)
library(tidyverse)
library(quanteda)
library(data.table)

text_twitter_test <- tolower(read_lines("E:/honzi/Documents/final/en_US/test2_twitter.txt"))
text_news_test <- tolower(read_lines("E:/honzi/Documents/final/en_US/test2_news.txt"))
text_blogs_test <- tolower(read_lines("E:/honzi/Documents/final/en_US/test2_blogs.txt"))

vyraz1 <- c("(?<!e)-(?!mail)")
vyraz2 <- c("[^a-zA-Z'’ \\.\\-]")
clear_twitter <- gsub(vyraz2, "", gsub(vyraz1, " ", text_twitter_test, perl = TRUE))
clear_news <- gsub(vyraz2, "", gsub(vyraz1, " ", text_news_test, perl = TRUE))
clear_blogs <- gsub(vyraz2, "", gsub(vyraz1, " ", text_blogs_test, perl = TRUE))

vyraz3 <- c("((?<! [mM]iss)(?<! (U\\.S|[mM]rs|etc|[Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ep|[Oo]ct|[Nn]ov|[Dd]ec))(?<! ([mM][sr]|[Nn]o))[\\.\\?\\!] )|\\.{2,}")
sentences_twitter <- unlist(strsplit(gsub("\\.+$", "", clear_twitter), vyraz3, perl = TRUE))
sentences_news <- unlist(strsplit(gsub("\\.$", "", clear_news), vyraz3, perl = TRUE))
sentences_blogs <- unlist(strsplit(gsub("\\.$", "", clear_blogs), vyraz3, perl = TRUE))

vyraz4 <- c("((?<=[[:alpha:]])(?='(s|d|m) ))|((?<=[[:alpha:]])(?='(re|ll|ve) ))|((?<=s)(?=' ))| +")
split_text_twitter <- strsplit(gsub("’", "'", sentences_twitter), vyraz4, perl = TRUE)
split_text_news <- strsplit(gsub("’", "'", sentences_news), vyraz4, perl = TRUE)
split_text_blogs <- strsplit(gsub("’", "'", sentences_blogs), vyraz4, perl = TRUE)

filterEmptyStrings <-  function(x){x[x != "" & !grepl("\\.+", x)]}

result_twitter <- lapply(split_text_twitter, filterEmptyStrings)
result_twitter <- result_twitter[lapply(result_twitter, length) > 1]

result_news <- lapply(split_text_news, filterEmptyStrings)
result_news <- result_news[lapply(result_news, length) > 1]

result_blogs <- lapply(split_text_blogs, filterEmptyStrings)
result_blogs <- result_blogs[lapply(result_blogs, length) > 1]

results_test <- append(append(result_twitter, result_news), result_blogs)

sorted_slova <- frekvence_slov[order(frekvence_slov$Freq, decreasing = TRUE),]

hranice <-  3
potreba_zmenit_za_token <-  dplyr::filter(sorted_slova, Freq <= hranice)$unlisted_result
potreba_zmenit_za_token <- as.character(levels(potreba_zmenit_za_token))[potreba_zmenit_za_token]
hashed_character <- hash(potreba_zmenit_za_token, NA)

# jako token budu používat "<>"

zmenitNaToken <- function(x){
  if (has.key(x, hashed_character))
    return("<>")
  else
    return(x)
}

result_tokeny <- lapply(results_test, function(x){unlist(vapply(x, zmenitNaToken, character(1)))})

x <- tokens(result_tokeny, what = "character")

bigramy <- textstat_collocations(x, min_count = 5, size = 2)

app_bigramy <- data.table(str_extract(bigramy$collocation, regex("^[[:alpha:][:punct:]<>]+(?= )")), 
                          str_extract(bigramy$collocation, regex("(?<= )[[:alpha:][:punct:]<>]+$")), bigramy$count)

setnames(app_bigramy, c("V1", "V2", "V3"), c("PrvniSlovo", "DruheSlovo", "Freq"))

app_bigramy <- app_bigramy[DruheSlovo != "<>"]

setorder(app_bigramy, -Freq)
