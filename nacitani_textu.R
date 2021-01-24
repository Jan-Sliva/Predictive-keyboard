<<<<<<< HEAD
library(readr)

text_twitter <- read_lines("E:/honzi/Documents/final/en_US/test_twitter.txt")
text_news <- read_lines("E:/honzi/Documents/final/en_US/test_news.txt")
text_blogs <- read_lines("E:/honzi/Documents/final/en_US/test_blogs.txt")

vyraz1 <- c("(?<!e)-(?!mail)")
vyraz2 <- c("[^a-zA-Z'’ \\.\\-]")
clear_twitter <- gsub(vyraz2, "", gsub(vyraz1, " ", text_twitter, perl = TRUE))
clear_news <- gsub(vyraz2, "", gsub(vyraz1, " ", text_news, perl = TRUE))
clear_blogs <- gsub(vyraz2, "", gsub(vyraz1, " ", text_blogs, perl = TRUE))

vyraz3 <- c("((?<! [mM]iss)(?<! (U\\.S|[mM]rs|etc|[Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ep|[Oo]ct|[Nn]ov|[Dd]ec))(?<! ([mM][sr]|[Nn]o))[\\.\\?\\!] )|\\.{2,}")
sentences_twitter <- unlist(strsplit(gsub("\\.+$", "", clear_twitter), vyraz3, perl = TRUE))
sentences_news <- unlist(strsplit(gsub("\\.$", "", clear_news), vyraz3, perl = TRUE))
sentences_blogs <- unlist(strsplit(gsub("\\.$", "", clear_blogs), vyraz3, perl = TRUE))

vyraz4 <- c(" +")
split_text_twitter <- strsplit(sentences_twitter, vyraz4)
split_text_news <- strsplit(sentences_news, vyraz4)
split_text_blogs <- strsplit(sentences_blogs, vyraz4)

filterEmptyStrings <-  function(x){x[x != "" & !grepl("\\.+", x)]}
vyraz_apostrophes = c("((?<=[[:alpha:]])['’](?=((s|re|ll|d|ve|m)$)))|((?<=s)['’]$)")
splitApostrophes <- function(x){strsplit(gsub(vyraz_apostrophes, " '", x, perl = TRUE), " (?=')", perl = TRUE)}
lapplySplitApostrophes <- function(x){unlist(lapply(x, splitApostrophes))}

result_twitter <- lapply(split_text_twitter, filterEmptyStrings)
result_twitter <- result_twitter[lapply(result_twitter, length) > 1]

result_news <- lapply(split_text_news, filterEmptyStrings)
result_news <- result_news[lapply(result_news, length) > 1]

result_blogs <- lapply(split_text_blogs, filterEmptyStrings)
result_blogs <- result_blogs[lapply(result_blogs, length) > 1]

resultsNoApostrophes <- append(append(result_twitter, result_news), result_blogs)

results <- lapply(resultsNoApostrophes, lapplySplitApostrophes)
=======
library(readr)

text_twitter <- read_lines("E:/honzi/Documents/final/en_US/test_twitter.txt")
text_news <- read_lines("E:/honzi/Documents/final/en_US/test_news.txt")
text_blogs <- read_lines("E:/honzi/Documents/final/en_US/test_blogs.txt")

vyraz1 <- c("(?<!e)-(?!mail)")
vyraz2 <- c("[^a-zA-Z' \\.\\-]")
clear_twitter <- gsub(vyraz2, "", gsub(vyraz1, " ", text_twitter, perl = TRUE))
clear_news <- gsub(vyraz2, "", gsub(vyraz1, " ", text_news, perl = TRUE))
clear_blogs <- gsub(vyraz2, "", gsub(vyraz1, " ", text_blogs, perl = TRUE))

vyraz3 <- c("((?<! [mM]iss)(?<! (U\\.S|[mM]rs|etc|[Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ep|[Oo]ct|[Nn]ov|[Dd]ec))(?<! ([mM][sr]|[Nn]o))[\\.\\?\\!] )|\\.{2,}")
sentences_twitter <- unlist(strsplit(gsub("\\.+$", "", clear_twitter), vyraz3, perl = TRUE))
sentences_news <- unlist(strsplit(gsub("\\.$", "", clear_news), vyraz3, perl = TRUE))
sentences_blogs <- unlist(strsplit(gsub("\\.$", "", clear_blogs), vyraz3, perl = TRUE))

vyraz4 <- c("[ ]+")
split_text_twitter <- strsplit(sentences_twitter, vyraz4)
split_text_news <- strsplit(sentences_news, vyraz4)
split_text_blogs <- strsplit(sentences_blogs, vyraz4)

filterEmptyStrings <-  function(x){x[x != "" & !grepl("\\.+", x)]}

result_twitter <- lapply(split_text_twitter, filterEmptyStrings)
result_twitter <- result_twitter[lapply(result_twitter, length) > 1]

result_news <- lapply(split_text_news, filterEmptyStrings)
result_news <- result_news[lapply(result_news, length) > 1]

result_blogs <- lapply(split_text_blogs, filterEmptyStrings)
result_blogs <- result_blogs[lapply(result_blogs, length) > 1]


>>>>>>> 0f492dacbf29825c4826da4f928fcd6ff2b43809
