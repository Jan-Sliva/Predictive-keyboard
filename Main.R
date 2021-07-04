# tento skript načte text a udělá z něj tabulku vhodnou pro vyhledávání v ní

# načítání knihoven
library(readr) # načítání textu ze souboru
library(stringr) # zpracování textu
library(dplyr) # zpracování tabulky
library(quanteda) # analýza počtu bigramů
library(quanteda.textstats)
library(data.table) # knihovna s tabulkou, která používá hešování
library(hash) # hešování

# načítání textů ze souborů
# budu pracovat se třemi typy textů - z twitteru, ze zpráv a z blogů
# pro každý tento typ budu aktivovat dané funkce jednotlivě, 
# což mi umožní v budoucnosti využívat jiný regex pro každý typ souboru (např. pro twitter, kde se často používají smajlíky a zkratky)
# jako output dostanu list se stringy s jednotlivými řádky

raw_text_twitter <- tolower(read_lines("E:/honzi/Documents/final/en_US/test_twitter.txt"))
raw_text_news <- tolower(read_lines("E:/honzi/Documents/final/en_US/test_news.txt"))
raw_text_blogs <- tolower(read_lines("E:/honzi/Documents/final/en_US/test_blogs.txt"))

raw_text <- append(append(raw_text_twitter, raw_text_news), raw_text_blogs)

raw_words <- tokens(raw_text, what = "word", remove_symbols = TRUE, 
                    remove_numbers = TRUE, remove_punct = TRUE,
                    remove_url = TRUE, padding = FALSE)

filtered_words <- tokens_remove(raw_words, "[^[:alpha:][:punct:]]", valuetype = "regex")


# tato funkce spočítá frekvenci všech bigramů v x, vyřadí všechny, které mají frekvenci méně jak pět

bigramy <- textstat_collocations(filtered_words, size = 2, min_count = 25) # size = 2 říká funkci, že má počítat bigramy

# tato funkce udělá data.table, kde jsou tři sloupce: První slovo, Druhé slovo, Frekvence

#app_bigramy <- data.table(str_extract(bigramy$collocation, regex("^[[:alpha:][:punct:]<>]+(?= )")), 
#                          str_extract(bigramy$collocation, regex("(?<= )[[:alpha:][:punct:]<>]+$")), bigramy$count)

# nastavení jmen sloupců v tabulce

#setnames(app_bigramy, c("V1", "V2", "V3"), c("PrvniSlovo", "DruheSlovo", "Freq"))

# vyfiltruji řádky, kde je druhé slovo token

#app_bigramy <- app_bigramy[DruheSlovo != "<>"]

# nastavím pořadí v této tabulce a to sestupně podle frekvence

#setorder(app_bigramy, -Freq)

