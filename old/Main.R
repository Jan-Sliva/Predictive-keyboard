# tento skript načte text a udělá z něj tabulku vhodnou pro vyhledávání v ní

# načítání knihoven
library(readr) # načítání textu ze souboru
library(stringr) # zpracování textu
library(dplyr) # zpracování tabulky
library(quanteda) # analýza počtu bigramů
library(data.table) # knihovna s tabulkou, která používá hešování
library(hash) # hešování

# načítání textů ze souborů
# budu pracovat se třemi typy textů - z twitteru, ze zpráv a z blogů
# pro každý tento typ budu aktivovat dané funkce jednotlivě, 
# což mi umožní v budoucnosti využívat jiný regex pro každý typ souboru (např. pro twitter, kde se často používají smajlíky a zkratky)
# jako output dostanu list se stringy s jednotlivými řádky

text_twitter_test <- tolower(read_lines("E:/honzi/Documents/final/en_US/test_twitter.txt"))
text_news_test <- tolower(read_lines("E:/honzi/Documents/final/en_US/test_news.txt"))
text_blogs_test <- tolower(read_lines("E:/honzi/Documents/final/en_US/test_blogs.txt"))

# filtrování znaků, které tam nechci mít pomocí regex
# za vyraz1 mezeru, za vyraz2 prázdný string

vyraz1 <- c("(?<!e)-(?!mail)")
vyraz2 <- c("[^a-zA-Z'’ \\.\\-]")
clear_twitter <- gsub(vyraz2, "", gsub(vyraz1, " ", text_twitter_test, perl = TRUE))
clear_news <- gsub(vyraz2, "", gsub(vyraz1, " ", text_news_test, perl = TRUE))
clear_blogs <- gsub(vyraz2, "", gsub(vyraz1, " ", text_blogs_test, perl = TRUE))

# nejdříve smažu všechny tečky na konci stringů
# oddělování vět - snažím se oddělovat věty pomocí tečky, která nenáleží nějaké zkratce, danou tečky potom smažu
# potom věty dám do jednoho listu (předtím to bylo v listu, ve kterém jsou listy s jednotlivými větami na jedné řádce)

vyraz3 <- c("((?<! [mM]iss)(?<! (U\\.S|[mM]rs|etc|[Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ep|[Oo]ct|[Nn]ov|[Dd]ec))(?<! ([mM][sr]|[Nn]o))[\\.\\?\\!] )|\\.{2,}")
sentences_twitter <- unlist(strsplit(gsub("\\.+$", "", clear_twitter), vyraz3, perl = TRUE))
sentences_news <- unlist(strsplit(gsub("\\.+$", "", clear_news), vyraz3, perl = TRUE))
sentences_blogs <- unlist(strsplit(gsub("\\.+$", "", clear_blogs), vyraz3, perl = TRUE))

# nejdřív nahradím všechny apostrofy za jeden typ apostrofu (jsou dva typy apostrofů)
# potom oddělím slova pomocí mezery nebo podle toho, když je tam apostrof ('m, 's, 've ... považuji za samostatná slova)
# toto mi vrátí list s větami, kde věta je list se slovy z dané věty

vyraz4 <- c("((?<=[[:alpha:]])(?='(s|d|m) ))|((?<=[[:alpha:]])(?='(re|ll|ve) ))|((?<=s)(?=' ))| +")
split_text_twitter <- strsplit(gsub("’", "'", sentences_twitter), vyraz4, perl = TRUE)
split_text_news <- strsplit(gsub("’", "'", sentences_news), vyraz4, perl = TRUE)
split_text_blogs <- strsplit(gsub("’", "'", sentences_blogs), vyraz4, perl = TRUE)

# tato funkce vymaže z listu všechny prázdné stringy a stringy obsahující jenom tečky

filterEmptyStrings <-  function(x){x[x != "" & !grepl("\\.+", x)]}

# Nejdřív aktivuji filterEmptyStrings pro všechny věty
# potom z listu vymažu všchny věty, které nemají délku větší než 1

result_twitter <- lapply(split_text_twitter, filterEmptyStrings)
result_twitter <- result_twitter[lapply(result_twitter, length) > 1]

result_news <- lapply(split_text_news, filterEmptyStrings)
result_news <- result_news[lapply(result_news, length) > 1]

result_blogs <- lapply(split_text_blogs, filterEmptyStrings)
result_blogs <- result_blogs[lapply(result_blogs, length) > 1]

# dám všechny listy do jednoho

results_test <- append(append(result_twitter, result_news), result_blogs)

# udělám tabulky s výskyty slova

unlisted_results = unlist(results_test)

frekvence_slov <- as.data.frame(table(unlisted_results))

# udělám tabulku se seřazenými výskyty slova

sorted_slova <- frekvence_slov[order(frekvence_slov$Freq, decreasing = TRUE),]

# Teď udělám Hešovaný list, kde budou slova, která změním za token (žolíka, který bude nahrazovat slova s malým výskytem)
# jako token budu používat "<>"

hranice <-  32
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

# udělám z mého dosavadního výsledku objekt, se kterým může pracovat funkce textstat_collocations

x <- tokens(result_tokeny, what = "character")

# tato funkce spočítá frekvenci všech bigramů v x, vyřadí všechny, které mají frekvenci méně jak pět

bigramy <- textstat_collocations(x, min_count = 25, size = 2) # size = 2 říká funkci, že má počítat bigramy

# tato funkce udělá data.table, kde jsou tři sloupce: První slovo, Druhé slovo, Frekvence

app_bigramy <- data.table(str_extract(bigramy$collocation, regex("^[[:alpha:][:punct:]<>]+(?= )")), 
                          str_extract(bigramy$collocation, regex("(?<= )[[:alpha:][:punct:]<>]+$")), bigramy$count)

# nastavení jmen sloupců v tabulce

setnames(app_bigramy, c("V1", "V2", "V3"), c("PrvniSlovo", "DruheSlovo", "Freq"))

# vyfiltruji řádky, kde je druhé slovo token

app_bigramy <- app_bigramy[DruheSlovo != "<>"]

# nastavím pořadí v této tabulce a to sestupně podle frekvence

setorder(app_bigramy, -Freq)
