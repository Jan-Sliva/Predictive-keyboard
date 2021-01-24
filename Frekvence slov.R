library(ggplot2)

# tento skript udělá grafy týkající se frekvence slov
# vypíše seznam všech slov, které by se měly vyměnit za token podle nastavené hranice (viz níže)
# vstupy: list s oddělenými slovy (results_test)

unlisted_results = unlist(results_test)

velikost_results = length(unlisted_results)

velikost_unique_results = length(unique(unlisted_results))

frekvence_slov <- as.data.frame(table(unlisted_results))

frekvence_frekvenci <- as.data.frame(table(frekvence_slov$Freq))

frekvence_frekvenci$Var1 <- as.numeric(levels(frekvence_frekvenci$Var1))[frekvence_frekvenci$Var1]

frekvence_frekvenci$FreqPercent <- frekvence_frekvenci$Freq * 100 / velikost_unique_results

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = FreqPercent, group = 1)) +
  geom_line() + geom_point() + labs(title = "Kolik procent slov má danou frekvenci", x = "Frekvence slov", y = "Kolik procent slov má danou frekvenci [%]") +
  xlim(0, NA) + ylim(0, NA)

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = FreqPercent, group = 1)) +
  geom_line() + geom_point() + labs(title = "Kolik procent slov má danou frekvenci - pro hodnoty frekvence slov 0 - 50", x = "Frekvence slov", y = "Kolik procent slov má danou frekvenci [%]") +
  xlim(0, 50) + ylim(0, NA)

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = FreqPercent, group = 1)) +
  geom_line() + geom_point() + labs(title = "Kolik procent slov má danou frekvenci - pro hodnoty frekvence slov 5 - 50", x = "Frekvence slov", y = "Kolik procent slov má danou frekvenci [%]") +
  xlim(5, 50) + ylim(0, 5)

frekvence_frekvenci$Vyskyt <- frekvence_frekvenci$Freq * frekvence_frekvenci$Var1 * 100 / velikost_results

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = frekvence_frekvenci$Vyskyt, group = 1)) +
  geom_point() + labs(title = "Celkový výskyt všech slov s určitou frekvencí", x = "Frekvence slov", y = "Celkový výskyt všech slov s určitou frekvencí [%]") +
  xlim(0, NA) + ylim(0, NA)

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = frekvence_frekvenci$Vyskyt, group = 1)) +
  geom_point() + labs(title = "Celkový výskyt všech slov s určitou frekvencí - pro hodnoty frekvence slov 0 - 1000", x = "Frekvence slov", y = "Celkový výskyt všech slov s určitou frekvencí [%]") +
  xlim(0, 1000) + ylim(0, NA)

sorted_slova <- frekvence_slov[order(frekvence_slov$Freq, decreasing = TRUE),]

rownames(sorted_slova) <- NULL

sorted_slova$number <- 1:length(sorted_slova$unlisted_results)

sorted_slova$unlisted_results <- factor(sorted_slova$unlisted_results, levels=unique(sorted_slova$unlisted_results))

sorted_slova$FreqPercent <- sorted_slova$Freq * 100 / velikost_results

sum_columns <- c(sorted_slova$FreqPercent[1])

for (i in 2:length(sorted_slova$FreqPercent)){
  x <- sum_columns[i-1] + sorted_slova$FreqPercent[i]
  sum_columns <- append(sum_columns, x)
}

sorted_slova$SumFrequence <- sum_columns

ggplot(data = sorted_slova, aes(x = number, y = SumFrequence, group = 1)) +
  geom_line(lwd = 0.5) +xlim(0, NA) + ylim(0, 100) + labs(title = "Slova seřazená podle výskytu od nejvyššího, na grafu je součet výskytu tohoto a všech předešlých slov",
                                                          y = "Součet výskytu tohoto a všech předešlých slov [%]",
                                                          x = "Pořadí slova")

# hranice frekvence slova, pod kterou budu nahrazovat všechny slova za token
hranice = 3
potreba_zmenit_za_token = filter(sorted_slova, Freq <= hranice)$unlisted_result
