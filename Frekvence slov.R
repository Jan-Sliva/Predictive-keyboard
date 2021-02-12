# tento skript udělá grafy týkající se frekvence slov
# vstupy: list s oddělenými slovy (results_test)

# knihovny
library(ggplot2) # grafy

# všechna slova do jednoho listu

unlisted_results <-  unlist(results_test)

# počet všech slov

velikost_results <-  length(unlisted_results)

# počet všech unikátních slov

velikost_unique_results = length(unique(unlisted_results))

#tabulky s počty frekvencí slov

frekvence_slov <- as.data.frame(table(unlisted_results))

# tabulka s počty frekvencí jednotlivých frekvencí slov

frekvence_frekvenci <- as.data.frame(table(frekvence_slov$Freq))

# udělám toto, aby se Var1 (tedy frekvence jednotlivých slov) považovali za číselný typ místo defaultního typu factor, který nemžu porovnávat

frekvence_frekvenci$Var1 <- as.numeric(levels(frekvence_frekvenci$Var1))[frekvence_frekvenci$Var1]

# vypočítám frekvenci jednotlivých frekvencí v procentech 

frekvence_frekvenci$FreqPercent <- frekvence_frekvenci$Freq * 100 / velikost_unique_results

# graf frekvence jednotlivých frekvencí
# na ose x jsou jedntlivé frkvence (Var1) a na ose y jsou frekvence frekvencí v procentech (group = 1 je tam proto, aby fungovala funkce geom_line())
# potom nastavím, že je to graf, na kterém jsou čáry a body, potom nastavím nadpis a popisky jednotlivých os
# a nastavím, že na ose x i y se má zobrazovat nula

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = FreqPercent, group = 1)) +
  geom_line() + geom_point() + labs(title = "Kolik procent slov má danou frekvenci", x = "Frekvence slov", y = "Kolik procent slov má danou frekvenci [%]") +
  xlim(0, NA) + ylim(0, NA)

# to samé jako v předešlém grafu, jenom omezím hodnoty osy x na 0 - 50

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = FreqPercent, group = 1)) +
  geom_line() + geom_point() + labs(title = "Kolik procent slov má danou frekvenci - pro hodnoty frekvence slov 0 - 50", x = "Frekvence slov", y = "Kolik procent slov má danou frekvenci [%]") +
  xlim(0, 50) + ylim(0, NA)

# teď omezím hosnoty osy x na 5 až 50, a nastavím osu y na hgodnoty 0 až 5

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = FreqPercent, group = 1)) +
  geom_line() + geom_point() + labs(title = "Kolik procent slov má danou frekvenci - pro hodnoty frekvence slov 5 - 50", x = "Frekvence slov", y = "Kolik procent slov má danou frekvenci [%]") +
  xlim(5, 50) + ylim(0, 5)

# teď vypočítám celkový výskyt všech slov s určitou frekvencí

frekvence_frekvenci$Vyskyt <- frekvence_frekvenci$Freq * frekvence_frekvenci$Var1 * 100 / velikost_results

# teď udělám graf, kde na osu y dám celkový výskyt všech slov s určitou frekvencí

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = frekvence_frekvenci$Vyskyt, group = 1)) +
  geom_point() + labs(title = "Celkový výskyt všech slov s určitou frekvencí", x = "Frekvence slov", y = "Celkový výskyt všech slov s určitou frekvencí [%]") +
  xlim(0, NA) + ylim(0, NA)

# teď tento graf omezím na ose x na frekvence jednotlivých frekvencí 0 až 1000

ggplot(data = frekvence_frekvenci, aes(x = Var1, y = frekvence_frekvenci$Vyskyt, group = 1)) +
  geom_point() + labs(title = "Celkový výskyt všech slov s určitou frekvencí - pro hodnoty frekvence slov 0 - 1000", x = "Frekvence slov", y = "Celkový výskyt všech slov s určitou frekvencí [%]") +
  xlim(0, 1000) + ylim(0, NA)

# seřadím všechna slova podle frekvence a udělám z toho novou tabulku

sorted_slova <- frekvence_slov[order(frekvence_slov$Freq, decreasing = TRUE),]

# resetuji čísla řádků (automaticky generované) (je to lepší pro zobrazení tabulky)

rownames(sorted_slova) <- NULL

# dám řádkům pořadí, abych s tím mohl pracovat

sorted_slova$number <- 1:length(sorted_slova$unlisted_results)

# spočátám frekvenci jednotlivých slov v procentech

sorted_slova$FreqPercent <- sorted_slova$Freq * 100 / velikost_results

# udělám list, do kterého budu ukládat součet výskytu daného slova se stejnou pozicí a všech předešlých slov

sum_columns <- c(sorted_slova$FreqPercent[1])

# teď budu přidávat prvky do tohoto listu

for (i in 2:length(sorted_slova$FreqPercent)){
  x <- sum_columns[i-1] + sorted_slova$FreqPercent[i]
  sum_columns <- append(sum_columns, x)
}

# teď to přiřadím do tabulky sorted_slova

sorted_slova$SumFrequence <- sum_columns

# udělám graf, kde na ose x bude pořadí slova a na ose y bude součet výskytu tohoto a všech předešlých slov
# šířka čáry bude 0.5
# osu y omezím na 0 až 100.001, aby se mi ukazoval i poslední záznam, který se rovná 100

ggplot(data = sorted_slova, aes(x = number, y = SumFrequence, group = 1)) +
  geom_line(lwd = 0.5) +xlim(0, NA) + ylim(0, 100.001) + labs(title = "Slova seřazená podle výskytu od nejvyššího, na grafu je součet výskytu tohoto a všech předešlých slov",
                                                          y = "Součet výskytu tohoto a všech předešlých slov [%]",
                                                          x = "Pořadí slova")
