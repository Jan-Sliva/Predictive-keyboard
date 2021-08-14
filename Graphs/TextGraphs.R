library(ggplot2)

TextGraph1 <- function(data_freq){
  
  ggplot(data = data_freq, aes(x = order, y = freqPercent)) +
    geom_line() + xlim(0, NA) + ylim(0, NA) +
    labs(title = "Slova seřazená podle výskytu od nejvyššího",
         y = "Výskyt slova [%]",
         x = "Pořadí slova")
}

TextGraph2 <- function(data_freq){
  
  sum_columns <- data_freq$freqPercent[1]
  
  for (i in 2:length(data_freq$freqPercent)){
    x <- sum_columns[i-1] + data_freq$freqPercent[i]
    sum_columns <- append(sum_columns, x)
  }
  
  data_freq$sumFreq <- sum_columns
  
  ggplot(data = data_freq, aes(x = order, y = sumFreq, group = 1)) +
    geom_line(lwd = 0.5) + xlim(0, NA) + ylim(0, NA) + 
    labs(title = "Slova seřazená podle výskytu od nejvyššího, na grafu je součet výskytu tohoto a všech předešlých slov",
         y = "Součet výskytu tohoto a všech předešlých slov [%]",
         x = "Pořadí slova") +
    theme(title = element_text(size = 10))
}

TextGraph3 <- function(data_freq, all_words_count, unique_words_count){
  
  freq_freq = as.data.frame(table(data_freq$frequency))
  
  names(freq_freq) <- c("freq", "freqOfFreq")
  
  freq_freq$freq <- as.numeric(levels(freq_freq$freq))[freq_freq$freq]
  
  freq_freq$freqPercent = freq_freq$freqOfFreq / unique_words_count * 100
  
  ret <-  list()
  
  ret[[1]] <- ggplot(data = freq_freq, aes(x = freq, y = freqPercent, group = 1)) +
    geom_line() + geom_point() + labs(title = "Kolik procent slov má danou frekvenci", x = "Frekvence slov", y = "Kolik procent slov má danou frekvenci [%]") +
    xlim(1, NA) + ylim(0, NA)
  
  ret[[2]] <- ggplot(data = freq_freq, aes(x = freq, y = freqPercent, group = 1)) +
    geom_line() + geom_point() + labs(title = "Kolik procent slov má danou frekvenci - pro hodnoty frekvence slov 1 - 30", x = "Frekvence slov", y = "Kolik procent slov má danou frekvenci [%]") +
    ylim(0, NA) + scale_x_continuous(breaks = 1:30, minor_breaks = NULL, limits = c(1, 30))
  
  freq_freq$occur <- freq_freq$freq * freq_freq$freqOfFreq / all_words_count * 100
  
  ret[[3]] <- ggplot(data = freq_freq, aes(x = freq, y = occur, group = 1)) +
    geom_point() + labs(title = "Celkový výskyt všech slov s určitou frekvencí v textu", x = "Frekvence slov", y = "Celkový výskyt všech slov s určitou frekvencí [%]") +
    xlim(0, NA) + ylim(0, NA)
  
  ret[[4]] <- ggplot(data = freq_freq, aes(x = freq, y = occur, group = 1)) +
    geom_point() + labs(title = "Celkový výskyt všech slov s určitou frekvencí v textu - pro hodnoty frekvence slov 1 - 1000", x = "Frekvence slov", y = "Celkový výskyt všech slov s určitou frekvencí [%]") +
     xlim(0, 1000) + ylim(0, NA)
  
  ret[[5]] <- ggplot(data = freq_freq, aes(x = freq, y = occur, group = 1)) +
    geom_point() + labs(title = "Celkový výskyt všech slov s určitou frekvencí v textu - pro hodnoty frekvence slov 1 - 250", x = "Frekvence slov", y = "Celkový výskyt všech slov s určitou frekvencí [%]") +
    xlim(0, 250) + ylim(0, NA)
  
  return(ret)
}


