library(readr)


mySize = 5*1024*1024

myFile = "E:\\honzi\\Corpora\\EN_US\\news.txt"

txt = read_lines(myFile)

size = file.info(myFile)$size

num = round((mySize * length(txt)) / size)

txt2 = txt[sample.int(length(txt), num)]


write_lines(txt2, "E:\\honzi\\Corpora\\EN_US\\final.txt", append = TRUE)
