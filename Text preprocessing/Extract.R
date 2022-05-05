library(readr)

file = "HC\\Eng_US_Twitter.txt"
out = "twitter.txt"

path = "E:\\honzi\\Corpora\\EN_US\\"

txt = read_lines(paste0(path, file))

ms = "(?<=[\\d/]{10}\t\\d{1,10}\t[\\d,]{1,20}\t).*"



txt2 = lapply(txt, function(x){ str_extract(x, ms)})

write_lines(txt2, paste0(path, out))


