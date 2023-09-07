#### Formatage données foot 2009 - 2016

library(readr)
Football = read_delim("Football_2009_2016.csv", 
                                 ";", escape_double = FALSE, 
                                locale = locale(encoding = "ISO-8859-2"), 
                                 trim_ws = TRUE)
View(Football)

library(stringr)
n = nrow(Football)
Football$age = as.integer(unlist(str_split(Football$age, "Ę"))[seq(2,2*n, by = 2)])

save(Football, file = "Football_2009_2016.RData")
