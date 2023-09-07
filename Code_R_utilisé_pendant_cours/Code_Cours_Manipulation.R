#install_course("The R Programming Environment")

getwd()
setwd("C:/Users/J00380/Desktop/Mes dossiers personnels/mes cours/Documents_cours_Programmation_R/Cours")


# 1. Manipulation de donnees ----------------------------------------------
install.packages("nom_package")
library(tidyverse)
#sessioninfo::session_info(c("tidyverse"))
library(tibble)
library(dplyr)

head(mtcars)
as_tibble(mtcars)

ma_table <- tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y
)



# tibble peut avoir des noms de colonnes qui ne sont pas valides dans d'autres formats
# avantages : affiche le type de variables, les 10 premi?res lignes et toutes les colonnes
ma_table <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)


# read_csv() reads comma delimited files,
# read_csv2() reads semicolon separated files (common in countries where , is used as the decimal place), 
# read_tsv() reads tab delimited files, and
# read_delim reads files with any separator

library(readr)
iris = read_delim("Donnees/iris.txt", delim="\t")
head(iris)

library(readxl)
iris = read_excel("Donnees/iris.xlsx")
head(iris)

library(haven)
iris = read_sas("Donnees/iris.sas7bdat")
head(iris)

library(rvest)
sw = read_html("https://www.imdb.com/title/tt0076759/")




write.table(mtcars, file = "Donnees/mtcars_test.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

mtcars_test = read_delim("Donnees/mtcars_test.txt", delim="\t")


creation_csv = read_csv("a,b,c
1,2,3
4,5,6")


creation_csv_meta_data = read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2)

creation_csv_coms = read_csv("# A comment I want to skip
  x,y,z
  1,2,3", comment = "#")

library(magrittr)
x = c(1,4,2,3,6,10,2,11)
mean(x)
x %>% mean
x %>% sum

## Enchainer les operations de manipulation de donn?es avec le package dplyr 
# Filter 
library(tibble)
cars = mtcars %>% rownames_to_column("model")

# Comment l'utiliser avec le pipe pour succ?der des op?rations
library(dplyr)
cars %>% filter(mpg>=30)
cars %>% filter(cyl==4)
cars %>% filter(cyl==4 & qsec >17)
cars %>% filter(between(mpg, 30, 32))


new_cars = cars %>% filter(mpg >= 30)  #car dplyr ne transforme l'input donc necessaire d'enregister
new_cars


# Arrange
arrange(cars, mpg)
cars %>% arrange(mpg)
cars %>% arrange(am,mpg)
cars %>% arrange(desc(mpg))


# Slice 
cars %>% slice(1:2)
cars %>% slice(c(2,6))
nrow(cars)
cars %>% slice(seq(1,n(), by=4))
cars %>% slice(31:n())
cars %>% slice((n()-4):n())



# Select
cars %>% select(model)
cars %>% select(mpg,cyl,model)
cars %>% select(2,5,7)
cars %>% select(starts_with("m"))

#rename(mon_data_frame, var1 = var2)
?select_helpers

# Mutate
cars_2 = cars %>% mutate(cyl_ratio=cyl/carb)
cars_2 = cars %>% mutate(cyl_ratio=cyl/carb, wt_ratio=wt/hp)

# Transmutate
cars_1 = cars %>% transmute(cyl_ratio=cyl/carb)

# Doublons et distinc
cars %>% select(cyl) %>% distinct
t = unique(cars$cyl)

# Enchainement d'operations (groupy et summerize)
cars %>% select(starts_with("c")) %>% summary
cars %>% select(mpg,wt,hp) %>% as_tibble %>% head

cars %>% group_by(cyl)
cars %>% group_by(cyl) %>% summarise(n=n())

cars %>% group_by(cyl) %>% summarise(mean(mpg))
summarise(cars, mean = mean(mpg , na.rm = TRUE)) # na.rm important si des valeurs manquantes existent



# Jointure et rappels sur les differents types de jointure
engine = tibble(
  cyl = c(6,8,12),
  type = c("medium", "big", "very big")
)

# Diff?rentes jointures
cars %>% inner_join(engine)
cars %>% left_join(engine)
cars %>% right_join(engine)
cars %>% full_join(engine)
cars %>% semi_join(engine)
cars %>% anti_join(engine)



# 2. Donnees specifiques -----------------------------------------------------

# 2.1 Code seance precedente --------------------------------------------------
# Ctrl+Shift+R (Cmd+Shift+R on the Mac) pour creer des section
# Shift+Alt+J pour choisir une section 



# 2.2 Donnees de type STRING -----------------------------------------------------

library(tibble)
library(dplyr)
as_tibble(mtcars)
head(mtcars)
# ctrl+shift+m pour fiare le pipe
cars = mtcars %>% rownames_to_column("model")

install.packages("stringr")
library(stringr)
string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'


cars$model
str_length(cars$model)
str_c(cars$model, collapse = ", ")
str_sub(cars$model, 1, 5)
str_sub(cars$model, -3, -1)



str_subset(cars$model, "Merc")
str_subset(cars$model, "[0-9]")
str_detect(cars$model, "[0-9]")
#str_match(cars$model, "(.+)[ ](.+)")
str_split(cars$model, " ")


# Attention !!! Rappel sur les NA
NA > 5
10 == NA
NA + 10
NA / 2

install.packages("lubridate")

# 2.2 Donneees de type DATE -----------------------------------------------------

library(lubridate)
now()
today()
year(today())


today = today()
month(today)
month(today, label = TRUE)
month(today, label = TRUE, abbr = FALSE) #abbr abbreviation
day(today)
mday(today) #day of the month
wday(today) #day of the week
wday(today, label = TRUE)
wday(today, week_start = 1)
wday(today, label = TRUE, week_start = 1)
yday(today) #day of the year

today + period(week = 1, day = 3)
today + period("1W 3D")
today - years(1) - days(10)


bday = ymd("19771114") 
bday

diff = today - bday
diff
## Time difference of 15588 days
as.period(diff)
## [1] "15588d 0H 0M 0S"
as.duration(diff)



cars = mtcars %>% rownames_to_column("model")

cars %>% summarise(n=n(), mpg_av=mean(mpg),
                   wt_med=median(wt)) 

mtcars %>% summarise_all(mean)


cars %>% group_by(cyl) %>%
  summarise(n=n(), mpg_av=mean(mpg), wt_med=median(wt))

cars %>% select(mpg,wt,qsec) %>% colMeans

cars %>% select(mpg,wt,qsec) %>% cor

cars %>% group_by(cyl) %>%
  select(cyl,mpg,wt,qsec) %>%
  summarise_all(mean)

cars %>% select(mpg,wt) %>%
  plot(main="Weight vs Miles/gallon")


# condition --------------------------------------------------------------------

y=5
if (y < 20) {
  message("Too low")
} else {
  message("Too high")
}

if (y < 20) "Too low" else "Too high"
ifelse(y < 20, "Too low", "Too high")


# loops -------------------------------------------------------------------
library(dplyr)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)


median(df$a)
median(df$b)
median(df$c)
median(df$d)


output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}


# Equivalent to
i <- 1
a = while (i <= length(df)) {
  output[[i]] <- median(df[[i]])      # 3. body
  i <- i + 1 
}



# functions ---------------------------------------------------------------

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_mean(1:6, 1:3)


# lists -------------------------------------------------------------------

list()
l= list(v=1:10, head(LETTERS), head(mtcars))
l[[1]]
l$v
l[1:2]



# Fonctions specifiques ---------------------------------------------------

df %>% summarise(mean(a),
                    median(a))
